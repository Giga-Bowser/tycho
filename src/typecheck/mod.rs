pub mod ctx;
pub mod error;
pub mod pool;
pub mod type_env;
pub mod types;

use crate::{
    error::DiagCtx,
    parser::{
        ast,
        pool::{ExprPool, ExprRef},
    },
    sourcemap::SourceFile,
    utils::{spanned::Spanned, Ident, Span, Symbol},
};

use self::{
    ctx::TypeContext,
    error::{
        CheckErr, MethodOnWrongType, MismatchedTypes, NoReturn, NoSuchField, NoSuchMethod,
        NoSuchVal, ReturnCount,
    },
    pool::TypeRef,
    type_env::Resolved,
    types::{Function, Struct, TableType, Type, TypeKind},
};

type TResult<T> = Result<T, Box<CheckErr>>;
type TRVec<T> = TResult<Vec<T>>;

pub struct TypeChecker<'a> {
    pub file: &'a SourceFile,
    pub tcx: &'a mut TypeContext,
    pub expr_pool: &'a ExprPool,
}

impl<'a> TypeChecker<'a> {
    pub const fn new(
        file: &'a SourceFile,
        tcx: &'a mut TypeContext,
        expr_pool: &'a ExprPool,
    ) -> Self {
        TypeChecker {
            file,
            tcx,
            expr_pool,
        }
    }

    pub fn into_diag_ctx(self) -> DiagCtx<'a> {
        DiagCtx {
            tcx: self.tcx,
            expr_pool: self.expr_pool,
            file: self.file,
        }
    }
}

impl TypeChecker<'_> {
    pub fn check_stmt(&mut self, stmt: &ast::Stmt) -> TResult<()> {
        match stmt {
            ast::Stmt::Declare(decl) => self.check_decl(decl),
            ast::Stmt::MethodDecl(method_decl) => self.check_method_decl(method_decl),
            ast::Stmt::MultiDecl(multi_decl) => self.check_multi_decl(multi_decl),
            ast::Stmt::IfStmt(if_stmt) => self.check_if_stmt(if_stmt),
            ast::Stmt::WhileStmt(while_stmt) => self.check_while_stmt(while_stmt),
            ast::Stmt::RangeFor(range_for) => self.check_range_for(range_for),
            ast::Stmt::KeyValFor(keyval_for) => self.check_keyval_for(keyval_for),
            ast::Stmt::Assign(ast::Assign { lhs, rhs }) => {
                let lhs_type = self.check_suffixed_name(lhs)?;
                let rhs_type = self.check_expr(*rhs)?;
                if self.can_equal(lhs_type, rhs_type) {
                    Ok(())
                } else {
                    Err(MismatchedTypes::err(lhs_type, rhs_type))
                }
            }
            ast::Stmt::MultiAssign(multi_assign) => self.check_multi_assign(multi_assign),
            // TODO: investigate if making a more expression-statement specific checking function is worth it
            ast::Stmt::ExprStmt(suffixed_expr) => self.check_suffixed_expr(suffixed_expr).map(drop),
            ast::Stmt::Block(block) => self.with_scope(|this| {
                for stmt in block {
                    this.check_stmt(stmt)?;
                }
                Ok(())
            }),
            ast::Stmt::StructDecl(struct_decl) => self.check_struct_decl(struct_decl),
            _ => Ok(()),
        }
    }

    fn check_decl(&mut self, decl: &ast::Declare) -> TResult<()> {
        if let Some(val) = decl.val {
            let val_type = self.check_expr(val)?;

            let decl_ty = if let Some(ty) = &decl.ty {
                self.resolve_type_node(ty)?
            } else {
                self.tcx
                    .value_map
                    .insert_value(decl.lhs.name.ident(self.file), val_type);
                return Ok(());
            };

            if !self.can_equal(decl_ty, val_type) {
                return Err(MismatchedTypes::err(decl_ty, val_type));
            }

            self.tcx
                .value_map
                .insert_value(decl.lhs.name.ident(self.file), decl_ty);
            Ok(())
        } else {
            let decl_ty = match &decl.ty {
                Some(ty) => self.resolve_type_node(ty)?,
                None => {
                    unreachable!("decl without type or value? that's just a name")
                }
            };

            if decl.lhs.suffixes.is_empty() {
                self.tcx
                    .value_map
                    .insert_value(decl.lhs.name.ident(self.file), decl_ty);
                return Ok(());
            }

            let mut ty = self
                .tcx
                .value_map
                .get_top(&decl.lhs.name.ident(self.file))
                .ok_or_else(|| {
                    Box::new(CheckErr::NoSuchVal(NoSuchVal {
                        val_name: decl.lhs.name,
                    }))
                })?
                .inner();

            for suffix in &mut decl.lhs.suffixes.iter().take(decl.lhs.suffixes.len() - 1) {
                match suffix {
                    ast::Suffix::Access(ast::Access { field_name: name }) => {
                        if let Some(field) =
                            self.tcx.pool[ty].kind.get_field(name.symbol(self.file))
                        {
                            ty = field;
                        } else {
                            return Err(Box::new(CheckErr::NoSuchField(NoSuchField {
                                field_name: *name,
                            })));
                        }
                    }
                    _ => unreachable!(),
                }
            }

            let last = decl.lhs.suffixes.last().unwrap();

            if let ast::Suffix::Access(ast::Access { field_name }) = last {
                match &mut self.tcx.pool[ty].kind {
                    TypeKind::Table(_) => Ok(()),
                    TypeKind::Struct(strukt) => {
                        strukt.fields.push((field_name.symbol(self.file), decl_ty));
                        Ok(())
                    }
                    _ => Err(Box::new(CheckErr::BadAccess {
                        span: *field_name,
                        ty,
                    })),
                }
            } else {
                unreachable!()
            }
        }
    }

    fn check_method_decl(&mut self, method_decl: &ast::MethodDecl) -> TResult<()> {
        let ty = self
            .tcx
            .value_map
            .get_type(&method_decl.struct_name.ident(self.file))
            .unwrap();

        let TypeKind::Struct(_) = self.tcx.pool[ty].kind else {
            return Err(Box::new(CheckErr::MethodOnWrongType(MethodOnWrongType {
                span: Span::cover(method_decl.struct_name, method_decl.method_name),
                ty,
            })));
        };

        let func: &ast::FuncNode = &method_decl.func;
        let method_type = self.with_scope(|this| {
            this.tcx.value_map.insert_value(Ident::from_str("self"), ty);
            let func_ty = this.resolve_function_type(&func.ty)?;
            for (name, ty) in &func_ty.params {
                if !name.is_empty() {
                    this.tcx.value_map.insert_value(name.ident(this.file), *ty);
                }
            }

            if let TypeKind::Nil = this.tcx.pool[func_ty.returns].kind {
                // TODO: this is obviously bad
                return Ok(this.tcx.pool.add(TypeKind::Function(func_ty).into()));
            }

            let has_return = this.check_func_body(&func.body, func_ty.returns)?;

            if !has_return {
                return Err(Box::new(CheckErr::NoReturn(NoReturn {
                    func_node: func.clone(),
                })));
            }

            Ok(this.tcx.pool.add(TypeKind::Function(func_ty).into()))
        })?;

        let TypeKind::Struct(Struct { fields, name: _ }) = &mut self.tcx.pool[self
            .tcx
            .value_map
            .get_type(&method_decl.struct_name.ident(self.file))
            .unwrap()]
        .kind
        else {
            unreachable!()
        };

        fields.push((method_decl.method_name.symbol(self.file), method_type));

        Ok(())
    }

    fn check_multi_decl(&mut self, multi_decl: &ast::MultiDecl) -> TResult<()> {
        // ok so we need to basically flatten the types. so for example
        // a, b, c := twoReturnFunction(), oneReturnFunction()
        // a, b, c := (type1, type2), type3
        // a, b, c := type1, type2, type3
        let mut types = Vec::new();

        for rhs in &multi_decl.rhs_arr {
            let expr_type = self.check_expr(*rhs)?;
            if let TypeKind::Multiple(mult) = &self.tcx.pool[expr_type].kind {
                types.extend(mult);
            } else {
                types.push(expr_type);
            }
        }

        for (i, name) in multi_decl.lhs_arr.iter().enumerate() {
            if i < types.len() {
                self.tcx
                    .value_map
                    .insert_value(name.ident(self.file), types[i]);
            } else {
                let ty = self.tcx.pool.add(Type {
                    kind: TypeKind::Nil,
                    span: None,
                });
                self.tcx.value_map.insert_value(name.ident(self.file), ty);
            }
        }

        Ok(())
    }

    fn check_struct_decl(&mut self, struct_decl: &ast::StructDecl) -> TResult<()> {
        let name = struct_decl.name.ident(self.file);
        let fields = struct_decl
            .members
            .iter()
            .map(|it| Ok((it.name.symbol(self.file), self.resolve_type_node(&it.ty)?)))
            .collect::<TRVec<_>>()?;
        let ty = Type {
            kind: TypeKind::Struct(Struct {
                name: name.clone(),
                fields,
            }),
            span: Some(struct_decl.name),
        };
        self.tcx.value_map.insert_type(name, self.tcx.pool.add(ty));
        Ok(())
    }

    fn check_multi_assign(&mut self, multi_assign: &ast::MultiAssign) -> TResult<()> {
        // ok so we need to basically flatten the types. so for example
        // a, b, c := twoReturnFunction(), oneReturnFunction()
        // a, b, c := (type1, type2), type3
        // a, b, c := type1, type2, type3
        let mut types = Vec::new();

        for rhs in &multi_assign.rhs_arr {
            let rhs_type = self.check_expr(*rhs)?;
            if let TypeKind::Multiple(mult) = &self.tcx.pool[rhs_type].kind {
                types.extend(mult);
            } else {
                types.push(rhs_type);
            }
        }

        for (idx, suffixed_expr) in multi_assign.lhs_arr.iter().enumerate() {
            let expected = self.check_suffixed_expr(suffixed_expr)?;
            let recieved = if idx < types.len() {
                types[idx]
            } else {
                self.tcx.pool.add(Type {
                    kind: TypeKind::Nil,
                    span: None,
                })
            };

            if !self.can_equal(expected, recieved) {
                return Err(MismatchedTypes::err(expected, recieved));
            }
        }

        Ok(())
    }

    fn check_expr(&mut self, expr: ExprRef) -> TResult<TypeRef> {
        match &self.expr_pool[expr] {
            ast::Expr::BinOp(binop) => self.check_binop(binop),
            ast::Expr::UnOp(unop) => match unop.op {
                ast::UnOpKind::Neg => {
                    let res = self.check_expr(unop.val)?;
                    if let TypeKind::Number = &self.tcx.pool[res].kind {
                        Ok(res)
                    } else {
                        Err(Box::new(CheckErr::BadNegate {
                            op_span: unop.op_span,
                            ty: res,
                        }))
                    }
                }
                ast::UnOpKind::Len => Ok(self.tcx.pool.add(Type {
                    kind: TypeKind::Number,
                    span: None,
                })),
                ast::UnOpKind::Not => {
                    let res = self.check_expr(unop.val)?;
                    if let TypeKind::Boolean = self.tcx.pool[res].kind {
                        Ok(res)
                    } else {
                        Err(Box::new(CheckErr::BadNot {
                            op_span: unop.op_span,
                            ty: res,
                        }))
                    }
                }
            },
            ast::Expr::Paren(paren_expr) => self.check_expr(paren_expr.val),
            ast::Expr::Simple(simple_expr) => self.check_simple_expr(simple_expr),
            ast::Expr::Name(span) => self
                .tcx
                .value_map
                .get_value(&span.ident(self.file))
                .ok_or_else(|| Box::new(CheckErr::NoSuchVal(NoSuchVal { val_name: *span }))),
        }
    }

    fn check_field(&mut self, field_node: &ast::FieldNode) -> TResult<TableType> {
        Ok(TableType {
            key_type: self.check_field_key(field_node)?,
            val_type: self.check_field_val(field_node)?,
        })
    }

    fn check_field_key(&mut self, field_node: &ast::FieldNode) -> TResult<TypeRef> {
        match field_node {
            ast::FieldNode::Field { key, .. } => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::String,
                span: Some(*key),
            })),
            ast::FieldNode::ExprField { key, .. } => self.check_expr(*key),
            ast::FieldNode::ValField { .. } => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::Number,
                span: None,
            })),
        }
    }

    fn check_field_val(&mut self, field_node: &ast::FieldNode) -> TResult<TypeRef> {
        match field_node {
            ast::FieldNode::Field { val, .. }
            | ast::FieldNode::ExprField { val, .. }
            | ast::FieldNode::ValField { val } => self.check_expr(*val),
        }
    }

    fn check_table(&mut self, table_node: &ast::TableNode) -> TResult<TableType> {
        if table_node.fields.is_empty() {
            return Ok(TableType {
                key_type: self.tcx.pool.add(TypeKind::Adaptable.into()),
                val_type: self.tcx.pool.add(TypeKind::Adaptable.into()),
            });
        }

        let mut result = self.check_field(&table_node.fields[0])?;

        for field in table_node.fields.iter().skip(1) {
            let key = self.check_field_key(field)?;
            if !self.can_equal(result.key_type, key) {
                result.key_type = self.tcx.pool.any();
                break;
            }
        }

        for field in table_node.fields.iter().skip(1) {
            let val = self.check_field_val(field)?;
            if !self.can_equal(result.val_type, val) {
                result.val_type = self.tcx.pool.any();
                break;
            }
        }

        Ok(result)
    }

    fn check_simple_expr(&mut self, simple_expr: &ast::SimpleExpr) -> TResult<TypeRef> {
        match simple_expr {
            ast::SimpleExpr::Num(s) => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::Number,
                span: Some(*s),
            })),
            ast::SimpleExpr::Str(s) => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::String,
                span: Some(*s),
            })),
            ast::SimpleExpr::Bool(s) => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::Boolean,
                span: Some(*s),
            })),
            ast::SimpleExpr::Nil(s) => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::Nil,
                span: Some(*s),
            })),
            ast::SimpleExpr::FuncNode(func) => self.check_func(func),
            ast::SimpleExpr::TableNode(table_node) => {
                let table_type = self.check_table(table_node)?;
                Ok(self.tcx.pool.add(TypeKind::Table(table_type).into()))
            }
            ast::SimpleExpr::SuffixedExpr(suffixed_expr) => self.check_suffixed_expr(suffixed_expr),
        }
    }

    fn check_func(&mut self, func: &ast::FuncNode) -> TResult<TypeRef> {
        self.with_scope(|this| {
            let func_ty = this.resolve_function_type(&func.ty)?;
            for (name, ty) in &func_ty.params {
                this.tcx.value_map.insert_value(name.ident(this.file), *ty);
            }

            if let TypeKind::Nil = this.tcx.pool[func_ty.returns].kind {
                // TODO: this is obviously bad
                return Ok(this.tcx.pool.add(TypeKind::Function(func_ty).into()));
            }

            let has_return = this.check_func_body(&func.body, func_ty.returns)?;

            if !has_return {
                return Err(Box::new(CheckErr::NoReturn(NoReturn {
                    func_node: func.clone(),
                })));
            }

            Ok(this.tcx.pool.add(TypeKind::Function(func_ty).into()))
        })
    }

    fn check_func_body(&mut self, body: &[ast::Stmt], return_type: TypeRef) -> TResult<bool> {
        self.with_scope(|this| {
            for stmt in body {
                match stmt {
                    ast::Stmt::IfStmt(ast::IfStmt { body: if_body, .. }) => {
                        if this.check_func_body(if_body, return_type)? {
                            return Ok(true);
                        }
                    }
                    ast::Stmt::WhileStmt(ast::WhileStmt {
                        body: while_body, ..
                    }) => {
                        if this.check_func_body(while_body, return_type)? {
                            return Ok(true);
                        }
                    }
                    ast::Stmt::RangeFor(ast::RangeFor { body, .. }) => {
                        if this.check_func_body(body, return_type)? {
                            return Ok(true);
                        }
                    }
                    ast::Stmt::KeyValFor(ast::KeyValFor { body, .. }) => {
                        if this.check_func_body(body, return_type)? {
                            return Ok(true);
                        }
                    }
                    ast::Stmt::Return(node @ ast::ReturnStmt { vals, .. }) => {
                        if vals.is_empty() {
                            return match &this.tcx.pool[return_type].kind {
                                TypeKind::Nil | TypeKind::Variadic => Ok(true),
                                TypeKind::Multiple(types) => {
                                    if types.is_empty() {
                                        Ok(true)
                                    } else {
                                        Err(Box::new(CheckErr::ReturnCount(ReturnCount {
                                            return_node: node.clone(),
                                            expected: types.len(),
                                        })))
                                    }
                                }
                                _ => Err(Box::new(CheckErr::ReturnCount(ReturnCount {
                                    return_node: node.clone(),
                                    expected: 1,
                                }))),
                            };
                        }

                        let returned_types = vals
                            .iter()
                            .map(|it| this.check_expr(*it))
                            .collect::<TRVec<_>>()?;

                        if let TypeKind::Multiple(types) = &this.tcx.pool[return_type].kind {
                            if types.len() != vals.len() {
                                return Err(Box::new(CheckErr::ReturnCount(ReturnCount {
                                    return_node: node.clone(),
                                    expected: types.len(),
                                })));
                            }

                            for (lhs, rhs) in types.iter().zip(returned_types.iter()) {
                                if !this.can_equal(*lhs, *rhs) {
                                    return Err(MismatchedTypes::err(*lhs, *rhs));
                                }
                            }
                        } else {
                            if vals.len() != 1 {
                                return Err(Box::new(CheckErr::ReturnCount(ReturnCount {
                                    return_node: node.clone(),
                                    expected: 1,
                                })));
                            }

                            let return_val = this.check_expr(vals[0])?;
                            return if this.can_equal(return_type, return_val) {
                                Ok(true)
                            } else {
                                Err(MismatchedTypes::err(return_type, return_val))
                            };
                        }

                        return Ok(true);
                    }
                    _ => (),
                }
                this.check_stmt(stmt)?;
            }

            Ok(false)
        })
    }

    fn check_suffixed_name(&mut self, suffixed_name: &ast::SuffixedName) -> TResult<TypeRef> {
        let mut ty = self
            .tcx
            .value_map
            .get_value(&suffixed_name.name.ident(self.file))
            .ok_or_else(|| {
                Box::new(CheckErr::NoSuchVal(NoSuchVal {
                    val_name: suffixed_name.name,
                }))
            })?;

        for suffix in &suffixed_name.suffixes {
            ty = self.check_suffix(ty, suffix)?;
        }

        Ok(ty)
    }

    fn check_suffixed_expr(&mut self, suffixed_expr: &ast::SuffixedExpr) -> TResult<TypeRef> {
        let mut ty = match &self.expr_pool[suffixed_expr.val] {
            ast::Expr::Name(name) => {
                if name.to_str(self.file) == "debug_print_ctx" {
                    eprintln!("{:#?}", self.tcx);
                    return Ok(self.tcx.pool.nil());
                }

                let res = self
                    .tcx
                    .value_map
                    .get(&name.ident(self.file))
                    .ok_or_else(|| Box::new(CheckErr::NoSuchVal(NoSuchVal { val_name: *name })))?;

                if let Resolved::Type(_) = res {
                    let Some(ast::Suffix::Method(_) | ast::Suffix::Access(_)) =
                        suffixed_expr.suffixes.first()
                    else {
                        return Err(Box::new(CheckErr::NoSuchVal(NoSuchVal { val_name: *name })));
                    };
                }

                res.inner()
            }
            _ => self.check_expr(suffixed_expr.val)?,
        };

        for suffix in &suffixed_expr.suffixes {
            ty = self.check_suffix(ty, suffix)?;
        }

        Ok(ty)
    }

    fn check_suffix(&mut self, mut base: TypeRef, suffix: &ast::Suffix) -> TResult<TypeRef> {
        match suffix {
            ast::Suffix::Index(ast::Index { key: _, span }) => match &self.tcx.pool[base].kind {
                TypeKind::Table(TableType { val_type, .. }) => base = *val_type,
                TypeKind::String | TypeKind::Adaptable | TypeKind::Any => (),
                _ => {
                    return Err(Box::new(CheckErr::BadIndex {
                        span: *span,
                        ty: base,
                    }));
                }
            },
            ast::Suffix::Access(ast::Access { field_name }) => match self.tcx.pool[base].kind {
                TypeKind::Struct(ref strukt) => {
                    if let Some(field) = strukt.get_field(field_name.symbol(self.file)) {
                        base = field;
                    } else {
                        return Err(Box::new(CheckErr::NoSuchField(NoSuchField {
                            field_name: *field_name,
                        })));
                    }
                }
                TypeKind::Table(TableType { key_type, val_type }) => {
                    let string_ty = self.tcx.pool.string();
                    if self.can_equal(string_ty, key_type) {
                        base = val_type;
                    } else {
                        return Err(MismatchedTypes::err(
                            self.tcx.pool.add(Type {
                                kind: TypeKind::String,
                                span: Some(*field_name),
                            }),
                            key_type,
                        ));
                    }
                }
                _ => {
                    return Err(Box::new(CheckErr::BadAccess {
                        span: *field_name,
                        ty: base,
                    }));
                }
            },
            ast::Suffix::Call(ast::Call { args }) => {
                base = self.check_call(base, args)?;
            }
            ast::Suffix::Method(ast::Method { method_name, args }) => {
                let method_str = method_name.to_str(self.file);
                if method_str == "new" {
                    return Ok(base);
                }

                match self.tcx.pool[base]
                    .kind
                    .get_field(Symbol::intern(method_str))
                {
                    Some(method) => base = method,
                    None => {
                        return Err(Box::new(CheckErr::NoSuchMethod(NoSuchMethod {
                            method_name: *method_name,
                        })))
                    }
                }

                base = self.check_call(base, args)?;
            }
        }

        Ok(base)
    }

    fn check_call(&mut self, ty: TypeRef, args: &[ExprRef]) -> TResult<TypeRef> {
        let args = args
            .iter()
            .map(|&arg| self.check_expr(arg))
            .collect::<TRVec<_>>()?;

        let TypeKind::Function(func_ty) = &self.tcx.pool[ty].kind else {
            // TODO: should probably be it's own error
            let expected = TypeKind::Function(Function {
                params: Vec::new(),
                returns: self.tcx.pool.nil(),
            })
            .into();
            return Err(MismatchedTypes::err(self.tcx.pool.add(expected), ty));
        };

        if args.len() > func_ty.params.len() {
            let last_param = func_ty
                .params
                .last()
                .ok_or_else(|| Box::new(CheckErr::TooManyArgs))?;
            let TypeKind::Variadic = self.tcx.pool[last_param.1].kind else {
                return Err(Box::new(CheckErr::TooManyArgs));
            };
        }

        for (arg_ty, (_, param_ty)) in args.iter().zip(&func_ty.params) {
            if !self.can_equal(*param_ty, *arg_ty) {
                return Err(MismatchedTypes::err(*param_ty, *arg_ty));
            }
        }

        // ensure remaining params can be skipped
        // should we allow skipping non-optional nil params?
        for (_, param_ty) in func_ty.params.iter().skip(args.len()) {
            if !self.can_equal(*param_ty, self.tcx.pool.nil()) {
                return Err(MismatchedTypes::err(*param_ty, self.tcx.pool.nil()));
            }
        }

        Ok(func_ty.returns)
    }

    fn check_binop(&mut self, binop: &ast::BinOp) -> TResult<TypeRef> {
        match binop.op {
            ast::OpKind::Add
            | ast::OpKind::Sub
            | ast::OpKind::Mul
            | ast::OpKind::Div
            | ast::OpKind::Mod
            | ast::OpKind::Pow => {
                let lhs_type = self.check_expr(binop.lhs)?;
                let rhs_type = self.check_expr(binop.rhs)?;
                if self.comm_eq(lhs_type, rhs_type) {
                    Ok(lhs_type)
                } else {
                    Err(MismatchedTypes::err(lhs_type, rhs_type))
                }
            }
            ast::OpKind::And | ast::OpKind::Or => {
                let lhs_type = self.check_expr(binop.lhs)?;
                let rhs_type = self.check_expr(binop.rhs)?;

                if self.comm_eq(lhs_type, rhs_type) {
                    Ok(lhs_type)
                } else {
                    Err(MismatchedTypes::err(lhs_type, rhs_type))
                }
            }
            ast::OpKind::Equ
            | ast::OpKind::Neq
            | ast::OpKind::Gre
            | ast::OpKind::Grq
            | ast::OpKind::Les
            | ast::OpKind::Leq => {
                let lhs_type = self.check_expr(binop.lhs)?;
                let rhs_type = self.check_expr(binop.rhs)?;

                if self.comm_eq(lhs_type, rhs_type) {
                    Ok(self.tcx.pool.boolean())
                } else {
                    Err(MismatchedTypes::err(lhs_type, rhs_type))
                }
            }
            ast::OpKind::Cat => {
                let lhs_type = self.check_expr(binop.lhs)?;
                let rhs_type = self.check_expr(binop.rhs)?;

                if self.can_equal(lhs_type, self.tcx.pool.string()) {
                    if self.can_equal(rhs_type, self.tcx.pool.string()) {
                        Ok(self.tcx.pool.string())
                    } else {
                        Err(MismatchedTypes::err(self.tcx.pool.string(), rhs_type))
                    }
                } else {
                    Err(MismatchedTypes::err(self.tcx.pool.string(), lhs_type))
                }
            }
        }
    }

    fn check_if_stmt(&mut self, if_stmt: &ast::IfStmt) -> TResult<()> {
        let condition_type = self.check_expr(if_stmt.condition)?;
        if !self.can_equal_primitive(condition_type, &TypeKind::Boolean) {
            return Err(MismatchedTypes::err(
                self.tcx.pool.boolean(),
                condition_type,
            ));
        }

        self.with_scope::<TResult<()>>(|this| {
            for stmt in &if_stmt.body {
                this.check_stmt(stmt)?;
            }

            Ok(())
        })?;

        match &if_stmt.else_ {
            Some(else_) => match else_.as_ref() {
                ast::ElseBranch::Else(body) => self.with_scope(|this| {
                    for stmt in body {
                        this.check_stmt(stmt)?;
                    }

                    Ok(())
                }),
                ast::ElseBranch::ElseIf(else_if_stmt) => self.check_if_stmt(else_if_stmt),
            },
            None => Ok(()),
        }
    }

    fn check_while_stmt(&mut self, while_stmt: &ast::WhileStmt) -> TResult<()> {
        let condition_type = self.check_expr(while_stmt.condition)?;
        if !self.can_equal_primitive(condition_type, &TypeKind::Boolean) {
            return Err(MismatchedTypes::err(
                self.tcx.pool.boolean(),
                condition_type,
            ));
        }

        self.with_scope(|this| {
            for stmt in &while_stmt.body {
                this.check_stmt(stmt)?;
            }

            Ok(())
        })
    }

    fn check_range_for(&mut self, range_for: &ast::RangeFor) -> TResult<()> {
        let lhs_type = self.check_expr(range_for.range.lhs)?;
        let rhs_type = self.check_expr(range_for.range.rhs)?;
        let number_type = self.tcx.pool.number();

        if !self.can_equal(lhs_type, number_type) {
            return Err(MismatchedTypes::err(number_type, lhs_type));
        }

        if !self.can_equal(rhs_type, number_type) {
            return Err(MismatchedTypes::err(number_type, lhs_type));
        }

        self.with_scope(|this| {
            this.tcx
                .value_map
                .insert_value(range_for.var.ident(this.file), this.tcx.pool.number());

            for stmt in &range_for.body {
                this.check_stmt(stmt)?;
            }

            Ok(())
        })
    }

    fn check_keyval_for(&mut self, keyval_for: &ast::KeyValFor) -> TResult<()> {
        let lhs_type = self.check_expr(keyval_for.iter)?;

        if let TypeKind::Table(TableType { key_type, val_type }) = self.tcx.pool[lhs_type].kind {
            self.with_scope(|this| {
                this.tcx
                    .value_map
                    .insert_value(keyval_for.key_name.ident(this.file), key_type);
                this.tcx
                    .value_map
                    .insert_value(keyval_for.val_name.ident(this.file), val_type);

                for stmt in &keyval_for.body {
                    this.check_stmt(stmt)?;
                }

                Ok(())
            })
        } else {
            Err(Box::new(CheckErr::NotIterable))
        }
    }
}

impl TypeChecker<'_> {
    fn resolve_type_node(&mut self, type_node: &ast::TypeNode) -> TResult<TypeRef> {
        match type_node {
            ast::TypeNode::Name(span) => self
                .tcx
                .value_map
                .get_type(&span.ident(self.file))
                .ok_or(Box::new(CheckErr::NoSuchVal(NoSuchVal { val_name: *span }))),
            ast::TypeNode::Nil(span) => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::Nil,
                span: Some(*span),
            })),
            ast::TypeNode::FunctionType(func_ty) => {
                let span = func_ty.span();
                let func_ty = self.resolve_function_type(func_ty)?;
                Ok(self.tcx.pool.add(Type {
                    kind: TypeKind::Function(func_ty),
                    span: Some(span),
                }))
            }
            ast::TypeNode::TableType(table_type) => self.resolve_table_type(table_type),
            ast::TypeNode::OptionalType(optional_ty) => {
                let span = optional_ty.span();
                let optional_ty = self.resolve_type_node(&optional_ty.inner)?;
                Ok(self.tcx.pool.add(Type {
                    kind: TypeKind::Optional(optional_ty),
                    span: Some(span),
                }))
            }
            ast::TypeNode::VariadicType(span) => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::Variadic,
                span: Some(*span),
            })),
        }
    }

    fn resolve_function_type(&mut self, function_type: &ast::FunctionType) -> TResult<Function> {
        let returns = match function_type.return_type.as_ref().map(AsRef::as_ref) {
            Some(ast::ReturnType::Single(ty)) => self.resolve_type_node(ty)?,
            Some(ast::ReturnType::Multiple(ast::MultipleType { types, span })) => {
                let types = types
                    .iter()
                    .map(|it| self.resolve_type_node(it))
                    .collect::<TRVec<_>>()?;

                self.tcx.pool.add(Type {
                    kind: TypeKind::Multiple(types),
                    span: Some(*span),
                })
            }
            None => self.tcx.pool.add(Type {
                kind: TypeKind::Nil,
                span: Some(Span::empty(function_type.header_span.end)),
            }),
        };

        Ok(Function {
            params: function_type
                .params
                .iter()
                .map(|it| Ok((it.name, self.resolve_type_node(&it.ty)?)))
                .collect::<TRVec<_>>()?,
            returns,
        })
    }

    fn resolve_table_type(&mut self, table_type: &ast::TableType) -> TResult<TypeRef> {
        let key_type = match &table_type.key_type {
            Some(key_type) => self.resolve_type_node(key_type.as_ref())?,
            None => self.tcx.pool.add(Type {
                kind: TypeKind::Number,
                span: Some(Span::new(
                    table_type.key_span.start + 1,
                    table_type.key_span.end - 1,
                )),
            }),
        };

        let val_type = self.resolve_type_node(table_type.val_type.as_ref())?;

        Ok(self.tcx.pool.add(Type {
            kind: TypeKind::Table(TableType { key_type, val_type }),
            span: Some(table_type.span()),
        }))
    }
}

impl TypeChecker<'_> {
    fn with_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.tcx.value_map.push_scope();
        let res = f(self);
        self.tcx.value_map.pop_scope();
        res
    }
}

impl TypeChecker<'_> {
    fn can_equal(&self, lhs: TypeRef, rhs: TypeRef) -> bool {
        let lhs = &self.tcx.pool[lhs];
        let rhs = &self.tcx.pool[rhs];
        if let TypeKind::Any | TypeKind::Variadic = lhs.kind {
            return true;
        }

        if let TypeKind::Adaptable = rhs.kind {
            return true;
        }

        if let TypeKind::Optional(base) = lhs.kind {
            let base = &self.tcx.pool[base];
            return std::mem::discriminant(&rhs.kind) == std::mem::discriminant(&base.kind)
                || std::mem::discriminant(&rhs.kind) == std::mem::discriminant(&TypeKind::Nil);
        }

        if let TypeKind::Optional(base) = rhs.kind {
            let base = &self.tcx.pool[base];
            return std::mem::discriminant(&lhs.kind) == std::mem::discriminant(&base.kind);
        }

        if std::mem::discriminant(&lhs.kind) != std::mem::discriminant(&rhs.kind) {
            return false;
        }

        if let TypeKind::Table(TableType {
            key_type: lhs_key,
            val_type: lhs_val,
        }) = lhs.kind
        {
            let TypeKind::Table(TableType {
                key_type: rhs_key,
                val_type: rhs_val,
            }) = rhs.kind
            else {
                unreachable!() // we know they have the same discriminant
            };

            return self.can_equal(lhs_key, rhs_key) && self.can_equal(lhs_val, rhs_val);
        }

        true
    }

    fn can_equal_primitive(&self, lhs: TypeRef, rhs: &TypeKind) -> bool {
        assert!(matches!(
            rhs,
            TypeKind::Nil
                | TypeKind::Any
                | TypeKind::Number
                | TypeKind::String
                | TypeKind::Boolean
                | TypeKind::Adaptable
                | TypeKind::Variadic
        ));

        let lhs = &self.tcx.pool[lhs];
        if let TypeKind::Any = lhs.kind {
            return true;
        }

        if let TypeKind::Adaptable = rhs {
            return true;
        }

        if let TypeKind::Optional(base) = lhs.kind {
            let base = &self.tcx.pool[base];
            return std::mem::discriminant(rhs) == std::mem::discriminant(&base.kind)
                || std::mem::discriminant(rhs) == std::mem::discriminant(&TypeKind::Nil);
        }

        if std::mem::discriminant(&lhs.kind) != std::mem::discriminant(rhs) {
            return false;
        }

        true
    }

    fn comm_eq(&self, lhs: TypeRef, rhs: TypeRef) -> bool {
        let lhs = &self.tcx.pool[lhs].kind;
        let rhs = &self.tcx.pool[rhs].kind;
        match (lhs, rhs) {
            (TypeKind::Adaptable, _) | (_, TypeKind::Adaptable) => return true,
            _ => (),
        }

        match (lhs, rhs) {
            (TypeKind::Optional(opt), other) | (other, TypeKind::Optional(opt)) => {
                let opt = &self.tcx.pool[*opt];
                if let TypeKind::Nil = other {
                    return true;
                }

                if std::mem::discriminant(other) == std::mem::discriminant(&opt.kind) {
                    return true;
                }

                return false;
            }
            _ => (),
        }

        if std::mem::discriminant(lhs) != std::mem::discriminant(rhs) {
            return false;
        }

        if let TypeKind::Table(TableType {
            key_type: lhs_key,
            val_type: lhs_val,
        }) = lhs
        {
            let TypeKind::Table(TableType {
                key_type: rhs_key,
                val_type: rhs_val,
            }) = rhs
            else {
                unreachable!() // we know they have the same discriminant
            };

            return self.comm_eq(*lhs_key, *rhs_key) && self.comm_eq(*lhs_val, *rhs_val);
        }

        true
    }
}
