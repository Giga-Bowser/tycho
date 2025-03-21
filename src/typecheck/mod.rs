mod error;

pub mod ctx;
pub mod func_ctx;
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
    typecheck::{error::*, func_ctx::FuncCtxStack, pool::TypePool, types::Param},
    utils::{spanned::Spanned, Ident, Span, Symbol},
};

use self::{
    ctx::TypeContext,
    error::{
        CheckErrKind, MethodOnWrongType, MismatchedTypes, NoReturn, NoSuchField, NoSuchMethod,
        NoSuchVal,
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
    pub func_ctx: FuncCtxStack,
}

impl<'a> TypeChecker<'a> {
    pub fn new(file: &'a SourceFile, tcx: &'a mut TypeContext, expr_pool: &'a ExprPool) -> Self {
        TypeChecker {
            file,
            tcx,
            expr_pool,
            func_ctx: FuncCtxStack::new(),
        }
    }

    pub(crate) fn into_diag_ctx(self) -> DiagCtx<'a> {
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
            ast::Stmt::Assign(assign) => self.check_assign(assign),
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
            ast::Stmt::Return(return_stmt) => self.check_return(return_stmt),
            ast::Stmt::Break(_) => Ok(()), // TODO: actually handle this
        }
        .map_err(|e| e.while_checking(self.expr_pool.wrap(stmt)))
    }

    fn check_decl(&mut self, decl: &ast::Declare) -> TResult<()> {
        match (decl.val, &decl.ty) {
            (Some(val), Some(type_node)) => {
                self.check_decl_with_both(decl.lhs.name, val, type_node)
            }
            (None, Some(type_node)) => self.check_decl_with_type(&decl.lhs, type_node),
            (Some(val), None) => self.check_decl_with_val(decl.lhs.name, val),
            (None, None) => unreachable!(),
        }
    }

    fn check_decl_with_val(&mut self, name: Span, val: ExprRef) -> TResult<()> {
        if let ast::Expr::Simple(ast::SimpleExpr::FuncNode(func_node)) = &self.expr_pool[val] {
            let func_ty = self.resolve_function_type(&func_node.ty, None)?;
            let func_ty = self.tcx.pool.add(Type {
                kind: TypeKind::Function(func_ty),
                span: Some(func_node.ty.span()),
            });

            self.tcx
                .value_map
                .insert_value(name.ident(self.file), func_ty);

            let val_type = self.check_expr(val)?;

            if !self.can_equal(func_ty, val_type) {
                return Err(MismatchedTypes::full(
                    func_ty,
                    &func_node.ty,
                    val_type,
                    self.expr_pool.wrap(val),
                ));
            }
        } else {
            let val_type = self.check_expr(val)?;

            self.tcx
                .value_map
                .insert_value(name.ident(self.file), val_type);
        }

        Ok(())
    }

    fn check_decl_with_type(
        &mut self,
        lhs: &ast::SuffixedName,
        type_node: &ast::TypeNode,
    ) -> TResult<()> {
        let decl_ty = self.resolve_type_node(type_node)?;

        if lhs.suffixes.is_empty() {
            self.tcx
                .value_map
                .insert_value(lhs.name.ident(self.file), decl_ty);
            return Ok(());
        }

        let mut ty = self
            .tcx
            .value_map
            .get_top(&lhs.name.ident(self.file))
            .ok_or_else(|| CheckErr::new(NoSuchVal { val_name: lhs.name }))?
            .inner();

        for suffix in &mut lhs.suffixes.iter().take(lhs.suffixes.len() - 1) {
            match suffix {
                ast::Suffix::Access(ast::Access { field_name: name }) => {
                    if let Some(field) = self.tcx.pool[ty].kind.get_field(name.symbol(self.file)) {
                        ty = field;
                    } else {
                        return Err(CheckErr::new(NoSuchField { field_name: *name }));
                    }
                }
                _ => unreachable!(),
            }
        }

        let last = lhs.suffixes.last().unwrap();

        if let ast::Suffix::Access(ast::Access { field_name }) = last {
            match &mut self.tcx.pool[ty].kind {
                TypeKind::Table(_) => Ok(()),
                TypeKind::Struct(strukt) => {
                    strukt.fields.push((field_name.symbol(self.file), decl_ty));
                    Ok(())
                }
                _ => Err(BadAccess::err(*field_name, ty)),
            }
        } else {
            unreachable!()
        }
    }

    fn check_decl_with_both(
        &mut self,
        name: Span,
        val: ExprRef,
        type_node: &ast::TypeNode,
    ) -> TResult<()> {
        let decl_ty = self.resolve_type_node(type_node)?;

        // We need to check for this so we can insert the function type early in case of recursive functions
        if let ast::Expr::Simple(ast::SimpleExpr::FuncNode(func_node)) = &self.expr_pool[val] {
            let func_ty = self.resolve_function_type(&func_node.ty, None)?;
            let func_ty = self.tcx.pool.add(Type {
                kind: TypeKind::Function(func_ty),
                span: Some(func_node.ty.span()),
            });

            if !self.can_equal(decl_ty, func_ty) {
                return Err(MismatchedTypes::full(
                    decl_ty,
                    type_node,
                    func_ty,
                    self.expr_pool.wrap(func_node.as_ref()),
                ));
            }

            self.tcx
                .value_map
                .insert_value(name.ident(self.file), func_ty);

            let val_type = self.check_expr(val)?;

            if !self.can_equal(func_ty, val_type) {
                return Err(MismatchedTypes::full(
                    func_ty,
                    self.expr_pool.wrap(func_node.as_ref()),
                    val_type,
                    self.expr_pool.wrap(val),
                ));
            }
        } else {
            let val_type = self.check_expr(val)?;

            if !self.can_equal(decl_ty, val_type) {
                return Err(MismatchedTypes::full(
                    decl_ty,
                    type_node,
                    val_type,
                    self.expr_pool.wrap(val),
                ));
            }

            self.tcx
                .value_map
                .insert_value(name.ident(self.file), decl_ty);
        }

        Ok(())
    }

    fn check_method_decl(&mut self, method_decl: &ast::MethodDecl) -> TResult<()> {
        let struct_ident = method_decl.struct_name.ident(self.file);
        let Some(self_ty) = self.tcx.value_map.get_type(&struct_ident) else {
            let kind: CheckErrKind = match self.tcx.value_map.get_value(&struct_ident) {
                Some(ty) => MethodOnVal {
                    span: Span::cover(method_decl.struct_name, method_decl.method_name),
                    ty,
                }
                .into(),
                None => NoSuchType {
                    ty_name: method_decl.struct_name,
                }
                .into(),
            };

            return Err(CheckErr::new(kind));
        };

        let TypeKind::Struct(_) = self.tcx.pool[self_ty].kind else {
            return Err(CheckErr::new(MethodOnWrongType {
                span: Span::cover(method_decl.struct_name, method_decl.method_name),
                ty: self_ty,
            }));
        };

        let func: &ast::FuncNode = &method_decl.func;
        let method_type = self.check_method_func(func, self_ty)?;

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
                let ty = TypePool::nil();
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

    fn check_assign(&mut self, assign: &ast::Assign) -> TResult<()> {
        let lhs_type = self.check_suffixed_name(&assign.lhs)?;
        let rhs_type = self.check_expr(assign.rhs)?;
        if self.can_equal(lhs_type, rhs_type) {
            Ok(())
        } else if let TypeKind::Adaptable = &self.tcx.pool[lhs_type].kind {
            let new_type = self.tcx.pool[rhs_type].clone();
            self.tcx.pool[lhs_type] = new_type;
            Ok(())
        } else {
            Err(MismatchedTypes::full(
                lhs_type,
                self.expr_pool.wrap(assign.lhs.as_ref()),
                rhs_type,
                self.expr_pool.wrap(assign.rhs),
            ))
        }
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
                TypePool::nil()
            };

            if !self.can_equal(expected, recieved) {
                return Err(CheckErr::new(
                    MismatchedTypes::new(expected, recieved)
                        .expected(self.expr_pool.wrap(suffixed_expr)),
                ));
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
                        Err(BadNegate::err(unop.op_span, res))
                    }
                }
                ast::UnOpKind::Len => Ok(self.tcx.pool.add(Type {
                    kind: TypeKind::Number,
                    span: None,
                })),
                ast::UnOpKind::Not => {
                    let res = self.check_expr(unop.val)?;
                    if self.can_equal(TypePool::boolean(), res)
                        || self.can_equal(TypePool::nil(), res)
                    {
                        Ok(res)
                    } else {
                        Err(BadNot::err(unop.op_span, res))
                    }
                }
            },
            ast::Expr::Paren(paren_expr) => self.check_expr(paren_expr.val),
            ast::Expr::Simple(simple_expr) => self.check_simple_expr(simple_expr),
            ast::Expr::Name(span) => self
                .tcx
                .value_map
                .get_value(&span.ident(self.file))
                .ok_or_else(|| CheckErr::new(NoSuchVal { val_name: *span })),
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
            ast::FieldNode::Field { .. } => Ok(TypePool::string()),
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
                result.key_type = TypePool::any();
                break;
            }
        }

        for field in table_node.fields.iter().skip(1) {
            let val = self.check_field_val(field)?;
            if !self.can_equal(result.val_type, val) {
                result.val_type = TypePool::any();
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
            ast::SimpleExpr::Str(_) => Ok(TypePool::string()),
            ast::SimpleExpr::Bool(s) => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::Boolean,
                span: Some(*s),
            })),
            ast::SimpleExpr::Nil(s) => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::Nil,
                span: Some(*s),
            })),
            ast::SimpleExpr::Variadic(s) => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::Variadic,
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

    /// returns the function type
    fn check_func(&mut self, func: &ast::FuncNode) -> TResult<TypeRef> {
        self.check_func_inner(func, None)
    }

    /// returns the function type
    fn check_method_func(&mut self, func: &ast::FuncNode, self_ty: TypeRef) -> TResult<TypeRef> {
        self.check_func_inner(func, Some(self_ty))
    }

    fn check_func_inner(
        &mut self,
        func: &ast::FuncNode,
        self_ty: Option<TypeRef>,
    ) -> TResult<TypeRef> {
        self.with_scope(|this| {
            if let Some(ty) = self_ty {
                this.tcx.value_map.insert_value(Ident::from_str("self"), ty);
            }

            let func_ty = this.resolve_function_type(&func.ty, self_ty)?;
            for param in &func_ty.params {
                let ident = if let TypeKind::Variadic = &this.tcx.pool[param.ty].kind {
                    Ident::from_str("...")
                } else if let Some(name) = param.name {
                    name.ident(this.file)
                } else {
                    continue;
                };
                this.tcx.value_map.insert_value(ident, param.ty);
            }

            this.func_ctx.push(func_ty.returns);

            for stmt in &func.body {
                this.check_stmt(stmt)?;
            }

            if this.needs_return(func_ty.returns) && !this.func_ctx.has_return() {
                return Err(CheckErr::new(NoReturn {
                    func_node: func.clone(),
                }));
            }

            this.func_ctx.pop();

            Ok(this.tcx.pool.add(TypeKind::Function(func_ty).into()))
        })
    }

    fn check_return(&mut self, return_stmt: &ast::ReturnStmt) -> TResult<()> {
        let ret_types = return_stmt
            .vals
            .iter()
            .map(|it| self.check_expr(*it))
            .collect::<TRVec<_>>()?;

        let ret_type = match ret_types.as_slice() {
            [] => TypePool::nil(),
            [single] => *single,
            _ => self.tcx.pool.add(TypeKind::Multiple(ret_types).into()),
        };

        if self.can_equal(self.func_ctx.ret_ty(), ret_type) {
            self.func_ctx.set_return(true);
            Ok(())
        } else {
            Err(CheckErr::new(
                MismatchedTypes::new(self.func_ctx.ret_ty(), ret_type)
                    .recieved(self.expr_pool.wrap(return_stmt)),
            ))
        }
    }

    fn check_suffixed_name(&mut self, suffixed_name: &ast::SuffixedName) -> TResult<TypeRef> {
        let mut ty = self
            .tcx
            .value_map
            .get_value(&suffixed_name.name.ident(self.file))
            .ok_or_else(|| {
                CheckErr::new(NoSuchVal {
                    val_name: suffixed_name.name,
                })
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
                    eprintln!("{:#}", self.tcx);
                    return Ok(TypePool::nil());
                }

                let res = self
                    .tcx
                    .value_map
                    .get(&name.ident(self.file))
                    .ok_or_else(|| {
                        CheckErr::new(NoSuchVal { val_name: *name })
                            .while_checking(self.expr_pool.wrap(suffixed_expr))
                    })?;

                if let Resolved::Type(_) = res {
                    let Some(ast::Suffix::Method(_) | ast::Suffix::Access(_)) =
                        suffixed_expr.suffixes.first()
                    else {
                        return Err(CheckErr::new(NoSuchVal { val_name: *name })
                            .while_checking(self.expr_pool.wrap(suffixed_expr)));
                    };
                }

                res.inner()
            }
            _ => self.check_expr(suffixed_expr.val)?,
        };

        for suffix in &suffixed_expr.suffixes {
            ty = self
                .check_suffix(ty, suffix)
                .map_err(|e| e.while_checking(self.expr_pool.wrap(suffixed_expr)))?;
        }

        Ok(ty)
    }

    fn check_suffix(&mut self, mut base: TypeRef, suffix: &ast::Suffix) -> TResult<TypeRef> {
        match suffix {
            ast::Suffix::Index(ast::Index { key: _, span }) => match &self.tcx.pool[base].kind {
                TypeKind::Table(TableType { val_type, .. }) => {
                    base = self.tcx.pool.add(TypeKind::Optional(*val_type).into());
                }
                TypeKind::Adaptable | TypeKind::Any => (),
                _ => {
                    return Err(BadIndex::err(*span, base));
                }
            },
            ast::Suffix::Access(ast::Access { field_name }) => match self.tcx.pool[base].kind {
                TypeKind::Struct(ref strukt) => {
                    if let Some(field) = strukt.get_field(field_name.symbol(self.file)) {
                        base = field;
                    } else {
                        return Err(CheckErr::new(NoSuchField {
                            field_name: *field_name,
                        }));
                    }
                }
                TypeKind::Table(TableType { key_type, val_type }) => {
                    if self.can_equal(TypePool::string(), key_type) {
                        base = val_type;
                    } else {
                        return Err(CheckErr::new(
                            MismatchedTypes::new(key_type, TypePool::string())
                                .recieved(*field_name),
                        ));
                    }
                }
                _ => {
                    return Err(BadAccess::err(*field_name, base));
                }
            },
            ast::Suffix::Call(ast::Call { args, span }) => {
                base = self.check_call(base, args, None, *span)?;
            }
            ast::Suffix::Method(ast::Method { method_name, args }) => {
                let self_ty = base;
                let method_str = method_name.to_str(self.file);
                if method_str == "new" {
                    return Ok(self_ty);
                }

                match self.tcx.pool[self_ty]
                    .kind
                    .get_field(Symbol::intern(method_str))
                {
                    Some(method) => base = method,
                    None => {
                        return Err(CheckErr::new(NoSuchMethod {
                            method_name: *method_name,
                        }))
                    }
                }

                base = self.check_call(base, args, Some(self_ty), *method_name)?;
            }
        }

        Ok(base)
    }

    fn check_call(
        &mut self,
        ty: TypeRef,
        args: &[ExprRef],
        self_ty: Option<TypeRef>,
        // for diagnostics
        call_span: Span,
    ) -> TResult<TypeRef> {
        let mut arg_types;
        if let Some(self_ty) = self_ty {
            arg_types = Vec::with_capacity(args.len() + 1);
            arg_types.push(self_ty);
        } else {
            arg_types = Vec::with_capacity(args.len());
        }

        for arg in args {
            arg_types.push(self.check_expr(*arg)?);
        }

        let func_ty = match &self.tcx.pool[ty].kind {
            TypeKind::Function(func_ty) => func_ty,
            TypeKind::Any | TypeKind::Adaptable => return Ok(ty),
            _ => return Err(CallNonFunc::err(ty, call_span)),
        };

        if arg_types.len() > func_ty.params.len() {
            let Some(last_param) = func_ty.params.last() else {
                return Err(ArgCount::err(
                    call_span,
                    func_ty.params.len(),
                    arg_types.len(),
                ));
            };
            let TypeKind::Variadic = self.tcx.pool[last_param.ty].kind else {
                return Err(ArgCount::err(
                    call_span,
                    func_ty.params.len(),
                    arg_types.len(),
                ));
            };
        }

        for (arg_ty, param) in arg_types.iter().zip(&func_ty.params) {
            if !self.can_equal(param.ty, *arg_ty) {
                return Err(CheckErr::new(MismatchedTypes::new(param.ty, *arg_ty)));
            }
        }

        // ensure remaining params can be skipped
        // should we allow skipping non-optional nil params?
        for param in func_ty.params.iter().skip(arg_types.len()) {
            if !self.can_equal(param.ty, TypePool::nil()) {
                // probably not great
                return Err(ArgCount::err(
                    call_span,
                    func_ty.params.len(),
                    arg_types.len(),
                ));
            }
        }

        Ok(func_ty.returns)
    }

    fn check_binop(&mut self, binop: &ast::BinOp) -> TResult<TypeRef> {
        let lhs_type = self.check_expr(binop.lhs)?;
        let rhs_type = self.check_expr(binop.rhs)?;
        if let ast::OpKind::Cat = binop.op {
            return if self.can_equal(TypePool::string(), lhs_type) {
                if self.can_equal(TypePool::string(), rhs_type) {
                    Ok(TypePool::string())
                } else {
                    Err(MismatchedTypes::full(
                        TypePool::string(),
                        binop.op_span(),
                        rhs_type,
                        self.expr_pool.wrap(binop.rhs),
                    ))
                }
            } else {
                Err(MismatchedTypes::full(
                    TypePool::string(),
                    binop.op_span(),
                    lhs_type,
                    self.expr_pool.wrap(binop.lhs),
                ))
            };
        }

        if !self.comm_eq(lhs_type, rhs_type) {
            return Err(MismatchedTypes::full(
                lhs_type,
                self.expr_pool.wrap(binop.lhs),
                rhs_type,
                self.expr_pool.wrap(binop.rhs),
            ));
        }

        match binop.op {
            ast::OpKind::Add
            | ast::OpKind::Sub
            | ast::OpKind::Mul
            | ast::OpKind::Div
            | ast::OpKind::Mod
            | ast::OpKind::Pow
            | ast::OpKind::Or => Ok(lhs_type),
            ast::OpKind::And => Ok(rhs_type),
            ast::OpKind::Equ
            | ast::OpKind::Neq
            | ast::OpKind::Gre
            | ast::OpKind::Grq
            | ast::OpKind::Les
            | ast::OpKind::Leq => Ok(TypePool::boolean()),
            ast::OpKind::Cat => unreachable!(),
        }
    }

    fn check_if_stmt(&mut self, if_stmt: &ast::IfStmt) -> TResult<()> {
        let condition_type = self.check_expr(if_stmt.condition)?;
        if !self.can_equal_primitive(condition_type, &TypeKind::Boolean) {
            if self.can_equal_primitive(condition_type, &TypeKind::Nil) {
            } else {
                return Err(CheckErr::new(
                    MismatchedTypes::new(TypePool::boolean(), condition_type)
                        .recieved(self.expr_pool.wrap(if_stmt.condition)),
                ));
            }
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
            return Err(CheckErr::new(
                MismatchedTypes::new(TypePool::boolean(), condition_type)
                    .recieved(self.expr_pool.wrap(while_stmt.condition)),
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
        let number_type = TypePool::number();

        if !self.can_equal(number_type, lhs_type) {
            return Err(MismatchedTypes::full(
                number_type,
                range_for.range.op_span,
                lhs_type,
                self.expr_pool.wrap(range_for.range.lhs),
            ));
        }

        if !self.can_equal(number_type, rhs_type) {
            return Err(MismatchedTypes::full(
                number_type,
                range_for.range.op_span,
                rhs_type,
                self.expr_pool.wrap(range_for.range.rhs),
            ));
        }

        self.with_scope(|this| {
            this.tcx
                .value_map
                .insert_value(range_for.var.ident(this.file), TypePool::number());

            for stmt in &range_for.body {
                this.check_stmt(stmt)?;
            }

            Ok(())
        })
    }

    fn check_keyval_for(&mut self, keyval_for: &ast::KeyValFor) -> TResult<()> {
        let iter_ty = self.check_expr(keyval_for.iter)?;

        if let TypeKind::Table(TableType { key_type, val_type }) = self.tcx.pool[iter_ty].kind {
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
            Err(NotIterable::err(iter_ty, keyval_for.iter))
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
                .ok_or_else(|| CheckErr::new(NoSuchVal { val_name: *span })),
            ast::TypeNode::Nil(span) => Ok(self.tcx.pool.add(Type {
                kind: TypeKind::Nil,
                span: Some(*span),
            })),
            ast::TypeNode::FunctionType(func_ty) => {
                let span = func_ty.span();
                let func_ty = self.resolve_function_type(func_ty, None)?;
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
            ast::TypeNode::ParenType(paren_type) => self.resolve_type_node(&paren_type.inner),
        }
    }

    fn resolve_function_type(
        &mut self,
        function_type: &ast::FunctionType,
        self_ty: Option<TypeRef>,
    ) -> TResult<Function> {
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
            None => TypePool::nil(),
        };

        let mut params = if let Some(self_ty) = self_ty {
            let mut v = Vec::with_capacity(function_type.params.len() + 1);
            v.push(Param {
                name: None,
                ty: self_ty,
            });
            v
        } else {
            Vec::with_capacity(function_type.params.len())
        };

        for param in &function_type.params {
            let res = match param {
                ast::Param::Named { name, ty } => Param {
                    name: Some(*name),
                    ty: self.resolve_type_node(ty)?,
                },
                ast::Param::Anon(ty) => Param {
                    name: None,
                    ty: self.resolve_type_node(ty)?,
                },
                ast::Param::Variadic(span) => Param {
                    name: None,
                    ty: self.tcx.pool.add(Type {
                        kind: TypeKind::Variadic,
                        span: Some(*span),
                    }),
                },
            };

            params.push(res);
        }

        Ok(Function { params, returns })
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
    fn can_equal(&self, expected: TypeRef, value: TypeRef) -> bool {
        let expected_kind = &self.tcx.pool[expected].kind;

        let mut value_kind = &self.tcx.pool[value].kind;
        if let TypeKind::Multiple(vals) = &value_kind {
            if let TypeKind::Multiple(exps) = &expected_kind {
                return exps
                    .iter()
                    .zip(vals.iter())
                    .all(|(exp, val)| self.can_equal(*exp, *val));
            }
            value_kind = match vals.first() {
                Some(ty) => &self.tcx.pool[*ty].kind,
                None => &TypeKind::Nil,
            };
        }

        if let TypeKind::Adaptable | TypeKind::Any | TypeKind::Variadic = expected_kind {
            return true;
        }

        if let TypeKind::Adaptable | TypeKind::Any | TypeKind::Variadic = value_kind {
            return true;
        }

        if let TypeKind::Optional(exp_base) = expected_kind {
            return self.can_equal(*exp_base, value) || self.can_equal(TypePool::nil(), value);
        }

        if let TypeKind::Optional(base) = value_kind {
            return self.can_equal(expected, *base) || self.can_equal(expected, TypePool::nil());
        }

        if std::mem::discriminant(expected_kind) != std::mem::discriminant(value_kind) {
            return false;
        }

        if let TypeKind::Table(TableType {
            key_type: lhs_key,
            val_type: lhs_val,
        }) = *expected_kind
        {
            let TypeKind::Table(TableType {
                key_type: rhs_key,
                val_type: rhs_val,
            }) = *value_kind
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

    fn comm_eq(&self, lhs_ref: TypeRef, rhs_ref: TypeRef) -> bool {
        let lhs = &self.tcx.pool[lhs_ref].kind;
        let rhs = &self.tcx.pool[rhs_ref].kind;
        match (lhs, rhs) {
            (TypeKind::Adaptable | TypeKind::Any, _) | (_, TypeKind::Adaptable | TypeKind::Any) => {
                return true
            }
            _ => (),
        }

        match (lhs, rhs) {
            (TypeKind::Optional(opt), _) => {
                return self.comm_eq(*opt, rhs_ref) || self.comm_eq(TypePool::nil(), rhs_ref)
            }
            (_, TypeKind::Optional(opt)) => {
                return self.comm_eq(lhs_ref, *opt) || self.comm_eq(TypePool::nil(), lhs_ref)
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

    fn needs_return(&self, return_ty: TypeRef) -> bool {
        match &self.tcx.pool[return_ty].kind {
            TypeKind::Nil
            | TypeKind::Any
            | TypeKind::Optional(_)
            | TypeKind::Variadic
            | TypeKind::Adaptable => false,

            TypeKind::Multiple(type_refs) => type_refs.iter().any(|it| self.needs_return(*it)),

            _ => true,
        }
    }
}
