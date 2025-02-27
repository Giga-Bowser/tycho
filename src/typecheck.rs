use crate::{
    errors::{
        CheckErr, MethodOnWrongType, MismatchedTypes, NoReturn, NoSuchField, NoSuchMethod,
        NoSuchVal, ReturnCount,
    },
    lexer::Span,
    parser::{
        ast,
        pool::{ExprPool, ExprRef},
    },
    type_env::TypeEnv,
    types::{
        pool::{TypePool, TypeRef},
        *,
    },
};

type TResult<'s, T> = Result<T, Box<CheckErr<'s>>>;

pub struct TypeChecker<'a, 's> {
    pub type_env: &'a mut TypeEnv<'s>,
    pub expr_pool: &'a ExprPool<'s>,
    pub type_pool: &'a mut TypePool<'s>,
    pub source: &'s str,
}

impl TypeChecker<'_, '_> {
    pub fn with_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        self.type_env.push_scope();
        let res = f(self);
        self.type_env.pop_scope();
        res
    }
}

impl<'s> TypeChecker<'_, 's> {
    fn can_equal(&self, lhs: TypeRef<'s>, rhs: TypeRef<'s>) -> bool {
        let lhs = &self.type_pool[lhs];
        let rhs = &self.type_pool[rhs];
        if let TypeKind::Any = lhs.kind {
            return true;
        }

        if let TypeKind::Adaptable = rhs.kind {
            return true;
        }

        if let TypeKind::Optional(base) = lhs.kind {
            let base = &self.type_pool[base];
            return std::mem::discriminant(&rhs.kind) == std::mem::discriminant(&base.kind)
                || std::mem::discriminant(&rhs.kind) == std::mem::discriminant(&TypeKind::Nil);
        }

        if let TypeKind::Optional(base) = rhs.kind {
            let base = &self.type_pool[base];
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

    fn can_equal_primitive(&self, lhs: TypeRef<'s>, rhs: &TypeKind<'s>) -> bool {
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

        let lhs = &self.type_pool[lhs];
        if let TypeKind::Any = lhs.kind {
            return true;
        }

        if let TypeKind::Adaptable = rhs {
            return true;
        }

        if let TypeKind::Optional(base) = lhs.kind {
            let base = &self.type_pool[base];
            return std::mem::discriminant(rhs) == std::mem::discriminant(&base.kind)
                || std::mem::discriminant(rhs) == std::mem::discriminant(&TypeKind::Nil);
        }

        if std::mem::discriminant(&lhs.kind) != std::mem::discriminant(rhs) {
            return false;
        }

        true
    }

    fn comm_eq(&self, lhs: TypeRef<'s>, rhs: TypeRef<'s>) -> bool {
        let lhs = &self.type_pool[lhs].kind;
        let rhs = &self.type_pool[rhs].kind;
        match (lhs, rhs) {
            (TypeKind::Adaptable, _) | (_, TypeKind::Adaptable) => return true,
            _ => (),
        }

        match (lhs, rhs) {
            (TypeKind::Optional(opt), other) | (other, TypeKind::Optional(opt)) => {
                let opt = &self.type_pool[*opt];
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

impl<'s> TypeChecker<'_, 's> {
    pub fn check_statement(&mut self, stmt: &ast::Statement<'s>) -> TResult<'s, ()> {
        match stmt {
            ast::Statement::Declare(decl) => self.check_decl(decl),
            ast::Statement::MethodDecl(method_decl) => self.check_method_decl(method_decl),
            ast::Statement::MultiDecl(multi_decl) => self.check_multi_decl(multi_decl),
            ast::Statement::IfStat(if_stat) => self.check_if_stat(if_stat),
            ast::Statement::WhileStat(while_stat) => self.check_while_stat(while_stat),
            ast::Statement::RangeFor(range_for) => self.check_range_for(range_for),
            ast::Statement::KeyValFor(keyval_for) => self.check_keyval_for(keyval_for),
            ast::Statement::Assign(ast::Assign { lhs, rhs }) => {
                let lhs_type = self.check_suffixed_name(lhs)?;
                let rhs_type = self.check_expr(*rhs)?;
                if self.can_equal(lhs_type, rhs_type) {
                    Ok(())
                } else {
                    Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })))
                }
            }
            ast::Statement::MultiAssign(multi_assign) => self.check_multi_assign(multi_assign),
            // TODO: investigate if making a more expression-statement specific checking function is worth it
            ast::Statement::ExprStat(suffixed_expr) => {
                self.check_suffixed_expr(suffixed_expr).map(drop)
            }
            ast::Statement::Block(block) => self.with_scope(|this| {
                for stmt in block {
                    this.check_statement(stmt)?;
                }
                Ok(())
            }),
            ast::Statement::StructDecl(ast::StructDecl {
                name,
                ty,
                constructor: _,
            }) => {
                self.type_env.insert(
                    (*name).to_str(self.source),
                    self.type_pool.add(Type {
                        kind: TypeKind::User(*ty.clone()),
                        span: None,
                    }),
                );
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn check_decl(&mut self, decl: &ast::Declare<'s>) -> TResult<'s, ()> {
        if let Some(val) = decl.val {
            let lhs_type = self.check_expr(val)?;

            if let TypeKind::Adaptable = self.type_pool[decl.ty].kind {
                self.type_env
                    .insert(decl.lhs.name.to_str(self.source), lhs_type);
                return Ok(());
            }

            if !self.can_equal(decl.ty, lhs_type) {
                return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                    expected: decl.ty,
                    recieved: lhs_type,
                })));
            }

            self.type_env
                .insert(decl.lhs.name.to_str(self.source), decl.ty);
            Ok(())
        } else {
            if decl.lhs.suffixes.is_empty() {
                self.type_env
                    .insert(decl.lhs.name.to_str(self.source), decl.ty);
                return Ok(());
            }

            let mut ty = self
                .type_env
                .get_top(decl.lhs.name.to_str(self.source))
                .unwrap();

            for suffix in &mut decl.lhs.suffixes.iter().take(decl.lhs.suffixes.len() - 1) {
                match suffix {
                    ast::Suffix::Access(ast::Access { field_name: name }) => {
                        if let Some(field) =
                            self.type_pool[ty].kind.get_field(name.to_str(self.source))
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
                match &mut self.type_pool[ty].kind {
                    TypeKind::Table(_) => Ok(()),
                    TypeKind::User(user) => {
                        user.fields.push((field_name.to_str(self.source), decl.ty));
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

    fn check_method_decl(&mut self, method_decl: &ast::MethodDecl<'s>) -> TResult<'s, ()> {
        let ty = self
            .type_env
            .get(method_decl.struct_name.to_str(self.source))
            .unwrap();

        let TypeKind::User(_) = self.type_pool[ty].kind else {
            return Err(Box::new(CheckErr::MethodOnWrongType(MethodOnWrongType {
                span: Span::cover(method_decl.struct_name, method_decl.method_name),
                ty,
            })));
        };

        let func: &ast::FuncNode<'s> = &method_decl.func;
        let method_type = self.with_scope(|this| {
            this.type_env.insert("self", ty);
            for (name, ty) in &func.ty.params {
                this.type_env.insert(name, *ty);
            }

            if let TypeKind::Nil = this.type_pool[func.ty.returns].kind {
                // TODO: this is obviously bad
                return Ok(this
                    .type_pool
                    .add(TypeKind::Function(*func.ty.clone()).into()));
            }

            let has_return = this.check_func_body(&func.body, func.ty.returns)?;

            if !has_return {
                return Err(Box::new(CheckErr::NoReturn(NoReturn {
                    func_node: func.clone(),
                })));
            }

            Ok(this
                .type_pool
                .add(TypeKind::Function(*func.ty.clone()).into()))
        })?;

        let TypeKind::User(User { fields, name: _ }) = &mut self.type_pool[self
            .type_env
            .get(method_decl.struct_name.to_str(self.source))
            .unwrap()]
        .kind
        else {
            unreachable!()
        };

        fields.push((method_decl.method_name.to_str(self.source), method_type));

        Ok(())
    }

    fn check_multi_decl(&mut self, multi_decl: &ast::MultiDecl<'s>) -> TResult<'s, ()> {
        // ok so we need to basically flatten the types. so for example
        // a, b, c := twoReturnFunction(), oneReturnFunction()
        // a, b, c := (type1, type2), type3
        // a, b, c := type1, type2, type3
        let mut types = Vec::new();

        for rhs in &multi_decl.rhs_arr {
            let expr_type = self.check_expr(*rhs)?;
            if let TypeKind::Multiple(mult) = &self.type_pool[expr_type].kind {
                types.extend(mult);
            } else {
                types.push(expr_type);
            }
        }

        for (i, name) in multi_decl.lhs_arr.iter().enumerate() {
            if i < types.len() {
                self.type_env.insert(name.to_str(self.source), types[i]);
            } else {
                let ty = self.type_pool.add(Type {
                    kind: TypeKind::Nil,
                    span: None,
                });
                self.type_env.insert(name.to_str(self.source), ty);
            }
        }

        Ok(())
    }

    fn check_multi_assign(&mut self, multi_assign: &ast::MultiAssign<'s>) -> TResult<'s, ()> {
        // ok so we need to basically flatten the types. so for example
        // a, b, c := twoReturnFunction(), oneReturnFunction()
        // a, b, c := (type1, type2), type3
        // a, b, c := type1, type2, type3
        let mut types = Vec::new();

        for rhs in &multi_assign.rhs_arr {
            let rhs_type = self.check_expr(*rhs)?;
            if let TypeKind::Multiple(mult) = &self.type_pool[rhs_type].kind {
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
                self.type_pool.add(Type {
                    kind: TypeKind::Nil,
                    span: None,
                })
            };

            if !self.can_equal(expected, recieved) {
                return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                    expected,
                    recieved,
                })));
            }
        }

        Ok(())
    }

    fn check_expr(&mut self, expr: ExprRef) -> TResult<'s, TypeRef<'s>> {
        match &self.expr_pool[expr] {
            ast::Expr::BinOp(binop) => self.check_binop(binop),
            ast::Expr::UnOp(unop) => match unop.op {
                ast::UnOpKind::Neg => {
                    let res = self.check_expr(unop.val)?;
                    if let TypeKind::Number = &self.type_pool[res].kind {
                        Ok(res)
                    } else {
                        Err(Box::new(CheckErr::BadNegate {
                            op_span: unop.op_span,
                            ty: res,
                        }))
                    }
                }
                ast::UnOpKind::Len => Ok(self.type_pool.add(Type {
                    kind: TypeKind::Number,
                    span: None,
                })),
                ast::UnOpKind::Not => {
                    let res = self.check_expr(unop.val)?;
                    if let TypeKind::Boolean = self.type_pool[res].kind {
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
                .type_env
                .get(span.to_str(self.source))
                .ok_or_else(|| Box::new(CheckErr::NoSuchVal(NoSuchVal { val_name: *span }))),
        }
    }

    fn check_field(&mut self, field_node: &ast::FieldNode<'s>) -> TResult<'s, TableType<'s>> {
        Ok(TableType {
            key_type: self.check_field_key(field_node)?,
            val_type: self.check_field_val(field_node)?,
        })
    }

    fn check_field_key(&mut self, field_node: &ast::FieldNode<'s>) -> TResult<'s, TypeRef<'s>> {
        match field_node {
            ast::FieldNode::Field { key, .. } => Ok(self.type_pool.add(Type {
                kind: TypeKind::String,
                span: Some(*key),
            })),
            ast::FieldNode::ExprField { key, .. } => self.check_expr(*key),
            ast::FieldNode::ValField { .. } => Ok(self.type_pool.add(Type {
                kind: TypeKind::Number,
                span: None,
            })),
        }
    }

    fn check_field_val(&mut self, field_node: &ast::FieldNode<'s>) -> TResult<'s, TypeRef<'s>> {
        match field_node {
            ast::FieldNode::Field { val, .. }
            | ast::FieldNode::ExprField { val, .. }
            | ast::FieldNode::ValField { val } => self.check_expr(*val),
        }
    }

    fn check_table(&mut self, table_node: &ast::TableNode<'s>) -> TResult<'s, TableType<'s>> {
        if table_node.fields.is_empty() {
            return Ok(TableType {
                key_type: self.type_pool.add(TypeKind::Adaptable.into()),
                val_type: self.type_pool.add(TypeKind::Adaptable.into()),
            });
        }

        let mut result = self.check_field(&table_node.fields[0])?;

        for field in table_node.fields.iter().skip(1) {
            let key = self.check_field_key(field)?;
            if !self.can_equal(result.key_type, key) {
                result.key_type = self.type_pool.any();
                break;
            }
        }

        for field in table_node.fields.iter().skip(1) {
            let val = self.check_field_val(field)?;
            if !self.can_equal(result.val_type, val) {
                result.val_type = self.type_pool.any();
                break;
            }
        }

        Ok(result)
    }

    fn check_simple_expr(&mut self, simple_expr: &ast::SimpleExpr<'s>) -> TResult<'s, TypeRef<'s>> {
        match simple_expr {
            ast::SimpleExpr::Num(s) => Ok(self.type_pool.add(Type {
                kind: TypeKind::Number,
                span: Some(*s),
            })),
            ast::SimpleExpr::Str(s) => Ok(self.type_pool.add(Type {
                kind: TypeKind::String,
                span: Some(*s),
            })),
            ast::SimpleExpr::Bool(s) => Ok(self.type_pool.add(Type {
                kind: TypeKind::Boolean,
                span: Some(*s),
            })),
            ast::SimpleExpr::Nil(s) => Ok(self.type_pool.add(Type {
                kind: TypeKind::Nil,
                span: Some(*s),
            })),
            ast::SimpleExpr::FuncNode(func) => self.check_func(func),
            ast::SimpleExpr::TableNode(table_node) => {
                let table_type = self.check_table(table_node)?;
                Ok(self.type_pool.add(TypeKind::Table(table_type).into()))
            }
            ast::SimpleExpr::SuffixedExpr(suffixed_expr) => self.check_suffixed_expr(suffixed_expr),
        }
    }

    fn check_func(&mut self, func: &ast::FuncNode<'s>) -> TResult<'s, TypeRef<'s>> {
        self.with_scope(|this| {
            for (name, ty) in &func.ty.params {
                this.type_env.insert(name, *ty);
            }

            if let TypeKind::Nil = this.type_pool[func.ty.returns].kind {
                // TODO: this is obviously bad
                return Ok(this
                    .type_pool
                    .add(TypeKind::Function(*func.ty.clone()).into()));
            }

            let has_return = this.check_func_body(&func.body, func.ty.returns)?;

            if !has_return {
                return Err(Box::new(CheckErr::NoReturn(NoReturn {
                    func_node: func.clone(),
                })));
            }

            Ok(this
                .type_pool
                .add(TypeKind::Function(*func.ty.clone()).into()))
        })
    }

    fn check_func_body(
        &mut self,
        body: &[ast::Statement<'s>],
        return_type: TypeRef<'s>,
    ) -> TResult<'s, bool> {
        self.with_scope(|this| {
            for stmt in body {
                match stmt {
                    ast::Statement::IfStat(ast::IfStat { body: if_body, .. }) => {
                        if this.check_func_body(if_body, return_type)? {
                            return Ok(true);
                        }
                    }
                    ast::Statement::WhileStat(ast::WhileStat {
                        body: while_body, ..
                    }) => {
                        if this.check_func_body(while_body, return_type)? {
                            return Ok(true);
                        }
                    }
                    ast::Statement::RangeFor(ast::RangeFor { body, .. }) => {
                        if this.check_func_body(body, return_type)? {
                            return Ok(true);
                        }
                    }
                    ast::Statement::KeyValFor(ast::KeyValFor { body, .. }) => {
                        if this.check_func_body(body, return_type)? {
                            return Ok(true);
                        }
                    }
                    ast::Statement::Return(node @ ast::ReturnStmt { vals, .. }) => {
                        if vals.is_empty() {
                            return match &this.type_pool[return_type].kind {
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
                            .collect::<TResult<'s, Vec<_>>>()?;

                        if let TypeKind::Multiple(types) = &this.type_pool[return_type].kind {
                            if types.len() != vals.len() {
                                return Err(Box::new(CheckErr::ReturnCount(ReturnCount {
                                    return_node: node.clone(),
                                    expected: types.len(),
                                })));
                            }

                            for (lhs, rhs) in types.iter().zip(returned_types.iter()) {
                                if !this.can_equal(*lhs, *rhs) {
                                    return Err(Box::new(CheckErr::MismatchedTypes(
                                        MismatchedTypes {
                                            expected: *lhs,
                                            recieved: *rhs,
                                        },
                                    )));
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
                                Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                                    expected: return_type,
                                    recieved: return_val,
                                })))
                            };
                        }

                        return Ok(true);
                    }
                    _ => (),
                }
                this.check_statement(stmt)?;
            }

            Ok(false)
        })
    }

    fn check_suffixed_name(
        &mut self,
        suffixed_name: &ast::SuffixedName<'s>,
    ) -> TResult<'s, TypeRef<'s>> {
        let mut ty = self
            .type_env
            .get(suffixed_name.name.to_str(self.source))
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

    fn check_suffixed_expr(
        &mut self,
        suffixed_expr: &ast::SuffixedExpr<'s>,
    ) -> TResult<'s, TypeRef<'s>> {
        let mut ty = self.check_expr(suffixed_expr.val)?;

        for suffix in &suffixed_expr.suffixes {
            ty = self.check_suffix(ty, suffix)?;
        }

        Ok(ty)
    }

    fn check_suffix(
        &mut self,
        mut base: TypeRef<'s>,
        suffix: &ast::Suffix<'s>,
    ) -> TResult<'s, TypeRef<'s>> {
        match suffix {
            ast::Suffix::Index(ast::Index { key: _, span }) => match &self.type_pool[base].kind {
                TypeKind::Table(TableType { val_type, .. }) => base = *val_type,
                TypeKind::String | TypeKind::Adaptable | TypeKind::Any => (),
                _ => {
                    return Err(Box::new(CheckErr::BadIndex {
                        span: *span,
                        ty: base,
                    }));
                }
            },
            ast::Suffix::Access(ast::Access { field_name }) => match self.type_pool[base].kind {
                TypeKind::User(ref user) => {
                    if let Some(field) = user.get_field(field_name.to_str(self.source)) {
                        base = field;
                    } else {
                        return Err(Box::new(CheckErr::NoSuchField(NoSuchField {
                            field_name: *field_name,
                        })));
                    }
                }
                TypeKind::Table(TableType { key_type, val_type }) => {
                    let string_ty = self.type_pool.string();
                    if self.can_equal(string_ty, key_type) {
                        base = val_type;
                    } else {
                        return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                            expected: self.type_pool.add(Type {
                                kind: TypeKind::String,
                                span: Some(*field_name),
                            }),
                            recieved: key_type,
                        })));
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
                for arg in args {
                    self.check_expr(*arg)?;
                }

                if let TypeKind::Function(func_type) = &self.type_pool[base].kind {
                    base = func_type.returns;
                } else if let TypeKind::Adaptable = self.type_pool[base].kind {
                    return Ok(base);
                } else {
                    // TODO: should probably be it's own error
                    let expected = TypeKind::Function(Function {
                        params: Vec::new(),
                        returns: self.type_pool.nil(),
                    })
                    .into();
                    return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                        expected: self.type_pool.add(expected),
                        recieved: base,
                    })));
                }
            }
            ast::Suffix::Method(ast::Method { method_name, .. }) => {
                let method_str = method_name.to_str(self.source);
                if method_str == "new" {
                    return Ok(base);
                }

                match self.type_pool[base].kind.get_field(method_str) {
                    Some(method) => base = method,
                    None => {
                        return Err(Box::new(CheckErr::NoSuchMethod(NoSuchMethod {
                            method_name: *method_name,
                        })))
                    }
                }
            }
        }

        Ok(base)
    }

    fn check_binop(&mut self, binop: &ast::BinOp) -> TResult<'s, TypeRef<'s>> {
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
                    Ok(self.type_pool.add(TypeKind::Adaptable.into()))
                }
            }
            ast::OpKind::And | ast::OpKind::Or => {
                let lhs_type = self.check_expr(binop.lhs)?;
                let rhs_type = self.check_expr(binop.rhs)?;

                if self.comm_eq(lhs_type, rhs_type) {
                    Ok(lhs_type)
                } else {
                    Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })))
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
                    Ok(self.type_pool.boolean())
                } else {
                    Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })))
                }
            }
            // TODO: typecheck this
            ast::OpKind::Cat => Ok(self.type_pool.string()),
        }
    }

    fn check_if_stat(&mut self, if_stat: &ast::IfStat<'s>) -> TResult<'s, ()> {
        let condition_type = self.check_expr(if_stat.condition)?;
        if !self.can_equal_primitive(condition_type, &TypeKind::Boolean) {
            return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                expected: self.type_pool.boolean(),
                recieved: condition_type,
            })));
        }

        self.with_scope::<TResult<'s, ()>>(|this| {
            for stmt in &if_stat.body {
                this.check_statement(stmt)?;
            }

            Ok(())
        })?;

        match &if_stat.else_ {
            Some(else_) => match else_.as_ref() {
                ast::ElseBranch::Else(body) => self.with_scope(|this| {
                    for stmt in body {
                        this.check_statement(stmt)?;
                    }

                    Ok(())
                }),
                ast::ElseBranch::ElseIf(else_if_stat) => self.check_if_stat(else_if_stat),
            },
            None => Ok(()),
        }
    }

    fn check_while_stat(&mut self, while_stat: &ast::WhileStat<'s>) -> TResult<'s, ()> {
        let condition_type = self.check_expr(while_stat.condition)?;
        if !self.can_equal_primitive(condition_type, &TypeKind::Boolean) {
            return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                expected: self.type_pool.boolean(),
                recieved: condition_type,
            })));
        }

        self.with_scope(|this| {
            for stmt in &while_stat.body {
                this.check_statement(stmt)?;
            }

            Ok(())
        })
    }

    fn check_range_for(&mut self, range_for: &ast::RangeFor<'s>) -> TResult<'s, ()> {
        let lhs_type = self.check_expr(range_for.range.lhs)?;
        let rhs_type = self.check_expr(range_for.range.rhs)?;
        let number_type = self.type_pool.number();

        if !self.can_equal(lhs_type, number_type) {
            return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                expected: number_type,
                recieved: lhs_type,
            })));
        }

        if !self.can_equal(rhs_type, number_type) {
            return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                expected: number_type,
                recieved: lhs_type,
            })));
        }

        self.with_scope(|this| {
            this.type_env
                .insert(range_for.var.to_str(this.source), this.type_pool.number());

            for stmt in &range_for.body {
                this.check_statement(stmt)?;
            }

            Ok(())
        })
    }

    fn check_keyval_for(&mut self, keyval_for: &ast::KeyValFor<'s>) -> TResult<'s, ()> {
        // now because i'm evil, we have to retokenize the `key, val` string_view
        // but who cares.
        let names = keyval_for.names.to_str(self.source).as_bytes();

        let mut i = 0;
        while names[i] != b',' {
            i += 1;
        }

        let key_name = unsafe { std::str::from_utf8_unchecked(&names[0..i]) };

        // now we skip the comma
        i += 1;

        while names[i].is_ascii_whitespace() {
            i += 1;
        }

        let val_name = unsafe { std::str::from_utf8_unchecked(&names[i..]) };

        let lhs_type = self.check_expr(keyval_for.iter)?;

        if let TypeKind::Table(TableType { key_type, val_type }) = self.type_pool[lhs_type].kind {
            self.with_scope(|this| {
                this.type_env.insert(key_name, key_type);
                this.type_env.insert(val_name, val_type);

                for stmt in &keyval_for.body {
                    this.check_statement(stmt)?;
                }

                Ok(())
            })
        } else {
            Err(Box::new(CheckErr::NotIterable))
        }
    }
}
