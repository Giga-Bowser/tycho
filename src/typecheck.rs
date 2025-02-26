use std::rc::Rc;

use crate::{
    errors::{
        CheckErr, MethodOnWrongType, MismatchedTypes, NoReturn, NoSuchField, NoSuchMethod,
        NoSuchVal, ReturnCount,
    },
    lexer::Span,
    parser::{
        ast::*,
        pool::{ExprPool, ExprRef},
    },
    type_env::TypeEnv,
    types::*,
};

type TResult<'s, T> = Result<T, Box<CheckErr<'s>>>;

pub struct TypeChecker<'a, 's> {
    pub type_env: &'a mut TypeEnv<'s>,
    pub pool: &'a ExprPool<'s>,
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
    pub fn check_statement(&mut self, stmt: &Statement<'s>) -> TResult<'s, ()> {
        match stmt {
            Statement::Declare(decl) => self.check_decl(decl),
            Statement::MethodDecl(method_decl) => self.check_method_decl(method_decl),
            Statement::MultiDecl(multi_decl) => self.check_multi_decl(multi_decl),
            Statement::IfStat(if_stat) => self.check_if_stat(if_stat),
            Statement::WhileStat(while_stat) => self.check_while_stat(while_stat),
            Statement::RangeFor(range_for) => self.check_range_for(range_for),
            Statement::KeyValFor(keyval_for) => self.check_keyval_for(keyval_for),
            Statement::Assign(Assign { lhs, rhs }) => {
                let lhs_type = self.check_suffixed_name(lhs)?;
                let rhs_type = self.check_expr(*rhs)?;
                if lhs_type.can_equal(&rhs_type) {
                    Ok(())
                } else {
                    Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })))
                }
            }
            Statement::MultiAssign(multi_assign) => self.check_multi_assign(multi_assign),
            // TODO: investigate if making a more expression-statement specific checking function is worth it
            Statement::ExprStat(suffixed_expr) => self.check_suffixed_expr(suffixed_expr).map(drop),
            Statement::Block(block) => self.with_scope(|this| {
                for stmt in block {
                    this.check_statement(stmt)?;
                }
                Ok(())
            }),
            Statement::StructDecl(StructDecl {
                name,
                type_,
                constructor: _,
            }) => {
                self.type_env.insert(
                    (*name).to_str(self.source),
                    Type {
                        kind: TypeKind::User(*type_.clone()),
                        span: None,
                    },
                );
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn check_decl(&mut self, decl: &Declare<'s>) -> TResult<'s, ()> {
        if let Some(val) = decl.val {
            let lhs_type = self.check_expr(val)?;

            if let TypeKind::Adaptable = decl.type_.kind {
                self.type_env
                    .insert(decl.lhs.name.to_str(self.source), lhs_type);
                return Ok(());
            }

            if !decl.type_.can_equal(&lhs_type) {
                return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                    expected: decl.type_.as_ref().clone(),
                    recieved: lhs_type,
                })));
            }

            self.type_env
                .insert(decl.lhs.name.to_str(self.source), *decl.type_.clone());
            Ok(())
        } else {
            if decl.lhs.suffixes.is_empty() {
                self.type_env
                    .insert(decl.lhs.name.to_str(self.source), *decl.type_.clone());
                return Ok(());
            }

            let mut ty = self
                .type_env
                .get_mut(decl.lhs.name.to_str(self.source))
                .unwrap();

            for suffix in &mut decl.lhs.suffixes.iter().take(decl.lhs.suffixes.len() - 1) {
                match suffix {
                    Suffix::Access(Access { field_name: name }) => {
                        if let Some(field) = ty.kind.get_field_mut(name.to_str(self.source)) {
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

            if let Suffix::Access(Access { field_name }) = last {
                match &mut ty.kind {
                    TypeKind::Table(_) => Ok(()),
                    TypeKind::User(user) => {
                        user.fields
                            .push((field_name.to_str(self.source), *decl.type_.clone()));
                        Ok(())
                    }
                    _ => Err(Box::new(CheckErr::BadAccess {
                        span: *field_name,
                        ty: ty.clone(),
                    })),
                }
            } else {
                unreachable!()
            }
        }
    }

    fn check_method_decl(&mut self, method_decl: &MethodDecl<'s>) -> TResult<'s, ()> {
        let ty = self
            .type_env
            .get(method_decl.struct_name.to_str(self.source))
            .unwrap()
            .clone();

        let TypeKind::User(_) = ty.kind else {
            return Err(Box::new(CheckErr::MethodOnWrongType(MethodOnWrongType {
                span: Span::cover(method_decl.struct_name, method_decl.method_name),
                ty: ty.clone(),
            })));
        };

        let func: &FuncNode<'s> = &method_decl.func;
        let method_type = self.with_scope(|this| {
            this.type_env.insert("self", ty);
            for (name, type_) in &func.type_.params {
                this.type_env.insert(name, type_.clone());
            }

            if let TypeKind::Nil = func.type_.returns.kind {
                // TODO: this is obviously bad
                return Ok(TypeKind::Function(*func.type_.clone()).into());
            }

            let has_return = this.check_func_body(&func.body, func.type_.returns.as_ref())?;

            if !has_return {
                return Err(Box::new(CheckErr::NoReturn(NoReturn {
                    func_node: func.clone(),
                })));
            }

            Ok(TypeKind::Function(*func.type_.clone()).into())
        })?;

        let TypeKind::User(User { fields, name: _ }) = &mut self
            .type_env
            .get_mut(method_decl.struct_name.to_str(self.source))
            .unwrap()
            .kind
        else {
            unreachable!()
        };

        fields.push((method_decl.method_name.to_str(self.source), method_type));

        Ok(())
    }

    fn check_multi_decl(&mut self, multi_decl: &MultiDecl<'s>) -> TResult<'s, ()> {
        // ok so we need to basically flatten the types. so for example
        // a, b, c := twoReturnFunction(), oneReturnFunction()
        // a, b, c := (type1, type2), type3
        // a, b, c := type1, type2, type3
        let mut types = Vec::new();

        for rhs in &multi_decl.rhs_arr {
            let expr_type = self.check_expr(*rhs)?;
            if let TypeKind::Multiple(mult) = expr_type.kind {
                types.extend(mult);
            } else {
                types.push(expr_type);
            }
        }

        for (i, name) in multi_decl.lhs_arr.iter().enumerate() {
            if i < types.len() {
                self.type_env
                    .insert(name.to_str(self.source), types[i].clone());
            } else {
                self.type_env.insert(
                    name.to_str(self.source),
                    Type {
                        kind: TypeKind::Nil,
                        span: None,
                    },
                );
            }
        }

        Ok(())
    }

    fn check_multi_assign(&mut self, multi_assign: &MultiAssign<'s>) -> TResult<'s, ()> {
        // ok so we need to basically flatten the types. so for example
        // a, b, c := twoReturnFunction(), oneReturnFunction()
        // a, b, c := (type1, type2), type3
        // a, b, c := type1, type2, type3
        let mut types = Vec::new();

        for rhs in &multi_assign.rhs_arr {
            let rhs_type = self.check_expr(*rhs)?;
            if let TypeKind::Multiple(mult) = rhs_type.kind {
                types.extend(mult);
            } else {
                types.push(rhs_type);
            }
        }

        for (idx, suffixed_expr) in multi_assign.lhs_arr.iter().enumerate() {
            let expected = self.check_suffixed_expr(suffixed_expr)?;
            let recieved = if idx < types.len() {
                &types[idx]
            } else {
                &Type {
                    kind: TypeKind::Nil,
                    span: None,
                }
            };

            if !expected.can_equal(recieved) {
                return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                    expected,
                    recieved: recieved.clone(),
                })));
            }
        }

        Ok(())
    }

    fn check_expr(&mut self, expr: ExprRef) -> TResult<'s, Type<'s>> {
        match &self.pool[expr] {
            Expr::BinOp(binop) => self.check_binop(binop),
            Expr::UnOp(unop) => match unop.op {
                UnOpKind::Neg => {
                    let res = self.check_expr(unop.val)?;
                    if let TypeKind::Number = res.kind {
                        Ok(res)
                    } else {
                        Err(Box::new(CheckErr::BadNegate {
                            op_span: unop.op_span,
                            ty: res,
                        }))
                    }
                }
                UnOpKind::Len => Ok(Type {
                    kind: TypeKind::Number,
                    span: None,
                }),
                UnOpKind::Not => {
                    let res = self.check_expr(unop.val)?;
                    if let TypeKind::Boolean = res.kind {
                        Ok(res)
                    } else {
                        Err(Box::new(CheckErr::BadNot {
                            op_span: unop.op_span,
                            ty: res,
                        }))
                    }
                }
            },
            Expr::Paren(paren_expr) => self.check_expr(paren_expr.val),
            Expr::Simple(simple_expr) => self.check_simple_expr(simple_expr),
            Expr::Name(span) => match self.type_env.get(span.to_str(self.source)) {
                Some(type_) => Ok(type_.clone()),
                None => Err(Box::new(CheckErr::NoSuchVal(NoSuchVal { val_name: *span }))),
            },
        }
    }

    fn check_field(&mut self, field_node: &FieldNode<'s>) -> TResult<'s, TableType<'s>> {
        Ok(TableType {
            key_type: Rc::new(self.check_field_key(field_node)?),
            val_type: Rc::new(self.check_field_val(field_node)?),
        })
    }

    fn check_field_key(&mut self, field_node: &FieldNode<'s>) -> TResult<'s, Type<'s>> {
        match field_node {
            FieldNode::Field { key, .. } => Ok(Type {
                kind: TypeKind::String,
                span: Some(*key),
            }),
            FieldNode::ExprField { key, .. } => self.check_expr(*key),
            FieldNode::ValField { .. } => Ok(Type {
                kind: TypeKind::Number,
                span: None,
            }),
        }
    }

    fn check_field_val(&mut self, field_node: &FieldNode<'s>) -> TResult<'s, Type<'s>> {
        match field_node {
            FieldNode::Field { val, .. }
            | FieldNode::ExprField { val, .. }
            | FieldNode::ValField { val } => self.check_expr(*val),
        }
    }

    fn check_table(&mut self, table_node: &TableNode<'s>) -> TResult<'s, TableType<'s>> {
        if table_node.fields.is_empty() {
            return Ok(TableType {
                key_type: Rc::new(TypeKind::Adaptable.into()),
                val_type: Rc::new(TypeKind::Adaptable.into()),
            });
        }

        let mut result = self.check_field(&table_node.fields[0])?;

        for field in table_node.fields.iter().skip(1) {
            if !result.key_type.can_equal(&self.check_field_key(field)?) {
                result.key_type = Rc::new(TypeKind::Any.into());
                break;
            }
        }

        for field in table_node.fields.iter().skip(1) {
            if !result.val_type.can_equal(&self.check_field_val(field)?) {
                result.val_type = Rc::new(TypeKind::Any.into());
                break;
            }
        }

        Ok(result)
    }

    fn check_simple_expr(&mut self, simple_expr: &SimpleExpr<'s>) -> TResult<'s, Type<'s>> {
        match simple_expr {
            SimpleExpr::Num(s) => Ok(Type {
                kind: TypeKind::Number,
                span: Some(*s),
            }),
            SimpleExpr::Str(s) => Ok(Type {
                kind: TypeKind::String,
                span: Some(*s),
            }),
            SimpleExpr::Bool(s) => Ok(Type {
                kind: TypeKind::Boolean,
                span: Some(*s),
            }),
            SimpleExpr::Nil(s) => Ok(Type {
                kind: TypeKind::Nil,
                span: Some(*s),
            }),
            SimpleExpr::FuncNode(func) => self.check_func(func),
            SimpleExpr::TableNode(table_node) => {
                Ok(TypeKind::Table(self.check_table(table_node)?).into())
            }
            SimpleExpr::SuffixedExpr(suffixed_expr) => self.check_suffixed_expr(suffixed_expr),
        }
    }

    fn check_func(&mut self, func: &FuncNode<'s>) -> TResult<'s, Type<'s>> {
        self.with_scope(|this| {
            for (name, type_) in &func.type_.params {
                this.type_env.insert(name, type_.clone());
            }

            if let TypeKind::Nil = func.type_.returns.kind {
                // TODO: this is obviously bad
                return Ok(TypeKind::Function(*func.type_.clone()).into());
            }

            let has_return = this.check_func_body(&func.body, func.type_.returns.as_ref())?;

            if !has_return {
                return Err(Box::new(CheckErr::NoReturn(NoReturn {
                    func_node: func.clone(),
                })));
            }

            Ok(TypeKind::Function(*func.type_.clone()).into())
        })
    }

    fn check_func_body(
        &mut self,
        body: &[Statement<'s>],
        return_type: &Type<'s>,
    ) -> TResult<'s, bool> {
        self.with_scope(|this| {
            for stmt in body {
                match stmt {
                    Statement::IfStat(IfStat { body: if_body, .. }) => {
                        if this.check_func_body(if_body, return_type)? {
                            return Ok(true);
                        }
                    }
                    Statement::WhileStat(WhileStat {
                        body: while_body, ..
                    }) => {
                        if this.check_func_body(while_body, return_type)? {
                            return Ok(true);
                        }
                    }
                    Statement::RangeFor(RangeFor { body, .. }) => {
                        if this.check_func_body(body, return_type)? {
                            return Ok(true);
                        }
                    }
                    Statement::KeyValFor(KeyValFor { body, .. }) => {
                        if this.check_func_body(body, return_type)? {
                            return Ok(true);
                        }
                    }
                    Statement::Return(node @ ReturnStmt { vals, .. }) => {
                        if vals.is_empty() {
                            return match &return_type.kind {
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

                        if let TypeKind::Multiple(types) = &return_type.kind {
                            if types.len() != vals.len() {
                                return Err(Box::new(CheckErr::ReturnCount(ReturnCount {
                                    return_node: node.clone(),
                                    expected: types.len(),
                                })));
                            }

                            for (type_, val) in types.iter().zip(vals.iter()) {
                                if !type_.can_equal(&this.check_expr(*val)?) {
                                    return Err(Box::new(CheckErr::MismatchedTypes(
                                        MismatchedTypes {
                                            expected: type_.clone(),
                                            recieved: this.check_expr(*val)?,
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

                            return if return_type.can_equal(&this.check_expr(vals[0])?) {
                                Ok(true)
                            } else {
                                Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                                    expected: return_type.clone(),
                                    recieved: this.check_expr(vals[0])?,
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

    fn check_suffixed_name(&mut self, suffixed_name: &SuffixedName<'s>) -> TResult<'s, Type<'s>> {
        let ty = self
            .type_env
            .get(suffixed_name.name.to_str(self.source))
            .ok_or_else(|| {
                Box::new(CheckErr::NoSuchVal(NoSuchVal {
                    val_name: suffixed_name.name,
                }))
            })?
            .clone();

        let mut ty = &ty;

        for suffix in &suffixed_name.suffixes {
            ty = self.check_suffix(ty, suffix)?;
        }

        Ok(ty.clone())
    }

    fn check_suffixed_expr(&mut self, suffixed_expr: &SuffixedExpr<'s>) -> TResult<'s, Type<'s>> {
        let mut ty = &self.check_expr(suffixed_expr.val)?;

        for suffix in &suffixed_expr.suffixes {
            ty = self.check_suffix(ty, suffix)?;
        }

        Ok(ty.clone())
    }

    fn check_suffix<'a>(
        &mut self,
        mut base: &'a Type<'s>,
        suffix: &Suffix<'s>,
    ) -> TResult<'s, &'a Type<'s>> {
        match suffix {
            Suffix::Index(Index { key: _, span }) => match &base.kind {
                TypeKind::Table(TableType { val_type, .. }) => base = val_type,
                TypeKind::String | TypeKind::Adaptable | TypeKind::Any => (),
                _ => {
                    return Err(Box::new(CheckErr::BadIndex {
                        span: *span,
                        ty: base.clone(),
                    }));
                }
            },
            Suffix::Access(Access { field_name: name }) => match &base.kind {
                TypeKind::User(_) => {
                    if let Some(field) = base.kind.get_field(name.to_str(self.source)) {
                        base = field;
                    } else {
                        return Err(Box::new(CheckErr::NoSuchField(NoSuchField {
                            field_name: *name,
                        })));
                    }
                }
                TypeKind::Table(TableType { key_type, val_type }) => {
                    if TypeKind::String.can_equal(&key_type.kind) {
                        base = val_type;
                    } else {
                        return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                            expected: Type {
                                kind: TypeKind::String,
                                span: Some(*name),
                            },
                            recieved: (**key_type).clone(),
                        })));
                    }
                }
                _ => {
                    return Err(Box::new(CheckErr::BadAccess {
                        span: *name,
                        ty: base.clone(),
                    }))
                }
            },
            Suffix::Call(Call { args }) => {
                for arg in args {
                    self.check_expr(*arg)?;
                }

                if let TypeKind::Function(func_type) = &base.kind {
                    base = func_type.returns.as_ref();
                } else if let TypeKind::Adaptable = base.kind {
                    return Ok(base);
                } else {
                    return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                        expected: TypeKind::Function(Function {
                            params: Vec::new(),
                            returns: Box::default(),
                        })
                        .into(),
                        recieved: base.clone(),
                    })));
                }
            }
            Suffix::Method(Method {
                method_name: name, ..
            }) => {
                if name.to_str(self.source) == "new" {
                    return Ok(base);
                } else if let Some(method) = base.kind.get_field(name.to_str(self.source)) {
                    base = method;
                } else {
                    return Err(Box::new(CheckErr::NoSuchMethod(NoSuchMethod {
                        method_name: *name,
                    })));
                }
            }
        }

        Ok(base)
    }

    fn check_binop(&mut self, binop: &BinOp) -> TResult<'s, Type<'s>> {
        match binop.op {
            OpKind::Add | OpKind::Sub | OpKind::Mul | OpKind::Div | OpKind::Mod | OpKind::Pow => {
                let lhs_type = self.check_expr(binop.lhs)?;
                if lhs_type == self.check_expr(binop.rhs)? {
                    Ok(lhs_type)
                } else {
                    Ok(TypeKind::Adaptable.into())
                }
            }
            OpKind::And | OpKind::Or => {
                let lhs_type = self.check_expr(binop.lhs)?;
                let rhs_type = self.check_expr(binop.rhs)?;

                if lhs_type == rhs_type {
                    Ok(lhs_type)
                } else {
                    Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })))
                }
            }
            OpKind::Equ | OpKind::Neq | OpKind::Gre | OpKind::Grq | OpKind::Les | OpKind::Leq => {
                let lhs_type = self.check_expr(binop.lhs)?;
                let rhs_type = self.check_expr(binop.rhs)?;

                if lhs_type == rhs_type {
                    Ok(TypeKind::Boolean.into())
                } else {
                    Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })))
                }
            }
            OpKind::Cat => Ok(TypeKind::String.into()),
        }
    }

    fn check_if_stat(&mut self, if_stat: &IfStat<'s>) -> TResult<'s, ()> {
        let condition_type = self.check_expr(if_stat.condition)?;
        if !condition_type.kind.can_equal(&TypeKind::Boolean) {
            return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                expected: TypeKind::Boolean.into(),
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
                ElseBranch::Else(body) => self.with_scope(|this| {
                    for stmt in body {
                        this.check_statement(stmt)?;
                    }

                    Ok(())
                }),
                ElseBranch::ElseIf(else_if_stat) => self.check_if_stat(else_if_stat),
            },
            None => Ok(()),
        }
    }

    fn check_while_stat(&mut self, while_stat: &WhileStat<'s>) -> TResult<'s, ()> {
        let condition_type = self.check_expr(while_stat.condition)?;
        if !condition_type.kind.can_equal(&TypeKind::Boolean) {
            return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                expected: TypeKind::Boolean.into(),
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

    fn check_range_for(&mut self, range_for: &RangeFor<'s>) -> TResult<'s, ()> {
        let lhs_type = self.check_expr(range_for.range.lhs)?;
        let rhs_type = self.check_expr(range_for.range.lhs)?;

        if !lhs_type.kind.can_equal(&TypeKind::Number) {
            return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                expected: TypeKind::Number.into(),
                recieved: lhs_type,
            })));
        }

        if !rhs_type.kind.can_equal(&TypeKind::Number) {
            return Err(Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
                expected: TypeKind::Number.into(),
                recieved: rhs_type,
            })));
        }

        self.with_scope(|this| {
            this.type_env
                .insert(range_for.var.to_str(this.source), TypeKind::Number.into());

            for stmt in &range_for.body {
                this.check_statement(stmt)?;
            }

            Ok(())
        })
    }

    fn check_keyval_for(&mut self, keyval_for: &KeyValFor<'s>) -> TResult<'s, ()> {
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

        if let TypeKind::Table(TableType { key_type, val_type }) = lhs_type.kind {
            self.with_scope(|this| {
                this.type_env.insert(key_name, (*key_type).clone());
                this.type_env.insert(val_name, (*val_type).clone());

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
