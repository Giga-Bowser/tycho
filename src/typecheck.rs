use std::rc::Rc;

use crate::{
    errors::CheckErr,
    parser::{
        ast::*,
        pool::{ExprPool, ExprRef},
    },
    type_env::TypeEnv,
    types::*,
};

pub struct TypeChecker<'src, 'pool> {
    pub pool: &'pool ExprPool<'src>,
}

impl<'src> TypeChecker<'src, '_> {
    pub fn check_statement(
        &self,
        stat: &Statement<'src>,
        type_env: &mut TypeEnv<'_>,
    ) -> Result<(), CheckErr> {
        match stat {
            Statement::Declare(decl) => self.check_decl(decl, type_env),
            Statement::MethodDecl(method_decl) => self.check_method_decl(method_decl, type_env),
            Statement::MultiDecl(multi_decl) => self.check_multi_decl(multi_decl, type_env),
            Statement::IfStat(if_stat) => self.check_if_stat(if_stat, type_env),
            Statement::WhileStat(while_stat) => self.check_while_stat(while_stat, type_env),
            Statement::RangeFor(range_for) => self.check_range_for(range_for, type_env),
            Statement::KeyValFor(keyval_for) => self.check_keyval_for(keyval_for, type_env),
            Statement::Assign(Assign { lhs, rhs }) => {
                let lhs_type = self.check_suffixed_name(lhs, type_env)?;
                let rhs_type = self.check_expr(*rhs, type_env)?;
                if lhs_type.can_equal(&rhs_type) {
                    Ok(())
                } else {
                    Err(CheckErr::MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })
                }
            }
            Statement::MultiAssign(multi_assign) => self.check_multi_assign(multi_assign, type_env),
            // TODO: investigate if making a more expression-statement specific checking function is worth it
            Statement::ExprStat(suffixed_expr) => {
                self.check_suffixed_expr(suffixed_expr, type_env).map(drop)
            }
            Statement::Block(statements) => {
                let mut new_env = TypeEnv::new_with_parent(type_env);
                for stat in statements {
                    self.check_statement(stat, &mut new_env)?;
                }
                Ok(())
            }
            Statement::StructDecl(StructDecl {
                name,
                type_,
                constructor: _,
            }) => {
                type_env.push((*name).to_owned(), Type::User(*type_.clone()));
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn check_decl(&self, decl: &Declare<'src>, type_env: &mut TypeEnv<'_>) -> Result<(), CheckErr> {
        if let Some(val) = decl.val {
            let lhs_type = self.check_expr(val, type_env)?;

            if let Type::Adaptable = *decl.type_ {
                type_env.push(decl.lhs.name.to_owned(), lhs_type);
                return Ok(());
            }

            if !decl.type_.can_equal(&lhs_type) {
                return Err(CheckErr::MismatchedTypes {
                    expected: decl.type_.as_ref().clone(),
                    recieved: lhs_type,
                });
            }

            type_env.push(decl.lhs.name.to_owned(), *decl.type_.clone());
            Ok(())
        } else {
            if decl.lhs.suffixes.is_empty() {
                type_env.push(decl.lhs.name.to_owned(), *decl.type_.clone());
                return Ok(());
            }

            let mut type_ = type_env.get_mut(decl.lhs.name).unwrap();

            for suffix in &mut decl.lhs.suffixes.iter().take(decl.lhs.suffixes.len() - 1) {
                match suffix {
                    Suffix::Access(Access { field_name: str }) => {
                        if let Some(field) = type_.get_field_mut(str) {
                            type_ = field;
                        } else {
                            return Err(CheckErr::NoSuchField((*str).to_owned()));
                        }
                    }
                    _ => unreachable!(),
                }
            }

            let last = decl.lhs.suffixes.last().unwrap();

            if let Suffix::Access(Access { field_name: str }) = last {
                match type_ {
                    Type::Table(_) => Ok(()),
                    Type::User(ref mut user) => {
                        user.fields.push(((*str).to_owned(), *decl.type_.clone()));
                        Ok(())
                    }
                    _ => Err(CheckErr::CustomError(
                        "tried to define field on wrong type".to_owned(),
                    )),
                }
            } else {
                unreachable!()
            }
        }
    }

    fn check_method_decl(
        &self,
        method_decl: &MethodDecl<'src>,
        type_env: &mut TypeEnv<'_>,
    ) -> Result<(), CheckErr> {
        let type_ = type_env.get(method_decl.struct_name).unwrap();

        let method_type = if let Type::User(_) = type_ {
            type_env.push("self".to_owned(), type_.clone());
            self.check_func(&method_decl.func, type_env)?
        } else {
            return Err(CheckErr::CustomError(
                "cannot declare method on non-struct type".to_owned(),
            ));
        };
        type_env.pop(); // remove "self"

        let Type::User(User { fields }) = type_env.get_mut(method_decl.struct_name).unwrap() else {
            unreachable!()
        };

        fields.push((method_decl.method_name.to_owned(), method_type));

        Ok(())
    }

    fn check_multi_decl(
        &self,
        multi_decl: &MultiDecl<'src>,
        type_env: &mut TypeEnv<'_>,
    ) -> Result<(), CheckErr> {
        // ok so we need to basically flatten the types. so for example
        // a, b, c := twoReturnFunction(), oneReturnFunction()
        // a, b, c := (type1, type2), type3
        // a, b, c := type1, type2, type3
        let mut types = Vec::new();

        for rhs in &multi_decl.rhs_arr {
            let expr_type = self.check_expr(*rhs, type_env)?;
            if let Type::Multiple(mult) = expr_type {
                types.extend(mult);
            } else {
                types.push(expr_type);
            }
        }

        for (i, name) in multi_decl.lhs_arr.iter().enumerate() {
            if i < types.len() {
                type_env.push((*name).to_owned(), types[i].clone());
            } else {
                type_env.push((*name).to_owned(), Type::Nil);
            }
        }

        Ok(())
    }

    fn check_multi_assign(
        &self,
        multi_assign: &MultiAssign<'src>,
        type_env: &mut TypeEnv<'_>,
    ) -> Result<(), CheckErr> {
        // ok so we need to basically flatten the types. so for example
        // a, b, c := twoReturnFunction(), oneReturnFunction()
        // a, b, c := (type1, type2), type3
        // a, b, c := type1, type2, type3
        let mut types = Vec::new();

        for rhs in &multi_assign.rhs_arr {
            let rhs_type = self.check_expr(*rhs, type_env)?;
            if let Type::Multiple(mult) = rhs_type {
                types.extend(mult);
            } else {
                types.push(rhs_type);
            }
        }

        for (i, suffixed_expr) in multi_assign.lhs_arr.iter().enumerate() {
            let expected = self.check_suffixed_expr(suffixed_expr, type_env)?;
            let recieved = if i < types.len() {
                &types[i]
            } else {
                &Type::Nil
            };

            if !expected.can_equal(recieved) {
                return Err(CheckErr::MismatchedTypes {
                    expected,
                    recieved: recieved.clone(),
                });
            }
        }

        Ok(())
    }

    fn check_expr(&self, expr: ExprRef, type_env: &TypeEnv<'_>) -> Result<Type, CheckErr> {
        match &self.pool[expr] {
            Expr::BinOp(binop) => self.check_binop(binop, type_env),
            Expr::UnOp(unop) => match unop.op {
                UnOpKind::Neg => {
                    if std::mem::discriminant(&self.check_expr(unop.val, type_env)?)
                        == std::mem::discriminant(&Type::Number)
                    {
                        Ok(Type::Number)
                    } else {
                        Err(CheckErr::EmptyError)
                    }
                }
                UnOpKind::Len => Ok(Type::Number),
            },
            Expr::Paren(paren_expr) => self.check_expr(paren_expr.val, type_env),
            Expr::Simple(simple_expr) => self.check_simple_expr(simple_expr, type_env),
            Expr::Name(str) => match type_env.get(*str) {
                Some(type_) => Ok(type_.clone()),
                None => Err(CheckErr::NoSuchVal((*str).to_owned())),
            },
        }
    }

    fn check_field(
        &self,
        field_node: &FieldNode<'src>,
        type_env: &TypeEnv<'_>,
    ) -> Result<TableType, CheckErr> {
        Ok(TableType {
            key_type: Rc::new(self.check_field_key(field_node, type_env)?),
            val_type: Rc::new(self.check_field_val(field_node, type_env)?),
        })
    }

    fn check_field_key(
        &self,
        field_node: &FieldNode<'src>,
        type_env: &TypeEnv<'_>,
    ) -> Result<Type, CheckErr> {
        match field_node {
            FieldNode::Field { .. } => Ok(Type::String),
            FieldNode::ExprField { key, .. } => self.check_expr(*key, type_env),
            FieldNode::ValField { .. } => Ok(Type::Number),
        }
    }

    fn check_field_val(
        &self,
        field_node: &FieldNode<'src>,
        type_env: &TypeEnv<'_>,
    ) -> Result<Type, CheckErr> {
        match field_node {
            FieldNode::Field { val, .. }
            | FieldNode::ExprField { val, .. }
            | FieldNode::ValField { val } => self.check_expr(*val, type_env),
        }
    }

    fn check_table(
        &self,
        table_node: &TableNode<'src>,
        type_env: &TypeEnv<'_>,
    ) -> Result<TableType, CheckErr> {
        if table_node.fields.is_empty() {
            return Ok(TableType {
                key_type: Rc::new(Type::Adaptable),
                val_type: Rc::new(Type::Adaptable),
            });
        }

        let mut result = self.check_field(&table_node.fields[0], type_env)?;

        for field in table_node.fields.iter().skip(1) {
            if !result
                .key_type
                .can_equal(&self.check_field_key(field, type_env)?)
            {
                result.key_type = Rc::new(Type::Any);
                break;
            }
        }

        for field in table_node.fields.iter().skip(1) {
            if !result
                .val_type
                .can_equal(&self.check_field_val(field, type_env)?)
            {
                result.val_type = Rc::new(Type::Any);
                break;
            }
        }

        Ok(result)
    }

    fn check_simple_expr(
        &self,
        simple_expr: &SimpleExpr<'src>,
        type_env: &TypeEnv<'_>,
    ) -> Result<Type, CheckErr> {
        match simple_expr {
            SimpleExpr::Num(_) => Ok(Type::Number),
            SimpleExpr::Str(_) => Ok(Type::String),
            SimpleExpr::Bool(_) => Ok(Type::Boolean),
            SimpleExpr::Nil(_) => Ok(Type::Nil),
            SimpleExpr::FuncNode(func) => self.check_func(func, type_env),
            SimpleExpr::TableNode(table_node) => {
                Ok(Type::Table(self.check_table(table_node, type_env)?))
            }
            SimpleExpr::SuffixedExpr(suffixed_expr) => {
                self.check_suffixed_expr(suffixed_expr, type_env)
            }
        }
    }

    fn check_func(
        &self,
        func: &'src FuncNode<'src>,
        type_env: &TypeEnv<'_>,
    ) -> Result<Type, CheckErr> {
        let mut new_env = TypeEnv::new_with_parent(type_env);
        for (name, type_) in &func.type_.params {
            new_env.push(name.to_owned(), type_.clone());
        }

        if *func.type_.returns == Type::Nil {
            return Ok(Type::Nil);
        }

        let has_return =
            self.check_func_body(&func.body, &mut new_env, func.type_.returns.as_ref())?;

        if !has_return {
            return Err(CheckErr::NoReturn);
        }

        Ok(Type::Function(*func.type_.clone()))
    }

    fn check_func_body(
        &self,
        body: &[Statement<'src>],
        type_env: &mut TypeEnv<'_>,
        return_type: &Type,
    ) -> Result<bool, CheckErr> {
        let start_len = type_env.len();
        for stat in body {
            match stat {
                Statement::IfStat(IfStat { body: if_body, .. }) => {
                    if self.check_func_body(if_body, type_env, return_type)? {
                        return Ok(true);
                    }
                }
                Statement::WhileStat(WhileStat {
                    body: while_body, ..
                }) => {
                    if self.check_func_body(while_body, type_env, return_type)? {
                        return Ok(true);
                    }
                }
                Statement::RangeFor(RangeFor { body, .. }) => {
                    if self.check_func_body(body, type_env, return_type)? {
                        return Ok(true);
                    }
                }
                Statement::KeyValFor(KeyValFor { body, .. }) => {
                    if self.check_func_body(body, type_env, return_type)? {
                        return Ok(true);
                    }
                }
                Statement::Return(vals) => {
                    if vals.is_empty() {
                        return Err(CheckErr::ReturnCount);
                    }

                    if let Type::Multiple(types) = return_type {
                        if types.len() != vals.len() {
                            return Err(CheckErr::ReturnCount);
                        }

                        for (type_, val) in types.iter().zip(vals.iter()) {
                            if !type_.can_equal(&self.check_expr(*val, type_env)?) {
                                return Err(CheckErr::MismatchedTypes {
                                    expected: type_.clone(),
                                    recieved: self.check_expr(*val, type_env)?,
                                });
                            }
                        }
                    } else {
                        if vals.len() != 1 {
                            return Err(CheckErr::ReturnCount);
                        }

                        return if return_type.can_equal(&self.check_expr(vals[0], type_env)?) {
                            Ok(true)
                        } else {
                            Err(CheckErr::MismatchedTypes {
                                expected: return_type.clone(),
                                recieved: self.check_expr(vals[0], type_env)?,
                            })
                        };
                    }

                    type_env.truncate(start_len);
                    return Ok(true);
                }
                _ => (),
            }
            self.check_statement(stat, type_env)?;
        }

        type_env.truncate(start_len);
        Ok(false)
    }

    fn check_suffixed_name(
        &self,
        suffixed_name: &SuffixedName<'src>,
        type_env: &mut TypeEnv<'_>,
    ) -> Result<Type, CheckErr> {
        let Some(mut type_) = type_env.get(suffixed_name.name) else {
            return Err(CheckErr::NoSuchVal(suffixed_name.name.to_owned()));
        };

        for suffix in &suffixed_name.suffixes {
            type_ = self.check_suffix(type_, suffix, type_env)?;
        }

        Ok(type_.clone())
    }

    fn check_suffixed_expr(
        &self,
        suffixed_expr: &SuffixedExpr<'src>,
        type_env: &TypeEnv<'_>,
    ) -> Result<Type, CheckErr> {
        let mut type_ = &self.check_expr(suffixed_expr.val, type_env)?;

        for suffix in &suffixed_expr.suffixes {
            type_ = self.check_suffix(type_, suffix, type_env)?;
        }

        Ok(type_.clone())
    }

    fn check_suffix<'a>(
        &self,
        mut base: &'a Type,
        suffix: &Suffix<'src>,
        type_env: &TypeEnv<'_>,
    ) -> Result<&'a Type, CheckErr> {
        match suffix {
            Suffix::Index(Index { .. }) => match base {
                Type::Table(TableType { val_type, .. }) => base = val_type,
                Type::String => (),
                _ => unreachable!(),
            },
            Suffix::Access(Access { field_name: str }) => match base {
                Type::User(_) => {
                    if let Some(field) = base.get_field(str) {
                        base = field;
                    } else {
                        return Err(CheckErr::NoSuchField((*str).to_owned()));
                    }
                }
                Type::Table(TableType { key_type, val_type }) => {
                    if Type::String.can_equal(key_type) {
                        base = val_type;
                    } else {
                        return Err(CheckErr::MismatchedTypes {
                            expected: Type::String,
                            recieved: key_type.as_ref().to_owned(),
                        });
                    }
                }
                _ => {
                    return Err(CheckErr::CustomError(
                        "cannot perform access on this type".to_owned(),
                    ))
                }
            },
            Suffix::Call(Call { args }) => {
                for arg in args {
                    self.check_expr(*arg, type_env)?;
                }

                if let Type::Function(func_type) = base {
                    base = func_type.returns.as_ref();
                } else if *base == Type::Adaptable {
                    return Ok(base);
                } else {
                    return Err(CheckErr::MismatchedTypes {
                        expected: Type::Function(Function {
                            params: Vec::new(),
                            returns: Box::default(),
                        }),
                        recieved: base.clone(),
                    });
                }
            }
            Suffix::Method(Method {
                method_name: name, ..
            }) => {
                if name == &"new" {
                    return Ok(base);
                } else if let Some(method) = base.get_field(name) {
                    base = method;
                } else {
                    return Err(CheckErr::NoSuchMethod((*name).to_owned()));
                }
            }
        }

        Ok(base)
    }

    fn check_binop(&self, binop: &BinOp, type_env: &TypeEnv<'_>) -> Result<Type, CheckErr> {
        match binop.op {
            OpKind::Add | OpKind::Sub | OpKind::Mul | OpKind::Div | OpKind::Pow => {
                let lhs_type = self.check_expr(binop.lhs, type_env)?;
                if lhs_type == self.check_expr(binop.rhs, type_env)? {
                    Ok(lhs_type)
                } else {
                    Ok(Type::Adaptable)
                }
            }
            OpKind::And | OpKind::Or => {
                let lhs_type = self.check_expr(binop.lhs, type_env)?;
                let rhs_type = self.check_expr(binop.rhs, type_env)?;

                if lhs_type == rhs_type {
                    Ok(lhs_type)
                } else {
                    Err(CheckErr::MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })
                }
            }
            OpKind::Equ | OpKind::Neq | OpKind::Gre | OpKind::Grq | OpKind::Les | OpKind::Leq => {
                let lhs_type = self.check_expr(binop.lhs, type_env)?;
                let rhs_type = self.check_expr(binop.rhs, type_env)?;

                if lhs_type == rhs_type {
                    Ok(Type::Boolean)
                } else {
                    Err(CheckErr::MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })
                }
            }
            OpKind::Cat => Ok(Type::String),
        }
    }

    fn check_if_stat(
        &self,
        if_stat: &IfStat<'src>,
        type_env: &TypeEnv<'_>,
    ) -> Result<(), CheckErr> {
        let condition_type = self.check_expr(if_stat.condition, type_env)?;
        if !condition_type.can_equal(&Type::Boolean) {
            return Err(CheckErr::MismatchedTypes {
                expected: Type::Boolean,
                recieved: condition_type,
            });
        }

        let mut new_env = TypeEnv::new_with_parent(type_env);

        for stat in &if_stat.body {
            self.check_statement(stat, &mut new_env)?;
        }

        match &if_stat.else_ {
            Some(else_) => match else_.as_ref() {
                ElseBranch::Else(body) => {
                    let mut new_env = TypeEnv::new_with_parent(type_env);
                    for stat in body {
                        self.check_statement(stat, &mut new_env)?;
                    }

                    Ok(())
                }
                ElseBranch::ElseIf(else_if_stat) => self.check_if_stat(else_if_stat, type_env),
            },
            None => Ok(()),
        }
    }

    fn check_while_stat(
        &self,
        while_stat: &WhileStat<'_>,
        type_env: &mut TypeEnv<'_>,
    ) -> Result<(), CheckErr> {
        let condition_type = self.check_expr(while_stat.condition, type_env)?;
        if !condition_type.can_equal(&Type::Boolean) {
            return Err(CheckErr::MismatchedTypes {
                expected: Type::Boolean,
                recieved: condition_type,
            });
        }

        let mut new_env = TypeEnv::new_with_parent(type_env);

        for stat in &while_stat.body {
            self.check_statement(stat, &mut new_env)?;
        }

        Ok(())
    }

    fn check_range_for(
        &self,
        range_for: &RangeFor<'src>,
        type_env: &TypeEnv<'_>,
    ) -> Result<(), CheckErr> {
        let lhs_type = self.check_expr(range_for.range.lhs, type_env)?;
        let rhs_type = self.check_expr(range_for.range.lhs, type_env)?;

        if !lhs_type.can_equal(&Type::Number) {
            return Err(CheckErr::MismatchedTypes {
                expected: Type::Number,
                recieved: lhs_type,
            });
        }

        if !rhs_type.can_equal(&Type::Number) {
            return Err(CheckErr::MismatchedTypes {
                expected: Type::Number,
                recieved: rhs_type,
            });
        }

        let mut new_env = TypeEnv::new_with_parent(type_env);

        new_env.push(range_for.var.to_owned(), Type::Number);

        for stat in &range_for.body {
            self.check_statement(stat, &mut new_env)?;
        }

        Ok(())
    }

    fn check_keyval_for(
        &self,
        keyval_for: &KeyValFor<'src>,
        type_env: &TypeEnv<'_>,
    ) -> Result<(), CheckErr> {
        // now because i'm evil, we have to retokenize the `key, val` string_view
        // but who cares.
        let names = keyval_for.names.as_bytes();

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

        let lhs_type = self.check_expr(keyval_for.iter, type_env)?;

        if let Type::Table(TableType { key_type, val_type }) = lhs_type {
            let mut new_env = TypeEnv::new_with_parent(type_env);

            new_env.push(key_name.to_owned(), (*key_type).clone());
            new_env.push(val_name.to_owned(), (*val_type).clone());

            for stat in &keyval_for.body {
                self.check_statement(stat, &mut new_env)?;
            }

            Ok(())
        } else {
            Err(CheckErr::NotIterable)
        }
    }
}
