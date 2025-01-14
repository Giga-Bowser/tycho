use std::rc::Rc;

use crate::errors::*;
use crate::parser::*;
use crate::type_env::*;
use crate::types::*;

pub struct TypeChecker<'src> {
    pub pool: ExprPool<'src>,
}

impl<'src> TypeChecker<'src> {
    fn check_decl(&self, decl: &Declare<'src>, type_env: &mut TypeEnv) -> Result<(), CheckErr> {
        if let Some(val) = decl.val {
            if let Node::SimpleExpr(SimpleExpr::StructNode(StructNode { type_, .. })) =
                &self.pool[val]
            {
                type_env.push(decl.lhs.name.to_owned(), Type::User(*type_.clone()));
                return Ok(());
            }

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
                            return Err(CheckErr::NoSuchField((*str).to_string()));
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

    fn check_expr(&self, expr: ExprRef, type_env: &TypeEnv) -> Result<Type, CheckErr> {
        match &self.pool[expr] {
            Node::BinOp(binop) => self.check_binop(binop, type_env),
            Node::UnOp(unop) => match unop.op {
                UnOpKind::Neg => {
                    if std::mem::discriminant(&self.check_expr(unop.val, type_env)?)
                        != std::mem::discriminant(&Type::Number)
                    {
                        Err(CheckErr::EmptyError)
                    } else {
                        Ok(Type::Number)
                    }
                }
                UnOpKind::Len => Ok(Type::Number),
            },
            Node::SimpleExpr(simple_expr) => self.check_simple_expr(simple_expr, type_env),
            Node::Name(str) => match type_env.get(str) {
                Some(type_) => Ok(type_.clone()),
                None => Err(CheckErr::NoSuchVal((*str).to_owned())),
            },
            Node::SuffixedExpr(suffixed_expr) => self.check_suffixed_expr(suffixed_expr, type_env),
            _ => unreachable!(),
        }
    }

    fn check_field(
        &self,
        field_node: &FieldNode<'src>,
        type_env: &TypeEnv,
    ) -> Result<TableType, CheckErr> {
        Ok(TableType {
            key_type: Rc::new(self.check_field_key(field_node, type_env)?),
            val_type: Rc::new(self.check_field_val(field_node, type_env)?),
        })
    }

    fn check_field_key(
        &self,
        field_node: &FieldNode<'src>,
        type_env: &TypeEnv,
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
        type_env: &TypeEnv,
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
        type_env: &TypeEnv,
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
        type_env: &TypeEnv,
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
            SimpleExpr::StructNode(_) => todo!(),
            SimpleExpr::SuffixedExpr(suffixed_expr) => {
                self.check_suffixed_expr(suffixed_expr, type_env)
            }
        }
    }

    fn check_func(&self, func: &'src FuncNode<'src>, type_env: &TypeEnv) -> Result<Type, CheckErr> {
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
        body: &Vec<Node<'src>>,
        type_env: &mut TypeEnv,
        return_type: &Type,
    ) -> Result<bool, CheckErr> {
        let start_len = type_env.len();
        for stat in body {
            match stat {
                Node::IfNode(IfNode { body: if_body, .. }) => {
                    if self.check_func_body(if_body, type_env, return_type)? {
                        return Ok(true);
                    }
                }
                Node::WhileNode(WhileNode {
                    body: while_body, ..
                }) => {
                    if self.check_func_body(while_body, type_env, return_type)? {
                        return Ok(true);
                    }
                }
                Node::RangeFor(RangeFor { body, .. }) => {
                    if self.check_func_body(body, type_env, return_type)? {
                        return Ok(true);
                    }
                }
                Node::KeyValFor(KeyValFor { body, .. }) => {
                    if self.check_func_body(body, type_env, return_type)? {
                        return Ok(true);
                    }
                }
                Node::Return(vals) => {
                    if vals.is_empty() {
                        return Err(CheckErr::ReturnCount);
                    }

                    match return_type {
                        Type::Multiple(types) => {
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
                        }
                        _ => {
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
        type_env: &mut TypeEnv,
    ) -> Result<Type, CheckErr> {
        let mut type_ = match type_env.get(suffixed_name.name) {
            Some(type_) => type_,
            None => return Err(CheckErr::NoSuchVal(suffixed_name.name.to_owned())),
        };

        for suffix in &suffixed_name.suffixes {
            match suffix {
                Suffix::Index(Index { .. }) => match type_ {
                    Type::Table(TableType { val_type, .. }) => type_ = val_type,
                    Type::String => (),
                    _ => unreachable!(),
                },
                Suffix::Access(Access { field_name: str }) => match type_ {
                    Type::User(_) => {
                        if let Some(field) = type_.get_field(str) {
                            type_ = field;
                        } else {
                            return Err(CheckErr::NoSuchField((*str).to_owned()));
                        }
                    }
                    Type::Table(TableType { key_type, val_type }) => {
                        if !Type::String.can_equal(key_type) {
                            return Err(CheckErr::MismatchedTypes {
                                expected: Type::String,
                                recieved: key_type.as_ref().to_owned(),
                            });
                        } else {
                            type_ = val_type;
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

                    if let Type::Function(func_type) = type_ {
                        type_ = func_type.returns.as_ref();
                    } else if *type_ == Type::Adaptable {
                        continue;
                    } else {
                        return Err(CheckErr::MismatchedTypes {
                            expected: Type::Function(Function {
                                params: Vec::new(),
                                returns: Box::default(),
                            }),
                            recieved: type_.clone(),
                        });
                    }
                }
                Suffix::Method(Method {
                    method_name: name, ..
                }) => {
                    if name == &"new" {
                        continue;
                    } else if let Some(method) = type_.get_field(name) {
                        type_ = method;
                    } else {
                        return Err(CheckErr::NoSuchMethod((*name).to_owned()));
                    }
                }
            }
        }

        Ok(type_.clone())
    }

    fn check_suffixed_expr(
        &self,
        suffixed_expr: &SuffixedExpr<'src>,
        type_env: &TypeEnv,
    ) -> Result<Type, CheckErr> {
        let mut type_ = &self.check_expr(suffixed_expr.val, type_env)?;

        for suffix in &suffixed_expr.suffixes {
            match suffix {
                Suffix::Index(Index { .. }) => match type_ {
                    Type::Table(TableType { val_type, .. }) => type_ = val_type,
                    Type::String => (),
                    _ => unreachable!(),
                },
                Suffix::Access(Access { field_name: str }) => match type_ {
                    Type::User(_) => {
                        if let Some(field) = type_.get_field(str) {
                            type_ = field;
                        } else {
                            return Err(CheckErr::NoSuchField((*str).to_owned()));
                        }
                    }
                    Type::Table(TableType { key_type, val_type }) => {
                        if !Type::String.can_equal(key_type) {
                            return Err(CheckErr::MismatchedTypes {
                                expected: Type::String,
                                recieved: key_type.as_ref().to_owned(),
                            });
                        } else {
                            type_ = val_type;
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

                    if let Type::Function(func_type) = type_ {
                        type_ = func_type.returns.as_ref();
                    } else if *type_ == Type::Adaptable {
                        continue;
                    } else {
                        return Err(CheckErr::MismatchedTypes {
                            expected: Type::Function(Function {
                                params: Vec::new(),
                                returns: Box::default(),
                            }),
                            recieved: type_.clone(),
                        });
                    }
                }
                Suffix::Method(Method {
                    method_name: name, ..
                }) => {
                    if name == &"new" {
                        continue;
                    } else if let Some(method) = type_.get_field(name) {
                        type_ = method;
                    } else {
                        return Err(CheckErr::NoSuchMethod((*name).to_owned()));
                    }
                }
            }
        }

        Ok(type_.clone())
    }

    fn check_binop(&self, binop: &BinOp, type_env: &TypeEnv) -> Result<Type, CheckErr> {
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

                if lhs_type != rhs_type {
                    Err(CheckErr::MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })
                } else {
                    Ok(lhs_type)
                }
            }
            OpKind::Equ | OpKind::Neq | OpKind::Gre | OpKind::Grq | OpKind::Les | OpKind::Leq => {
                let lhs_type = self.check_expr(binop.lhs, type_env)?;
                let rhs_type = self.check_expr(binop.rhs, type_env)?;

                if lhs_type != rhs_type {
                    Err(CheckErr::MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })
                } else {
                    Ok(Type::Boolean)
                }
            }
            OpKind::Cat => Ok(Type::String),
        }
    }

    fn check_if_node(&self, if_node: &IfNode<'src>, type_env: &TypeEnv) -> Result<(), CheckErr> {
        let condition_type = self.check_expr(if_node.condition, type_env)?;
        if !condition_type.can_equal(&Type::Boolean) {
            return Err(CheckErr::MismatchedTypes {
                expected: Type::Boolean,
                recieved: condition_type,
            });
        }

        let mut new_env = TypeEnv::new_with_parent(type_env);

        for stat in &if_node.body {
            self.check_statement(stat, &mut new_env)?;
        }

        match &if_node.else_ {
            Some(else_) => match else_.as_ref() {
                Else::Else(body) => {
                    for stat in body {
                        self.check_statement(stat, &mut TypeEnv::new_with_parent(type_env))?;
                    }

                    Ok(())
                }
                Else::ElseIf(else_if_node) => self.check_if_node(else_if_node, type_env),
            },
            None => Ok(()),
        }
    }

    fn check_range_for(
        &self,
        range_for: &RangeFor<'src>,
        type_env: &TypeEnv,
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
        type_env: &TypeEnv,
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

        while !(names[i] as char).is_ascii_whitespace() {
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

    fn check_multi_decl(
        &self,
        multi_decl: &MultiDecl<'src>,
        type_env: &mut TypeEnv,
    ) -> Result<(), CheckErr> {
        // ok so we need to basically flatten the types. so for example
        // a, b, c := twoReturnFunction(), oneReturnFunction()
        // a, b, c := (type1, type2), type3
        // a, b, c := type1, type2, type3
        let mut types = Vec::new();

        for rhs in &multi_decl.rhs_arr {
            let type_ = self.check_expr(*rhs, type_env)?;
            if let Type::Multiple(mult) = type_ {
                types.extend(mult);
            } else {
                types.push(type_);
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

    fn check_method_decl(
        &self,
        method_decl: &MethodDecl<'src>,
        type_env: &mut TypeEnv,
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

    pub fn check_statement(
        &self,
        stat: &Node<'src>,
        type_env: &mut TypeEnv,
    ) -> Result<(), CheckErr> {
        match stat {
            Node::Declare(decl) => self.check_decl(decl, type_env),
            Node::MethodDecl(method_decl) => self.check_method_decl(method_decl, type_env),
            Node::IfNode(if_node) => self.check_if_node(if_node, type_env),
            Node::RangeFor(range_for) => self.check_range_for(range_for, type_env),
            Node::KeyValFor(keyval_for) => self.check_keyval_for(keyval_for, type_env),
            Node::Assign(Assign { lhs, rhs }) => {
                let lhs_type = self.check_suffixed_name(lhs, type_env)?;
                let rhs_type = self.check_expr(*rhs, type_env)?;
                if !lhs_type.can_equal(&rhs_type) {
                    Err(CheckErr::MismatchedTypes {
                        expected: lhs_type,
                        recieved: rhs_type,
                    })
                } else {
                    Ok(())
                }
            }
            Node::MultiDecl(multi_decl) => self.check_multi_decl(multi_decl, type_env),
            // TODO: investigate if making a more expression-statement specific checking function is worth it
            Node::SuffixedExpr(suffixed_expr) => {
                self.check_suffixed_expr(suffixed_expr, type_env).map(drop)
            }
            _ => Ok(()),
        }
    }
}
