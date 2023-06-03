use std::rc::Rc;

use crate::errors::*;
use crate::parser::*;
use crate::types::*;
use crate::TypeEnv;

pub struct TypeChecker<'src> {
    pub pool: ExprPool<'src>,
}

impl<'src> TypeChecker<'src> {
    fn check_decl(
        &self,
        decl: &Declare<'src>,
        type_env: &mut TypeEnv<'src>,
    ) -> Result<(), CheckErr> {
        if let Some(val) = decl.val {
            if let Node::SimpleExpr(SimpleExpr::StructNode(StructNode { type_, .. })) =
                &self.pool[val]
            {
                type_env.insert(decl.lhs.name.to_owned(), Type::User(*type_.clone()));
                return Ok(());
            }

            let lhs_type = self.check_expr(val, type_env)?;

            if let Type::Adaptable = *decl.type_ {
                type_env.insert(decl.lhs.name.to_owned(), lhs_type);
                return Ok(());
            }

            if !decl.type_.can_equal(&lhs_type) {
                return Err(CheckErr::MismatchedTypes {
                    expected: Some(decl.type_.as_ref().clone()),
                    recieved: Some(lhs_type),
                });
            }

            type_env.insert(decl.lhs.name.to_owned(), *decl.type_.clone());
            Ok(())
        } else {
            if decl.lhs.suffixes.is_empty() {
                type_env.insert(decl.lhs.name.to_owned(), *decl.type_.clone());
            }

            let mut type_ = type_env.get_mut(decl.lhs.name).unwrap();

            for suffix in &mut decl.lhs.suffixes.iter().take(decl.lhs.suffixes.len() - 1) {
                match suffix {
                    Suffix::Access(Access { str }) => {
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

            if let Suffix::Access(Access { str }) = last {
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

    fn check_expr(&self, expr: ExprRef, type_env: &TypeEnv<'src>) -> Result<Type, CheckErr> {
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
            Node::Name(str) => match type_env.get(*str) {
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
        type_env: &TypeEnv<'src>,
    ) -> Result<TableType, CheckErr> {
        Ok(TableType {
            key_type: Rc::new(self.check_field_key(field_node, type_env)?),
            val_type: Rc::new(self.check_field_val(field_node, type_env)?),
        })
    }

    fn check_field_key(
        &self,
        field_node: &FieldNode<'src>,
        type_env: &TypeEnv<'src>,
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
        type_env: &TypeEnv<'src>,
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
        type_env: &TypeEnv<'src>,
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
        type_env: &TypeEnv<'src>,
    ) -> Result<Type, CheckErr> {
        match simple_expr {
            SimpleExpr::Num(..) => Ok(Type::Number),
            SimpleExpr::Str(_) => Ok(Type::String),
            SimpleExpr::Bool(_) => Ok(Type::Boolean),
            SimpleExpr::Nil(_) => Ok(Type::Nil),
            SimpleExpr::FuncNode(func) => self.check_func(func, type_env.clone()),
            SimpleExpr::TableNode(table_node) => {
                Ok(Type::Table(self.check_table(table_node, type_env)?))
            }
            SimpleExpr::StructNode(_) => todo!(),
            SimpleExpr::SuffixedExpr(suffixed_expr) => {
                self.check_suffixed_expr(suffixed_expr, type_env)
            }
        }
    }

    fn check_func(
        &self,
        func: &'src FuncNode<'src>,
        mut type_env: TypeEnv<'src>,
    ) -> Result<Type, CheckErr> {
        for (name, type_) in &func.type_.params {
            type_env.insert(name.to_owned(), type_.clone());
        }

        if func.type_.returns.is_empty() {
            return Ok(Type::Nil);
        }

        let has_return = self.check_func_body(&func.body, type_env, &func.type_.returns)?;

        if !has_return {
            return Err(CheckErr::NoReturn);
        }

        Ok(Type::Function(*func.type_.clone()))
    }

    fn check_func_body(
        &self,
        body: &Vec<Node<'src>>,
        mut type_env: TypeEnv<'src>,
        return_types: &Vec<Type>,
    ) -> Result<bool, CheckErr> {
        for stat in body {
            match stat {
                Node::IfNode(IfNode { body: if_body, .. }) => {
                    if self.check_func_body(if_body, type_env.clone(), return_types)? {
                        return Ok(true);
                    }
                }
                Node::WhileNode(WhileNode {
                    body: while_body, ..
                }) => {
                    if self.check_func_body(while_body, type_env.clone(), return_types)? {
                        return Ok(true);
                    }
                }
                Node::RangeFor(RangeFor { body, .. }) => {
                    if self.check_func_body(body, type_env.clone(), return_types)? {
                        return Ok(true);
                    }
                }
                Node::KeyValFor(KeyValFor { body, .. }) => {
                    if self.check_func_body(body, type_env.clone(), return_types)? {
                        return Ok(true);
                    }
                }
                Node::Return(vals) => {
                    if return_types.len() != vals.len() {
                        return Err(CheckErr::ReturnCount);
                    }

                    for (type_, val) in return_types.iter().zip(vals.iter()) {
                        if !type_.can_equal(&self.check_expr(*val, &type_env)?) {
                            return Err(CheckErr::MismatchedTypes {
                                expected: Some(type_.clone()),
                                recieved: Some(self.check_expr(*val, &type_env)?),
                            });
                        }
                    }

                    return Ok(true);
                }
                _ => (),
            }
            self.check_statement(stat, &mut type_env)?;
        }

        Ok(false)
    }

    fn check_suffixed_expr(
        &self,
        suffixed_expr: &SuffixedExpr<'src>,
        type_env: &TypeEnv<'src>,
    ) -> Result<Type, CheckErr> {
        let mut type_ = &self.check_expr(suffixed_expr.val, type_env)?;

        for suffix in &suffixed_expr.suffixes {
            match suffix {
                Suffix::Index(Index { .. }) => match type_ {
                    Type::Table(TableType { val_type, .. }) => type_ = val_type,
                    Type::String => (),
                    _ => unreachable!(),
                },
                Suffix::Access(Access { str }) => match type_ {
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
                                expected: Some(Type::String),
                                recieved: Some(key_type.as_ref().to_owned()),
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
                        type_ = if func_type.returns.is_empty() {
                            &Type::Nil
                        } else if func_type.returns.len() == 1 {
                            &func_type.returns[0]
                        } else {
                            &Type::Multiple
                        }
                    } else if *type_ == Type::Adaptable {
                        continue;
                    } else {
                        return Err(CheckErr::MismatchedTypes {
                            expected: Some(Type::Function(Function {
                                params: Vec::new(),
                                returns: Vec::new(),
                            })),
                            recieved: Some(type_.clone()),
                        });
                    }
                }
                Suffix::Method(Method { name, .. }) => {
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

    fn check_binop(&self, binop: &BinOp, type_env: &TypeEnv<'src>) -> Result<Type, CheckErr> {
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
                        expected: Some(lhs_type),
                        recieved: Some(rhs_type),
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
                        expected: Some(lhs_type),
                        recieved: Some(rhs_type),
                    })
                } else {
                    Ok(Type::Boolean)
                }
            }
            OpKind::Cat => Ok(Type::String),
        }
    }

    fn check_if_node(
        &self,
        if_node: &IfNode<'src>,
        type_env: &TypeEnv<'src>,
    ) -> Result<(), CheckErr> {
        let condition_type = self.check_expr(if_node.condition, type_env)?;
        if !condition_type.can_equal(&Type::Boolean) {
            return Err(CheckErr::MismatchedTypes {
                expected: Some(Type::Boolean),
                recieved: Some(condition_type),
            });
        }

		let mut scoped_type_env = type_env.clone();

		for stat in &if_node.body {
			self.check_statement(stat, &mut scoped_type_env)?;
		}
		
		Ok(())
    }

    pub fn check_statement(
        &self,
        stat: &Node<'src>,
        type_env: &mut TypeEnv<'src>,
    ) -> Result<(), CheckErr> {
        match stat {
            Node::Declare(decl) => self.check_decl(decl, type_env),
            Node::IfNode(if_node) => self.check_if_node(if_node, type_env),
            _ => Ok(()),
        }
    }
}
