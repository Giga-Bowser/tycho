pub mod ast;
pub mod pool;

use std::{rc::Rc, slice, str};

use rustc_hash::FxHashMap;

use self::{ast::*, pool::*};

use crate::{
    errors::{ParseError, UnexpectedToken},
    lexer::{
        TokenKind::{self, *},
        Tokens,
    },
    types::{Function, TableType, Type, User},
};

pub struct Parser<'src, 'pool> {
    pub tokens: Tokens<'src>,
    pub pool: &'pool mut ExprPool<'src>,
}

impl<'src> Parser<'src, '_> {
    pub fn parse_statement(
        &mut self,
        typelist: &mut TypeList,
    ) -> Result<Statement<'src>, ParseError> {
        match self.tokens[0].kind {
            Name => {
                if self.tokens[1].kind == Colon
                    && self.tokens[2].kind == Name
                    && self.tokens[3].kind == Colon
                {
                    Ok(Statement::MethodDecl(self.parse_method_decl(typelist)?))
                } else {
                    self.parse_expr_stat(typelist)
                }
            }
            If => Ok(Statement::IfStat(self.if_stat(typelist)?)),
            While => Ok(Statement::WhileStat(self.while_stat(typelist)?)),
            For => self.for_stat(typelist),
            Return => {
                self.tokens.pop_front();
                Ok(Statement::Return(self.parse_expr_list(typelist)?))
            }
            Break => {
                self.tokens.pop_front();
                Ok(Statement::Break)
            }
            LCurly => {
                self.tokens.pop_front();
                let mut body = Vec::new();

                while self.tokens[0].kind != RCurly {
                    body.push(self.parse_statement(typelist)?);
                }

                Ok(Statement::Block(body))
            }
            _ => self.parse_expr_stat(typelist),
        }
    }

    fn parse_method_decl(&mut self, typelist: &TypeList) -> Result<MethodDecl<'src>, ParseError> {
        let struct_name = self.tokens[0].str;

        self.tokens.pop_front(); // name
        self.tokens.pop_front(); // colon
        let method_name = self.tokens[0].str;

        self.tokens.pop_front(); // method name
        self.tokens.pop_front(); // colon
        self.tokens.expect(Equal)?;
        Ok(MethodDecl {
            struct_name,
            method_name,
            func: Box::new(self.func_constructor(typelist)?),
        })
    }

    fn parse_decl(
        &mut self,
        typelist: &mut TypeList,
        lhs: SuffixedName<'src>,
    ) -> Result<Declare<'src>, ParseError> {
        self.tokens.pop_front(); // pop ':'

        if self.tokens[0].kind == Equal {
            self.tokens.pop_front();

            let val = self.parse_expr(typelist)?;

            if let Expr::Simple(SimpleExpr::StructNode(StructNode { type_, name, .. })) =
                &mut self.pool[val]
            {
                if !lhs.suffixes.is_empty() {
                    return Err(ParseError::EmptyError);
                }
                typelist.insert(lhs.name.to_owned(), Type::User(*type_.clone()));
                *name = Some(lhs.name);
            }

            return Ok(Declare {
                lhs: Box::new(lhs),
                type_: Box::new(Type::Adaptable),
                val: Some(val),
            });
        }

        let type_ = Box::new(self.parse_type(typelist)?);

        if self.tokens[0].kind != Equal {
            return Ok(Declare {
                lhs: Box::new(lhs),
                type_,
                val: None,
            });
        }

        self.tokens.pop_front(); // pop '='

        let val = self.parse_expr(typelist)?;

        if let Expr::Simple(SimpleExpr::StructNode(StructNode {
            type_: struct_type,
            name,
            ..
        })) = &mut self.pool[val]
        {
            if !lhs.suffixes.is_empty() {
                return Err(ParseError::EmptyError);
            }

            typelist.insert(lhs.name.to_owned(), Type::User(*struct_type.clone()));
            *name = Some(lhs.name);
        }

        Ok(Declare {
            lhs: Box::new(lhs),
            type_,
            val: Some(val),
        })
    }

    fn parse_assignment(
        &mut self,
        typelist: &TypeList,
        lhs: SuffixedName<'src>,
    ) -> Result<Assign<'src>, ParseError> {
        self.tokens.pop_front(); // pop '='

        let rhs = self.parse_expr(typelist)?;

        Ok(Assign {
            lhs: Box::new(lhs),
            rhs,
        })
    }

    fn if_stat(&mut self, typelist: &TypeList) -> Result<IfStat<'src>, ParseError> {
        self.tokens.pop_front(); // pop 'if'

        let condition = self.parse_expr(typelist)?;

        self.tokens.expect(LCurly)?;

        let mut scoped_typelist = typelist.clone();

        let mut body = Vec::new();

        while self.tokens[0].kind != RCurly {
            body.push(self.parse_statement(&mut scoped_typelist)?);
        }

        self.tokens.pop_front(); // pop '}'

        if self.tokens[0].kind != Else {
            return Ok(IfStat {
                condition,
                body,
                else_: None,
            });
        }

        self.tokens.pop_front(); // pop 'else'

        if self.tokens[0].kind == If {
            let else_ = Some(Box::new(ElseBranch::ElseIf(self.if_stat(typelist)?)));

            return Ok(IfStat {
                condition,
                body,
                else_,
            });
        }

        scoped_typelist = typelist.clone(); // reset typelist for next if

        let mut else_body = Vec::new();

        self.tokens.expect(LCurly)?;

        while self.tokens[0].kind != RCurly {
            else_body.push(self.parse_statement(&mut scoped_typelist)?);
        }

        self.tokens.pop_front(); // pop '}'

        let else_ = Some(Box::new(ElseBranch::Else(else_body)));

        Ok(IfStat {
            condition,
            body,
            else_,
        })
    }

    fn while_stat(&mut self, typelist: &TypeList) -> Result<WhileStat<'src>, ParseError> {
        self.tokens.pop_front(); // pop 'while'

        let condition = self.parse_expr(typelist)?;

        self.tokens.expect(LCurly)?;

        let mut scoped_typelist = typelist.clone();

        let mut body = Vec::new();

        while self.tokens[0].kind != RCurly {
            body.push(self.parse_statement(&mut scoped_typelist)?);
        }

        self.tokens.pop_front(); // pop '}'

        Ok(WhileStat { condition, body })
    }

    fn for_stat(&mut self, typelist: &TypeList) -> Result<Statement<'src>, ParseError> {
        self.tokens.pop_front(); // pop 'for'

        let first_name = self.tokens.pop_name()?;

        match self.tokens[0].kind {
            Comma => {
                self.tokens.pop_front();

                let second_name = self.tokens.pop_name()?;

                let len = second_name.as_ptr() as usize - first_name.as_ptr() as usize
                    + second_name.len();

                // this is gross
                let names = unsafe {
                    str::from_utf8_unchecked(slice::from_raw_parts(first_name.as_ptr(), len))
                };

                self.tokens.expect(In)?;

                let iter = self.parse_expr(typelist)?;

                self.tokens.expect(LCurly)?;

                let mut scoped_typelist = typelist.clone();

                let mut body = Vec::new();

                while self.tokens[0].kind != RCurly {
                    body.push(self.parse_statement(&mut scoped_typelist)?);
                }

                self.tokens.pop_front(); // '}'

                Ok(Statement::KeyValFor(KeyValFor { names, iter, body }))
            }
            In => {
                self.tokens.pop_front();

                let lhs = self.parse_range_expr(typelist)?;

                self.tokens.expect(DotDot)?;

                let rhs = self.parse_range_expr(typelist)?;

                self.tokens.expect(LCurly)?;

                let mut scoped_typelist = typelist.clone();

                let mut body = Vec::new();

                while self.tokens[0].kind != RCurly {
                    body.push(self.parse_statement(&mut scoped_typelist)?);
                }

                self.tokens.pop_front(); // '}'

                Ok(Statement::RangeFor(RangeFor {
                    var: first_name,
                    range: Box::new(RangeExpr { lhs, rhs }),
                    body,
                }))
            }
            _ => Err(ParseError::UnexpectedToken(UnexpectedToken {
                token: (&self.tokens[0]).into(),
                expected_kinds: vec![Comma, In],
            })),
        }
    }

    fn parse_expr_stat(&mut self, typelist: &mut TypeList) -> Result<Statement<'src>, ParseError> {
        let sufexpr = self.parse_suffixed_expr(typelist)?;

        if let Expr::Name(name) = self.pool[sufexpr.val] {
            if self.tokens[0].kind == Colon {
                return Ok(Statement::Declare(self.parse_decl(
                    typelist,
                    SuffixedName {
                        name,
                        suffixes: sufexpr.suffixes,
                    },
                )?));
            }

            if self.tokens[0].kind == Equal {
                return Ok(Statement::Assign(self.parse_assignment(
                    typelist,
                    SuffixedName {
                        name,
                        suffixes: sufexpr.suffixes,
                    },
                )?));
            }

            if self.tokens[0].kind != Comma {
                return Ok(Statement::ExprStat(sufexpr));
            }

            let mut lhs_arr = vec![sufexpr];

            while self.tokens[0].kind == Comma {
                self.tokens.pop_front();
                let temp = self.parse_suffixed_expr(typelist)?;
                match self.pool[temp.val] {
                    Expr::Name(..) => {
                        lhs_arr.push(temp);
                    }
                    _ => return Err(ParseError::EmptyError),
                }
            }

            return match self.tokens[0].kind {
                Colon => {
                    self.tokens.pop_front();
                    self.tokens.expect(Equal)?;

                    let rhs_arr = self.parse_expr_list(typelist)?;

                    let mut new_lhs_arr = Vec::new();

                    for SuffixedExpr { val, suffixes } in lhs_arr.into_iter() {
                        if let Expr::Name(name) = self.pool[val] {
                            assert!(suffixes.is_empty());

                            new_lhs_arr.push(name)
                        } else {
                            return Err(ParseError::EmptyError);
                        }
                    }

                    Ok(Statement::MultiDecl(MultiDecl {
                        lhs_arr: new_lhs_arr,
                        rhs_arr,
                    }))
                }
                Equal => {
                    self.tokens.pop_front();
                    let rhs_arr = self.parse_expr_list(typelist)?;

                    Ok(Statement::MultiAssign(MultiAssign { lhs_arr, rhs_arr }))
                }
                _ => Err(ParseError::UnexpectedToken(UnexpectedToken {
                    token: (&self.tokens[0]).into(),
                    expected_kinds: vec![Colon, Equal],
                })),
            };
        }

        Ok(Statement::ExprStat(sufexpr))
    }

    fn parse_suffixed_expr(
        &mut self,
        typelist: &TypeList,
    ) -> Result<SuffixedExpr<'src>, ParseError> {
        let val = self.parse_primary_expr(typelist)?;
        let mut suffixes = Vec::new();
        loop {
            match self.tokens[0].kind {
                Dot => {
                    self.tokens.pop_front();
                    suffixes.push(Suffix::Access(Access {
                        field_name: self.tokens.pop_name()?,
                    }));
                }
                Colon => {
                    if self.tokens[1].kind == Equal {
                        return Ok(SuffixedExpr { val, suffixes });
                    }

                    if self.tokens[1].kind == Name && self.tokens[2].kind == LParen {
                        self.tokens.pop_front(); // ':'
                        let name = self.tokens[0].str;
                        self.tokens.pop_front(); // name

                        if self.tokens[0].kind != LParen {
                            return Err(ParseError::UnexpectedToken(UnexpectedToken {
                                token: (&self.tokens[0]).into(),
                                expected_kinds: vec![LParen],
                            }));
                        }

                        suffixes.push(Suffix::Method(Method {
                            method_name: name,
                            args: self.parse_func_args(typelist)?,
                        }));
                    } else {
                        return Ok(SuffixedExpr { val, suffixes });
                    }
                }
                LSquare => {
                    suffixes.push(Suffix::Index(self.parse_index(typelist)?));
                }
                LParen => {
                    suffixes.push(Suffix::Call(Call {
                        args: self.parse_func_args(typelist)?,
                    }));
                }
                _ => return Ok(SuffixedExpr { val, suffixes }),
            }
        }
    }

    fn parse_index(&mut self, typelist: &TypeList) -> Result<Index, ParseError> {
        self.tokens.pop_front();

        let result = Index {
            key: self.parse_expr(typelist)?,
        };

        self.tokens.expect(RSquare)?;

        Ok(result)
    }

    fn parse_expr_list(&mut self, typelist: &TypeList) -> Result<Vec<ExprRef>, ParseError> {
        let mut result = Vec::new();
        result.push(self.parse_expr(typelist)?);

        while self.tokens[0].kind == Comma {
            self.tokens.pop_front();
            result.push(self.parse_expr(typelist)?);
        }

        Ok(result)
    }

    fn parse_func_args(&mut self, typelist: &TypeList) -> Result<Vec<ExprRef>, ParseError> {
        self.tokens.pop_front(); // pop '('
        if self.tokens[0].kind == RParen {
            self.tokens.pop_front();
            return Ok(Vec::new());
        }

        let mut result = Vec::new();
        result.push(self.parse_expr(typelist)?);

        while self.tokens[0].kind == Comma {
            self.tokens.pop_front();
            result.push(self.parse_expr(typelist)?);
        }

        self.tokens.expect(RParen)?;

        Ok(result)
    }

    fn parse_primary_expr(&mut self, typelist: &TypeList) -> Result<ExprRef, ParseError> {
        match self.tokens[0].kind {
            Name => {
                let name = self.tokens[0].str;
                self.tokens.pop_front();
                Ok(self.pool.add(Expr::Name(name)))
            }
            LParen => {
                self.tokens.pop_front();
                let val = self.parse_expr(typelist)?;
                self.tokens.expect(RParen)?;

                Ok(self.pool.add(Expr::Paren(ParenExpr { val })))
            }
            _ => Err(ParseError::UnexpectedToken(UnexpectedToken {
                token: (&self.tokens[0]).into(),
                expected_kinds: vec![Name, LParen],
            })),
        }
    }

    fn parse_expr(&mut self, typelist: &TypeList) -> Result<ExprRef, ParseError> {
        self.expr_impl(typelist, 0)
    }

    fn expr_impl(&mut self, typelist: &TypeList, limit: u8) -> Result<ExprRef, ParseError> {
        let mut result = if let Some(op) = get_unop(&self.tokens[0].kind) {
            self.tokens.pop_front();
            let val = self.expr_impl(typelist, 12)?;

            self.pool.add(Expr::UnOp(UnOp { op, val }))
        } else {
            let val = self.simple_expr(typelist)?;

            self.pool.add(Expr::Simple(val))
        };

        while let Some((op, prec)) = get_op(self.tokens[0].kind) {
            if prec.left <= limit {
                break;
            }

            self.tokens.pop_front();

            let rhs = self.expr_impl(typelist, prec.right)?;

            result = self.pool.add(Expr::BinOp(BinOp {
                op,
                lhs: result,
                rhs,
            }));
        }

        Ok(result)
    }

    fn simple_expr(&mut self, typelist: &TypeList) -> Result<SimpleExpr<'src>, ParseError> {
        let str = self.tokens[0].str;
        match self.tokens[0].kind {
            NumLit => {
                self.tokens.pop_front();
                Ok(SimpleExpr::Num(str))
            }
            StrLit => {
                self.tokens.pop_front();
                Ok(SimpleExpr::Str(str))
            }
            True | False => {
                self.tokens.pop_front();
                Ok(SimpleExpr::Bool(str))
            }
            Nil => {
                self.tokens.pop_front();
                Ok(SimpleExpr::Nil(str))
            }
            LCurly => Ok(SimpleExpr::TableNode(self.table_constructor(typelist)?)),
            Struct => Ok(SimpleExpr::StructNode(self.parse_struct(typelist)?)),
            Func => Ok(SimpleExpr::FuncNode(self.func_constructor(typelist)?)),
            _ => Ok(SimpleExpr::SuffixedExpr(
                self.parse_suffixed_expr(typelist)?,
            )),
        }
    }

    fn field(&mut self, typelist: &TypeList) -> Result<FieldNode<'src>, ParseError> {
        match self.tokens[0].kind {
            LSquare => {
                self.tokens.pop_front();
                let key = self.parse_expr(typelist)?;
                self.tokens.expect(RSquare)?;
                Ok(FieldNode::ExprField {
                    key,
                    val: self.parse_expr(typelist)?,
                })
            }
            Name => {
                if self.tokens[1].kind == Equal {
                    // key = val
                    let key = self.tokens[0].str;
                    self.tokens.pop_front(); // pop name;
                    self.tokens.pop_front(); // pop '=' now
                    Ok(FieldNode::Field {
                        key,
                        val: self.parse_expr(typelist)?,
                    })
                } else {
                    // we got bamboozled. the name was an expr this whole time.
                    Ok(FieldNode::ValField {
                        val: self.parse_expr(typelist)?,
                    })
                }
            }
            _ => Ok(FieldNode::ValField {
                val: self.parse_expr(typelist)?,
            }),
        }
    }

    fn table_constructor(&mut self, typelist: &TypeList) -> Result<TableNode<'src>, ParseError> {
        self.tokens.pop_front();

        let mut fields = Vec::new();

        if self.tokens[0].kind == RCurly {
            self.tokens.pop_front();
            return Ok(TableNode { fields });
        }

        fields.push(self.field(typelist)?);

        while self.tokens[0].kind == Comma {
            self.tokens.pop_front();
            fields.push(self.field(typelist)?);
        }

        self.tokens.expect(RCurly)?;

        Ok(TableNode { fields })
    }

    fn member(&mut self, typelist: &TypeList) -> Result<(String, Type), ParseError> {
        let name = self.tokens.pop_name()?.to_owned();
        self.tokens.expect(Colon)?;

        Ok((name, self.parse_type(typelist)?))
    }

    fn parse_struct_constructor(
        &mut self,
        typelist: &TypeList,
    ) -> Result<FuncNode<'src>, ParseError> {
        self.tokens.pop_front();
        self.tokens.expect(LParen)?;

        let mut params = Vec::new();

        if self.tokens[0].kind == RParen {
            self.tokens.pop_front();
        } else {
            loop {
                if self.tokens[0].kind == Name && self.tokens[1].kind == Colon {
                    let argname = self.tokens[0].str;
                    self.tokens.pop_front();
                    self.tokens.pop_front();

                    params.push((argname.to_owned(), self.parse_type(typelist)?));
                } else {
                    params.push(("".to_owned(), self.parse_type(typelist)?));
                }

                if self.tokens[0].kind == RParen {
                    self.tokens.pop_front();
                    break;
                }

                self.tokens.expect(Comma)?;
            }
        }

        self.tokens.expect(LCurly)?;

        let mut scoped_typelist = typelist.clone();

        let mut body = Vec::new();

        while self.tokens[0].kind != RCurly {
            body.push(self.parse_statement(&mut scoped_typelist)?);
        }

        self.tokens.pop_front();

        Ok(FuncNode {
            type_: Box::new(Function {
                params,
                returns: Box::default(),
            }),
            body,
        })
    }

    fn parse_struct(&mut self, typelist: &TypeList) -> Result<StructNode<'src>, ParseError> {
        self.tokens.pop_front(); // pop 'struct'

        self.tokens.expect(LCurly)?;

        let mut fields = Vec::new();

        if self.tokens[0].kind == RCurly {
            self.tokens.pop_front();

            return Ok(StructNode {
                type_: Box::new(User { fields }),
                constructor: None,
                name: None,
            });
        }

        fields.push(self.member(typelist)?);

        while self.tokens[0].kind == Comma {
            self.tokens.pop_front();
            if self.tokens[0].kind == RCurly {
                // optional trailing comma
                break;
            }
            fields.push(self.member(typelist)?);
        }

        let type_ = Box::new(User { fields });

        match self.tokens[0].kind {
            RCurly => {
                self.tokens.pop_front();
                Ok(StructNode {
                    type_,
                    constructor: None,
                    name: None,
                })
            }
            Constructor => {
                let constructor = Some(self.parse_struct_constructor(typelist)?);
                self.tokens.expect(RCurly)?;
                Ok(StructNode {
                    type_,
                    constructor,
                    name: None,
                })
            }
            _ => Err(ParseError::UnexpectedToken(UnexpectedToken {
                token: (&self.tokens[0]).into(),
                expected_kinds: vec![RCurly, Constructor],
            })),
        }
    }

    fn func_constructor(&mut self, typelist: &TypeList) -> Result<FuncNode<'src>, ParseError> {
        self.tokens.pop_front(); // pop 'func'
        self.tokens.expect(LParen)?;

        let mut params = Vec::new();

        if self.tokens[0].kind == RParen {
            self.tokens.pop_front();
        } else {
            loop {
                if self.tokens[0].kind == Elipsis {
                    params.push((self.tokens.pop_front().str.to_owned(), Type::Variadic));
                    self.tokens.expect(RParen)?;
                    break;
                }

                if self.tokens[0].kind == Name && self.tokens[1].kind == Colon {
                    let argname = self.tokens[0].str;
                    self.tokens.pop_front();
                    self.tokens.pop_front();

                    params.push((argname.to_owned(), self.parse_type(typelist)?));
                } else {
                    params.push(("".to_owned(), self.parse_type(typelist)?));
                }

                if self.tokens[0].kind == RParen {
                    self.tokens.pop_front();
                    break;
                }

                self.tokens.expect(Comma)?;
            }
        }

        let returns = Box::new(self.parse_return_type(typelist)?);

        self.tokens.expect(LCurly)?;

        let mut scoped_typelist = typelist.clone();

        let mut body = Vec::new();

        while self.tokens[0].kind != RCurly {
            body.push(self.parse_statement(&mut scoped_typelist)?);
        }

        self.tokens.pop_front();

        Ok(FuncNode {
            type_: Box::new(Function { params, returns }),
            body,
        })
    }

    fn parse_return_type(&mut self, typelist: &TypeList) -> Result<Type, ParseError> {
        if self.tokens[0].kind != Arrow {
            return Ok(Type::Nil);
        }

        self.tokens.pop_front(); // pop '->'

        if self.tokens[0].kind == LParen {
            self.tokens.pop_front();
            let mut result = Vec::new();
            result.push(self.parse_type(typelist)?);

            while self.tokens[0].kind == Comma {
                self.tokens.pop_front();
                result.push(self.parse_type(typelist)?);
            }

            self.tokens.expect(RParen)?;

            Ok(Type::Multiple(result))
        } else {
            Ok(self.parse_type(typelist)?)
        }
    }

    fn parse_basic_type(&mut self, typelist: &TypeList) -> Result<Type, ParseError> {
        match self.tokens[0].kind {
            Name => {
                let name = self.tokens[0].str;

                if !typelist.contains(name) {
                    return Err(ParseError::NoSuchVal(Some(name.to_owned())));
                }

                self.tokens.pop_front();

                Ok(typelist[name].clone())
            }
            Nil => {
                self.tokens.pop_front();
                Ok(Type::Nil)
            }
            LSquare => {
                self.tokens.pop_front();

                if self.tokens[0].kind == RSquare {
                    self.tokens.pop_front();

                    return Ok(Type::Table(TableType {
                        key_type: Rc::new(Type::Number),
                        val_type: Rc::new(self.parse_type(typelist)?),
                    }));
                }

                let key_type = self.parse_type(typelist)?;

                self.tokens.expect(RSquare)?;

                let val_type = self.parse_type(typelist)?;

                Ok(Type::Table(TableType {
                    key_type: Rc::new(key_type),
                    val_type: Rc::new(val_type),
                }))
            }
            Func => {
                self.tokens.pop_front();
                self.tokens.expect(LParen)?;

                let mut args = Vec::new();

                if self.tokens[0].kind == RParen {
                    self.tokens.pop_front();

                    return Ok(Type::Function(Function {
                        params: args,
                        returns: Box::new(self.parse_return_type(typelist)?),
                    }));
                }

                loop {
                    if self.tokens[1].kind == Colon {
                        let name = self.tokens.pop_name()?;
                        self.tokens.pop_front();
                        args.push((name.to_owned(), self.parse_type(typelist)?));
                    } else {
                        // unnamed param
                        args.push(("".to_owned(), self.parse_type(typelist)?));
                    }

                    if self.tokens[0].kind == RParen {
                        self.tokens.pop_front();
                        break;
                    }

                    self.tokens.expect(Comma)?
                }

                Ok(Type::Function(Function {
                    params: args,
                    returns: Box::new(self.parse_return_type(typelist)?),
                }))
            }
            _ => Err(ParseError::UnexpectedToken(UnexpectedToken {
                token: (&self.tokens[0]).into(),
                expected_kinds: vec![Name, Nil, LSquare, Func],
            })),
        }
    }

    fn parse_type(&mut self, typelist: &TypeList) -> Result<Type, ParseError> {
        let mut result = self.parse_basic_type(typelist)?;

        if self.tokens[0].kind == Question {
            self.tokens.pop_front();
            result = Type::Optional(Box::new(result));
        }

        Ok(result)
    }

    /// expr without concat operator
    fn parse_range_expr(&mut self, typelist: &TypeList) -> Result<ExprRef, ParseError> {
        self.range_expr_impl(typelist, 0)
    }

    fn range_expr_impl(&mut self, typelist: &TypeList, limit: u8) -> Result<ExprRef, ParseError> {
        let mut result = if let Some(op) = get_unop(&self.tokens[0].kind) {
            self.tokens.pop_front();
            let val = self.range_expr_impl(typelist, 12)?;

            self.pool.add(Expr::UnOp(UnOp { op, val }))
        } else {
            let val = self.simple_expr(typelist)?;

            self.pool.add(Expr::Simple(val))
        };

        while let Some((op, prec)) = get_op(self.tokens[0].kind) {
            if prec.left <= limit {
                break;
            }

            if op == OpKind::Cat {
                break;
            }

            self.tokens.pop_front();

            let rhs = self.range_expr_impl(typelist, prec.right)?;

            result = self.pool.add(Expr::BinOp(BinOp {
                op,
                lhs: result,
                rhs,
            }));
        }

        Ok(result)
    }
}

#[derive(Debug, Clone)]
pub struct TypeList {
    map: FxHashMap<String, Type>,
}

impl TypeList {
    pub fn with_core() -> Self {
        Self {
            map: FxHashMap::from_iter([
                ("number".to_owned(), Type::Number),
                ("string".to_owned(), Type::String),
                ("boolean".to_owned(), Type::Boolean),
                ("any".to_owned(), Type::Any),
            ]),
        }
    }

    pub fn insert(&mut self, k: String, v: Type) -> Option<Type> {
        self.map.insert(k, v)
    }

    pub fn contains(&self, k: &str) -> bool {
        self.map.contains_key(k)
    }
}

impl std::ops::Index<&str> for TypeList {
    type Output = Type;

    fn index(&self, index: &str) -> &Self::Output {
        self.map.index(index)
    }
}

pub struct Precedence {
    pub left: u8,
    pub right: u8,
}

fn get_op(tok: TokenKind) -> Option<(OpKind, Precedence)> {
    match tok {
        Plus => Some((
            OpKind::Add,
            Precedence {
                left: 10,
                right: 10,
            },
        )),
        Minus => Some((
            OpKind::Sub,
            Precedence {
                left: 10,
                right: 10,
            },
        )),
        Asterisk => Some((
            OpKind::Mul,
            Precedence {
                left: 11,
                right: 11,
            },
        )),
        Slash => Some((
            OpKind::Div,
            Precedence {
                left: 11,
                right: 11,
            },
        )),
        DotDot => Some((OpKind::Cat, Precedence { left: 9, right: 8 })),
        Caret => Some((
            OpKind::Pow,
            Precedence {
                left: 14,
                right: 13,
            },
        )),
        Equality => Some((OpKind::Equ, Precedence { left: 3, right: 3 })),
        Inequality => Some((OpKind::Neq, Precedence { left: 3, right: 3 })),
        Greater => Some((OpKind::Gre, Precedence { left: 3, right: 3 })),
        GreterEqual => Some((OpKind::Grq, Precedence { left: 3, right: 3 })),
        Less => Some((OpKind::Les, Precedence { left: 3, right: 3 })),
        LessEqual => Some((OpKind::Leq, Precedence { left: 3, right: 3 })),
        And => Some((OpKind::And, Precedence { left: 2, right: 2 })),
        Or => Some((OpKind::Or, Precedence { left: 1, right: 1 })),
        _ => None,
    }
}

fn get_unop(tok: &TokenKind) -> Option<UnOpKind> {
    match tok {
        Minus => Some(UnOpKind::Neg),
        Octothorpe => Some(UnOpKind::Len),
        _ => None,
    }
}
