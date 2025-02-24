pub mod ast;
pub mod pool;

use std::rc::Rc;

use rustc_hash::FxHashMap;

use self::{ast::*, pool::*};

use crate::{
    errors::{ParseError, UnexpectedToken},
    lexer::{
        Span, SpanTokens, SrcLoc,
        TokenKind::{self, *},
    },
    types::{Function, TableType, Type, TypeKind, User},
};

type PResult<'s, T> = Result<T, Box<ParseError<'s>>>;

pub struct Parser<'s, 'pool> {
    pub tokens: SpanTokens<'s>,
    pub pool: &'pool mut ExprPool<'s>,
}

impl<'s> Parser<'s, '_> {
    pub fn parse_statement(&mut self, typelist: &mut TypeList<'s>) -> PResult<'s, Statement<'s>> {
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
            Struct => Ok(Statement::StructDecl(self.parse_struct_decl(typelist)?)),
            LCurly => Ok(Statement::Block(self.parse_block(typelist)?)),
            _ => self.parse_expr_stat(typelist),
        }
    }

    fn parse_block(&mut self, typelist: &TypeList<'s>) -> PResult<'s, Block<'s>> {
        // technically unnecessary sometimes. idk, probably no biggie
        self.tokens.expect(LCurly)?; // `{`

        let mut stmts = Vec::new();
        let mut scoped_typelist = typelist.clone();
        while self.tokens[0].kind != RCurly {
            stmts.push(self.parse_statement(&mut scoped_typelist)?);
        }

        self.tokens.pop_front(); // `}`

        Ok(Block {
            stmts: stmts.into_boxed_slice(),
        })
    }

    fn parse_method_decl(&mut self, typelist: &TypeList<'s>) -> PResult<'s, MethodDecl<'s>> {
        let struct_name = self.tokens[0].text;

        self.tokens.pop_front(); // name
        self.tokens.pop_front(); // colon
        let method_name = self.tokens[0].text;

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
        typelist: &mut TypeList<'s>,
        lhs: SuffixedName<'s>,
    ) -> PResult<'s, Declare<'s>> {
        self.tokens.pop_front(); // pop ':'

        if self.tokens[0].kind == Equal {
            self.tokens.pop_front();

            let val = self.parse_expr(typelist)?;

            return Ok(Declare {
                lhs: Box::new(lhs),
                type_: Box::new(Type {
                    kind: TypeKind::Adaptable,
                    span: None,
                }),
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

        Ok(Declare {
            lhs: Box::new(lhs),
            type_,
            val: Some(val),
        })
    }

    fn parse_assignment(
        &mut self,
        typelist: &TypeList<'s>,
        lhs: SuffixedName<'s>,
    ) -> PResult<'s, Assign<'s>> {
        self.tokens.pop_front(); // pop '='

        let rhs = self.parse_expr(typelist)?;

        Ok(Assign {
            lhs: Box::new(lhs),
            rhs,
        })
    }

    fn if_stat(&mut self, typelist: &TypeList<'s>) -> PResult<'s, IfStat<'s>> {
        self.tokens.pop_front(); // pop 'if'

        let condition = self.parse_expr(typelist)?;
        let body = self.parse_block(typelist)?;

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

        let else_body = self.parse_block(typelist)?;
        let else_ = Some(Box::new(ElseBranch::Else(else_body)));

        Ok(IfStat {
            condition,
            body,
            else_,
        })
    }

    fn while_stat(&mut self, typelist: &TypeList<'s>) -> PResult<'s, WhileStat<'s>> {
        self.tokens.pop_front(); // pop 'while'

        let condition = self.parse_expr(typelist)?;
        let body = self.parse_block(typelist)?;

        Ok(WhileStat { condition, body })
    }

    fn for_stat(&mut self, typelist: &TypeList<'s>) -> PResult<'s, Statement<'s>> {
        self.tokens.pop_front(); // pop 'for'

        let first_name = self.tokens.pop_name()?;

        match self.tokens[0].kind {
            Comma => {
                self.tokens.pop_front();

                let second_name = self.tokens.pop_name()?;

                // this is gross
                let names = Span::new(first_name.start, second_name.end);

                self.tokens.expect(In)?;

                let iter = self.parse_expr(typelist)?;

                let body = self.parse_block(typelist)?;

                Ok(Statement::KeyValFor(KeyValFor { names, iter, body }))
            }
            In => {
                self.tokens.pop_front();

                let lhs = self.parse_range_expr(typelist)?;

                self.tokens.expect(DotDot)?;

                let rhs = self.parse_range_expr(typelist)?;

                let body = self.parse_block(typelist)?;

                Ok(Statement::RangeFor(RangeFor {
                    var: first_name,
                    range: Box::new(RangeExpr { lhs, rhs }),
                    body,
                }))
            }
            _ => Err(Box::new(ParseError::UnexpectedToken(UnexpectedToken {
                token: self.tokens[0].clone(),
                expected_kinds: vec![Comma, In],
            }))),
        }
    }

    fn parse_expr_stat(&mut self, typelist: &mut TypeList<'s>) -> PResult<'s, Statement<'s>> {
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
                if sufexpr.suffixes.is_empty() {
                    return Err(Box::new(ParseError::BadExprStat(
                        self.pool[sufexpr.val].clone(),
                    )));
                }
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
                    _ => return Err(Box::new(ParseError::EmptyError)),
                }
            }

            return match self.tokens[0].kind {
                Colon => {
                    self.tokens.pop_front();
                    self.tokens.expect(Equal)?;

                    let rhs_arr = self.parse_expr_list(typelist)?;

                    let mut new_lhs_arr = Vec::new();

                    for SuffixedExpr { val, suffixes } in lhs_arr {
                        if let Expr::Name(name) = self.pool[val] {
                            assert!(suffixes.is_empty());

                            new_lhs_arr.push(name);
                        } else {
                            return Err(Box::new(ParseError::EmptyError));
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
                _ => Err(Box::new(ParseError::UnexpectedToken(UnexpectedToken {
                    token: self.tokens[0].clone(),
                    expected_kinds: vec![Colon, Equal],
                }))),
            };
        }

        if sufexpr.suffixes.is_empty() {
            return Err(Box::new(ParseError::BadExprStat(
                self.pool[sufexpr.val].clone(),
            )));
        }

        Ok(Statement::ExprStat(sufexpr))
    }

    fn parse_suffixed_expr(&mut self, typelist: &TypeList<'s>) -> PResult<'s, SuffixedExpr<'s>> {
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
                        let name = self.tokens[0].text;
                        self.tokens.pop_front(); // name

                        if self.tokens[0].kind != LParen {
                            return Err(Box::new(ParseError::UnexpectedToken(UnexpectedToken {
                                token: self.tokens[0].clone(),
                                expected_kinds: vec![LParen],
                            })));
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

    fn parse_index(&mut self, typelist: &TypeList<'s>) -> PResult<'s, Index> {
        self.tokens.pop_front();

        let result = Index {
            key: self.parse_expr(typelist)?,
        };

        self.tokens.expect(RSquare)?;

        Ok(result)
    }

    fn parse_expr_list(&mut self, typelist: &TypeList<'s>) -> PResult<'s, Vec<ExprRef>> {
        let mut result = Vec::new();
        result.push(self.parse_expr(typelist)?);

        while self.tokens[0].kind == Comma {
            self.tokens.pop_front();
            result.push(self.parse_expr(typelist)?);
        }

        Ok(result)
    }

    fn parse_func_args(&mut self, typelist: &TypeList<'s>) -> PResult<'s, Vec<ExprRef>> {
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

    fn parse_primary_expr(&mut self, typelist: &TypeList<'s>) -> PResult<'s, ExprRef> {
        match self.tokens[0].kind {
            Name | Elipsis => {
                let name = self.tokens[0].text;
                self.tokens.pop_front();
                Ok(self.pool.add(Expr::Name(name)))
            }
            LParen => {
                self.tokens.pop_front();
                let val = self.parse_expr(typelist)?;
                self.tokens.expect(RParen)?;

                Ok(self.pool.add(Expr::Paren(ParenExpr { val })))
            }
            _ => Err(Box::new(ParseError::UnexpectedToken(UnexpectedToken {
                token: self.tokens[0].clone(),
                expected_kinds: vec![Name, LParen],
            }))),
        }
    }

    fn parse_expr(&mut self, typelist: &TypeList<'s>) -> PResult<'s, ExprRef> {
        self.expr_impl(typelist, 0)
    }

    fn expr_impl(&mut self, typelist: &TypeList<'s>, limit: u8) -> PResult<'s, ExprRef> {
        let mut result = if let Some(op) = get_unop(self.tokens[0].kind) {
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

    fn simple_expr(&mut self, typelist: &TypeList<'s>) -> PResult<'s, SimpleExpr<'s>> {
        let str = self.tokens[0].text;
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
            Func => Ok(SimpleExpr::FuncNode(self.func_constructor(typelist)?)),
            _ => Ok(SimpleExpr::SuffixedExpr(
                self.parse_suffixed_expr(typelist)?,
            )),
        }
    }

    fn field(&mut self, typelist: &TypeList<'s>) -> PResult<'s, FieldNode<'s>> {
        match self.tokens[0].kind {
            LSquare => {
                self.tokens.pop_front();
                let key = self.parse_expr(typelist)?;
                self.tokens.expect(RSquare)?;
                self.tokens.expect(Equal)?;
                Ok(FieldNode::ExprField {
                    key,
                    val: self.parse_expr(typelist)?,
                })
            }
            Name => {
                if self.tokens[1].kind == Equal {
                    // key = val
                    let key = self.tokens[0].text;
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

    fn table_constructor(&mut self, typelist: &TypeList<'s>) -> PResult<'s, TableNode<'s>> {
        self.tokens.pop_front();

        let mut fields = Vec::new();

        if self.tokens[0].kind == RCurly {
            self.tokens.pop_front();
            return Ok(TableNode { fields });
        }

        while self.tokens[0].kind != RCurly {
            fields.push(self.field(typelist)?);

            if self.tokens[0].kind == Comma {
                self.tokens.pop_front();
            } else {
                break;
            }
        }

        self.tokens.expect(RCurly)?;

        Ok(TableNode { fields })
    }

    fn member(&mut self, typelist: &TypeList<'s>) -> PResult<'s, (&'s str, Type<'s>)> {
        let name = self.tokens.pop_name()?;
        self.tokens.expect(Colon)?;

        Ok((name.to_str(self.tokens.source), self.parse_type(typelist)?))
    }

    fn parse_struct_constructor(&mut self, typelist: &TypeList<'s>) -> PResult<'s, FuncNode<'s>> {
        self.tokens.pop_front();
        self.tokens.expect(LParen)?;

        let mut params = Vec::new();

        if self.tokens[0].kind == RParen {
            self.tokens.pop_front();
        } else {
            loop {
                if self.tokens[0].kind == Name && self.tokens[1].kind == Colon {
                    let argname = self.tokens[0].text;
                    self.tokens.pop_front();
                    self.tokens.pop_front();

                    params.push((
                        argname.to_str(self.tokens.source),
                        self.parse_type(typelist)?,
                    ));
                } else {
                    let offset = self.tokens[0].text.start;
                    params.push((
                        Span::empty(offset).to_str(self.tokens.source),
                        self.parse_type(typelist)?,
                    ));
                }

                if self.tokens[0].kind == RParen {
                    self.tokens.pop_front();
                    break;
                }

                self.tokens.expect(Comma)?;
            }
        }

        let body = self.parse_block(typelist)?;

        Ok(FuncNode {
            type_: Box::new(Function {
                params,
                returns: Box::default(),
            }),
            body,
        })
    }

    fn parse_struct_decl(&mut self, typelist: &mut TypeList<'s>) -> PResult<'s, StructDecl<'s>> {
        let start = self.tokens.pop_front().text.start; // pop 'struct'

        let name_span = self.tokens.pop_name()?;
        let name_str = name_span.to_str(self.tokens.source);

        self.tokens.expect(LCurly)?;

        let mut fields = Vec::new();

        if self.tokens[0].kind == RCurly {
            let end_str = self.tokens.pop_front().text.end;

            let type_ = Box::new(User {
                fields,
                name: name_str,
            });
            typelist.insert(
                name_str.to_owned(),
                Type {
                    kind: TypeKind::User(*type_.clone()),
                    span: Some(Span::new(start, end_str)),
                },
            );
            return Ok(StructDecl {
                type_,
                constructor: None,
                name: name_span,
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

        let type_ = Box::new(User { fields, name: name_str });
        typelist.insert(
            name_str.to_owned(),
            Type {
                kind: TypeKind::User(*type_.clone()),
                span: Some(Span::new(start, self.tokens[0].text.start)),
            },
        );

        match self.tokens[0].kind {
            RCurly => {
                self.tokens.pop_front();
                Ok(StructDecl {
                    type_,
                    constructor: None,
                    name: name_span,
                })
            }
            Constructor => {
                let constructor = Some(self.parse_struct_constructor(typelist)?);
                self.tokens.expect(RCurly)?;
                Ok(StructDecl {
                    name: name_span,
                    type_,
                    constructor,
                })
            }
            _ => Err(Box::new(ParseError::UnexpectedToken(UnexpectedToken {
                token: self.tokens[0].clone(),
                expected_kinds: vec![RCurly, Constructor],
            }))),
        }
    }

    fn parse_func_header(&mut self, typelist: &TypeList<'s>) -> PResult<'s, Function<'s>> {
        self.tokens.pop_front(); // pop 'func'
        self.tokens.expect(LParen)?;

        let mut params = Vec::new();

        if self.tokens[0].kind == RParen {
            self.tokens.pop_front();
        } else {
            loop {
                if self.tokens[0].kind == Elipsis {
                    let span = self.tokens.pop_front().text;
                    params.push((
                        span.to_str(self.tokens.source),
                        Type {
                            kind: TypeKind::Variadic,
                            span: Some(span),
                        },
                    ));
                    self.tokens.expect(RParen)?;
                    break;
                }

                if self.tokens[0].kind == Name && self.tokens[1].kind == Colon {
                    let argname = self.tokens[0].text.to_str(self.tokens.source);
                    self.tokens.pop_front();
                    self.tokens.pop_front();

                    params.push((argname, self.parse_type(typelist)?));
                } else {
                    let offset = self.tokens[0].text.start;
                    params.push((
                        Span::empty(offset).to_str(self.tokens.source),
                        self.parse_type(typelist)?,
                    ));
                }

                if self.tokens[0].kind == RParen {
                    self.tokens.pop_front();
                    break;
                }

                self.tokens.expect(Comma)?;
            }
        }

        let returns = Box::new(self.parse_return_type(typelist)?);

        Ok(Function { params, returns })
    }

    fn func_constructor(&mut self, typelist: &TypeList<'s>) -> PResult<'s, FuncNode<'s>> {
        let ty = self.parse_func_header(typelist)?;

        let body = self.parse_block(typelist)?;

        Ok(FuncNode {
            type_: Box::new(ty),
            body,
        })
    }

    fn parse_return_type(&mut self, typelist: &TypeList<'s>) -> PResult<'s, Type<'s>> {
        if self.tokens[0].kind != Arrow {
            return Ok(Type {
                kind: TypeKind::Nil,
                span: Some(Span::empty(self.tokens[0].text.start)),
            });
        }

        self.tokens.pop_front(); // pop '->'
        let ty_start = self.tokens[0].text.start;
        if self.tokens[0].kind == LParen {
            self.tokens.pop_front();
            let mut result = Vec::new();
            result.push(self.parse_type(typelist)?);

            while self.tokens[0].kind == Comma {
                self.tokens.pop_front();
                result.push(self.parse_type(typelist)?);
            }

            let ty_end = self.tokens.expect(RParen)?.text.end;

            Ok(Type {
                kind: TypeKind::Multiple(result),
                span: Some(Span::new(ty_start, ty_end)),
            })
        } else {
            Ok(self.parse_type(typelist)?)
        }
    }

    fn parse_basic_type(&mut self, typelist: &TypeList<'s>) -> PResult<'s, Type<'s>> {
        match self.tokens[0].kind {
            Name => {
                let name = self.tokens.pop_front().text;

                if !typelist.contains(name.to_str(self.tokens.source)) {
                    return Err(Box::new(ParseError::NoSuchType(name)));
                }

                Ok(Type {
                    kind: typelist[name.to_str(self.tokens.source)].kind.clone(),
                    span: Some(name),
                })
            }
            Nil => {
                let nil = self.tokens.pop_front();
                Ok(Type {
                    kind: TypeKind::Nil,
                    span: Some(nil.text),
                })
            }
            LSquare => {
                self.tokens.pop_front();

                if self.tokens[0].kind == RSquare {
                    self.tokens.pop_front();

                    return Ok(Type {
                        kind: TypeKind::Table(TableType {
                            key_type: Rc::new(Type {
                                kind: TypeKind::Number,
                                span: None,
                            }),
                            val_type: Rc::new(self.parse_type(typelist)?),
                        }),
                        span: None,
                    });
                }

                let key_type = self.parse_type(typelist)?;

                self.tokens.expect(RSquare)?;

                let val_type = self.parse_type(typelist)?;

                Ok(Type {
                    kind: TypeKind::Table(TableType {
                        key_type: Rc::new(key_type),
                        val_type: Rc::new(val_type),
                    }),
                    span: None,
                })
            }
            Func => {
                let func_span = self.tokens[0].text;
                let ty = self.parse_func_header(typelist)?;
                let src = match ty.params.last() {
                    Some((last_str, _)) => {
                        Span::offset_len(func_span.start, last_str.len() as SrcLoc)
                    }
                    None => func_span,
                };
                Ok(Type {
                    kind: TypeKind::Function(ty),
                    span: Some(src),
                })
            }
            _ => Err(Box::new(ParseError::UnexpectedToken(UnexpectedToken {
                token: self.tokens[0].clone(),
                expected_kinds: vec![Name, Nil, LSquare, Func],
            }))),
        }
    }

    fn parse_type(&mut self, typelist: &TypeList<'s>) -> PResult<'s, Type<'s>> {
        let mut result = self.parse_basic_type(typelist)?;

        if self.tokens[0].kind == Question {
            let question = self.tokens.pop_front().text;

            let span = match result.span {
                Some(span) => Span::cover(span, question),
                None => question,
            };

            result = Type {
                kind: TypeKind::Optional(Box::new(result)),
                span: Some(span),
            };
        }

        Ok(result)
    }

    /// expr without concat operator
    fn parse_range_expr(&mut self, typelist: &TypeList<'s>) -> PResult<'s, ExprRef> {
        self.range_expr_impl(typelist, 0)
    }

    fn range_expr_impl(&mut self, typelist: &TypeList<'s>, limit: u8) -> PResult<'s, ExprRef> {
        let mut result = if let Some(op) = get_unop(self.tokens[0].kind) {
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
pub struct TypeList<'s> {
    map: FxHashMap<String, Type<'s>>,
}

impl<'s> TypeList<'s> {
    pub fn with_core() -> Self {
        Self {
            map: FxHashMap::from_iter([
                (
                    "number".to_owned(),
                    Type {
                        kind: TypeKind::Number,
                        span: None,
                    },
                ),
                (
                    "string".to_owned(),
                    Type {
                        kind: TypeKind::String,
                        span: None,
                    },
                ),
                (
                    "boolean".to_owned(),
                    Type {
                        kind: TypeKind::Boolean,
                        span: None,
                    },
                ),
                (
                    "any".to_owned(),
                    Type {
                        kind: TypeKind::Any,
                        span: None,
                    },
                ),
            ]),
        }
    }

    #[inline]
    pub fn insert(&mut self, k: String, v: Type<'s>) -> Option<Type<'s>> {
        self.map.insert(k, v)
    }

    #[inline]
    pub fn contains(&self, k: &str) -> bool {
        self.map.contains_key(k)
    }
}

impl<'s> std::ops::Index<&str> for TypeList<'s> {
    type Output = Type<'s>;

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
        Percent => Some((
            OpKind::Mod,
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

fn get_unop(tok: TokenKind) -> Option<UnOpKind> {
    match tok {
        Minus => Some(UnOpKind::Neg),
        Octothorpe => Some(UnOpKind::Len),
        Not => Some(UnOpKind::Not),
        _ => None,
    }
}
