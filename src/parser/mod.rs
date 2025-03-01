pub mod ast;
pub mod pool;

use crate::{
    errors::{DiagCtx, ParseError, UnexpectedToken},
    lexer::{
        Span, SpanTokens,
        TokenKind::{self, *},
    },
    typecheck::ctx::TypeContext,
};

use self::{
    ast::*,
    pool::{ExprPool, ExprRef},
};

type PResult<'s, T> = Result<T, Box<ParseError<'s>>>;

pub struct Parser<'a, 's> {
    pub tokens: SpanTokens<'s>,
    pub expr_pool: &'a mut ExprPool<'s>,
}

impl<'a, 's> Parser<'a, 's> {
    pub const fn new(tokens: SpanTokens<'s>, expr_pool: &'a mut ExprPool<'s>) -> Self {
        Parser { tokens, expr_pool }
    }

    pub fn into_diag_ctx(self, tcx: &'a TypeContext<'s>) -> DiagCtx<'a, 's> {
        DiagCtx {
            tcx,
            expr_pool: self.expr_pool,
            source: self.tokens.source,
        }
    }
}

impl<'s> Parser<'_, 's> {
    pub fn parse_stmt(&mut self) -> PResult<'s, Stmt<'s>> {
        match self.tokens[0].kind {
            Name => {
                if self.tokens[1].kind == Colon
                    && self.tokens[2].kind == Name
                    && self.tokens[3].kind == Colon
                {
                    Ok(Stmt::MethodDecl(self.parse_method_decl()?))
                } else {
                    self.parse_expr_stmt()
                }
            }
            If => Ok(Stmt::IfStmt(self.if_stmt()?)),
            While => Ok(Stmt::WhileStmt(self.while_stmt()?)),
            For => self.for_stmt(),
            Return => {
                let kw_span = self.tokens.pop_front().text;
                Ok(Stmt::Return(ReturnStmt {
                    vals: self.parse_expr_list()?,
                    kw_span,
                }))
            }
            Break => {
                self.tokens.pop_front();
                Ok(Stmt::Break)
            }
            Struct => Ok(Stmt::StructDecl(self.parse_struct_decl()?)),
            LCurly => Ok(Stmt::Block(self.parse_block()?)),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_block(&mut self) -> PResult<'s, Block<'s>> {
        // technically unnecessary sometimes. idk, probably no biggie
        self.tokens.expect(LCurly)?; // `{`

        let mut stmts = Vec::new();
        while self.tokens[0].kind != RCurly {
            stmts.push(self.parse_stmt()?);
        }

        self.tokens.pop_front(); // `}`

        Ok(Block {
            stmts: stmts.into_boxed_slice(),
        })
    }

    fn parse_method_decl(&mut self) -> PResult<'s, MethodDecl<'s>> {
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
            func: Box::new(self.func_constructor()?),
        })
    }

    fn parse_decl(&mut self, lhs: SuffixedName<'s>) -> PResult<'s, Declare<'s>> {
        self.tokens.pop_front(); // pop ':'

        if self.tokens[0].kind == Equal {
            self.tokens.pop_front();

            let val = self.parse_expr()?;

            return Ok(Declare {
                lhs: Box::new(lhs),
                ty: None,
                val: Some(val),
            });
        }

        let ty = Box::new(self.parse_type()?);

        if self.tokens[0].kind != Equal {
            return Ok(Declare {
                lhs: Box::new(lhs),
                ty: Some(ty),
                val: None,
            });
        }

        self.tokens.pop_front(); // pop '='

        let val = self.parse_expr()?;

        Ok(Declare {
            lhs: Box::new(lhs),
            ty: Some(ty),
            val: Some(val),
        })
    }

    fn parse_assignment(&mut self, lhs: SuffixedName<'s>) -> PResult<'s, Assign<'s>> {
        self.tokens.pop_front(); // pop '='

        let rhs = self.parse_expr()?;

        Ok(Assign {
            lhs: Box::new(lhs),
            rhs,
        })
    }

    fn if_stmt(&mut self) -> PResult<'s, IfStmt<'s>> {
        self.tokens.pop_front(); // pop 'if'

        let condition = self.parse_expr()?;
        let body = self.parse_block()?;

        if self.tokens[0].kind != Else {
            return Ok(IfStmt {
                condition,
                body,
                else_: None,
            });
        }

        self.tokens.pop_front(); // pop 'else'

        if self.tokens[0].kind == If {
            let else_ = Some(Box::new(ElseBranch::ElseIf(self.if_stmt()?)));

            return Ok(IfStmt {
                condition,
                body,
                else_,
            });
        }

        let else_body = self.parse_block()?;
        let else_ = Some(Box::new(ElseBranch::Else(else_body)));

        Ok(IfStmt {
            condition,
            body,
            else_,
        })
    }

    fn while_stmt(&mut self) -> PResult<'s, WhileStmt<'s>> {
        self.tokens.pop_front(); // pop 'while'

        let condition = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(WhileStmt { condition, body })
    }

    fn for_stmt(&mut self) -> PResult<'s, Stmt<'s>> {
        self.tokens.pop_front(); // pop 'for'

        let first_name = self.tokens.pop_name()?;

        match self.tokens[0].kind {
            Comma => {
                self.tokens.pop_front();

                let second_name = self.tokens.pop_name()?;

                // this is gross
                let names = Span::new(first_name.start, second_name.end);

                self.tokens.expect(In)?;

                let iter = self.parse_expr()?;

                let body = self.parse_block()?;

                Ok(Stmt::KeyValFor(KeyValFor { names, iter, body }))
            }
            In => {
                self.tokens.pop_front();

                let lhs = self.parse_range_expr()?;

                self.tokens.expect(DotDot)?;

                let rhs = self.parse_range_expr()?;

                let body = self.parse_block()?;

                Ok(Stmt::RangeFor(RangeFor {
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

    fn parse_expr_stmt(&mut self) -> PResult<'s, Stmt<'s>> {
        let sufexpr = self.parse_suffixed_expr()?;

        if let Expr::Name(name) = self.expr_pool[sufexpr.val] {
            if self.tokens[0].kind == Colon {
                return Ok(Stmt::Declare(self.parse_decl(SuffixedName {
                    name,
                    suffixes: sufexpr.suffixes,
                })?));
            }

            if self.tokens[0].kind == Equal {
                return Ok(Stmt::Assign(self.parse_assignment(SuffixedName {
                    name,
                    suffixes: sufexpr.suffixes,
                })?));
            }

            if self.tokens[0].kind != Comma {
                if sufexpr.suffixes.is_empty() {
                    return Err(Box::new(ParseError::BadExprStmt(
                        self.expr_pool[sufexpr.val].clone(),
                    )));
                }
                return Ok(Stmt::ExprStmt(sufexpr));
            }

            let mut lhs_arr = vec![sufexpr];

            while self.tokens[0].kind == Comma {
                self.tokens.pop_front();
                let temp = self.parse_suffixed_expr()?;
                match self.expr_pool[temp.val] {
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

                    let rhs_arr = self.parse_expr_list()?;

                    let mut new_lhs_arr = Vec::new();

                    for SuffixedExpr { val, suffixes } in lhs_arr {
                        if let Expr::Name(name) = self.expr_pool[val] {
                            assert!(suffixes.is_empty());

                            new_lhs_arr.push(name);
                        } else {
                            return Err(Box::new(ParseError::EmptyError));
                        }
                    }

                    Ok(Stmt::MultiDecl(MultiDecl {
                        lhs_arr: new_lhs_arr,
                        rhs_arr,
                    }))
                }
                Equal => {
                    self.tokens.pop_front();
                    let rhs_arr = self.parse_expr_list()?;

                    Ok(Stmt::MultiAssign(MultiAssign { lhs_arr, rhs_arr }))
                }
                _ => Err(Box::new(ParseError::UnexpectedToken(UnexpectedToken {
                    token: self.tokens[0].clone(),
                    expected_kinds: vec![Colon, Equal],
                }))),
            };
        }

        if sufexpr.suffixes.is_empty() {
            return Err(Box::new(ParseError::BadExprStmt(
                self.expr_pool[sufexpr.val].clone(),
            )));
        }

        Ok(Stmt::ExprStmt(sufexpr))
    }

    fn parse_suffixed_expr(&mut self) -> PResult<'s, SuffixedExpr<'s>> {
        let val = self.parse_primary_expr()?;
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
                            args: self.parse_func_args()?,
                        }));
                    } else {
                        return Ok(SuffixedExpr { val, suffixes });
                    }
                }
                LSquare => {
                    suffixes.push(Suffix::Index(self.parse_index()?));
                }
                LParen => {
                    suffixes.push(Suffix::Call(Call {
                        args: self.parse_func_args()?,
                    }));
                }
                _ => return Ok(SuffixedExpr { val, suffixes }),
            }
        }
    }

    fn parse_index(&mut self) -> PResult<'s, Index<'s>> {
        let start = self.tokens.pop_front().text.start;

        let key = self.parse_expr()?;

        let end = self.tokens.expect(RSquare)?.text.end;

        Ok(Index {
            key,
            span: Span::new(start, end),
        })
    }

    fn parse_expr_list(&mut self) -> PResult<'s, Vec<ExprRef>> {
        let mut result = Vec::new();
        result.push(self.parse_expr()?);

        while self.tokens[0].kind == Comma {
            self.tokens.pop_front();
            result.push(self.parse_expr()?);
        }

        Ok(result)
    }

    fn parse_func_args(&mut self) -> PResult<'s, Vec<ExprRef>> {
        self.tokens.pop_front(); // pop '('
        if self.tokens[0].kind == RParen {
            self.tokens.pop_front();
            return Ok(Vec::new());
        }

        let mut result = Vec::new();
        result.push(self.parse_expr()?);

        while self.tokens[0].kind == Comma {
            self.tokens.pop_front();
            result.push(self.parse_expr()?);
        }

        self.tokens.expect(RParen)?;

        Ok(result)
    }

    fn parse_primary_expr(&mut self) -> PResult<'s, ExprRef> {
        match self.tokens[0].kind {
            Name | Elipsis => {
                let name = self.tokens[0].text;
                self.tokens.pop_front();
                Ok(self.expr_pool.add(Expr::Name(name)))
            }
            LParen => {
                self.tokens.pop_front();
                let val = self.parse_expr()?;
                self.tokens.expect(RParen)?;

                Ok(self.expr_pool.add(Expr::Paren(ParenExpr { val })))
            }
            _ => Err(Box::new(ParseError::UnexpectedToken(UnexpectedToken {
                token: self.tokens[0].clone(),
                expected_kinds: vec![Name, Elipsis, LParen],
            }))),
        }
    }

    fn parse_expr(&mut self) -> PResult<'s, ExprRef> {
        self.expr_impl(0)
    }

    fn expr_impl(&mut self, limit: u8) -> PResult<'s, ExprRef> {
        let mut result = if let Some(op) = get_unop(self.tokens[0].kind) {
            let op_span = self.tokens.pop_front().text;
            let val = self.expr_impl(12)?;

            self.expr_pool.add(Expr::UnOp(UnOp { op, op_span, val }))
        } else {
            let val = self.simple_expr()?;

            self.expr_pool.add(Expr::Simple(val))
        };

        while let Some((op, prec)) = get_op(self.tokens[0].kind) {
            if prec.left <= limit {
                break;
            }

            self.tokens.pop_front();

            let rhs = self.expr_impl(prec.right)?;

            result = self.expr_pool.add(Expr::BinOp(BinOp {
                op,
                lhs: result,
                rhs,
            }));
        }

        Ok(result)
    }

    fn simple_expr(&mut self) -> PResult<'s, SimpleExpr<'s>> {
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
            LCurly => Ok(SimpleExpr::TableNode(self.table_constructor()?)),
            Func => Ok(SimpleExpr::FuncNode(self.func_constructor()?)),
            _ => Ok(SimpleExpr::SuffixedExpr(self.parse_suffixed_expr()?)),
        }
    }

    fn field(&mut self) -> PResult<'s, FieldNode<'s>> {
        match self.tokens[0].kind {
            LSquare => {
                self.tokens.pop_front();
                let key = self.parse_expr()?;
                self.tokens.expect(RSquare)?;
                self.tokens.expect(Equal)?;
                Ok(FieldNode::ExprField {
                    key,
                    val: self.parse_expr()?,
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
                        val: self.parse_expr()?,
                    })
                } else {
                    // we got bamboozled. the name was an expr this whole time.
                    Ok(FieldNode::ValField {
                        val: self.parse_expr()?,
                    })
                }
            }
            _ => Ok(FieldNode::ValField {
                val: self.parse_expr()?,
            }),
        }
    }

    fn table_constructor(&mut self) -> PResult<'s, TableNode<'s>> {
        self.tokens.pop_front();

        let mut fields = Vec::new();

        if self.tokens[0].kind == RCurly {
            self.tokens.pop_front();
            return Ok(TableNode { fields });
        }

        while self.tokens[0].kind != RCurly {
            fields.push(self.field()?);

            if self.tokens[0].kind == Comma {
                self.tokens.pop_front();
            } else {
                break;
            }
        }

        self.tokens.expect(RCurly)?;

        Ok(TableNode { fields })
    }

    fn member(&mut self) -> PResult<'s, Member<'s>> {
        let name = self.tokens.pop_name()?;
        self.tokens.expect(Colon)?;

        Ok(Member {
            name,
            ty: self.parse_type()?,
        })
    }

    fn parse_struct_constructor(&mut self) -> PResult<'s, FuncNode<'s>> {
        let start = self.tokens.pop_front().text.start;
        self.tokens.expect(LParen)?;

        let mut params = Vec::new();

        let end = if self.tokens[0].kind == RParen {
            self.tokens.pop_front().text.end
        } else {
            loop {
                if self.tokens[0].kind == Name && self.tokens[1].kind == Colon {
                    let argname = self.tokens[0].text;
                    self.tokens.pop_front();
                    self.tokens.pop_front();

                    params.push(Param {
                        name: argname,
                        ty: self.parse_type()?,
                    });
                } else {
                    let offset = self.tokens[0].text.start;
                    params.push(Param {
                        name: Span::empty(offset),
                        ty: self.parse_type()?,
                    });
                }

                if self.tokens[0].kind == RParen {
                    break self.tokens.pop_front().text.end;
                }

                self.tokens.expect(Comma)?;
            }
        };

        let body = self.parse_block()?;

        Ok(FuncNode {
            ty: Box::new(FunctionType {
                params,
                header_span: Span::new(start, end),
                return_type: None,
            }),
            body,
        })
    }

    fn parse_struct_decl(&mut self) -> PResult<'s, StructDecl<'s>> {
        let _start = self.tokens.pop_front().text.start; // pop 'struct'

        let name = self.tokens.pop_name()?;

        self.tokens.expect(LCurly)?;

        let mut members = Vec::new();

        if self.tokens[0].kind == RCurly {
            let _end_str = self.tokens.pop_front().text.end;
            return Ok(StructDecl {
                name,
                members,
                constructor: None,
            });
        }

        members.push(self.member()?);

        while self.tokens[0].kind == Comma {
            self.tokens.pop_front();
            if self.tokens[0].kind == RCurly {
                // optional trailing comma
                break;
            }
            members.push(self.member()?);
        }

        match self.tokens[0].kind {
            RCurly => {
                self.tokens.pop_front();
                Ok(StructDecl {
                    name,
                    members,
                    constructor: None,
                })
            }
            Constructor => {
                let constructor = Some(Box::new(self.parse_struct_constructor()?));
                self.tokens.expect(RCurly)?;
                Ok(StructDecl {
                    name,
                    members,
                    constructor,
                })
            }
            _ => Err(Box::new(ParseError::UnexpectedToken(UnexpectedToken {
                token: self.tokens[0].clone(),
                expected_kinds: vec![RCurly, Constructor],
            }))),
        }
    }

    fn parse_func_header(&mut self) -> PResult<'s, FunctionType<'s>> {
        let start = self.tokens.pop_front().text.start; // pop 'func'
        self.tokens.expect(LParen)?;

        let mut params = Vec::new();

        let end = if self.tokens[0].kind == RParen {
            self.tokens.pop_front().text.end
        } else {
            loop {
                if self.tokens[0].kind == Elipsis {
                    let span = self.tokens.pop_front().text;
                    params.push(Param {
                        name: Span::empty(span.start),
                        ty: TypeNode::VariadicType(span),
                    });
                    break self.tokens.expect(RParen)?.text.end;
                    // TODO: diagnostic here for params after variadic
                }

                if self.tokens[0].kind == Name && self.tokens[1].kind == Colon {
                    let name = self.tokens[0].text;
                    self.tokens.pop_front();
                    self.tokens.pop_front();

                    params.push(Param {
                        name,
                        ty: self.parse_type()?,
                    });
                } else {
                    let offset = self.tokens[0].text.start;
                    params.push(Param {
                        name: Span::empty(offset),
                        ty: self.parse_type()?,
                    });
                }

                if self.tokens[0].kind == RParen {
                    break self.tokens.pop_front().text.end;
                }

                self.tokens.expect(Comma)?;
            }
        };

        let return_type = self.parse_return_type()?.map(Box::new);

        Ok(FunctionType {
            params,
            header_span: Span::new(start, end),
            return_type,
        })
    }

    fn func_constructor(&mut self) -> PResult<'s, FuncNode<'s>> {
        let ty = self.parse_func_header()?;

        let body = self.parse_block()?;

        Ok(FuncNode {
            ty: Box::new(ty),
            body,
        })
    }

    fn parse_return_type(&mut self) -> PResult<'s, Option<ReturnType<'s>>> {
        if self.tokens[0].kind != Arrow {
            return Ok(None);
        }

        self.tokens.pop_front(); // pop '->'
        let ty_start = self.tokens[0].text.start;
        let ty = if self.tokens[0].kind == LParen {
            self.tokens.pop_front();
            let mut types = Vec::new();
            types.push(self.parse_type()?);

            while self.tokens[0].kind == Comma {
                self.tokens.pop_front();
                types.push(self.parse_type()?);
            }

            let ty_end = self.tokens.expect(RParen)?.text.end;

            ReturnType::Multiple(MultipleType {
                types,
                span: Span::new(ty_start, ty_end),
            })
        } else {
            ReturnType::Single(self.parse_type()?)
        };

        Ok(Some(ty))
    }

    fn parse_basic_type(&mut self) -> PResult<'s, TypeNode<'s>> {
        match self.tokens[0].kind {
            Name | Nil => {
                let name = self.tokens.pop_front().text;
                Ok(TypeNode::Name(name))
            }
            LSquare => {
                let start = self.tokens.pop_front().text.start;

                if self.tokens[0].kind == RSquare {
                    self.tokens.pop_front();

                    return Ok(TypeNode::TableType(TableType {
                        key_type: None,
                        key_span: Span::empty(start),
                        val_type: Box::new(self.parse_type()?),
                    }));
                }

                let key_type = self.parse_type()?;

                let end = self.tokens.expect(RSquare)?.text.end;

                let val_type = self.parse_type()?;

                Ok(TypeNode::TableType(TableType {
                    key_type: Some(Box::new(key_type)),
                    key_span: Span::new(start, end),
                    val_type: Box::new(val_type),
                }))
            }
            Func => {
                let ty = self.parse_func_header()?;
                Ok(TypeNode::FunctionType(ty))
            }
            _ => Err(Box::new(ParseError::UnexpectedToken(UnexpectedToken {
                token: self.tokens[0].clone(),
                expected_kinds: vec![Name, Nil, LSquare, Func],
            }))),
        }
    }

    fn parse_type(&mut self) -> PResult<'s, TypeNode<'s>> {
        let mut result = self.parse_basic_type()?;

        if self.tokens[0].kind == Question {
            let question = self.tokens.pop_front().text;

            // let span = match result.span {
            //     Some(span) => Span::cover(span, question),
            //     None => question,
            // };

            result = TypeNode::OptionalType(OptionalType {
                inner: Box::new(result),
                question,
            });
        }

        Ok(result)
    }

    /// expr without concat operator
    fn parse_range_expr(&mut self) -> PResult<'s, ExprRef> {
        self.range_expr_impl(0)
    }

    fn range_expr_impl(&mut self, limit: u8) -> PResult<'s, ExprRef> {
        let mut result = if let Some(op) = get_unop(self.tokens[0].kind) {
            let op_span = self.tokens.pop_front().text;
            let val = self.range_expr_impl(12)?;

            self.expr_pool.add(Expr::UnOp(UnOp { op, op_span, val }))
        } else {
            let val = self.simple_expr()?;

            self.expr_pool.add(Expr::Simple(val))
        };

        while let Some((op, prec)) = get_op(self.tokens[0].kind) {
            if prec.left <= limit {
                break;
            }

            if op == OpKind::Cat {
                break;
            }

            self.tokens.pop_front();

            let rhs = self.range_expr_impl(prec.right)?;

            result = self.expr_pool.add(Expr::BinOp(BinOp {
                op,
                lhs: result,
                rhs,
            }));
        }

        Ok(result)
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
