pub mod ast;
pub mod error;
pub mod pool;

use crate::{
    error::DiagCtx,
    lexer::{
        TokenKind::{self, *},
        Tokens,
    },
    parser::error::{BadExprStmt, ExprInDeclLHS, ParseError},
    sourcemap::SourceFile,
    typecheck::ctx::TypeContext,
    utils::Span,
};

use self::{
    ast::*,
    error::UnexpectedToken,
    pool::{ExprPool, ExprRef},
};

type PResult<T> = Result<T, Box<ParseError>>;

pub struct Parser<'a> {
    pub file: &'a SourceFile,
    pub tokens: Tokens,
    pub expr_pool: &'a mut ExprPool,
}

impl<'a> Parser<'a> {
    pub const fn new(file: &'a SourceFile, tokens: Tokens, expr_pool: &'a mut ExprPool) -> Self {
        Parser {
            file,
            tokens,
            expr_pool,
        }
    }

    pub(crate) fn into_diag_ctx(self, tcx: &'a TypeContext) -> DiagCtx<'a> {
        DiagCtx {
            tcx,
            expr_pool: self.expr_pool,
            file: self.file,
        }
    }
}

impl Parser<'_> {
    pub fn parse_stmt(&mut self) -> PResult<Stmt> {
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
                let kw_span = self.tokens.pop_front().span;
                let vals = match self.tokens[0].kind {
                    RCurly | EndOfFile => Vec::new(),
                    _ => self.parse_expr_list()?,
                };
                Ok(Stmt::Return(ReturnStmt { vals, kw_span }))
            }
            Break => {
                let span = self.tokens.pop_front().span;
                Ok(Stmt::Break(span))
            }
            Struct => Ok(Stmt::StructDecl(self.parse_struct_decl()?)),
            LCurly => Ok(Stmt::Block(self.parse_block_spanned()?)),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_block(&mut self) -> PResult<Block> {
        // technically unnecessary sometimes. idk, probably no biggie
        let start = self.tokens.expect(LCurly)?.span.start;

        let mut stmts = Vec::new();
        while self.tokens[0].kind != RCurly {
            stmts.push(self.parse_stmt()?);
        }

        let end = self.tokens.pop_front().span.end;

        if stmts.is_empty() {
            Ok(Block::None(Span::new(start, end)))
        } else {
            Ok(Block::Some(stmts.into_boxed_slice()))
        }
    }

    fn parse_block_spanned(&mut self) -> PResult<SpannedBlock> {
        // technically unnecessary sometimes. idk, probably no biggie
        let start = self.tokens.expect(LCurly)?.span.start;

        let mut stmts = Vec::new();
        while self.tokens[0].kind != RCurly {
            stmts.push(self.parse_stmt()?);
        }

        let end = self.tokens.pop_front().span.end;

        Ok(SpannedBlock {
            stmts: stmts.into_boxed_slice(),
            span: Span::new(start, end),
        })
    }

    fn parse_method_decl(&mut self) -> PResult<MethodDecl> {
        let struct_name = self.tokens[0].span;

        self.tokens.pop_front(); // name
        self.tokens.pop_front(); // colon
        let method_name = self.tokens[0].span;

        self.tokens.pop_front(); // method name
        self.tokens.pop_front(); // colon
        self.tokens.expect(Equal)?;
        Ok(MethodDecl {
            struct_name,
            method_name,
            func: Box::new(self.func_constructor()?),
        })
    }

    fn parse_decl(&mut self, lhs: SuffixedName) -> PResult<Declare> {
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

    fn parse_assignment(&mut self, lhs: SuffixedName) -> PResult<Assign> {
        self.tokens.pop_front(); // pop '='

        let rhs = self.parse_expr()?;

        Ok(Assign {
            lhs: Box::new(lhs),
            rhs,
        })
    }

    fn if_stmt(&mut self) -> PResult<IfStmt> {
        let kw_span = self.tokens.pop_front().span; // pop 'if'

        let condition = self.parse_expr()?;
        let body = self.parse_block_spanned()?;

        if self.tokens[0].kind != Else {
            return Ok(IfStmt {
                condition,
                body,
                else_: None,
                kw_span,
            });
        }

        self.tokens.pop_front(); // pop 'else'

        if self.tokens[0].kind == If {
            let else_ = Some(Box::new(ElseBranch::ElseIf(self.if_stmt()?)));

            return Ok(IfStmt {
                condition,
                body,
                else_,
                kw_span,
            });
        }

        let else_body = self.parse_block_spanned()?;
        let else_ = Some(Box::new(ElseBranch::Else(else_body)));

        Ok(IfStmt {
            condition,
            body,
            else_,
            kw_span,
        })
    }

    fn while_stmt(&mut self) -> PResult<WhileStmt> {
        let kw_span = self.tokens.pop_front().span; // pop 'while'

        let condition = self.parse_expr()?;
        let body = self.parse_block()?;

        Ok(WhileStmt {
            condition,
            body,
            kw_span,
        })
    }

    fn for_stmt(&mut self) -> PResult<Stmt> {
        let kw_span = self.tokens.pop_front().span; // pop 'for'

        let key_name = self.tokens.pop_name()?;

        match self.tokens[0].kind {
            Comma => {
                self.tokens.pop_front();

                let val_name = self.tokens.pop_name()?;

                self.tokens.expect(In)?;

                let iter = self.parse_expr()?;

                let body = self.parse_block()?;

                Ok(Stmt::KeyValFor(KeyValFor {
                    key_name,
                    val_name,
                    iter,
                    body,
                    kw_span,
                }))
            }
            In => {
                self.tokens.pop_front();

                let lhs = self.parse_range_expr()?;

                let op_span = self.tokens.expect(DotDot)?.span;

                let rhs = self.parse_range_expr()?;

                let body = self.parse_block()?;

                Ok(Stmt::RangeFor(RangeFor {
                    var: key_name,
                    range: Box::new(RangeExpr { lhs, rhs, op_span }),
                    body,
                    kw_span,
                }))
            }
            _ => Err(UnexpectedToken::err(self.tokens[0].clone(), [Comma, In])),
        }
    }

    fn parse_expr_stmt(&mut self) -> PResult<Stmt> {
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
                    return Err(ParseError::new(BadExprStmt { expr_stmt: sufexpr }));
                }
                return Ok(Stmt::ExprStmt(sufexpr));
            }

            let mut lhs_arr = vec![sufexpr];

            while self.tokens[0].kind == Comma {
                self.tokens.pop_front();
                // NOTE: should we be matching lua's behavior here? they only allow expressions
                //       of these types: VLOCAL, VUPVAL, VGLOBAL, VINDEXED. we could match this
                //       through some combination of checking the last stuffix and checking the
                //       primary expr if there are no suffixes.
                lhs_arr.push(self.parse_suffixed_expr()?);
            }

            return match self.tokens[0].kind {
                Colon => {
                    self.tokens.pop_front();
                    self.tokens.expect(Equal)?;

                    let rhs_arr = self.parse_expr_list()?;

                    let mut new_lhs_arr = Vec::new();

                    for suffixed_expr in lhs_arr {
                        if let Expr::Name(name) = self.expr_pool[suffixed_expr.val] {
                            if !suffixed_expr.suffixes.is_empty() {
                                return Err(ExprInDeclLHS::err(suffixed_expr));
                            }

                            new_lhs_arr.push(name);
                        } else {
                            return Err(ExprInDeclLHS::err(suffixed_expr));
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
                _ => Err(UnexpectedToken::err(self.tokens[0].clone(), [Colon, Equal])),
            };
        }

        if sufexpr.suffixes.is_empty() {
            return Err(BadExprStmt::err(sufexpr.clone()));
        }

        Ok(Stmt::ExprStmt(sufexpr))
    }

    fn parse_suffixed_expr(&mut self) -> PResult<SuffixedExpr> {
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
                        return Ok(SuffixedExpr {
                            val,
                            suffixes: suffixes.into(),
                        });
                    }

                    if self.tokens[1].kind == Name && self.tokens[2].kind == LParen {
                        self.tokens.pop_front(); // ':'
                        let name = self.tokens.pop_front().span;

                        suffixes.push(Suffix::Method(Method {
                            method_name: name,
                            args: self.parse_func_args()?,
                        }));
                    } else {
                        return Ok(SuffixedExpr {
                            val,
                            suffixes: suffixes.into(),
                        });
                    }
                }
                LSquare => {
                    suffixes.push(Suffix::Index(self.parse_index()?));
                }
                LParen => {
                    let (args, span) = self.parse_func_args_spanned()?;
                    suffixes.push(Suffix::Call(Call { args, span }));
                }
                _ => {
                    return Ok(SuffixedExpr {
                        val,
                        suffixes: suffixes.into(),
                    })
                }
            }
        }
    }

    fn parse_index(&mut self) -> PResult<Index> {
        let start = self.tokens.pop_front().span.start;

        let key = self.parse_expr()?;

        let end = self.tokens.expect(RSquare)?.span.end;

        Ok(Index {
            key,
            span: Span::new(start, end),
        })
    }

    fn parse_expr_list(&mut self) -> PResult<Vec<ExprRef>> {
        let mut result = Vec::new();
        result.push(self.parse_expr()?);

        while self.tokens[0].kind == Comma {
            self.tokens.pop_front();
            result.push(self.parse_expr()?);
        }

        Ok(result)
    }

    fn parse_func_args(&mut self) -> PResult<Vec<ExprRef>> {
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

    fn parse_func_args_spanned(&mut self) -> PResult<(Box<[ExprRef]>, Span)> {
        let start = self.tokens.pop_front().span.start; // pop '('
        if self.tokens[0].kind == RParen {
            let end = self.tokens.pop_front().span.end;
            return Ok((Box::default(), Span::new(start, end)));
        }

        let mut result = Vec::new();
        result.push(self.parse_expr()?);

        while self.tokens[0].kind == Comma {
            self.tokens.pop_front();
            result.push(self.parse_expr()?);
        }

        let end = self.tokens.expect(RParen)?.span.end;

        Ok((result.into_boxed_slice(), Span::new(start, end)))
    }

    fn parse_primary_expr(&mut self) -> PResult<ExprRef> {
        match self.tokens[0].kind {
            Name => {
                let name = self.tokens[0].span;
                self.tokens.pop_front();
                Ok(self.expr_pool.add(Expr::Name(name)))
            }
            LParen => {
                let start = self.tokens.pop_front().span.start;
                let val = self.parse_expr()?;
                let end = self.tokens.expect(RParen)?.span.end;

                Ok(self.expr_pool.add(Expr::Paren(ParenExpr {
                    val,
                    span: Span::new(start, end),
                })))
            }
            _ => Err(UnexpectedToken::err(self.tokens[0].clone(), [Name, LParen])),
        }
    }

    fn parse_expr(&mut self) -> PResult<ExprRef> {
        self.expr_impl(0)
    }

    fn expr_impl(&mut self, limit: u8) -> PResult<ExprRef> {
        let mut result = if let Some(op) = get_unop(self.tokens[0].kind) {
            let op_span = self.tokens.pop_front().span;
            let val = self.expr_impl(12)?;

            self.expr_pool.add(Expr::UnOp(UnOp { op, val, op_span }))
        } else {
            let val = self.simple_expr()?;

            self.expr_pool.add(Expr::Simple(val))
        };

        while let Some((op, prec)) = get_op(self.tokens[0].kind) {
            if prec.left <= limit {
                break;
            }

            let op_start = self.tokens.pop_front().span.start;

            let rhs = self.expr_impl(prec.right)?;

            result = self.expr_pool.add(Expr::BinOp(BinOp {
                op,
                lhs: result,
                rhs,
                op_start,
            }));
        }

        Ok(result)
    }

    fn simple_expr(&mut self) -> PResult<SimpleExpr> {
        let span = self.tokens[0].span;
        match self.tokens[0].kind {
            NumLit => {
                self.tokens.pop_front();
                Ok(SimpleExpr::Num(span))
            }
            StrLit => {
                self.tokens.pop_front();
                Ok(SimpleExpr::Str(span))
            }
            True | False => {
                self.tokens.pop_front();
                Ok(SimpleExpr::Bool(span))
            }
            Nil => {
                self.tokens.pop_front();
                Ok(SimpleExpr::Nil(span))
            }
            Elipsis => {
                self.tokens.pop_front();
                Ok(SimpleExpr::Variadic(span))
            }
            LCurly => Ok(SimpleExpr::TableNode(self.table_constructor()?)),
            Func => Ok(SimpleExpr::FuncNode(Box::new(self.func_constructor()?))),
            _ => Ok(SimpleExpr::SuffixedExpr(self.parse_suffixed_expr()?)),
        }
    }

    fn field(&mut self) -> PResult<FieldNode> {
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
                    let key = self.tokens[0].span;
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

    fn table_constructor(&mut self) -> PResult<TableNode> {
        let start = self.tokens.pop_front().span.start;

        if self.tokens[0].kind == RCurly {
            let end = self.tokens.pop_front().span.end;
            return Ok(TableNode {
                fields: Box::default(),
                span: Span::new(start, end),
            });
        }

        let mut fields = Vec::new();

        while self.tokens[0].kind != RCurly {
            fields.push(self.field()?);

            if self.tokens[0].kind == Comma {
                self.tokens.pop_front();
            } else {
                break;
            }
        }

        let end = self.tokens.expect(RCurly)?.span.end;

        Ok(TableNode {
            fields: fields.into_boxed_slice(),
            span: Span::new(start, end),
        })
    }

    fn member(&mut self) -> PResult<Member> {
        let name = self.tokens.pop_name()?;
        self.tokens.expect(Colon)?;

        Ok(Member {
            name,
            ty: self.parse_type()?,
        })
    }

    fn parse_struct_constructor(&mut self) -> PResult<FuncNode> {
        let start = self.tokens.pop_front().span.start;
        self.tokens.expect(LParen)?;

        let mut params = Vec::new();

        let end = if self.tokens[0].kind == RParen {
            self.tokens.pop_front().span.end
        } else {
            loop {
                if self.tokens[0].kind == Elipsis {
                    let span = self.tokens.pop_front().span;
                    params.push(Param {
                        name: span,
                        ty: TypeNode::VariadicType(span),
                    });
                    break self.tokens.expect(RParen)?.span.end;
                    // TODO: diagnostic here for params after variadic
                }

                if self.tokens[0].kind == Name && self.tokens[1].kind == Colon {
                    let argname = self.tokens[0].span;
                    self.tokens.pop_front();
                    self.tokens.pop_front();

                    params.push(Param {
                        name: argname,
                        ty: self.parse_type()?,
                    });
                } else {
                    let offset = self.tokens[0].span.start;
                    params.push(Param {
                        name: Span::empty(offset),
                        ty: self.parse_type()?,
                    });
                }

                if self.tokens[0].kind == RParen {
                    break self.tokens.pop_front().span.end;
                }

                self.tokens.expect(Comma)?;
            }
        };

        let body = self.parse_block_spanned()?;

        Ok(FuncNode {
            ty: FunctionType {
                params,
                return_type: None,
                header_span: Span::new(start, end),
            },
            body,
        })
    }

    fn parse_struct_decl(&mut self) -> PResult<StructDecl> {
        let _start = self.tokens.pop_front().span.start; // pop 'struct'

        let name = self.tokens.pop_name()?;

        self.tokens.expect(LCurly)?;

        let mut members = Vec::new();

        if self.tokens[0].kind == RCurly {
            let end_loc = self.tokens.pop_front().span.end;
            return Ok(StructDecl {
                name,
                members,
                constructor: None,
                end_loc,
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
                let end_loc = self.tokens.pop_front().span.end;
                Ok(StructDecl {
                    name,
                    members,
                    constructor: None,
                    end_loc,
                })
            }
            Constructor => {
                let constructor = Some(Box::new(self.parse_struct_constructor()?));
                let end_loc = self.tokens.expect(RCurly)?.span.end;
                Ok(StructDecl {
                    name,
                    members,
                    constructor,
                    end_loc,
                })
            }
            _ => Err(UnexpectedToken::err(
                self.tokens[0].clone(),
                [RCurly, Constructor],
            )),
        }
    }

    fn parse_func_header(&mut self) -> PResult<FunctionType> {
        let start = self.tokens.pop_front().span.start; // pop 'func'
        self.tokens.expect(LParen)?;

        let mut params = Vec::new();

        let end = if self.tokens[0].kind == RParen {
            self.tokens.pop_front().span.end
        } else {
            loop {
                if self.tokens[0].kind == Elipsis {
                    let span = self.tokens.pop_front().span;
                    params.push(Param {
                        name: span,
                        ty: TypeNode::VariadicType(span),
                    });
                    break self.tokens.expect(RParen)?.span.end;
                    // TODO: diagnostic here for params after variadic
                }

                if self.tokens[0].kind == Name && self.tokens[1].kind == Colon {
                    let name = self.tokens[0].span;
                    self.tokens.pop_front();
                    self.tokens.pop_front();

                    params.push(Param {
                        name,
                        ty: self.parse_type()?,
                    });
                } else {
                    let offset = self.tokens[0].span.start;
                    params.push(Param {
                        name: Span::empty(offset),
                        ty: self.parse_type()?,
                    });
                }

                if self.tokens[0].kind == RParen {
                    break self.tokens.pop_front().span.end;
                }

                self.tokens.expect(Comma)?;
            }
        };

        let return_type = self.parse_return_type()?.map(Box::new);

        Ok(FunctionType {
            params,
            return_type,
            header_span: Span::new(start, end),
        })
    }

    fn func_constructor(&mut self) -> PResult<FuncNode> {
        let ty = self.parse_func_header()?;

        let body = self.parse_block_spanned()?;

        Ok(FuncNode { ty, body })
    }

    fn parse_return_type(&mut self) -> PResult<Option<ReturnType>> {
        if self.tokens[0].kind != Arrow {
            return Ok(None);
        }

        self.tokens.pop_front(); // pop '->'
        let ty_start = self.tokens[0].span.start;
        let ty = match self.tokens[0].kind {
            LParen => {
                self.tokens.pop_front();
                let mut types = Vec::new();
                types.push(self.parse_type()?);

                while self.tokens[0].kind == Comma {
                    self.tokens.pop_front();
                    types.push(self.parse_type()?);
                }

                let ty_end = self.tokens.expect(RParen)?.span.end;

                ReturnType::Multiple(MultipleType {
                    types,
                    span: Span::new(ty_start, ty_end),
                })
            }
            Elipsis => {
                // NOTE: should this be it's own variant on ReturnType?

                let span = self.tokens.pop_front().span;
                ReturnType::Single(TypeNode::VariadicType(span))
            }
            _ => ReturnType::Single(self.parse_type()?),
        };

        Ok(Some(ty))
    }

    fn parse_basic_type(&mut self) -> PResult<TypeNode> {
        match self.tokens[0].kind {
            Name | Nil => {
                let name = self.tokens.pop_front().span;
                Ok(TypeNode::Name(name))
            }
            LSquare => {
                let start = self.tokens.pop_front().span.start;

                if self.tokens[0].kind == RSquare {
                    self.tokens.pop_front();

                    return Ok(TypeNode::TableType(TableType {
                        key_type: None,
                        key_span: Span::empty(start),
                        val_type: Box::new(self.parse_type()?),
                    }));
                }

                let key_type = self.parse_type()?;

                let end = self.tokens.expect(RSquare)?.span.end;

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
            LParen => {
                let start = self.tokens.pop_front().span.start;
                let inner = Box::new(self.parse_type()?);
                let end = self.tokens.expect(RParen)?.span.end;

                Ok(TypeNode::ParenType(ParenType {
                    inner,
                    span: Span::new(start, end),
                }))
            }
            _ => Err(UnexpectedToken::err(
                self.tokens[0].clone(),
                [Name, Nil, LSquare, Func],
            )),
        }
    }

    fn parse_type(&mut self) -> PResult<TypeNode> {
        let mut result = self.parse_basic_type()?;

        if self.tokens[0].kind == Question {
            let question = self.tokens.pop_front().span;

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
    fn parse_range_expr(&mut self) -> PResult<ExprRef> {
        self.range_expr_impl(0)
    }

    fn range_expr_impl(&mut self, limit: u8) -> PResult<ExprRef> {
        let mut result = if let Some(op) = get_unop(self.tokens[0].kind) {
            let op_span = self.tokens.pop_front().span;
            let val = self.range_expr_impl(12)?;

            self.expr_pool.add(Expr::UnOp(UnOp { op, val, op_span }))
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

            let op_start = self.tokens.pop_front().span.start;

            let rhs = self.range_expr_impl(prec.right)?;

            result = self.expr_pool.add(Expr::BinOp(BinOp {
                op,
                lhs: result,
                rhs,
                op_start,
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
