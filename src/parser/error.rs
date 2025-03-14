use crate::{
    error::{Annotation, Diag, DiagCtx, Level, Snippetize},
    error_kind,
    lexer::{Token, TokenKind},
    parser::ast,
    token_pat,
    utils::spanned::Spanned,
};

#[derive(Debug)]
pub struct ParseError {
    kind: ParseErrorKind,
    context: Option<String>,
}

impl ParseError {
    pub(crate) fn new(kind: impl Into<ParseErrorKind>) -> Box<Self> {
        Box::new(Self {
            kind: kind.into(),
            context: None,
        })
    }
}

impl Snippetize for ParseError {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let mut diag = self.kind.snippetize(ctx);

        if let Some(context) = &self.context {
            diag.add_note(format!("while parsing {context}"));
        }

        diag
    }
}

error_kind! {
    #[derive(Debug)]
    pub(crate) enum ParseErrorKind {
        UnexpectedToken,
        BadExprStmt,
        ExprInDeclLHS,
    }
}

fn pretty_token(kind: TokenKind) -> String {
    match kind {
        TokenKind::Name => "name".to_owned(),
        TokenKind::EndOfFile => "end of file".to_owned(),

        TokenKind::NumLit => "number literal".to_owned(),
        TokenKind::StrLit => "string literal".to_owned(),
        TokenKind::True => "`true` literal".to_owned(),
        TokenKind::False => "`false` literal".to_owned(),
        TokenKind::Nil => "`nil` literal".to_owned(),

        token_pat!(KEYWORD) => format!("keyword `{}`", kind.text().unwrap()),
        token_pat!(PUNCT) => format!("`{}`", kind.text().unwrap()),
    }
}

#[derive(Debug)]
pub(crate) struct UnexpectedToken {
    token: Token,
    expected_kinds: Vec<TokenKind>,
}

impl UnexpectedToken {
    pub(crate) fn err(
        token: Token,
        expected: impl IntoIterator<Item = TokenKind>,
    ) -> Box<ParseError> {
        ParseError::new(Self {
            token,
            expected_kinds: expected.into_iter().collect(),
        })
    }
}

impl Snippetize for UnexpectedToken {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let mut diag = Diag::new(
            Level::Error,
            format!("unexpected token: `{}`", self.token.span.to_str(ctx.file)),
        )
        .with_annotation(Annotation::new(Level::Error, self.token.span).label("token here"));

        if !self.expected_kinds.is_empty() {
            let kinds = self
                .expected_kinds
                .iter()
                .map(|it| pretty_token(*it))
                .collect::<Vec<String>>()
                .join(", ");

            diag.add_annotation(
                Annotation::new(Level::Info, self.token.span).label(format!("expected: {kinds}")),
            );
        }

        diag
    }
}

#[derive(Debug)]
pub(crate) struct BadExprStmt {
    pub expr_stmt: ast::SuffixedExpr,
}

impl BadExprStmt {
    pub(crate) fn err(expr_stmt: ast::SuffixedExpr) -> Box<ParseError> {
        ParseError::new(Self { expr_stmt })
    }
}

impl Snippetize for BadExprStmt {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let mut diag = Diag::new(Level::Error, "bad expression-statement").with_annotation(
            Annotation::new(Level::Error, ctx.expr_pool.wrap(&self.expr_stmt).span())
                .label("expression-statement here"),
        );

        if let ast::Expr::Name(name) = &ctx.expr_pool[self.expr_stmt.val] {
            let name_str = name.to_str(ctx.file);
            if let "local" | "let" = name_str {
                diag.add_note(format!("tycho does not have `{name_str}` statements."));
            }
        }

        diag
    }
}

#[derive(Debug)]
pub(crate) struct ExprInDeclLHS {
    suffixed_expr: ast::SuffixedExpr,
}

impl ExprInDeclLHS {
    pub(crate) fn err(suffixed_expr: ast::SuffixedExpr) -> Box<ParseError> {
        ParseError::new(Self { suffixed_expr })
    }
}

impl Snippetize for ExprInDeclLHS {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        Diag::new(
            Level::Error,
            "expression found in left-hand side of declaration",
        )
        .with_annotation(Annotation::new(
            Level::Error,
            ctx.expr_pool.wrap(&self.suffixed_expr).span(),
        ))
    }
}
