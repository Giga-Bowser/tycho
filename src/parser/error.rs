use crate::{
    error::{Annotation, Diag, DiagCtx, Level, Snippetize},
    lexer::{SpanToken, TokenKind},
    parser::ast,
    utils::{spanned::Spanned, Span},
};

#[derive(Default, Debug)]
pub enum ParseError {
    #[default]
    EmptyError,
    NoSuchType(Span),
    UnexpectedToken(UnexpectedToken),
    BadExprStmt(ast::SuffixedExpr),
}

#[derive(Debug)]
pub struct UnexpectedToken {
    pub token: SpanToken,
    pub expected_kinds: Vec<TokenKind>,
}

impl From<UnexpectedToken> for ParseError {
    fn from(value: UnexpectedToken) -> Self {
        ParseError::UnexpectedToken(value)
    }
}

impl<'s> Snippetize<'s> for ParseError {
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        match self {
            ParseError::EmptyError => Diag::new(Level::Error, "oh no, empty error"),
            ParseError::NoSuchType(val_name) => Diag::new(
                Level::Error,
                format!(
                    "cannot find type `{}` in this scope",
                    val_name.to_str(ctx.source)
                ),
            )
            .add_annotation(
                Annotation::new_span(Level::Error, *val_name).label("not found in this scope"),
            ),
            ParseError::UnexpectedToken(unexpected_token) => unexpected_token.snippetize(ctx),
            ParseError::BadExprStmt(suffixed_expr) => {
                let mut diag = Diag::new(
                    Level::Error,
                    format!("bad expression statement: `{suffixed_expr:?}`"),
                )
                .add_annotation(
                    Annotation::new_span(Level::Error, ctx.expr_pool.wrap(suffixed_expr).span())
                        .label("expression statement here".to_owned()),
                );

                if let ast::Expr::Name(name) = &ctx.expr_pool[suffixed_expr.val] {
                    let name_str = name.to_str(ctx.source);
                    if let "local" | "let" = name_str {
                        diag = diag.add_annotation(
                            Annotation::new_span(Level::Help, *name)
                                .label(format!("tycho does not have `{name_str}` statements.")),
                        );
                    }
                }

                diag
            }
        }
    }
}

impl<'s> Snippetize<'s> for UnexpectedToken {
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        let mut diag = Diag::new(
            Level::Error,
            format!("unexpected token: `{}`", self.token.text.to_str(ctx.source)),
        )
        .add_annotation(Annotation::new_span(Level::Error, self.token.text).label("token here"));

        if !self.expected_kinds.is_empty() {
            let kinds = self
                .expected_kinds
                .iter()
                .map(|it| format!("`{it:?}`"))
                .collect::<Vec<String>>()
                .join(", ");

            diag = diag.add_annotation(
                Annotation::new_span(Level::Info, self.token.text)
                    .label(format!("expected: {kinds}")),
            );
        }

        diag
    }
}
