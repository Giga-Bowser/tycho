use crate::{
    error::{Annotation, Diag, DiagCtx, Level, Snippetize},
    lexer::{SpanToken, TokenKind},
    parser::ast,
    utils::{spanned::Spanned, Span},
};

#[derive(Default, Debug)]
pub enum ParseError<'s> {
    #[default]
    EmptyError,
    NoSuchType(Span<'s>),
    UnexpectedToken(UnexpectedToken<'s>),
    BadExprStmt(ast::SuffixedExpr<'s>),
}

#[derive(Debug)]
pub struct UnexpectedToken<'s> {
    pub token: SpanToken<'s>,
    pub expected_kinds: Vec<TokenKind>,
}

impl<'s> From<UnexpectedToken<'s>> for ParseError<'s> {
    fn from(value: UnexpectedToken<'s>) -> Self {
        ParseError::UnexpectedToken(value)
    }
}

impl<'s> Snippetize<'s> for ParseError<'s> {
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        match self {
            ParseError::EmptyError => Diag {
                title: "oh no, empty error".to_owned(),
                level: Level::Error,
                annotations: Vec::new(),
            },
            ParseError::NoSuchType(val_name) => Diag {
                title: format!(
                    "cannot find type `{}` in this scope",
                    val_name.to_str(ctx.source)
                ),
                level: Level::Error,
                annotations: vec![
                    Annotation::new_span(Level::Error, *val_name).label("not found in this scope")
                ],
            },
            ParseError::UnexpectedToken(unexpected_token) => unexpected_token.snippetize(ctx),
            ParseError::BadExprStmt(suffixed_expr) => {
                let mut annotations = vec![Annotation::new_span(
                    Level::Error,
                    ctx.expr_pool.wrap(suffixed_expr).span(),
                )
                .label("expression statement here".to_owned())];

                if let ast::Expr::Name(name) = &ctx.expr_pool[suffixed_expr.val] {
                    let name_str = name.to_str(ctx.source);
                    if let "local" | "let" = name_str {
                        annotations.push(
                            Annotation::new_span(Level::Help, *name)
                                .label(format!("tycho does not have `{name_str}` statements.")),
                        );
                    }
                }
                Diag {
                    title: format!("bad expression statements: `{suffixed_expr:?}`"),
                    level: Level::Error,
                    annotations,
                }
            }
        }
    }
}

impl<'s> Snippetize<'s> for UnexpectedToken<'s> {
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        let mut annotations = vec![
            Annotation::new_span(Level::Error, self.token.text).label("token here".to_owned())
        ];

        if !self.expected_kinds.is_empty() {
            let kinds = self
                .expected_kinds
                .iter()
                .map(|it| format!("`{it:?}`"))
                .collect::<Vec<String>>()
                .join(", ");

            annotations.push(
                Annotation::new_span(Level::Info, self.token.text)
                    .label(format!("expected: {kinds}")),
            );
        }

        Diag {
            title: format!("unexpected token: `{}`", self.token.text.to_str(ctx.source)),
            level: Level::Error,
            annotations,
        }
    }
}
