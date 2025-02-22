use std::ops::Range;

use ariadne::{Color, ReportKind};

use crate::{
    lexer::{Span, SpanToken, TokenKind},
    parser::ast,
    types::Type,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Level {
    Error,
    Warning,
    Info,
    Note,
    Help,
}

impl Level {
    pub fn color(self) -> Color {
        match self {
            Level::Error => Color::Red,
            Level::Warning => Color::Yellow,
            Level::Info => Color::BrightBlue,
            Level::Note => Color::BrightGreen,
            Level::Help => Color::BrightCyan,
        }
    }

    pub fn report_kind(self) -> ReportKind<'static> {
        let s = match self {
            Level::Error => "error",
            Level::Warning => "warning",
            Level::Info => "info",
            Level::Note => "note",
            Level::Help => "help",
        };

        ReportKind::Custom(s, self.color())
    }
}

pub struct Diag {
    pub title: String,
    pub level: Level,
    pub annotations: Vec<Annotation>,
}

pub struct Annotation {
    pub level: Level,
    pub range: Range<usize>,
    pub label: Option<String>,
}

pub trait Snippetize<'s> {
    fn snippetize(self, source: &'s str) -> Diag;
}

#[derive(Default, Debug)]
pub enum ParseError<'s> {
    #[default]
    EmptyError,
    NoSuchType(Span<'s>),
    UnexpectedToken(UnexpectedToken<'s>),
    BadExprStat(ast::Expr<'s>),
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
    fn snippetize(self, source: &'s str) -> Diag {
        match self {
            ParseError::EmptyError => Diag {
                title: "oh no, empty error".to_owned(),
                level: Level::Error,
                annotations: vec![],
            },
            ParseError::NoSuchType(val_name) => {
                let range = val_name.to_range();

                Diag {
                    title: format!(
                        "cannot find type `{}` in this scope",
                        val_name.to_str(source)
                    ),
                    level: Level::Error,
                    annotations: vec![Annotation {
                        level: Level::Error,
                        range,
                        label: Some("not found in this scope".to_owned()),
                    }],
                }
            }
            ParseError::UnexpectedToken(unexpected_token) => unexpected_token.snippetize(source),
            ParseError::BadExprStat(expr) => {
                let mut annotations = Vec::new();
                if let ast::Expr::Name(name) = &expr {
                    annotations.push(Annotation {
                        level: Level::Error,
                        range: name.to_range(),
                        label: Some("expression statement here".to_owned()),
                    });
                    let name_str = name.to_str(source);
                    if let "local" | "let" = name_str {
                        annotations.push(Annotation {
                            level: Level::Help,
                            range: name.to_range(),
                            label: Some(format!("tycho does not have `{name_str}` statements.")),
                        });
                    }
                }
                Diag {
                    title: format!("bad expression statement: `{expr:?}`"),
                    level: Level::Error,
                    annotations,
                }
            }
        }
    }
}

impl<'s> Snippetize<'s> for UnexpectedToken<'s> {
    fn snippetize(self, _source: &'s str) -> Diag {
        let mut annotations = vec![Annotation {
            level: Level::Error,
            range: self.token.text.to_range(),
            label: Some("token here".to_owned()),
        }];

        if !self.expected_kinds.is_empty() {
            let kinds = self
                .expected_kinds
                .into_iter()
                .map(|it| format!("`{it:?}`"))
                .collect::<Vec<String>>()
                .join(", ");

            annotations.push(Annotation {
                level: Level::Info,
                range: self.token.text.to_range(),
                label: Some(format!("expected: {kinds}")),
            });
        }

        Diag {
            title: format!("unexpected token: {:?}", self.token),
            level: Level::Error,
            annotations,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub enum CheckErr<'s> {
    #[default]
    EmptyError,
    NoSuchVal(Span<'s>),
    MismatchedTypes {
        expected: Type<'s>,
        recieved: Type<'s>,
    },
    ReturnCount,
    NotIterable,
    NoSuchField(Span<'s>),
    NoSuchMethod(Span<'s>),
    NoReturn(ast::FuncNode<'s>),
    CustomError(String),
}

impl<'s> Snippetize<'s> for CheckErr<'s> {
    fn snippetize(self, source: &'s str) -> Diag {
        match self {
            CheckErr::EmptyError => Diag {
                title: "oh no, empty error".to_owned(),
                level: Level::Error,
                annotations: vec![],
            },
            CheckErr::NoSuchVal(val_name) => {
                let val_str = val_name.to_str(source);

                Diag {
                    title: format!("cannot find value `{val_str}` in this scope"),
                    level: Level::Error,
                    annotations: vec![Annotation {
                        level: Level::Error,
                        range: val_name.to_range(),
                        label: Some("not found in this scope".to_owned()),
                    }],
                }
            }
            CheckErr::MismatchedTypes { expected, recieved } => {
                let expected_span = expected.span;
                let recieved_span = recieved.span;

                let mut annotations = Vec::new();

                if let Some(expected_span) = expected_span {
                    annotations.push(Annotation {
                        level: Level::Info,
                        range: expected_span.to_range(),
                        label: Some("expected due to this".to_owned()),
                    });
                }

                if let Some(recieved_str) = recieved_span {
                    annotations.push(Annotation {
                        level: Level::Error,
                        range: recieved_str.to_range(),
                        label: Some(format!(
                            "expected `{:?}`, found `{:?}`",
                            expected.kind, recieved.kind
                        )),
                    });
                }

                Diag {
                    title: "type mismatch".to_owned(),
                    level: Level::Error,
                    annotations,
                }
            }
            CheckErr::ReturnCount => Diag {
                title: "wrong number of returns".to_owned(),
                level: Level::Error,
                annotations: vec![],
            },
            CheckErr::NotIterable => Diag {
                title: "can't iterate over non-iterable".to_owned(),
                level: Level::Error,
                annotations: vec![],
            },
            CheckErr::NoSuchField(field) => Diag {
                title: "no such field".to_owned(),
                level: Level::Error,
                annotations: vec![Annotation {
                    level: Level::Error,
                    range: field.to_range(),
                    label: Some("this field".to_owned()),
                }],
            },
            CheckErr::NoSuchMethod(method) => Diag {
                title: format!("no method named `{}`", method.to_str(source)),
                level: Level::Error,
                annotations: vec![Annotation {
                    level: Level::Error,
                    range: method.to_range(),
                    label: Some("method not found".to_owned()),
                }],
            },
            CheckErr::NoReturn(func_node) => {
                let return_span = func_node.type_.returns.span;

                let mut annotations = Vec::new();

                if let Some(return_span) = return_span {
                    annotations.push(Annotation {
                        level: Level::Error,
                        range: return_span.to_range(),
                        label: Some("expected return type".to_owned()),
                    });
                }

                Diag {
                    title: "non-nil function does not return".to_owned(),
                    level: Level::Error,
                    annotations,
                }
            }
            CheckErr::CustomError(s) => Diag {
                title: s,
                level: Level::Error,
                annotations: Vec::new(),
            },
        }
    }
}
