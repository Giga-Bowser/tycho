use std::ops::Range;

use ariadne::{Color, ReportKind};

use crate::{
    lexer::{Token, TokenKind},
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

pub trait Snippetize<'src> {
    fn snippetize(self, source: &'src str) -> Diag;
}

#[derive(Default, Debug)]
pub enum ParseError<'a> {
    #[default]
    EmptyError,
    NoSuchType(&'a str),
    UnexpectedToken(UnexpectedToken<'a>),
    BadExprStat(ast::Expr<'a>),
}

#[derive(Debug)]
pub struct UnexpectedToken<'a> {
    pub token: Token<'a>,
    pub expected_kinds: Vec<TokenKind>,
}

impl<'a> From<UnexpectedToken<'a>> for ParseError<'a> {
    fn from(value: UnexpectedToken<'a>) -> Self {
        ParseError::UnexpectedToken(value)
    }
}

impl<'src> Snippetize<'src> for ParseError<'src> {
    fn snippetize(self, source: &'src str) -> Diag {
        match self {
            ParseError::EmptyError => Diag {
                title: "oh no, empty error".to_owned(),
                level: Level::Error,
                annotations: vec![],
            },
            ParseError::NoSuchType(val_name) => {
                let range = get_substring_range(source, val_name);

                Diag {
                    title: format!("cannot find type `{val_name}` in this scope"),
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
                if let ast::Expr::Name(name) = expr {
                    let range = get_substring_range(source, name);
                    annotations.push(Annotation {
                        level: Level::Error,
                        range: range.clone(),
                        label: Some("expression statement here".to_owned()),
                    });
                    if let "local" | "let" = name {
                        annotations.push(Annotation {
                            level: Level::Help,
                            range,
                            label: Some(format!("tycho does not have `{name}` statements.")),
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

impl<'src> Snippetize<'src> for UnexpectedToken<'src> {
    fn snippetize(self, source: &'src str) -> Diag {
        let range = get_substring_range(source, self.token.text);
        let mut annotations = Vec::new();

        if !self.expected_kinds.is_empty() {
            let kinds = self
                .expected_kinds
                .into_iter()
                .map(|it| format!("`{it:?}`"))
                .collect::<Vec<String>>()
                .join(", ");

            annotations.push(Annotation {
                level: Level::Info,
                range,
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
pub enum CheckErr<'src> {
    #[default]
    EmptyError,
    NoSuchVal(&'src str),
    MismatchedTypes {
        expected: Type<'src>,
        recieved: Type<'src>,
    },
    ReturnCount,
    NotIterable,
    NoSuchField(&'src str),
    NoSuchMethod(&'src str),
    NoReturn(ast::FuncNode<'src>),
    CustomError(String),
}

impl<'src> Snippetize<'src> for CheckErr<'src> {
    fn snippetize(self, source: &'src str) -> Diag {
        match self {
            CheckErr::EmptyError => Diag {
                title: "oh no, empty error".to_owned(),
                level: Level::Error,
                annotations: vec![],
            },
            CheckErr::NoSuchVal(val_name) => {
                let range = get_substring_range(source, val_name);

                Diag {
                    title: format!("cannot find value `{val_name}` in this scope"),
                    level: Level::Error,
                    annotations: vec![Annotation {
                        level: Level::Error,
                        range,
                        label: Some("not found in this scope".to_owned()),
                    }],
                }
            }
            CheckErr::MismatchedTypes { expected, recieved } => {
                let expected_str = expected.src;
                let recieved_str = recieved.src;

                let mut annotations = Vec::new();

                if let Some(expected_str) = expected_str {
                    let range = get_substring_range(source, expected_str);

                    annotations.push(Annotation {
                        level: Level::Info,
                        range,
                        label: Some("expected due to this".to_owned()),
                    });
                }

                if let Some(recieved_str) = recieved_str {
                    let range = get_substring_range(source, recieved_str);

                    annotations.push(Annotation {
                        level: Level::Error,
                        range,
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
                    range: get_substring_range(source, field),
                    label: Some("this field".to_owned()),
                }],
            },
            CheckErr::NoSuchMethod(method) => Diag {
                title: format!("no method named `{method}`"),
                level: Level::Error,
                annotations: vec![Annotation {
                    level: Level::Error,
                    range: get_substring_range(source, method),
                    label: Some("method not found".to_owned()),
                }],
            },
            CheckErr::NoReturn(func_node) => {
                let return_str = func_node.type_.returns.src;

                let mut annotations = Vec::new();

                if let Some(return_str) = return_str {
                    let range = get_substring_range(source, return_str);

                    annotations.push(Annotation {
                        level: Level::Error,
                        range,
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

fn get_substring_range(source: &str, substr: &str) -> Range<usize> {
    let offset = unsafe { substr.as_ptr().offset_from(source.as_ptr()).abs() };

    let start_idx = offset.try_into().unwrap();
    let end_idx = start_idx + substr.len();

    assert!(end_idx <= source.len(), "substr exceeds source");

    start_idx..end_idx
}
