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
    fn snippetize(&self, source: &'s str) -> Diag;
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
    fn snippetize(&self, source: &'s str) -> Diag {
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
    fn snippetize(&self, source: &'s str) -> Diag {
        let mut annotations = vec![Annotation {
            level: Level::Error,
            range: self.token.text.to_range(),
            label: Some("token here".to_owned()),
        }];

        if !self.expected_kinds.is_empty() {
            let kinds = self
                .expected_kinds
                .iter()
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
            title: format!("unexpected token: `{}`", self.token.text.to_str(source)),
            level: Level::Error,
            annotations,
        }
    }
}

#[derive(Debug, Clone)]
pub enum CheckErr<'s> {
    MismatchedTypes(MismatchedTypes<'s>),
    NoReturn(NoReturn<'s>),
    ReturnCount(ReturnCount<'s>),
    MethodOnWrongType(MethodOnWrongType<'s>),
    NoSuchVal(NoSuchVal<'s>),
    NoSuchField(NoSuchField<'s>),
    NoSuchMethod(NoSuchMethod<'s>),
    NotIterable,
    BadAccess { span: Span<'s>, ty: Type<'s> },
    BadIndex { span: Span<'s>, ty: Type<'s> },
    BadNegate { op_span: Span<'s>, ty: Type<'s> },
    BadNot { op_span: Span<'s>, ty: Type<'s> },
}

impl<'s> Snippetize<'s> for CheckErr<'s> {
    fn snippetize(&self, source: &'s str) -> Diag {
        match self {
            CheckErr::MismatchedTypes(mismatched_types) => mismatched_types.snippetize(source),
            CheckErr::NoReturn(no_return) => no_return.snippetize(source),
            CheckErr::ReturnCount(return_count) => return_count.snippetize(source),
            CheckErr::MethodOnWrongType(method_on_wrong_type) => {
                method_on_wrong_type.snippetize(source)
            }
            CheckErr::NoSuchVal(no_such_val) => no_such_val.snippetize(source),
            CheckErr::NoSuchField(no_such_field) => no_such_field.snippetize(source),
            CheckErr::NoSuchMethod(no_such_method) => no_such_method.snippetize(source),
            CheckErr::NotIterable => Diag {
                title: "can't iterate over non-iterable".to_owned(),
                level: Level::Error,
                annotations: vec![],
            },
            CheckErr::BadNegate { op_span, ty } => Diag {
                title: "cannot unary negate non-number type".to_owned(),
                level: Level::Error,
                annotations: vec![Annotation {
                    level: Level::Error,
                    range: op_span.to_range(),
                    label: Some(format!("expected `number` for this operator, found `{ty}`")),
                }],
            },
            CheckErr::BadNot { op_span, ty } => Diag {
                title: "cannot unary not non-boolean type".to_owned(),
                level: Level::Error,
                annotations: vec![Annotation {
                    level: Level::Error,
                    range: op_span.to_range(),
                    label: Some(format!(
                        "expected `boolean` for this operator, found `{ty}`"
                    )),
                }],
            },
            CheckErr::BadIndex { span, ty } => Diag {
                title: format!("cannot index into value of type `{ty}`"),
                level: Level::Error,
                annotations: vec![Annotation {
                    level: Level::Error,
                    range: span.to_range(),
                    label: Some(format!("cannot index into value of type `{ty}`")),
                }],
            },
            CheckErr::BadAccess { span, ty } => Diag {
                title: format!("cannot perform access on value of type `{ty}`"),
                level: Level::Error,
                annotations: vec![Annotation {
                    level: Level::Error,
                    range: span.to_range(),
                    label: Some(format!("cannot perform access on value of type `{ty}`")),
                }],
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct MismatchedTypes<'s> {
    pub expected: Type<'s>,
    pub recieved: Type<'s>,
}

impl<'s> Snippetize<'s> for MismatchedTypes<'s> {
    fn snippetize(&self, _source: &'s str) -> Diag {
        let expected_span = self.expected.span;
        let recieved_span = self.recieved.span;

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
                    "expected `{}`, found `{}`",
                    self.expected, self.recieved
                )),
            });
        }

        Diag {
            title: "type mismatch".to_owned(),
            level: Level::Error,
            annotations,
        }
    }
}

#[derive(Debug, Clone)]
pub struct NoSuchVal<'s> {
    pub val_name: Span<'s>,
}

impl<'s> Snippetize<'s> for NoSuchVal<'s> {
    fn snippetize(&self, source: &'s str) -> Diag {
        let val_str = self.val_name.to_str(source);

        Diag {
            title: format!("cannot find value `{val_str}` in this scope"),
            level: Level::Error,
            annotations: vec![Annotation {
                level: Level::Error,
                range: self.val_name.to_range(),
                label: Some("not found in this scope".to_owned()),
            }],
        }
    }
}

#[derive(Debug, Clone)]
pub struct NoSuchField<'s> {
    pub field_name: Span<'s>,
}

impl<'s> Snippetize<'s> for NoSuchField<'s> {
    fn snippetize(&self, source: &'s str) -> Diag {
        let field_str = self.field_name.to_str(source);

        Diag {
            title: format!("cannot find field `{field_str}` on this type"),
            level: Level::Error,
            annotations: vec![Annotation {
                level: Level::Error,
                range: self.field_name.to_range(),
                label: Some("field not found on this type".to_owned()),
            }],
        }
    }
}

#[derive(Debug, Clone)]
pub struct NoSuchMethod<'s> {
    pub method_name: Span<'s>,
}

impl<'s> Snippetize<'s> for NoSuchMethod<'s> {
    fn snippetize(&self, source: &'s str) -> Diag {
        let method_str = self.method_name.to_str(source);

        Diag {
            title: format!("cannot find method `{method_str}` on this type"),
            level: Level::Error,
            annotations: vec![Annotation {
                level: Level::Error,
                range: self.method_name.to_range(),
                label: Some("method not found on this type".to_owned()),
            }],
        }
    }
}

#[derive(Debug, Clone)]
pub struct NoReturn<'s> {
    pub func_node: ast::FuncNode<'s>,
}

impl<'s> Snippetize<'s> for NoReturn<'s> {
    fn snippetize(&self, _source: &'s str) -> Diag {
        let return_span = self.func_node.type_.returns.span;

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
}

#[derive(Debug, Clone)]
pub struct ReturnCount<'s> {
    pub return_node: ast::ReturnStmt<'s>,
    pub expected: usize,
}

impl<'s> Snippetize<'s> for ReturnCount<'s> {
    fn snippetize(&self, _source: &'s str) -> Diag {
        Diag {
            title: "wrong number of returns".to_owned(),
            level: Level::Error,
            annotations: vec![Annotation {
                level: Level::Error,
                range: self.return_node.kw_span.to_range(),
                label: Some(format!(
                    "expected `{}` return values here, recieved `{}`",
                    self.expected,
                    self.return_node.vals.len()
                )),
            }],
        }
    }
}

#[derive(Debug, Clone)]
pub struct MethodOnWrongType<'s> {
    pub span: Span<'s>,
    pub ty: Type<'s>,
}

impl<'s> Snippetize<'s> for MethodOnWrongType<'s> {
    fn snippetize(&self, _source: &'s str) -> Diag {
        Diag {
            title: format!("tried to declare method on non-struct type `{}`", self.ty),
            level: Level::Error,
            annotations: vec![Annotation {
                level: Level::Error,
                range: self.span.to_range(),
                label: Some("method declaration here".to_owned()),
            }],
        }
    }
}
