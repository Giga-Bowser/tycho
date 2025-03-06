use crate::{
    error::{Annotation, Diag, DiagCtx, Level, Snippetize},
    parser::ast,
    typecheck::pool::TypeRef,
    utils::{spanned::Spanned, Span},
};

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
    BadAccess { span: Span<'s>, ty: TypeRef<'s> },
    BadIndex { span: Span<'s>, ty: TypeRef<'s> },
    BadNegate { op_span: Span<'s>, ty: TypeRef<'s> },
    BadNot { op_span: Span<'s>, ty: TypeRef<'s> },
    TooManyArgs,
    TooFewArgs,
}

impl<'s> Snippetize<'s> for CheckErr<'s> {
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        match self {
            CheckErr::MismatchedTypes(mismatched_types) => mismatched_types.snippetize(ctx),
            CheckErr::NoReturn(no_return) => no_return.snippetize(ctx),
            CheckErr::ReturnCount(return_count) => return_count.snippetize(ctx),
            CheckErr::MethodOnWrongType(method_on_wrong_type) => {
                method_on_wrong_type.snippetize(ctx)
            }
            CheckErr::NoSuchVal(no_such_val) => no_such_val.snippetize(ctx),
            CheckErr::NoSuchField(no_such_field) => no_such_field.snippetize(ctx),
            CheckErr::NoSuchMethod(no_such_method) => no_such_method.snippetize(ctx),
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
                    label: Some(format!(
                        "expected `number` for this operator, found `{}`",
                        ctx.tcx.pool.wrap(*ty)
                    )),
                }],
            },
            CheckErr::BadNot { op_span, ty } => Diag {
                title: "cannot unary not non-boolean type".to_owned(),
                level: Level::Error,
                annotations: vec![Annotation {
                    level: Level::Error,
                    range: op_span.to_range(),
                    label: Some(format!(
                        "expected `boolean` for this operator, found `{}`",
                        ctx.tcx.pool.wrap(*ty)
                    )),
                }],
            },
            CheckErr::BadIndex { span, ty } => Diag {
                title: format!(
                    "cannot index into value of type `{}`",
                    ctx.tcx.pool.wrap(*ty)
                ),
                level: Level::Error,
                annotations: vec![Annotation {
                    level: Level::Error,
                    range: span.to_range(),
                    label: Some(format!(
                        "cannot index into value of type `{}`",
                        ctx.tcx.pool.wrap(*ty)
                    )),
                }],
            },
            CheckErr::BadAccess { span, ty } => Diag {
                title: format!(
                    "cannot perform access on value of type `{}`",
                    ctx.tcx.pool.wrap(*ty)
                ),
                level: Level::Error,
                annotations: vec![Annotation {
                    level: Level::Error,
                    range: span.to_range(),
                    label: Some(format!(
                        "cannot perform access on value of type `{}`",
                        ctx.tcx.pool.wrap(*ty)
                    )),
                }],
            },
            CheckErr::TooManyArgs => Diag {
                title: "too many args for function".to_owned(),
                level: Level::Error,
                annotations: Vec::new(),
            },
            CheckErr::TooFewArgs => Diag {
                title: "too few args for function".to_owned(),
                level: Level::Error,
                annotations: Vec::new(),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct MismatchedTypes<'s> {
    pub expected: TypeRef<'s>,
    pub recieved: TypeRef<'s>,
}

impl<'s> MismatchedTypes<'s> {
    pub fn err(expected: TypeRef<'s>, recieved: TypeRef<'s>) -> Box<CheckErr<'s>> {
        Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
            expected,
            recieved,
        }))
    }
}

impl<'s> Snippetize<'s> for MismatchedTypes<'s> {
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        let expected = &ctx.tcx.pool[self.expected];
        let recieved = &ctx.tcx.pool[self.recieved];
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
                    "expected `{}`, found `{}`",
                    ctx.tcx.pool.wrap(self.expected),
                    ctx.tcx.pool.wrap(self.recieved)
                )),
            });
        }

        Diag {
            title: format!(
                "type mismatch, `{}` vs `{}`",
                ctx.tcx.pool.wrap(self.expected),
                ctx.tcx.pool.wrap(self.recieved),
            ),
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
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        let val_str = self.val_name.to_str(ctx.source);

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
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        let field_str = self.field_name.to_str(ctx.source);

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
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        let method_str = self.method_name.to_str(ctx.source);

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
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        let mut annotations = Vec::new();

        if let Some(return_type) = &self.func_node.ty.return_type {
            annotations.push(Annotation {
                level: Level::Error,
                range: return_type.span().to_range(),
                label: Some("expected return type".to_owned()),
            });
        }

        annotations.push(Annotation {
            level: Level::Info,
            range: ctx.expr_pool.wrap(&self.func_node).span().to_range(),
            label: Some("function here".to_owned()),
        });

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
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        let return_span = ctx.expr_pool.wrap(&self.return_node).span();
        Diag {
            title: "wrong number of returns".to_owned(),
            level: Level::Error,
            annotations: vec![Annotation {
                level: Level::Error,
                range: return_span.to_range(),
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
    pub ty: TypeRef<'s>,
}

impl<'s> Snippetize<'s> for MethodOnWrongType<'s> {
    fn snippetize(&self, ctx: &DiagCtx<'_, 's>) -> Diag {
        Diag {
            title: format!(
                "tried to declare method on non-struct type `{}`",
                ctx.tcx.pool.wrap(self.ty)
            ),
            level: Level::Error,
            annotations: vec![Annotation {
                level: Level::Error,
                range: self.span.to_range(),
                label: Some("method declaration here".to_owned()),
            }],
        }
    }
}
