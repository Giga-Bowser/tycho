use crate::{
    error::{Annotation, Diag, DiagCtx, Level, Snippetize},
    parser::ast,
    typecheck::pool::TypeRef,
    utils::{spanned::Spanned, Span},
};

#[derive(Debug, Clone)]
pub enum CheckErr {
    MismatchedTypes(MismatchedTypes),
    NoReturn(NoReturn),
    ReturnCount(ReturnCount),
    MethodOnWrongType(MethodOnWrongType),
    NoSuchVal(NoSuchVal),
    NoSuchField(NoSuchField),
    NoSuchMethod(NoSuchMethod),
    NotIterable,
    BadAccess { span: Span, ty: TypeRef },
    BadIndex { span: Span, ty: TypeRef },
    BadNegate { op_span: Span, ty: TypeRef },
    BadNot { op_span: Span, ty: TypeRef },
    TooManyArgs,
    TooFewArgs,
}

impl Snippetize for CheckErr {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
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
            CheckErr::NotIterable => Diag::new(Level::Error, "can't iterate over non-iterable"),
            CheckErr::BadNegate { op_span, ty } => {
                Diag::new(Level::Error, "cannot unary negate non-number type").add_annotation(
                    Annotation::new(Level::Error, *op_span).label(format!(
                        "expected `number` for this operator, found `{}`",
                        ctx.tcx.pool.wrap(*ty)
                    )),
                )
            }
            CheckErr::BadNot { op_span, ty } => {
                Diag::new(Level::Error, "cannot unary not non-boolean type").add_annotation(
                    Annotation::new(Level::Error, *op_span).label(format!(
                        "expected `boolean` for this operator, found `{}`",
                        ctx.tcx.pool.wrap(*ty)
                    )),
                )
            }
            CheckErr::BadIndex { span, ty } => Diag::new(
                Level::Error,
                format!(
                    "cannot index into value of type `{}`",
                    ctx.tcx.pool.wrap(*ty)
                ),
            )
            .add_annotation(Annotation::new(Level::Error, *span).label(format!(
                "cannot index into value of type `{}`",
                ctx.tcx.pool.wrap(*ty)
            ))),
            CheckErr::BadAccess { span, ty } => Diag::new(
                Level::Error,
                format!(
                    "cannot perform access on value of type `{}`",
                    ctx.tcx.pool.wrap(*ty)
                ),
            )
            .add_annotation(Annotation::new(Level::Error, *span).label(format!(
                "cannot perform access on value of type `{}`",
                ctx.tcx.pool.wrap(*ty)
            ))),
            CheckErr::TooManyArgs => Diag::new(Level::Error, "too many args for function"),
            CheckErr::TooFewArgs => Diag::new(Level::Error, "too few args for function"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MismatchedTypes {
    pub expected: TypeRef,
    pub recieved: TypeRef,
}

impl MismatchedTypes {
    pub fn err(expected: TypeRef, recieved: TypeRef) -> Box<CheckErr> {
        Box::new(CheckErr::MismatchedTypes(MismatchedTypes {
            expected,
            recieved,
        }))
    }
}

impl Snippetize for MismatchedTypes {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let mut diag = Diag::new(
            Level::Error,
            format!(
                "type mismatch, `{}` vs `{}`",
                ctx.tcx.pool.wrap(self.expected),
                ctx.tcx.pool.wrap(self.recieved),
            ),
        );

        let expected = &ctx.tcx.pool[self.expected];
        let recieved = &ctx.tcx.pool[self.recieved];
        let expected_span = expected.span;
        let recieved_span = recieved.span;

        if let Some(expected_span) = expected_span {
            diag = diag.add_annotation(
                Annotation::new(Level::Info, expected_span).label("expected due to this"),
            );
        }

        if let Some(recieved_str) = recieved_span {
            diag = diag.add_annotation(Annotation::new(Level::Error, recieved_str).label(format!(
                "expected `{}`, found `{}`",
                ctx.tcx.pool.wrap(self.expected),
                ctx.tcx.pool.wrap(self.recieved)
            )));
        }

        diag
    }
}

#[derive(Debug, Clone)]
pub struct NoSuchVal {
    pub val_name: Span,
}

impl Snippetize for NoSuchVal {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let val_str = self.val_name.to_str(ctx.file);

        Diag::new(
            Level::Error,
            format!("cannot find value `{val_str}` in this scope"),
        )
        .add_annotation(
            Annotation::new(Level::Error, self.val_name).label("not found in this scope"),
        )
    }
}

#[derive(Debug, Clone)]
pub struct NoSuchField {
    pub field_name: Span,
}

impl Snippetize for NoSuchField {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let field_str = self.field_name.to_str(ctx.file);

        Diag::new(
            Level::Error,
            format!("cannot find field `{field_str}` on this type"),
        )
        .add_annotation(
            Annotation::new(Level::Error, self.field_name).label("field not found on this type"),
        )
    }
}

#[derive(Debug, Clone)]
pub struct NoSuchMethod {
    pub method_name: Span,
}

impl Snippetize for NoSuchMethod {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let method_str = self.method_name.to_str(ctx.file);

        Diag::new(
            Level::Error,
            format!("cannot find method `{method_str}` on this type"),
        )
        .add_annotation(
            Annotation::new(Level::Error, self.method_name).label("method not found on this type"),
        )
    }
}

#[derive(Debug, Clone)]
pub struct NoReturn {
    pub func_node: ast::FuncNode,
}

impl Snippetize for NoReturn {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let mut diag = Diag::new(Level::Error, "non-nil function does not return");

        if let Some(return_type) = &self.func_node.ty.return_type {
            diag = diag.add_annotation(
                Annotation::new(Level::Error, return_type.span()).label("expected return type"),
            );
        }

        diag = diag.add_annotation(
            Annotation::new(Level::Info, ctx.expr_pool.wrap(&self.func_node).span())
                .label("function here"),
        );

        diag
    }
}

#[derive(Debug, Clone)]
pub struct ReturnCount {
    pub return_node: ast::ReturnStmt,
    pub expected: usize,
}

impl Snippetize for ReturnCount {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let return_span = ctx.expr_pool.wrap(&self.return_node).span();
        Diag::new(Level::Error, "wrong number of returns").add_annotation(
            Annotation::new(Level::Error, return_span).label(format!(
                "expected `{}` return values here, recieved `{}`",
                self.expected,
                self.return_node.vals.len()
            )),
        )
    }
}

#[derive(Debug, Clone)]
pub struct MethodOnWrongType {
    pub span: Span,
    pub ty: TypeRef,
}

impl Snippetize for MethodOnWrongType {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        Diag::new(
            Level::Error,
            format!(
                "tried to declare method on non-struct type `{}`",
                ctx.tcx.pool.wrap(self.ty)
            ),
        )
        .add_annotation(Annotation::new(Level::Error, self.span).label("method declaration here"))
    }
}
