use crate::{
    error::{Annotation, Diag, DiagCtx, Level, Snippetize},
    error_kind,
    parser::{ast, pool::ExprRef},
    typecheck::pool::TypeRef,
    utils::{spanned::Spanned, Span},
};

#[derive(Debug, Clone)]
pub struct CheckErr {
    kind: CheckErrKind,
    while_checking: Option<Span>,
}

impl CheckErr {
    pub(crate) fn new(kind: impl Into<CheckErrKind>) -> Box<Self> {
        Box::new(CheckErr {
            kind: kind.into(),
            while_checking: None,
        })
    }

    pub(crate) fn while_checking(mut self: Box<Self>, spanned: impl Spanned) -> Box<Self> {
        self.while_checking = self.while_checking.or(Some(spanned.span()));
        self
    }
}

impl Snippetize for CheckErr {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let mut diag = self.kind.snippetize(ctx);

        if !self.kind.has_context() {
            if let Some(span) = self.while_checking {
                diag.add_annotation(
                    Annotation::new(Level::Info, span).label("while checking this"),
                );
            }
        }

        diag
    }
}

error_kind! {
    #[derive(Debug, Clone)]
    pub(crate) enum CheckErrKind {
        MismatchedTypes,
        NoReturn,
        MethodOnWrongType,
        MethodOnVal,
        NoSuchVal,
        NoSuchType,
        NoSuchField,
        NoSuchMethod,
        NotIterable,
        CallNonFunc,
        BadAccess,
        BadIndex,
        BadNegate,
        BadNot,
        ArgCount,
    }
}

impl CheckErrKind {
    fn has_context(&self) -> bool {
        matches!(self, Self::NoReturn(_))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct MismatchedTypes {
    pub expected: TypeRef,
    pub expected_span: Option<Span>,
    pub recieved: TypeRef,
    pub recieved_span: Option<Span>,
}

impl MismatchedTypes {
    pub(crate) fn full(
        exp_ty: TypeRef,
        exp_span: impl Spanned,
        rec_ty: TypeRef,
        rec_span: impl Spanned,
    ) -> Box<CheckErr> {
        CheckErr::new(MismatchedTypes {
            expected: exp_ty,
            expected_span: Some(exp_span.span()),
            recieved: rec_ty,
            recieved_span: Some(rec_span.span()),
        })
    }

    pub(crate) fn new(expected: TypeRef, recieved: TypeRef) -> Self {
        MismatchedTypes {
            expected,
            recieved,
            expected_span: None,
            recieved_span: None,
        }
    }

    pub(crate) fn expected(mut self, expected: impl Spanned) -> Self {
        self.expected_span = Some(expected.span());
        self
    }

    pub(crate) fn recieved(mut self, recieved: impl Spanned) -> Self {
        self.recieved_span = Some(recieved.span());
        self
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
        let expected_span = self.expected_span.or(expected.span);
        let recieved_span = self.recieved_span.or(recieved.span);

        if let Some(expected_span) = expected_span {
            diag.add_annotation(
                Annotation::new(Level::Info, expected_span).label("expected due to this"),
            );
        }

        if let Some(recieved_str) = recieved_span {
            diag.add_annotation(Annotation::new(Level::Error, recieved_str).label(format!(
                "expected `{}`, found `{}`",
                ctx.tcx.pool.wrap(self.expected),
                ctx.tcx.pool.wrap(self.recieved)
            )));
        }

        diag
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NoReturn {
    pub func_node: ast::FuncNode,
}

impl Snippetize for NoReturn {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let mut diag = Diag::new(Level::Error, "non-nil function does not return");

        if let Some(return_type) = &self.func_node.ty.return_type {
            diag.add_annotation(
                Annotation::new(Level::Error, return_type.span()).label("expected return type"),
            );
        }

        diag.add_annotation(
            Annotation::new(Level::Info, ctx.expr_pool.wrap(&self.func_node).span())
                .label("function here"),
        );

        diag
    }
}

#[derive(Debug, Clone)]
pub(crate) struct MethodOnWrongType {
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
        .with_annotation(Annotation::new(Level::Error, self.span).label("method declaration here"))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct MethodOnVal {
    pub span: Span,
    pub ty: TypeRef,
}

impl Snippetize for MethodOnVal {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        Diag::new(
            Level::Error,
            format!(
                "tried to declare method on value of type `{}`",
                ctx.tcx.pool.wrap(self.ty)
            ),
        )
        .with_annotation(Annotation::new(Level::Error, self.span).label("method declaration here"))
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NoSuchVal {
    pub val_name: Span,
}

impl Snippetize for NoSuchVal {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let val_str = self.val_name.to_str(ctx.file);

        Diag::new(
            Level::Error,
            format!("cannot find value `{val_str}` in this scope"),
        )
        .with_annotation(
            Annotation::new(Level::Error, self.val_name).label("not found in this scope"),
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NoSuchType {
    pub ty_name: Span,
}

impl Snippetize for NoSuchType {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let ty_str = self.ty_name.to_str(ctx.file);

        Diag::new(
            Level::Error,
            format!("cannot find type `{ty_str}` in this scope"),
        )
        .with_annotation(
            Annotation::new(Level::Error, self.ty_name).label("not found in this scope"),
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NoSuchField {
    pub field_name: Span,
}

impl Snippetize for NoSuchField {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let field_str = self.field_name.to_str(ctx.file);

        Diag::new(
            Level::Error,
            format!("cannot find field `{field_str}` on this type"),
        )
        .with_annotation(
            Annotation::new(Level::Error, self.field_name).label("field not found on this type"),
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NoSuchMethod {
    pub method_name: Span,
}

impl Snippetize for NoSuchMethod {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let method_str = self.method_name.to_str(ctx.file);

        Diag::new(
            Level::Error,
            format!("cannot find method `{method_str}` on this type"),
        )
        .with_annotation(
            Annotation::new(Level::Error, self.method_name).label("method not found on this type"),
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NotIterable {
    ty: TypeRef,
    expr: ExprRef,
}

impl NotIterable {
    pub(crate) fn err(ty: TypeRef, expr: ExprRef) -> Box<CheckErr> {
        CheckErr::new(NotIterable { ty, expr })
    }
}

impl Snippetize for NotIterable {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let ty = ctx.tcx.pool.wrap(self.ty);
        Diag::new(
            Level::Error,
            format!("cannot iterate over value of type `{ty}`",),
        )
        .with_annotation(
            Annotation::new(Level::Error, ctx.expr_pool.wrap(self.expr).span())
                .label(format!("expected iterable type, found `{ty}`")),
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct CallNonFunc {
    ty: TypeRef,
    call_span: Span,
}

impl CallNonFunc {
    pub(crate) fn err(ty: TypeRef, call_span: Span) -> Box<CheckErr> {
        CheckErr::new(CallNonFunc { ty, call_span })
    }
}

impl Snippetize for CallNonFunc {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let ty = ctx.tcx.pool.wrap(self.ty);
        Diag::new(Level::Error, "cannot call non-function value").with_annotation(
            Annotation::new(Level::Error, self.call_span)
                .label(format!("expected `func` type, found `{ty}`")),
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BadAccess {
    span: Span,
    ty: TypeRef,
}

impl BadAccess {
    pub(crate) fn err(span: Span, ty: TypeRef) -> Box<CheckErr> {
        CheckErr::new(Self { span, ty })
    }
}

impl Snippetize for BadAccess {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let ty = ctx.tcx.pool.wrap(self.ty);
        Diag::new(
            Level::Error,
            format!("cannot perform access on value of type `{ty}`",),
        )
        .with_annotation(
            Annotation::new(Level::Error, self.span)
                .label(format!("expected accessible type, found `{ty}`",)),
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BadIndex {
    span: Span,
    ty: TypeRef,
}

impl BadIndex {
    pub(crate) fn err(span: Span, ty: TypeRef) -> Box<CheckErr> {
        CheckErr::new(Self { span, ty })
    }
}

impl Snippetize for BadIndex {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        let ty = ctx.tcx.pool.wrap(self.ty);
        Diag::new(
            Level::Error,
            format!("cannot index into value of type `{ty}`"),
        )
        .with_annotation(
            Annotation::new(Level::Error, self.span)
                .label(format!("expected indexable type, found `{ty}`")),
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BadNegate {
    op_span: Span,
    ty: TypeRef,
}

impl BadNegate {
    pub(crate) fn err(op_span: Span, ty: TypeRef) -> Box<CheckErr> {
        CheckErr::new(Self { op_span, ty })
    }
}

impl Snippetize for BadNegate {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        Diag::new(Level::Error, "cannot unary negate non-number type").with_annotation(
            Annotation::new(Level::Error, self.op_span).label(format!(
                "expected `number` for this operator, found `{}`",
                ctx.tcx.pool.wrap(self.ty)
            )),
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BadNot {
    op_span: Span,
    ty: TypeRef,
}

impl BadNot {
    pub(crate) fn err(op_span: Span, ty: TypeRef) -> Box<CheckErr> {
        CheckErr::new(Self { op_span, ty })
    }
}

impl Snippetize for BadNot {
    fn snippetize(&self, ctx: &DiagCtx<'_>) -> Diag {
        Diag::new(Level::Error, "cannot unary not non-boolean type").with_annotation(
            Annotation::new(Level::Error, self.op_span).label(format!(
                "expected `boolean` for this operator, found `{}`",
                ctx.tcx.pool.wrap(self.ty)
            )),
        )
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ArgCount {
    call_span: Span,
    expected: usize,
    recieved: usize,
}

impl ArgCount {
    pub(crate) fn err(call_span: Span, expected: usize, recieved: usize) -> Box<CheckErr> {
        CheckErr::new(Self {
            call_span,
            expected,
            recieved,
        })
    }
}

impl Snippetize for ArgCount {
    fn snippetize(&self, _ctx: &DiagCtx<'_>) -> Diag {
        Diag::new(Level::Error, "wrong number of arguments for function").with_annotation(
            Annotation::new(Level::Error, self.call_span).label(format!(
                "expected {}, found {}",
                self.expected, self.recieved
            )),
        )
    }
}
