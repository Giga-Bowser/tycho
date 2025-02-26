use crate::parser::ast::Expr;

#[derive(Debug, Clone, Default)]
pub struct ExprPool<'s> {
    pub vec: Vec<Expr<'s>>,
}

impl<'s> ExprPool<'s> {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }

    pub fn add(&mut self, expr: Expr<'s>) -> ExprRef {
        let idx = ExprRef::from(self.vec.len());
        self.vec.push(expr);
        idx
    }
}

type ExprRefInner = usize;

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct ExprRef(ExprRefInner);

impl From<ExprRef> for usize {
    #[inline(always)]
    #[allow(clippy::unnecessary_cast)]
    fn from(value: ExprRef) -> Self {
        value.0 as usize
    }
}

impl From<usize> for ExprRef {
    #[inline(always)]
    fn from(value: usize) -> Self {
        ExprRef(value as ExprRefInner)
    }
}

impl<'s> std::ops::Index<ExprRef> for ExprPool<'s> {
    type Output = Expr<'s>;

    fn index(&self, index: ExprRef) -> &Self::Output {
        &self.vec[usize::from(index)]
    }
}

impl<'s> std::ops::IndexMut<ExprRef> for ExprPool<'s> {
    fn index_mut(&mut self, index: ExprRef) -> &mut Expr<'s> {
        &mut self.vec[usize::from(index)]
    }
}
