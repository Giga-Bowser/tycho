use crate::{mem_size::DeepSize, parser::ast::Expr, utils::pooled::Pooled};

#[derive(Debug, Clone, Default)]
pub struct ExprPool {
    vec: Vec<Expr>,
}

impl ExprPool {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }

    pub fn add(&mut self, expr: Expr) -> ExprRef {
        let idx = ExprRef::from(self.vec.len());
        self.vec.push(expr);
        idx
    }

    pub fn wrap<T>(&self, expr: T) -> Pooled<'_, T, Self> {
        Pooled::new(self, expr)
    }
}

type ExprRefInner = usize;

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct ExprRef(ExprRefInner);

impl From<ExprRef> for usize {
    #[inline]
    #[allow(clippy::unnecessary_cast)]
    fn from(value: ExprRef) -> Self {
        value.0 as usize
    }
}

impl From<usize> for ExprRef {
    #[inline]
    fn from(value: usize) -> Self {
        ExprRef(value as ExprRefInner)
    }
}

impl std::ops::Index<ExprRef> for ExprPool {
    type Output = Expr;

    fn index(&self, index: ExprRef) -> &Self::Output {
        &self.vec[usize::from(index)]
    }
}

impl std::ops::IndexMut<ExprRef> for ExprPool {
    fn index_mut(&mut self, index: ExprRef) -> &mut Expr {
        &mut self.vec[usize::from(index)]
    }
}

impl DeepSize for ExprPool {
    fn deep_size_of_children(&self) -> usize {
        self.vec.deep_size_of_children()
    }
}
