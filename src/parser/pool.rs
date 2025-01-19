use crate::parser::ast::Expr;

#[derive(Debug, Clone, Default)]
pub struct ExprPool<'src> {
    pub vec: Vec<Expr<'src>>,
}

pub type ExprRef = usize;

impl<'src> ExprPool<'src> {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }

    pub fn add(&mut self, expr: Expr<'src>) -> ExprRef {
        let idx = self.vec.len() as ExprRef;
        self.vec.push(expr);
        idx
    }
}

impl<'src> std::ops::Index<ExprRef> for ExprPool<'src> {
    type Output = Expr<'src>;

    fn index(&self, index: ExprRef) -> &Self::Output {
        &self.vec[index]
    }
}

impl<'src> std::ops::IndexMut<ExprRef> for ExprPool<'src> {
    fn index_mut(&mut self, index: ExprRef) -> &mut Expr<'src> {
        &mut self.vec[index]
    }
}
