use crate::parser::ast::Expr;

#[derive(Debug, Clone, Default)]
pub struct ExprPool<'s> {
    pub vec: Vec<Expr<'s>>,
}

pub type ExprRef = usize;

impl<'s> ExprPool<'s> {
    pub fn new() -> Self {
        Self { vec: Vec::new() }
    }

    pub fn add(&mut self, expr: Expr<'s>) -> ExprRef {
        let idx = self.vec.len() as ExprRef;
        self.vec.push(expr);
        idx
    }
}

impl<'s> std::ops::Index<ExprRef> for ExprPool<'s> {
    type Output = Expr<'s>;

    fn index(&self, index: ExprRef) -> &Self::Output {
        &self.vec[index]
    }
}

impl<'s> std::ops::IndexMut<ExprRef> for ExprPool<'s> {
    fn index_mut(&mut self, index: ExprRef) -> &mut Expr<'s> {
        &mut self.vec[index]
    }
}
