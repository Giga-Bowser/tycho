use crate::typecheck::{pool::TypePool, type_env::TypeEnv};

#[derive(Debug, Clone)]
pub struct TypeContext<'s> {
    /// types of values
    pub value_map: TypeEnv<'s>,
    pub pool: TypePool<'s>,
}

impl Default for TypeContext<'_> {
    fn default() -> Self {
        let pool = TypePool::new();
        Self {
            value_map: TypeEnv::with_core(&pool),
            pool,
        }
    }
}
