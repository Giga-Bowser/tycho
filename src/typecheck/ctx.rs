use std::fmt;

use rustc_hash::FxHashMap;

use crate::typecheck::{pool::TypePool, type_env::TypeEnv};

#[derive(Clone)]
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

impl fmt::Debug for TypeContext<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let entries: Vec<_> = self
            .value_map
            .scopes()
            .iter()
            .map(|it| {
                it.iter()
                    .map(|(&k, v)| (k, self.pool.wrap(v.inner())))
                    .collect::<FxHashMap<_, _>>()
            })
            .collect();
        f.debug_struct("TypeContext")
            .field("pool", &self.pool)
            .field("value_map", &entries)
            .finish()
    }
}
