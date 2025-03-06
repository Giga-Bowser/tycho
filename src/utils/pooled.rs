pub struct Pooled<'a, T, P> {
    pub pool: &'a P,
    pub val: T,
}

impl<'a, T, P> Pooled<'a, T, P> {
    pub fn new(pool: &'a P, idx: T) -> Self {
        Pooled { pool, val: idx }
    }

    pub fn wrap<U>(&self, idx: U) -> Pooled<'_, U, P> {
        Pooled {
            pool: self.pool,
            val: idx,
        }
    }
}
