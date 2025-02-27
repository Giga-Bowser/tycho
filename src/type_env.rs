use rustc_hash::FxHashMap;

use crate::types::pool::TypeRef;

pub type Scope<'s> = FxHashMap<&'s str, TypeRef<'s>>;

#[derive(Debug, Clone)]
pub struct TypeEnv<'s> {
    scopes: Vec<Scope<'s>>,
}

impl<'s> TypeEnv<'s> {
    pub fn new() -> Self {
        TypeEnv {
            scopes: vec![Scope::default()],
        }
    }

    pub fn scopes(&self) -> &[Scope<'s>] {
        &self.scopes
    }

    pub fn get(&self, key: &str) -> Option<TypeRef<'s>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|map| map.get(key))
            .copied()
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut TypeRef<'s>> {
        self.top_mut().get_mut(key)
    }

    pub fn get_top(&mut self, key: &str) -> Option<TypeRef<'s>> {
        self.top_mut().get(key).copied()
    }

    fn top_mut(&mut self) -> &mut Scope<'s> {
        unsafe { self.scopes.last_mut().unwrap_unchecked() }
    }

    pub fn insert(&mut self, key: &'s str, val: TypeRef<'s>) {
        self.top_mut().insert(key, val);
    }

    pub fn remove(&mut self, key: &str) -> Option<TypeRef<'s>> {
        self.top_mut().remove(key)
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn pop_scope(&mut self) {
        assert!(self.scopes.len() > 1, "TypeEnv cannot be empty");
        self.scopes.pop();
    }
}

impl Default for TypeEnv<'_> {
    fn default() -> Self {
        Self::new()
    }
}
