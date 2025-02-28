use rustc_hash::FxHashMap;

use crate::typecheck::pool::{TypePool, TypeRef};

#[derive(Debug, Clone, Copy)]
pub enum Resolved<'s> {
    Value(TypeRef<'s>),
    Type(TypeRef<'s>),
}

impl<'s> Resolved<'s> {
    #[inline]
    pub const fn inner(self) -> TypeRef<'s> {
        match self {
            Resolved::Value(ty) | Resolved::Type(ty) => ty,
        }
    }

    #[inline]
    pub const fn as_value(self) -> Option<TypeRef<'s>> {
        match self {
            Resolved::Value(ty) => Some(ty),
            Resolved::Type(_) => None,
        }
    }

    #[inline]
    pub const fn as_type(self) -> Option<TypeRef<'s>> {
        match self {
            Resolved::Type(ty) => Some(ty),
            Resolved::Value(_) => None,
        }
    }
}

pub type Scope<'s> = FxHashMap<&'s str, Resolved<'s>>;

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

    pub fn with_core(pool: &TypePool<'s>) -> Self {
        TypeEnv {
            scopes: vec![Scope::from_iter([
                ("number", Resolved::Type(pool.number())),
                ("string", Resolved::Type(pool.string())),
                ("boolean", Resolved::Type(pool.boolean())),
                ("any", Resolved::Type(pool.any())),
            ])],
        }
    }

    pub fn scopes(&self) -> &[Scope<'s>] {
        &self.scopes
    }

    pub fn get(&self, key: &str) -> Option<Resolved<'s>> {
        self.scopes
            .iter()
            .rev()
            .find_map(|map| map.get(key))
            .copied()
    }

    pub fn get_value(&self, key: &str) -> Option<TypeRef<'s>> {
        self.get(key).and_then(Resolved::as_value)
    }

    pub fn get_type(&self, key: &str) -> Option<TypeRef<'s>> {
        self.get(key).and_then(Resolved::as_type)
    }

    pub fn get_value_top(&mut self, key: &str) -> Option<TypeRef<'s>> {
        self.top_mut().get(key).and_then(|it| it.as_value())
    }

    fn top_mut(&mut self) -> &mut Scope<'s> {
        unsafe { self.scopes.last_mut().unwrap_unchecked() }
    }

    pub fn insert_value(&mut self, key: &'s str, val: TypeRef<'s>) {
        self.top_mut().insert(key, Resolved::Value(val));
    }

    pub fn insert_type(&mut self, key: &'s str, val: TypeRef<'s>) {
        self.top_mut().insert(key, Resolved::Type(val));
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
