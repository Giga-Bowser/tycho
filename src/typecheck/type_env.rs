use rustc_hash::FxHashMap;

use crate::{
    typecheck::pool::{TypePool, TypeRef},
    utils::Ident,
};

#[derive(Debug, Clone, Copy)]
pub enum Resolved {
    Value(TypeRef),
    Type(TypeRef),
}

impl Resolved {
    #[inline]
    pub const fn inner(self) -> TypeRef {
        match self {
            Resolved::Value(ty) | Resolved::Type(ty) => ty,
        }
    }

    #[inline]
    pub const fn as_value(self) -> Option<TypeRef> {
        match self {
            Resolved::Value(ty) => Some(ty),
            Resolved::Type(_) => None,
        }
    }

    #[inline]
    pub const fn as_type(self) -> Option<TypeRef> {
        match self {
            Resolved::Type(ty) => Some(ty),
            Resolved::Value(_) => None,
        }
    }
}

pub type Scope = FxHashMap<Ident, Resolved>;

#[derive(Debug, Clone)]
pub struct TypeEnv {
    scopes: Vec<Scope>,
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            scopes: vec![Scope::default()],
        }
    }

    pub fn with_core(pool: &TypePool) -> Self {
        TypeEnv {
            scopes: vec![Scope::from_iter([
                (Ident::from_str("number"), Resolved::Type(pool.number())),
                (Ident::from_str("string"), Resolved::Type(pool.string())),
                (Ident::from_str("boolean"), Resolved::Type(pool.boolean())),
                (Ident::from_str("any"), Resolved::Type(pool.any())),
            ])],
        }
    }

    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    pub fn get(&self, key: &Ident) -> Option<Resolved> {
        self.scopes
            .iter()
            .rev()
            .find_map(|map| map.get(key))
            .copied()
    }

    pub fn get_value(&self, key: &Ident) -> Option<TypeRef> {
        self.get(key).and_then(Resolved::as_value)
    }

    pub fn get_type(&self, key: &Ident) -> Option<TypeRef> {
        self.get(key).and_then(Resolved::as_type)
    }

    pub fn get_top(&mut self, key: &Ident) -> Option<Resolved> {
        self.top_mut().get(key).copied()
    }

    pub fn get_value_top(&mut self, key: &Ident) -> Option<TypeRef> {
        self.top_mut().get(key).and_then(|it| it.as_value())
    }

    fn top_mut(&mut self) -> &mut Scope {
        unsafe { self.scopes.last_mut().unwrap_unchecked() }
    }

    pub fn insert_value(&mut self, key: Ident, val: TypeRef) {
        self.top_mut().insert(key, Resolved::Value(val));
    }

    pub fn insert_type(&mut self, key: Ident, val: TypeRef) {
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

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}
