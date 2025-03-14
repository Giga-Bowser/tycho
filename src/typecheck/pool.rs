use crate::{
    mem_size::DeepSize,
    typecheck::types::{PooledType, Struct, Type, TypeKind},
    utils::Ident,
};

#[derive(Debug, Clone)]
pub struct TypePool {
    vec: Vec<Type>,
}

impl TypePool {
    pub fn new() -> Self {
        Self {
            vec: vec![
                TypeKind::Nil.into(),
                TypeKind::Any.into(),
                TypeKind::Number.into(),
                TypeKind::Struct(Struct {
                    name: Ident::from_str("string"),
                    fields: Vec::new(),
                })
                .into(),
                TypeKind::Boolean.into(),
            ],
        }
    }

    pub fn add(&mut self, ty: Type) -> TypeRef {
        let idx = TypeRef::from(self.vec.len());
        self.vec.push(ty);
        idx
    }

    pub const fn nil() -> TypeRef {
        TypeRef::from_usize(0)
    }

    pub const fn any() -> TypeRef {
        TypeRef::from_usize(1)
    }

    pub const fn number() -> TypeRef {
        TypeRef::from_usize(2)
    }

    pub const fn string() -> TypeRef {
        TypeRef::from_usize(3)
    }

    pub const fn boolean() -> TypeRef {
        TypeRef::from_usize(4)
    }

    pub fn wrap(&self, ty: TypeRef) -> PooledType<'_> {
        PooledType::new(self, ty)
    }
}

impl Default for TypePool {
    fn default() -> Self {
        TypePool::new()
    }
}

type TypeRefInner = usize;

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct TypeRef {
    raw: TypeRefInner,
}

impl TypeRef {
    #[inline]
    pub const fn from_usize(value: usize) -> Self {
        TypeRef {
            raw: value as TypeRefInner,
        }
    }

    /// this *probably* isn't what you want
    pub const fn exact_eq(self, rhs: Self) -> bool {
        self.raw == rhs.raw
    }
}

impl From<TypeRef> for usize {
    #[inline]
    #[allow(clippy::unnecessary_cast)]
    fn from(value: TypeRef) -> Self {
        value.raw as usize
    }
}

impl From<usize> for TypeRef {
    #[inline]
    fn from(value: usize) -> Self {
        TypeRef::from_usize(value)
    }
}

impl std::ops::Index<TypeRef> for TypePool {
    type Output = Type;

    fn index(&self, index: TypeRef) -> &Self::Output {
        &self.vec[usize::from(index)]
    }
}

impl std::ops::IndexMut<TypeRef> for TypePool {
    fn index_mut(&mut self, index: TypeRef) -> &mut Type {
        &mut self.vec[usize::from(index)]
    }
}

impl DeepSize for TypePool {
    fn deep_size_of_children(&self) -> usize {
        self.vec.deep_size_of_children()
    }
}
