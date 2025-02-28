use std::marker::PhantomData;

use crate::{
    mem_size::DeepSize,
    typecheck::types::{Type, TypeKind},
};

#[derive(Debug, Clone)]
pub struct TypePool<'s> {
    vec: Vec<Type<'s>>,
}

impl<'s> TypePool<'s> {
    pub fn new() -> Self {
        Self {
            vec: vec![
                TypeKind::Nil.into(),
                TypeKind::Any.into(),
                TypeKind::Number.into(),
                TypeKind::String.into(),
                TypeKind::Boolean.into(),
            ],
        }
    }

    pub fn add(&mut self, ty: Type<'s>) -> TypeRef<'s> {
        let idx = TypeRef::from(self.vec.len());
        self.vec.push(ty);
        idx
    }

    pub const fn nil(&self) -> TypeRef<'s> {
        TypeRef::from_usize(0)
    }

    pub const fn any(&self) -> TypeRef<'s> {
        TypeRef::from_usize(1)
    }

    pub const fn number(&self) -> TypeRef<'s> {
        TypeRef::from_usize(2)
    }

    pub const fn string(&self) -> TypeRef<'s> {
        TypeRef::from_usize(3)
    }

    pub const fn boolean(&self) -> TypeRef<'s> {
        TypeRef::from_usize(4)
    }
}

impl Default for TypePool<'_> {
    fn default() -> Self {
        TypePool::new()
    }
}

type TypeRefInner = usize;

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct TypeRef<'s> {
    raw: TypeRefInner,
    _m: PhantomData<Type<'s>>,
}

impl TypeRef<'_> {
    #[inline]
    pub const fn from_usize(value: usize) -> Self {
        TypeRef {
            raw: value as TypeRefInner,
            _m: PhantomData,
        }
    }
}

impl From<TypeRef<'_>> for usize {
    #[inline]
    #[allow(clippy::unnecessary_cast)]
    fn from(value: TypeRef<'_>) -> Self {
        value.raw as usize
    }
}

impl From<usize> for TypeRef<'_> {
    #[inline]
    fn from(value: usize) -> Self {
        TypeRef::from_usize(value)
    }
}

impl<'s> std::ops::Index<TypeRef<'s>> for TypePool<'s> {
    type Output = Type<'s>;

    fn index(&self, index: TypeRef<'s>) -> &Self::Output {
        &self.vec[usize::from(index)]
    }
}

impl<'s> std::ops::IndexMut<TypeRef<'s>> for TypePool<'s> {
    fn index_mut(&mut self, index: TypeRef<'s>) -> &mut Type<'s> {
        &mut self.vec[usize::from(index)]
    }
}

impl DeepSize for TypePool<'_> {
    fn deep_size_of_children(&self) -> usize {
        self.vec.deep_size_of_children()
    }
}
