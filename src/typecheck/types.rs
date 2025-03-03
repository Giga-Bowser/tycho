use std::fmt;

use crate::{
    lexer::Span,
    typecheck::pool::{TypePool, TypeRef},
};

#[derive(Debug, Clone)]
pub struct Function<'s> {
    pub params: Vec<(&'s str, TypeRef<'s>)>,
    pub returns: TypeRef<'s>,
}

#[derive(Debug, Clone)]
pub struct TableType<'s> {
    pub key_type: TypeRef<'s>,
    pub val_type: TypeRef<'s>,
}

#[derive(Debug, Clone)]
pub struct Struct<'s> {
    pub name: &'s str,
    pub fields: Vec<(&'s str, TypeRef<'s>)>,
}

impl<'s> Struct<'s> {
    pub fn get_field(&self, key: &str) -> Option<TypeRef<'s>> {
        for field in &self.fields {
            if field.0 == key {
                return Some(field.1);
            }
        }
        None
    }
}

#[derive(Debug, Clone, Default)]
pub enum TypeKind<'s> {
    #[default]
    Nil,
    Any,
    Number,
    String,
    Boolean,
    Function(Function<'s>),
    Table(TableType<'s>),
    Struct(Struct<'s>),
    Adaptable,
    Variadic,
    Multiple(Vec<TypeRef<'s>>),
    Optional(TypeRef<'s>),
}

#[derive(Debug, Clone)]
pub struct Type<'s> {
    pub kind: TypeKind<'s>,
    pub span: Option<Span<'s>>,
}

impl<'s> TypeKind<'s> {
    pub fn get_field(&self, key: &str) -> Option<TypeRef<'s>> {
        match self {
            Self::Struct(strukt) => strukt.get_field(key),
            _ => None,
        }
    }
}

impl<'s> From<TypeKind<'s>> for Type<'s> {
    fn from(value: TypeKind<'s>) -> Self {
        Self {
            kind: value,
            span: None,
        }
    }
}

impl<'s> Type<'s> {
    pub fn pooled<'a>(&'a self, pool: &'a TypePool<'s>) -> PooledType<'a, 's> {
        PooledType { ty: self, pool }
    }
}

pub struct PooledType<'a, 's> {
    pub ty: &'a Type<'s>,
    pub pool: &'a TypePool<'s>,
}

impl fmt::Debug for PooledType<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.ty.kind {
            TypeKind::Nil => f.write_str("nil"),
            TypeKind::Any => f.write_str("any"),
            TypeKind::Number => f.write_str("number"),
            TypeKind::String => f.write_str("string"),
            TypeKind::Boolean => f.write_str("boolean"),
            TypeKind::Function(function) => {
                f.write_str("func")?;

                // params
                let inner = function
                    .params
                    .iter()
                    .map(|(name, ty)| {
                        let ty = self.pool.wrap(*ty);
                        if name.is_empty() {
                            format!("{ty}")
                        } else {
                            format!("{name}: {ty}")
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "({inner})")?;

                match &self.pool[function.returns].kind {
                    TypeKind::Nil => Ok(()),
                    TypeKind::Multiple(types) if types.is_empty() => Ok(()),
                    _ => write!(f, " -> {}", self.pool.wrap(function.returns)),
                }
            }
            TypeKind::Table(TableType { key_type, val_type }) => {
                let key_type = self.pool.wrap(*key_type);
                let val_type = self.pool.wrap(*val_type);
                write!(f, "[{key_type}]{val_type}",)
            }
            TypeKind::Struct(strukt) => {
                let mut s = &mut (f.debug_struct(strukt.name));

                for (name, ty) in &strukt.fields {
                    s = s.field(name, &self.pool.wrap(*ty));
                }

                s.finish()
            }
            TypeKind::Adaptable => f.write_str("_"),
            TypeKind::Variadic => f.write_str("..."),
            TypeKind::Multiple(items) => {
                let inner = items
                    .iter()
                    .map(|ty| format!("{}", self.pool.wrap(*ty)))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "({inner})")
            }
            TypeKind::Optional(inner) => write!(f, "{}?", self.pool.wrap(*inner)),
        }
    }
}

impl fmt::Display for PooledType<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}
