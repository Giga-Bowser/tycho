use std::fmt;

use crate::{
    typecheck::pool::{TypePool, TypeRef},
    utils::Span,
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

pub struct PooledType<'a, 's> {
    pool: &'a TypePool<'s>,
    ty: TypeRef<'s>,
    depth: u32,
    inside: Option<TypeRef<'s>>,
}

impl<'a, 's> PooledType<'a, 's> {
    pub fn new(pool: &'a TypePool<'s>, ty: TypeRef<'s>) -> Self {
        PooledType {
            pool,
            ty,
            depth: 0,
            inside: None,
        }
    }

    #[must_use]
    pub fn wrap(&self, ty: TypeRef<'s>) -> Self {
        PooledType {
            pool: self.pool,
            ty,
            depth: self.depth + 1,
            inside: self.inside,
        }
    }

    #[must_use]
    pub fn inside(mut self, ty: TypeRef<'s>) -> Self {
        self.inside = Some(ty);
        self
    }
}

impl fmt::Debug for PooledType<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.depth > 255 {
            return f.write_str("<depth limit>");
        }

        match &self.pool[self.ty].kind {
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
                        let ty = self.wrap(*ty);
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
                    _ => write!(f, " -> {}", self.wrap(function.returns)),
                }
            }
            TypeKind::Table(TableType { key_type, val_type }) => {
                let key_type = self.wrap(*key_type);
                let val_type = self.wrap(*val_type);
                write!(f, "[{key_type}]{val_type}",)
            }
            TypeKind::Struct(strukt) => {
                if let Some(inside) = &self.inside {
                    if inside.exact_eq(self.ty) {
                        return f.write_str("Self");
                    }
                }

                // TODO: escape the hell that i am in lol
                if strukt.name == "string" {
                    return f.write_str("string");
                }

                let mut s = &mut (f.debug_struct(strukt.name));

                for (name, ty) in &strukt.fields {
                    s = s.field(name, &self.wrap(*ty).inside(self.ty));
                }

                s.finish()
            }
            TypeKind::Adaptable => f.write_str("_"),
            TypeKind::Variadic => f.write_str("..."),
            TypeKind::Multiple(items) => {
                let inner = items
                    .iter()
                    .map(|ty| format!("{}", self.wrap(*ty)))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "({inner})")
            }
            TypeKind::Optional(inner) => write!(f, "{}?", self.wrap(*inner)),
        }
    }
}

impl fmt::Display for PooledType<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}
