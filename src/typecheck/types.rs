use std::fmt;

use crate::{
    typecheck::pool::{TypePool, TypeRef},
    utils::{Ident, Span, Symbol},
};

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<(Span, TypeRef)>,
    pub returns: TypeRef,
}

#[derive(Debug, Clone)]
pub struct TableType {
    pub key_type: TypeRef,
    pub val_type: TypeRef,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Ident,
    pub fields: Vec<(Symbol, TypeRef)>,
}

impl Struct {
    pub fn get_field(&self, key: Symbol) -> Option<TypeRef> {
        for field in &self.fields {
            if field.0 == key {
                return Some(field.1);
            }
        }
        None
    }
}

#[derive(Debug, Clone, Default)]
pub enum TypeKind {
    #[default]
    Nil,
    Any,
    Number,
    Boolean,
    Function(Function),
    Table(TableType),
    Struct(Struct),
    Adaptable,
    Variadic,
    Multiple(Vec<TypeRef>),
    Optional(TypeRef),
}

#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Option<Span>,
}

impl TypeKind {
    pub fn get_field(&self, key: Symbol) -> Option<TypeRef> {
        match self {
            Self::Struct(strukt) => strukt.get_field(key),
            _ => None,
        }
    }
}

impl From<TypeKind> for Type {
    fn from(value: TypeKind) -> Self {
        Self {
            kind: value,
            span: None,
        }
    }
}

pub struct PooledType<'a> {
    pool: &'a TypePool,
    ty: TypeRef,
    depth: u32,
    inside: Option<TypeRef>,
}

impl<'a> PooledType<'a> {
    pub fn new(pool: &'a TypePool, ty: TypeRef) -> Self {
        PooledType {
            pool,
            ty,
            depth: 0,
            inside: None,
        }
    }

    #[must_use]
    pub fn wrap(&self, ty: TypeRef) -> Self {
        PooledType {
            pool: self.pool,
            ty,
            depth: self.depth + 1,
            inside: self.inside,
        }
    }

    #[must_use]
    pub fn inside(mut self, ty: TypeRef) -> Self {
        self.inside = Some(ty);
        self
    }
}

impl fmt::Debug for PooledType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.depth > 255 {
            return f.write_str("<depth limit>");
        }

        match &self.pool[self.ty].kind {
            TypeKind::Nil => f.write_str("nil"),
            TypeKind::Any => f.write_str("any"),
            TypeKind::Number => f.write_str("number"),
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
                            format!("{name:?}: {ty}")
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
                if strukt.name.symbol == Symbol::intern("string") {
                    return f.write_str("string");
                }

                let mut s =
                    &mut (f.debug_struct(&strukt.name.symbol.get_string().unwrap_or_default()));

                for (name, ty) in &strukt.fields {
                    s = s.field(
                        &name.get_string().unwrap_or_default(),
                        &self.wrap(*ty).inside(self.ty),
                    );
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

impl fmt::Display for PooledType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}
