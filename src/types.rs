use std::rc::Rc;

use crate::lexer::Span;

#[derive(Debug, Clone)]
pub struct Function<'s> {
    pub params: Vec<(&'s str, Type<'s>)>,
    pub returns: Box<Type<'s>>,
}

#[derive(Debug, Clone)]
pub struct TableType<'s> {
    pub key_type: Rc<Type<'s>>,
    pub val_type: Rc<Type<'s>>,
}

#[derive(Debug, Clone)]
pub struct User<'s> {
    pub name: &'s str,
    pub fields: Vec<(&'s str, Type<'s>)>,
}

impl<'s> User<'s> {
    pub fn get_field(&self, key: &str) -> Option<&Type<'s>> {
        for field in &self.fields {
            if field.0 == key {
                return Some(&field.1);
            }
        }
        None
    }

    pub fn get_field_mut(&mut self, key: &str) -> Option<&mut Type<'s>> {
        for field in &mut self.fields {
            if field.0 == key {
                return Some(&mut field.1);
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
    User(User<'s>),
    Adaptable,
    Variadic,
    Multiple(Vec<Type<'s>>),
    Optional(Box<Type<'s>>),
}

#[derive(Debug, Clone, Default)]
pub struct Type<'s> {
    pub kind: TypeKind<'s>,
    pub span: Option<Span<'s>>,
}

impl PartialEq for Type<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}

impl Type<'_> {
    pub fn can_equal(&self, other: &Self) -> bool {
        self.kind.can_equal(&other.kind)
    }
}

impl PartialEq for TypeKind<'_> {
    /// now THIS is COMMUTATIVE BAYBEE LETS GOO I LOVE COMMUTATIVE OPS
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (TypeKind::Adaptable, _) | (_, TypeKind::Adaptable) => return true,
            _ => (),
        }

        match (self, rhs) {
            (TypeKind::Optional(opt), other) | (other, TypeKind::Optional(opt)) => {
                if let TypeKind::Nil = other {
                    return true;
                }

                if std::mem::discriminant(other) == std::mem::discriminant(&opt.kind) {
                    return true;
                }

                return false;
            }
            _ => (),
        }

        if std::mem::discriminant(self) != std::mem::discriminant(rhs) {
            return false;
        }

        if let TypeKind::Table(TableType { key_type, val_type }) = self {
            let TypeKind::Table(TableType {
                key_type: rhs_key,
                val_type: rhs_val,
            }) = rhs
            else {
                unreachable!() // we know they have the same discriminant
            };

            return key_type.kind == rhs_key.kind && val_type.kind == rhs_val.kind;
        }

        true
    }
}

impl<'s> TypeKind<'s> {
    /// please note this is NON-COMMUTATIVE
    pub fn can_equal(&self, rhs: &Self) -> bool {
        if let TypeKind::Any = self {
            return true;
        }

        if let TypeKind::Adaptable = rhs {
            return true;
        }

        if let TypeKind::Optional(base) = self {
            return std::mem::discriminant(rhs) == std::mem::discriminant(&base.kind)
                || std::mem::discriminant(rhs) == std::mem::discriminant(&TypeKind::Nil);
        }

        if let TypeKind::Optional(base) = rhs {
            return std::mem::discriminant(self) == std::mem::discriminant(&base.kind);
        }

        if std::mem::discriminant(self) != std::mem::discriminant(rhs) {
            return false;
        }

        if let TypeKind::Table(TableType { key_type, val_type }) = self {
            let TypeKind::Table(TableType {
                key_type: rhs_key,
                val_type: rhs_val,
            }) = rhs
            else {
                unreachable!() // we know they have the same discriminant
            };

            return key_type.kind.can_equal(&rhs_key.kind)
                && val_type.kind.can_equal(&rhs_val.kind);
        }

        true
    }

    pub fn get_field(&self, key: &str) -> Option<&Type<'s>> {
        match self {
            Self::User(user) => user.get_field(key),
            _ => None,
        }
    }

    pub fn get_field_mut(&mut self, key: &str) -> Option<&mut Type<'s>> {
        match self {
            Self::User(user) => user.get_field_mut(key),
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

impl std::fmt::Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeKind::Nil => f.write_str("nil"),
            TypeKind::Any => f.write_str("any"),
            TypeKind::Number => f.write_str("number"),
            TypeKind::String => f.write_str("string"),
            TypeKind::Boolean => f.write_str("boolean"),
            TypeKind::Function(function) => {
                write!(f, "func")?;

                // params
                let inner = function
                    .params
                    .iter()
                    .map(|(name, ty)| {
                        if name.is_empty() {
                            format!("{ty}")
                        } else {
                            format!("{name}: {ty}")
                        }
                    })
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "({inner})")
            }
            TypeKind::Table(TableType { key_type, val_type }) => {
                write!(f, "[{}]{}", key_type, val_type)
            }
            TypeKind::User(user) => f.write_str(user.name),
            TypeKind::Adaptable => f.write_str("<adaptable>"),
            TypeKind::Variadic => f.write_str("..."),
            TypeKind::Multiple(items) => {
                let inner = items
                    .iter()
                    .map(|ty| format!("{ty}"))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "({inner})")
            }
            TypeKind::Optional(inner) => write!(f, "{}?", inner),
        }
    }
}
