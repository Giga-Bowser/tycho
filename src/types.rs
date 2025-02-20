use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Function<'a> {
    pub params: Vec<(&'a str, Type<'a>)>,
    pub returns: Box<Type<'a>>,
}

#[derive(Debug, Clone)]
pub struct TableType<'a> {
    pub key_type: Rc<Type<'a>>,
    pub val_type: Rc<Type<'a>>,
}

#[derive(Debug, Clone)]
pub struct User<'a> {
    pub fields: Vec<(&'a str, Type<'a>)>,
}

impl<'a> User<'a> {
    pub fn get_field(&self, key: &str) -> Option<&Type<'a>> {
        for field in &self.fields {
            if field.0 == key {
                return Some(&field.1);
            }
        }
        None
    }

    pub fn get_field_mut(&mut self, key: &str) -> Option<&mut Type<'a>> {
        for field in &mut self.fields {
            if field.0 == key {
                return Some(&mut field.1);
            }
        }
        None
    }
}

#[derive(Debug, Clone, Default)]
pub enum TypeKind<'a> {
    #[default]
    Nil,
    Any,
    Number,
    String,
    Boolean,
    Function(Function<'a>),
    Table(TableType<'a>),
    User(User<'a>),
    Adaptable,
    Variadic,
    Multiple(Vec<Type<'a>>),
    Optional(Box<Type<'a>>),
}

#[derive(Debug, Clone, Default)]
pub struct Type<'a> {
    pub kind: TypeKind<'a>,
    pub src: Option<&'a str>,
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

impl<'a> TypeKind<'a> {
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

    pub fn get_field(&self, key: &str) -> Option<&Type<'a>> {
        match self {
            Self::User(user) => user.get_field(key),
            _ => None,
        }
    }

    pub fn get_field_mut(&mut self, key: &str) -> Option<&mut Type<'a>> {
        match self {
            Self::User(user) => user.get_field_mut(key),
            _ => None,
        }
    }
}

impl<'a> From<TypeKind<'a>> for Type<'a> {
    fn from(value: TypeKind<'a>) -> Self {
        Self {
            kind: value,
            src: None,
        }
    }
}
