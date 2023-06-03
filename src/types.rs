use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<(String, Type)>,
    pub returns: Vec<Type>,
}

#[derive(Debug, Clone)]
pub struct TableType {
    pub key_type: Rc<Type>,
    pub val_type: Rc<Type>,
}

#[derive(Debug, Clone)]
pub struct User {
    pub fields: Vec<(String, Type)>,
}

impl User {
    pub fn get_field(&self, key: &str) -> Option<&Type> {
        for field in &self.fields {
            if field.0 == key {
                return Some(&field.1);
            }
        }
        None
    }

    pub fn get_field_mut(&mut self, key: &str) -> Option<&mut Type> {
        for field in &mut self.fields {
            if field.0 == key {
                return Some(&mut field.1);
            }
        }
        None
    }
}

#[derive(Debug, Clone, Default)]
pub enum Type {
    #[default]
    Nil,
    Any,
    Number,
    String,
    Boolean,
    Function(Function),
    Table(TableType),
    User(User),
    Adaptable,
    Variadic,
    Multiple,
}

impl PartialEq for Type {
    /// now THIS is COMMUTATIVE BAYBEE LETS GOO I LOVE COMMUTATIVE OPS
    fn eq(&self, rhs: &Self) -> bool {
        if std::mem::discriminant(self) == std::mem::discriminant(&Self::Adaptable)
            || std::mem::discriminant(rhs) == std::mem::discriminant(&Self::Adaptable)
        {
            return true;
        }

        if std::mem::discriminant(self) != std::mem::discriminant(rhs) {
            return false;
        }

        if let Self::Table(TableType { key_type, val_type }) = self {
            if let Self::Table(TableType {
                key_type: rhs_key,
                val_type: rhs_val,
            }) = rhs
            {
                return key_type == rhs_key && val_type == rhs_val;
            } else {
                unreachable!() // we know they have the same discriminant
            }
        }

        true
    }
}

impl Type {
    /// please note this is NON-COMMUTATIVE
    pub fn can_equal(&self, rhs: &Type) -> bool {
        if std::mem::discriminant(self) == std::mem::discriminant(&Self::Any) {
            return true;
        }

        if std::mem::discriminant(rhs) == std::mem::discriminant(&Self::Adaptable) {
            return true;
        }

        if std::mem::discriminant(self) != std::mem::discriminant(rhs) {
            return false;
        }

        if let Type::Table(TableType { key_type, val_type }) = self {
            if let Type::Table(TableType {
                key_type: rhs_key,
                val_type: rhs_val,
            }) = rhs
            {
                return key_type.can_equal(rhs_key) && val_type.can_equal(rhs_val);
            } else {
                unreachable!() // we know they have the same discriminant
            }
        }

        true
    }

    pub fn get_field(&self, key: &str) -> Option<&Type> {
        match self {
            Self::User(user) => user.get_field(key),
            _ => None,
        }
    }

    pub fn get_field_mut(&mut self, key: &str) -> Option<&mut Type> {
        match self {
            Self::User(user) => user.get_field_mut(key),
            _ => None,
        }
    }
}
