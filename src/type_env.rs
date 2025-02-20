use crate::{types::Type, utils::ScopedMap};

pub type TypeEnv<'a, 'src> = ScopedMap<'a, String, Type<'src>>;
