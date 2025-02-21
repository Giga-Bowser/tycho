use crate::{types::Type, utils::ScopedMap};

pub type TypeEnv<'a, 's> = ScopedMap<'a, String, Type<'s>>;
