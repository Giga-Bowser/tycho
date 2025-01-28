use crate::{types::Type, utils::ScopedMap};

pub type TypeEnv<'a> = ScopedMap<'a, String, Type>;
