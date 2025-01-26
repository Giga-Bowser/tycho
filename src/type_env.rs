use crate::{types::Type, util::ScopedMap};

pub type TypeEnv<'a> = ScopedMap<'a, String, Type>;
