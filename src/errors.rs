use thiserror::Error;

use crate::{lexer::*, types::Type};

macro_rules! thingy {
    ($enum: ident, $target: ident) => {
        impl From<$target> for $enum {
            fn from(value: $target) -> Self {
                $enum::$target(value)
            }
        }
    };
}

#[derive(Debug)]
pub struct UnexpectedToken {
    pub token: PermaToken,
    pub expected_kinds: Vec<TokenKind>,
}

#[derive(Default, Debug)]
pub enum ParseError {
    #[default]
    EmptyError,
    NoSuchVal(Option<String>),
    UnexpectedToken(UnexpectedToken),
}

thingy!(ParseError, UnexpectedToken);

#[derive(Error, Default, Debug)]
pub enum CheckErr {
    #[default]
    #[error("oof")]
    EmptyError,
    #[error("no such val '{0}'")]
    NoSuchVal(String),
    #[error("type mismatch\nexpected: {expected:?}\nrecieved: {recieved:?}")]
    MismatchedTypes { expected: Type, recieved: Type },
    #[error("wrong number of returns")]
    ReturnCount,
    #[error("can't iterate over non-iterable")]
    NotIterable,
    #[error("no such field '{0}'")]
    NoSuchField(String),
    #[error("no method '{0}'")]
    NoSuchMethod(String),
    #[error("non-nil function does not return a value")]
    NoReturn,
    #[error("{0}")]
    CustomError(String),
}
