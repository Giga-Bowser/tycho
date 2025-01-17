use std::path::Path;

use logos::Logos;

use crate::{
    lexer::{Token, TokenKind, Tokens},
    parser::{ExprPool, Parser, TypeList},
    type_env::TypeEnv,
    typecheck::TypeChecker,
    types::Type,
};

#[macro_export]
macro_rules! format_to {
    ($buf:expr) => ();
    ($buf:expr, $lit:literal $($arg:tt)*) => {
        {
            use ::std::fmt::Write as _;
            _ = $buf.write_fmt(format_args!($lit $($arg)*))
        }
    };
}

pub fn add_defines(file_path: &Path, type_env: &mut TypeEnv) {
    let contents = std::fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Sould have been able to read file {}", file_path.display()));

    let lex = TokenKind::lexer(&contents);

    let tokens: Tokens = lex
        .spanned()
        .map(|(t, r)| Token {
            kind: t.unwrap(),
            str: unsafe { contents.get_unchecked(r) },
        })
        .collect();

    let mut typelist = TypeList::default();
    typelist.insert("number".to_owned(), Type::Number);
    typelist.insert("string".to_owned(), Type::String);
    typelist.insert("boolean".to_owned(), Type::Boolean);
    typelist.insert("any".to_owned(), Type::Any);

    let mut parser = Parser {
        tokens,
        pool: ExprPool(Vec::new()),
    };

    let mut stats = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        stats.push(parser.parse_statement(&mut typelist).unwrap());
    }

    let typechecker = TypeChecker { pool: parser.pool };

    for stat in stats {
        match typechecker.check_statement(&stat, type_env) {
            Ok(_) => (),
            Err(err) => panic!("{}", err),
        }
    }
}
