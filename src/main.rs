mod lexer;

use crate::lexer::*;

mod parser;

use crate::parser::*;

mod types;

use crate::types::*;

mod errors;
mod typecheck;
use crate::typecheck::*;

use logos::Logos;
use mimalloc::MiMalloc;
use rustc_hash::FxHasher;
use std::collections::HashMap;
use std::fs;
use std::hash::BuildHasherDefault;
use std::path::PathBuf;
use structopt::StructOpt;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

type TypeList<'a> = HashMap<String, Type, BuildHasherDefault<FxHasher>>;

fn add_defines(file_path: PathBuf, type_env: &mut TypeList) {
    let contents = fs::read_to_string(file_path.clone())
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
        stats.push(parser.statement(&mut typelist).unwrap());
    }

    let typechecker = TypeChecker { pool: parser.pool };

    for stat in stats {
        match typechecker.check_statement(&stat, type_env) {
            Ok(_) => (),
            Err(err) => panic!("{}", err),
        }
    }
}

#[derive(StructOpt)]
pub struct Opt {
    #[structopt(short, long, default_value = "1000")]
    number: usize,

    #[structopt(parse(from_os_str))]
    file: PathBuf,

    #[structopt(parse(from_os_str))]
    includes: Vec<PathBuf>,
}

fn main() {
    let args = Opt::from_args();

    let mut type_env_orig = TypeList::default();

    for filename in args.includes {
        add_defines(filename, &mut type_env_orig)
    }

    let contents = fs::read_to_string(args.file.clone())
        .unwrap_or_else(|_| panic!("Sould have been able to read file {}", args.file.display()));

    let lex = TokenKind::lexer(&contents);

    let tokens: Tokens = lex
        .spanned()
        .map(|(t, r)| Token {
            kind: t.unwrap(),
            str: unsafe { contents.get_unchecked(r) },
        })
        .collect();

    // for v in tokens.0.iter() {
    // 	print!("({:?}: '{}'), ", v.kind, v.str);
    // }
    //
    // println!();

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
        stats.push(parser.statement(&mut typelist).unwrap());
    }

    let typechecker = TypeChecker { pool: parser.pool };

    for _ in 0..args.number {
        let mut type_env = type_env_orig.clone();
        for stat in &stats {
            match typechecker.check_statement(stat, &mut type_env) {
                Ok(_) => (),
                Err(err) => panic!("{}", err),
            }
        }
    }
}
