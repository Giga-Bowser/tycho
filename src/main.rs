use std::{
    collections::HashMap,
    fs,
    hash::BuildHasherDefault,
    path::PathBuf,
    time::{Duration, Instant},
};

use logos::Logos;
use mimalloc::MiMalloc;
use rustc_hash::FxHasher;
use structopt::StructOpt;

mod compiler;
mod errors;
mod lexer;
mod parser;
mod pretty;
mod type_env;
mod typecheck;
mod types;
mod util;

use crate::{
    compiler::Compiler,
    lexer::{Token, TokenKind, Tokens},
    parser::{ExprPool, Parser},
    pretty::Printer,
    type_env::TypeEnv,
    typecheck::TypeChecker,
    types::Type,
};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

type TypeList<'a> = HashMap<String, Type, BuildHasherDefault<FxHasher>>;

fn add_defines(file_path: PathBuf, type_env: &mut TypeEnv) {
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
#[structopt(name = "tycho")]
pub struct Opt {
    #[structopt(parse(from_os_str))]
    file: PathBuf,

    #[structopt(parse(from_os_str))]
    includes: Vec<PathBuf>,

    #[structopt(short, long, default_value = "1000")]
    number: usize,
}

fn bench_lex() {
    let args = Opt::from_args();

    let contents = fs::read_to_string(args.file.clone())
        .unwrap_or_else(|_| panic!("Sould have been able to read file {}", args.file.display()));

    let mut total = Duration::default();
    for _ in 0..args.number {
        let lex = TokenKind::lexer(&contents);

        let start = Instant::now();
        let tokens: Tokens = lex
            .spanned()
            .map(|(t, r)| Token {
                kind: t.unwrap(),
                str: unsafe { contents.get_unchecked(r) },
            })
            .collect();
        std::hint::black_box(tokens);
        let end = Instant::now();
        total += end - start;
    }

    eprintln!(
        "avg lex time: {:.3} us",
        total.as_micros() as f32 / args.number as f32
    );
}

fn bench_parse() {
    let args = Opt::from_args();

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

    let mut total = Duration::default();

    let mut typelist = TypeList::default();
    typelist.insert("number".to_owned(), Type::Number);
    typelist.insert("string".to_owned(), Type::String);
    typelist.insert("boolean".to_owned(), Type::Boolean);
    typelist.insert("any".to_owned(), Type::Any);
    let mut parser = Parser {
        tokens: tokens.clone(),
        pool: ExprPool(Vec::new()),
    };

    for _ in 0..args.number {
        parser.pool.0.clear();
        parser.tokens = tokens.clone();

        let start = Instant::now();

        while parser.tokens[0].kind != TokenKind::EndOfFile {
            parser.statement(&mut typelist).unwrap();
        }

        let end = Instant::now();

        total += end - start;
    }

    eprintln!(
        "avg parse time: {:.3} us",
        total.as_micros() as f32 / args.number as f32
    );
}

fn bench_check() {
    let args = Opt::from_args();

    let mut type_env_orig = TypeEnv::default();

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

    let mut total = Duration::default();
    for _ in 0..args.number {
        let mut type_env = type_env_orig.clone();
        let start = Instant::now();
        for stat in &stats {
            match typechecker.check_statement(stat, &mut type_env) {
                Ok(()) => (),
                Err(_) => {
                    let printer = Printer {
                        pool: typechecker.pool,
                    };
                    panic!("\n{}", printer.print(stat))
                }
            }
        }
        let end = Instant::now();
        total += end - start;
    }

    eprintln!(
        "avg check time: {:.3} us",
        total.as_micros() as f32 / args.number as f32
    );
}

fn bench_compile() {
    let args = Opt::from_args();

    let mut type_env = TypeEnv::default();

    for filename in args.includes {
        add_defines(filename, &mut type_env)
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
    for stat in &stats {
        match typechecker.check_statement(stat, &mut type_env) {
            Ok(()) => (),
            Err(_) => {
                let printer = Printer {
                    pool: typechecker.pool,
                };
                panic!("\n{}", printer.print(stat))
            }
        }
    }

    let compiler = Compiler::new(typechecker.pool);

    let mut total = Duration::default();
    let mut result = String::new();
    for _ in 0..args.number {
        result = String::new();
        let start = Instant::now();

        for stat in &stats {
            result += &std::hint::black_box(compiler.compile_statement(std::hint::black_box(stat)));
            result.push('\n');
        }
        let end = Instant::now();
        total += end - start;
    }

    eprintln!(
        "avg compile time: {:.3} us",
        total.as_micros() as f32 / args.number as f32
    );

    print!("{result}");
}

fn main() {
    bench_lex();
    bench_parse();
    bench_check();
    bench_compile();
}
