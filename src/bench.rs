use std::time::{Duration, Instant};

use logos::Logos;

use crate::{
    compiler::Compiler,
    driver::add_defines,
    lexer::{Token, TokenKind, Tokens},
    parser::{ExprPool, Parser, TypeList},
    pretty::Printer,
    type_env::TypeEnv,
    typecheck::TypeChecker,
    BenchOpt,
};

pub fn bench_all(args: BenchOpt) {
    report_results(&[
        ("lex", bench_lex(&args)),
        ("parse", bench_parse(&args)),
        ("check", bench_check(&args)),
        ("compile", bench_compile(&args)),
    ]);
}

fn report_results(results: &[(&str, f64)]) {
    let results: Vec<_> = results
        .iter()
        .map(|(name, rate)| (format!("{name}:"), *rate as usize))
        .collect();
    let max_name_len = results
        .iter()
        .map(|(name, _)| name.len())
        .max()
        .unwrap_or_default();
    let max_rate_len = results
        .iter()
        .map(|(_, rate)| rate.ilog10() as usize + 1)
        .max()
        .unwrap_or_default();
    for (name, rate) in results {
        eprintln!("{name: >max_name_len$} {rate: >max_rate_len$} iter/s")
    }
}

fn bench_lex(args: &BenchOpt) -> f64 {
    let contents = std::fs::read_to_string(args.file.clone())
        .unwrap_or_else(|_| panic!("Sould have been able to read file {}", args.file.display()));

    let mut counter = 0;
    let mut timer = Duration::default();
    let total_time = Duration::from_secs_f64(args.total_time);
    while timer < total_time {
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
        counter += 1;
        timer += end - start;
    }

    counter as f64 / total_time.as_secs_f64()
}

fn bench_parse(args: &BenchOpt) -> f64 {
    let contents = std::fs::read_to_string(args.file.clone())
        .unwrap_or_else(|_| panic!("Sould have been able to read file {}", args.file.display()));

    let lex = TokenKind::lexer(&contents);

    let tokens: Tokens = lex
        .spanned()
        .map(|(t, r)| Token {
            kind: t.unwrap(),
            str: unsafe { contents.get_unchecked(r) },
        })
        .collect();

    let mut typelist = TypeList::with_core();

    let mut pool = ExprPool::new();
    let mut parser = Parser {
        tokens: tokens.clone(),
        pool: &mut pool,
    };

    let mut counter = 0;
    let mut timer = Duration::default();
    let total_time = Duration::from_secs_f64(args.total_time);
    while timer < total_time {
        *parser.pool = ExprPool::new();
        parser.tokens = tokens.clone();

        let start = Instant::now();

        while parser.tokens[0].kind != TokenKind::EndOfFile {
            parser.parse_statement(&mut typelist).unwrap();
        }

        let end = Instant::now();
        counter += 1;
        timer += end - start;
    }

    counter as f64 / total_time.as_secs_f64()
}

fn bench_check(args: &BenchOpt) -> f64 {
    let mut type_env_orig = TypeEnv::default();
    for filename in &args.includes {
        add_defines(filename, &mut type_env_orig)
    }

    let contents = std::fs::read_to_string(args.file.clone())
        .unwrap_or_else(|_| panic!("Sould have been able to read file {}", args.file.display()));

    let lex = TokenKind::lexer(&contents);

    let tokens: Tokens = lex
        .spanned()
        .map(|(t, r)| Token {
            kind: t.unwrap(),
            str: unsafe { contents.get_unchecked(r) },
        })
        .collect();

    let mut typelist = TypeList::with_core();

    let mut pool = ExprPool::new();
    let mut parser = Parser {
        tokens: tokens.clone(),
        pool: &mut pool,
    };

    let mut stats = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        stats.push(parser.parse_statement(&mut typelist).unwrap());
    }

    let typechecker = TypeChecker { pool: &pool };

    let mut counter = 0;
    let mut timer = Duration::ZERO;
    let total_time = Duration::from_secs_f64(args.total_time);
    while timer < total_time {
        let mut type_env = type_env_orig.clone();

        let start = Instant::now();

        for stat in &stats {
            match typechecker.check_statement(stat, &mut type_env) {
                Ok(()) => (),
                Err(_) => {
                    let printer = Printer { pool: &pool };
                    panic!("\n{}", printer.print(stat))
                }
            }
        }

        let end = Instant::now();
        counter += 1;
        timer += end - start;
    }

    counter as f64 / total_time.as_secs_f64()
}

fn bench_compile(args: &BenchOpt) -> f64 {
    let mut type_env = TypeEnv::default();

    for filename in &args.includes {
        add_defines(filename, &mut type_env)
    }

    let contents = std::fs::read_to_string(args.file.clone())
        .unwrap_or_else(|_| panic!("Sould have been able to read file {}", args.file.display()));

    let lex = TokenKind::lexer(&contents);

    let tokens: Tokens = lex
        .spanned()
        .map(|(t, r)| Token {
            kind: t.unwrap(),
            str: unsafe { contents.get_unchecked(r) },
        })
        .collect();

    let mut typelist = TypeList::with_core();

    let mut pool = ExprPool::new();
    let mut parser = Parser {
        tokens: tokens.clone(),
        pool: &mut pool,
    };

    let mut stats = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        stats.push(parser.parse_statement(&mut typelist).unwrap());
    }

    let typechecker = TypeChecker { pool: &pool };
    for stat in &stats {
        match typechecker.check_statement(stat, &mut type_env) {
            Ok(()) => (),
            Err(_) => {
                let printer = Printer { pool: &pool };
                panic!("\n{}", printer.print(stat))
            }
        }
    }

    let compiler = Compiler::new(&pool);

    let mut counter = 0;
    let mut timer = Duration::ZERO;
    let total_time = Duration::from_secs_f64(args.total_time);
    while timer < total_time {
        let start = Instant::now();

        for stat in std::hint::black_box(&stats) {
            std::hint::black_box(compiler.compile_statement(stat));
        }

        let end = Instant::now();
        counter += 1;
        timer += end - start;
    }

    counter as f64 / total_time.as_secs_f64()
}
