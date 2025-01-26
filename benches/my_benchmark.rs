use std::{
    path::Path,
    time::{Duration, Instant},
};

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use logos::Logos;
use mimalloc::MiMalloc;
use tycho::{
    driver::add_defines,
    lexer::{Token, TokenKind, Tokens},
    parser::{pool::ExprPool, Parser, TypeList},
    transpiler::Transpiler,
    type_env::TypeEnv,
    typecheck::TypeChecker,
};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn benchmark_lexer(c: &mut Criterion) {
    let contents = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));

    c.bench_function("lex", |b| {
        b.iter_custom(|iters| {
            let start = Instant::now();
            for _i in 0..iters {
                let lex = TokenKind::lexer(black_box(&contents));
                black_box(
                    lex.spanned()
                        .map(|(t, r)| Token {
                            kind: t.unwrap(),
                            str: unsafe { contents.get_unchecked(r) },
                        })
                        .collect::<Tokens>(),
                );
            }
            start.elapsed()
        })
    });
}

fn benchmark_parser(c: &mut Criterion) {
    let contents = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));
    let lex = TokenKind::lexer(&contents);
    let tokens = lex
        .spanned()
        .map(|(t, r)| Token {
            kind: t.unwrap(),
            str: unsafe { contents.get_unchecked(r) },
        })
        .collect::<Tokens>();
    let mut pool = ExprPool::new();
    c.bench_function("parse", |b| {
        b.iter_custom(|iters| {
            let mut typelist = TypeList::with_core();
            let mut parser = Parser {
                tokens: tokens.clone(),
                pool: &mut pool,
            };

            let mut elapsed = Duration::ZERO;
            for _ in 0..iters {
                *parser.pool = ExprPool::new();
                parser.tokens = tokens.clone();
                let mut statements = Vec::new();
                let start = Instant::now();
                while parser.tokens[0].kind != TokenKind::EndOfFile {
                    statements.push(parser.parse_statement(&mut typelist).unwrap());
                }
                black_box(statements);
                elapsed += start.elapsed();
            }

            elapsed
        })
    });
}

fn benchmark_typechecker(c: &mut Criterion) {
    let mut type_env_orig = TypeEnv::default();
    let includes = ["includes/base.ty", "includes/math.ty"];
    for filename in includes {
        add_defines(Path::new(filename), &mut type_env_orig)
    }

    let contents = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));
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

    let mut statements = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        statements.push(parser.parse_statement(&mut typelist).unwrap());
    }

    let typechecker = TypeChecker { pool: &pool };

    c.bench_function("typecheck", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut type_env = type_env_orig.clone();

                let start = Instant::now();
                for stat in &statements {
                    match typechecker.check_statement(black_box(stat), &mut type_env) {
                        Ok(()) => (),
                        Err(_) => {
                            panic!()
                        }
                    }
                }
                black_box(type_env);
                elapsed += start.elapsed();
            }
            elapsed
        })
    });
}

fn benchmark_transpiler(c: &mut Criterion) {
    let contents = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));
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

    let mut statements = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        statements.push(parser.parse_statement(&mut typelist).unwrap());
    }

    c.bench_function("compile", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut compiler = Transpiler::new(&pool);
                let start = Instant::now();
                for stat in &statements {
                    compiler.transpile_statement(black_box(stat));
                }
                elapsed += start.elapsed();
                black_box(compiler.result);
            }
            elapsed
        })
    });
}

fn benchmark_all(c: &mut Criterion) {
    let mut type_env_orig = TypeEnv::default();
    let includes = ["includes/base.ty", "includes/math.ty"];
    let contents = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));
    for filename in includes {
        add_defines(Path::new(filename), &mut type_env_orig)
    }

    c.bench_function("all", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut type_env = type_env_orig.clone();
                let contents = black_box(&contents);

                let start = Instant::now();

                let lex = TokenKind::lexer(contents);

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

                let mut statements = Vec::new();

                while parser.tokens[0].kind != TokenKind::EndOfFile {
                    statements.push(parser.parse_statement(&mut typelist).unwrap());
                }

                let typechecker = TypeChecker { pool: &pool };
                let mut compiler = Transpiler::new(&pool);
                for stat in &statements {
                    typechecker.check_statement(stat, &mut type_env).unwrap();
                    compiler.transpile_statement(black_box(stat));
                }

                elapsed += start.elapsed();
                black_box(compiler.result.as_str());
                black_box(type_env);
            }
            elapsed
        })
    });
}

criterion_group!(
    benches,
    benchmark_lexer,
    benchmark_parser,
    benchmark_typechecker,
    benchmark_transpiler,
    benchmark_all
);
criterion_main!(benches);
