use std::{
    path::PathBuf,
    time::{Duration, Instant},
};

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use mimalloc::MiMalloc;
use tycho::{
    driver::{add_defines, define_sources},
    lexer::{Lexer, TokenKind},
    luajit::{
        bytecode::{dump_bc, Header},
        compiler::LJCompiler,
    },
    parser::{pool::ExprPool, Parser, TypeList},
    transpiler::Transpiler,
    type_env::TypeEnv,
    typecheck::TypeChecker,
    types::pool::TypePool,
};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn benchmark_lexer(c: &mut Criterion) {
    let contents = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));

    c.bench_function("lex", |b| {
        b.iter_custom(|iters| {
            let start = Instant::now();
            for _i in 0..iters {
                black_box(Lexer::lex_all_span(black_box(&contents)));
            }
            start.elapsed()
        });
    });
}

fn benchmark_parser(c: &mut Criterion) {
    let contents = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));
    let tokens = Lexer::lex_all_span(&contents);
    let mut expr_pool = ExprPool::new();
    let mut type_pool = TypePool::new();
    c.bench_function("parse", |b| {
        b.iter_custom(|iters| {
            let mut typelist = TypeList::with_core();
            let mut parser = Parser {
                tokens: tokens.clone(),
                expr_pool: &mut expr_pool,
                type_pool: &mut type_pool,
            };

            let mut elapsed = Duration::ZERO;
            for _ in 0..iters {
                *parser.expr_pool = ExprPool::new();
                *parser.type_pool = TypePool::new();
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
        });
    });
}

fn benchmark_typechecker(c: &mut Criterion) {
    let mut type_env_orig = TypeEnv::default();
    let mut type_pool = TypePool::new();
    let includes = vec![
        PathBuf::from("includes/basic.ty"),
        PathBuf::from("includes/math.ty"),
    ];
    let include_sources = define_sources(includes);
    for (_, source) in &include_sources {
        add_defines(source, &mut type_env_orig, &mut type_pool).unwrap();
    }

    let contents = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));
    let tokens = Lexer::lex_all_span(&contents);

    let mut typelist = TypeList::with_core();

    let mut expr_pool = ExprPool::new();
    let mut parser = Parser {
        tokens: tokens.clone(),
        expr_pool: &mut expr_pool,
        type_pool: &mut type_pool,
    };

    let mut statements = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        statements.push(parser.parse_statement(&mut typelist).unwrap());
    }

    c.bench_function("typecheck", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut type_env = type_env_orig.clone();
                let mut type_pool = type_pool.clone();

                let mut typechecker = TypeChecker {
                    type_env: &mut type_env,
                    expr_pool: &expr_pool,
                    source: &contents,
                    type_pool: &mut type_pool,
                };

                let start = Instant::now();
                for stmt in &statements {
                    match typechecker.check_statement(black_box(stmt)) {
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
        });
    });
}

fn benchmark_compiler(c: &mut Criterion) {
    let contents = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));
    let tokens = Lexer::lex_all_span(&contents);

    let mut typelist = TypeList::with_core();

    let mut expr_pool = ExprPool::new();
    let mut type_pool = TypePool::new();
    let mut parser = Parser {
        tokens: tokens.clone(),
        expr_pool: &mut expr_pool,
        type_pool: &mut type_pool,
    };

    let mut statements = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        statements.push(parser.parse_statement(&mut typelist).unwrap());
    }

    c.bench_function("compile", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut compiler = LJCompiler::new(&expr_pool, &contents);
                let start = Instant::now();
                compiler.compile_chunk(&statements);
                compiler.fs_finish();
                black_box(dump_bc(&Header::default(), &compiler.protos));
                elapsed += start.elapsed();
            }
            elapsed
        });
    });
}

fn benchmark_transpiler(c: &mut Criterion) {
    let contents = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));
    let tokens = Lexer::lex_all_span(&contents);

    let mut typelist = TypeList::with_core();

    let mut expr_pool = ExprPool::new();
    let mut type_pool = TypePool::new();
    let mut parser = Parser {
        tokens: tokens.clone(),
        expr_pool: &mut expr_pool,
        type_pool: &mut type_pool,
    };

    let mut statements = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        statements.push(parser.parse_statement(&mut typelist).unwrap());
    }

    c.bench_function("transpile", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut compiler = Transpiler::new(&expr_pool, &contents);
                let start = Instant::now();
                for stmt in &statements {
                    compiler.transpile_statement(black_box(stmt));
                }
                elapsed += start.elapsed();
                black_box(compiler.result);
            }
            elapsed
        });
    });
}

fn benchmark_all_compile(c: &mut Criterion) {
    let mut type_env_orig = TypeEnv::default();
    let mut type_pool = TypePool::new();
    let includes = vec![
        PathBuf::from("includes/basic.ty"),
        PathBuf::from("includes/math.ty"),
    ];
    let include_sources = define_sources(includes);
    for (_, source) in &include_sources {
        add_defines(source, &mut type_env_orig, &mut type_pool).unwrap();
    }
    let source = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));

    c.bench_function("all [compile]", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut type_env = type_env_orig.clone();
                let mut type_pool = type_pool.clone();
                let source = black_box(&source);

                let start = Instant::now();

                let tokens = Lexer::lex_all_span(source);

                let mut typelist = TypeList::with_core();

                let mut expr_pool = ExprPool::new();
                let mut parser = Parser {
                    tokens: tokens.clone(),
                    expr_pool: &mut expr_pool,
                    type_pool: &mut type_pool,
                };

                let mut statements = Vec::new();

                while parser.tokens[0].kind != TokenKind::EndOfFile {
                    statements.push(parser.parse_statement(&mut typelist).unwrap());
                }

                let mut typechecker = TypeChecker {
                    type_env: &mut type_env,
                    expr_pool: &expr_pool,
                    source,
                    type_pool: &mut type_pool,
                };
                let mut compiler = LJCompiler::new(&expr_pool, source);
                for stmt in &statements {
                    typechecker.check_statement(stmt).unwrap();
                    compiler.compile_statement(black_box(stmt));
                    compiler.func_state.free_reg = compiler.func_state.nactvar;
                }

                elapsed += start.elapsed();
                black_box(compiler.protos);
                black_box(type_env);
            }
            elapsed
        });
    });
}

fn benchmark_all_transpile(c: &mut Criterion) {
    let mut type_env_orig = TypeEnv::default();
    let mut type_pool = TypePool::new();
    let includes = vec![
        PathBuf::from("includes/basic.ty"),
        PathBuf::from("includes/math.ty"),
    ];
    let include_sources = define_sources(includes);
    for (_, source) in &include_sources {
        add_defines(source, &mut type_env_orig, &mut type_pool).unwrap();
    }
    let contents = std::fs::read_to_string("test/test.ty").unwrap_or_else(|e| panic!("{e}"));

    c.bench_function("all [transpile]", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut type_env = type_env_orig.clone();
                let mut type_pool = type_pool.clone();
                let source = black_box(&contents);

                let start = Instant::now();

                let tokens = Lexer::lex_all_span(source);

                let mut typelist = TypeList::with_core();

                let mut expr_pool = ExprPool::new();
                let mut parser = Parser {
                    tokens: tokens.clone(),
                    expr_pool: &mut expr_pool,
                    type_pool: &mut type_pool,
                };

                let mut statements = Vec::new();

                while parser.tokens[0].kind != TokenKind::EndOfFile {
                    statements.push(parser.parse_statement(&mut typelist).unwrap());
                }

                let mut typechecker = TypeChecker {
                    type_env: &mut type_env,
                    expr_pool: &expr_pool,
                    source,
                    type_pool: &mut type_pool,
                };
                let mut transpiler = Transpiler::new(&expr_pool, source);
                for stmt in &statements {
                    typechecker.check_statement(stmt).unwrap();
                    transpiler.transpile_statement(black_box(stmt));
                }

                elapsed += start.elapsed();
                black_box(transpiler.result.as_str());
                black_box(type_env);
            }
            elapsed
        });
    });
}

criterion_group!(
    benches,
    benchmark_lexer,
    benchmark_parser,
    benchmark_typechecker,
    benchmark_compiler,
    benchmark_transpiler,
    benchmark_all_compile,
    benchmark_all_transpile
);
criterion_main!(benches);
