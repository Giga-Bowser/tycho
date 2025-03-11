use std::{
    path::PathBuf,
    time::{Duration, Instant},
};

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use mimalloc::MiMalloc;

use tycho::{
    driver::{full_includes, run_lexer, run_parser},
    lexer::{Lexer, TokenKind},
    luajit::{
        bytecode::{dump_bc, Header},
        compiler::LJCompiler,
    },
    parser::{pool::ExprPool, Parser},
    sourcemap::SourceMap,
    transpiler::Transpiler,
    typecheck::{ctx::TypeContext, TypeChecker},
};

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

fn benchmark_lexer(c: &mut Criterion) {
    let mut source_map = SourceMap::new();
    let file = source_map.load_file("test/test.ty").unwrap();

    c.bench_function("lex", |b| {
        b.iter_custom(|iters| {
            let start = Instant::now();
            for _i in 0..iters {
                black_box(Lexer::new(black_box(&file)).lex_all());
            }
            start.elapsed()
        });
    });
}

fn benchmark_parser(c: &mut Criterion) {
    let mut source_map = SourceMap::new();
    let file = source_map.load_file("test/test.ty").unwrap();
    let tokens = run_lexer(&file);
    let mut expr_pool = ExprPool::new();
    c.bench_function("parse", |b| {
        b.iter_custom(|iters| {
            let mut parser = Parser::new(&file, tokens.clone(), &mut expr_pool);

            let mut elapsed = Duration::ZERO;
            for _ in 0..iters {
                *parser.expr_pool = ExprPool::new();
                parser.tokens = tokens.clone();
                let mut stmts = Vec::new();
                let start = Instant::now();
                while parser.tokens[0].kind != TokenKind::EndOfFile {
                    stmts.push(parser.parse_stmt().unwrap());
                }
                black_box(stmts);
                elapsed += start.elapsed();
            }

            elapsed
        });
    });
}

fn benchmark_typechecker(c: &mut Criterion) {
    let mut source_map = SourceMap::new();
    let mut tcx = TypeContext::default();
    full_includes(
        &[
            PathBuf::from("includes/basic.ty"),
            PathBuf::from("includes/math.ty"),
        ],
        &mut source_map,
        &mut tcx,
    )
    .unwrap();
    let file = source_map.load_file("test/test.ty").unwrap();
    let tokens = run_lexer(&file);
    let (expr_pool, stmts) = run_parser(&file, &source_map, &tcx, tokens).unwrap();

    c.bench_function("typecheck", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut tcx = tcx.clone();

                let mut typechecker = TypeChecker::new(&file, &mut tcx, &expr_pool);

                let start = Instant::now();
                for stmt in &stmts {
                    match typechecker.check_stmt(black_box(stmt)) {
                        Ok(()) => (),
                        Err(_) => {
                            panic!()
                        }
                    }
                }
                elapsed += start.elapsed();
            }
            elapsed
        });
    });
}

fn benchmark_compiler(c: &mut Criterion) {
    let mut source_map = SourceMap::new();
    let tcx = TypeContext::default();
    let file = source_map.load_file("test/test.ty").unwrap();
    let tokens = run_lexer(&file);
    let (expr_pool, stmts) = run_parser(&file, &source_map, &tcx, tokens).unwrap();

    c.bench_function("compile", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut compiler = LJCompiler::new(&file, &expr_pool);
                let start = Instant::now();
                compiler.compile_chunk(&stmts);
                compiler.fs_finish();
                black_box(dump_bc(&Header::default(), &compiler.protos));
                elapsed += start.elapsed();
            }
            elapsed
        });
    });
}

fn benchmark_transpiler(c: &mut Criterion) {
    let mut source_map = SourceMap::new();
    let tcx = TypeContext::default();
    let file = source_map.load_file("test/test.ty").unwrap();
    let tokens = run_lexer(&file);
    let (expr_pool, stmts) = run_parser(&file, &source_map, &tcx, tokens).unwrap();

    c.bench_function("transpile", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut compiler = Transpiler::new(&file, &expr_pool);
                let start = Instant::now();
                for stmt in &stmts {
                    compiler.transpile_stmt(black_box(stmt));
                }
                elapsed += start.elapsed();
                black_box(compiler.result);
            }
            elapsed
        });
    });
}

fn benchmark_all_compile(c: &mut Criterion) {
    let mut source_map = SourceMap::new();
    let mut tcx = TypeContext::default();
    full_includes(
        &[
            PathBuf::from("includes/basic.ty"),
            PathBuf::from("includes/math.ty"),
        ],
        &mut source_map,
        &mut tcx,
    )
    .unwrap();
    let file = source_map.load_file("test/test.ty").unwrap();

    c.bench_function("all [compile]", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut tcx = tcx.clone();
                let file = black_box(&file);

                let start = Instant::now();

                let tokens = Lexer::new(file).lex_all();

                let mut expr_pool = ExprPool::new();
                let mut parser = Parser::new(file, tokens, &mut expr_pool);

                let mut stmts = Vec::new();

                while parser.tokens[0].kind != TokenKind::EndOfFile {
                    stmts.push(parser.parse_stmt().unwrap());
                }

                let mut typechecker = TypeChecker::new(file, &mut tcx, &expr_pool);
                let mut compiler = LJCompiler::new(file, &expr_pool);
                for stmt in &stmts {
                    typechecker.check_stmt(stmt).unwrap();
                    compiler.compile_stmt(black_box(stmt));
                    compiler.func_state.free_reg = compiler.func_state.nactvar;
                }

                elapsed += start.elapsed();
                black_box(compiler.protos);
            }
            elapsed
        });
    });
}

fn benchmark_all_transpile(c: &mut Criterion) {
    let mut source_map = SourceMap::new();
    let mut tcx = TypeContext::default();
    full_includes(
        &[
            PathBuf::from("includes/basic.ty"),
            PathBuf::from("includes/math.ty"),
        ],
        &mut source_map,
        &mut tcx,
    )
    .unwrap();
    let file = source_map.load_file("test/test.ty").unwrap();

    c.bench_function("all [transpile]", |b| {
        b.iter_custom(|iters| {
            let mut elapsed = Duration::ZERO;
            for _i in 0..iters {
                let mut tcx = tcx.clone();
                let file = black_box(&file);

                let start = Instant::now();

                let tokens = Lexer::new(file).lex_all();

                let mut expr_pool = ExprPool::new();
                let mut parser = Parser::new(file, tokens, &mut expr_pool);

                let mut stmts = Vec::new();

                while parser.tokens[0].kind != TokenKind::EndOfFile {
                    stmts.push(parser.parse_stmt().unwrap());
                }

                let mut typechecker = TypeChecker::new(file, &mut tcx, &expr_pool);
                let mut transpiler = Transpiler::new(file, &expr_pool);
                for stmt in &stmts {
                    typechecker.check_stmt(stmt).unwrap();
                    transpiler.transpile_stmt(black_box(stmt));
                }

                elapsed += start.elapsed();
                black_box(transpiler.result.as_str());
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
