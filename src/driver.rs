use std::{error::Error, path::PathBuf, time::Instant};

use anstream::{eprintln, println};

use crate::{
    cli::{self},
    error::{report_diag, report_err, Diag, Snippetize},
    lexer::{Lexer, Token, TokenKind, Tokens},
    luajit::{
        bytecode::{dump_bc, read_dump, Header, Proto},
        compiler::LJCompiler,
    },
    mem_size::DeepSize,
    parser::{ast, pool::ExprPool, Parser},
    sourcemap::{SourceFile, SourceMap},
    transpiler::Transpiler,
    typecheck::{ctx::TypeContext, TypeChecker},
    utils::{duration_fmt, ByteFmt},
};

pub fn build_main(args: &cli::Build) -> Result<(), Box<dyn Error>> {
    let total_timer = Instant::now();

    build(args)?;

    if cli::verbose() {
        eprintln!("total build: {}", duration_fmt(total_timer.elapsed()));
    }

    Ok(())
}

fn build(args: &cli::Build) -> Result<(), Box<dyn Error>> {
    let mut source_map = SourceMap::new();
    let mut tcx = TypeContext::default();
    full_includes(&args.includes, &mut source_map, &mut tcx)?;
    let file = source_map.load_file(&args.file).unwrap();
    let tokens = run_lexer(&file);
    let (expr_pool, stmts) = run_parser(&file, &source_map, &tcx, tokens)?;
    run_typechecker(&file, &source_map, tcx, &expr_pool, &stmts)?;

    let result = if args.bc {
        let protos = run_compiler(&file, &expr_pool, &stmts);
        dump_bc(&Header::default(), &protos)
    } else {
        run_transpiler(&file, &expr_pool, &stmts)
    };

    let mut output: Box<dyn std::io::Write> = match &args.output {
        Some(file) => Box::new(std::fs::File::create(file)?),
        None => Box::new(std::io::stdout()),
    };

    output.write_all(&result)?;

    Ok(())
}

pub fn read_main(args: &cli::Read) -> Result<(), Box<dyn Error>> {
    let dump = std::fs::read(&args.file)?;
    let (header, protos) = read_dump(&dump);

    println!("{header:#?}");
    println!("{protos:#?}");

    Ok(())
}

pub fn print_main(args: &cli::Print) -> Result<(), Box<dyn Error>> {
    let mut source_map = SourceMap::new();
    let mut tcx = TypeContext::default();
    full_includes(&args.includes, &mut source_map, &mut tcx)?;
    let file = source_map.load_file(&args.file).unwrap();
    let tokens = run_lexer(&file);
    let (expr_pool, stmts) = run_parser(&file, &source_map, &tcx, tokens)?;
    run_typechecker(&file, &source_map, tcx, &expr_pool, &stmts)?;
    let protos = run_compiler(&file, &expr_pool, &stmts);
    println!("{protos:#?}");

    Ok(())
}

pub fn full_includes(
    includes: &[PathBuf],
    source_map: &mut SourceMap,
    tcx: &mut TypeContext,
) -> Result<(), Box<dyn Error>> {
    let include_timer = Instant::now();

    let include_files = includes.to_vec();
    source_map.add_sources(include_files)?;

    for file in &source_map.files {
        match add_defines(file, tcx) {
            Ok(()) => (),
            Err(diag) => return Err(report_diag(diag, source_map)?.into()),
        }
    }

    if cli::verbose() {
        eprintln!("added includes: {}", duration_fmt(include_timer.elapsed()));
    }

    Ok(())
}

pub fn run_lexer(file: &SourceFile) -> Tokens {
    let lex_timer = Instant::now();

    let tokens = Lexer::new(file).lex_all();

    if cli::verbose() {
        eprintln!("lexing done: {}", duration_fmt(lex_timer.elapsed()));
        eprintln!(
            "tokens memory: {}",
            ByteFmt(tokens.dq.len() * size_of::<Token>())
        );
    }

    tokens
}

pub fn run_parser(
    file: &SourceFile,
    map: &SourceMap,
    tcx: &TypeContext,
    tokens: Tokens,
) -> Result<(ExprPool, Vec<ast::Stmt>), Box<dyn Error>> {
    let parse_timer = Instant::now();

    let mut expr_pool = ExprPool::new();
    let mut parser = Parser::new(file, tokens, &mut expr_pool);

    let mut stmts = Vec::new();
    while parser.tokens[0].kind != TokenKind::EndOfFile {
        match parser.parse_stmt() {
            Ok(stmt) => stmts.push(stmt),
            Err(e) => {
                return Err(report_err(e.as_ref(), &parser.into_diag_ctx(tcx), map)?.into());
            }
        }
    }

    if cli::verbose() {
        eprintln!("parsing done: {}", duration_fmt(parse_timer.elapsed()));
        eprintln!("ast memory: {}", ByteFmt(stmts.deep_size_of()));
        eprintln!("pool memory: {}", ByteFmt(expr_pool.deep_size_of()));
    }

    Ok((expr_pool, stmts))
}

pub fn run_typechecker(
    file: &SourceFile,
    map: &SourceMap,
    mut tcx: TypeContext,
    expr_pool: &ExprPool,
    stmts: &[ast::Stmt],
) -> Result<(), Box<dyn Error>> {
    let check_timer = Instant::now();

    let mut typechecker = TypeChecker::new(file, &mut tcx, expr_pool);
    for stmt in stmts {
        if let Err(e) = typechecker.check_stmt(stmt) {
            return Err(report_err(e.as_ref(), &typechecker.into_diag_ctx(), map)?.into());
        }
    }

    if cli::verbose() {
        eprintln!("checking done: {}", duration_fmt(check_timer.elapsed()));
        eprintln!("type env memory: {}", ByteFmt(tcx.value_map.deep_size_of()));
        eprintln!("type pool memory: {}", ByteFmt(tcx.pool.deep_size_of()));
    }

    Ok(())
}

pub fn run_transpiler(file: &SourceFile, pool: &ExprPool, stmts: &[ast::Stmt]) -> Vec<u8> {
    let transpile_timer = Instant::now();

    let mut transpiler = Transpiler::new(file, pool);
    for stmt in stmts {
        transpiler.transpile_stmt(stmt);
        transpiler.result.push('\n');
    }
    let res = transpiler.result.into_bytes();

    if cli::verbose() {
        eprintln!(
            "transpiling done: {}",
            duration_fmt(transpile_timer.elapsed())
        );
    }

    res
}

pub fn run_compiler(file: &SourceFile, pool: &ExprPool, stmts: &[ast::Stmt]) -> Vec<Proto> {
    let compile_timer = Instant::now();

    let mut compiler = LJCompiler::new(file, pool);
    compiler.compile_chunk(stmts);
    compiler.fs_finish();

    if cli::verbose() {
        eprintln!("compiling done: {}", duration_fmt(compile_timer.elapsed()));
    }

    compiler.protos
}

fn add_defines(file: &SourceFile, tcx: &mut TypeContext) -> Result<(), Diag> {
    let tokens = Lexer::new(file).lex_all();

    let mut expr_pool = ExprPool::new();
    let mut parser = Parser::new(file, tokens, &mut expr_pool);

    let mut stmts = Vec::new();
    while parser.tokens[0].kind != TokenKind::EndOfFile {
        let stmt = match parser.parse_stmt() {
            Ok(stmt) => stmt,
            Err(e) => return Err(e.snippetize(&parser.into_diag_ctx(tcx))),
        };
        stmts.push(stmt);
    }

    let mut typechecker = TypeChecker::new(file, tcx, &expr_pool);

    for stmt in stmts {
        match typechecker.check_stmt(&stmt) {
            Ok(()) => (),
            Err(e) => {
                return Err(e.snippetize(&typechecker.into_diag_ctx()));
            }
        }
    }

    Ok(())
}
