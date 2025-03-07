use std::{
    io::BufWriter,
    path::{Path, PathBuf},
    time::Instant,
};

use ariadne::{Label, Report, Source};

use crate::{
    cli::{self},
    error::{Diag, DiagCtx, Snippetize},
    lexer::{Lexer, SpanToken, TokenKind},
    luajit::{
        bytecode::{dump_bc, Header},
        compiler::LJCompiler,
    },
    mem_size::DeepSize,
    parser::{ast, pool::ExprPool, Parser},
    transpiler::Transpiler,
    typecheck::{ctx::TypeContext, TypeChecker},
    utils::{duration_fmt, ByteFmt},
};

pub fn main(args: &cli::Build) {
    let total_timer = Instant::now();
    build(args);
    if cli::verbose() {
        eprintln!("total build: {}", duration_fmt(total_timer.elapsed()));
    }
}

fn build(args: &cli::Build) {
    let mut tcx = TypeContext::default();

    let include_timer = Instant::now();

    let include_files = args.includes.clone();
    let include_sources = define_sources(include_files);

    for (path, source) in &include_sources {
        match add_defines(source, &mut tcx) {
            Ok(()) => (),
            Err(diag) => panic!("{}", report_diag(diag, source, path)),
        }
    }

    if cli::verbose() {
        eprintln!("added includes: {}", duration_fmt(include_timer.elapsed()));
    }

    let source = std::fs::read_to_string(args.file.clone()).unwrap_or_else(|e| panic!("{e}"));

    let lex_timer = Instant::now();

    let tokens = Lexer::lex_all_span(&source);

    if cli::verbose() {
        eprintln!("lexing done: {}", duration_fmt(lex_timer.elapsed()));
        eprintln!(
            "tokens memory: {}",
            ByteFmt(tokens.dq.len() * size_of::<SpanToken>())
        );
    }

    let parse_timer = Instant::now();

    let mut expr_pool = ExprPool::new();
    let mut parser = Parser::new(tokens, &mut expr_pool);

    let mut stmts = Vec::new();
    while parser.tokens[0].kind != TokenKind::EndOfFile {
        match parser.parse_stmt() {
            Ok(stmt) => stmts.push(stmt),
            Err(e) => panic!(
                "{}",
                report_err(*e, &parser.into_diag_ctx(&tcx), &args.file)
            ),
        }
    }

    if cli::verbose() {
        eprintln!("parsing done: {}", duration_fmt(parse_timer.elapsed()));
        eprintln!("ast memory: {}", ByteFmt(stmts.deep_size_of()));
        eprintln!("pool memory: {}", ByteFmt(expr_pool.deep_size_of()));
    }

    let check_timer = Instant::now();

    let mut typechecker = TypeChecker::new(&mut tcx, &expr_pool, &source);
    for stmt in &stmts {
        match typechecker.check_stmt(stmt) {
            Ok(()) => (),
            Err(e) => panic!(
                "{}",
                report_err(*e, &typechecker.into_diag_ctx(), &args.file)
            ),
        }
    }

    if cli::verbose() {
        eprintln!("checking done: {}", duration_fmt(check_timer.elapsed()));
        eprintln!("type env memory: {}", ByteFmt(tcx.value_map.deep_size_of()));
        eprintln!("type pool memory: {}", ByteFmt(tcx.pool.deep_size_of()));
    }

    let compile_timer = Instant::now();

    let result = if args.bc {
        compile(&expr_pool, &stmts, &source)
    } else {
        transpile(&expr_pool, &stmts, &source)
    };

    if cli::verbose() {
        eprintln!("compiling done: {}", duration_fmt(compile_timer.elapsed()));
    }

    let mut output: Box<dyn std::io::Write> = match &args.output {
        Some(file) => Box::new(std::fs::File::create(file).unwrap_or_else(|e| panic!("{e}"))),
        None => Box::new(std::io::stdout()),
    };

    output.write_all(&result).unwrap_or_else(|e| panic!("{e}"));
}

pub fn print_main(args: &cli::Print) {
    let mut tcx = TypeContext::default();

    let include_timer = Instant::now();

    let include_files = args.includes.clone();
    let include_sources = define_sources(include_files);

    for (path, source) in &include_sources {
        match add_defines(source, &mut tcx) {
            Ok(()) => (),
            Err(diag) => panic!("{}", report_diag(diag, source, path)),
        }
    }

    if cli::verbose() {
        eprintln!("added includes: {}", duration_fmt(include_timer.elapsed()));
    }

    let source = std::fs::read_to_string(args.file.clone()).unwrap_or_else(|e| panic!("{e}"));

    let lex_timer = Instant::now();

    let tokens = Lexer::lex_all_span(&source);

    if cli::verbose() {
        eprintln!("lexing done: {}", duration_fmt(lex_timer.elapsed()));
        eprintln!(
            "tokens memory: {}",
            ByteFmt(tokens.dq.len() * size_of::<SpanToken>())
        );
    }

    let parse_timer = Instant::now();

    let mut expr_pool = ExprPool::new();
    let mut parser = Parser::new(tokens, &mut expr_pool);

    let mut stmts = Vec::new();
    while parser.tokens[0].kind != TokenKind::EndOfFile {
        match parser.parse_stmt() {
            Ok(stmt) => stmts.push(stmt),
            Err(e) => panic!(
                "{}",
                report_err(*e, &parser.into_diag_ctx(&tcx), &args.file)
            ),
        }
    }

    if cli::verbose() {
        eprintln!("parsing done: {}", duration_fmt(parse_timer.elapsed()));
        eprintln!("ast memory: {}", ByteFmt(stmts.deep_size_of()));
        eprintln!("pool memory: {}", ByteFmt(expr_pool.deep_size_of()));
    }

    let check_timer = Instant::now();

    let mut typechecker = TypeChecker::new(&mut tcx, &expr_pool, &source);
    for stmt in &stmts {
        match typechecker.check_stmt(stmt) {
            Ok(()) => (),
            Err(e) => panic!(
                "{}",
                report_err(*e, &typechecker.into_diag_ctx(), &args.file)
            ),
        }
    }

    if cli::verbose() {
        eprintln!("checking done: {}", duration_fmt(check_timer.elapsed()));
        eprintln!("type env memory: {}", ByteFmt(tcx.value_map.deep_size_of()));
    }

    let compile_timer = Instant::now();

    let mut compiler = LJCompiler::new(&expr_pool, &source);
    compiler.compile_chunk(&stmts);
    compiler.fs_finish();

    if cli::verbose() {
        eprintln!("compiling done: {}", duration_fmt(compile_timer.elapsed()));
    }

    eprintln!("{:#?}", compiler.protos);
}

fn transpile(pool: &ExprPool, stmts: &[ast::Stmt], source: &str) -> Vec<u8> {
    let mut compiler = Transpiler::new(pool, source);
    for stmt in stmts {
        compiler.transpile_stmt(stmt);
        compiler.result.push('\n');
    }
    compiler.result.into_bytes()
}

fn compile(pool: &ExprPool, stmts: &[ast::Stmt], source: &str) -> Vec<u8> {
    let mut compiler = LJCompiler::new(pool, source);
    compiler.compile_chunk(stmts);

    compiler.fs_finish();
    dump_bc(&Header::default(), &compiler.protos)
}

pub fn add_defines<'s>(source: &'s str, tcx: &mut TypeContext<'s>) -> Result<(), Diag> {
    let tokens = Lexer::lex_all_span(source);

    let mut expr_pool = ExprPool::new();
    let mut parser = Parser::new(tokens, &mut expr_pool);

    let mut stmts = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        let stmt = match parser.parse_stmt() {
            Ok(stmt) => stmt,
            Err(e) => return Err(e.snippetize(&parser.into_diag_ctx(tcx))),
        };
        stmts.push(stmt);
    }

    let mut typechecker = TypeChecker::new(tcx, &expr_pool, source);

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

pub fn define_sources(mut include_files: Vec<PathBuf>) -> Vec<(PathBuf, String)> {
    let mut include_sources = Vec::new();

    while let Some(cur) = include_files.pop() {
        if cur.is_dir() {
            include_files.extend(
                std::fs::read_dir(cur)
                    .unwrap()
                    .filter_map(|entry| entry.map(|it| it.path()).ok()),
            );
        } else {
            let contents = std::fs::read_to_string(&cur)
                .unwrap_or_else(|_| panic!("Sould have been able to read file {}", cur.display()));
            include_sources.push((cur, contents));
        }
    }

    include_sources
}

fn report_err<'s>(err: impl Snippetize<'s>, ctx: &DiagCtx<'_, 's>, path: &Path) -> String {
    report_diag(err.snippetize(ctx), ctx.source, path)
}

fn report_diag(diag: Diag, source: &str, path: &Path) -> String {
    let Diag {
        title,
        level,
        annotations,
    } = diag;
    let path = path.to_string_lossy();

    let range = annotations
        .first()
        .map(|it| it.range.clone())
        .unwrap_or_default();

    let report = Report::build(level.report_kind(), (&path, range))
        .with_message(&title)
        .with_labels(annotations.iter().map(|it| {
            let mut label = Label::new((&path, it.range.clone())).with_color(it.level.color());
            if let Some(s) = &it.label {
                label = label.with_message(s);
            }
            label
        }));

    let mut buf = BufWriter::new(Vec::new());
    report
        .finish()
        .write((&path, Source::from(source)), &mut buf)
        .unwrap();

    let bytes = buf.into_inner().unwrap();
    String::from_utf8(bytes).unwrap()
}
