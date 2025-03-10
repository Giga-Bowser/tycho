use std::{
    error::Error,
    io::BufWriter,
    path::{Path, PathBuf},
    time::Instant,
};

use ariadne::{Label, Report, Source};

use crate::{
    cli::{self},
    error::{Diag, DiagCtx, Snippetize},
    lexer::{Lexer, SpanToken, SpanTokens, TokenKind},
    luajit::{
        bytecode::{dump_bc, Header, Proto},
        compiler::LJCompiler,
    },
    mem_size::DeepSize,
    parser::{ast, pool::ExprPool, Parser},
    transpiler::Transpiler,
    typecheck::{ctx::TypeContext, TypeChecker},
    utils::{duration_fmt, ByteFmt},
};

pub fn main(args: &cli::Build) -> Result<(), Box<dyn Error>> {
    let total_timer = Instant::now();

    build(args)?;

    if cli::verbose() {
        eprintln!("total build: {}", duration_fmt(total_timer.elapsed()));
    }

    Ok(())
}

fn build(args: &cli::Build) -> Result<(), Box<dyn Error>> {
    let mut tcx = TypeContext::default();
    full_includes(&args.includes, &mut tcx)?;
    let source = std::fs::read_to_string(args.file.clone())?;
    let tokens = run_lexer(&source);
    let (expr_pool, stmts) = run_parser(&args.file, &tcx, tokens)?;
    run_typechecker(&args.file, tcx, &source, &expr_pool, &stmts)?;

    let result = if args.bc {
        let protos = run_compiler(&expr_pool, &stmts, &source);
        dump_bc(&Header::default(), &protos)
    } else {
        run_transpiler(&expr_pool, &stmts, &source)
    };

    let mut output: Box<dyn std::io::Write> = match &args.output {
        Some(file) => Box::new(std::fs::File::create(file)?),
        None => Box::new(std::io::stdout()),
    };

    output.write_all(&result)?;

    Ok(())
}

pub fn print_main(args: &cli::Print) -> Result<(), Box<dyn Error>> {
    let mut tcx = TypeContext::default();
    full_includes(&args.includes, &mut tcx)?;
    let source = std::fs::read_to_string(args.file.clone())?;
    let tokens = run_lexer(&source);
    let (expr_pool, stmts) = run_parser(&args.file, &tcx, tokens)?;
    run_typechecker(&args.file, tcx, &source, &expr_pool, &stmts)?;
    let protos = run_compiler(&expr_pool, &stmts, &source);
    eprintln!("{protos:#?}");

    Ok(())
}

fn full_includes(includes: &[PathBuf], tcx: &mut TypeContext) -> Result<(), Box<dyn Error>> {
    let include_timer = Instant::now();

    let include_files = define_sources(includes.to_vec());

    for (path, source) in &include_files {
        match add_defines(source, tcx) {
            Ok(()) => (),
            Err(diag) => return Err(report_diag(diag, source, path)?.into()),
        }
    }

    if cli::verbose() {
        eprintln!("added includes: {}", duration_fmt(include_timer.elapsed()));
    }

    Ok(())
}

fn run_lexer(source: &str) -> SpanTokens<'_> {
    let lex_timer = Instant::now();

    let tokens = Lexer::lex_all_span(source);

    if cli::verbose() {
        eprintln!("lexing done: {}", duration_fmt(lex_timer.elapsed()));
        eprintln!(
            "tokens memory: {}",
            ByteFmt(tokens.dq.len() * size_of::<SpanToken>())
        );
    }

    tokens
}

fn run_parser(
    file: &Path,
    tcx: &TypeContext,
    tokens: SpanTokens<'_>,
) -> Result<(ExprPool, Vec<ast::Stmt>), Box<dyn Error>> {
    let parse_timer = Instant::now();

    let mut expr_pool = ExprPool::new();
    let mut parser = Parser::new(tokens, &mut expr_pool);

    let mut stmts = Vec::new();
    while parser.tokens[0].kind != TokenKind::EndOfFile {
        match parser.parse_stmt() {
            Ok(stmt) => stmts.push(stmt),
            Err(e) => {
                return Err(report_err(e.as_ref(), &parser.into_diag_ctx(tcx), file)?.into());
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

fn run_typechecker(
    file: &Path,
    mut tcx: TypeContext,
    source: &str,
    expr_pool: &ExprPool,
    stmts: &[ast::Stmt],
) -> Result<(), Box<dyn Error>> {
    let check_timer = Instant::now();

    let mut typechecker = TypeChecker::new(&mut tcx, expr_pool, source);
    for stmt in stmts {
        if let Err(e) = typechecker.check_stmt(stmt) {
            return Err(report_err(e.as_ref(), &typechecker.into_diag_ctx(), file)?.into());
        }
    }

    if cli::verbose() {
        eprintln!("checking done: {}", duration_fmt(check_timer.elapsed()));
        eprintln!("type env memory: {}", ByteFmt(tcx.value_map.deep_size_of()));
        eprintln!("type pool memory: {}", ByteFmt(tcx.pool.deep_size_of()));
    }

    Ok(())
}

fn run_transpiler(pool: &ExprPool, stmts: &[ast::Stmt], source: &str) -> Vec<u8> {
    let transpile_timer = Instant::now();

    let mut transpiler = Transpiler::new(pool, source);
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

fn run_compiler(pool: &ExprPool, stmts: &[ast::Stmt], source: &str) -> Vec<Proto> {
    let compile_timer = Instant::now();

    let mut compiler = LJCompiler::new(pool, source);
    compiler.compile_chunk(stmts);
    compiler.fs_finish();

    if cli::verbose() {
        eprintln!("compiling done: {}", duration_fmt(compile_timer.elapsed()));
    }

    compiler.protos
}

pub fn add_defines(source: &str, tcx: &mut TypeContext) -> Result<(), Diag> {
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

fn report_err<'s>(
    err: &impl Snippetize<'s>,
    ctx: &DiagCtx<'_, 's>,
    path: &Path,
) -> Result<String, Box<dyn Error>> {
    report_diag(err.snippetize(ctx), ctx.source, path)
}

fn report_diag(diag: Diag, source: &str, path: &Path) -> Result<String, Box<dyn Error>> {
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
        .write((&path, Source::from(source)), &mut buf)?;

    let bytes = buf.into_inner()?;
    Ok(String::from_utf8(bytes)?)
}
