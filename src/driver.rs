use std::{
    io::BufWriter,
    path::{Path, PathBuf},
    time::Instant,
};

use ariadne::{Label, Report, Source};

use crate::{
    errors::{Diag, Snippetize},
    lexer::{Lexer, SpanToken, TokenKind},
    luajit::{
        bytecode::{dump_bc, Header},
        compiler::LJCompiler,
    },
    mem_size::DeepSize,
    parser::{ast, pool::ExprPool, Parser, TypeList},
    transpiler::Transpiler,
    type_env::TypeEnv,
    typecheck::TypeChecker,
    utils::{duration_fmt, ByteFmt},
    BuildOpt, PrintOpt,
};

pub fn main(args: BuildOpt) {
    let total_timer = Instant::now();
    build(&args);
    if args.verbose {
        eprintln!("total build: {}", duration_fmt(total_timer.elapsed()));
    }
}

fn build(args: &BuildOpt) {
    let mut type_env = TypeEnv::default();

    let include_timer = Instant::now();

    let include_files = args.includes.clone();
    let include_sources = define_sources(include_files);

    for (path, source) in &include_sources {
        match add_defines(source, &mut type_env) {
            Ok(()) => (),
            Err(diag) => panic!("{}", report_diag(diag, source, path)),
        }
    }

    if args.verbose {
        eprintln!("added includes: {}", duration_fmt(include_timer.elapsed()));
    }

    let contents = std::fs::read_to_string(args.file.clone()).unwrap_or_else(|e| panic!("{e}"));

    let lex_timer = Instant::now();

    let tokens = Lexer::lex_all_span(&contents);

    if args.verbose {
        eprintln!("lexing done: {}", duration_fmt(lex_timer.elapsed()));
        eprintln!(
            "tokens memory: {}",
            ByteFmt(tokens.dq.len() * size_of::<SpanToken<'_>>())
        );
    }

    let mut typelist = TypeList::with_core();

    let parse_timer = Instant::now();

    let mut pool = ExprPool::new();
    let mut parser = Parser {
        tokens,
        pool: &mut pool,
    };

    let mut stmts = Vec::new();
    while parser.tokens[0].kind != TokenKind::EndOfFile {
        match parser.parse_statement(&mut typelist) {
            Ok(stmt) => stmts.push(stmt),
            Err(e) => panic!("{}", report_err(e, &contents, &args.file)),
        }
    }

    if args.verbose {
        eprintln!("parsing done: {}", duration_fmt(parse_timer.elapsed()));
        eprintln!("ast memory: {}", ByteFmt(stmts.deep_size_of()));
        eprintln!("pool memory: {}", ByteFmt(pool.vec.deep_size_of()));
    }

    let check_timer = Instant::now();

    let typechecker = TypeChecker {
        pool: &pool,
        source: &contents,
    };
    for stmt in &stmts {
        match typechecker.check_statement(stmt, &mut type_env) {
            Ok(()) => (),
            Err(e) => panic!("{}", report_err(e, &contents, &args.file)),
        }
    }

    if args.verbose {
        eprintln!("checking done: {}", duration_fmt(check_timer.elapsed()));
        eprintln!("type env memory: {}", ByteFmt(type_env.deep_size_of()));
    }

    let compile_timer = Instant::now();

    let result = if args.bc {
        compile(&pool, &stmts, &contents)
    } else {
        transpile(&pool, &stmts, &contents)
    };

    if args.verbose {
        eprintln!("compiling done: {}", duration_fmt(compile_timer.elapsed()));
    }

    let mut output: Box<dyn std::io::Write> = match &args.output {
        Some(file) => Box::new(std::fs::File::create(file).unwrap_or_else(|e| panic!("{e}"))),
        None => Box::new(std::io::stdout()),
    };

    output.write_all(&result).unwrap_or_else(|e| panic!("{e}"));
}

pub fn print_main(args: &PrintOpt) {
    let mut type_env = TypeEnv::default();

    let include_timer = Instant::now();

    let mut include_files = args.includes.clone();
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
            include_sources.push((contents, cur));
        }
    }

    for (source, path) in &include_sources {
        match add_defines(source, &mut type_env) {
            Ok(()) => (),
            Err(diag) => panic!("{}", report_diag(diag, source, path)),
        }
    }

    if args.verbose {
        eprintln!("added includes: {}", duration_fmt(include_timer.elapsed()));
    }

    let contents = std::fs::read_to_string(args.file.clone()).unwrap_or_else(|e| panic!("{e}"));

    let lex_timer = Instant::now();

    let tokens = Lexer::lex_all_span(&contents);

    if args.verbose {
        eprintln!("lexing done: {}", duration_fmt(lex_timer.elapsed()));
        eprintln!(
            "tokens memory: {}",
            ByteFmt(tokens.dq.len() * size_of::<SpanToken<'_>>())
        );
    }

    let mut typelist = TypeList::with_core();

    let parse_timer = Instant::now();

    let mut pool = ExprPool::new();
    let mut parser = Parser {
        tokens,
        pool: &mut pool,
    };

    let mut stmts = Vec::new();
    while parser.tokens[0].kind != TokenKind::EndOfFile {
        match parser.parse_statement(&mut typelist) {
            Ok(stmt) => stmts.push(stmt),
            Err(e) => panic!("{}", report_err(e, &contents, &args.file)),
        }
    }

    if args.verbose {
        eprintln!("parsing done: {}", duration_fmt(parse_timer.elapsed()));
        eprintln!("ast memory: {}", ByteFmt(stmts.deep_size_of()));
        eprintln!("pool memory: {}", ByteFmt(pool.vec.deep_size_of()));
    }

    let check_timer = Instant::now();

    let typechecker = TypeChecker {
        pool: &pool,
        source: &contents,
    };
    for stmt in &stmts {
        match typechecker.check_statement(stmt, &mut type_env) {
            Ok(()) => (),
            Err(e) => panic!("{}", report_err(e, &contents, &args.file)),
        }
    }

    if args.verbose {
        eprintln!("checking done: {}", duration_fmt(check_timer.elapsed()));
        eprintln!("type env memory: {}", ByteFmt(type_env.deep_size_of()));
    }

    let compile_timer = Instant::now();

    let mut compiler = LJCompiler::new(&pool, &contents);
    compiler.compile_chunk(&stmts);
    compiler.fs_finish();

    if args.verbose {
        eprintln!("compiling done: {}", duration_fmt(compile_timer.elapsed()));
    }

    eprintln!("{:#?}", compiler.protos);
}

fn transpile<'pool>(
    pool: &'pool ExprPool<'pool>,
    stmts: &[ast::Statement<'_>],
    source: &str,
) -> Vec<u8> {
    let mut compiler = Transpiler::new(pool, source);
    for stmt in stmts {
        compiler.transpile_statement(stmt);
        compiler.result.push('\n');
    }
    compiler.result.into_bytes()
}

fn compile<'pool>(
    pool: &'pool ExprPool<'pool>,
    stmts: &[ast::Statement<'_>],
    source: &str,
) -> Vec<u8> {
    let mut compiler = LJCompiler::new(pool, source);
    compiler.compile_chunk(stmts);

    compiler.fs_finish();
    dump_bc(&Header::default(), &compiler.protos)
}

pub fn add_defines<'s>(source: &'s str, type_env: &mut TypeEnv<'_, 's>) -> Result<(), Diag> {
    let tokens = Lexer::lex_all_span(source);

    let mut typelist = TypeList::with_core();

    let mut pool = ExprPool::new();
    let mut parser = Parser {
        tokens,
        pool: &mut pool,
    };

    let mut stmts = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        let stmt = parser
            .parse_statement(&mut typelist)
            .map_err(|e| e.snippetize(source))?;
        stmts.push(stmt);
    }

    let typechecker = TypeChecker {
        pool: &pool,
        source,
    };

    for stmt in stmts {
        typechecker
            .check_statement(&stmt, type_env)
            .map_err(|e| e.snippetize(source))?;
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

fn report_err<'s>(err: impl Snippetize<'s>, source: &'s str, path: &Path) -> String {
    report_diag(err.snippetize(source), source, path)
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
