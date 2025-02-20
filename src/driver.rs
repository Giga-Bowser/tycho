use std::{path::Path, time::Instant};

use crate::{
    lexer::{Lexer, Token, TokenKind, Tokens},
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

    let mut include_files = args.includes.clone();

    while let Some(cur) = include_files.pop() {
        if cur.is_dir() {
            include_files.extend(
                std::fs::read_dir(cur)
                    .unwrap()
                    .filter_map(|entry| entry.map(|it| it.path()).ok()),
            );
        } else {
            add_defines(&cur, &mut type_env);
        }
    }

    if args.verbose {
        eprintln!("added includes: {}", duration_fmt(include_timer.elapsed()));
    }

    let contents = std::fs::read_to_string(args.file.clone()).unwrap_or_else(|e| panic!("{e}"));

    let lex_timer = Instant::now();

    let lex = Lexer::new(&contents);
    let tokens: Tokens<'_> = lex.collect();

    if args.verbose {
        eprintln!("lexing done: {}", duration_fmt(lex_timer.elapsed()));
        eprintln!(
            "tokens memory: {}",
            ByteFmt(tokens.0.len() * size_of::<Token<'_>>())
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
        stmts.push(parser.parse_statement(&mut typelist).unwrap());
    }

    if args.verbose {
        eprintln!("parsing done: {}", duration_fmt(parse_timer.elapsed()));
        eprintln!("ast memory: {}", ByteFmt(stmts.deep_size_of()));
        eprintln!("pool memory: {}", ByteFmt(pool.vec.deep_size_of()));
    }

    let check_timer = Instant::now();

    let typechecker = TypeChecker { pool: &pool };
    for stmt in &stmts {
        match typechecker.check_statement(stmt, &mut type_env) {
            Ok(()) => (),
            Err(e) => {
                panic!("typechecking error: {e}\n{stmt:#?}")
            }
        }
    }

    if args.verbose {
        eprintln!("checking done: {}", duration_fmt(check_timer.elapsed()));
        eprintln!("type env memory: {}", ByteFmt(type_env.deep_size_of()));
    }

    let compile_timer = Instant::now();

    let result = if args.bc {
        compile(&pool, &stmts)
    } else {
        transpile(&pool, &stmts)
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

    while let Some(cur) = include_files.pop() {
        if cur.is_dir() {
            include_files.extend(
                std::fs::read_dir(cur)
                    .unwrap()
                    .filter_map(|entry| entry.map(|it| it.path()).ok()),
            );
        } else {
            add_defines(&cur, &mut type_env);
        }
    }

    if args.verbose {
        eprintln!("added includes: {}", duration_fmt(include_timer.elapsed()));
    }

    let contents = std::fs::read_to_string(args.file.clone()).unwrap_or_else(|e| panic!("{e}"));

    let lex_timer = Instant::now();

    let lex = Lexer::new(&contents);
    let tokens: Tokens<'_> = lex.collect();

    if args.verbose {
        eprintln!("lexing done: {}", duration_fmt(lex_timer.elapsed()));
        eprintln!(
            "tokens memory: {}",
            ByteFmt(tokens.0.len() * size_of::<Token<'_>>())
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
        stmts.push(parser.parse_statement(&mut typelist).unwrap());
    }

    if args.verbose {
        eprintln!("parsing done: {}", duration_fmt(parse_timer.elapsed()));
        eprintln!("ast memory: {}", ByteFmt(stmts.deep_size_of()));
        eprintln!("pool memory: {}", ByteFmt(pool.vec.deep_size_of()));
    }

    let check_timer = Instant::now();

    let typechecker = TypeChecker { pool: &pool };
    for stmt in &stmts {
        match typechecker.check_statement(stmt, &mut type_env) {
            Ok(()) => (),
            Err(e) => {
                panic!("typechecking error: {e}\n{stmt:#?}")
            }
        }
    }

    if args.verbose {
        eprintln!("checking done: {}", duration_fmt(check_timer.elapsed()));
        eprintln!("type env memory: {}", ByteFmt(type_env.deep_size_of()));
    }

    let compile_timer = Instant::now();

    let mut compiler = LJCompiler::new(&pool);
    compiler.compile_chunk(&stmts);
    compiler.fs_finish();

    if args.verbose {
        eprintln!("compiling done: {}", duration_fmt(compile_timer.elapsed()));
    }

    eprintln!("{:#?}", compiler.protos);
}

fn transpile<'pool>(pool: &'pool ExprPool<'pool>, stmts: &[ast::Statement<'_>]) -> Vec<u8> {
    let mut compiler = Transpiler::new(pool);
    for stmt in stmts {
        compiler.transpile_statement(stmt);
        compiler.result.push('\n');
    }
    compiler.result.into_bytes()
}

fn compile<'pool>(pool: &'pool ExprPool<'pool>, stmts: &[ast::Statement<'_>]) -> Vec<u8> {
    let mut compiler = LJCompiler::new(pool);
    compiler.compile_chunk(stmts);

    compiler.fs_finish();
    dump_bc(&Header::default(), &compiler.protos)
}

pub fn add_defines(file_path: &Path, type_env: &mut TypeEnv<'_>) {
    let contents = std::fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Sould have been able to read file {}", file_path.display()));

    let lex = Lexer::new(&contents);
    let tokens: Tokens<'_> = lex.collect();

    let mut typelist = TypeList::with_core();

    let mut pool = ExprPool::new();
    let mut parser = Parser {
        tokens,
        pool: &mut pool,
    };

    let mut stmts = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        stmts.push(parser.parse_statement(&mut typelist).unwrap());
    }

    let typechecker = TypeChecker { pool: &pool };

    for stmt in stmts {
        match typechecker.check_statement(&stmt, type_env) {
            Ok(()) => (),
            Err(err) => panic!("{}", err),
        }
    }
}
