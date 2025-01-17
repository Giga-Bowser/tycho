use std::time::Instant;

use logos::Logos;

use crate::{
    compiler::Compiler,
    lexer::{Token, TokenKind, Tokens},
    mem_size::DeepSize,
    parser::{ExprPool, Parser, TypeList},
    pretty::Printer,
    type_env::TypeEnv,
    typecheck::TypeChecker,
    util::{add_defines, duration_fmt, ByteFmt},
    BuildOpt,
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

    for filename in &args.includes {
        add_defines(filename, &mut type_env)
    }

    if args.verbose {
        eprintln!("added includes: {}", duration_fmt(include_timer.elapsed()));
    }

    let contents = std::fs::read_to_string(args.file.clone()).unwrap_or_else(|e| panic!("{e}"));

    let lex_timer = Instant::now();

    let lex = TokenKind::lexer(&contents);
    let tokens: Tokens = lex
        .spanned()
        .map(|(t, r)| Token {
            kind: t.unwrap(),
            str: unsafe { contents.get_unchecked(r) },
        })
        .collect();

    if args.verbose {
        eprintln!("lexing done: {}", duration_fmt(lex_timer.elapsed()));
        eprintln!(
            "tokens memory: {}",
            ByteFmt(tokens.0.len() * size_of::<Token>())
        );
    }

    let mut typelist = TypeList::with_core();

    let parse_timer = Instant::now();

    let mut pool = ExprPool::new();
    let mut parser = Parser {
        tokens,
        pool: &mut pool,
    };

    let mut stats = Vec::new();
    while parser.tokens[0].kind != TokenKind::EndOfFile {
        stats.push(parser.parse_statement(&mut typelist).unwrap());
    }

    stats.deep_size_of();

    if args.verbose {
        eprintln!("parsing done: {}", duration_fmt(parse_timer.elapsed()));
        eprintln!("ast memory: {}", ByteFmt(stats.deep_size_of()));
        eprintln!("pool memory: {}", ByteFmt(pool.vec.deep_size_of()));
    }

    let check_timer = Instant::now();

    let typechecker = TypeChecker { pool: &pool };
    for stat in &stats {
        match typechecker.check_statement(stat, &mut type_env) {
            Ok(()) => (),
            Err(e) => {
                let printer = Printer { pool: &pool };
                panic!("typechecking error: {e}\n{}", printer.print(stat))
            }
        }
    }

    if args.verbose {
        eprintln!("checking done: {}", duration_fmt(check_timer.elapsed()));
        eprintln!("type env memory: {}", ByteFmt(type_env.deep_size_of()));
    }

    let compile_timer = Instant::now();

    let compiler = Compiler::new(&pool);
    let mut result = "require(\"lualib.tycho\")\n".to_owned();
    for stat in &stats {
        result += &compiler.compile_statement(stat);
        result.push('\n');
    }

    if args.verbose {
        eprintln!("compiling done: {}", duration_fmt(compile_timer.elapsed()));
    }

    let mut output: Box<dyn std::io::Write> = match &args.output {
        Some(file) => Box::new(std::fs::File::create(file).unwrap_or_else(|e| panic!("{e}"))),
        None => Box::new(std::io::stdout()),
    };

    output
        .write_all(result.as_bytes())
        .unwrap_or_else(|e| panic!("{e}"));
}
