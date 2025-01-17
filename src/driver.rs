use logos::Logos;

use crate::{
    compiler::Compiler,
    lexer::{Token, TokenKind, Tokens},
    parser::{ExprPool, Parser, TypeList},
    pretty::Printer,
    type_env::TypeEnv,
    typecheck::TypeChecker,
    types::Type,
    util::add_defines,
    BuildOpt,
};

pub fn build(args: BuildOpt) {
    let mut type_env = TypeEnv::default();

    for filename in &args.includes {
        add_defines(filename, &mut type_env)
    }

    let contents = std::fs::read_to_string(args.file.clone()).unwrap_or_else(|e| panic!("{e}"));

    let lex = TokenKind::lexer(&contents);

    let tokens: Tokens = lex
        .spanned()
        .map(|(t, r)| Token {
            kind: t.unwrap(),
            str: unsafe { contents.get_unchecked(r) },
        })
        .collect();

    let mut typelist = TypeList::default();
    typelist.insert("number".to_owned(), Type::Number);
    typelist.insert("string".to_owned(), Type::String);
    typelist.insert("boolean".to_owned(), Type::Boolean);
    typelist.insert("any".to_owned(), Type::Any);

    let mut parser = Parser {
        tokens,
        pool: ExprPool(Vec::new()),
    };

    let mut stats = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        stats.push(parser.parse_statement(&mut typelist).unwrap());
    }

    let typechecker = TypeChecker { pool: parser.pool };
    for stat in &stats {
        match typechecker.check_statement(stat, &mut type_env) {
            Ok(()) => (),
            Err(e) => {
                let printer = Printer {
                    pool: typechecker.pool,
                };
                panic!("typechecking error: {e}\n{}", printer.print(stat))
            }
        }
    }

    let compiler = Compiler::new(typechecker.pool);

    let mut output: Box<dyn std::io::Write> = match args.output {
        Some(file) => Box::new(std::fs::File::create(file).unwrap_or_else(|e| panic!("{e}"))),
        None => Box::new(std::io::stdout()),
    };

    let mut result = "require(\"lualib.tycho\")\n".to_owned();

    for stat in &stats {
        result += &compiler.compile_statement(stat);
        result.push('\n');
    }

    output
        .write_all(result.as_bytes())
        .unwrap_or_else(|e| panic!("{e}"));
}
