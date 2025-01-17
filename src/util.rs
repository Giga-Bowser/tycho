use std::{path::Path, time::Duration};

use logos::Logos;

use crate::{
    lexer::{Token, TokenKind, Tokens},
    parser::{ExprPool, Parser, TypeList},
    type_env::TypeEnv,
    typecheck::TypeChecker,
};

#[macro_export]
macro_rules! format_to {
    ($buf:expr) => ();
    ($buf:expr, $lit:literal $($arg:tt)*) => {
        {
            use ::std::fmt::Write as _;
            _ = $buf.write_fmt(format_args!($lit $($arg)*))
        }
    };
}

pub fn add_defines(file_path: &Path, type_env: &mut TypeEnv) {
    let contents = std::fs::read_to_string(file_path)
        .unwrap_or_else(|_| panic!("Sould have been able to read file {}", file_path.display()));

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
        tokens,
        pool: &mut pool,
    };

    let mut stats = Vec::new();

    while parser.tokens[0].kind != TokenKind::EndOfFile {
        stats.push(parser.parse_statement(&mut typelist).unwrap());
    }

    let typechecker = TypeChecker { pool: &pool };

    for stat in stats {
        match typechecker.check_statement(&stat, type_env) {
            Ok(_) => (),
            Err(err) => panic!("{}", err),
        }
    }
}

pub fn duration_fmt(duration: Duration) -> String {
    if duration.is_zero() {
        return "0 ns".to_owned();
    }

    let mut si_idx = 0;
    let mut float = duration.as_secs_f64() * 1e9;
    let mut rounded = f64::round(float * 100.0) / 100.0;
    while rounded >= 1e3 {
        float /= 1e3;
        rounded = f64::round(float * 100.0) / 100.0;
        si_idx += 1;
        if si_idx == 3 {
            break;
        }
    }

    let unit = match si_idx {
        0 => "ns",
        1 => "µs",
        2 => "ms",
        3 => "s",

        _ => unreachable!(),
    };

    format!("{rounded} {unit}")
}

#[test]
fn test_format_duration() {
    assert_eq!("1 ns", duration_fmt(Duration::from_secs_f64(0.000000001)));
    assert_eq!("10 ns", duration_fmt(Duration::from_secs_f64(0.00000001)));
    assert_eq!("100 ns", duration_fmt(Duration::from_secs_f64(0.0000001)));

    assert_eq!("1 µs", duration_fmt(Duration::from_secs_f64(0.000001)));
    assert_eq!("10 µs", duration_fmt(Duration::from_secs_f64(0.00001)));
    assert_eq!("100 µs", duration_fmt(Duration::from_secs_f64(0.0001)));

    assert_eq!("1 ms", duration_fmt(Duration::from_secs_f64(0.001)));
    assert_eq!("10 ms", duration_fmt(Duration::from_secs_f64(0.01)));
    assert_eq!("100 ms", duration_fmt(Duration::from_secs_f64(0.1)));

    assert_eq!("1 s", duration_fmt(Duration::from_secs_f64(1.0)));
    assert_eq!("10 s", duration_fmt(Duration::from_secs_f64(10.0)));
    assert_eq!("100 s", duration_fmt(Duration::from_secs_f64(100.0)));
    assert_eq!("1000 s", duration_fmt(Duration::from_secs_f64(1000.0)));

    assert_eq!("15.6 µs", duration_fmt(Duration::from_nanos(15_600)));
    assert_eq!("14.12 ms", duration_fmt(Duration::from_nanos(14_123_333)));
    assert_eq!("1 ms", duration_fmt(Duration::from_secs_f64(0.000999999)));
    assert_eq!("20 ms", duration_fmt(Duration::from_secs_f64(0.0199999)));
    assert_eq!("110 ms", duration_fmt(Duration::from_secs_f64(0.1099999)));
    assert_eq!("160 ms", duration_fmt(Duration::from_secs_f64(0.1599999)));
    assert_eq!("801.5 ms", duration_fmt(Duration::from_secs_f64(0.8015)));
    assert_eq!("3.43 s", duration_fmt(Duration::from_secs_f64(3.434999)));
    assert_eq!("3.44 s", duration_fmt(Duration::from_secs_f64(3.435999)));
}
