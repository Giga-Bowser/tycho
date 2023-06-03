use std::{collections::VecDeque, ops::Index};

use logos::Logos;

use crate::errors::UnexpectedToken;

#[derive(Logos, Debug, Clone, Copy)]
#[logos(skip r"[ \t\n\f]+")]
#[logos(skip r"//[^\n]*")]
pub enum TokenKind {
    #[regex("\"([^\"\\\\]|\\\\[\\s\\S])*\"")]
    StrLit,

    #[regex("[a-zA-Z_][a-zA-Z_0-9]*")]
    Name,

    #[token("->")]
    Arrow,

    #[token("..")]
    DotDot,

    #[token("break")]
    Break,

    #[token("return")]
    Return,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("struct")]
    Struct,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("for")]
    For,

    #[token("while")]
    While,

    #[token("in")]
    In,

    #[token("&&")]
    And,

    #[token("||")]
    Or,

    #[token("!")]
    Not,

    #[token("==")]
    Equality,

    #[token("!=")]
    Inequality,

    #[token(">=")]
    GreterEqual,

    #[token("<=")]
    LessEqual,

    #[token("func")]
    Func,

    #[token("constructor")]
    Constructor,

    #[token("...")]
    Elipsis,

    #[token("nil")]
    Nil,

    #[token("#")]
    Octothorpe,

    #[token("&")]
    BitAnd,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("*")]
    Asterisk,

    #[token("+")]
    Plus,

    #[token(",")]
    Comma,

    #[token("-")]
    Minus,

    #[token(".")]
    Dot,

    #[token("/")]
    Slash,

    #[regex("[0-9]+([.][0-9]+([eE][+-]?[0-9]+)?)?")]
    NumLit,

    #[token(":")]
    Colon,

    #[token("<")]
    Less,

    #[token("=")]
    Equal,

    #[token(">")]
    Greater,

    #[token("[")]
    LSquare,

    #[token("]")]
    RSquare,

    #[token("^")]
    Caret,

    #[token("{")]
    LCurly,

    #[token("|")]
    BitOr,

    #[token("}")]
    RCurly,

    EndOfFile,
}

impl PartialEq for TokenKind {
    fn eq(&self, other: &Self) -> bool {
        core::mem::discriminant(self) == core::mem::discriminant(other)
    }
}

#[derive(Debug)]
pub struct Token<'source> {
    pub kind: TokenKind,
    pub str: &'source str,
}

/// permanent token for error reporting.
#[derive(Debug)]
pub struct PermaToken {
    pub kind: TokenKind,
    pub str: String,
}

impl From<&Token<'_>> for PermaToken {
    fn from(value: &Token<'_>) -> Self {
        Self {
            kind: value.kind,
            str: value.str.to_owned(),
        }
    }
}

#[derive(Debug)]
pub struct Tokens<'source>(pub VecDeque<Token<'source>>);

impl<'source> Index<usize> for Tokens<'source> {
    type Output = Token<'source>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<'source> FromIterator<Token<'source>> for Tokens<'source> {
    fn from_iter<T: IntoIterator<Item = Token<'source>>>(iter: T) -> Self {
        let mut dq = VecDeque::from_iter(iter);
        dq.push_back(Token {
            kind: TokenKind::EndOfFile,
            str: "",
        });
        Tokens(dq)
    }
}

impl<'source> Tokens<'source> {
    pub fn pop_front(&mut self) -> Token {
        self.0.pop_front().unwrap()
    }

    pub fn pop_name(&mut self) -> Result<&'source str, UnexpectedToken> {
        if self.0[0].kind != TokenKind::Name {
            return Err(UnexpectedToken {
                token: (&self.0[0]).into(),
                expected_kinds: vec![TokenKind::Name],
            });
        }

        Ok(self.0.pop_front().unwrap().str)
    }

    pub fn expect(&mut self, expected_kind: TokenKind) -> Result<(), UnexpectedToken> {
        if self.0[0].kind == expected_kind {
            self.0.pop_front();
            Ok(())
        } else {
            Err(UnexpectedToken {
                token: (&self.0[0]).into(),
                expected_kinds: vec![expected_kind],
            })
        }
    }
}
