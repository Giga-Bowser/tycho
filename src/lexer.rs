use std::{
    collections::VecDeque,
    ops::{Index, IndexMut},
};

use logos::{Lexer, Logos, SpannedIter};

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

    #[token("?")]
    Question,

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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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
    pub fn pop_front(&mut self) -> Token<'_> {
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

const RING_LEN: usize = 4;
#[derive(Debug, Clone)]
pub struct Ring<T: Clone> {
    buf: [T; RING_LEN],
    read_idx: usize,
}

impl<T: Clone> Ring<T> {
    pub fn new(buf: [T; RING_LEN]) -> Self {
        Self { buf, read_idx: 0 }
    }

    fn mask(idx: usize) -> usize {
        idx & (RING_LEN - 1)
    }

    fn rotate(&mut self, value: T) -> T {
        let res = std::mem::replace(&mut self[0], value);
        self.read_idx += 1;
        res
    }
}

impl<T: Clone> Index<usize> for Ring<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.buf[Self::mask(self.read_idx + index)]
    }
}

impl<T: Clone> IndexMut<usize> for Ring<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.buf[Self::mask(self.read_idx + index)]
    }
}

#[derive(Clone)]
pub struct Tokens2<'src> {
    iter: SpannedIter<'src, TokenKind>,
    ring: Ring<Token<'src>>,
    contents: &'src str,
}

impl<'src> Index<usize> for Tokens2<'src> {
    type Output = Token<'src>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.ring[index]
    }
}

impl<'src> Tokens2<'src> {
    pub fn new(lex: Lexer<'src, TokenKind>, contents: &'src str) -> Self {
        let mut iter = lex.spanned();

        let arr = std::array::from_fn(|_| {
            iter.next()
                .map(|(t, r)| Token {
                    kind: t.unwrap(),
                    str: unsafe { contents.get_unchecked(r) },
                })
                .unwrap_or_else(|| Token {
                    kind: TokenKind::EndOfFile,
                    str: unsafe { contents.get_unchecked(contents.len()..) },
                })
        });
        let ring = Ring::new(arr);

        Self {
            iter,
            ring,
            contents,
        }
    }

    pub fn pop_front(&mut self) -> Token<'src> {
        let next = self
            .iter
            .next()
            .map(|(t, r)| Token {
                kind: t.unwrap(),
                str: unsafe { self.contents.get_unchecked(r) },
            })
            .unwrap_or_else(|| Token {
                kind: TokenKind::EndOfFile,
                str: unsafe { self.contents.get_unchecked(self.contents.len()..) },
            });

        self.ring.rotate(next)
    }

    pub fn pop_name(&mut self) -> Result<&'src str, UnexpectedToken> {
        if self.ring[0].kind != TokenKind::Name {
            return Err(UnexpectedToken {
                token: (&self.ring[0]).into(),
                expected_kinds: vec![TokenKind::Name],
            });
        }

        Ok(self.pop_front().str)
    }

    pub fn expect(&mut self, expected_kind: TokenKind) -> Result<(), UnexpectedToken> {
        if self.ring[0].kind == expected_kind {
            self.pop_front();
            Ok(())
        } else {
            Err(UnexpectedToken {
                token: (&self.ring[0]).into(),
                expected_kinds: vec![expected_kind],
            })
        }
    }
}
