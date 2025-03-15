use std::{collections::VecDeque, mem::ManuallyDrop, ops::Index};

use crate::{
    parser::error::{ParseError, UnexpectedToken},
    sourcemap::SourceFile,
    utils::{Span, SrcLoc},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TokenKind {
    StrLit,
    Name,
    Arrow,
    DotDot,
    Break,
    Return,
    True,
    False,
    Struct,
    If,
    Else,
    For,
    While,
    In,
    And,
    Or,
    Not,
    Equality,
    Inequality,
    GreterEqual,
    LessEqual,
    Func,
    Constructor,
    Elipsis,
    Nil,
    Octothorpe,
    Percent,
    BitAnd,
    LParen,
    RParen,
    Asterisk,
    Plus,
    Comma,
    Minus,
    Dot,
    Slash,
    NumLit,
    Colon,
    Less,
    Equal,
    Greater,
    Question,
    LSquare,
    RSquare,
    Caret,
    LCurly,
    BitOr,
    RCurly,
    EndOfFile,
}

#[macro_export]
macro_rules! token_pat {
    (LITERAL) => {
        TokenKind::NumLit | TokenKind::StrLit | TokenKind::True | TokenKind::False | TokenKind::Nil
    };
    (KEYWORD) => {
        TokenKind::Break
            | TokenKind::Return
            | TokenKind::Struct
            | TokenKind::If
            | TokenKind::Else
            | TokenKind::For
            | TokenKind::While
            | TokenKind::In
            | TokenKind::Func
            | TokenKind::Constructor
    };
    (PUNCT) => {
        TokenKind::Arrow
            | TokenKind::DotDot
            | TokenKind::And
            | TokenKind::Or
            | TokenKind::Not
            | TokenKind::Equality
            | TokenKind::Inequality
            | TokenKind::GreterEqual
            | TokenKind::LessEqual
            | TokenKind::Elipsis
            | TokenKind::Octothorpe
            | TokenKind::Percent
            | TokenKind::BitAnd
            | TokenKind::LParen
            | TokenKind::RParen
            | TokenKind::Asterisk
            | TokenKind::Plus
            | TokenKind::Comma
            | TokenKind::Minus
            | TokenKind::Dot
            | TokenKind::Slash
            | TokenKind::Colon
            | TokenKind::Less
            | TokenKind::Equal
            | TokenKind::Greater
            | TokenKind::Question
            | TokenKind::LSquare
            | TokenKind::RSquare
            | TokenKind::Caret
            | TokenKind::LCurly
            | TokenKind::BitOr
            | TokenKind::RCurly
    };
}

impl TokenKind {
    pub const fn is_literal(self) -> bool {
        matches!(self, token_pat!(LITERAL))
    }

    pub const fn is_keyword(self) -> bool {
        matches!(self, token_pat!(KEYWORD))
    }

    pub const fn is_punct(self) -> bool {
        matches!(self, token_pat!(PUNCT))
    }

    pub const fn text(self) -> Option<&'static str> {
        let s = match self {
            TokenKind::Break => "break",
            TokenKind::Return => "return",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Struct => "struct",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::For => "for",
            TokenKind::While => "while",
            TokenKind::In => "in",
            TokenKind::Func => "func",
            TokenKind::Constructor => "constructor",
            TokenKind::Nil => "nil",

            TokenKind::Arrow => "->",
            TokenKind::DotDot => "..",
            TokenKind::And => "&&",
            TokenKind::Or => "||",
            TokenKind::Not => "!",
            TokenKind::Equality => "==",
            TokenKind::Inequality => "!=",
            TokenKind::GreterEqual => ">=",
            TokenKind::LessEqual => "<=",
            TokenKind::Elipsis => "...",
            TokenKind::Octothorpe => "#",
            TokenKind::Percent => "%",
            TokenKind::BitAnd => "&",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::Asterisk => "*",
            TokenKind::Plus => "+",
            TokenKind::Comma => ",",
            TokenKind::Minus => "-",
            TokenKind::Dot => ".",
            TokenKind::Slash => "/",
            TokenKind::Colon => ":",
            TokenKind::Less => "<",
            TokenKind::Equal => "=",
            TokenKind::Greater => ">",
            TokenKind::Question => "?",
            TokenKind::LSquare => "[",
            TokenKind::RSquare => "]",
            TokenKind::Caret => "^",
            TokenKind::LCurly => "{",
            TokenKind::BitOr => "|",
            TokenKind::RCurly => "}",

            TokenKind::StrLit | TokenKind::Name | TokenKind::NumLit | TokenKind::EndOfFile => {
                return None
            }
        };

        Some(s)
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    file: &'a SourceFile,

    token: ManuallyDrop<Option<TokenKind>>,
    token_start: usize,
    token_end: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(file: &'a SourceFile) -> Self {
        Lexer {
            file,
            token: ManuallyDrop::default(),
            token_start: 0,
            token_end: 0,
        }
    }

    pub fn lex_all(&mut self) -> Tokens {
        let mut dq = VecDeque::new();

        loop {
            self.token_start = self.token_end;

            self.lex();

            // This basically treats self.token as a temporary field.
            // Since we always immediately return a newly set token here,
            // we don't have to replace it with `None` or manually drop
            // it later.

            let kind = unsafe { ManuallyDrop::take(&mut self.token) };
            let token = Token::new(kind.unwrap_or(TokenKind::EndOfFile), self.span());
            dq.push_back(token);
            if kind.is_none() {
                break;
            }
        }

        Tokens { dq }
    }

    #[inline]
    fn source_read<C: Chunk>(&self, offset: usize) -> Option<C> {
        if offset + (C::SIZE - 1) < self.file.src.len() {
            // # Safety: we just performed a bounds check.
            Some(unsafe { Chunk::from_ptr(self.file.src.as_ptr().add(offset)) })
        } else {
            None
        }
    }

    #[inline]
    fn source_find_boundary(&self, mut index: usize) -> usize {
        while !self.file.src.is_char_boundary(index) {
            index += 1;
        }

        index
    }

    #[inline]
    fn read<C: Chunk>(&self) -> Option<C> {
        self.source_read(self.token_end)
    }

    /// Read a `Chunk` at a position offset by `n`.
    #[inline]
    fn read_at<C: Chunk>(&self, n: usize) -> Option<C> {
        self.source_read(self.token_end + n)
    }

    /// Test a chunk at current position with a closure.
    #[inline]
    fn test<T: Chunk, F: FnOnce(T) -> bool>(&self, test: F) -> bool {
        match self.source_read::<T>(self.token_end) {
            Some(chunk) => test(chunk),
            None => false,
        }
    }

    /// Bump the position `Lexer` is reading from by `size`.
    #[inline]
    fn bump_unchecked(&mut self, size: usize) {
        debug_assert!(
            self.token_end + size <= self.file.src.len(),
            "Bumping out of bounds!"
        );

        self.token_end += size;
    }

    /// Reset `token_start` to `token_end`.
    #[inline]
    fn trivia(&mut self) {
        self.token_start = self.token_end;
    }

    /// Guarantee that `token_end` is at char boundary for `&str`.
    #[inline]
    fn error(&mut self) {
        self.token_end = self.source_find_boundary(self.token_end);
        let text = unsafe {
            self.file
                .src
                .get_unchecked(self.token_start..self.token_end)
        };
        panic!("{}: bad token `{text}`", self.file.path.display());
    }

    #[inline]
    fn end(&mut self) {
        self.token = ManuallyDrop::default();
    }

    #[inline]
    fn set(&mut self, token: TokenKind) {
        self.token = ManuallyDrop::new(Some(token));
    }

    #[inline]
    fn span(&self) -> Span {
        Span::new(
            self.file.start_pos + self.token_start as SrcLoc,
            self.file.start_pos + self.token_end as SrcLoc,
        )
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone)]
pub struct Tokens {
    pub dq: VecDeque<Token>,
}

impl Index<usize> for Tokens {
    type Output = Token;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        &self.dq[index]
    }
}

impl Tokens {
    #[inline]
    pub fn pop_front(&mut self) -> Token {
        self.dq.pop_front().unwrap()
    }

    pub fn pop_name(&mut self) -> Result<Span, Box<ParseError>> {
        if self.dq[0].kind == TokenKind::Name {
            Ok(self.pop_front().span)
        } else {
            Err(UnexpectedToken::err(self.dq[0].clone(), [TokenKind::Name]))
        }
    }

    pub fn expect(&mut self, expected_kind: TokenKind) -> Result<Token, Box<ParseError>> {
        if self.dq[0].kind == expected_kind {
            Ok(self.pop_front())
        } else {
            Err(UnexpectedToken::err(self.dq[0].clone(), [expected_kind]))
        }
    }
}

pub trait Chunk: Sized + Copy + PartialEq + Eq {
    const SIZE: usize;

    /// # Safety
    ///
    /// Raw byte pointer should point to a valid location in source.
    unsafe fn from_ptr(ptr: *const u8) -> Self;
}

impl Chunk for u8 {
    const SIZE: usize = 1;

    #[inline]
    unsafe fn from_ptr(ptr: *const u8) -> Self {
        unsafe { *ptr }
    }
}

impl<const N: usize> Chunk for &[u8; N] {
    const SIZE: usize = N;

    #[inline]
    unsafe fn from_ptr(ptr: *const u8) -> Self {
        unsafe { &*ptr.cast() }
    }
}

macro_rules! crunch16 {
    ($lex:ident, $test:path, $miss:expr) => {
        while let Some(arr) = $lex.read::<&[u8; 16]>() {
            if $test(arr[0]) {
                if $test(arr[1]) {
                    if $test(arr[2]) {
                        if $test(arr[3]) {
                            if $test(arr[4]) {
                                if $test(arr[5]) {
                                    if $test(arr[6]) {
                                        if $test(arr[7]) {
                                            if $test(arr[8]) {
                                                if $test(arr[9]) {
                                                    if $test(arr[10]) {
                                                        if $test(arr[11]) {
                                                            if $test(arr[12]) {
                                                                if $test(arr[13]) {
                                                                    if $test(arr[14]) {
                                                                        if $test(arr[15]) {
                                                                            $lex.bump_unchecked(16);
                                                                            continue;
                                                                        }
                                                                        $lex.bump_unchecked(15);
                                                                        return $miss;
                                                                    }
                                                                    $lex.bump_unchecked(14);
                                                                    return $miss;
                                                                }
                                                                $lex.bump_unchecked(13);
                                                                return $miss;
                                                            }
                                                            $lex.bump_unchecked(12);
                                                            return $miss;
                                                        }
                                                        $lex.bump_unchecked(11);
                                                        return $miss;
                                                    }
                                                    $lex.bump_unchecked(10);
                                                    return $miss;
                                                }
                                                $lex.bump_unchecked(9);
                                                return $miss;
                                            }
                                            $lex.bump_unchecked(8);
                                            return $miss;
                                        }
                                        $lex.bump_unchecked(7);
                                        return $miss;
                                    }
                                    $lex.bump_unchecked(6);
                                    return $miss;
                                }
                                $lex.bump_unchecked(5);
                                return $miss;
                            }
                            $lex.bump_unchecked(4);
                            return $miss;
                        }
                        $lex.bump_unchecked(3);
                        return $miss;
                    }
                    $lex.bump_unchecked(2);
                    return $miss;
                }
                $lex.bump_unchecked(1);
                return $miss;
            }
            return $miss;
        }
        while $lex.test($test) {
            $lex.bump_unchecked(1);
        }
        $miss
    };
}

macro_rules! finish_kw {
    ($lex:ident, $kind:path) => {
        let Some(byte) = $lex.read::<u8>() else {
            return $lex.set($kind);
        };
        match byte {
            byte if is_name_body(byte) => {
                $lex.bump_unchecked(1);
                $lex.crunch_name();
            }
            _ => $lex.set($kind),
        };
    };
}

impl Lexer<'_> {
    fn lex(&mut self) {
        enum Jump {
            __,
            Name,
            Zero,
            NumLit,
            Quote,
            Trivia,

            Asterisk,
            Caret,
            Colon,
            Comma,
            Octothorpe,
            Percent,
            Plus,
            Question,
            LParen,
            RParen,
            LSquare,
            RSquare,
            LCurly,
            RCurly,

            B,
            C,
            E,
            F,
            I,
            N,
            R,
            S,
            T,
            W,

            Amp,
            Pipe,
            Less,
            Greater,
            Not,
            Equal,
            Minus,
            Slash,
            Dot,
        }
        const LUT: [Jump; 256] = {
            use Jump::*;
            [
                __, __, __, __, __, __, __, __, __, Trivia, Trivia, __, Trivia, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, Trivia, Not, Quote,
                Octothorpe, __, Percent, Amp, __, LParen, RParen, Asterisk, Plus, Comma, Minus,
                Dot, Slash, Zero, NumLit, NumLit, NumLit, NumLit, NumLit, NumLit, NumLit, NumLit,
                NumLit, Colon, __, Less, Equal, Greater, Question, __, Name, Name, Name, Name,
                Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name,
                Name, Name, Name, Name, Name, Name, Name, Name, LSquare, __, RSquare, Caret, Name,
                __, Name, B, C, Name, E, F, Name, Name, I, Name, Name, Name, Name, N, Name, Name,
                Name, R, S, T, Name, Name, W, Name, Name, Name, LCurly, Pipe, RCurly, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __,
            ]
        };
        let Some(byte) = self.read::<u8>() else {
            return self.end();
        };
        self.bump_unchecked(1);
        match LUT[byte as usize] {
            Jump::Name => self.crunch_name(),
            Jump::Zero => self.numlit_zero(),
            Jump::NumLit => self.numlit_decimal(),
            Jump::Quote => self.crunch_strlit(),
            Jump::Trivia => self.crunch_trivia(),

            // unambiguous tokens
            Jump::Asterisk => self.set(TokenKind::Asterisk),
            Jump::Caret => self.set(TokenKind::Caret),
            Jump::Colon => self.set(TokenKind::Colon),
            Jump::Comma => self.set(TokenKind::Comma),
            Jump::Octothorpe => self.set(TokenKind::Octothorpe),
            Jump::Percent => self.set(TokenKind::Percent),
            Jump::Plus => self.set(TokenKind::Plus),
            Jump::Question => self.set(TokenKind::Question),
            Jump::LParen => self.set(TokenKind::LParen),
            Jump::RParen => self.set(TokenKind::RParen),
            Jump::LSquare => self.set(TokenKind::LSquare),
            Jump::RSquare => self.set(TokenKind::RSquare),
            Jump::LCurly => self.set(TokenKind::LCurly),
            Jump::RCurly => self.set(TokenKind::RCurly),

            // possible keywords
            Jump::B => match self.read::<&[u8; 4]>() {
                Some(b"reak") => {
                    self.bump_unchecked(4);
                    finish_kw!(self, TokenKind::Break);
                }
                _ => self.crunch_name(),
            },
            Jump::C => match self.read::<&[u8; 10]>() {
                Some(b"onstructor") => {
                    self.bump_unchecked(10);
                    finish_kw!(self, TokenKind::Constructor);
                }
                _ => self.crunch_name(),
            },
            Jump::E => match self.read::<&[u8; 3]>() {
                Some(b"lse") => {
                    self.bump_unchecked(3);
                    finish_kw!(self, TokenKind::Else);
                }
                _ => self.crunch_name(),
            },
            Jump::F => self.handle_f(),
            Jump::I => self.handle_i(),
            Jump::N => match self.read::<&[u8; 2]>() {
                Some(b"il") => {
                    self.bump_unchecked(2);
                    finish_kw!(self, TokenKind::Nil);
                }
                _ => self.crunch_name(),
            },
            Jump::R => match self.read::<&[u8; 5]>() {
                Some(b"eturn") => {
                    self.bump_unchecked(5);
                    finish_kw!(self, TokenKind::Return);
                }
                _ => self.crunch_name(),
            },
            Jump::S => match self.read::<&[u8; 5]>() {
                Some(b"truct") => {
                    self.bump_unchecked(5);
                    finish_kw!(self, TokenKind::Struct);
                }
                _ => self.crunch_name(),
            },
            Jump::T => match self.read::<&[u8; 3]>() {
                Some(b"rue") => {
                    self.bump_unchecked(3);
                    finish_kw!(self, TokenKind::True);
                }
                _ => self.crunch_name(),
            },
            Jump::W => match self.read::<&[u8; 4]>() {
                Some(b"hile") => {
                    self.bump_unchecked(4);
                    finish_kw!(self, TokenKind::While);
                }
                _ => self.crunch_name(),
            },

            Jump::Amp => match self.read::<u8>() {
                Some(b'&') => {
                    self.bump_unchecked(1);
                    self.set(TokenKind::And);
                }
                _ => self.set(TokenKind::BitAnd),
            },
            Jump::Pipe => match self.read::<u8>() {
                Some(b'|') => {
                    self.bump_unchecked(1);
                    self.set(TokenKind::Or);
                }
                _ => self.set(TokenKind::BitOr),
            },
            Jump::Less => match self.read::<u8>() {
                Some(b'=') => {
                    self.bump_unchecked(1);
                    self.set(TokenKind::LessEqual);
                }
                _ => self.set(TokenKind::Less),
            },
            Jump::Greater => match self.read::<u8>() {
                Some(b'=') => {
                    self.bump_unchecked(1);
                    self.set(TokenKind::GreterEqual);
                }
                _ => self.set(TokenKind::Greater),
            },
            Jump::Not => match self.read::<u8>() {
                Some(b'=') => {
                    self.bump_unchecked(1);
                    self.set(TokenKind::Inequality);
                }
                _ => self.set(TokenKind::Not),
            },
            Jump::Equal => match self.read::<u8>() {
                Some(b'=') => {
                    self.bump_unchecked(1);
                    self.set(TokenKind::Equality);
                }
                _ => self.set(TokenKind::Equal),
            },
            Jump::Minus => match self.read::<u8>() {
                Some(b'>') => {
                    self.bump_unchecked(1);
                    self.set(TokenKind::Arrow);
                }
                _ => self.set(TokenKind::Minus),
            },
            Jump::Slash => match self.read::<u8>() {
                Some(b'/') => {
                    self.bump_unchecked(1);
                    self.crunch_comment();
                }
                _ => self.set(TokenKind::Slash),
            },
            Jump::Dot => match self.read::<u8>() {
                Some(b'.') => {
                    self.bump_unchecked(1);
                    self.handle_dotdot();
                }
                _ => self.set(TokenKind::Dot),
            },
            Jump::__ => {
                self.error();
            }
        }
    }

    #[inline]
    fn crunch_name(&mut self) {
        crunch16!(self, is_name_body, self.set(TokenKind::Name));
    }

    fn numlit_zero(&mut self) {
        let Some(byte) = self.read::<u8>() else {
            return self.set(TokenKind::NumLit);
        };

        if byte | 0x20 == b'x' {
            self.bump_unchecked(1);
            self.numlit_hex();
        } else {
            self.numlit_decimal();
        }
    }

    // [0-9]*
    #[inline]
    fn numlit_decimal(&mut self) {
        crunch16!(self, is_digit, self.numlit_second());
    }

    // ([.][0-9]+)?([eE][+-]?[0-9]+)?
    #[inline]
    fn numlit_second(&mut self) {
        if let Some([b'.', b'0'..=b'9']) = self.read::<&[u8; 2]>() {
            self.bump_unchecked(2);
            self.numlit_crunch_frac();
        } else {
            self.numlit_postfrac();
        }
    }

    // [0-9]*([eE][+-]?[0-9]+)?
    #[inline]
    fn numlit_crunch_frac(&mut self) {
        crunch16!(self, is_digit, self.numlit_postfrac());
    }

    // ([eE][+-]?[0-9]+)?
    #[inline]
    fn numlit_postfrac(&mut self) {
        if let Some(b'E' | b'e') = self.read::<u8>() {
            self.numlit_exponent();
        } else {
            self.set(TokenKind::NumLit);
        };
    }

    // [+-]?[0-9]+
    #[inline]
    fn numlit_exponent(&mut self) {
        if let Some(b'+' | b'-') = self.read_at::<u8>(1) {
            self.numlit_exponent_signed();
        } else {
            self.numlit_exponent_unsigned();
        };
    }

    // [0-9]+
    #[inline]
    fn numlit_exponent_signed(&mut self) {
        if let Some(b'0'..=b'9') = self.read_at::<u8>(2) {
            self.bump_unchecked(3);
            self.numlit_crunch_finish();
        } else {
            self.set(TokenKind::NumLit);
        }
    }

    // [0-9]+
    #[inline]
    fn numlit_exponent_unsigned(&mut self) {
        if let Some(b'0'..=b'9') = self.read_at::<u8>(1) {
            self.bump_unchecked(2);
            self.numlit_crunch_finish();
        } else {
            self.set(TokenKind::NumLit);
        }
    }

    #[inline]
    fn numlit_crunch_finish(&mut self) {
        crunch16!(self, is_digit, self.set(TokenKind::NumLit));
    }

    #[inline]
    fn numlit_hex(&mut self) {
        crunch16!(self, is_hexdigit, self.numlit_second_hex());
    }

    // ([.][0-9a-fA-F]+)?([pP][+-]?[0-9]+)?
    #[inline]
    fn numlit_second_hex(&mut self) {
        if let Some([b'.', b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z']) = self.read::<&[u8; 2]>() {
            self.bump_unchecked(2);
            self.numlit_crunch_frac_hex();
        } else {
            self.numlit_postfrac_hex();
        }
    }

    // [0-9a-fA-F]*([pP][+-]?[0-9]+)?
    #[inline]
    fn numlit_crunch_frac_hex(&mut self) {
        crunch16!(self, is_hexdigit, self.numlit_postfrac_hex());
    }

    // ([pP][+-]?[0-9]+)?
    #[inline]
    fn numlit_postfrac_hex(&mut self) {
        if let Some(b'P' | b'p') = self.read::<u8>() {
            self.numlit_exponent();
        } else {
            self.set(TokenKind::NumLit);
        };
    }

    #[inline]
    fn set_trivia(&mut self) {
        self.trivia();
        self.lex();
    }

    #[inline]
    fn crunch_trivia(&mut self) {
        crunch16!(self, is_trivia, self.set_trivia());
    }

    #[inline]
    fn handle_fo(&mut self) {
        if let Some(b'r') = self.read::<u8>() {
            self.bump_unchecked(1);
            finish_kw!(self, TokenKind::For);
        } else {
            self.crunch_name();
        }
    }

    #[inline]
    fn handle_fals(&mut self) {
        let Some(byte) = self.read::<u8>() else {
            return self.set(TokenKind::Name);
        };
        match byte {
            b'e' => {
                self.bump_unchecked(1);
                finish_kw!(self, TokenKind::False);
            }
            byte if is_name_not_e(byte) => {
                self.bump_unchecked(1);
                self.crunch_name();
            }
            _ => self.set(TokenKind::Name),
        }
    }

    #[inline]
    fn handle_fal(&mut self) {
        let Some(byte) = self.read::<u8>() else {
            return self.set(TokenKind::Name);
        };
        match byte {
            b's' => {
                self.bump_unchecked(1);
                self.handle_fals();
            }
            byte if is_name_not_s(byte) => {
                self.bump_unchecked(1);
                self.crunch_name();
            }
            _ => self.set(TokenKind::Name),
        }
    }

    #[inline]
    fn handle_fa(&mut self) {
        let Some(byte) = self.read::<u8>() else {
            return self.set(TokenKind::Name);
        };
        match byte {
            byte if is_name_not_l(byte) => {
                self.bump_unchecked(1);
                self.crunch_name();
            }
            b'l' => {
                self.bump_unchecked(1);
                self.handle_fal();
            }
            _ => self.set(TokenKind::Name),
        }
    }

    #[inline]
    fn handle_fu(&mut self) {
        if let Some(b"nc") = self.read::<&[u8; 2]>() {
            self.bump_unchecked(2);
            finish_kw!(self, TokenKind::Func);
        } else {
            self.crunch_name();
        }
    }

    #[inline]
    fn handle_f(&mut self) {
        enum Jump {
            __,
            Name,
            O,
            A,
            U,
        }
        const LUT: [Jump; 256] = {
            use Jump::*;
            [
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name,
                __, __, __, __, __, __, __, Name, Name, Name, Name, Name, Name, Name, Name, Name,
                Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name,
                Name, Name, Name, __, __, __, __, Name, __, A, Name, Name, Name, Name, Name, Name,
                Name, Name, Name, Name, Name, Name, Name, O, Name, Name, Name, Name, Name, U, Name,
                Name, Name, Name, Name, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __,
            ]
        };
        let Some(byte) = self.read::<u8>() else {
            return self.set(TokenKind::Name);
        };
        match LUT[byte as usize] {
            Jump::Name => {
                self.bump_unchecked(1);
                self.crunch_name();
            }
            Jump::O => {
                self.bump_unchecked(1);
                self.handle_fo();
            }
            Jump::A => {
                self.bump_unchecked(1);
                self.handle_fa();
            }
            Jump::U => {
                self.bump_unchecked(1);
                self.handle_fu();
            }
            Jump::__ => self.set(TokenKind::Name),
        }
    }

    #[inline]
    fn set_comment(&mut self) {
        self.trivia();
        self.lex();
    }
    #[inline]
    fn crunch_comment(&mut self) {
        crunch16!(self, not_newline, self.set_comment());
    }

    #[inline]
    fn handle_i(&mut self) {
        enum Jump {
            __,
            N,
            Name,
            F,
        }
        const LUT: [Jump; 256] = {
            use Jump::*;
            [
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name,
                __, __, __, __, __, __, __, Name, Name, Name, Name, Name, Name, Name, Name, Name,
                Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name, Name,
                Name, Name, Name, __, __, __, __, Name, __, Name, Name, Name, Name, Name, F, Name,
                Name, Name, Name, Name, Name, Name, N, Name, Name, Name, Name, Name, Name, Name,
                Name, Name, Name, Name, Name, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
                __, __, __, __, __, __, __, __, __, __, __, __, __, __, __,
            ]
        };
        let Some(byte) = self.read::<u8>() else {
            return self.set(TokenKind::Name);
        };
        match LUT[byte as usize] {
            Jump::N => {
                self.bump_unchecked(1);
                finish_kw!(self, TokenKind::In);
            }
            Jump::Name => {
                self.bump_unchecked(1);
                self.crunch_name();
            }
            Jump::F => {
                self.bump_unchecked(1);
                finish_kw!(self, TokenKind::If);
            }
            Jump::__ => self.set(TokenKind::Name),
        }
    }

    #[inline]
    fn handle_dotdot(&mut self) {
        match self.read::<u8>() {
            Some(b'.') => {
                self.bump_unchecked(1);
                self.set(TokenKind::Elipsis);
            }
            _ => self.set(TokenKind::DotDot),
        }
    }

    #[inline]
    fn handle_escape(&mut self) {
        match self.read_at::<u8>(1) {
            Some(_) => {
                self.bump_unchecked(2);
                self.crunch_strlit();
            }
            None => self.error(),
        }
    }

    #[inline]
    fn handle_endquote_or_escape(&mut self) {
        match self.read::<u8>() {
            Some(b'\\') => self.handle_escape(),
            Some(_) => {
                self.bump_unchecked(1);
                self.set(TokenKind::StrLit);
            }
            None => self.error(),
        }
    }

    #[inline]
    fn crunch_strlit(&mut self) {
        crunch16!(self, not_quote_or_escape, self.handle_endquote_or_escape());
    }
}

const GENERAL_LUT: [u8; 256] = [
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, //
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, //
    16, 16, 00, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, //
    31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 16, 16, 16, 16, 16, 16, //
    16, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, //
    31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 16, 00, 16, 16, 31, //
    16, 31, 31, 31, 31, 27, 31, 31, 31, 31, 31, 31, 29, 31, 31, 31, //
    31, 31, 31, 23, 31, 31, 31, 31, 31, 31, 31, 16, 16, 16, 16, 16, //
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, //
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, //
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, //
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, //
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, //
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, //
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, //
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, //
];

#[inline]
fn is_name_body(byte: u8) -> bool {
    GENERAL_LUT[byte as usize] & 1 > 0
}

#[inline]
fn is_name_not_l(byte: u8) -> bool {
    GENERAL_LUT[byte as usize] & 2 > 0
}

#[inline]
fn is_name_not_e(byte: u8) -> bool {
    GENERAL_LUT[byte as usize] & 4 > 0
}

#[inline]
fn is_name_not_s(byte: u8) -> bool {
    GENERAL_LUT[byte as usize] & 8 > 0
}

#[inline]
fn not_quote_or_escape(byte: u8) -> bool {
    GENERAL_LUT[byte as usize] & 16 > 0
}

#[inline]
fn is_digit(byte: u8) -> bool {
    byte.is_ascii_digit()
}

#[inline]
fn is_hexdigit(byte: u8) -> bool {
    byte.is_ascii_hexdigit()
}

#[inline]
fn is_trivia(byte: u8) -> bool {
    // const LUT: u64 = 4294972928u64;
    const LUT: u64 = (1 << b'\t') | (1 << b'\n') | (1 << b' ');
    match 1u64.checked_shl(byte as u32) {
        Some(shift) => LUT & shift != 0,
        None => false,
    }
}

#[inline]
fn not_newline(byte: u8) -> bool {
    byte != b'\n'
}
