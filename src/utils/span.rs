use std::{
    cell::RefCell,
    fmt,
    hash::{BuildHasherDefault, Hash},
    ops::Range,
};

use crate::{sourcemap::SourceFile, utils::FxIndexSet};

pub(crate) type SrcLoc = u32;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: SrcLoc,
    pub end: SrcLoc,
}

impl Span {
    pub const DUMMY: Self = Self::new(0, 0);

    #[inline]
    pub const fn new(start: SrcLoc, end: SrcLoc) -> Self {
        Self { start, end }
    }

    #[inline]
    pub const fn empty(offset: SrcLoc) -> Self {
        Self::new(offset, offset)
    }

    #[inline]
    pub const fn offset_len(offset: SrcLoc, len: SrcLoc) -> Self {
        Self::new(offset, offset + len)
    }

    #[inline]
    #[must_use]
    pub fn cover(self, rhs: Self) -> Self {
        let start = SrcLoc::min(self.start, rhs.start);
        let end = SrcLoc::max(self.end, rhs.end);
        Self::new(start, end)
    }

    #[inline]
    #[must_use]
    pub fn cover_loc(self, loc: SrcLoc) -> Self {
        let start = SrcLoc::min(self.start, loc);
        let end = SrcLoc::max(self.end, loc);
        Self::new(start, end)
    }

    pub fn len(self) -> usize {
        (self.end - self.start) as usize
    }

    pub fn is_empty(self) -> bool {
        self.start == self.end
    }

    #[inline]
    pub fn to_str(self, source: &SourceFile) -> &str {
        let start = (self.start - source.start_pos) as usize;
        let end = (self.end - source.start_pos) as usize;
        unsafe { source.src.get_unchecked(start..end) }
    }

    #[inline]
    pub const fn to_range(self) -> Range<usize> {
        (self.start as usize)..(self.end as usize)
    }

    #[inline]
    pub fn symbol(self, source: &SourceFile) -> Symbol {
        Symbol::intern(self.to_str(source))
    }

    #[inline]
    pub fn ident(self, source: &SourceFile) -> Ident {
        Ident::new(self.symbol(source), self)
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::DUMMY
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Span({}..{})", self.start, self.end)
    }
}

#[derive(Debug, Clone, Eq)]
pub struct Ident {
    pub symbol: Symbol,
    pub span: Span,
}

impl Ident {
    pub const fn new(symbol: Symbol, span: Span) -> Self {
        Self { symbol, span }
    }

    pub fn from_str(string: &str) -> Self {
        Self {
            symbol: Symbol::intern(string),
            span: Span::DUMMY,
        }
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.symbol == other.symbol
    }
}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.symbol.idx.hash(state);
    }
}

thread_local! {
    pub static SYMBOL_INTERNER: RefCell<FxIndexSet<String>> = const {
        RefCell::new(FxIndexSet::with_hasher(BuildHasherDefault::new()))
    };
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Symbol {
    pub idx: u32,
}

impl Symbol {
    const fn new(idx: u32) -> Self {
        Self { idx }
    }

    pub fn intern(string: &str) -> Self {
        let idx = SYMBOL_INTERNER.with_borrow_mut(|interner| {
            if let Some(idx) = interner.get_index_of(string) {
                return idx;
            }

            interner.insert_full(string.to_owned()).0
        });

        Self::new(idx as u32)
    }

    pub fn get_string(self) -> Option<String> {
        SYMBOL_INTERNER.with_borrow(|it| it.get_index(self.idx as usize).cloned())
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.get_string() {
            Some(s) => f.write_str(&s),
            None => write!(f, "<unknown: {}>", self.idx),
        }
    }
}
