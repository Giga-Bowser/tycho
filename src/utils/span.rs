use std::ops::Range;

pub(crate) type SrcLoc = u32;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: SrcLoc,
    pub end: SrcLoc,
}

impl Span {
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
    pub fn to_str(self, source: &str) -> &str {
        unsafe { source.get_unchecked(self.to_range()) }
    }

    #[inline]
    pub const fn to_range(self) -> Range<usize> {
        (self.start as usize)..(self.end as usize)
    }
}
