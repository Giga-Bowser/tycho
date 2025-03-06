use std::{marker::PhantomData, ops::Range};

pub(crate) type SrcLoc = u32;

#[derive(Debug, Clone, Copy)]
pub struct Span<'s> {
    pub start: SrcLoc,
    pub end: SrcLoc,
    _m: PhantomData<&'s str>,
}

impl<'s> Span<'s> {
    #[inline]
    pub const fn new(start: SrcLoc, end: SrcLoc) -> Self {
        Self {
            start,
            end,
            _m: PhantomData,
        }
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

    #[inline]
    pub fn to_str(self, source: &'s str) -> &'s str {
        unsafe { source.get_unchecked(self.to_range()) }
    }

    #[inline]
    pub const fn to_range(self) -> Range<usize> {
        (self.start as usize)..(self.end as usize)
    }
}
