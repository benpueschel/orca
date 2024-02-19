#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<(usize, usize)> for Span {
    fn from(span: (usize, usize)) -> Span {
        Span {
            start: span.0,
            end: span.1,
        }
    }
}

impl From<Span> for (usize, usize) {
    fn from(span: Span) -> (usize, usize) {
        (span.start, span.end)
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Span {
        Span { start, end }
    }
    pub fn single(pos: usize) -> Span {
        Span::with_len(pos, 1)
    }
    pub fn with_len(start: usize, len: usize) -> Span {
        Span {
            start,
            end: start + len,
        }
    }
    pub fn compose(start: Span, end: Span) -> Span {
        Span {
            start: start.start,
            end: end.end,
        }
    }
    pub fn empty() -> Span {
        Span { start: 0, end: 0 }
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }
}
