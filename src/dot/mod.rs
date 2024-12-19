//! Sam style dot manipulation
//!
//! See http://sam.cat-v.org/ for details on how sam works, particularly
//! http://doc.cat-v.org/plan_9/4th_edition/papers/sam/ which is the original paper
//! where Rob Pike lays out how the editor works.
//!
//! All indexing is 0-based when working with the contents of a specific buffer.
//! Converting to 1-based indices for the terminal is exclusively handled in the
//! rendering logic.
use crate::buffer::Buffer;
use std::cmp::min;

mod cur;
pub(crate) mod find;
mod range;
mod text_object;

pub(crate) use cur::Cur;
pub(crate) use range::Range;
pub(crate) use text_object::TextObject;

/// A Dot represents the currently selected contents of a Buffer.
///
/// Most of the editing commands available in ad which manipulate the buffer contents
/// do so via setting and manipulating the current dot. The name comes from the fact
/// that the representation of the current Dot in the editing language is `.`
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Dot {
    /// A single character [Cur]
    Cur {
        /// The cursor
        c: Cur,
    },
    /// A [Range] between two cursors
    Range {
        /// The range
        r: Range,
    },
}

impl Default for Dot {
    fn default() -> Self {
        Self::Cur { c: Cur::default() }
    }
}

impl From<Cur> for Dot {
    fn from(c: Cur) -> Self {
        Self::Cur { c }
    }
}

impl From<Range> for Dot {
    fn from(r: Range) -> Self {
        Self::Range { r }
    }
}

impl Dot {
    /// Construct a new [Range] dot from two cursor indices.
    ///
    /// `to` will be used as the active cursor position.
    pub fn from_char_indices(from: usize, to: usize) -> Self {
        Self::Range {
            r: Range::from_cursors(Cur { idx: from }, Cur { idx: to }, false),
        }
    }

    /// Convert a [Dot] into character offsets within the buffer idendifying its start and end
    /// positions.
    ///
    /// For a [Cur] dot the start and end are equal
    pub fn as_char_indices(&self) -> (usize, usize) {
        match *self {
            Self::Cur { c: Cur { idx } } => (idx, idx),
            Self::Range {
                r:
                    Range {
                        start: Cur { idx: from },
                        end: Cur { idx: to },
                        ..
                    },
            } => (from, to),
        }
    }

    /// The number of characters contained within this Dot.
    pub fn n_chars(&self) -> usize {
        let (from, to) = self.as_char_indices();

        to - from + 1
    }

    /// Whether or not this dot is a [Cur].
    pub fn is_cur(&self) -> bool {
        matches!(self, Dot::Cur { .. })
    }

    /// Whether or not this dot is a [Range].
    pub fn is_range(&self) -> bool {
        matches!(self, Dot::Range { .. })
    }

    /// Whether or not this dot contains `cur` within it.
    pub fn contains(&self, cur: &Cur) -> bool {
        match self {
            Dot::Cur { c } => cur == c,
            Dot::Range { r } => r.contains(cur),
        }
    }

    /// Whether or not this dot contains all of `rng` within it.
    pub fn contains_range(&self, rng: &Range) -> bool {
        self.contains(&rng.start) && self.contains(&rng.end)
    }

    /// The address representation of this dot in the form that is enterable by the user.
    /// Indices are 1-based rather than their internal 0-based representation.
    pub fn addr(&self, b: &Buffer) -> String {
        match self {
            Self::Cur { c } => c.as_string_addr(b),
            Self::Range { r } => r.as_string_addr(b),
        }
    }

    /// Use this [Dot] to index in to a [Buffer] and extract the range of text it denotes.
    pub fn content(&self, b: &Buffer) -> String {
        let len_chars = b.txt.len_chars();

        if len_chars == 0 {
            return String::new();
        }

        let (from, to) = self.as_char_indices();
        b.txt.slice(from, min(to + 1, len_chars)).to_string()
    }

    /// The active cursor position for this [Dot] which will be manipulated by movement operations
    #[inline]
    pub fn active_cur(&self) -> Cur {
        match self {
            Self::Cur { c } => *c,
            Self::Range { r } => r.active_cursor(),
        }
    }

    /// Set the active cursor position for this [Dot] directly, replacing the current active
    /// cursor.
    pub fn set_active_cur(&mut self, cur: Cur) {
        match self {
            Self::Cur { c } => *c = cur,
            Self::Range { r } => r.set_active_cursor(cur),
        }
    }

    /// The cursor position closest to the start of the Buffer.
    #[inline]
    pub fn first_cur(&self) -> Cur {
        match self {
            Self::Cur { c } => *c,
            Self::Range { r } => r.start,
        }
    }

    /// The cursor position closest to the end of the Buffer.
    #[inline]
    pub fn last_cur(&self) -> Cur {
        match self {
            Self::Cur { c } => *c,
            Self::Range { r } => r.end,
        }
    }

    /// This [Dot] expressed as a [Range].
    ///
    /// For range dots the underlying range is returned directly. For cursor dots a new range is
    /// constructed where `start` and `end` are equal and the `end` is the active cursor.
    #[inline]
    pub fn as_range(&self) -> Range {
        match self {
            Self::Cur { c } => Range {
                start: *c,
                end: *c,
                start_active: false,
            },
            Self::Range { r } => *r,
        }
    }

    /// The [Dot] equivalent of [Dot::first_cur].
    #[inline]
    pub fn collapse_to_first_cur(&self) -> Self {
        Dot::Cur {
            c: self.first_cur(),
        }
    }

    /// The [Dot] equivalent of [Dot::last_cur].
    #[inline]
    pub fn collapse_to_last_cur(&self) -> Self {
        Dot::Cur { c: self.last_cur() }
    }

    /// Swap the active cursor between `start` and `end` of [Range] dots.
    #[inline]
    pub fn flip(&mut self) {
        if let Dot::Range { r } = self {
            r.flip();
        }
    }

    /// If both ends of a Range match then replace with a single Cur
    pub(crate) fn collapse_null_range(self) -> Self {
        match self {
            Dot::Range {
                r: Range { start, end, .. },
            } if start == end => Dot::Cur { c: start },
            _ => self,
        }
    }

    /// Clamp this dot to be valid for the given Buffer
    pub(crate) fn clamp_idx(&mut self, max_idx: usize) {
        match self {
            Dot::Cur { c } => c.clamp_idx(max_idx),
            Dot::Range { r } => {
                r.start.clamp_idx(max_idx);
                r.end.clamp_idx(max_idx);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{
        text_object::TextObject::{self, *},
        *,
    };
    use simple_test_case::test_case;

    const EXAMPLE_TEXT: &str = "\
This is the first line of the file. Followed
by the second line. Some of the sentences are split
over multiple lines.
Others are not.

There is a second paragraph as well. But it
is quite short when compared to the first.


The third paragraph is even shorter.";

    fn cur(y: usize, x: usize) -> Cur {
        let y = if y == 0 {
            0
        } else {
            EXAMPLE_TEXT
                .lines()
                .take(y)
                .map(|line| line.len() + 1)
                .sum()
        };

        Cur { idx: y + x }
    }

    fn c(y: usize, x: usize) -> Dot {
        Dot::Cur { c: cur(y, x) }
    }

    fn r(y: usize, x: usize, y2: usize, x2: usize) -> Dot {
        Dot::Range {
            r: Range {
                start: cur(y, x),
                end: cur(y2, x2),
                start_active: false,
            },
        }
    }

    #[test_case(BufferStart, c(0, 0); "buffer start")]
    #[test_case(BufferEnd, c(9, 36); "buffer end")]
    #[test_case(Character, c(5, 2); "character")]
    #[test_case(Line, r(5, 0, 5, 43); "line")]
    #[test_case(LineEnd, c(5, 43); "line end")]
    #[test_case(LineStart, c(5, 0); "line start")]
    #[test]
    fn set_dot_works(to: TextObject, expected: Dot) {
        let mut b = Buffer::new_virtual(0, "test".to_string(), EXAMPLE_TEXT.to_string());
        b.dot = c(5, 1); // Start of paragraph 2
        to.set_dot(&mut b);

        assert_eq!(b.dot, expected);
    }

    #[test_case(c(0, 0), "T"; "first character")]
    #[test_case(r(0, 0, 0, 34), "This is the first line of the file."; "first sentence")]
    #[test_case(
        r(0, 0, 1, 18),
        "This is the first line of the file. Followed\nby the second line.";
        "spanning a newline"
    )]
    #[test]
    fn dot_content_includes_expected_text(dot: Dot, expected: &str) {
        let mut b = Buffer::new_virtual(0, "test".to_string(), EXAMPLE_TEXT.to_string());
        b.dot = dot;
        let content = b.dot_contents();

        assert_eq!(content, expected);
    }
}
