//! Support for tree-sitter incremental parsing, querying and highlighting of Buffers
//!
//! For a given language the user needs to provide a .so file containing the compiled
//! tree-sitter parser and a highlights .scm file for driving the highlighting.
use crate::{
    buffer::{Buffer, GapBuffer, SliceIter},
    dot::Range,
    term::Style,
};
use libloading::{Library, Symbol};
use std::collections::HashMap;
use std::{
    cmp::{max, min, Ord, Ordering, PartialOrd},
    fmt,
    iter::Peekable,
    ops::{Deref, DerefMut},
    path::Path,
    slice,
};
use streaming_iterator::StreamingIterator;
use tree_sitter::{self as ts, ffi::TSLanguage};
// use unicode_width::UnicodeWidthChar;

const TK_DEFAULT: &str = "default";
const TK_DOT: &str = "dot";
const TK_LOAD: &str = "load";
const TK_EXEC: &str = "exec";

// TODO: move to config when it gets rewritten
type ColorScheme = HashMap<String, Vec<Style>>;

// Required for us to be able to pass GapBuffers to the tree-sitter API
impl<'a> ts::TextProvider<&'a [u8]> for &'a GapBuffer {
    type I = SliceIter<'a>;

    fn text(&mut self, node: ts::Node<'_>) -> Self::I {
        let ts::Range {
            start_byte,
            end_byte,
            ..
        } = node.range();
        let char_from = self.byte_to_char(start_byte);
        let char_to = self.byte_to_char(end_byte);

        self.slice(char_from, char_to).slice_iter()
    }
}

/// A dynamically loaded tree-sitter parser backed by an on disk .so file
pub struct Parser {
    lang_name: String,
    inner: ts::Parser,
    lang: ts::Language,
    _lib: Library, // Need to prevent drop while the parser is in use
}

impl Deref for Parser {
    type Target = ts::Parser;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Parser {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl fmt::Debug for Parser {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Parser({})", self.lang_name)
    }
}

impl Parser {
    /// Error values returned by this function are intended as status messages to be
    /// presented to the user.
    pub fn try_new<P: AsRef<Path>>(so_dir: P, lang_name: &str) -> Result<Self, String> {
        let p = so_dir.as_ref().join(format!("{lang_name}.so"));
        let lang_fn = format!("tree_sitter_{lang_name}");

        // SAFETY: if the library loads and contains the target symbol we expect the
        //         given .so file to be a valid tree-sitter parser
        unsafe {
            let lib = Library::new(p).map_err(|e| e.to_string())?;
            let func: Symbol<'_, unsafe extern "C" fn() -> *const TSLanguage> =
                lib.get(lang_fn.as_bytes()).map_err(|e| e.to_string())?;

            let lang = ts::Language::from_raw(func());
            if lang.version() < ts::MIN_COMPATIBLE_LANGUAGE_VERSION {
                return Err(format!(
                    "incompatible .so tree-sitter parser version: {} < {}",
                    lang.version(),
                    ts::MIN_COMPATIBLE_LANGUAGE_VERSION
                ));
            }

            let mut inner = ts::Parser::new();
            inner.set_language(&lang).map_err(|e| e.to_string())?;

            Ok(Self {
                lang_name: lang_name.to_owned(),
                inner,
                lang,
                _lib: lib,
            })
        }
    }

    pub fn new_tokenizer(&self, query: &str) -> Result<Tokenizer, String> {
        let q = ts::Query::new(&self.lang, query).map_err(|e| format!("{e:?}"))?;
        let cur = ts::QueryCursor::new();

        Ok(Tokenizer {
            q,
            cur,
            ranges: Vec::new(),
        })
    }
}

pub struct Tokenizer {
    q: ts::Query,
    cur: ts::QueryCursor,
    ranges: Vec<SyntaxRange>,
}

impl fmt::Debug for Tokenizer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tokenizer")
    }
}

impl Tokenizer {
    pub fn update(&mut self, line_from: usize, line_to: usize, root: ts::Node<'_>, b: &Buffer) {
        // TODO: clear ranges within the newly updated region

        // This is a streaming-iterator not an interator, hence the odd while-let that follows
        let mut it = self
            .cur
            .set_point_range(ts::Point::new(line_from, 0)..ts::Point::new(line_to, 0))
            .captures(&self.q, root, &b.txt);

        while let Some((m, _)) = it.next() {
            for cap_idx in 0..self.q.capture_names().len() {
                for node in m.nodes_for_capture_index(cap_idx as u32) {
                    self.ranges.push(SyntaxRange {
                        r: node.range().into(),
                        cap_idx: Some(cap_idx),
                    });
                }
            }
        }

        // Compound queries such as the example below can result in duplicate nodes being returned
        // from the caputures iterator.
        //
        // (macro_invocation
        //   macro: (identifier) @function.macro
        //   "!" @function.macro)
        // TODO: compress adjacent ranges
        self.ranges.sort_unstable();
        self.ranges.dedup();
    }

    pub fn iter_tokenized_lines_from(
        &self,
        line: usize,
        b: &Buffer,
        load_exec_range: Option<(bool, Range)>,
    ) -> LineIter<'_> {
        let line_endings = b.txt.byte_line_endings();
        let start_byte = if line == 0 {
            0
        } else {
            line_endings[line - 1] + 1
        };

        let dot_range = ByteRange::from_range(b.dot.as_range(), b);
        let load_exec_range =
            load_exec_range.map(|(is_load, r)| (is_load, ByteRange::from_range(r, b)));

        LineIter {
            names: self.q.capture_names(),
            line_endings,
            ranges: &self.ranges,
            start_byte,
            line,
            dot_range,
            load_exec_range,
        }
    }
}

// #[inline]
// fn start_end_chars(
//     slice: Slice<'_>,
//     tabstop: usize,
//     col_off: usize,
//     max_cols: usize,
// ) -> (usize, usize, Option<usize>) {
//     let mut it = slice.chars().enumerate();
//     let mut wide = Vec::new();
//     let mut cols = 0;
//     let mut col_off = col_off;
//     let mut start = None;
//     let mut end = 0;

//     // Determine our start and end characters
//     loop {
//         let (i, w) = match it.next() {
//             Some((_, '\n')) | None => break,
//             Some((i, '\t')) => {
//                 wide.push((i, tabstop));
//                 (i, tabstop)
//             }
//             Some((i, c)) => {
//                 let w = UnicodeWidthChar::width(c).unwrap_or(1);
//                 if w > 1 {
//                     wide.push((i, w));
//                 }
//                 (i, w)
//             }
//         };
//         if w <= col_off {
//             col_off -= w;
//         } else if col_off > 0 {
//             col_off = 0;
//             start = Some((i, Some(w - col_off)));
//             cols += w - col_off;
//         } else {
//             if start.is_none() {
//                 start = Some((i, None))
//             }
//             cols += w;
//         }

//         if cols > max_cols {
//             break;
//         }

//         end = i;

//         if cols == max_cols {
//             break;
//         }
//     }

//     let (start, truncated) = start.unwrap_or_default();

//     (start, end, truncated)
// }

/// Byte offsets within a Buffer
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ByteRange {
    from: usize,
    to: usize,
}

impl ByteRange {
    fn from_range(r: Range, b: &Buffer) -> Self {
        let Range { start, end, .. } = r;

        Self {
            from: b.txt.char_to_byte(start.idx),
            to: b.txt.char_to_byte(end.idx),
        }
    }

    #[inline]
    fn intersects(&self, start_byte: usize, end_byte: usize) -> bool {
        self.from <= end_byte && start_byte <= self.to
    }

    #[inline]
    fn contains(&self, start_byte: usize, end_byte: usize) -> bool {
        self.from <= start_byte && self.to >= end_byte
    }
}

impl From<ts::Range> for ByteRange {
    fn from(r: ts::Range) -> Self {
        Self {
            from: r.start_byte,
            to: r.end_byte,
        }
    }
}

/// A tagged [ByteRange] denoting which tree-sitter capture index from our scheme query
/// matched this range within the buffer. A cap_idx of [None] indicates that this is a
/// default range for the purposes of syntax highlighting
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct SyntaxRange {
    cap_idx: Option<usize>,
    r: ByteRange,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RangeToken<'a> {
    pub(crate) ty: &'a str,
    pub(crate) r: ByteRange,
}

impl RangeToken<'_> {
    pub fn render(&self, buf: &mut String, b: &Buffer, cs: &ColorScheme) {
        let slice = b.txt.slice_from_byte_offsets(self.r.from, self.r.to);
        let styles = cs
            .get(self.ty)
            .or(cs.get(TK_DEFAULT))
            .expect("to have default styles");

        for s in styles {
            buf.push_str(&s.to_string());
        }

        let (l, r) = slice.as_strs();
        buf.push_str(l);
        buf.push_str(r);
        buf.push_str(&Style::Reset.to_string());
    }

    #[inline]
    fn split(self, at: usize) -> (Self, Self) {
        (
            RangeToken {
                ty: self.ty,
                r: ByteRange {
                    from: self.r.from,
                    to: at,
                },
            },
            RangeToken {
                ty: self.ty,
                r: ByteRange {
                    from: at,
                    to: self.r.to,
                },
            },
        )
    }
}

impl PartialOrd for SyntaxRange {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SyntaxRange {
    fn cmp(&self, other: &Self) -> Ordering {
        self.r.cmp(&other.r)
    }
}

/// Yield sub-iterators of tokens per-line in a file.
///
/// Any given [SyntaxRange] coming from the underlying [Tokenizer] may be
/// used by multiple [TokenIter]s coming from this iterator if the range
/// in question spans multiple lines
#[derive(Debug)]
pub struct LineIter<'a> {
    /// capture names to be used as the token types
    names: &'a [&'a str],
    /// byte offsets for the position of each newline in the input
    line_endings: Vec<usize>,
    /// full set of syntax ranges for the input
    ranges: &'a [SyntaxRange],
    start_byte: usize,
    /// the next line to yeild
    line: usize,
    dot_range: ByteRange,
    load_exec_range: Option<(bool, ByteRange)>,
}

fn map_range(
    ty: &str,
    br: ByteRange,
    start_byte: usize,
    end_byte: usize,
) -> Option<RangeToken<'_>> {
    if br.intersects(start_byte, end_byte) {
        Some(RangeToken {
            ty,
            r: ByteRange {
                from: max(br.from, start_byte),
                to: min(br.to, end_byte),
            },
        })
    } else {
        None
    }
}

impl<'a> Iterator for LineIter<'a> {
    type Item = TokenIter<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.line == self.line_endings.len() {
            return None;
        }

        let start_byte = self.start_byte;
        let end_byte = self.line_endings[self.line];

        self.line += 1;
        self.start_byte = end_byte + 1;

        // Determine tokens required for the next line
        let held: Option<RangeToken<'_>>;
        let ranges: Peekable<slice::Iter<'_, SyntaxRange>>;

        let dot_range = map_range(TK_DOT, self.dot_range, start_byte, end_byte);
        let load_exec_range = self.load_exec_range.and_then(|(is_load, br)| {
            let ty = if is_load { TK_LOAD } else { TK_EXEC };
            map_range(ty, br, start_byte, end_byte)
        });

        loop {
            match self.ranges.first() {
                // Advance to the next range
                Some(sr) if sr.r.to < start_byte => {
                    self.ranges = &self.ranges[1..];
                }

                // End of known tokens so everything else is just TK_DEFAULT
                None => {
                    held = Some(RangeToken {
                        ty: TK_DEFAULT,
                        r: ByteRange {
                            from: start_byte,
                            to: end_byte,
                        },
                    });
                    ranges = [].iter().peekable();
                    break;
                }

                // The next range is beyond this line
                Some(sr) if sr.r.from >= end_byte => {
                    held = Some(RangeToken {
                        ty: TK_DEFAULT,
                        r: ByteRange {
                            from: start_byte,
                            to: end_byte,
                        },
                    });
                    ranges = [].iter().peekable();
                    break;
                }

                // The next range fully contains the line
                Some(sr) if sr.r.contains(start_byte, end_byte) => {
                    held = Some(RangeToken {
                        ty: sr.cap_idx.map(|i| self.names[i]).unwrap_or(TK_DEFAULT),
                        r: ByteRange {
                            from: start_byte,
                            to: end_byte,
                        },
                    });
                    ranges = [].iter().peekable();
                    break;
                }

                // The next range starts at the beginning of the line or ends within the line
                Some(sr) => {
                    assert!(sr.r.from < end_byte);
                    if sr.r.from > start_byte {
                        held = Some(RangeToken {
                            ty: TK_DEFAULT,
                            r: ByteRange {
                                from: start_byte,
                                to: sr.r.from,
                            },
                        });
                    } else {
                        held = None;
                    }
                    ranges = self.ranges.iter().peekable();
                    break;
                }
            }
        }

        Some(TokenIter {
            start_byte,
            end_byte,
            names: self.names,
            ranges,
            held,
            dot_held: None,
            dot_range,
            load_exec_range,
        })
    }
}

type Rt<'a> = RangeToken<'a>;

#[derive(Debug, PartialEq, Eq)]
enum Held<'a> {
    One(Rt<'a>),
    Two(Rt<'a>, Rt<'a>),
    Three(Rt<'a>, Rt<'a>, Rt<'a>),
    Four(Rt<'a>, Rt<'a>, Rt<'a>, Rt<'a>),
    Five(Rt<'a>, Rt<'a>, Rt<'a>, Rt<'a>, Rt<'a>),
}

impl Held<'_> {
    fn byte_from_to(&self) -> (usize, usize) {
        match self {
            Held::One(a) => (a.r.from, a.r.to),
            Held::Two(a, b) => (a.r.from, b.r.to),
            Held::Three(a, _, b) => (a.r.from, b.r.to),
            Held::Four(a, _, _, b) => (a.r.from, b.r.to),
            Held::Five(a, _, _, _, b) => (a.r.from, b.r.to),
        }
    }

    fn split(self, at: usize) -> (Self, Self) {
        use Held::*;

        match self {
            One(a) => {
                let (l, r) = a.split(at);
                (One(l), One(r))
            }

            Two(a, b) => {
                if at == a.r.to {
                    (One(a), One(b))
                } else if a.r.contains(at, at) {
                    let (l, r) = a.split(at);
                    (One(l), Two(r, b))
                } else {
                    let (l, r) = b.split(at);
                    (Two(a, l), One(r))
                }
            }

            Three(a, b, c) => {
                if at == a.r.to {
                    (One(a), Two(b, c))
                } else if at == b.r.to {
                    (Two(a, b), One(c))
                } else if a.r.contains(at, at) {
                    let (l, r) = a.split(at);
                    (One(l), Three(r, b, c))
                } else if b.r.contains(at, at) {
                    let (l, r) = b.split(at);
                    (Two(a, l), Two(r, c))
                } else {
                    let (l, r) = c.split(at);
                    (Three(a, b, l), One(r))
                }
            }

            Four(_, _, _, _) => unreachable!("only called for 1-3"),
            Five(_, _, _, _, _) => unreachable!("only called for 1-3"),
        }
    }

    fn join(self, other: Self) -> Self {
        use Held::*;

        match (self, other) {
            (One(a), One(b)) => Two(a, b),
            (One(a), Two(b, c)) => Three(a, b, c),
            (One(a), Three(b, c, d)) => Four(a, b, c, d),
            (One(a), Four(b, c, d, e)) => Five(a, b, c, d, e),

            (Two(a, b), One(c)) => Three(a, b, c),
            (Two(a, b), Two(c, d)) => Four(a, b, c, d),
            (Two(a, b), Three(c, d, e)) => Five(a, b, c, d, e),

            (Three(a, b, c), One(d)) => Four(a, b, c, d),
            (Three(a, b, c), Two(d, e)) => Five(a, b, c, d, e),

            (Four(a, b, c, d), One(e)) => Five(a, b, c, d, e),

            _ => unreachable!("only have a max of 5 held"),
        }
    }
}

/// An iterator of tokens for a single line.
///
/// "default" ranges will be injected in-between the known syntax regions
/// so a consumer may treat the output of this iterator as a continous,
/// non-overlapping set of sub-regions spanning a single line within a
/// given buffer.
#[derive(Debug)]
pub struct TokenIter<'a> {
    /// byte offset for the start of this line
    start_byte: usize,
    /// byte offset for the end of this line
    end_byte: usize,
    /// Capture names to be used as the token types
    names: &'a [&'a str],
    /// The set of ranges applicable to this line
    ranges: Peekable<slice::Iter<'a, SyntaxRange>>,
    /// When yielding a dot range we may end up partially consuming
    /// the following range so we need to stash a Token for yielding
    /// on the next call to .next()
    held: Option<RangeToken<'a>>,
    dot_held: Option<Held<'a>>,
    dot_range: Option<RangeToken<'a>>,
    load_exec_range: Option<RangeToken<'a>>,
}

impl<'a> TokenIter<'a> {
    fn next_without_selections(&mut self) -> Option<RangeToken<'a>> {
        let held = self.held.take();
        if held.is_some() {
            return held;
        }

        let next = self.ranges.next()?;

        if next.r.from > self.end_byte {
            // Next available token is after this line and any 'default' held token will
            // have been emitted above before we hit this point, so we're done.
            return None;
        } else if next.r.to >= self.end_byte {
            // Last token runs until at least the end of this line so we just need to truncate
            // to the end of the line and ensure that the following call to .next() returns None.
            self.ranges = [].iter().peekable();

            return Some(RangeToken {
                ty: next.cap_idx.map(|i| self.names[i]).unwrap_or(TK_DEFAULT),
                r: ByteRange {
                    from: max(next.r.from, self.start_byte),
                    to: self.end_byte,
                },
            });
        }

        match self.ranges.peek() {
            Some(sr) if sr.r.from > self.end_byte => {
                self.ranges = [].iter().peekable();

                self.held = Some(RangeToken {
                    ty: TK_DEFAULT,
                    r: ByteRange {
                        from: next.r.to,
                        to: self.end_byte,
                    },
                });
            }

            Some(sr) if sr.r.from > next.r.to => {
                self.held = Some(RangeToken {
                    ty: TK_DEFAULT,
                    r: ByteRange {
                        from: next.r.to,
                        to: sr.r.from,
                    },
                });
            }

            None if next.r.to < self.end_byte => {
                self.held = Some(RangeToken {
                    ty: TK_DEFAULT,
                    r: ByteRange {
                        from: next.r.to,
                        to: self.end_byte,
                    },
                });
            }

            _ => (),
        }

        Some(RangeToken {
            ty: next.cap_idx.map(|i| self.names[i]).unwrap_or(TK_DEFAULT),
            r: ByteRange {
                from: max(next.r.from, self.start_byte),
                to: next.r.to,
            },
        })
    }

    fn update_held(&mut self, mut held: Held<'a>, rt: RangeToken<'a>) -> Held<'a> {
        let (self_from, self_to) = held.byte_from_to();
        let (from, to) = (rt.r.from, rt.r.to);

        match (from.cmp(&self_from), to.cmp(&self_to)) {
            (Ordering::Less, _) => unreachable!("only called when rt >= self"),

            (Ordering::Equal, Ordering::Less) => {
                // hold rt then remaining of held
                let (_, r) = held.split(to);
                held = Held::One(rt).join(r);
            }

            (Ordering::Greater, Ordering::Less) => {
                // hold held up to rt, rt & held from rt
                let (l, r) = held.split(from);
                let (_, r) = r.split(to);
                held = l.join(Held::One(rt)).join(r);
            }

            (Ordering::Equal, Ordering::Equal) => {
                // replace held with rt
                held = Held::One(rt);
            }

            (Ordering::Greater, Ordering::Equal) => {
                // hold held to rt & rt
                let (l, _) = held.split(from);
                held = l.join(Held::One(rt));
            }

            (Ordering::Equal, Ordering::Greater) => {
                // hold rt, consume to find other held tokens (if any)
                held = self.find_end_of_selection(Held::One(rt), to);
            }

            (Ordering::Greater, Ordering::Greater) => {
                // hold held to rt & rt, consume to find other held tokens (if any)
                let (l, _) = held.split(from);
                held = self.find_end_of_selection(l.join(Held::One(rt)), to);
            }
        }

        held
    }

    fn find_end_of_selection(&mut self, mut held: Held<'a>, to: usize) -> Held<'a> {
        loop {
            let mut next = match self.next_without_selections() {
                None => break,
                Some(next) => next,
            };
            if next.r.to <= to {
                continue; // token is entirely within rt
            }
            next.r.from = to;
            held = held.join(Held::One(next));
            break;
        }

        held
    }

    fn pop(&mut self) -> Option<RangeToken<'a>> {
        match self.dot_held {
            None => None,
            Some(Held::One(a)) => {
                self.dot_held = None;
                Some(a)
            }
            Some(Held::Two(a, b)) => {
                self.dot_held = Some(Held::One(b));
                Some(a)
            }
            Some(Held::Three(a, b, c)) => {
                self.dot_held = Some(Held::Two(b, c));
                Some(a)
            }
            Some(Held::Four(a, b, c, d)) => {
                self.dot_held = Some(Held::Three(b, c, d));
                Some(a)
            }
            Some(Held::Five(a, b, c, d, e)) => {
                self.dot_held = Some(Held::Four(b, c, d, e));
                Some(a)
            }
        }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = RangeToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        // Emit pre-computed held tokens first
        let next = self.pop();
        if next.is_some() {
            return next;
        }

        // Determine the next token we would emit in the absense of any user selections and then
        // apply the selections in priority order:
        //   - dot overwrites original syntax highlighting
        //   - load/exec overwrite dot
        #[inline]
        fn intersects(opt: &Option<RangeToken<'_>>, from: usize, to: usize) -> bool {
            opt.as_ref()
                .map(|rt| rt.r.intersects(from, to))
                .unwrap_or(false)
        }

        let next = self.next_without_selections()?;
        let (from, to) = (next.r.from, next.r.to);
        let mut held = Held::One(next);

        if intersects(&self.dot_range, from, to) {
            let r = self.dot_range.take().unwrap();
            held = self.update_held(held, r);
        }

        let (from, to) = held.byte_from_to();
        if intersects(&self.load_exec_range, from, to) {
            let r = self.load_exec_range.take().unwrap();
            held = self.update_held(held, r);
        }

        if let Held::One(rt) = held {
            Some(rt) // held_dot is None so just return the token directly
        } else {
            self.dot_held = Some(held);
            self.pop()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use simple_test_case::test_case;

    fn sr(from: usize, to: usize) -> SyntaxRange {
        SyntaxRange {
            cap_idx: Some(0),
            r: ByteRange { from, to },
        }
    }

    fn rt_def(from: usize, to: usize) -> RangeToken<'static> {
        RangeToken {
            ty: TK_DEFAULT,
            r: ByteRange { from, to },
        }
    }

    fn rt_dot(from: usize, to: usize) -> RangeToken<'static> {
        RangeToken {
            ty: TK_DOT,
            r: ByteRange { from, to },
        }
    }

    fn rt_exe(from: usize, to: usize) -> RangeToken<'static> {
        RangeToken {
            ty: TK_EXEC,
            r: ByteRange { from, to },
        }
    }

    fn rt_str(from: usize, to: usize) -> RangeToken<'static> {
        RangeToken {
            ty: "string",
            r: ByteRange { from, to },
        }
    }

    // range at start of single token
    #[test_case(
        Held::One(rt_str(0, 5)),
        None,
        rt_dot(0, 5),
        &[sr(10, 15)],
        Held::One(rt_dot(0, 5));
        "held one range matches held"
    )]
    #[test_case(
        Held::One(rt_str(0, 5)),
        None,
        rt_dot(0, 3),
        &[sr(10, 15)],
        Held::Two(rt_dot(0, 3), rt_str(3, 5));
        "held one range start to within held"
    )]
    #[test_case(
        Held::One(rt_str(0, 5)),
        Some(rt_def(5, 10)),
        rt_dot(0, 7),
        &[sr(10, 15), sr(20, 30)],
        Held::Two(rt_dot(0, 7), rt_def(7, 10));
        "held one range start to past held but before next token"
    )]
    #[test_case(
        Held::One(rt_str(0, 5)),
        Some(rt_def(5, 10)),
        rt_dot(0, 13),
        &[sr(10, 15), sr(20, 30)],
        Held::Two(rt_dot(0, 13), rt_str(13, 15));
        "held one range start to into next token"
    )]
    #[test_case(
        Held::One(rt_str(0, 5)),
        Some(rt_def(5, 10)),
        rt_dot(0, 16),
        &[sr(10, 15), sr(20, 30)],
        Held::Two(rt_dot(0, 16), rt_def(16, 20));
        "held one range start to past next token"
    )]
    // range within single token
    #[test_case(
        Held::One(rt_str(0, 5)),
        None,
        rt_dot(3, 5),
        &[sr(10, 15)],
        Held::Two(rt_str(0, 3), rt_dot(3, 5));
        "held one range from within to end of held"
    )]
    #[test_case(
        Held::One(rt_str(0, 5)),
        None,
        rt_dot(2, 4),
        &[sr(10, 15)],
        Held::Three(rt_str(0, 2), rt_dot(2, 4), rt_str(4, 5));
        "held one range with to within held"
    )]
    #[test_case(
        Held::One(rt_str(0, 5)),
        Some(rt_def(5, 10)),
        rt_dot(3, 7),
        &[sr(10, 15), sr(20, 30)],
        Held::Three(rt_str(0, 3), rt_dot(3, 7), rt_def(7, 10));
        "held one range within to past held but before next token"
    )]
    #[test_case(
        Held::One(rt_str(0, 5)),
        Some(rt_def(5, 10)),
        rt_dot(3, 13),
        &[sr(10, 15), sr(20, 30)],
        Held::Three(rt_str(0, 3), rt_dot(3, 13), rt_str(13, 15));
        "held one range within to into next token"
    )]
    #[test_case(
        Held::One(rt_str(0, 5)),
        Some(rt_def(5, 10)),
        rt_dot(3, 16),
        &[sr(10, 15), sr(20, 30)],
        Held::Three(rt_str(0, 3), rt_dot(3, 16), rt_def(16, 20));
        "held one range within to past next token"
    )]
    // held 2 tokens
    #[test_case(
        Held::Two(rt_str(0, 3), rt_dot(3, 5)),
        None,
        rt_exe(0, 5),
        &[sr(10, 15)],
        Held::One(rt_exe(0, 5));
        "held two range matches all held"
    )]
    #[test_case(
        Held::Two(rt_str(0, 3), rt_dot(3, 5)),
        None,
        rt_exe(2, 5),
        &[sr(10, 15)],
        Held::Two(rt_str(0, 2), rt_exe(2, 5));
        "held two range from within first to end of held"
    )]
    #[test_case(
        Held::Two(rt_str(0, 3), rt_dot(3, 5)),
        None,
        rt_exe(4, 5),
        &[sr(10, 15)],
        Held::Three(rt_str(0, 3), rt_dot(3, 4), rt_exe(4, 5));
        "held two range from within second to end of held"
    )]
    #[test_case(
        Held::Two(rt_str(0, 3), rt_dot(3, 5)),
        Some(rt_def(5, 10)),
        rt_exe(4, 8),
        &[sr(10, 15)],
        Held::Four(rt_str(0, 3), rt_dot(3, 4), rt_exe(4, 8), rt_def(8, 10));
        "held two range from within second past end of held"
    )]
    // held 3 tokens
    #[test_case(
        Held::Three(rt_str(0, 3), rt_dot(3, 5), rt_str(5, 8)),
        None,
        rt_exe(0, 8),
        &[sr(10, 15)],
        Held::One(rt_exe(0, 8));
        "held three range matches all held"
    )]
    #[test_case(
        Held::Three(rt_str(0, 3), rt_dot(3, 5), rt_str(5, 8)),
        None,
        rt_exe(2, 8),
        &[sr(10, 15)],
        Held::Two(rt_str(0, 2), rt_exe(2, 8));
        "held three range from within first to end of held"
    )]
    #[test_case(
        Held::Three(rt_str(0, 3), rt_dot(3, 5), rt_str(5, 8)),
        None,
        rt_exe(4, 8),
        &[sr(10, 15)],
        Held::Three(rt_str(0, 3), rt_dot(3, 4), rt_exe(4, 8));
        "held three range from within second to end of held"
    )]
    #[test_case(
        Held::Three(rt_str(0, 3), rt_dot(3, 6), rt_str(6, 9)),
        None,
        rt_exe(4, 5),
        &[sr(10, 15)],
        Held::Five(rt_str(0, 3), rt_dot(3, 4), rt_exe(4, 5), rt_dot(5, 6), rt_str(6, 9));
        "held three range from within second"
    )]
    #[test]
    fn update_held(
        initial: Held<'static>,
        held: Option<RangeToken<'static>>,
        r: RangeToken<'static>,
        ranges: &[SyntaxRange],
        expected: Held<'static>,
    ) {
        let mut it = TokenIter {
            start_byte: 0,
            end_byte: 42,
            names: &["string"],
            ranges: ranges.iter().peekable(),
            held,
            dot_held: None,
            dot_range: None,
            load_exec_range: None,
        };

        let held = it.update_held(initial, r);

        assert_eq!(held, expected);
    }
}
