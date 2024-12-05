//! Support for tree-sitter incremental parsing, querying and highlighting of Buffers
//!
//! For a given language the user needs to provide a .so file containing the compiled
//! tree-sitter parser and a highlights .scm file for driving the highlighting.
use crate::{
    buffer::{Buffer, GapBuffer, SliceIter},
    dot::{LineRange, Range},
    term::Style,
};
use libloading::{Library, Symbol};
use std::collections::HashMap;
use std::{
    cmp::{max, Ord, Ordering, PartialOrd},
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
    }

    pub fn iter_tokenized_lines_from(&self, line: usize, b: &Buffer) -> LineIter<'_> {
        let line_endings = b.txt.byte_line_endings();
        let start_byte = if line == 0 {
            0
        } else {
            line_endings[line - 1] + 1
        };

        LineIter {
            names: self.q.capture_names(),
            line_endings,
            ranges: &self.ranges,
            start_byte,
            line,
            dot_range: ByteRange { from: 0, to: 12 },
            load_exec_range: None,
        }
    }

    // Tokenizing and rendering a line:
    // - inputs: Buffer, col_off, y, lpad, screen_cols, dot_range, tabstop
    // - compute:
    //   - max_cols (screen_cols - lpad)
    //   - start and end character offsets within the line
    //   - required padding
    // pub fn styled_line(
    //     &self,
    //     b: &Buffer,
    //     // col_off: usize,
    //     // tabstop: usize,
    //     y: usize,
    //     // lpad: usize,
    //     // screen_cols: usize,
    //     load_exec_range: Option<(bool, Range)>,
    //     cs: &ColorScheme,
    // ) -> String {
    //     let slice = b.txt.line(y);
    //     let byte_from = slice.from_byte();
    //     let len = b.txt.line_len_chars(y);
    //     let byte_to = byte_from + len;
    //     // let max_cols = screen_cols - lpad;

    //     // FIXME: handle col_off
    //     // let (start, end, truncated) = start_end_chars(slice, tabstop, col_off, max_cols);
    //     let tokens = self.tokenize_line(byte_from, byte_to);

    //     let dot_range = b.dot.line_range(y, b).map(|lr| map_line_range(lr, len));
    //     let mut tokens = match dot_range {
    //         Some((start, end)) => {
    //             Tokens::Multi(tokens.with_highlighted_dot(start, end, len, TK_DOT))
    //         }
    //         None => tokens,
    //     };

    //     match load_exec_range {
    //         Some((is_load, rng)) if !b.dot.contains_range(&rng) => {
    //             if let Some((start, end)) = rng.line_range(y, b).map(|lr| map_line_range(lr, len)) {
    //                 let ty = if is_load { TK_LOAD } else { TK_EXEC };
    //                 tokens = Tokens::Multi(tokens.with_highlighted_dot(start, end, len, ty));
    //             }
    //         }

    //         _ => (),
    //     }

    //     let tks = match tokens {
    //         Tokens::Single(tk) => vec![tk],
    //         Tokens::Multi(tks) => tks,
    //     };

    //     let mut buf = String::with_capacity(len);
    //     let mut post: Option<Vec<Style>> = None;
    //     let mut idx = 0;

    //     for tk in tks.into_iter() {
    //         if tk.i > 0 {
    //             buf.extend(slice.chars().skip(idx).take(tk.i - idx));
    //         }

    //         if let Some(styles) = post {
    //             for s in styles {
    //                 buf.push_str(&s.to_string());
    //             }
    //         }

    //         post = tk.push_styles(cs, &mut buf);
    //         idx = tk.i;
    //     }

    //     buf
    // }
}

// // FIXME: need to convert char indices into byte indices here
// #[inline]
// fn map_line_range(lr: LineRange, len: usize, b: &Buffer) -> (usize, usize) {
//     match lr {
//         // LineRange is an inclusive range so we need to insert after `end` if its
//         // not the end of the line
//         LineRange::Partial { start, end, .. } => (start, end + 1),
//         LineRange::FromStart { end, .. } => (0, end + 1),
//         LineRange::ToEnd { start, .. } => (start, len),
//         LineRange::Full { .. } => (0, len),
//     }
// }

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

        let mut post = Vec::with_capacity(styles.len());

        for s in styles {
            buf.push_str(&s.to_string());
            match s {
                Style::Bold => post.push(Style::NoBold),
                Style::Italic => post.push(Style::NoItalic),
                Style::Underline => post.push(Style::NoUnderline),
                Style::Reverse => post.push(Style::NoReverse),
                _ => (),
            }
        }

        let (l, r) = slice.as_strs();
        buf.push_str(l);
        buf.push_str(r);

        for s in post.into_iter() {
            buf.push_str(&s.to_string());
        }
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

impl<'a> Iterator for LineIter<'a> {
    type Item = TokenIter<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.line == self.line_endings.len() {
            return None;
        }

        let start_byte = self.start_byte;
        let end_byte = self.line_endings[self.line];

        let dot_range = if self.dot_range.intersects(start_byte, end_byte) {
            Some(self.dot_range)
        } else {
            None
        };
        let load_exec_range = self.load_exec_range.and_then(|(is_load, r)| {
            if r.intersects(start_byte, end_byte) {
                Some((is_load, r))
            } else {
                None
            }
        });

        // Determine tokens required for the next line
        let held: Option<RangeToken<'_>>;
        let ranges: Peekable<slice::Iter<'_, SyntaxRange>>;

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

        self.line += 1;
        self.start_byte = end_byte + 1;

        Some(TokenIter {
            start_byte,
            end_byte,
            names: self.names,
            ranges,
            held,
            dot_range,
            load_exec_range,
        })
    }
}

/// An iterator of tokens for a single line.
///
/// "normal" ranges will be injected in-between the known syntax regions
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
    dot_range: Option<ByteRange>,
    load_exec_range: Option<(bool, ByteRange)>,
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = RangeToken<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let held = self.held.take();
        if held.is_some() {
            return held;
        }

        let next = self.ranges.next()?;

        if next.r.from > self.end_byte {
            return None;
        } else if next.r.to >= self.end_byte {
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
}
