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
    fmt,
    ops::{ControlFlow, Deref, DerefMut},
    path::Path,
};
use streaming_iterator::StreamingIterator;
use tree_sitter::{self as ts, ffi::TSLanguage};
// use unicode_width::UnicodeWidthChar;

const TK_DEFAULT: &str = "default";
const TK_END: &str = "end";
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
                    let ts::Range {
                        start_point,
                        end_point,
                        ..
                    } = node.range();

                    self.ranges.push(SyntaxRange {
                        start: start_point,
                        end: end_point,
                        cap_idx,
                    });
                }
            }
        }
    }

    pub fn tokenize_line(&self, line: usize, len: usize) -> Tokens<'_> {
        let names = self.q.capture_names();
        let start = ts::Point::new(line, 0);
        let end = ts::Point::new(line, len);

        let mut tokens = Vec::new();
        let mut i = 0;

        for r in self.ranges.iter() {
            if r.push_tokens(line, start, end, &mut i, &mut tokens, names)
                .is_break()
            {
                break;
            }
        }

        if tokens.is_empty() {
            tokens.push(Token {
                ty: TK_DEFAULT,
                i: 0,
            });
        } else if i > 0 && i < len {
            tokens.push(Token { ty: TK_DEFAULT, i });
        }

        tokens.push(Token { ty: TK_END, i: len });

        Tokens::Multi(tokens)
    }

    // Tokenizing and rendering a line:
    // - inputs: Buffer, col_off, y, lpad, screen_cols, dot_range, tabstop
    // - compute:
    //   - max_cols (screen_cols - lpad)
    //   - start and end character offsets within the line
    //   - required padding
    pub fn styled_line(
        &self,
        b: &Buffer,
        // col_off: usize,
        // tabstop: usize,
        y: usize,
        // lpad: usize,
        // screen_cols: usize,
        load_exec_range: Option<(bool, Range)>,
        cs: &ColorScheme,
    ) -> String {
        let slice = b.txt.line(y);
        let len = slice.len_utf8();
        // let max_cols = screen_cols - lpad;

        // FIXME: handle col_off
        // let (start, end, truncated) = start_end_chars(slice, tabstop, col_off, max_cols);
        let tokens = self.tokenize_line(y, len);

        let dot_range = b.dot.line_range(y, b).map(|lr| map_line_range(lr, len));
        let mut tokens = match dot_range {
            Some((start, end)) => {
                Tokens::Multi(tokens.with_highlighted_dot(start, end, len, TK_DOT))
            }
            None => tokens,
        };

        match load_exec_range {
            Some((is_load, rng)) if !b.dot.contains_range(&rng) => {
                if let Some((start, end)) = rng.line_range(y, b).map(|lr| map_line_range(lr, len)) {
                    let ty = if is_load { TK_LOAD } else { TK_EXEC };
                    tokens = Tokens::Multi(tokens.with_highlighted_dot(start, end, len, ty));
                }
            }

            _ => (),
        }

        let tks = match tokens {
            Tokens::Single(tk) => vec![tk],
            Tokens::Multi(tks) => tks,
        };

        let mut buf = String::with_capacity(len);
        let mut post: Option<Vec<Style>> = None;
        let mut idx = 0;

        for tk in tks.into_iter() {
            if tk.i > 0 {
                buf.extend(slice.chars().skip(idx).take(tk.i - idx));
            }

            if let Some(styles) = post {
                for s in styles {
                    buf.push_str(&s.to_string());
                }
            }

            post = tk.push_styles(cs, &mut buf);
            idx = tk.i;
        }

        buf
    }
}

#[inline]
fn map_line_range(lr: LineRange, len: usize) -> (usize, usize) {
    match lr {
        // LineRange is an inclusive range so we need to insert after `end` if its
        // not the end of the line
        LineRange::Partial { start, end, .. } => (start, end + 1),
        LineRange::FromStart { end, .. } => (0, end + 1),
        LineRange::ToEnd { start, .. } => (start, len),
        LineRange::Full { .. } => (0, len),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct SyntaxRange {
    start: ts::Point,
    end: ts::Point,
    cap_idx: usize,
}

impl SyntaxRange {
    fn push_tokens<'a>(
        &self,
        line: usize,
        start: ts::Point,
        end: ts::Point,
        idx: &mut usize,
        tokens: &mut Vec<Token<'a>>,
        names: &[&'a str],
    ) -> ControlFlow<(), ()> {
        if self.end.row < line {
            return ControlFlow::Continue(());
        } else if self.start.row > line {
            return ControlFlow::Break(());
        } else if self.start <= start && self.end >= end {
            // full line
            tokens.push(Token {
                ty: names[self.cap_idx],
                i: 0,
            });
            return ControlFlow::Break(());
        } else if self.start.row == line && self.end >= end {
            // from within line until the end
            tokens.push(Token {
                ty: TK_DEFAULT,
                i: *idx,
            });
            tokens.push(Token {
                ty: names[self.cap_idx],
                i: self.start.column,
            });
            return ControlFlow::Break(());
        }

        if self.start.row < line {
            tokens.push(Token {
                ty: names[self.cap_idx],
                i: 0,
            });
        } else {
            tokens.push(Token {
                ty: TK_DEFAULT,
                i: *idx,
            });
            tokens.push(Token {
                ty: names[self.cap_idx],
                i: self.start.column,
            });
        }

        *idx = self.end.column;

        ControlFlow::Continue(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Token<'a> {
    pub ty: &'a str,
    pub i: usize,
}

impl<'a> Token<'a> {
    // If there is any resets that need to take place at the end of this token they are returned
    #[must_use]
    pub(crate) fn push_styles(&self, cs: &ColorScheme, buf: &mut String) -> Option<Vec<Style>> {
        let styles = cs
            .get(self.ty)
            .or(cs.get(TK_DEFAULT))
            .expect("to have default styles");

        let mut post = Vec::new();

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

        if post.is_empty() {
            None
        } else {
            Some(post)
        }
    }

    fn with_highlighted_dot(
        self,
        start: usize,
        end: usize,
        tk_end: usize,
        ty: &'a str,
    ) -> Vec<Token<'a>> {
        let mut tks = Vec::with_capacity(3);
        if start > self.i {
            tks.push(self);
        }
        tks.push(Token { ty, i: start });
        if end < tk_end {
            tks.push(Token {
                ty: self.ty,
                i: end,
            });
        }

        tks
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tokens<'a> {
    Single(Token<'a>),
    Multi(Vec<Token<'a>>),
}

impl<'a> Tokens<'a> {
    pub fn with_highlighted_dot(
        self,
        start: usize,
        end: usize,
        len: usize,
        ty: &'a str,
    ) -> Vec<Token<'a>> {
        match self {
            Self::Single(tk) => tk.with_highlighted_dot(start, end, len, ty),

            Self::Multi(tks) => {
                let mut new_tks = Vec::new();
                let mut it = tks.into_iter().peekable();

                while let Some(tk) = it.next() {
                    let tk_end = it.peek().map(|t| t.i).unwrap_or(len);

                    if start >= tk_end || end <= tk.i {
                        new_tks.push(tk);
                    } else {
                        new_tks.extend_from_slice(&tk.with_highlighted_dot(start, end, tk_end, ty));
                    }
                }

                new_tks
            }
        }
    }
}
