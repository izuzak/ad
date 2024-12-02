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

const TK_DEFAULT: &str = "default";
const TK_END: &str = "end";

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

    pub fn tokenize_line(&self, line: usize, len: usize) -> Vec<(&str, usize)> {
        let names = self.q.capture_names();
        let start = ts::Point::new(line, 0);
        let end = ts::Point::new(line, len);

        let mut tokens = Vec::new();
        let mut idx = 0;

        for r in self.ranges.iter() {
            if r.push_tokens(line, start, end, &mut idx, &mut tokens, &names)
                .is_break()
            {
                break;
            }
        }

        if tokens.is_empty() {
            tokens.push((TK_DEFAULT, 0));
        } else if idx > 0 && idx < len {
            tokens.push((TK_DEFAULT, idx));
        }

        tokens.push((TK_END, len));

        tokens
    }

    // // Tokenizing and rendering a line:
    // // - inputs: Buffer, col_off, y, lpad, screen_cols, dot_range, tabstop
    // // - compute:
    // //   - max_cols (screen_cols - lpad)
    // //   - start and end character offsets within the line
    // //   - required padding
    // fn styled_line(
    //     &self,
    //     b: &Buffer,
    //     col_off: usize,
    //     tabstop: usize,
    //     y: usize,
    //     lpad: usize,
    //     screen_cols: usize,
    //     load_exec_range: Option<(bool, Range)>,
    //     cs: &ColorScheme,
    // ) -> String {
    //     let max_cols = screen_cols - lpad;
    // }
}

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
        tokens: &mut Vec<(&'a str, usize)>,
        names: &[&'a str],
    ) -> ControlFlow<(), ()> {
        if self.end.row < line {
            return ControlFlow::Continue(());
        } else if self.start.row > line {
            return ControlFlow::Break(());
        } else if self.start <= start && self.end >= end {
            // full line
            tokens.push((names[self.cap_idx], 0));
            return ControlFlow::Break(());
        } else if self.start.row == line && self.end >= end {
            // from within line until the end
            tokens.push((TK_DEFAULT, *idx));
            tokens.push((names[self.cap_idx], self.start.column));
            return ControlFlow::Break(());
        }

        if self.start.row < line {
            tokens.push((names[self.cap_idx], 0));
        } else {
            tokens.push((TK_DEFAULT, *idx));
            tokens.push((names[self.cap_idx], self.start.column));
        }

        *idx = self.end.column;

        ControlFlow::Continue(())
    }
}
