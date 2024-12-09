use ad_editor::{
    buffer::Buffer,
    dot::Dot,
    term::{Color, Style},
    ts::{Parser, TokenIter, TK_DEFAULT},
};
use std::{cmp::Ordering, collections::HashMap, error::Error, fs};
use unicode_width::UnicodeWidthChar;

type ColorScheme = HashMap<String, Vec<Style>>;

const QUERY: &str = "\
(macro_invocation
  macro: (identifier) @function.macro
  \"!\" @function.macro)
(line_comment) @comment
(block_comment) @comment
(char_literal) @char
(string_literal) @string
(raw_string_literal) @string";

fn main() -> Result<(), Box<dyn Error>> {
    let mut parser = Parser::try_new(
        "/home/sminez/.local/share/nvim/lazy/nvim-treesitter/parser",
        "rust",
    )?;
    let content = fs::read_to_string(file!()).unwrap();
    let b = Buffer::new_unnamed(0, content);
    let tree = parser.parse(b.contents(), None).unwrap();
    let root = tree.root_node();

    let mut tkz = parser.new_tokenizer(QUERY).unwrap();
    tkz.update(0, usize::MAX, root, &b);

    // hacked up versions of the in-crate code for this example
    let bg: Color = "#1B1720".try_into().unwrap();
    let fg: Color = "#E6D29E".try_into().unwrap();
    let comment: Color = "#624354".try_into().unwrap();
    let string: Color = "#61DCA5".try_into().unwrap();
    let dot_bg: Color = "#336677".try_into().unwrap();
    let load_bg: Color = "#957FB8".try_into().unwrap();
    let exec_bg: Color = "#Bf616A".try_into().unwrap();

    let cs: HashMap<String, Vec<Style>> = [
        ("default", vec![Style::Bg(bg), Style::Fg(fg)]),
        ("comment", vec![Style::Italic, Style::Fg(comment)]),
        ("string", vec![Style::Fg(string)]),
        ("char", vec![Style::Bold, Style::Fg(string)]),
        ("dot", vec![Style::Fg(fg), Style::Bg(dot_bg)]),
        ("load", vec![Style::Fg(fg), Style::Bg(load_bg)]),
        ("exec", vec![Style::Fg(fg), Style::Bg(exec_bg)]),
        ("function.macro", vec![Style::Fg(load_bg)]),
    ]
    .map(|(s, v)| (s.to_string(), v))
    .into_iter()
    .collect();

    let exec_rng = Some((false, Dot::from_char_indices(56, 90).as_range()));
    let tabstop = 4;
    let col_off = 0;
    let max_cols = 80;

    for it in tkz.iter_tokenized_lines_from(0, &b, exec_rng) {
        let s = render_line(&b, it, col_off, max_cols, tabstop, &cs);
        println!("{s}");
    }

    Ok(())
}

fn render_line(
    b: &Buffer,
    it: TokenIter<'_>,
    col_off: usize,
    max_cols: usize,
    tabstop: usize,
    cs: &ColorScheme,
) -> String {
    let mut buf = String::new();
    let mut to_skip = col_off;
    let mut cols = 0;

    for tk in it {
        let styles = cs
            .get(tk.tag())
            .or(cs.get(TK_DEFAULT))
            .expect("to have default styles");
        let slice = tk.as_slice(b);
        let mut chars = slice.chars().peekable();
        let mut spaces = None;

        if to_skip > 0 {
            for ch in chars.by_ref() {
                let w = if ch == '\t' {
                    tabstop
                } else {
                    UnicodeWidthChar::width(ch).unwrap_or(1)
                };

                match to_skip.cmp(&w) {
                    Ordering::Less => {
                        spaces = Some(w - to_skip);
                        break;
                    }
                    Ordering::Equal => break,
                    Ordering::Greater => to_skip -= w,
                }
            }
        }

        for s in styles {
            buf.push_str(&s.to_string());
        }

        if let Some(n) = spaces {
            buf.extend(std::iter::repeat_n(' ', n));
            cols = n;
        }

        for ch in chars {
            let w = if ch == '\t' {
                tabstop
            } else {
                UnicodeWidthChar::width(ch).unwrap_or(1)
            };

            if cols + w <= max_cols {
                buf.push(ch);
                cols += w;
            } else {
                break;
            }
        }

        buf.push_str(&Style::Reset.to_string());
    }

    if cols < max_cols {
        buf.extend(std::iter::repeat_n(' ', max_cols - cols));
    }

    buf
}
