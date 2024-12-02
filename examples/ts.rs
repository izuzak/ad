use ad_editor::{buffer::Buffer, ts::Parser};
use std::{error::Error, fs};

const QUERY: &str = "\
(line_comment) @comment
(block_comment) @comment
(char_literal) @string
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

    // hacked up versions of the in-crate code for this demo
    let fg: Color = "#E6D29E".try_into().unwrap();
    let comment: Color = "#624354".try_into().unwrap();
    let string: Color = "#61DCA5".try_into().unwrap();

    let mut i = 0;

    while let Some(slice) = b.line(i) {
        let mut buf = String::new();
        let mut idx = 0;

        for (tag, ch_idx) in tkz.tokenize_line(i, slice.len_utf8()).into_iter() {
            if ch_idx > 0 {
                buf.extend(slice.chars().skip(idx).take(ch_idx - idx));
            }
            match tag {
                "default" => buf.push_str(&Style::Fg(fg).to_string()),
                "comment" => buf.push_str(&Style::Fg(comment).to_string()),
                "string" => buf.push_str(&Style::Fg(string).to_string()),
                "end" => buf.push_str(&Style::Reset.to_string()),
                _ => panic!("unknown tag: {tag}"),
            }
            idx = ch_idx;
        }

        print!("{buf}");
        i += 1;
    }

    Ok(())
}

// From the term module

use std::fmt;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
struct Color {
    r: u8,
    b: u8,
    g: u8,
}

impl TryFrom<&str> for Color {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, String> {
        let [_, r, g, b] = match u32::from_str_radix(s.strip_prefix('#').unwrap_or(s), 16) {
            Ok(hex) => hex.to_be_bytes(),
            Err(e) => return Err(format!("invalid color ('{s}'): {e}")),
        };

        Ok(Self { r, g, b })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Style {
    Fg(Color),
    Reset,
}

impl fmt::Display for Style {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Style::*;

        match self {
            Fg(Color { r, b, g }) => write!(f, "\x1b[38;2;{r};{g};{b}m"),
            Reset => write!(f, "\x1b[m"),
        }
    }
}
