use ad_editor::{
    buffer::Buffer,
    dot::Dot,
    term::{Color, Style},
    ts::Parser,
};
use std::{collections::HashMap, error::Error, fs};

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

    // hacked up versions of the in-crate code for this example
    let bg: Color = "#1B1720".try_into().unwrap();
    let fg: Color = "#E6D29E".try_into().unwrap();
    let comment: Color = "#624354".try_into().unwrap();
    let string: Color = "#61DCA5".try_into().unwrap();
    let dot_bg: Color = "#336677".try_into().unwrap();
    let load_bg: Color = "#957FB8".try_into().unwrap();
    let exec_bg: Color = "#Bf616A".try_into().unwrap();

    let cs: HashMap<String, Vec<Style>> = [
        ("default".to_string(), vec![Style::Bg(bg), Style::Fg(fg)]),
        (
            "comment".to_string(),
            vec![Style::Italic, Style::Fg(comment)],
        ),
        ("string".to_string(), vec![Style::Fg(string)]),
        ("dot".to_string(), vec![Style::Fg(fg), Style::Bg(dot_bg)]),
        ("load".to_string(), vec![Style::Fg(fg), Style::Bg(load_bg)]),
        ("exec".to_string(), vec![Style::Fg(fg), Style::Bg(exec_bg)]),
        ("end".to_string(), vec![Style::Bg(bg), Style::Fg(fg)]),
    ]
    .into_iter()
    .collect();

    let exec_rng = Some((false, Dot::from_char_indices(56, 73).as_range()));

    for y in 0..b.len_lines() {
        print!("{}", tkz.styled_line(&b, y, exec_rng, &cs));
    }

    Ok(())
}
