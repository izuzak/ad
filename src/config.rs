//! A minimal config file format for ad
use crate::{
    buffer::Buffer,
    key::Input,
    mode::normal_mode,
    term::{Color, Style},
    trie::Trie,
};
use std::{collections::HashMap, env, fs, io};

pub const TK_DEFAULT: &str = "default";
pub const TK_DOT: &str = "dot";
pub const TK_LOAD: &str = "load";
pub const TK_EXEC: &str = "exec";

/// A colorscheme for rendering the UI.
///
/// UI elements are available as properties and syntax stylings are available as a map of string
/// tag to [Style]s that should be applied.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ColorScheme {
    pub bg: Color,
    pub fg: Color,
    pub bar_bg: Color,
    pub signcol_fg: Color,
    pub minibuffer_hl: Color,
    pub syntax: HashMap<String, Vec<Style>>,
}

impl Default for ColorScheme {
    fn default() -> Self {
        let bg: Color = "#1B1720".try_into().unwrap();
        let fg: Color = "#E6D29E".try_into().unwrap();
        let dot_bg: Color = "#336677".try_into().unwrap();
        let load_bg: Color = "#957FB8".try_into().unwrap();
        let exec_bg: Color = "#Bf616A".try_into().unwrap();
        let comment: Color = "#624354".try_into().unwrap();
        let constant: Color = "#FF9E3B".try_into().unwrap();
        let type_: Color = "#7E9CD8".try_into().unwrap();
        let function: Color = "#957FB8".try_into().unwrap();
        let module: Color = "#2D4F67".try_into().unwrap();
        let keyword: Color = "#Bf616A".try_into().unwrap();
        let punctuation: Color = "#DCA561".try_into().unwrap();
        let string: Color = "#61DCA5".try_into().unwrap();

        let syntax = [
            (TK_DEFAULT, vec![Style::Bg(bg), Style::Fg(fg)]),
            (TK_DOT, vec![Style::Fg(fg), Style::Bg(dot_bg)]),
            (TK_LOAD, vec![Style::Fg(fg), Style::Bg(load_bg)]),
            (TK_EXEC, vec![Style::Fg(fg), Style::Bg(exec_bg)]),
            ("character", vec![Style::Bold, Style::Fg(string)]),
            ("comment", vec![Style::Italic, Style::Fg(comment)]),
            ("constant", vec![Style::Fg(constant)]),
            ("function", vec![Style::Fg(function)]),
            ("string", vec![Style::Fg(string)]),
            ("type", vec![Style::Fg(type_)]),
            ("variable", vec![Style::Fg(load_bg)]),
            ("keyword", vec![Style::Fg(keyword)]),
            ("module", vec![Style::Fg(module)]),
            ("punctuation", vec![Style::Fg(punctuation)]),
        ]
        .map(|(s, v)| (s.to_string(), v))
        .into_iter()
        .collect();

        Self {
            bg,
            fg,
            bar_bg: "#4E415C".try_into().unwrap(),
            signcol_fg: "#544863".try_into().unwrap(),
            minibuffer_hl: "#3E3549".try_into().unwrap(),
            syntax,
        }
    }
}

impl ColorScheme {
    /// Determine UI [Style]s to be applied for a given syntax tag.
    ///
    /// If the full tag does not have associated styling but its dotted prefix does (e.g.
    /// "function.macro" -> "function") then the styling of the prefix is used. Otherwise default
    /// styling will be used ([TK_DEFAULT]).
    pub fn styles_for(&self, tag: &str) -> &[Style] {
        match self.syntax.get(tag) {
            Some(styles) => styles,
            None => tag
                .split_once('.')
                .and_then(|(prefix, _)| self.syntax.get(prefix))
                .or(self.syntax.get(TK_DEFAULT))
                .expect("to have default styles"),
        }
    }
}

/// Config for determining which Tree-Sitter language parser should be used for a given file
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TsConfig {
    pub(crate) lang: String,
    pub(crate) extensions: Vec<String>,
    pub(crate) first_lines: Vec<String>,
}

/// Editor level configuration
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub tabstop: usize,
    pub expand_tab: bool,
    pub auto_mount: bool,
    pub match_indent: bool,
    pub status_timeout: u64,
    pub double_click_ms: u128,
    pub minibuffer_lines: usize,
    pub find_command: String,
    pub colorscheme: ColorScheme,
    pub bindings: Trie<Input, String>,
    pub ts_config: Vec<TsConfig>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            tabstop: 4,
            expand_tab: true,
            auto_mount: false,
            match_indent: true,
            status_timeout: 3,
            double_click_ms: 200,
            minibuffer_lines: 8,
            find_command: "fd -t f".to_string(),
            colorscheme: ColorScheme::default(),
            bindings: Trie::from_pairs(Vec::new()).unwrap(),
            ts_config: vec![TsConfig {
                lang: "rust".to_owned(),
                extensions: vec!["rs".to_owned()],
                first_lines: Vec::new(),
            }],
        }
    }
}

impl Config {
    /// Attempt to load a config file from the default location
    pub fn try_load() -> Result<Self, String> {
        let home = env::var("HOME").unwrap();

        let s = match fs::read_to_string(format!("{home}/.ad/init.conf")) {
            Ok(s) => s,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(Config::default()),
            Err(e) => return Err(format!("Unable to load config file: {e}")),
        };

        match Config::parse(&s) {
            Ok(cfg) => Ok(cfg),
            Err(e) => Err(format!("Invalid config file: {e}")),
        }
    }

    /// Check to see if there is a known tree-sitter configuration for this buffer
    pub fn ts_lang_for_buffer(&self, b: &Buffer) -> Option<&str> {
        let os_ext = b.path()?.extension()?;
        let ext = os_ext.to_str()?;
        let first_line = b.line(0).map(|l| l.to_string()).unwrap_or_default();

        self.ts_config
            .iter()
            .find(|c| {
                c.extensions.iter().any(|e| e == ext)
                    || c.first_lines.iter().any(|l| first_line.starts_with(l))
            })
            .map(|c| c.lang.as_str())
    }

    /// Attempt to parse the given file content as a Config file. If the file is invalid then an
    /// error message for the user is returned for displaying in the status bar.
    pub fn parse(contents: &str) -> Result<Self, String> {
        let mut cfg = Config::default();
        cfg.update_from(contents)?;

        Ok(cfg)
    }

    pub(crate) fn update_from(&mut self, input: &str) -> Result<(), String> {
        let mut raw_bindings = Vec::new();

        for line in input.lines() {
            let line = line.trim_end();
            if line.starts_with('#') || line.is_empty() {
                continue;
            }

            match line.strip_prefix("set ") {
                Some(line) => self.try_set_prop(line)?,
                None => match line.strip_prefix("map ") {
                    Some(line) => raw_bindings.push(try_parse_binding(line)?),
                    None => {
                        return Err(format!(
                            "'{line}' should be 'set prop=val' or 'map ... => prog'"
                        ))
                    }
                },
            }
        }

        if !raw_bindings.is_empty() {
            // Make sure that none of the user provided bindings clash with Normal mode
            // bindings as that will mean they never get run
            let nm = normal_mode();
            for (keys, _) in raw_bindings.iter() {
                if nm.keymap.contains_key_or_prefix(keys) {
                    let mut s = String::new();
                    for k in keys {
                        if let Input::Char(c) = k {
                            s.push(*c);
                        }
                    }

                    return Err(format!("mapping '{s}' collides with a Normal mode mapping"));
                }
            }
            let mut bindings = self.bindings.clone();
            bindings
                .extend_from_pairs(raw_bindings)
                .map_err(|s| s.to_owned())?;
            self.bindings = bindings;
        }

        Ok(())
    }

    pub(crate) fn try_set_prop(&mut self, input: &str) -> Result<(), String> {
        let (prop, val) = input
            .split_once('=')
            .ok_or_else(|| format!("'{input}' is not a 'set prop=val' statement"))?;

        match prop {
            // Strings
            "find-command" => self.find_command = val.trim().to_string(),

            // Numbers
            "tabstop" => self.tabstop = parse_usize(prop, val)?,
            "minibuffer-lines" => self.minibuffer_lines = parse_usize(prop, val)?,
            "status-timeout" => self.status_timeout = parse_usize(prop, val)? as u64,
            "double-click-ms" => self.double_click_ms = parse_usize(prop, val)? as u128,

            // Flags
            "expand-tab" => self.expand_tab = parse_bool(prop, val)?,
            "auto-mount" => self.auto_mount = parse_bool(prop, val)?,
            "match-indent" => self.match_indent = parse_bool(prop, val)?,

            // Colors
            // !! Ignored for now so that parsing config still works while things move to the new format
            "bg-color" => (),
            "fg-color" => (),
            "dot-bg-color" => (),
            "load-bg-color" => (),
            "exec-bg-color" => (),
            "bar-bg-color" => (),
            "signcol-fg-color" => (),
            "minibuffer-hl-color" => (),
            "comment-color" => (),
            "keyword-color" => (),
            "control-flow-color" => (),
            "definition-color" => (),
            "punctuation-color" => (),
            "string-color" => (),

            _ => return Err(format!("'{prop}' is not a known config property")),
        }

        Ok(())
    }
}

fn parse_usize(prop: &str, val: &str) -> Result<usize, String> {
    match val.parse() {
        Ok(num) => Ok(num),
        Err(_) => Err(format!("expected number for '{prop}' but found '{val}'")),
    }
}

fn parse_bool(prop: &str, val: &str) -> Result<bool, String> {
    match val {
        "true" => Ok(true),
        "false" => Ok(false),
        _ => Err(format!(
            "expected true/false for '{prop}' but found '{val}'"
        )),
    }
}

fn try_parse_binding(input: &str) -> Result<(Vec<Input>, String), String> {
    let (keys, prog) = input
        .split_once("=>")
        .ok_or_else(|| format!("'{input}' is not a 'map ... => prog' statement"))?;

    let keys: Vec<Input> = keys
        .split_whitespace()
        .filter_map(|s| {
            if s.len() == 1 {
                let c = s.chars().next().unwrap();
                if c.is_whitespace() {
                    None
                } else {
                    Some(Input::Char(c))
                }
            } else {
                match s {
                    "<space>" => Some(Input::Char(' ')),
                    _ => None,
                }
            }
        })
        .collect();

    Ok((keys, prog.trim().to_string()))
}

#[cfg(test)]
mod tests {
    use super::*;

    const EXAMPLE_CONFIG: &str = include_str!("../data/init.conf");
    const CUSTOM_CONFIG: &str = "
# This is a comment


# Blank lines should be skipped
set tabstop=7

set expand-tab=false
set match-indent=false

map G G => my-prog
";

    // This should be our default so we are just verifying that we have not diverged from
    // what is in the repo.
    #[test]
    fn parse_of_example_config_works() {
        let cfg = Config::parse(EXAMPLE_CONFIG).unwrap();
        let bindings = Trie::from_pairs(vec![
            (vec![Input::Char(' '), Input::Char('F')], "fmt".to_string()),
            (vec![Input::Char('>')], "indent".to_string()),
            (vec![Input::Char('<')], "unindent".to_string()),
        ])
        .unwrap();

        let expected = Config {
            bindings,
            ..Default::default()
        };

        assert_eq!(cfg, expected);
    }

    #[test]
    fn custom_vals_work() {
        let cfg = Config::parse(CUSTOM_CONFIG).unwrap();

        let expected = Config {
            tabstop: 7,
            expand_tab: false,
            match_indent: false,
            bindings: Trie::from_pairs(vec![(
                vec![Input::Char('G'), Input::Char('G')],
                "my-prog".to_string(),
            )])
            .unwrap(),
            ..Default::default()
        };

        assert_eq!(cfg, expected);
    }
}
