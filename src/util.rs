//! Utility functions
use crate::{config::config_path, editor::built_in_commands};
use std::{
    iter::Peekable,
    path::Path,
    str::Chars,
    sync::{Arc, LockResult, RwLock, RwLockReadGuard},
};
use tracing::warn;

/// A wrapper around an Arc<RwLock<T>> so that the owner is only
/// permitted read access to the underlying value.
#[derive(Debug, Default, Clone)]
pub struct ReadOnlyLock<T>(Arc<RwLock<T>>);

impl<T> ReadOnlyLock<T> {
    /// Construct a new ReadOnlyLock wrapping an inner Arc<RwLock<T>>
    pub fn new(inner: Arc<RwLock<T>>) -> Self {
        Self(inner)
    }

    /// Obtain a read guard from the underlying RwLock
    pub fn read(&self) -> LockResult<RwLockReadGuard<'_, T>> {
        self.0.read()
    }
}

/// Pull in data from the ad crate itself to auto-generate the docs on the functionality
/// available in the editor.
pub(crate) fn gen_help_docs() -> String {
    let help_template = include_str!("../data/help-template.txt");

    help_template
        .replace("{{BUILT_IN_COMMANDS}}", &commands_section())
        .replace("{{CONFIG_PATH}}", &config_path())
}

fn commands_section() -> String {
    let commands = built_in_commands();
    let mut buf = Vec::with_capacity(commands.len());

    for (cmds, desc) in commands.into_iter() {
        buf.push((cmds.join(" | "), desc));
    }

    let w_max = buf.iter().map(|(s, _)| s.len()).max().unwrap();
    let mut s = String::new();

    for (cmds, desc) in buf.into_iter() {
        s.push_str(&format!("{:width$} -- {desc}\n", cmds, width = w_max));
    }

    s
}

// returns the parsed number and following character if there was one.
// initial must be a valid ascii digit
pub(crate) fn parse_num(initial: char, it: &mut Peekable<Chars<'_>>) -> usize {
    let mut s = String::from(initial);
    loop {
        match it.peek() {
            Some(ch) if ch.is_ascii_digit() => {
                s.push(it.next().unwrap());
            }
            _ => return s.parse().unwrap(),
        }
    }
}

pub(crate) fn normalize_line_endings(mut s: String) -> String {
    if !s.contains('\r') {
        return s;
    }

    warn!("normalizing \\r characters to \\n");
    s = s.replace("\r\n", "\n");
    s.replace("\r", "\n")
}

/// Locate the first parent directory containing a target file
pub(crate) fn parent_dir_containing<'a>(initial: &'a Path, target: &str) -> Option<&'a Path> {
    initial
        .ancestors()
        .find(|&p| p.is_dir() && p.join(target).exists())
}
