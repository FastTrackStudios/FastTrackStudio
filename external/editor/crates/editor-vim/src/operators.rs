//! Operators: `d`, `c`, `y` and friends. An operator takes a
//! byte range (built from a motion or text object) and returns a
//! [`TransactionSpec`] that performs the edit + updates the
//! relevant register.
//!
//! vim ref: codemirror-vim/src/vim.js (`operators` table)
//! vim ref: zed/crates/vim/src/normal/delete.rs

use editor_state::{Changes, EditorState, Selection, TransactionSpec};

use crate::state::VimState;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator {
    Delete,  // d
    Change,  // c
    Yank,    // y
    Indent,  // >
    Outdent, // <
}

impl Operator {
    pub fn from_char(ch: char) -> Option<Self> {
        Some(match ch {
            'd' => Self::Delete,
            'c' => Self::Change,
            'y' => Self::Yank,
            '>' => Self::Indent,
            '<' => Self::Outdent,
            _ => return None,
        })
    }
}

/// Apply an operator to a half-open byte range `[from, to)`.
/// Charwise — does **not** snap to line bounds.
pub fn apply_range(
    state: &EditorState,
    vim: &mut VimState,
    op: Operator,
    from: usize,
    to: usize,
) -> TransactionSpec {
    let (lo, hi) = (from.min(to), from.max(to));
    let text = if hi > lo {
        state.doc.slice(lo..hi)
    } else {
        String::new()
    };
    let reg = vim.pending_register.take();
    vim.registers.write(reg, &text);
    vim.clear_pending();
    match op {
        Operator::Yank => {
            // No edit, caret returns to start of range.
            TransactionSpec::new().selection(Selection::caret(lo))
        }
        Operator::Delete => TransactionSpec::new()
            .changes(Changes::delete(lo..hi))
            .selection(Selection::caret(lo)),
        Operator::Change => {
            vim.mode = crate::state::Mode::Insert;
            TransactionSpec::new()
                .changes(Changes::delete(lo..hi))
                .selection(Selection::caret(lo))
        }
        Operator::Indent => indent_range(state, lo, hi, /*outdent=*/ false),
        Operator::Outdent => indent_range(state, lo, hi, /*outdent=*/ true),
    }
}

/// Linewise operator. The dispatcher passes a range that already
/// includes the trailing newline (when one exists). For yank/del
/// the register payload is suffixed with `\n` so paste reinserts
/// the line correctly.
pub fn apply_linewise(
    state: &EditorState,
    vim: &mut VimState,
    op: Operator,
    from: usize,
    to: usize,
) -> TransactionSpec {
    let (lo, hi) = (from.min(to), from.max(to));
    let mut text = if hi > lo {
        state.doc.slice(lo..hi)
    } else {
        String::new()
    };
    if !text.ends_with('\n') {
        text.push('\n');
    }
    let reg = vim.pending_register.take();
    vim.registers.write(reg, &text);
    vim.clear_pending();
    match op {
        Operator::Yank => TransactionSpec::new().selection(Selection::caret(lo)),
        Operator::Delete => TransactionSpec::new()
            .changes(Changes::delete(lo..hi))
            .selection(Selection::caret(lo)),
        Operator::Change => {
            vim.mode = crate::state::Mode::Insert;
            TransactionSpec::new()
                .changes(Changes::delete(lo..hi))
                .selection(Selection::caret(lo))
        }
        Operator::Indent => indent_range(state, lo, hi, false),
        Operator::Outdent => indent_range(state, lo, hi, true),
    }
}

fn indent_range(state: &EditorState, lo: usize, hi: usize, outdent: bool) -> TransactionSpec {
    // Walk the lines in [lo, hi]; for each, either prepend "    "
    // or strip up to four leading spaces / one tab.
    let s = state.doc.to_string();
    let bytes = s.as_bytes();
    let mut changes = Vec::new();
    let mut p = crate::motions::line_start(state, lo);
    while p <= hi && p < bytes.len() {
        if outdent {
            let mut strip = 0;
            for i in 0..4 {
                if p + i < bytes.len() && bytes[p + i] == b' ' {
                    strip += 1;
                } else if i == 0 && p < bytes.len() && bytes[p] == b'\t' {
                    strip = 1;
                    break;
                } else {
                    break;
                }
            }
            if strip > 0 {
                changes.push(editor_state::Change::delete(p..p + strip));
            }
        } else {
            changes.push(editor_state::Change::insert(p, "    "));
        }
        // jump to next line
        match bytes[p..].iter().position(|&b| b == b'\n') {
            Some(i) => p += i + 1,
            None => break,
        }
    }
    TransactionSpec::new()
        .changes(Changes::from_sorted(changes))
        .selection(Selection::caret(lo))
}
