//! End-to-end tests for the vim state machine. Each test drives
//! `handle_key` against an `EditorState`, applies the returned
//! `TransactionSpec`, and asserts on the resulting doc + caret.

use editor_state::{EditorState, KeySpec, Range, Selection};
use editor_vim::{handle_key, VimState};

fn k(ch: &str) -> KeySpec {
    KeySpec {
        key: ch.to_string(),
        ..Default::default()
    }
}

fn state_with_caret(text: &str, caret: usize) -> EditorState {
    let mut s = EditorState::new(text);
    s.selection = Selection::caret(caret);
    s
}

fn drive(state: EditorState, vim: &mut VimState, keys: &[&str]) -> EditorState {
    let mut s = state;
    for key in keys {
        let key = k(key);
        if let Some(spec) = handle_key(&s, vim, &key) {
            s = s.update(spec);
        }
    }
    s
}

#[test]
fn hjkl_basic_movement() {
    let mut vim = VimState::new();
    let s = state_with_caret("abc\ndef", 0);
    let s = drive(s, &mut vim, &["l", "l"]);
    assert_eq!(s.selection.primary(), Range::caret(2));
    let s = drive(s, &mut vim, &["j"]);
    assert_eq!(s.selection.primary().head, 6); // "abc\nde[f]"
    let s = drive(s, &mut vim, &["h"]);
    assert_eq!(s.selection.primary().head, 5);
    let s = drive(s, &mut vim, &["k"]);
    assert_eq!(s.selection.primary().head, 1);
}

#[test]
fn hjkl_stays_in_bounds() {
    let mut vim = VimState::new();
    let s = state_with_caret("abc\ndef", 0);
    // many h's at start stay put
    let s = drive(s, &mut vim, &["h", "h", "h"]);
    assert_eq!(s.selection.primary().head, 0);
    // k at top stays put
    let s = drive(s, &mut vim, &["k"]);
    assert_eq!(s.selection.primary().head, 0);
}

#[test]
fn w_advances_to_next_word() {
    let mut vim = VimState::new();
    let s = state_with_caret("foo bar baz", 0);
    let s = drive(s, &mut vim, &["w"]);
    assert_eq!(s.selection.primary().head, 4);
}

#[test]
fn dw_deletes_word() {
    let mut vim = VimState::new();
    let s = state_with_caret("foo bar", 0);
    let s = drive(s, &mut vim, &["d", "w"]);
    assert_eq!(s.doc.to_string(), "bar");
    assert_eq!(s.selection.primary().head, 0);
}

#[test]
fn daw_deletes_a_word() {
    let mut vim = VimState::new();
    let s = state_with_caret(" foo ", 2);
    let s = drive(s, &mut vim, &["d", "a", "w"]);
    assert_eq!(s.doc.to_string(), "");
}

#[test]
fn yyp_duplicates_line() {
    let mut vim = VimState::new();
    let s = state_with_caret("line", 0);
    let s = drive(s, &mut vim, &["y", "y", "p"]);
    assert_eq!(s.doc.to_string(), "line\nline");
}

#[test]
fn count_three_w() {
    let mut vim = VimState::new();
    let s = state_with_caret("a b c d", 0);
    let s = drive(s, &mut vim, &["3", "w"]);
    assert_eq!(s.selection.primary().head, 6);
}

#[test]
fn i_enters_insert_mode() {
    let mut vim = VimState::new();
    let s = state_with_caret("abc", 1);
    let _ = drive(s, &mut vim, &["i"]);
    assert_eq!(vim.mode, editor_vim::Mode::Insert);
}

#[test]
fn escape_returns_to_normal() {
    let mut vim = VimState::new();
    vim.mode = editor_vim::Mode::Insert;
    let s = state_with_caret("abc", 1);
    let _ = drive(s, &mut vim, &["Escape"]);
    assert_eq!(vim.mode, editor_vim::Mode::Normal);
}

#[test]
fn x_deletes_char_under_caret() {
    let mut vim = VimState::new();
    let s = state_with_caret("abc", 1);
    let s = drive(s, &mut vim, &["x"]);
    assert_eq!(s.doc.to_string(), "ac");
    assert_eq!(s.selection.primary().head, 1);
}

#[test]
fn dollar_moves_to_line_end() {
    let mut vim = VimState::new();
    let s = state_with_caret("hello\nworld", 0);
    let s = drive(s, &mut vim, &["$"]);
    assert_eq!(s.selection.primary().head, 5);
}

#[test]
fn zero_moves_to_line_start() {
    let mut vim = VimState::new();
    let s = state_with_caret("hello", 3);
    let s = drive(s, &mut vim, &["0"]);
    assert_eq!(s.selection.primary().head, 0);
}
