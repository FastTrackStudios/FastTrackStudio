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
fn big_w_advances_past_punctuation() {
    // `foo.bar baz` — `foo.bar` is one WORD, so W from 0 lands
    // at the start of `baz` (byte 8), not at the `.`.
    let mut vim = VimState::new();
    let s = state_with_caret("foo.bar baz", 0);
    let s = drive(s, &mut vim, &["W"]);
    assert_eq!(s.selection.primary().head, 8);
}

#[test]
fn big_b_walks_back_past_punctuation() {
    let mut vim = VimState::new();
    let s = state_with_caret("foo.bar baz", 11);
    let s = drive(s, &mut vim, &["B"]);
    assert_eq!(s.selection.primary().head, 8);
}

#[test]
fn big_e_lands_on_last_byte_of_word() {
    // From pos 0 in `foo.bar baz`, E lands on byte 6 (the `r`).
    let mut vim = VimState::new();
    let s = state_with_caret("foo.bar baz", 0);
    let s = drive(s, &mut vim, &["E"]);
    assert_eq!(s.selection.primary().head, 6);
}

#[test]
fn count_three_big_w() {
    // `a.b c.d e.f g` — three WORDs forward from 0 → start of `g`.
    let mut vim = VimState::new();
    let s = state_with_caret("a.b c.d e.f g", 0);
    let s = drive(s, &mut vim, &["3", "W"]);
    assert_eq!(s.selection.primary().head, 12);
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

#[test]
fn capital_d_deletes_to_eol() {
    let mut vim = VimState::new();
    let s = state_with_caret("hello world\nnext", 6);
    let s = drive(s, &mut vim, &["D"]);
    assert_eq!(s.doc.to_string(), "hello \nnext");
    assert_eq!(s.selection.primary().head, 6);
}

#[test]
fn capital_c_changes_to_eol_and_enters_insert() {
    let mut vim = VimState::new();
    let s = state_with_caret("hello world", 6);
    let s = drive(s, &mut vim, &["C"]);
    assert_eq!(s.doc.to_string(), "hello ");
    assert_eq!(vim.mode, editor_vim::Mode::Insert);
}

#[test]
fn capital_y_yanks_line() {
    let mut vim = VimState::new();
    let s = state_with_caret("first\nsecond", 0);
    let s = drive(s, &mut vim, &["Y", "j", "p"]);
    // After Y (line yank) → j → p (paste after) on line "second",
    // doc becomes "first\nsecond\nfirst".
    assert_eq!(s.doc.to_string(), "first\nsecond\nfirst");
}

#[test]
fn gg_jumps_to_first_line() {
    let mut vim = VimState::new();
    let s = state_with_caret("first\nsecond\nthird", 14);
    let s = drive(s, &mut vim, &["g", "g"]);
    assert_eq!(s.selection.primary().head, 0);
}

#[test]
fn gu_motion_lowercases_word() {
    let mut vim = VimState::new();
    let s = state_with_caret("HELLO world", 0);
    let s = drive(s, &mut vim, &["g", "u", "w"]);
    assert_eq!(s.doc.to_string(), "hello world");
}

#[test]
fn gcap_u_motion_uppercases_word() {
    let mut vim = VimState::new();
    let s = state_with_caret("hello world", 0);
    let s = drive(s, &mut vim, &["g", "U", "w"]);
    assert_eq!(s.doc.to_string(), "HELLO world");
}

#[test]
fn g_tilde_doubled_toggles_case_of_line() {
    let mut vim = VimState::new();
    let s = state_with_caret("Hello World", 0);
    let s = drive(s, &mut vim, &["g", "~", "~"]);
    assert_eq!(s.doc.to_string(), "hELLO wORLD");
}

#[test]
fn star_jumps_to_next_occurrence_of_word_under_caret() {
    let mut vim = VimState::new();
    // Caret at 0 ("f" of first "foo"). `*` should jump to the
    // start of the SECOND "foo".
    let s = state_with_caret("foo bar foo baz", 0);
    let s = drive(s, &mut vim, &["*"]);
    assert_eq!(s.selection.primary().head, 8);
}

#[test]
fn hash_jumps_to_previous_occurrence() {
    let mut vim = VimState::new();
    let s = state_with_caret("foo bar foo baz", 8);
    let s = drive(s, &mut vim, &["#"]);
    assert_eq!(s.selection.primary().head, 0);
}

#[test]
fn n_repeats_last_search_forward() {
    let mut vim = VimState::new();
    // `foo` appears 3 times. `*` jumps to occurrence 2; `n`
    // jumps to occurrence 3.
    let s = state_with_caret("foo bar foo baz foo end", 0);
    let s = drive(s, &mut vim, &["*", "n"]);
    assert_eq!(s.selection.primary().head, 16);
}

#[test]
fn capital_n_reverses_search_direction() {
    let mut vim = VimState::new();
    let s = state_with_caret("foo bar foo baz foo end", 0);
    // `*` → occurrence 2 (pos 8). `N` reverses → back to 0.
    let s = drive(s, &mut vim, &["*", "N"]);
    assert_eq!(s.selection.primary().head, 0);
}

#[test]
fn star_requires_whole_word_match() {
    let mut vim = VimState::new();
    // Caret on `foo` — `*` must skip `foobar` and land on the
    // standalone `foo`.
    let s = state_with_caret("foo foobar baz foo end", 0);
    let s = drive(s, &mut vim, &["*"]);
    assert_eq!(s.selection.primary().head, 15);
}
