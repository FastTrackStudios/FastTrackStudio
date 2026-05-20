//! Built-in commands. These are intentionally tiny —
//! `fn(&EditorState) -> Option<TransactionSpec>` — so they're
//! testable in isolation and composable into any keymap.
//!
//! Mirrors `@codemirror/commands`. We add commands here as we
//! find we want them in the default keymap.

use crate::change::Changes;
use crate::selection::{Range, Selection};
use crate::state::EditorState;
use crate::transaction::TransactionSpec;

/// Select the entire document. Bound by convention to `Mod-a`.
pub fn select_all(state: &EditorState) -> Option<TransactionSpec> {
    Some(TransactionSpec::new().selection(Selection::single(Range::new(0, state.doc.len()))))
}

/// Insert a newline at the caret. If there's a non-empty
/// selection, replace it with `"\n"`. Bound by convention to
/// `Enter`.
pub fn insert_newline(state: &EditorState) -> Option<TransactionSpec> {
    let p = state.selection.primary();
    let (from, to) = (p.from(), p.to());
    Some(TransactionSpec::new().changes(Changes::replace(from..to, "\n")))
}

/// Delete the character before the caret. With a non-empty
/// selection, deletes the selection. Bound by convention to
/// `Backspace`.
pub fn delete_backward(state: &EditorState) -> Option<TransactionSpec> {
    let p = state.selection.primary();
    let (from, to) = (p.from(), p.to());
    if from != to {
        return Some(TransactionSpec::new().changes(Changes::delete(from..to)));
    }
    if from == 0 {
        return None;
    }
    // For now we step one byte. A future commit will step by
    // grapheme cluster so we don't split multi-byte chars.
    Some(TransactionSpec::new().changes(Changes::delete(from - 1..from)))
}

/// Delete the character after the caret. With a non-empty
/// selection, deletes the selection. Bound by convention to
/// `Delete`.
pub fn delete_forward(state: &EditorState) -> Option<TransactionSpec> {
    let p = state.selection.primary();
    let (from, to) = (p.from(), p.to());
    if from != to {
        return Some(TransactionSpec::new().changes(Changes::delete(from..to)));
    }
    if to >= state.doc.len() {
        return None;
    }
    Some(TransactionSpec::new().changes(Changes::delete(to..to + 1)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn select_all_covers_doc() {
        let s = EditorState::new("hello");
        let spec = select_all(&s).unwrap();
        let next = s.update(spec);
        let p = next.selection.primary();
        assert_eq!(p.from(), 0);
        assert_eq!(p.to(), 5);
    }

    #[test]
    fn delete_backward_at_pos_5() {
        let mut s = EditorState::new("hello");
        s.selection = Selection::caret(5);
        let next = s.update(delete_backward(&s).unwrap());
        assert_eq!(next.doc.to_string(), "hell");
        assert_eq!(next.selection.primary().head, 4);
    }

    #[test]
    fn delete_backward_at_start_is_noop() {
        let mut s = EditorState::new("hello");
        s.selection = Selection::caret(0);
        assert!(delete_backward(&s).is_none());
    }

    #[test]
    fn delete_backward_with_selection_deletes_range() {
        let mut s = EditorState::new("hello");
        s.selection = Selection::single(Range::new(1, 4));
        let next = s.update(delete_backward(&s).unwrap());
        assert_eq!(next.doc.to_string(), "ho");
    }
}
