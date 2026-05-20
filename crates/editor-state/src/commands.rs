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

/// Toggle bold markdown markers (`**…**`) at the caret /
/// around the current selection. Behavior:
///
/// - **Empty caret, doc[caret..] starts with `**`**: caret is
///   sitting just before a closing marker (typical "I'm done
///   typing bold content" case). Skip past it — no doc change,
///   just move the caret +2.
/// - **Empty caret elsewhere**: insert `****` and park the
///   caret between the markers, so subsequent typing goes
///   inside the bold span.
/// - **Non-empty selection**: wrap the selection with `**…**`,
///   keeping the wrapped range selected.
///
/// Bound by convention to `Mod-b`.
pub fn toggle_bold(state: &EditorState) -> Option<TransactionSpec> {
    toggle_marker(state, "**")
}

/// Same as [`toggle_bold`] but with single `*…*` for italic.
/// Bound to `Mod-i`.
pub fn toggle_italic(state: &EditorState) -> Option<TransactionSpec> {
    toggle_marker(state, "*")
}

fn toggle_marker(state: &EditorState, marker: &str) -> Option<TransactionSpec> {
    let sel = state.selection.primary();
    let from = sel.from();
    let to = sel.to();
    let doc = state.doc.to_string();
    let m = marker;
    let mlen = m.len();

    if from == to {
        // Empty caret. If the next bytes are the marker, skip
        // past it — closes an open span the user just filled.
        if doc.get(from..).map_or(false, |s| s.starts_with(m)) {
            return Some(TransactionSpec::new().selection(Selection::caret(from + mlen)));
        }
        // Open a new span: insert "marker + marker" with caret
        // in the middle.
        let pair = format!("{m}{m}");
        return Some(
            TransactionSpec::new()
                .changes(Changes::insert(from, pair))
                .selection(Selection::caret(from + mlen)),
        );
    }
    // Wrap the selection.
    let selected = doc.get(from..to).unwrap_or("");
    let wrapped = format!("{m}{selected}{m}");
    let new_to = from + wrapped.len();
    Some(
        TransactionSpec::new()
            .changes(Changes::replace(from..to, wrapped))
            .selection(Selection::single(Range::new(from, new_to))),
    )
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

    #[test]
    fn toggle_bold_with_empty_caret_inserts_pair() {
        let mut s = EditorState::new("Testing ");
        s.selection = Selection::caret(8);
        let next = s.update(toggle_bold(&s).unwrap());
        assert_eq!(next.doc.to_string(), "Testing ****");
        // Caret parked between the markers.
        assert_eq!(next.selection.primary().head, 10);
        assert_eq!(next.selection.primary().anchor, 10);
    }

    #[test]
    fn toggle_bold_skips_past_closing_marker() {
        // "Testing **bold**" with caret at 14 (just after
        // "bold", before closing "**"). Pressing toggle_bold
        // should move caret to 16 without changing doc.
        let mut s = EditorState::new("Testing **bold**");
        s.selection = Selection::caret(14);
        let next = s.update(toggle_bold(&s).unwrap());
        assert_eq!(next.doc.to_string(), "Testing **bold**"); // unchanged
        assert_eq!(next.selection.primary().head, 16);
    }

    #[test]
    fn toggle_bold_wraps_selection() {
        let mut s = EditorState::new("Make this bold");
        s.selection = Selection::single(Range::new(5, 9)); // "this"
        let next = s.update(toggle_bold(&s).unwrap());
        assert_eq!(next.doc.to_string(), "Make **this** bold");
        let p = next.selection.primary();
        assert_eq!(p.from(), 5);
        assert_eq!(p.to(), 13); // covers **this**
    }

    #[test]
    fn toggle_italic_uses_single_marker() {
        let mut s = EditorState::new("foo");
        s.selection = Selection::caret(3);
        let next = s.update(toggle_italic(&s).unwrap());
        assert_eq!(next.doc.to_string(), "foo**");
        assert_eq!(next.selection.primary().head, 4);
    }
}
