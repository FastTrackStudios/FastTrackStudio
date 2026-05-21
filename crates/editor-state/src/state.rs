//! The immutable editor state. Holds the current document and
//! selection. Transactions consume a state and produce a new
//! one.
//!
//! Mirrors `@codemirror/state`'s `EditorState`. We deliberately
//! keep it small in v1; facets / extensions / history land in
//! follow-up commits.

use crate::doc::Doc;
use crate::selection::Selection;
use crate::transaction::{Transaction, TransactionSpec};

/// A snapshot of everything the editor knows: text + caret(s) +
/// folded ranges. Cheap to clone (`Doc` is a rope; `Selection`
/// is a small Vec; folds are a small Vec).
#[derive(Clone, Debug)]
pub struct EditorState {
    pub doc: Doc,
    pub selection: Selection,
    /// Currently folded byte ranges (sorted, non-overlapping).
    /// The decoration source emits Replace + a "…" widget on
    /// each range; transactions map them through the change set
    /// so they stay anchored as the doc edits around them.
    pub folds: Vec<std::ops::Range<usize>>,
}

impl EditorState {
    /// Create a fresh state with the given initial text and
    /// caret at offset 0.
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            doc: Doc::from_str(&text.into()),
            selection: Selection::caret(0),
            folds: Vec::new(),
        }
    }

    /// Convenience: wrap a [`TransactionSpec`] and apply it.
    /// Returns the resulting state.
    pub fn update(&self, spec: TransactionSpec) -> EditorState {
        Transaction::new(self.clone(), spec).apply()
    }
}

impl Default for EditorState {
    fn default() -> Self {
        Self::new("")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::change::Changes;
    use crate::selection::Range;

    #[test]
    fn new_state_has_empty_doc_and_caret_zero() {
        let s = EditorState::new("");
        assert_eq!(s.doc.len(), 0);
        assert_eq!(s.selection.primary(), Range::caret(0));
    }

    #[test]
    fn update_returns_new_state() {
        let s = EditorState::new("hello");
        let next = s.update(TransactionSpec::new().changes(Changes::insert(5, "!")));
        assert_eq!(next.doc.to_string(), "hello!");
        // Original unchanged.
        assert_eq!(s.doc.to_string(), "hello");
    }
}
