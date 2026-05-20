//! Transactions — the only way to mutate an [`EditorState`].
//!
//! A transaction describes *what* should change (text edits and
//! optionally a new selection) and is then applied to produce a
//! new state. Mirrors `@codemirror/state`'s `Transaction` /
//! `TransactionSpec`. See `~/Development/research/codemirror/state/src/transaction.ts`.

use crate::change::Changes;
use crate::selection::Selection;
use crate::state::EditorState;

/// Builder-style description of a transaction. Callers fill in
/// the fields they care about and leave the rest at default.
#[derive(Clone, Debug, Default)]
pub struct TransactionSpec {
    /// Edits to apply to the document.
    pub changes: Changes,
    /// New selection. If `None`, the previous selection is
    /// **mapped through the changes** and used.
    pub selection: Option<Selection>,
    /// Hint that this transaction came from user input that
    /// should be debounced into the history's last change set
    /// (typing). Not used yet.
    pub user_event: Option<String>,
    /// Free-form scoped metadata. Extensions read this to
    /// distinguish their own transactions. Stored as JSON-ish
    /// strings for now; an `Annotation<T>` type can come later.
    pub annotations: Vec<(String, String)>,
}

impl TransactionSpec {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn changes(mut self, changes: Changes) -> Self {
        self.changes = changes;
        self
    }

    pub fn selection(mut self, sel: Selection) -> Self {
        self.selection = Some(sel);
        self
    }

    pub fn user_event(mut self, name: impl Into<String>) -> Self {
        self.user_event = Some(name.into());
        self
    }

    pub fn annotate(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.annotations.push((key.into(), value.into()));
        self
    }
}

/// A transaction in flight: the spec plus the `before` state
/// pointer. Apply with `Transaction::apply` to get the new
/// state.
#[derive(Clone, Debug)]
pub struct Transaction {
    pub before: EditorState,
    pub spec: TransactionSpec,
}

impl Transaction {
    pub fn new(before: EditorState, spec: TransactionSpec) -> Self {
        Self { before, spec }
    }

    /// Produce the resulting `EditorState`. Pure — does not
    /// mutate `self` or `self.before`.
    pub fn apply(&self) -> EditorState {
        let new_doc = self.spec.changes.apply(&self.before.doc);
        let new_selection = self
            .spec
            .selection
            .clone()
            .unwrap_or_else(|| self.before.selection.map(&self.spec.changes));
        EditorState {
            doc: new_doc,
            selection: new_selection,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::doc::Doc;
    use crate::selection::{Range, Selection};

    #[test]
    fn insert_advances_caret() {
        let state = EditorState {
            doc: Doc::from_str("hello"),
            selection: Selection::caret(5),
        };
        let tr = Transaction::new(
            state,
            TransactionSpec::new().changes(Changes::insert(5, " world")),
        );
        let next = tr.apply();
        assert_eq!(next.doc.to_string(), "hello world");
        // Caret was at end, follows the insertion (After bias).
        assert_eq!(next.selection.primary(), Range::caret(11));
    }

    #[test]
    fn explicit_selection_overrides_mapping() {
        let state = EditorState {
            doc: Doc::from_str("hello"),
            selection: Selection::caret(5),
        };
        let tr = Transaction::new(
            state,
            TransactionSpec::new()
                .changes(Changes::insert(0, "X"))
                .selection(Selection::caret(0)),
        );
        let next = tr.apply();
        assert_eq!(next.doc.to_string(), "Xhello");
        assert_eq!(next.selection.primary(), Range::caret(0));
    }
}
