//! Yrs-backed body CRDT for markdown content.
//!
//! The markdown body (after the `---` frontmatter block) is stored
//! as a Yrs Text document, enabling concurrent editing of subtask
//! checklists, notes, and descriptions.

use yrs::{Doc, GetString, ReadTxn, Text, Transact};
use yrs::updates::decoder::Decode;
use yrs::updates::encoder::Encode;

use crate::service::VaultError;

/// Wraps a Yrs document representing the markdown body of a task.
pub struct BodyDoc {
    doc: Doc,
}

impl BodyDoc {
    /// Create a new empty body document.
    pub fn new() -> Self {
        Self { doc: Doc::new() }
    }

    /// Create a body document with initial text content.
    pub fn with_text(text: &str) -> Self {
        let doc = Doc::new();
        let body = doc.get_or_insert_text("body");
        let mut txn = doc.transact_mut();
        body.insert(&mut txn, 0, text);
        drop(txn);
        Self { doc }
    }

    /// Load from saved Yrs state vector + update.
    pub fn load(update: &[u8]) -> Result<Self, VaultError> {
        let doc = Doc::new();
        let update = yrs::Update::decode_v1(update)
            .map_err(|e| VaultError::ParseError(format!("Yrs decode: {e}")))?;
        let mut txn = doc.transact_mut();
        txn.apply_update(update)
            .map_err(|e| VaultError::ParseError(format!("Yrs apply: {e}")))?;
        drop(txn);
        Ok(Self { doc })
    }

    /// Save the full state as bytes.
    pub fn save(&self) -> Vec<u8> {
        let txn = self.doc.transact();
        txn.encode_state_as_update_v1(&yrs::StateVector::default())
    }

    /// Get the current text content.
    pub fn text(&self) -> String {
        let body = self.doc.get_or_insert_text("body");
        let txn = self.doc.transact();
        body.get_string(&txn)
    }

    /// Replace the entire text content.
    pub fn set_text(&self, text: &str) {
        let body = self.doc.get_or_insert_text("body");
        let mut txn = self.doc.transact_mut();
        let len = body.get_string(&txn).len() as u32;
        if len > 0 {
            body.remove_range(&mut txn, 0, len);
        }
        body.insert(&mut txn, 0, text);
    }

    /// Insert text at a position.
    pub fn insert(&self, index: u32, text: &str) {
        let body = self.doc.get_or_insert_text("body");
        let mut txn = self.doc.transact_mut();
        body.insert(&mut txn, index, text);
    }

    /// Delete a range of text.
    pub fn delete(&self, index: u32, length: u32) {
        let body = self.doc.get_or_insert_text("body");
        let mut txn = self.doc.transact_mut();
        body.remove_range(&mut txn, index, length);
    }

    /// Encode state for sync (incremental update).
    pub fn encode_update(&self, state_vector: &[u8]) -> Result<Vec<u8>, VaultError> {
        let sv = yrs::StateVector::decode_v1(state_vector)
            .map_err(|e| VaultError::ParseError(format!("Yrs SV decode: {e}")))?;
        let txn = self.doc.transact();
        Ok(txn.encode_state_as_update_v1(&sv))
    }

    /// Apply a remote update.
    pub fn apply_update(&self, update: &[u8]) -> Result<(), VaultError> {
        let update = yrs::Update::decode_v1(update)
            .map_err(|e| VaultError::ParseError(format!("Yrs decode: {e}")))?;
        let mut txn = self.doc.transact_mut();
        txn.apply_update(update)
            .map_err(|e| VaultError::ParseError(format!("Yrs apply: {e}")))?;
        Ok(())
    }

    /// Get the state vector (for requesting updates from peers).
    pub fn state_vector(&self) -> Vec<u8> {
        let txn = self.doc.transact();
        txn.state_vector().encode_v1()
    }

    /// Get the underlying Yrs Doc (for WebSocket sync integration).
    pub fn doc(&self) -> &Doc {
        &self.doc
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_text() {
        let body = BodyDoc::with_text("Hello world");
        assert_eq!(body.text(), "Hello world");
    }

    #[test]
    fn roundtrip() {
        let body = BodyDoc::with_text("## Subtasks\n- [ ] First\n- [x] Second");
        let saved = body.save();

        let loaded = BodyDoc::load(&saved).unwrap();
        assert_eq!(loaded.text(), "## Subtasks\n- [ ] First\n- [x] Second");
    }

    #[test]
    fn concurrent_edits() {
        // Peer A and B start with the same text
        let doc_a = BodyDoc::with_text("- [ ] Task 1\n- [ ] Task 2\n- [ ] Task 3");
        let state_a = doc_a.save();

        let doc_b = BodyDoc::load(&state_a).unwrap();

        // Peer A checks off task 1 (replace "[ ]" with "[x]")
        let text_a = doc_a.text();
        let idx = text_a.find("[ ] Task 1").unwrap() as u32;
        doc_a.delete(idx, 3);
        doc_a.insert(idx, "[x]");

        // Peer B checks off task 3
        let text_b = doc_b.text();
        let idx = text_b.find("[ ] Task 3").unwrap() as u32;
        doc_b.delete(idx, 3);
        doc_b.insert(idx, "[x]");

        // Sync: A sends update to B
        let sv_b = doc_b.state_vector();
        let update_a = doc_a.encode_update(&sv_b).unwrap();
        doc_b.apply_update(&update_a).unwrap();

        // Sync: B sends update to A
        let sv_a = doc_a.state_vector();
        let update_b = doc_b.encode_update(&sv_a).unwrap();
        doc_a.apply_update(&update_b).unwrap();

        // Both should have both checkboxes checked
        let final_text = doc_a.text();
        assert!(final_text.contains("[x] Task 1"), "Task 1 should be checked");
        assert!(final_text.contains("[x] Task 3"), "Task 3 should be checked");
        assert!(final_text.contains("[ ] Task 2"), "Task 2 should be unchecked");
    }
}
