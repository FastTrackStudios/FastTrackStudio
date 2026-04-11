//! CrdtDocument — unified metadata + body CRDT for a single .md file.
//!
//! Combines an Automerge document (YAML frontmatter fields) with a
//! Yrs document (markdown body) into a single syncable unit.

use crate::task::Task;
use crate::vault::Vault;
use crate::service::VaultError;

use super::metadata::MetadataDoc;
use super::body::BodyDoc;

/// A complete CRDT representation of a `.md` file.
///
/// Two CRDT documents working together:
/// - `metadata` (Automerge) — frontmatter fields, field-level merge
/// - `body` (Yrs) — markdown content, character-level merge
pub struct CrdtDocument {
    pub metadata: MetadataDoc,
    pub body: BodyDoc,
    /// File path this document represents.
    pub file_path: String,
}

impl CrdtDocument {
    /// Create from a parsed Task (e.g. after reading a .md file).
    pub fn from_task(task: &Task, file_path: &str) -> Self {
        let mut metadata = MetadataDoc::new();
        metadata.from_task(task);

        let body = if task.body.is_empty() {
            BodyDoc::new()
        } else {
            BodyDoc::with_text(&task.body)
        };

        Self {
            metadata,
            body,
            file_path: file_path.to_string(),
        }
    }

    /// Create from a raw .md file content string.
    pub fn from_md(content: &str, file_path: &str) -> Option<Self> {
        let task = Vault::parse_task_from_md(content)?;
        Some(Self::from_task(&task, file_path))
    }

    /// Convert back to a Task.
    pub fn to_task(&self) -> Task {
        let mut task = self.metadata.to_task();
        task.body = self.body.text();
        task
    }

    /// Render back to a .md file string.
    pub fn to_md(&self) -> Result<String, VaultError> {
        let task = self.metadata.to_task();
        let body = self.body.text();
        Vault::render_task_file(&task, &body)
    }

    /// Apply a field-level metadata change.
    pub fn set_field(&mut self, field: &str, value: &str) {
        self.metadata.set_field(field, value);
    }

    /// Get a field value.
    pub fn get_field(&self, field: &str) -> Option<String> {
        self.metadata.get_field(field)
    }

    /// Update the body text.
    pub fn set_body(&self, text: &str) {
        self.body.set_text(text);
    }

    /// Get the current body text.
    pub fn get_body(&self) -> String {
        self.body.text()
    }

    // ── Sync ─────────────────────────────────────────────────────────

    /// Save both CRDT states for persistence.
    pub fn save(&mut self) -> (Vec<u8>, Vec<u8>) {
        (self.metadata.save(), self.body.save())
    }

    /// Merge another document's metadata into this one.
    pub fn merge_metadata(&mut self, other: &mut MetadataDoc) -> Result<(), VaultError> {
        self.metadata.merge(other.doc_mut())
    }

    /// Apply a remote body update.
    pub fn apply_body_update(&self, update: &[u8]) -> Result<(), VaultError> {
        self.body.apply_update(update)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::task::{Task, Status, Priority, WikiLink};

    #[test]
    fn roundtrip_through_crdt() {
        let task = Task {
            title: "Test task".into(),
            status: Status::Open,
            priority: Priority::High,
            assignee: Some("alice".into()),
            tags: vec!["urgent".into()],
            projects: vec![WikiLink("Project X".into())],
            body: "## Subtasks\n- [ ] First\n- [ ] Second".into(),
            ..Default::default()
        };

        let doc = CrdtDocument::from_task(&task, "test.md");

        let roundtripped = doc.to_task();
        assert_eq!(roundtripped.title, "Test task");
        assert_eq!(roundtripped.status, Status::Open);
        assert_eq!(roundtripped.priority, Priority::High);
        assert_eq!(roundtripped.assignee, Some("alice".into()));
        assert_eq!(roundtripped.body, "## Subtasks\n- [ ] First\n- [ ] Second");
    }

    #[test]
    fn field_level_edit() {
        let task = Task {
            title: "Shared task".into(),
            status: Status::Open,
            ..Default::default()
        };

        let mut doc = CrdtDocument::from_task(&task, "test.md");
        doc.set_field("status", "Done");
        doc.set_field("assignee", "bob");

        let updated = doc.to_task();
        assert_eq!(updated.status, Status::Done);
        assert_eq!(updated.assignee, Some("bob".into()));
        assert_eq!(updated.title, "Shared task"); // unchanged
    }
}
