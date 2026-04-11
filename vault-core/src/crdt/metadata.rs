//! Automerge-backed metadata CRDT for task/event frontmatter fields.
//!
//! Each task's YAML frontmatter becomes an Automerge document where
//! each field is independently mergeable.

use automerge::{AutoCommit, ObjType, Prop, ReadDoc, ScalarValue, Value};
use automerge::transaction::Transactable;

use crate::task::{Task, Status, Priority, WikiLink};
use crate::service::VaultError;

/// Wraps an Automerge document representing a task's metadata fields.
pub struct MetadataDoc {
    doc: AutoCommit,
}

impl MetadataDoc {
    /// Create a new empty metadata document.
    pub fn new() -> Self {
        Self {
            doc: AutoCommit::new(),
        }
    }

    /// Load from saved Automerge bytes.
    pub fn load(bytes: &[u8]) -> Result<Self, VaultError> {
        let doc = AutoCommit::load(bytes)
            .map_err(|e| VaultError::ParseError(format!("Automerge load: {e}")))?;
        Ok(Self { doc })
    }

    /// Save to bytes for persistence.
    pub fn save(&mut self) -> Vec<u8> {
        self.doc.save()
    }

    /// Get the raw Automerge document for sync operations.
    pub fn doc(&self) -> &AutoCommit {
        &self.doc
    }

    /// Get mutable access for applying remote changes.
    pub fn doc_mut(&mut self) -> &mut AutoCommit {
        &mut self.doc
    }

    /// Generate incremental changes since last save (for WebSocket sync).
    pub fn save_incremental(&mut self) -> Vec<u8> {
        self.doc.save_incremental()
    }

    /// Apply changes from a remote peer.
    pub fn apply_changes(&mut self, changes: Vec<automerge::Change>) -> Result<(), VaultError> {
        self.doc
            .apply_changes(changes)
            .map_err(|e| VaultError::ParseError(format!("Automerge apply: {e}")))?;
        Ok(())
    }

    /// Merge another document into this one.
    pub fn merge(&mut self, other: &mut AutoCommit) -> Result<(), VaultError> {
        self.doc
            .merge(other)
            .map_err(|e| VaultError::ParseError(format!("Automerge merge: {e}")))?;
        Ok(())
    }

    // ── Task → Automerge ─────────────────────────────────────────────

    /// Populate the document from a Task struct.
    pub fn from_task(&mut self, task: &Task) {
        let root = automerge::ROOT;

        self.doc.put(&root, "title", task.title.as_str()).ok();
        self.doc.put(&root, "status", format!("{:?}", task.status)).ok();
        self.doc.put(&root, "priority", format!("{:?}", task.priority)).ok();

        if let Some(ref assignee) = task.assignee {
            self.doc.put(&root, "assignee", assignee.as_str()).ok();
        }

        if let Some(due) = task.due {
            self.doc.put(&root, "due", due.to_string()).ok();
        }

        if let Some(scheduled) = task.scheduled {
            self.doc.put(&root, "scheduled", scheduled.to_string()).ok();
        }

        if let Some(ref id) = task.id {
            self.doc.put(&root, "id", id.as_str()).ok();
        }

        if let Some(est) = task.time_estimate {
            self.doc.put(&root, "time_estimate", est as i64).ok();
        }

        if let Some(ref recurrence) = task.recurrence {
            self.doc.put(&root, "recurrence", recurrence.as_str()).ok();
        }

        if let Some(ref source) = task.external_source {
            self.doc.put(&root, "external_source", source.as_str()).ok();
        }

        if let Some(ref eid) = task.external_id {
            self.doc.put(&root, "external_id", eid.as_str()).ok();
        }

        if let Some(ref created_by) = task.created_by {
            self.doc.put(&root, "created_by", created_by.as_str()).ok();
        }

        // Lists — projects, tags, contexts
        if let Ok(projects_id) = self.doc.put_object(&root, "projects", ObjType::List) {
            for (i, proj) in task.projects.iter().enumerate() {
                self.doc.insert(&projects_id, i, proj.0.as_str()).ok();
            }
        }

        if let Ok(tags_id) = self.doc.put_object(&root, "tags", ObjType::List) {
            for (i, tag) in task.tags.iter().enumerate() {
                self.doc.insert(&tags_id, i, tag.as_str()).ok();
            }
        }

        if let Ok(contexts_id) = self.doc.put_object(&root, "contexts", ObjType::List) {
            for (i, ctx) in task.contexts.iter().enumerate() {
                self.doc.insert(&contexts_id, i, ctx.as_str()).ok();
            }
        }
    }

    // ── Automerge → Task ─────────────────────────────────────────────

    /// Read the document back into a Task struct.
    pub fn to_task(&self) -> Task {
        let root = automerge::ROOT;
        let mut task = Task::default();

        task.title = self.get_string(&root, "title").unwrap_or_default();
        task.id = self.get_string(&root, "id");
        task.assignee = self.get_string(&root, "assignee");
        task.created_by = self.get_string(&root, "created_by");
        task.external_source = self.get_string(&root, "external_source");
        task.external_id = self.get_string(&root, "external_id");

        if let Some(s) = self.get_string(&root, "status") {
            task.status = match s.as_str() {
                "Open" => Status::Open,
                "InProgress" => Status::InProgress,
                "Done" => Status::Done,
                "OnHold" => Status::OnHold,
                "Planned" => Status::Planned,
                "Cancelled" => Status::Cancelled,
                "Archived" => Status::Archived,
                _ => Status::Open,
            };
        }

        if let Some(p) = self.get_string(&root, "priority") {
            task.priority = match p.as_str() {
                "Urgent" => Priority::Urgent,
                "High" => Priority::High,
                "Normal" => Priority::Normal,
                "Low" => Priority::Low,
                _ => Priority::None,
            };
        }

        if let Some(d) = self.get_string(&root, "due") {
            task.due = chrono::NaiveDate::parse_from_str(&d, "%Y-%m-%d").ok();
        }

        if let Some(d) = self.get_string(&root, "scheduled") {
            task.scheduled = chrono::NaiveDate::parse_from_str(&d, "%Y-%m-%d").ok();
        }

        if let Some(r) = self.get_string(&root, "recurrence") {
            task.recurrence = Some(r);
        }

        if let Some(est) = self.get_i64(&root, "time_estimate") {
            task.time_estimate = Some(est as u32);
        }

        // Lists
        task.projects = self.get_string_list(&root, "projects")
            .into_iter()
            .map(|s| WikiLink(s))
            .collect();
        task.tags = self.get_string_list(&root, "tags");
        task.contexts = self.get_string_list(&root, "contexts");

        task
    }

    // ── Field-level operations ───────────────────────────────────────

    /// Set a single field. Used for real-time field-level updates.
    pub fn set_field(&mut self, field: &str, value: &str) {
        self.doc.put(&automerge::ROOT, field, value).ok();
    }

    /// Get a single field value.
    pub fn get_field(&self, field: &str) -> Option<String> {
        self.get_string(&automerge::ROOT, field)
    }

    // ── Helpers ──────────────────────────────────────────────────────

    fn get_string(&self, obj: &automerge::ObjId, key: &str) -> Option<String> {
        match self.doc.get(obj, Prop::Map(key.into())) {
            Ok(Some((Value::Scalar(cow), _))) => match cow.as_ref() {
                ScalarValue::Str(s) => Some(s.to_string()),
                _ => None,
            },
            _ => None,
        }
    }

    fn get_i64(&self, obj: &automerge::ObjId, key: &str) -> Option<i64> {
        match self.doc.get(obj, Prop::Map(key.into())) {
            Ok(Some((Value::Scalar(cow), _))) => match cow.as_ref() {
                ScalarValue::Int(n) => Some(*n),
                _ => None,
            },
            _ => None,
        }
    }

    fn get_string_list(&self, obj: &automerge::ObjId, key: &str) -> Vec<String> {
        let list_id = match self.doc.get(obj, Prop::Map(key.into())) {
            Ok(Some((_, id))) => id,
            _ => return vec![],
        };

        let len = self.doc.length(&list_id);
        let mut result = Vec::new();
        for i in 0..len {
            if let Ok(Some((Value::Scalar(cow), _))) = self.doc.get(&list_id, Prop::Seq(i)) {
                if let ScalarValue::Str(s) = cow.as_ref() {
                    result.push(s.to_string());
                }
            }
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::task::{Task, Status, Priority, WikiLink};

    #[test]
    fn roundtrip_task() {
        let task = Task {
            id: Some("test-1".into()),
            title: "Fix auth bug".into(),
            status: Status::InProgress,
            priority: Priority::High,
            assignee: Some("codywright".into()),
            due: chrono::NaiveDate::from_ymd_opt(2026, 4, 15),
            projects: vec![WikiLink("Task App".into())],
            tags: vec!["backend".into(), "urgent".into()],
            contexts: vec!["work".into()],
            time_estimate: Some(120),
            ..Default::default()
        };

        let mut doc = MetadataDoc::new();
        doc.from_task(&task);

        let roundtripped = doc.to_task();
        assert_eq!(roundtripped.title, "Fix auth bug");
        assert_eq!(roundtripped.status, Status::InProgress);
        assert_eq!(roundtripped.priority, Priority::High);
        assert_eq!(roundtripped.assignee, Some("codywright".into()));
        assert_eq!(roundtripped.projects.len(), 1);
        assert_eq!(roundtripped.tags, vec!["backend", "urgent"]);
        assert_eq!(roundtripped.time_estimate, Some(120));
    }

    #[test]
    fn concurrent_merge() {
        let task = Task {
            title: "Shared task".into(),
            status: Status::Open,
            assignee: Some("alice".into()),
            ..Default::default()
        };

        // Two peers start from the same state
        let mut doc_a = MetadataDoc::new();
        doc_a.from_task(&task);
        let saved = doc_a.save();

        let mut doc_b = MetadataDoc::load(&saved).unwrap();

        // Peer A changes status
        doc_a.set_field("status", "InProgress");

        // Peer B changes assignee
        doc_b.set_field("assignee", "bob");

        // Merge B into A
        doc_a.merge(doc_b.doc_mut()).unwrap();

        // Both changes preserved
        assert_eq!(doc_a.get_field("status").unwrap(), "InProgress");
        assert_eq!(doc_a.get_field("assignee").unwrap(), "bob");
    }
}
