//! Loro-backed CRDT document: unified metadata + body for a single .md file.
//!
//! One `LoroDoc` holds every piece of state for a task:
//!
//! - `meta` ([`LoroMap`]) — scalar frontmatter fields (title, status, priority,
//!   assignee, due, scheduled, …). LWW semantics.
//! - `body` ([`LoroText`]) — markdown body after the `---`. Character-level CRDT.
//! - `tags` / `projects` / `contexts` ([`LoroList`]) — string lists.
//!
//! # Why not LoroMap LWW across the board?
//!
//! LoroMap silently drops concurrent writes. Our conflict log lives in SQLite
//! (`record_conflict`) — [`super::sync`] does a **send-time** diff of the local
//! values before applying a remote update, so we catch concurrent writes
//! before LWW eats them. The [`DocumentSnapshot`] below is what that layer
//! diffs against.

use std::collections::BTreeMap;

use loro::{ExportMode, LoroDoc, LoroList, LoroValue, PeerID, ValueOrContainer};

use crate::service::VaultError;
use crate::task::{Priority, Status, Task, WikiLink};

const META_ID: &str = "meta";
const BODY_ID: &str = "body";
const TAGS_ID: &str = "tags";
const PROJECTS_ID: &str = "projects";
const CONTEXTS_ID: &str = "contexts";

/// Field keys we care about for conflict detection, defined in one place so
/// the sync layer and the metadata read path stay in sync.
pub const CONFLICT_FIELDS: &[&str] = &[
    "title",
    "status",
    "priority",
    "assignee",
    "due",
    "scheduled",
    "recurrence",
    "time_estimate",
    "external_id",
    "external_source",
    "created_by",
];

/// A complete CRDT representation of a `.md` file.
pub struct CrdtDocument {
    doc: LoroDoc,
    pub file_path: String,
}

/// Values captured from the document *before* an import, used by the sync
/// layer to detect concurrent writes after import and log conflict rows.
#[derive(Debug, Clone, Default)]
pub struct DocumentSnapshot {
    pub fields: BTreeMap<String, Option<String>>,
    pub field_editors: BTreeMap<String, Option<PeerID>>,
}

impl CrdtDocument {
    /// Create an empty doc attached to a file path.
    pub fn new(file_path: impl Into<String>) -> Self {
        Self {
            doc: LoroDoc::new(),
            file_path: file_path.into(),
        }
    }

    /// Create from a parsed [`Task`]. Commits the population as one transaction.
    pub fn from_task(task: &Task, file_path: impl Into<String>) -> Self {
        let me = Self::new(file_path);
        me.populate_from_task(task);
        me.doc.commit();
        me
    }

    /// Set the peer ID. Must be unique per device — collisions silently corrupt
    /// history. Caller is responsible for choosing stable IDs.
    pub fn set_peer_id(&self, peer: PeerID) -> Result<(), VaultError> {
        self.doc
            .set_peer_id(peer)
            .map_err(|e| VaultError::ParseError(format!("loro set_peer_id: {e}")))
    }

    pub fn peer_id(&self) -> PeerID {
        self.doc.peer_id()
    }

    /// Expose the inner doc for subscribe / advanced sync uses.
    pub fn inner(&self) -> &LoroDoc {
        &self.doc
    }

    // ── Task ↔ document ──────────────────────────────────────────────────────

    fn populate_from_task(&self, task: &Task) {
        let meta = self.doc.get_map(META_ID);
        let _ = meta.insert("title", task.title.as_str());
        let _ = meta.insert("status", format!("{:?}", task.status));
        let _ = meta.insert("priority", format!("{:?}", task.priority));
        let _ = meta.insert("id", task.id.to_string());
        if let Some(v) = &task.assignee {
            let _ = meta.insert("assignee", v.as_str());
        }
        if let Some(v) = task.due {
            let _ = meta.insert("due", v.to_string());
        }
        if let Some(v) = task.scheduled {
            let _ = meta.insert("scheduled", v.to_string());
        }
        if let Some(v) = &task.recurrence {
            let _ = meta.insert("recurrence", v.as_str());
        }
        if let Some(v) = task.time_estimate {
            let _ = meta.insert("time_estimate", v as i64);
        }
        if let Some(v) = &task.external_source {
            let _ = meta.insert("external_source", v.as_str());
        }
        if let Some(v) = &task.external_id {
            let _ = meta.insert("external_id", v.as_str());
        }
        if let Some(v) = &task.created_by {
            let _ = meta.insert("created_by", v.as_str());
        }

        let tags = self.doc.get_list(TAGS_ID);
        for t in &task.tags {
            let _ = tags.push(t.as_str());
        }
        let projects = self.doc.get_list(PROJECTS_ID);
        for p in &task.projects {
            let _ = projects.push(p.0.as_str());
        }
        let contexts = self.doc.get_list(CONTEXTS_ID);
        for c in &task.contexts {
            let _ = contexts.push(c.as_str());
        }

        let body = self.doc.get_text(BODY_ID);
        if !task.body.is_empty() {
            let _ = body.insert(0, &task.body);
        }
    }

    /// Build a fresh `Task` from the current doc state.
    pub fn to_task(&self) -> Task {
        let meta = self.doc.get_map(META_ID);
        let mut task = Task {
            title: get_string(&meta, "title").unwrap_or_default(),
            id: get_string(&meta, "id")
                .and_then(|id| uuid::Uuid::parse_str(&id).ok())
                .unwrap_or_else(uuid::Uuid::new_v4),
            assignee: get_string(&meta, "assignee"),
            created_by: get_string(&meta, "created_by"),
            external_source: get_string(&meta, "external_source"),
            external_id: get_string(&meta, "external_id"),
            recurrence: get_string(&meta, "recurrence"),
            ..Default::default()
        };

        if let Some(s) = get_string(&meta, "status") {
            task.status = parse_status(&s);
        }
        if let Some(p) = get_string(&meta, "priority") {
            task.priority = parse_priority(&p);
        }
        if let Some(d) = get_string(&meta, "due") {
            task.due = chrono::NaiveDate::parse_from_str(&d, "%Y-%m-%d").ok();
        }
        if let Some(d) = get_string(&meta, "scheduled") {
            task.scheduled = chrono::NaiveDate::parse_from_str(&d, "%Y-%m-%d").ok();
        }
        if let Some(n) = get_i64(&meta, "time_estimate") {
            task.time_estimate = Some(n as u32);
        }

        task.tags = read_string_list(&self.doc.get_list(TAGS_ID)).into();
        task.projects = read_string_list(&self.doc.get_list(PROJECTS_ID))
            .into_iter()
            .map(WikiLink)
            .collect::<Vec<_>>()
            .into();
        task.contexts = read_string_list(&self.doc.get_list(CONTEXTS_ID)).into();
        task.body = self.doc.get_text(BODY_ID).to_string();

        task
    }

    // ── Field-level edits ────────────────────────────────────────────────────

    /// Set a scalar meta field. Caller must `commit()` to finalize.
    pub fn set_field(&self, field: &str, value: &str) {
        let _ = self.doc.get_map(META_ID).insert(field, value);
    }

    pub fn get_field(&self, field: &str) -> Option<String> {
        get_string(&self.doc.get_map(META_ID), field)
    }

    /// Peer who last wrote this field. Used by the conflict detector after
    /// importing remote updates.
    pub fn last_editor(&self, field: &str) -> Option<PeerID> {
        self.doc.get_map(META_ID).get_last_editor(field)
    }

    /// Replace the entire body text. Caller must `commit()` to finalize.
    pub fn set_body(&self, text: &str) {
        let body = self.doc.get_text(BODY_ID);
        let len = body.len_unicode();
        if len > 0 {
            let _ = body.delete(0, len);
        }
        let _ = body.insert(0, text);
    }

    pub fn get_body(&self) -> String {
        self.doc.get_text(BODY_ID).to_string()
    }

    /// Commit the current transaction so the edits become visible to
    /// subscribers / exports.
    pub fn commit(&self) {
        self.doc.commit();
    }

    // ── Sync primitives ──────────────────────────────────────────────────────

    /// Full snapshot (state + full oplog). Use for persistence and cold start.
    pub fn export_snapshot(&self) -> Result<Vec<u8>, VaultError> {
        self.doc
            .export(ExportMode::Snapshot)
            .map_err(|e| VaultError::ParseError(format!("loro snapshot: {e}")))
    }

    /// Incremental updates since the peer's version vector.
    pub fn export_updates_since(&self, from: &loro::VersionVector) -> Result<Vec<u8>, VaultError> {
        self.doc
            .export(ExportMode::updates(from))
            .map_err(|e| VaultError::ParseError(format!("loro updates: {e}")))
    }

    /// Import bytes (either snapshot or updates — Loro auto-detects).
    pub fn import(&self, bytes: &[u8]) -> Result<(), VaultError> {
        self.doc
            .import(bytes)
            .map(|_| ())
            .map_err(|e| VaultError::ParseError(format!("loro import: {e}")))
    }

    /// Load a doc from a snapshot.
    pub fn from_snapshot(bytes: &[u8], file_path: impl Into<String>) -> Result<Self, VaultError> {
        let doc = LoroDoc::from_snapshot(bytes)
            .map_err(|e| VaultError::ParseError(format!("loro from_snapshot: {e}")))?;
        Ok(Self {
            doc,
            file_path: file_path.into(),
        })
    }

    pub fn version_vector(&self) -> loro::VersionVector {
        self.doc.oplog_vv()
    }

    pub fn frontiers(&self) -> loro::Frontiers {
        self.doc.oplog_frontiers()
    }

    /// Capture every conflict-relevant field value + its last editor. Used by
    /// the sync layer as the "before" side of an import so it can detect
    /// concurrent writes post-import.
    pub fn snapshot_fields(&self) -> DocumentSnapshot {
        let meta = self.doc.get_map(META_ID);
        let mut fields = BTreeMap::new();
        let mut field_editors = BTreeMap::new();
        for f in CONFLICT_FIELDS {
            fields.insert((*f).to_string(), get_string(&meta, f));
            field_editors.insert((*f).to_string(), meta.get_last_editor(f));
        }
        DocumentSnapshot {
            fields,
            field_editors,
        }
    }
}

// ── helpers ──────────────────────────────────────────────────────────────────

fn get_string(map: &loro::LoroMap, key: &str) -> Option<String> {
    match map.get(key)? {
        ValueOrContainer::Value(LoroValue::String(s)) => Some((*s).to_string()),
        _ => None,
    }
}

fn get_i64(map: &loro::LoroMap, key: &str) -> Option<i64> {
    match map.get(key)? {
        ValueOrContainer::Value(LoroValue::I64(n)) => Some(n),
        _ => None,
    }
}

fn read_string_list(list: &LoroList) -> Vec<String> {
    let mut out = Vec::new();
    list.for_each(|v| {
        if let ValueOrContainer::Value(LoroValue::String(s)) = v {
            out.push((*s).to_string());
        }
    });
    out
}

fn parse_status(s: &str) -> Status {
    match s {
        "Open" => Status::Open,
        "InProgress" => Status::InProgress,
        "Done" => Status::Done,
        "OnHold" => Status::OnHold,
        "Planned" => Status::Planned,
        "Cancelled" => Status::Cancelled,
        "Archived" => Status::Archived,
        _ => Status::Open,
    }
}

fn parse_priority(s: &str) -> Priority {
    match s {
        "Urgent" => Priority::Urgent,
        "High" => Priority::High,
        "Normal" => Priority::Normal,
        "Low" => Priority::Low,
        _ => Priority::None,
    }
}

// ── tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::task::{Priority, Status, WikiLink};

    fn make_task() -> Task {
        Task {
            id: uuid::Uuid::parse_str("00000000-0000-4000-8000-000000000401").unwrap(),
            title: "Mix track".into(),
            status: Status::Open,
            priority: Priority::High,
            assignee: Some("cody".into()),
            tags: vec!["urgent".into(), "audio".into()].into(),
            projects: vec![WikiLink("Album".into())].into(),
            contexts: vec!["studio".into()].into(),
            due: chrono::NaiveDate::from_ymd_opt(2026, 4, 20),
            body: "## Subtasks\n- [ ] Low end\n- [ ] Vocals".into(),
            ..Default::default()
        }
    }

    #[test]
    fn task_roundtrip() {
        let doc = CrdtDocument::from_task(&make_task(), "t-1.md");
        let t = doc.to_task();
        assert_eq!(t.title, "Mix track");
        assert_eq!(t.status, Status::Open);
        assert_eq!(t.priority, Priority::High);
        assert_eq!(t.assignee.as_deref(), Some("cody"));
        assert_eq!(t.tags.to_vec(), vec!["urgent".to_string(), "audio".into()]);
        assert_eq!(t.projects.len(), 1);
        assert_eq!(t.projects[0].0, "Album");
        assert_eq!(t.contexts.to_vec(), vec!["studio".to_string()]);
        assert_eq!(t.due, chrono::NaiveDate::from_ymd_opt(2026, 4, 20));
        assert!(t.body.contains("Low end"));
    }

    #[test]
    fn snapshot_export_import() {
        let doc = CrdtDocument::from_task(&make_task(), "t-1.md");
        let bytes = doc.export_snapshot().unwrap();
        let copy = CrdtDocument::from_snapshot(&bytes, "t-1.md").unwrap();
        assert_eq!(copy.to_task().title, "Mix track");
    }

    #[test]
    fn field_edit_and_last_editor() {
        let doc = CrdtDocument::from_task(&make_task(), "t-1.md");
        doc.set_peer_id(42).unwrap();
        doc.set_field("status", "Done");
        doc.commit();
        assert_eq!(doc.get_field("status").as_deref(), Some("Done"));
        assert_eq!(doc.last_editor("status"), Some(42));
    }

    /// Two peers edit different fields → merge converges on both values.
    #[test]
    fn concurrent_disjoint_fields_converge() {
        let a = CrdtDocument::from_task(&make_task(), "t-1.md");
        a.set_peer_id(1).unwrap();
        a.commit();
        let snap = a.export_snapshot().unwrap();

        let b = CrdtDocument::from_snapshot(&snap, "t-1.md").unwrap();
        b.set_peer_id(2).unwrap();

        a.set_field("status", "Done");
        a.commit();
        b.set_field("assignee", "amy");
        b.commit();

        let a_updates = a.export_updates_since(&b.version_vector()).unwrap();
        let b_updates = b.export_updates_since(&a.version_vector()).unwrap();
        a.import(&b_updates).unwrap();
        b.import(&a_updates).unwrap();

        let ta = a.to_task();
        let tb = b.to_task();
        assert_eq!(ta.status, Status::Done);
        assert_eq!(ta.assignee.as_deref(), Some("amy"));
        assert_eq!(tb.status, Status::Done);
        assert_eq!(tb.assignee.as_deref(), Some("amy"));
    }

    /// Snapshot captures the pre-import state used for conflict detection.
    #[test]
    fn field_snapshot_records_values_and_editors() {
        let doc = CrdtDocument::from_task(&make_task(), "t-1.md");
        doc.set_peer_id(7).unwrap();
        doc.set_field("status", "InProgress");
        doc.commit();

        let snap = doc.snapshot_fields();
        assert_eq!(snap.fields.get("status"), Some(&Some("InProgress".into())));
        assert_eq!(snap.field_editors.get("status"), Some(&Some(7)));
        assert_eq!(snap.fields.get("priority"), Some(&Some("High".into())));
    }
}
