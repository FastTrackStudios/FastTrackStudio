//! CRDT sync primitives.
//!
//! These types describe operations and conflicts that flow between peers in a
//! Loro-backed sync session. The transport layer (the `#[vox::service]`
//! per-entity `subscribe` / `apply_op` methods, designed in a follow-up)
//! is responsible for moving them around — this module only defines the
//! shapes plus the conflict-detection rule that the higher layer applies.

use loro::PeerID;

use crate::task::Task;

use super::document::{CONFLICT_FIELDS, CrdtDocument, DocumentSnapshot};

/// An operation broadcast to connected clients.
///
/// `TaskCreated` boxes its `Task` so the enum stays small — the other
/// variants are byte-cheap (a `Vec<u8>` body update plus some `String`s)
/// and shouldn't pay the size of the full Task struct on every clone.
#[derive(Debug, Clone)]
pub enum SyncOp {
    /// A metadata field changed on a task.
    FieldChanged {
        file_path: String,
        field: String,
        value: String,
        /// Peer that applied the change, if known.
        peer: Option<PeerID>,
    },
    /// Raw Loro update bytes — body edits and anything else.
    DocUpdate { file_path: String, update: Vec<u8> },
    /// A new task was created.
    TaskCreated { file_path: String, task: Box<Task> },
    /// A task was deleted.
    TaskDeleted { file_path: String },
    /// Full refresh — file was rewritten out-of-band.
    Refresh,
}

/// A concurrent-edit conflict detected when importing remote updates.
///
/// One of these is emitted per field where the local replica had a pending
/// write that got overwritten by the incoming import. The outer layer is
/// responsible for persisting the conflict via
/// [`TaskIndex::record_conflict`](crate::index::TaskIndex::record_conflict).
#[derive(Debug, Clone)]
pub struct ConflictEvent {
    pub file_path: String,
    pub field: String,
    /// The value we held locally before the import (now overwritten).
    pub losing_value: Option<String>,
    /// The value after the import — what the map now resolves to.
    pub winning_value: Option<String>,
    /// Peer that wrote the losing value (the local replica's perspective).
    pub losing_peer: Option<PeerID>,
    /// Peer that wrote the winning value (the remote's perspective).
    pub winning_peer: Option<PeerID>,
}

/// Decide which fields changed in a way that represents a concurrent overwrite
/// of a locally-authored write.
///
/// Rule: a field is a conflict iff
/// 1. the value after the import differs from the value before, AND
/// 2. the post-import `last_editor` is not the local peer, AND
/// 3. the pre-import `last_editor` **was** the local peer (we had a pending
///    local write that the remote just clobbered).
///
/// This is the "Option A" send-time detection — imperfect (misses A->B->A
/// chains where A's original losing write came from another remote) but
/// catches every case where the local replica actually lost a write it
/// authored.
pub fn detect_field_conflicts(
    doc: &CrdtDocument,
    before: &DocumentSnapshot,
    local_peer: PeerID,
) -> Vec<DetectedConflict> {
    let mut out = Vec::new();
    for field in CONFLICT_FIELDS {
        let before_value = before.fields.get(*field).cloned().flatten();
        let before_editor = before.field_editors.get(*field).and_then(|e| *e);
        let after_value = doc.get_field(field);
        let after_editor = doc.last_editor(field);

        if after_value == before_value {
            continue;
        }
        if after_editor == Some(local_peer) {
            continue;
        }
        if before_editor != Some(local_peer) {
            // We didn't author the losing value, so nothing of ours was
            // clobbered. It's a normal remote write, not a conflict.
            continue;
        }

        out.push(DetectedConflict {
            field: (*field).to_string(),
            losing_value: before_value,
            winning_value: after_value,
            losing_peer: before_editor,
            winning_peer: after_editor,
        });
    }
    out
}

#[derive(Debug, Clone)]
pub struct DetectedConflict {
    pub field: String,
    pub losing_value: Option<String>,
    pub winning_value: Option<String>,
    pub losing_peer: Option<PeerID>,
    pub winning_peer: Option<PeerID>,
}

// ── tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::task::{Priority, Status};

    fn doc_with(peer: PeerID, title: &str) -> CrdtDocument {
        let t = Task {
            id: uuid::Uuid::parse_str("00000000-0000-4000-8000-000000000402").unwrap(),
            title: title.into(),
            status: Status::Open,
            priority: Priority::Normal,
            ..Default::default()
        };
        let doc = CrdtDocument::from_task(&t, "t-1.md");
        doc.set_peer_id(peer).unwrap();
        doc.commit();
        doc
    }

    /// Two peers concurrently write the same field. After bidirectional
    /// import, the loser's write shows up as a detected conflict on the
    /// replica that lost.
    #[test]
    fn concurrent_same_field_produces_conflict() {
        let a = doc_with(1, "Task");
        let snap = a.export_snapshot().unwrap();

        let b = CrdtDocument::from_snapshot(&snap, "t-1.md").unwrap();
        b.set_peer_id(2).unwrap();

        // Each peer writes a different value to `status`.
        a.set_field("status", "Done");
        a.commit();
        b.set_field("status", "Cancelled");
        b.commit();

        // Snapshot A's state before it receives B's update.
        let before_a = a.snapshot_fields();
        let b_update = b.export_updates_since(&a.version_vector()).unwrap();
        a.import(&b_update).unwrap();

        let conflicts = detect_field_conflicts(&a, &before_a, 1);

        // Exactly one conflict, on `status`. Which value wins is Loro's LWW
        // call; the detector's job is to preserve both.
        let status_conflict = conflicts.iter().find(|c| c.field == "status");
        assert!(
            status_conflict.is_some(),
            "expected a status conflict, got {conflicts:?}"
        );
        let c = status_conflict.unwrap();
        assert_eq!(c.losing_value.as_deref(), Some("Done"));
        assert!(matches!(
            c.winning_value.as_deref(),
            Some("Done") | Some("Cancelled")
        ));
        assert_eq!(c.losing_peer, Some(1));
    }

    /// Disjoint field writes merge without generating conflicts.
    #[test]
    fn disjoint_writes_produce_no_conflicts() {
        let a = doc_with(1, "Task");
        let snap = a.export_snapshot().unwrap();
        let b = CrdtDocument::from_snapshot(&snap, "t-1.md").unwrap();
        b.set_peer_id(2).unwrap();

        a.set_field("status", "Done");
        a.commit();
        b.set_field("assignee", "amy");
        b.commit();

        let before_a = a.snapshot_fields();
        let b_update = b.export_updates_since(&a.version_vector()).unwrap();
        a.import(&b_update).unwrap();

        let conflicts = detect_field_conflicts(&a, &before_a, 1);
        assert!(
            conflicts.is_empty(),
            "disjoint writes must not conflict, got {conflicts:?}"
        );
        // Final state: both fields present.
        assert_eq!(a.get_field("status").as_deref(), Some("Done"));
        assert_eq!(a.get_field("assignee").as_deref(), Some("amy"));
    }
}
