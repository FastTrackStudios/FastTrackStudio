//! CRDT layer — conflict-free replicated data types for real-time collaboration.
//!
//! Each `.md` file is backed by one [`CrdtDocument`] built on top of Loro 1.x.
//! The doc holds one structured map (`meta`) for frontmatter fields, one
//! `LoroText` (`body`) for markdown content, and string lists for `tags`,
//! `projects`, and `contexts`.
//!
//! `LoroMap` is LWW with no multi-value register, so concurrent writes to the
//! same field would be silently lost. To surface those for the UI and audit
//! log, [`sync`] snapshots field values *before* importing remote updates and
//! emits [`ConflictEvent`](sync::ConflictEvent)s for every field whose local
//! write lost the race.

pub mod document;
pub mod offline;
pub mod sync;

pub use document::{CrdtDocument, DocumentSnapshot, CONFLICT_FIELDS};
pub use offline::{OfflineQueue, QueuedOp, QueuedOpType};
pub use sync::{ConflictEvent, CrdtSyncEngine, SyncOp};
