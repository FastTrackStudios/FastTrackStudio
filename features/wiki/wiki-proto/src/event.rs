//! Live change events. Mirrors `vault_proto::VaultEvent` —
//! subscribers get a stream of typed updates and can build
//! reactive UI on top.

use chrono::{DateTime, Utc};
use facet::Facet;

#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub enum WikiEvent {
    /// A wiki page was created or replaced.
    PageWritten { path: String, at: DateTime<Utc> },
    /// A wiki page was deleted.
    PageDeleted { path: String, at: DateTime<Utc> },
    /// A new ingest task hit the queue.
    IngestEnqueued {
        task_id: String,
        source_path: String,
    },
    /// An ingest task transitioned state.
    IngestStateChanged {
        task_id: String,
        /// `"Pending" | "Analyzing" | "Generating" | ...`
        /// matches [`crate::ingest::IngestStatus`].
        new_status: String,
    },
    /// A lint pass produced findings.
    LintCompleted {
        finding_count: u32,
        at: DateTime<Utc>,
    },
    /// A new review item is awaiting curator attention.
    ReviewEnqueued { item_id: String },
    /// A peer pull completed.
    PeerPulled { peer_id: String, changed: u32 },
    /// Broadcast lag — subscriber missed events. Re-pull
    /// state explicitly.
    Resync,
}
