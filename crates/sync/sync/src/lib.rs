//! Sync engine — DAW-to-DAW real-time synchronization.
//!
//! Subscribes to all local DAW change streams, wraps each event in a
//! [`SyncEvent`] envelope with origin peer + sequence number, and forwards
//! to all connected peers via roam. Receives remote events and applies them
//! through the DAW mutation API, with echo suppression to prevent infinite
//! bounce loops.
//!
//! # Architecture
//!
//! ```text
//! Local DAW (poll ~30Hz)
//!   ├─ TrackEvent ──┐
//!   ├─ Transport ───┤
//!   ├─ FxEvent ─────┤
//!   └─ ... ─────────┤
//!                    ▼
//!              Engine::run()
//!                ├─ wrap in SyncEvent { origin: self, seq: N }
//!                ├─ check suppression (skip if echo)
//!                └─ send to peers via roam
//!
//! Remote SyncEvent arrives
//!   ├─ filter: origin == self → skip
//!   ├─ record in suppression set
//!   └─ apply via daw mutation API
//! ```

mod apply;
mod engine;
#[cfg(feature = "link")]
pub mod link;
mod subscriptions;
mod suppression;

pub use engine::Engine;
pub use subscriptions::ProjectSubscriptions;
pub use sync_proto::{
    ConflictPolicy, SyncConfig, SyncDomain, SyncEvent, SyncPeer, SyncService, SyncSession,
    SyncStatus,
};
