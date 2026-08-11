//! Chunk-level garbage collection primitive (issue #258).
//!
//! iroh-blobs 0.103's only *supported* deletion path is its own periodic
//! mark-and-sweep task: `Blobs::delete` is `pub(crate)` ("Users should rely
//! only on garbage collection for blob deletion" — that method's own doc),
//! and `store::gc::gc_run_once` isn't reachable from outside the crate
//! either (`mod gc;`, not `pub mod gc;`, in `iroh_blobs::store`). The one
//! public surface is [`iroh_blobs::store::fs::Options::gc`]: a
//! [`iroh_blobs::store::GcConfig`] set at store-open time, which runs a
//! background task on a fixed interval and consults an `add_protected`
//! callback before each sweep.
//!
//! So chunk reclamation here is necessarily two-phase and asynchronous
//! relative to [`crate::ChunkStore::gc`]'s own call: that method (the
//! *mark* phase) durably removes swept manifests and publishes the
//! surviving chunk hashes into a shared protect-set; the actual blob
//! deletion (the *sweep* phase) happens on iroh-blobs' own schedule the
//! next time its background task's interval fires and reads that set
//! through the callback. A store opened via [`crate::ChunkStore::open`] or
//! [`crate::ChunkStore::open_with_config`] (no GC) has no interval and
//! never reclaims blobs — [`crate::ChunkStore::gc`] returns
//! [`crate::Error::GcDisabled`] on such a store, since deleting manifests
//! with nothing to eventually reclaim their chunks would just orphan them.

use std::time::Duration;

/// Chunk-level GC configuration for [`crate::ChunkStore::open_with_gc`]:
/// how often iroh-blobs' own background sweep is allowed to run against the
/// protect-set [`crate::ChunkStore::gc`] publishes.
#[derive(Debug, Clone, Copy)]
pub struct GcConfig {
    pub interval: Duration,
}

impl Default for GcConfig {
    /// A conservative production default — chunk reclamation is not
    /// latency-sensitive (unlike manifest removal, which `ChunkStore::gc`
    /// performs synchronously). Tests that need to observe a sweep pass
    /// within their own runtime should pass a much shorter interval
    /// explicitly.
    fn default() -> Self {
        Self {
            interval: Duration::from_secs(15 * 60),
        }
    }
}

/// The outcome of one [`crate::ChunkStore::gc`] mark pass.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct GcStats {
    /// Manifests removed this pass (unreferenced by the caller's `protected`
    /// set and older than `keep_newer`).
    pub manifests_swept: usize,
    /// Chunks that are, as of this pass, referenced by no surviving
    /// manifest — eligible for iroh-blobs' background sweep to reclaim.
    /// Not necessarily already gone: see the module doc on the two-phase
    /// mark/sweep split.
    pub chunks_marked_for_reclamation: usize,
}
