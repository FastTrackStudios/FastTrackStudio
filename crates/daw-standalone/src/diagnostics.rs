//! `impl Diagnostics for Standalone` — stub. Standalone has no
//! csurf, so the in-process latency probe returns empty.

use crate::Standalone;
use daw_proto::ProjectContext;
use daw_proto::diagnostics::{AudioSyncSnapshot, Diagnostics};

impl Diagnostics for Standalone {
    fn hub_publish_latency_us(&self, _project: ProjectContext, _samples: u32) -> Vec<u64> {
        Vec::new()
    }

    fn audio_sync_snapshot(&self) -> Option<AudioSyncSnapshot> {
        None
    }

    fn audio_sync_observe(&self, _count: u32, _interval_us: u64) -> Vec<AudioSyncSnapshot> {
        Vec::new()
    }
}
