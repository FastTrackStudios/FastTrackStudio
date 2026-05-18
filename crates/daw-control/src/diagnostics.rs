//! Diagnostics handle — exposes in-process probe RPCs (latency,
//! throughput) on top of the `Diagnostics` service.

use std::sync::Arc;

use crate::{DawClients, Result};
use daw_proto::ProjectContext;
use daw_proto::diagnostics::AudioSyncSnapshot;

#[derive(Clone)]
pub struct Probes {
    clients: Arc<DawClients>,
}

impl Probes {
    pub(crate) fn new(clients: Arc<DawClients>) -> Self {
        Self { clients }
    }

    /// Measure the in-process event-bus publish→receive floor. The
    /// probe body runs in a single main-thread dispatched closure,
    /// so no per-sample RPC, IPC, or async scheduling cost is paid.
    /// Returns microseconds per sample. See
    /// [`daw_proto::diagnostics::Diagnostics::hub_publish_latency_us`].
    pub async fn hub_publish_latency(&self, samples: u32) -> Result<Vec<u64>> {
        Ok(self
            .clients
            .diagnostics
            .hub_publish_latency_us(ProjectContext::Current, samples)
            .await?)
    }

    /// Latest snapshot from REAPER's audio thread, if the hook has
    /// fired at least once. Useful for measuring per-buffer state
    /// (sample-accurate playhead, host clock) from out-of-process
    /// observers.
    pub async fn audio_sync_snapshot(&self) -> Result<Option<AudioSyncSnapshot>> {
        Ok(self.clients.diagnostics.audio_sync_snapshot().await?)
    }

    /// Sample N consecutive distinct audio-thread snapshots,
    /// polling at `interval_us` µs between checks. Returns each
    /// unique sequence observed. Use to measure audio buffer rate
    /// or capture a window of per-buffer playhead positions.
    pub async fn audio_sync_observe(
        &self,
        count: u32,
        interval_us: u64,
    ) -> Result<Vec<AudioSyncSnapshot>> {
        Ok(self
            .clients
            .diagnostics
            .audio_sync_observe(count, interval_us)
            .await?)
    }
}

impl std::fmt::Debug for Probes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Probes").finish_non_exhaustive()
    }
}
