//! Diagnostic probes that execute entirely on REAPER's main thread.
//!
//! Architect's sync bridge dispatches the whole method to the main
//! thread once, so any tight loop inside the impl runs in-process
//! with zero IPC overhead. Useful for measuring intrinsic latency of
//! csurf / hub / forwarder paths without the cross-process noise the
//! standard subscribe path picks up.

use crate::ProjectContext;
use facet::Facet;

/// Audio-thread snapshot — mirrors `daw_audio_sync::AudioSnapshot` on
/// the wire so cross-process consumers can observe per-buffer state.
#[derive(Clone, Copy, Debug, Default, Facet)]
pub struct AudioSyncSnapshot {
    pub sequence: u64,
    pub host_micros: u64,
    pub playhead_seconds: f64,
    pub sample_rate: f64,
    pub buffer_len: u32,
    pub is_playing: bool,
}

#[architect::rpc]
pub trait Diagnostics {
    /// Measure the in-process event-bus publish→receive floor.
    ///
    /// Subscribes to the hub's broadcast receiver, then for each
    /// sample synthesizes a `VolumeChanged` event, calls
    /// `hub().publish_track(...)`, and spins on `try_recv` until it
    /// arrives. Returns microseconds per sample.
    ///
    /// This measures what an **in-process consumer** (a bridge-side
    /// OSC/MIDI task, an audio-graph node, an inspector tab) pays
    /// once a csurf event has already fired. It excludes:
    ///   - REAPER's csurf callback dispatch (deferred to next main
    ///     loop tick, outside our control)
    ///   - vox encode + IPC (the cross-process subscribe test
    ///     measures that path)
    ///   - tokio worker scheduling (try_recv is synchronous)
    ///
    /// Result demonstrates that the architect facade supports
    /// in-process subscribers at broadcast-channel speed (~1-50µs)
    /// using the same backend impl that serves cross-process
    /// subscribers over vox.
    fn hub_publish_latency_us(&self, project: ProjectContext, samples: u32) -> Vec<u64>;

    /// Latest [`AudioSyncSnapshot`] from REAPER's audio thread, or
    /// `None` if the audio hook hasn't fired yet (no audio engine
    /// running, hook registration failed, REAPER just started).
    fn audio_sync_snapshot(&self) -> Option<AudioSyncSnapshot>;

    /// Sample `count` consecutive [`AudioSyncSnapshot`]s spaced
    /// `interval_us` microseconds apart. Useful for measuring audio
    /// buffer rate from the test side without flooding RPC channels.
    fn audio_sync_observe(&self, count: u32, interval_us: u64) -> Vec<AudioSyncSnapshot>;
}
