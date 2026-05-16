//! Central event hub for daw-reaper streaming.
//!
//! Owns one [`broadcast::Sender`] per streaming domain. All
//! change-detection / poll sites push into the relevant sender;
//! the streaming-service impls subscribe a `Receiver` per client
//! and forward into the vox `Tx`.
//!
//! See `docs/streaming-design.md` (central hub pattern, occasional
//! vs continuous split). Mirrors helgobox's `ProtoSenders`
//! shape: one struct, one sender per stream, all wired through the
//! same hub instance.
//!
//! # Phase 1 scope
//!
//! Only transport streams are wired today (state + position). The
//! struct is shaped to grow — adding a new domain stream is one
//! field + one `subscribe_*` method + one `publish_*` method.

use std::sync::OnceLock;

use daw_proto::transport::{PositionTick, TransportEvent};
use tokio::sync::broadcast;

// Buffer sizes. Matched to helgobox's defaults for occasional
// channels; continuous channels run smaller because subscribers
// can drop intermediate samples safely.
const OCCASIONAL_BUFFER: usize = 128;
const CONTINUOUS_BUFFER: usize = 16;

/// The hub. One instance per process — fetched via [`hub()`].
///
/// Clone-cheap (each field is a `broadcast::Sender` which is
/// effectively an `Arc` internally), but consumers should reach
/// for [`hub()`] rather than copying fields around.
#[derive(Debug, Clone)]
pub struct DawEventHub {
    // ── Occasional ───────────────────────────────────────────────
    /// Transport state transitions (play/stop/record/tempo/loop).
    transport_state_tx: broadcast::Sender<TransportEvent>,

    // ── Continuous ───────────────────────────────────────────────
    /// Position ticks. Pushed at ~30Hz from the REAPER main loop.
    /// Drop-old semantics on backpressure.
    position_tx: broadcast::Sender<PositionTick>,
}

impl DawEventHub {
    fn new() -> Self {
        Self {
            transport_state_tx: broadcast::channel(OCCASIONAL_BUFFER).0,
            position_tx: broadcast::channel(CONTINUOUS_BUFFER).0,
        }
    }

    /// Subscribe to transport state transitions. The streaming
    /// service impl calls this per-client and forwards into the
    /// vox `Tx`.
    pub fn subscribe_transport_state(&self) -> broadcast::Receiver<TransportEvent> {
        self.transport_state_tx.subscribe()
    }

    /// Subscribe to position ticks.
    pub fn subscribe_position(&self) -> broadcast::Receiver<PositionTick> {
        self.position_tx.subscribe()
    }

    /// Publish a transport state transition. Called from the
    /// change-detection site (REAPER Control Surface) when the
    /// hub observes a transition.
    ///
    /// Returns the number of active subscribers. `Err` is silently
    /// ignored — if there are no subscribers, the event drops.
    pub fn publish_transport_state(&self, event: TransportEvent) {
        let _ = self.transport_state_tx.send(event);
    }

    /// Publish a position tick. Called from the 30Hz polling
    /// callback on the REAPER main thread.
    pub fn publish_position(&self, tick: PositionTick) {
        let _ = self.position_tx.send(tick);
    }

    /// Count of live transport-state subscribers. Polling sites
    /// can check this to skip expensive work when nothing's
    /// listening.
    pub fn transport_state_subscriber_count(&self) -> usize {
        self.transport_state_tx.receiver_count()
    }

    /// Count of live position subscribers. The 30Hz timer uses
    /// this to skip the FFI call when nothing's listening.
    pub fn position_subscriber_count(&self) -> usize {
        self.position_tx.receiver_count()
    }
}

// ── Global accessor ──────────────────────────────────────────────

static HUB: OnceLock<DawEventHub> = OnceLock::new();

/// Get the process-wide hub. First call initializes it. Subsequent
/// calls return the same instance.
///
/// Safe to call from any thread; the hub is read-only after init
/// and broadcast::Sender is `Send + Sync`.
pub fn hub() -> &'static DawEventHub {
    HUB.get_or_init(DawEventHub::new)
}

/// Explicit initialization hook. Optional — `hub()` lazy-inits.
/// Provided so bootstrap code can ensure the hub exists before any
/// subscriber binds, parallel to the existing
/// `init_item_broadcaster` / `init_tempo_map_broadcaster` shape.
pub fn init_event_hub() {
    let _ = hub();
}
