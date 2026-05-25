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

use daw_proto::marker::MarkerStreamEvent;
use daw_proto::project::ProjectStreamEvent;
use daw_proto::region::RegionStreamEvent;
use daw_proto::tempo_map::TempoMapStreamEvent;
use daw_proto::track::TrackStreamEvent;
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
    /// Marker add/remove/modify events.
    markers_tx: broadcast::Sender<MarkerStreamEvent>,
    /// Region add/remove/modify events.
    regions_tx: broadcast::Sender<RegionStreamEvent>,
    /// Track add/remove/modify events.
    tracks_tx: broadcast::Sender<TrackStreamEvent>,
    /// Tempo map point add/remove/modify events.
    tempo_map_tx: broadcast::Sender<TempoMapStreamEvent>,
    /// Project lifecycle (open/close/active-tab switch).
    projects_tx: broadcast::Sender<ProjectStreamEvent>,

    // ── Continuous ───────────────────────────────────────────────
    /// Position ticks. Pushed at ~30Hz from the REAPER main loop.
    /// Drop-old semantics on backpressure.
    position_tx: broadcast::Sender<PositionTick>,
}

impl DawEventHub {
    fn new() -> Self {
        Self {
            transport_state_tx: broadcast::channel(OCCASIONAL_BUFFER).0,
            markers_tx: broadcast::channel(OCCASIONAL_BUFFER).0,
            regions_tx: broadcast::channel(OCCASIONAL_BUFFER).0,
            tracks_tx: broadcast::channel(OCCASIONAL_BUFFER).0,
            tempo_map_tx: broadcast::channel(OCCASIONAL_BUFFER).0,
            projects_tx: broadcast::channel(OCCASIONAL_BUFFER).0,
            position_tx: broadcast::channel(CONTINUOUS_BUFFER).0,
        }
    }

    // ── Transport state ──────────────────────────────────────────

    pub fn subscribe_transport_state(&self) -> broadcast::Receiver<TransportEvent> {
        self.transport_state_tx.subscribe()
    }

    pub fn publish_transport_state(&self, event: TransportEvent) {
        let _ = self.transport_state_tx.send(event);
    }

    pub fn transport_state_subscriber_count(&self) -> usize {
        self.transport_state_tx.receiver_count()
    }

    // ── Position ─────────────────────────────────────────────────

    pub fn subscribe_position(&self) -> broadcast::Receiver<PositionTick> {
        self.position_tx.subscribe()
    }

    pub fn publish_position(&self, tick: PositionTick) {
        let _ = self.position_tx.send(tick);
    }

    pub fn position_subscriber_count(&self) -> usize {
        self.position_tx.receiver_count()
    }

    // ── Markers ──────────────────────────────────────────────────

    pub fn subscribe_markers(&self) -> broadcast::Receiver<MarkerStreamEvent> {
        self.markers_tx.subscribe()
    }

    pub fn publish_marker(&self, event: MarkerStreamEvent) {
        let _ = self.markers_tx.send(event);
    }

    pub fn markers_subscriber_count(&self) -> usize {
        self.markers_tx.receiver_count()
    }

    // ── Regions ──────────────────────────────────────────────────

    pub fn subscribe_regions(&self) -> broadcast::Receiver<RegionStreamEvent> {
        self.regions_tx.subscribe()
    }

    pub fn publish_region(&self, event: RegionStreamEvent) {
        let _ = self.regions_tx.send(event);
    }

    pub fn regions_subscriber_count(&self) -> usize {
        self.regions_tx.receiver_count()
    }

    // ── Tracks ───────────────────────────────────────────────────

    pub fn subscribe_tracks(&self) -> broadcast::Receiver<TrackStreamEvent> {
        self.tracks_tx.subscribe()
    }

    pub fn publish_track(&self, event: TrackStreamEvent) {
        let _ = self.tracks_tx.send(event);
    }

    pub fn tracks_subscriber_count(&self) -> usize {
        self.tracks_tx.receiver_count()
    }

    // ── Tempo map ────────────────────────────────────────────────

    pub fn subscribe_tempo_map(&self) -> broadcast::Receiver<TempoMapStreamEvent> {
        self.tempo_map_tx.subscribe()
    }

    pub fn publish_tempo_map(&self, event: TempoMapStreamEvent) {
        let _ = self.tempo_map_tx.send(event);
    }

    pub fn tempo_map_subscriber_count(&self) -> usize {
        self.tempo_map_tx.receiver_count()
    }

    // ── Projects ─────────────────────────────────────────────────

    pub fn subscribe_projects(&self) -> broadcast::Receiver<ProjectStreamEvent> {
        self.projects_tx.subscribe()
    }

    pub fn publish_project(&self, event: ProjectStreamEvent) {
        let _ = self.projects_tx.send(event);
    }

    pub fn projects_subscriber_count(&self) -> usize {
        self.projects_tx.receiver_count()
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
