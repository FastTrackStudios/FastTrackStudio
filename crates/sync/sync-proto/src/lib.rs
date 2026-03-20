//! Sync protocol — types and service definitions for DAW-to-DAW synchronization.
//!
//! This crate defines the shared types for the sync engine:
//! - [`SyncSession`] — session identity (peer ID, session name)
//! - [`SyncEvent`] — unified envelope wrapping all domain events with origin peer + sequence
//! - [`SyncDomain`] — discriminated union of all DAW event domains
//! - [`SyncConfig`] — what to sync and conflict resolution policy
//! - [`SyncPeer`] — connected peer state
//! - [`SyncService`] — join/leave/configure session, subscribe to sync events

pub mod actions;

use daw::service::{
    FxEvent, ItemEvent, MarkerEvent, ProjectEvent, RegionEvent, RoutingEvent, TakeEvent,
    TempoMapEvent, TrackEvent, Transport,
};
use facet::Facet;
use roam::{Tx, service};

// ── Session Identity ─────────────────────────────────────────────────────────

/// Identifies a sync session and the local peer within it.
#[derive(Clone, Debug, Facet)]
pub struct SyncSession {
    /// Unique identifier for the sync session (shared by all peers)
    pub session_id: String,
    /// Unique identifier for this peer within the session
    pub peer_id: String,
    /// Human-readable name for this peer (e.g. "Studio A", "Cody's Laptop")
    pub display_name: String,
}

// ── Peer State ───────────────────────────────────────────────────────────────

/// State of a connected peer in the sync session.
#[derive(Clone, Debug, Facet)]
pub struct SyncPeer {
    /// Peer's unique identifier
    pub peer_id: String,
    /// Human-readable display name
    pub display_name: String,
    /// Whether this peer is the session host (first to join)
    pub is_host: bool,
    /// Peer's current sync config
    pub config: SyncConfig,
}

// ── Sync Configuration ──────────────────────────────────────────────────────

/// Configuration for what domains to synchronize and how to resolve conflicts.
#[derive(Clone, Debug, Facet)]
pub struct SyncConfig {
    /// Sync transport state (play/pause/stop, position, tempo)
    pub transport: bool,
    /// Sync track state (volume, pan, mute, solo, arm, add/remove)
    pub tracks: bool,
    /// Sync FX state (parameters, enable/bypass, add/remove, presets)
    pub fx: bool,
    /// Sync item state (position, length, add/remove)
    pub items: bool,
    /// Sync routing state (sends, receives, hardware outputs)
    pub routing: bool,
    /// Sync tempo map changes
    pub tempo_map: bool,
    /// Sync markers
    pub markers: bool,
    /// Sync regions
    pub regions: bool,
    /// Conflict resolution policy
    pub conflict_policy: ConflictPolicy,
}

impl SyncConfig {
    /// Create a config that syncs everything with last-write-wins.
    pub fn all() -> Self {
        Self {
            transport: true,
            tracks: true,
            fx: true,
            items: true,
            routing: true,
            tempo_map: true,
            markers: true,
            regions: true,
            conflict_policy: ConflictPolicy::LastWriteWins,
        }
    }

    /// Create a config that syncs only transport (simplest mode).
    pub fn transport_only() -> Self {
        Self {
            transport: true,
            tracks: false,
            fx: false,
            items: false,
            routing: false,
            tempo_map: false,
            markers: false,
            regions: false,
            conflict_policy: ConflictPolicy::LastWriteWins,
        }
    }
}

/// How to resolve conflicting edits from multiple peers.
#[repr(u8)]
#[derive(Clone, Debug, Facet)]
pub enum ConflictPolicy {
    /// Most recent edit wins (using sequence numbers). Simple, predictable.
    LastWriteWins,
    /// Host peer's edits always take priority over other peers.
    HostPriority,
}

// ── Sync Domain (event wrapper) ─────────────────────────────────────────────

/// Discriminated union of all DAW event domains.
///
/// Each variant wraps the corresponding daw-proto event type. This lets the
/// sync engine handle all domain events uniformly — stamp with origin peer,
/// sequence number, and project GUID, then forward over the wire.
#[repr(u8)]
#[derive(Clone, Debug, Facet)]
pub enum SyncDomain {
    /// Transport state snapshot (play/pause/stop, position, tempo, etc.)
    Transport(Transport),
    /// Track state change (add/remove, volume, pan, mute, etc.)
    Track(TrackEvent),
    /// FX chain change (parameter, enable, add/remove, preset)
    Fx(FxEvent),
    /// Item change (position, length, add/remove)
    Item(ItemEvent),
    /// Take change (name, pitch, rate, source)
    Take(TakeEvent),
    /// Routing change (send/receive add/remove, volume, pan)
    Routing(RoutingEvent),
    /// Tempo map change (point add/remove/modify)
    TempoMap(TempoMapEvent),
    /// Marker change (add/remove/modify)
    Marker(MarkerEvent),
    /// Region change (add/remove/modify)
    Region(RegionEvent),
    /// Project-level change (open/close, current project)
    Project(ProjectEvent),
}

// ── Sync Event Envelope ─────────────────────────────────────────────────────

/// A sync event envelope — wraps a domain event with origin tracking.
///
/// Every change detected locally gets wrapped in a `SyncEvent` before being
/// sent to peers. Remote peers use the `origin_peer` to filter echoes and
/// the `sequence` for ordering/conflict resolution.
#[derive(Clone, Debug, Facet)]
pub struct SyncEvent {
    /// Peer that originated this change
    pub origin_peer: String,
    /// Monotonically increasing sequence number from the origin peer
    pub sequence: u64,
    /// GUID of the project this event applies to
    pub project_guid: String,
    /// The domain-specific event payload
    pub domain: SyncDomain,
    /// Wall-clock timestamp (ms since Unix epoch) when this event was created.
    ///
    /// Used by the drift corrector to estimate the master's current position
    /// at the time the follower processes a heartbeat, compensating for
    /// network + RPC latency.
    pub created_at_ms: u64,
}

impl SyncEvent {
    /// Current wall-clock time in milliseconds since Unix epoch.
    pub fn now_ms() -> u64 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as u64
    }
}

// ── Sync Status ─────────────────────────────────────────────────────────────

/// Current status of the sync engine.
#[repr(u8)]
#[derive(Clone, Debug, Facet)]
pub enum SyncStatus {
    /// Not in a sync session
    Disconnected,
    /// Joining a session (handshake in progress)
    Connecting,
    /// Connected and actively syncing
    Connected,
    /// Performing initial state sync (late join)
    InitialSync,
}

// ── Service Trait ────────────────────────────────────────────────────────────

/// RPC service for managing sync sessions.
///
/// Implementations handle session lifecycle (join/leave), peer discovery,
/// configuration, and event streaming.
#[service]
pub trait SyncService {
    /// Join a sync session.
    ///
    /// Returns the list of currently connected peers (including self).
    /// If the session doesn't exist yet, the joining peer becomes the host.
    async fn join_session(&self, session: SyncSession, config: SyncConfig) -> Vec<SyncPeer>;

    /// Leave the current sync session.
    ///
    /// Stops all event forwarding. If the leaving peer is the host,
    /// host status transfers to the next peer (by join order).
    async fn leave_session(&self);

    /// Get all currently connected peers.
    async fn get_peers(&self) -> Vec<SyncPeer>;

    /// Get the current sync status.
    async fn get_status(&self) -> SyncStatus;

    /// Update the local sync configuration.
    ///
    /// Takes effect immediately — newly disabled domains stop syncing,
    /// newly enabled domains begin syncing from current state.
    async fn update_config(&self, config: SyncConfig);

    /// Subscribe to sync events from all peers.
    ///
    /// Receives all remote sync events (local events are filtered out).
    /// The stream continues until the session ends or the subscriber disconnects.
    async fn subscribe_events(&self, tx: Tx<SyncEvent>);

    /// Request a full state snapshot from the host peer.
    ///
    /// Used for late-join initial sync. The host peer responds by sending
    /// a burst of SyncEvents covering the full project state through the
    /// subscribe_events stream.
    async fn request_full_state(&self, project_guid: String);
}
