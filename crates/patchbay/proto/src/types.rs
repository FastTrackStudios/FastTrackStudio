//! Graph, preset, and status types shared across the wire.

use facet::Facet;
use serde::{Deserialize, Serialize};

// ─── Graph model ────────────────────────────────────────────────────────

/// What kind of media flows through a node/port. Derived from
/// `media.class` the way helvum does it (substring match); `Other`
/// covers control/metadata nodes we still want visible.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub enum MediaKind {
    Audio,
    Video,
    Midi,
    Other,
}

/// Port direction as PipeWire reports it (`port.direction`).
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub enum PortDirection {
    Input,
    Output,
}

/// A PipeWire node (device, stream, virtual sink/source, …).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct PwNode {
    /// PipeWire global id (unstable across restarts — never persist).
    pub id: u32,
    /// `node.name` — the stable identity used by presets/aliases.
    pub name: String,
    /// Display label: `node.nick` → `node.description` → `node.name`.
    pub label: String,
    /// Raw `media.class` (e.g. `Audio/Sink`, `Stream/Output/Audio`).
    pub media_class: String,
    pub media_kind: MediaKind,
}

/// A port on a node.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct PwPort {
    /// PipeWire global id.
    pub id: u32,
    /// Owning node's global id.
    pub node_id: u32,
    /// `port.name` (e.g. `playback_97`, `capture_FL`) — stable identity.
    pub name: String,
    pub direction: PortDirection,
    pub media_kind: MediaKind,
}

/// A link between an output port and an input port.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct PwLink {
    /// PipeWire global id.
    pub id: u32,
    pub output_node: u32,
    pub output_port: u32,
    pub input_node: u32,
    pub input_port: u32,
    /// Whether the link is in `Active` state (data flowing).
    pub active: bool,
}

/// Complete graph snapshot — what a client renders from on connect;
/// afterwards it applies [`GraphEvent`]s incrementally.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize, Facet)]
pub struct GraphSnapshot {
    pub nodes: Vec<PwNode>,
    pub ports: Vec<PwPort>,
    pub links: Vec<PwLink>,
}

/// Incremental graph change, streamed via `#[subscribe]`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum GraphEvent {
    NodeAdded(PwNode),
    NodeRemoved { id: u32 },
    PortAdded(PwPort),
    PortRemoved { id: u32, node_id: u32 },
    LinkAdded(PwLink),
    LinkStateChanged { id: u32, active: bool },
    LinkRemoved { id: u32 },
}

// SelfRef compatibility: GraphEvent has no lifetime parameters, so Ref<'a> = Self.
#[allow(unsafe_code)]
unsafe impl vox_types::Reborrow for GraphEvent {
    type Ref<'a> = GraphEvent;
}

// ─── Presets (connection memory) ────────────────────────────────────────

/// One remembered connection, keyed by stable names (never global ids).
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize, Facet)]
pub struct PresetLink {
    pub output_node: String,
    pub output_port: String,
    pub input_node: String,
    pub input_port: String,
}

/// A named routing preset — a saved set of connections that can be
/// re-applied later (missing endpoints are reported, not fatal).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct RoutingPreset {
    pub name: String,
    pub description: String,
    pub links: Vec<PresetLink>,
}

/// What happened when a preset was applied.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize, Facet)]
pub struct ApplyReport {
    /// Links newly created.
    pub created: u32,
    /// Links that already existed.
    pub existing: u32,
    /// Links whose endpoints aren't currently in the graph.
    pub missing: Vec<PresetLink>,
    /// Links destroyed (exclusive mode only).
    pub destroyed: u32,
}

// ─── Aliases (pretty names) ─────────────────────────────────────────────

/// Display alias for a node (`target = node.name`) or a port
/// (`target = "node.name:port.name"`). Pure presentation — PipeWire
/// names are never rewritten, so nothing else on the system breaks.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct AliasEntry {
    pub target: String,
    pub alias: String,
}

// ─── Clock / latency ────────────────────────────────────────────────────

/// Live graph clock settings (from `pw-metadata -n settings`).
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize, Facet)]
pub struct ClockInfo {
    pub rate: u32,
    pub quantum: u32,
    /// Forced quantum, `0` when automatic.
    pub force_quantum: u32,
    /// Forced rate, `0` when automatic.
    pub force_rate: u32,
    pub min_quantum: u32,
    pub max_quantum: u32,
}

// ─── Dante / Inferno stack ──────────────────────────────────────────────

/// One systemd unit's state within the Dante stack.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct UnitStatus {
    pub unit: String,
    /// `active` / `inactive` / `failed` / `activating` / …
    pub state: String,
}

/// State of the `dante.target` AoIP stack on this host.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize, Facet)]
pub struct DanteStatus {
    /// Whether `dante.target` exists on this host at all.
    pub installed: bool,
    /// Whether the target is active.
    pub active: bool,
    pub units: Vec<UnitStatus>,
}
