//! Named types for Dioxus `Callback` payloads.
//!
//! Replaces anonymous tuples like `(Uuid, u32, f64)` with self-documenting
//! structs so callers and handler sites read clearly.

use signal_control::id::{BlockId, ProfileId, RigPresetId};
use uuid::Uuid;

// ── Parameter control ────────────────────────────────────────────────────────

/// Set a parameter on a UI node (identified by node graph UUID).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SetParameter {
    pub node_id: Uuid,
    pub param_index: u32,
    pub value: f64,
}

/// Set a parameter on a block (identified by node graph UUID, f32 precision).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SetBlockParameter {
    pub node_id: Uuid,
    pub param_index: u32,
    pub value: f32,
}

/// Change a macro value on a block (identified by domain BlockId).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MacroChange {
    pub block_id: BlockId,
    pub param_index: u32,
    pub value: f64,
}

// ── Scene / snapshot selection ───────────────────────────────────────────────

/// Load a scene from a profile (profile ID + scene index).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ProfileSceneSelect {
    pub profile_id: ProfileId,
    pub scene_index: usize,
}

/// Load a snapshot by index from a preset.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PresetSnapshotSelect {
    pub preset_id: RigPresetId,
    pub snapshot_index: usize,
}

/// Load a preset together with a specific snapshot.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PresetWithSnapshot {
    pub preset_id: RigPresetId,
    pub snapshot_id: RigPresetId,
}

// ── Node graph interactions ──────────────────────────────────────────────────

/// Port hover event on a node graph node.
#[derive(Debug, Clone, PartialEq)]
pub struct PortHoverEvent {
    pub node_id: Uuid,
    pub port_name: String,
    pub is_hovering: bool,
}

/// Port drag-start event (port name + is_output).
#[derive(Debug, Clone, PartialEq)]
pub struct PortDragStart {
    pub port_name: String,
    pub is_output: bool,
}

/// Minimap pan offset.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PanOffset {
    pub x: f64,
    pub y: f64,
}
