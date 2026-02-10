//! Block Library — predefined block types and in-memory preset/snapshot storage.
//!
//! Block types are predefined categories of DSP processing units (EQ, Drive, Amp, etc.).
//! Each block type can hold multiple **presets** (different plugins, chunk-swapped) and
//! each preset can hold multiple **snapshots** (parameter variations, morphable).

use crate::prelude::*;
use signal_control::block::BlockType;
use signal_control::daw_bridge::DawParameterSnapshot;
use std::collections::HashSet;
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// Block Type Definitions
// ─────────────────────────────────────────────────────────────────────────────

/// Display metadata for a predefined block type.
#[derive(Clone)]
pub struct BlockTypeDefinition {
    pub block_type: BlockType,
    pub display_name: &'static str,
    pub icon: &'static str,
    pub color: &'static str,
    pub description: &'static str,
}

/// The predefined block types available in the Block Editor.
pub fn predefined_block_types() -> Vec<BlockTypeDefinition> {
    vec![
        BlockTypeDefinition {
            block_type: BlockType::Eq,
            display_name: "EQ",
            icon: "equalizer",
            color: "text-sky-400",
            description: "Equalization — shape frequency response",
        },
        BlockTypeDefinition {
            block_type: BlockType::Volume,
            display_name: "Boost",
            icon: "trending-up",
            color: "text-lime-400",
            description: "Clean boost — drive the front of your signal",
        },
        BlockTypeDefinition {
            block_type: BlockType::Compressor,
            display_name: "Compressor",
            icon: "activity",
            color: "text-violet-400",
            description: "Dynamics control — tame peaks and add sustain",
        },
        BlockTypeDefinition {
            block_type: BlockType::Drive,
            display_name: "Drive",
            icon: "flame",
            color: "text-orange-400",
            description: "Overdrive / Distortion / Fuzz",
        },
        BlockTypeDefinition {
            block_type: BlockType::Amp,
            display_name: "Amp",
            icon: "speaker",
            color: "text-amber-400",
            description: "Amplifier modeling — tube, solid state, digital",
        },
        BlockTypeDefinition {
            block_type: BlockType::Cabinet,
            display_name: "Cab",
            icon: "box",
            color: "text-yellow-400",
            description: "Cabinet / IR loader — speaker simulation",
        },
    ]
}

// ─────────────────────────────────────────────────────────────────────────────
// In-Memory Library State
// ─────────────────────────────────────────────────────────────────────────────

/// A saved block preset — a specific plugin configuration captured from REAPER.
///
/// Presets for the same block type can have completely different plugins
/// (e.g., "ProQ 3" vs "TDR Nova" are both EQ presets). Swapping between
/// presets uses chunk replacement (the full plugin state is swapped).
#[derive(Debug, Clone)]
pub struct BlockPresetSlot {
    pub id: Uuid,
    pub name: String,
    pub block_type: BlockType,
    /// Plugin name from REAPER (e.g., "Pro-Q 3", "Neural Amp Modeler").
    pub plugin_name: Option<String>,
    /// GUIDs of the FX in REAPER that this preset was captured from.
    pub source_fx_guids: Vec<String>,
    /// Raw RPP FX chunk for full state recall (chunk-swap).
    pub chunk_data: Option<String>,
    /// Parameter snapshots within this preset (same plugin, different knob positions).
    pub snapshots: Vec<BlockSnapshotSlot>,
    /// Timestamp label for display.
    pub created_at: String,
}

/// A parameter snapshot within a block preset.
///
/// Snapshots of the same preset share the same plugin but have different
/// parameter values. Most snapshots are morphable (parameter interpolation),
/// but some (like NAM model switches) are not.
#[derive(Debug, Clone)]
pub struct BlockSnapshotSlot {
    pub id: Uuid,
    pub name: String,
    /// Captured parameter values for this snapshot.
    pub parameter_snapshot: Option<DawParameterSnapshot>,
    /// Whether this snapshot can be morphed with others.
    /// `false` for NAM-style "different model" snapshots that load different IRs.
    pub is_morphable: bool,
}

// ─────────────────────────────────────────────────────────────────────────────
// Global Signals
// ─────────────────────────────────────────────────────────────────────────────

/// All block presets in the library, grouped implicitly by block_type.
pub static BLOCK_LIBRARY: GlobalSignal<Vec<BlockPresetSlot>> = Signal::global(Vec::new);

/// Currently selected block type in the left sidebar.
pub static SELECTED_BLOCK_TYPE: GlobalSignal<Option<BlockType>> = Signal::global(|| None);

/// Currently selected block preset (by UUID).
pub static SELECTED_BLOCK_PRESET: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

/// Currently selected block snapshot (by UUID) within the selected preset.
pub static SELECTED_BLOCK_SNAPSHOT: GlobalSignal<Option<Uuid>> = Signal::global(|| None);

/// FX GUIDs selected in the FX chain tree for block capture.
pub static SELECTED_FX_GUIDS: GlobalSignal<HashSet<String>> = Signal::global(HashSet::new);

/// Status message for the block editor.
pub static BLOCK_EDITOR_STATUS: GlobalSignal<String> =
    Signal::global(|| "Select a block type to begin".to_string());

// Manual PartialEq: compare by UUID only (DawParameterSnapshot doesn't impl PartialEq).
impl PartialEq for BlockPresetSlot {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialEq for BlockSnapshotSlot {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
