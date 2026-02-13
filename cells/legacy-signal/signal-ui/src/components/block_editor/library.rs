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
    pub category: &'static str,
}

/// The predefined block types available in the Block Editor.
///
/// Organized by signal chain order: Input → Dynamics → Drive → Amp → Cab → EQ →
/// Modulation → Motion → Time → Special → Utility.
pub fn predefined_block_types() -> Vec<BlockTypeDefinition> {
    vec![
        // ── Drive ────────────────────────────
        BlockTypeDefinition {
            block_type: BlockType::Boost,
            display_name: "Boost",
            icon: "zap",
            color: "text-orange-300",
            description: "Clean gain stage",
            category: "Drive",
        },
        BlockTypeDefinition {
            block_type: BlockType::Drive,
            display_name: "Drive",
            icon: "flame",
            color: "text-orange-400",
            description: "OD / Distortion / Fuzz",
            category: "Drive",
        },
        // ── Amp + Cab ────────────────────────
        BlockTypeDefinition {
            block_type: BlockType::Amp,
            display_name: "Amp",
            icon: "speaker",
            color: "text-amber-400",
            description: "Amplifier modeling",
            category: "Amp",
        },
        BlockTypeDefinition {
            block_type: BlockType::Cabinet,
            display_name: "Cab",
            icon: "box",
            color: "text-yellow-400",
            description: "Cabinet / IR loader",
            category: "Amp",
        },
        // ── Dynamics ─────────────────────────
        BlockTypeDefinition {
            block_type: BlockType::Compressor,
            display_name: "Compressor",
            icon: "activity",
            color: "text-blue-400",
            description: "Dynamics control",
            category: "Dynamics",
        },
        BlockTypeDefinition {
            block_type: BlockType::Gate,
            display_name: "Gate",
            icon: "shield",
            color: "text-blue-400",
            description: "Noise gate",
            category: "Dynamics",
        },
        BlockTypeDefinition {
            block_type: BlockType::Limiter,
            display_name: "Limiter",
            icon: "shield",
            color: "text-blue-500",
            description: "Peak limiter",
            category: "Dynamics",
        },
        // ── EQ ───────────────────────────────
        BlockTypeDefinition {
            block_type: BlockType::Eq,
            display_name: "EQ",
            icon: "equalizer",
            color: "text-emerald-400",
            description: "Shape frequency response",
            category: "EQ",
        },
        BlockTypeDefinition {
            block_type: BlockType::Crossover,
            display_name: "Crossover",
            icon: "git-branch",
            color: "text-emerald-300",
            description: "Frequency band splitter",
            category: "EQ",
        },
        // ── Modulation ───────────────────────
        BlockTypeDefinition {
            block_type: BlockType::Chorus,
            display_name: "Chorus",
            icon: "waves",
            color: "text-purple-400",
            description: "Chorus modulation",
            category: "Modulation",
        },
        BlockTypeDefinition {
            block_type: BlockType::Flanger,
            display_name: "Flanger",
            icon: "waves",
            color: "text-purple-400",
            description: "Flanger modulation",
            category: "Modulation",
        },
        BlockTypeDefinition {
            block_type: BlockType::Phaser,
            display_name: "Phaser",
            icon: "waves",
            color: "text-purple-400",
            description: "Phase shifting",
            category: "Modulation",
        },
        // ── Motion ───────────────────────────
        BlockTypeDefinition {
            block_type: BlockType::Tremolo,
            display_name: "Tremolo",
            icon: "ripple",
            color: "text-violet-300",
            description: "Volume modulation",
            category: "Motion",
        },
        BlockTypeDefinition {
            block_type: BlockType::Vibrato,
            display_name: "Vibrato",
            icon: "ripple",
            color: "text-violet-300",
            description: "Pitch wobble",
            category: "Motion",
        },
        BlockTypeDefinition {
            block_type: BlockType::Rotary,
            display_name: "Rotary",
            icon: "refresh-cw",
            color: "text-violet-300",
            description: "Leslie cabinet sim",
            category: "Motion",
        },
        // ── Time ─────────────────────────────
        BlockTypeDefinition {
            block_type: BlockType::Delay,
            display_name: "Delay",
            icon: "clock",
            color: "text-cyan-400",
            description: "Echo / tape delay",
            category: "Time",
        },
        BlockTypeDefinition {
            block_type: BlockType::Reverb,
            display_name: "Reverb",
            icon: "cloud",
            color: "text-sky-400",
            description: "Room / hall / plate",
            category: "Time",
        },
        BlockTypeDefinition {
            block_type: BlockType::Freeze,
            display_name: "Freeze",
            icon: "snowflake",
            color: "text-cyan-300",
            description: "Infinite sustain",
            category: "Time",
        },
        // ── Special ──────────────────────────
        BlockTypeDefinition {
            block_type: BlockType::Pitch,
            display_name: "Pitch",
            icon: "trending-up",
            color: "text-indigo-400",
            description: "Pitch shift / harmony",
            category: "Special",
        },
        BlockTypeDefinition {
            block_type: BlockType::Wah,
            display_name: "Wah",
            icon: "move",
            color: "text-pink-400",
            description: "Wah / auto-wah",
            category: "Special",
        },
        BlockTypeDefinition {
            block_type: BlockType::Filter,
            display_name: "Filter",
            icon: "filter",
            color: "text-pink-400",
            description: "Envelope / resonant filter",
            category: "Special",
        },
        BlockTypeDefinition {
            block_type: BlockType::Doubler,
            display_name: "Doubler",
            icon: "copy",
            color: "text-pink-300",
            description: "Stereo doubling",
            category: "Special",
        },
    ]
}

/// Get all unique categories in display order.
pub fn block_type_categories() -> Vec<&'static str> {
    vec![
        "Drive",
        "Amp",
        "Dynamics",
        "EQ",
        "Modulation",
        "Motion",
        "Time",
        "Special",
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
/// Used as a local write-back cache; DB is the source of truth.
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

/// DB-loaded block presets for the currently selected type.
/// This is the primary read source; BLOCK_LIBRARY is the legacy in-memory fallback.
pub static DB_BLOCK_PRESETS: GlobalSignal<Vec<signal_control::block_preset::Model>> =
    Signal::global(Vec::new);

/// DB-loaded block snapshots for the currently selected preset.
pub static DB_BLOCK_SNAPSHOTS: GlobalSignal<Vec<signal_control::block_snapshot::Model>> =
    Signal::global(Vec::new);

/// Per-type preset counts from DB (for badge display in the type browser).
pub static DB_BLOCK_TYPE_COUNTS: GlobalSignal<std::collections::HashMap<String, usize>> =
    Signal::global(std::collections::HashMap::new);

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
