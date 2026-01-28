//! Rig Control — core data types and gapless module engine for
//! `FastTrackStudio`.
//!
//! # Data Types
//!
//! Pure domain types where **invalid states are unrepresentable**:
//!
//! - **Typed IDs**: `PresetId` vs `SnapshotId` — can't mix them up
//! - **Refined numerics**: `NormalizedF64`, `Rating`, `MidiNote` — always in range
//! - **Non-empty collections**: `NonEmptyVec<T>` — structurally >= 1 element
//! - **Typestate patterns**: `ActivePreset<Unresolved|Resolved>` — compile-time state machine
//! - **Category hierarchy**: `PresetCategory` enum — can't skip levels
//!
//! # Engine
//!
//! ```text
//! RigEngine (orchestrator)
//! ├── ModuleSlot<Source>      ── parallel plugin instances
//! ├── ModuleSlot<Drive>       ── parallel plugin instances
//! ├── ModuleSlot<Amp>         ── parallel plugin instances
//! ├── ...
//! └── ModuleSlot<Master>      ── parallel plugin instances
//! ```
//!
//! Each `ModuleSlot` manages N instances with a state machine:
//! `Loading → Ready → Active → Tailing → Unloaded`.
//!
//! The engine resolves (preset, scene, overrides) → per-slot `ModuleTarget`s,
//! diffs against current state, and executes the minimal set of changes.
//!
//! # Engine Modules
//!
//! - [`resolver`] — pure function: (preset, scene, overrides) → per-slot targets
//! - [`diff`] — pure function: (current, new) → `Vec<SlotDiff>`
//! - [`slot`] — `ModuleSlot` trait (per-module-type engine)
//! - [`rig_engine`] — `RigEngine` trait (orchestrator)
//! - [`mock`] — in-memory implementations for testing

// ── Data types ──────────────────────────────────────────────────────────────
pub mod id;
pub mod non_empty;
pub mod normalized;
pub mod category;
pub mod tags;
pub mod parameter;
pub mod block;
pub mod routing;
pub mod layer;
pub mod section;
pub mod patch;
pub mod preset;
pub mod profile;
pub mod performance;
pub mod selection;
pub mod module;
pub mod module_preset;
pub mod source;
pub mod rig;
pub mod director;
pub mod defaults;

// Re-export foundation types at crate root for convenience
pub use id::*;
pub use non_empty::NonEmptyVec;
pub use normalized::*;

// ── Engine ──────────────────────────────────────────────────────────────────
pub mod engine;
pub mod diff;
pub mod mock;
pub mod resolver;
pub mod rig_engine;
pub mod slot;

// Re-export core engine data types.
pub use engine::{
    EngineError, InstanceHandle, InstanceState, ModuleTarget, PreloadPriority, PreloadRequest,
    PresetLoadHandle, PresetReadiness, SlotDiff, SwitchOutcome,
};

// Re-export engine traits.
pub use rig_engine::{RigEngine, TransitionResult};
pub use slot::{ActivateResult, LoadResult, ModuleSlot};

// ─────────────────────────────────────────────────────────────────────────────
// compile_fail doctests — prove the type system catches mistakes
// ─────────────────────────────────────────────────────────────────────────────

/// Typed IDs are newtypes around `Uuid`. Passing a `PresetId` where a
/// `RigId` is expected is a compile error.
///
/// ```compile_fail
/// use rig_control::id::{PresetId, RigId};
///
/// fn needs_rig(_id: RigId) {}
///
/// let preset_id = PresetId::new();
/// needs_rig(preset_id); // ERROR: expected `RigId`, found `PresetId`
/// ```
#[allow(dead_code)]
const _TYPED_IDS_INCOMPATIBLE: () = ();

/// Comparing across ID types is a compile error.
///
/// ```compile_fail
/// use rig_control::id::{PresetId, SongId};
///
/// let a = PresetId::new();
/// let b = SongId::new();
/// let _ = a == b; // ERROR: mismatched types
/// ```
#[allow(dead_code)]
const _TYPED_IDS_NO_CROSS_COMPARE: () = ();

/// `PresetBuilder` requires `.name()` before `.category()`.
///
/// ```compile_fail
/// use rig_control::preset::builder::PresetBuilder;
/// use rig_control::category::{PresetCategory, BaseTone};
///
/// let _preset = PresetBuilder::new()
///     .category(PresetCategory::Generic { base_tone: BaseTone::Clean })
///     .build();
/// ```
#[allow(dead_code)]
const _BUILDER_NEEDS_NAME: () = ();

/// `PresetBuilder` requires `.category()` before `.build()`.
///
/// ```compile_fail
/// use rig_control::preset::builder::PresetBuilder;
///
/// let _preset = PresetBuilder::new()
///     .name("My Preset")
///     .build();
/// ```
#[allow(dead_code)]
const _BUILDER_NEEDS_CATEGORY: () = ();

/// `ActivePreset<Unresolved>` has no `.snapshot()` method.
///
/// ```compile_fail
/// use rig_control::selection::ActivePreset;
/// use rig_control::preset::Preset;
/// use rig_control::category::{PresetCategory, BaseTone};
///
/// let preset = Preset::new("Test", PresetCategory::Generic {
///     base_tone: BaseTone::Clean,
/// });
/// let sel = ActivePreset::new(preset);
/// let _ = sel.snapshot(); // ERROR: method not found
/// ```
#[allow(dead_code)]
const _UNRESOLVED_NO_SNAPSHOT: () = ();

/// `ActivePreset<Unresolved>` has no `.snapshot_id()` method.
///
/// ```compile_fail
/// use rig_control::selection::ActivePreset;
/// use rig_control::preset::Preset;
/// use rig_control::category::{PresetCategory, BaseTone};
///
/// let preset = Preset::new("Test", PresetCategory::Generic {
///     base_tone: BaseTone::Clean,
/// });
/// let sel = ActivePreset::new(preset);
/// let _ = sel.snapshot_id(); // ERROR: method not found
/// ```
#[allow(dead_code)]
const _UNRESOLVED_NO_SNAPSHOT_ID: () = ();

/// Calling `.unresolve()` moves back to `Unresolved`, removing
/// snapshot access again.
///
/// ```compile_fail
/// use rig_control::selection::ActivePreset;
/// use rig_control::preset::{Preset, Snapshot};
/// use rig_control::category::{PresetCategory, BaseTone};
///
/// let mut preset = Preset::new("Test", PresetCategory::Generic {
///     base_tone: BaseTone::Clean,
/// });
/// let snap = Snapshot::new("Verse");
/// let snap_id = snap.id;
/// preset.add_snapshot(snap);
///
/// let sel = ActivePreset::new(preset);
/// let resolved = sel.resolve(snap_id).unwrap();
/// let unresolved = resolved.unresolve();
/// let _ = unresolved.snapshot(); // ERROR: back to Unresolved
/// ```
#[allow(dead_code)]
const _UNRESOLVE_REMOVES_ACCESS: () = ();
