//! Signal Protocol — domain types for rig control.
//!
//! Pure domain types where **invalid states are unrepresentable**:
//!
//! - **Typed IDs**: `PresetId` vs `SnapshotId` — can't mix them up
//! - **Refined numerics**: `NormalizedF64`, `Rating`, `MidiNote` — always in range
//! - **Non-empty collections**: `NonEmptyVec<T>` — structurally >= 1 element
//! - **Typestate patterns**: `ActivePreset<Unresolved|Resolved>` — compile-time state machine
//! - **Category hierarchy**: `PresetCategory` enum — can't skip levels

pub mod block;
pub mod category;
pub mod defaults;
pub mod director;
pub mod id;
pub mod layer;
pub mod module;
pub mod module_preset;
pub mod morph;
pub mod non_empty;
pub mod normalized;
pub mod parameter;
pub mod patch;
pub mod performance;
pub mod preset;
pub mod profile;
pub mod rig;
pub mod routing;
pub mod section;
pub mod selection;
pub mod source;
pub mod tags;

// Re-export foundation types at crate root for convenience
pub use id::*;
pub use non_empty::NonEmptyVec;
pub use normalized::*;

// ─────────────────────────────────────────────────────────────────────────────
// compile_fail doctests — prove the type system catches mistakes
// ─────────────────────────────────────────────────────────────────────────────

/// Typed IDs are newtypes around `Uuid`. Passing a `PresetId` where a
/// `RigId` is expected is a compile error.
///
/// ```compile_fail
/// use signal_proto::id::{PresetId, RigId};
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
/// use signal_proto::id::{PresetId, SongId};
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
/// use signal_proto::preset::builder::PresetBuilder;
/// use signal_proto::category::{PresetCategory, BaseTone};
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
/// use signal_proto::preset::builder::PresetBuilder;
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
/// use signal_proto::selection::ActivePreset;
/// use signal_proto::preset::Preset;
/// use signal_proto::category::{PresetCategory, BaseTone};
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
/// use signal_proto::selection::ActivePreset;
/// use signal_proto::preset::Preset;
/// use signal_proto::category::{PresetCategory, BaseTone};
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
/// use signal_proto::selection::ActivePreset;
/// use signal_proto::preset::{Preset, Snapshot};
/// use signal_proto::category::{PresetCategory, BaseTone};
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
