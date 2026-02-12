//! Signal Protocol — domain types for rig control.
//!
//! Pure domain types where **invalid states are unrepresentable**:
//!
//! - **Typed IDs**: `BlockId` vs `EngineId` — can't mix them up
//! - **Refined numerics**: `NormalizedF64`, `Rating`, `MidiNote` — always in range
//! - **Non-empty collections**: `NonEmptyVec<T>` — structurally >= 1 element
//! - **Typestate patterns**: `ActiveBlock<Unresolved|Resolved>`, `Version<T, Draft|Committed>`
//! - **1-based indexing**: `LayerIndex` — cannot be 0
//! - **Snapshot forking**: `BlockSnapshot<Original|Forked>` — compile-time parent link safety
//! - **Override validation**: `SceneOverride<Validated>` — only validated overrides in sections
//!
//! ## Hierarchy
//!
//! **Physical**: Director → Rack → Rig → Engine → Layer → Module → Block
//!
//! **State**: Block Snapshots → Module Snapshots → Layer Scenes → Engine Scenes → Rig Scenes → Rack Scenes
//!
//! **Performance**: Songs → Sections (with validated overrides)

#[macro_use(derive)]
extern crate derive_aliases;

mod derive_alias;

// ─── Core ────────────────────────────────────────────────────────
pub mod category;
pub mod id;
pub mod non_empty;
pub mod normalized;
pub mod parameter;
pub mod tags;

// ─── Physical hierarchy ──────────────────────────────────────────
pub mod block;
pub mod director;
pub mod engine;
pub mod layer;
pub mod module;
pub mod rack;
pub mod rig;

// ─── State hierarchy ─────────────────────────────────────────────
pub mod scene;
pub mod snapshot;
pub mod version;

// ─── Active resolution ───────────────────────────────────────────
pub mod active;
pub mod override_tree;

// ─── Presets ─────────────────────────────────────────────────────
pub mod preset;

// ─── Performance ─────────────────────────────────────────────────
pub mod profile;
pub mod song;

// ─── Templates ───────────────────────────────────────────────────
pub mod template;

// ─── Utilities ───────────────────────────────────────────────────
pub mod container;
pub mod routing;
pub mod source;

// ─── Defaults ────────────────────────────────────────────────────
pub mod defaults;

// ─── Temporarily disabled — depend on old domain types ───────────
// These modules need rewriting to use the new BlockSnapshot/Scene/Override system.
// pub mod crossfade;
// pub mod morph;
// pub mod patch;

// Re-export foundation types at crate root for convenience
pub use id::*;
pub use non_empty::NonEmptyVec;
pub use normalized::*;
