//! `FastTrackStudio` Core Data Types
//!
//! Pure domain types where **invalid states are unrepresentable**.
//!
//! This crate is the foundation of the FTS data model. It has no async,
//! no database, no UI — just clean Rust types with compile-time invariants:
//!
//! - **Typed IDs**: `PresetId` vs `SnapshotId` — can't mix them up
//! - **Refined numerics**: `NormalizedF64`, `Rating`, `MidiNote` — always in range
//! - **Non-empty collections**: `NonEmptyVec<T>` — structurally >= 1 element
//! - **Typestate patterns**: `ActivePreset<Unresolved|Resolved>` — compile-time state machine
//! - **Category hierarchy**: `PresetCategory` enum — can't skip levels

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

// Re-export foundation types at crate root for convenience
pub use id::*;
pub use non_empty::NonEmptyVec;
pub use normalized::*;
