//! Gapless Module Engine — orchestrates per-slot plugin instances for
//! zero-gap preset/scene/snapshot switching.
//!
//! # Architecture
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
//! # Modules
//!
//! - [`resolver`] — pure function: (preset, scene, overrides) → per-slot targets
//! - [`diff`] — pure function: (current, new) → `Vec<SlotDiff>`
//! - [`slot`] — `ModuleSlot` trait (per-module-type engine)
//! - [`rig_engine`] — `RigEngine` trait (orchestrator)
//! - [`mock`] — in-memory implementations for testing

pub mod diff;
pub mod mock;
pub mod resolver;
pub mod rig_engine;
pub mod slot;

// Re-export core engine data types from the data crate.
pub use data::engine::{
    EngineError, InstanceHandle, InstanceState, ModuleTarget, PreloadPriority, PreloadRequest,
    PresetLoadHandle, PresetReadiness, SlotDiff, SwitchOutcome,
};

// Re-export engine traits.
pub use rig_engine::{RigEngine, TransitionResult};
pub use slot::{ActivateResult, LoadResult, ModuleSlot};
