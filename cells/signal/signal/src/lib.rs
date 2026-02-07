//! Signal — gapless module engine and ROAM service implementation
//! for rig control.
//!
//! Domain types live in [`signal_proto`]. This crate re-exports them
//! for convenience and adds the engine + service layer on top.
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

// ── Domain types (re-exported from signal-proto) ────────────────────────────
pub use signal_proto::{
    block, category, defaults, director, id, layer, module, module_preset, non_empty, normalized,
    parameter, patch, performance, preset, profile, rig, routing, section, selection, source, tags,
};

// Re-export foundation types at crate root for convenience
pub use signal_proto::id::*;
pub use signal_proto::non_empty::NonEmptyVec;
pub use signal_proto::normalized::*;

// ── Engine ──────────────────────────────────────────────────────────────────
pub mod diff;
pub mod engine;
pub mod mock;
pub mod resolver;
pub mod rig_engine;
pub mod slot;

// ── Service (ROAM) ─────────────────────────────────────────────────────────
pub mod roam_test;
pub mod service;

// Re-export core engine data types.
pub use engine::{
    EngineError, InstanceHandle, InstanceState, ModuleTarget, PreloadPriority, PreloadRequest,
    PresetLoadHandle, PresetReadiness, SlotDiff, SwitchOutcome,
};

// Re-export engine traits.
pub use rig_engine::{RigEngine, TransitionResult};
pub use slot::{ActivateResult, LoadResult, ModuleSlot};

// Re-export service types.
pub use service::{
    EngineStateInfo, InstanceInfo, MockRigControlService, PreloadStatusInfo, PresetInfo,
    PresetSnapshotInfo, ProfileInfo, ProfileSceneInfo, RigControlCommand, RigControlData,
    RigControlEvent, RigControlService, RigInfo, SetlistInfo, SlotErrorInfo, SlotStateInfo,
    SongInfo, SwitchOutcomeInfo, TransitionResultInfo,
};
