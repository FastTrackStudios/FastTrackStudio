//! Rig engine trait — orchestrator over all module slots.
//!
//! The [`RigEngine`] resolves scene references into per-slot targets, manages
//! preloading, handles deferred switching, and calls into individual
//! [`ModuleSlot`]s.
//!
//! # Scene-Centric API
//!
//! All operations take a `ScopedSceneRef` (pointing at any hierarchy level)
//! plus a `SceneStore` for lookups. There is no separate "load preset" vs
//! "switch scene" — both are just `load_scene()` with a different ref.

use async_trait::async_trait;

use crate::engine::{EngineError, PresetLoadHandle, PresetReadiness, SwitchOutcome};
use crate::module::ModuleType;
use crate::override_tree::{SceneOverride, Validated};
use crate::rig::Rig;
use crate::scene::ScopedSceneRef;
use crate::snapshot::ModuleSnapshot;
use crate::stores::SceneStore;

use super::slot::ModuleSlot;

// ─── TransitionResult ────────────────────────────────────────────

/// Full result of a scene transition, including any partial failures.
#[derive(Debug)]
pub struct TransitionResult {
    /// Overall outcome — completed, pending, or failed.
    pub outcome: SwitchOutcome,
    /// Per-slot errors that occurred during the transition (non-fatal).
    /// The engine kept the previous active instance for these slots.
    pub slot_errors: Vec<(ModuleType, EngineError)>,
}

impl TransitionResult {
    /// Create a successful result with no errors.
    pub fn completed() -> Self {
        Self {
            outcome: SwitchOutcome::Completed,
            slot_errors: Vec::new(),
        }
    }

    /// Create a pending result.
    pub fn pending(handle: PresetLoadHandle, readiness: PresetReadiness) -> Self {
        Self {
            outcome: SwitchOutcome::Pending { handle, readiness },
            slot_errors: Vec::new(),
        }
    }

    /// Create a failed result.
    pub fn failed(reason: impl Into<String>) -> Self {
        Self {
            outcome: SwitchOutcome::Failed {
                reason: reason.into(),
            },
            slot_errors: Vec::new(),
        }
    }

    /// Whether the transition completed with no errors at all.
    pub fn is_clean(&self) -> bool {
        self.outcome.is_completed() && self.slot_errors.is_empty()
    }

    /// Whether any slots had errors (even if the transition completed).
    pub fn has_slot_errors(&self) -> bool {
        !self.slot_errors.is_empty()
    }
}

// ─── RigEngine trait ─────────────────────────────────────────────

/// Orchestrator that manages all module slots for gapless switching.
///
/// # Scene-Centric Design
///
/// Every operation takes a `ScopedSceneRef` which points at any level
/// of the hierarchy (Layer, Engine, Rig, Rack). The engine + resolver
/// walk the ref down to LayerScene level, correlate with the physical
/// hierarchy, and produce per-slot `ModuleTarget`s.
///
/// # Lifecycle
///
/// 1. `initialize(rig)` — creates module slots for each type in the rig
/// 2. `load_scene(...)` — load a scene (initial or switch)
/// 3. `preload(...)` — background loading for predictive preloading
/// 4. `tick()` — called from 60Hz timer to process preload queue and cleanup
/// 5. `shutdown()` — release all resources
#[async_trait]
pub trait RigEngine: Send + Sync {
    /// Initialize module slots for each module type in the rig.
    ///
    /// Called once when the rig is first loaded. Creates the backend
    /// resources (tracks, channels, etc.) for each module type.
    async fn initialize(&self, rig: &Rig) -> Result<(), EngineError>;

    /// Load a scene — resolve, diff, and execute the transition.
    ///
    /// This is the primary entry point for both initial loads and scene
    /// switches. The resolver walks `scene_ref` down to LayerScene level,
    /// correlates with the physical `rig`, looks up data via `store`,
    /// applies `overrides`, then diffs against current state and executes.
    async fn load_scene(
        &self,
        scene_ref: &ScopedSceneRef,
        rig: &Rig,
        store: &dyn SceneStore,
        overrides: &[SceneOverride<Validated>],
    ) -> TransitionResult;

    /// Apply a module snapshot (parameter changes only, no instance switching).
    async fn apply_snapshot(
        &self,
        module_type: ModuleType,
        snapshot: &ModuleSnapshot,
    ) -> Result<(), EngineError>;

    /// Preload a scene in the background.
    ///
    /// Returns a handle that can be polled for readiness. When ready,
    /// a subsequent `load_scene` for the same target will complete
    /// instantly (gapless).
    async fn preload(
        &self,
        scene_ref: &ScopedSceneRef,
        rig: &Rig,
        store: &dyn SceneStore,
    ) -> PresetLoadHandle;

    /// Check the readiness of a preload operation.
    fn check_readiness(&self, handle: PresetLoadHandle) -> PresetReadiness;

    /// Wait until a preload operation completes.
    async fn wait_ready(&self, handle: PresetLoadHandle);

    /// Get a reference to a specific module slot.
    fn slot(&self, module_type: ModuleType) -> Option<&dyn ModuleSlot>;

    /// Periodic tick — process preload queue, cleanup tails, etc.
    ///
    /// Called from a 60Hz timer (REAPER) or tokio interval (standalone).
    async fn tick(&self);

    /// Shut down the engine, releasing all resources.
    async fn shutdown(&self);
}
