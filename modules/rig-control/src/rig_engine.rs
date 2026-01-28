//! Rig engine trait — orchestrator over all module slots.
//!
//! The [`RigEngine`] resolves presets into per-slot targets, manages
//! preloading, handles deferred switching, and calls into individual
//! [`ModuleSlot`]s. It enforces the rule that a preset is **always**
//! loaded with a scene — there is no bare `load_preset()` method.

use async_trait::async_trait;

use crate::engine::{
    EngineError, PresetLoadHandle, PresetReadiness, SwitchOutcome,
};
use crate::module::ModuleType;
use crate::module_preset::ModuleSnapshot;
use crate::performance::Scene;
use crate::preset::Preset;
use crate::rig::Rig;

use super::slot::ModuleSlot;

// ─────────────────────────────────────────────────────────────────────────────
// TransitionResult
// ─────────────────────────────────────────────────────────────────────────────

/// Full result of a preset/scene transition, including any partial failures.
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

// ─────────────────────────────────────────────────────────────────────────────
// RigEngine trait
// ─────────────────────────────────────────────────────────────────────────────

/// Orchestrator that manages all module slots for gapless switching.
///
/// # Design Constraint: Preset Always Has Scene
///
/// There is **no** `load_preset(preset)` method. Only
/// `load_preset_with_scene(preset, scene, rig)`. The existing
/// `RigCommand::LoadPreset` internally constructs a default scene from the
/// preset's first snapshot. This makes it structurally impossible to have
/// an active preset without a scene.
///
/// # Lifecycle
///
/// 1. `initialize(rig)` — creates module slots for each type in the rig
/// 2. `load_preset_with_scene(...)` — first load
/// 3. `switch_scene(...)` — scene changes within the same preset
/// 4. `preload(...)` — background loading for predictive preloading
/// 5. `tick()` — called from 60Hz timer to process preload queue and cleanup
/// 6. `shutdown()` — release all resources
#[async_trait]
pub trait RigEngine: Send + Sync {
    /// Initialize module slots for each module type in the rig.
    ///
    /// Called once when the rig is first loaded. Creates the backend
    /// resources (tracks, channels, etc.) for each module type.
    async fn initialize(&self, rig: &Rig) -> Result<(), EngineError>;

    /// Load a preset with a specific scene.
    ///
    /// This is the primary entry point. Resolves the preset+scene into
    /// per-slot module targets, diffs against current state, and executes
    /// the transition. If target slots aren't preloaded, the switch is
    /// deferred until ready.
    async fn load_preset_with_scene(
        &self,
        preset: &Preset,
        scene: &Scene,
        rig: &Rig,
    ) -> TransitionResult;

    /// Switch scene within the current preset.
    ///
    /// Only changed slots are touched. Slots that don't change between
    /// scenes produce `SlotDiff::NoChange`.
    async fn switch_scene(
        &self,
        scene: &Scene,
        preset: &Preset,
        rig: &Rig,
    ) -> TransitionResult;

    /// Apply a module snapshot (parameter changes only, no instance switching).
    async fn apply_snapshot(
        &self,
        module_type: ModuleType,
        snapshot: &ModuleSnapshot,
    ) -> Result<(), EngineError>;

    /// Preload a preset+scene in the background.
    ///
    /// Returns a handle that can be polled for readiness. When ready,
    /// a subsequent `load_preset_with_scene` for the same target will
    /// complete instantly (gapless).
    async fn preload(
        &self,
        preset: &Preset,
        scene: &Scene,
        rig: &Rig,
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
