//! Module slot trait — per-module-type engine managing parallel plugin instances.
//!
//! Each [`ModuleSlot`] owns one processing stage (e.g. Drive, Amp, EQ) and
//! manages N parallel plugin instances for gapless switching. Only one
//! instance is `Active` at a time; others may be `Loading`, `Ready`,
//! `Tailing`, or `Unloaded`.

use async_trait::async_trait;

use crate::engine::{EngineError, InstanceHandle, InstanceState, ModuleTarget};
use crate::module::ModuleType;
use crate::preset::ModulePreset;
use crate::snapshot::ModuleSnapshot;

// ─────────────────────────────────────────────────────────────────────────────
// LoadResult
// ─────────────────────────────────────────────────────────────────────────────

/// Result of loading a module preset into a slot.
#[derive(Debug)]
pub enum LoadResult {
    /// Instance was loaded successfully. Use the handle to activate it.
    Loaded(InstanceHandle),
    /// The exact same preset+snapshot is already loaded in this slot.
    AlreadyLoaded(InstanceHandle),
    /// Loading failed.
    Failed(EngineError),
}

impl LoadResult {
    /// Get the instance handle, if the load succeeded.
    pub fn handle(&self) -> Option<InstanceHandle> {
        match self {
            Self::Loaded(h) | Self::AlreadyLoaded(h) => Some(*h),
            Self::Failed(_) => None,
        }
    }

    /// Whether the load succeeded (either new or already loaded).
    pub fn is_ok(&self) -> bool {
        !matches!(self, Self::Failed(_))
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ActivateResult
// ─────────────────────────────────────────────────────────────────────────────

/// Result of activating an instance (gapless switch).
#[derive(Debug)]
pub enum ActivateResult {
    /// Instance is now active. The previous active instance (if any) is now tailing.
    Activated {
        /// The newly active instance.
        new_active: InstanceHandle,
        /// The previous instance, now in Tailing state (if there was one).
        previous: Option<InstanceHandle>,
    },
    /// Activation failed.
    Failed(EngineError),
}

impl ActivateResult {
    /// Whether activation succeeded.
    pub fn is_ok(&self) -> bool {
        matches!(self, Self::Activated { .. })
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ModuleSlot trait
// ─────────────────────────────────────────────────────────────────────────────

/// A single processing stage engine managing parallel plugin instances.
///
/// Backends implement this trait for each module type in the rig. The engine
/// orchestrator calls these methods to load, activate, snapshot, and clean up
/// instances. Each slot tracks N instances in different lifecycle states.
///
/// # Gapless Switching
///
/// When switching presets:
/// 1. `load()` creates a new instance in `Loading` → `Ready` state
/// 2. `activate()` unmutes the new instance and sets the old one to `Tailing`
/// 3. `cleanup_tails()` (called from `tick()`) unloads tailing instances
///    whose audio has gone silent
///
/// # Backend Implementations
///
/// - **REAPER**: 128-channel folder track with child tracks per instance.
///   Send muting controls which instance is audible.
/// - **Mock**: In-memory with configurable simulated load delays.
/// - **Standalone** (future): Direct plugin hosting without DAW.
#[async_trait]
pub trait ModuleSlot: Send + Sync {
    /// The module type this slot manages.
    fn module_type(&self) -> ModuleType;

    /// Load a module preset into this slot.
    ///
    /// If the exact same preset+snapshot is already loaded, returns
    /// `AlreadyLoaded`. Otherwise creates a new instance that transitions
    /// through `Loading` → `Ready`.
    async fn load(&self, target: &ModuleTarget, preset: &ModulePreset) -> LoadResult;

    /// Gapless switch: unmute the new instance, set the old one to Tailing.
    ///
    /// The handle must reference an instance in `Ready` state.
    async fn activate(&self, handle: InstanceHandle) -> ActivateResult;

    /// Apply a snapshot (parameter/bypass changes) without switching instances.
    ///
    /// The handle must reference the currently `Active` instance.
    async fn apply_snapshot(
        &self,
        handle: InstanceHandle,
        snapshot: &ModuleSnapshot,
    ) -> Result<(), EngineError>;

    /// Get the current state of an instance.
    fn instance_state(&self, handle: InstanceHandle) -> Option<InstanceState>;

    /// Get the currently active instance handle, if any.
    fn active_instance(&self) -> Option<InstanceHandle>;

    /// List all instances and their states.
    fn loaded_instances(&self) -> Vec<(InstanceHandle, InstanceState)>;

    /// Unload a specific instance, freeing its resources.
    async fn unload(&self, handle: InstanceHandle) -> Result<(), EngineError>;

    /// Clean up tailing instances whose audio has gone silent.
    ///
    /// Called periodically from the engine's `tick()`.
    async fn cleanup_tails(&self);

    /// Disable this module slot entirely (mute all output).
    async fn disable(&self);

    /// Re-enable a previously disabled module slot.
    async fn enable(&self);

    /// Whether this slot is currently disabled.
    fn is_disabled(&self) -> bool;
}
