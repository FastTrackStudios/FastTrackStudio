//! Engine runtime types — resolved module targets and lifecycle states.
//!
//! These are pure data types used by the engine layer (`fts::rig::engine`)
//! to track what each module slot should be running and its current state.
//! No async, no traits — just the shared vocabulary between the engine
//! orchestrator, module slots, and backends.

use std::fmt;

use facet::Facet;

use crate::id::{ModulePresetId, ModuleSnapshotId};
use crate::module::ModuleType;

// ─────────────────────────────────────────────────────────────────────────────
// InstanceHandle
// ─────────────────────────────────────────────────────────────────────────────

/// Opaque handle identifying a specific plugin instance within a module slot.
///
/// Each time a module preset is loaded into a slot, a new handle is minted.
/// The handle is used to activate, query, and unload that instance.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
pub struct InstanceHandle(u64);

impl InstanceHandle {
    /// Create a new instance handle from a raw counter value.
    pub fn from_raw(raw: u64) -> Self {
        Self(raw)
    }

    /// Get the raw counter value.
    pub fn raw(self) -> u64 {
        self.0
    }
}

impl fmt::Display for InstanceHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Instance({})", self.0)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// ModuleTarget
// ─────────────────────────────────────────────────────────────────────────────

/// What a module slot should load — resolved from a preset's module
/// assignments plus any scene/song/global overrides.
///
/// This is the output of the resolver: one `ModuleTarget` per active
/// module type tells the engine exactly which module preset (and optional
/// snapshot) to make live.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Facet)]
pub struct ModuleTarget {
    /// Which processing stage this target is for.
    pub module_type: ModuleType,
    /// The module preset to load.
    pub module_preset_id: ModulePresetId,
    /// Optional snapshot within the module preset (parameter variation).
    pub module_snapshot_id: Option<ModuleSnapshotId>,
}

impl ModuleTarget {
    /// Create a new module target.
    pub fn new(module_type: ModuleType, module_preset_id: ModulePresetId) -> Self {
        Self {
            module_type,
            module_preset_id,
            module_snapshot_id: None,
        }
    }

    /// Set the snapshot for this target.
    #[must_use]
    pub fn with_snapshot(mut self, snapshot_id: ModuleSnapshotId) -> Self {
        self.module_snapshot_id = Some(snapshot_id);
        self
    }

    /// Whether this target has the same preset (ignoring snapshot).
    pub fn same_preset(&self, other: &Self) -> bool {
        self.module_type == other.module_type && self.module_preset_id == other.module_preset_id
    }
}

impl fmt::Display for ModuleTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}→{}", self.module_type, self.module_preset_id)?;
        if let Some(snap) = self.module_snapshot_id {
            write!(f, "@{snap}")?;
        }
        Ok(())
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// InstanceState
// ─────────────────────────────────────────────────────────────────────────────

/// Per-instance lifecycle state within a module slot.
///
/// ```text
/// Loading → Ready → Active → Tailing → Unloaded
///                      ↑         │
///                      └─────────┘  (re-activate if needed)
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum InstanceState {
    /// FX chain is being built / plugins are loading.
    Loading = 0,
    /// Fully loaded, output muted, waiting to be activated.
    Ready = 1,
    /// Live output — this instance is the active sound for this slot.
    Active = 2,
    /// Previous instance whose tail (reverb/delay) is still ringing out.
    /// Send is ramping to -inf.
    Tailing = 3,
    /// Resources freed, instance can be reclaimed.
    Unloaded = 4,
}

impl InstanceState {
    /// Whether this instance is occupying audio resources.
    pub fn is_alive(self) -> bool {
        matches!(
            self,
            Self::Loading | Self::Ready | Self::Active | Self::Tailing
        )
    }

    /// Whether this instance is producing audible output.
    pub fn is_audible(self) -> bool {
        matches!(self, Self::Active | Self::Tailing)
    }
}

impl fmt::Display for InstanceState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Loading => "Loading",
            Self::Ready => "Ready",
            Self::Active => "Active",
            Self::Tailing => "Tailing",
            Self::Unloaded => "Unloaded",
        })
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// PresetReadiness
// ─────────────────────────────────────────────────────────────────────────────

/// Overall readiness of a full preset load across all module slots.
///
/// Used by the UI to show loading indicators and by the engine to decide
/// whether a deferred switch can proceed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
#[repr(u8)]
pub enum PresetReadiness {
    /// All module slots are loaded and ready to activate.
    Ready = 0,
    /// Some slots are still loading.
    Loading {
        /// Number of slots that have finished loading.
        loaded: u16,
        /// Total number of slots that need to load.
        total: u16,
    } = 1,
    /// One or more slots failed to load.
    Failed = 2,
}

impl PresetReadiness {
    /// Whether the preset is fully ready to activate.
    pub fn is_ready(self) -> bool {
        matches!(self, Self::Ready)
    }

    /// Whether the preset is still loading.
    pub fn is_loading(self) -> bool {
        matches!(self, Self::Loading { .. })
    }

    /// Whether the preset failed to load.
    pub fn is_failed(self) -> bool {
        matches!(self, Self::Failed)
    }

    /// Get the loading progress as a fraction (0.0–1.0), or 1.0 if ready.
    pub fn progress(self) -> f64 {
        match self {
            Self::Ready => 1.0,
            Self::Loading { loaded, total } => {
                if total == 0 {
                    1.0
                } else {
                    f64::from(loaded) / f64::from(total)
                }
            }
            Self::Failed => 0.0,
        }
    }
}

impl fmt::Display for PresetReadiness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ready => f.write_str("Ready"),
            Self::Loading { loaded, total } => write!(f, "Loading ({loaded}/{total})"),
            Self::Failed => f.write_str("Failed"),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// PresetLoadHandle
// ─────────────────────────────────────────────────────────────────────────────

/// Handle returned when a preset load is initiated (via `load_preset_with_scene`
/// or `preload`). Used to poll readiness or wait for completion.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet)]
pub struct PresetLoadHandle(u64);

impl PresetLoadHandle {
    /// Create a handle from a raw counter value.
    pub fn from_raw(raw: u64) -> Self {
        Self(raw)
    }

    /// Get the raw counter value.
    pub fn raw(self) -> u64 {
        self.0
    }
}

impl fmt::Display for PresetLoadHandle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "PresetLoad({})", self.0)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// SlotDiff
// ─────────────────────────────────────────────────────────────────────────────

/// The action a module slot should take when transitioning to a new target.
///
/// Produced by `compute_diff()` — compares current slot state to new targets
/// and emits the minimal set of changes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SlotDiff {
    /// Load a new module preset and activate it (full switch).
    LoadAndActivate {
        module_type: ModuleType,
        target: ModuleTarget,
    },
    /// Activate an already-preloaded instance (gapless instant switch).
    Activate {
        module_type: ModuleType,
        handle: InstanceHandle,
    },
    /// Same preset, different snapshot — apply parameter changes only.
    ApplySnapshot {
        module_type: ModuleType,
        module_preset_id: ModulePresetId,
        snapshot_id: ModuleSnapshotId,
    },
    /// Disable this module slot (override says Disable).
    Disable { module_type: ModuleType },
    /// Re-enable a previously disabled module slot.
    Enable { module_type: ModuleType },
    /// No change needed for this slot.
    NoChange { module_type: ModuleType },
}

impl SlotDiff {
    /// The module type this diff applies to.
    pub fn module_type(&self) -> ModuleType {
        match self {
            Self::LoadAndActivate { module_type, .. }
            | Self::Activate { module_type, .. }
            | Self::ApplySnapshot { module_type, .. }
            | Self::Disable { module_type }
            | Self::Enable { module_type }
            | Self::NoChange { module_type } => *module_type,
        }
    }

    /// Whether this diff requires loading new plugins.
    pub fn requires_load(&self) -> bool {
        matches!(self, Self::LoadAndActivate { .. })
    }

    /// Whether this diff requires any action at all.
    pub fn is_noop(&self) -> bool {
        matches!(self, Self::NoChange { .. })
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// SwitchOutcome
// ─────────────────────────────────────────────────────────────────────────────

/// Result of requesting a preset/scene switch through the engine.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SwitchOutcome {
    /// Switch completed immediately (all slots were preloaded).
    Completed,
    /// Switch is deferred — some slots need to load first.
    /// Use the handle to poll readiness or wait.
    Pending {
        handle: PresetLoadHandle,
        readiness: PresetReadiness,
    },
    /// Switch failed — engine error prevented the transition.
    Failed { reason: String },
}

impl SwitchOutcome {
    /// Whether the switch completed immediately.
    pub fn is_completed(&self) -> bool {
        matches!(self, Self::Completed)
    }

    /// Whether the switch is pending (waiting for loads).
    pub fn is_pending(&self) -> bool {
        matches!(self, Self::Pending { .. })
    }

    /// Whether the switch failed.
    pub fn is_failed(&self) -> bool {
        matches!(self, Self::Failed { .. })
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// EngineError
// ─────────────────────────────────────────────────────────────────────────────

/// Errors from the engine layer.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EngineError {
    /// A required plugin could not be found.
    PluginNotFound { plugin_id: String },
    /// The module slot for this type hasn't been initialized.
    SlotNotInitialized { module_type: ModuleType },
    /// Instance was in an unexpected state.
    InvalidState {
        expected: InstanceState,
        actual: InstanceState,
    },
    /// Loading took too long.
    LoadTimeout {
        module_preset_id: ModulePresetId,
        timeout_ms: u64,
    },
    /// The module preset ID was not found in the rig.
    ModulePresetNotFound { module_preset_id: ModulePresetId },
    /// The snapshot ID was not found in the module preset.
    SnapshotNotFound { snapshot_id: ModuleSnapshotId },
    /// Backend-specific error.
    Backend(String),
}

impl fmt::Display for EngineError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PluginNotFound { plugin_id } => {
                write!(f, "plugin not found: {plugin_id}")
            }
            Self::SlotNotInitialized { module_type } => {
                write!(f, "slot not initialized: {module_type}")
            }
            Self::InvalidState { expected, actual } => {
                write!(f, "invalid state: expected {expected}, got {actual}")
            }
            Self::LoadTimeout {
                module_preset_id,
                timeout_ms,
            } => {
                write!(
                    f,
                    "load timeout for {module_preset_id} after {timeout_ms}ms"
                )
            }
            Self::ModulePresetNotFound { module_preset_id } => {
                write!(f, "module preset not found: {module_preset_id}")
            }
            Self::SnapshotNotFound { snapshot_id } => {
                write!(f, "snapshot not found: {snapshot_id}")
            }
            Self::Backend(msg) => {
                write!(f, "backend error: {msg}")
            }
        }
    }
}

impl std::error::Error for EngineError {}

// ─────────────────────────────────────────────────────────────────────────────
// PreloadPriority
// ─────────────────────────────────────────────────────────────────────────────

/// Priority level for preloading targets.
///
/// The engine processes preload requests in priority order during each tick.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Facet)]
#[repr(u8)]
pub enum PreloadPriority {
    /// Next scene in the current song — must be ready instantly.
    Critical = 0,
    /// Previous scene + other scenes in the current song.
    High = 1,
    /// Next/prev song's default (first) scene.
    Medium = 2,
    /// Next/prev preset in profile (for browsing).
    Low = 3,
}

impl fmt::Display for PreloadPriority {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Critical => "Critical",
            Self::High => "High",
            Self::Medium => "Medium",
            Self::Low => "Low",
        })
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// PreloadRequest
// ─────────────────────────────────────────────────────────────────────────────

/// A request to preload a set of module targets at a given priority.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PreloadRequest {
    /// Priority for this preload batch.
    pub priority: PreloadPriority,
    /// Module targets to preload.
    pub targets: Vec<ModuleTarget>,
}

impl PreloadRequest {
    /// Create a new preload request.
    pub fn new(priority: PreloadPriority, targets: Vec<ModuleTarget>) -> Self {
        Self { priority, targets }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn instance_handle_roundtrip() {
        let handle = InstanceHandle::from_raw(42);
        assert_eq!(handle.raw(), 42);
        assert_eq!(format!("{handle}"), "Instance(42)");
    }

    #[test]
    fn module_target_creation() {
        let preset_id = ModulePresetId::new();
        let target = ModuleTarget::new(ModuleType::Drive, preset_id);

        assert_eq!(target.module_type, ModuleType::Drive);
        assert_eq!(target.module_preset_id, preset_id);
        assert!(target.module_snapshot_id.is_none());
    }

    #[test]
    fn module_target_with_snapshot() {
        let preset_id = ModulePresetId::new();
        let snap_id = ModuleSnapshotId::new();
        let target = ModuleTarget::new(ModuleType::Amp, preset_id).with_snapshot(snap_id);

        assert_eq!(target.module_snapshot_id, Some(snap_id));
    }

    #[test]
    fn module_target_same_preset() {
        let preset_id = ModulePresetId::new();
        let snap_a = ModuleSnapshotId::new();
        let snap_b = ModuleSnapshotId::new();

        let a = ModuleTarget::new(ModuleType::Drive, preset_id).with_snapshot(snap_a);
        let b = ModuleTarget::new(ModuleType::Drive, preset_id).with_snapshot(snap_b);

        assert!(a.same_preset(&b));
        assert_ne!(a, b); // Different snapshots
    }

    #[test]
    fn module_target_different_preset() {
        let a = ModuleTarget::new(ModuleType::Drive, ModulePresetId::new());
        let b = ModuleTarget::new(ModuleType::Drive, ModulePresetId::new());

        assert!(!a.same_preset(&b));
    }

    #[test]
    fn instance_state_is_alive() {
        assert!(InstanceState::Loading.is_alive());
        assert!(InstanceState::Ready.is_alive());
        assert!(InstanceState::Active.is_alive());
        assert!(InstanceState::Tailing.is_alive());
        assert!(!InstanceState::Unloaded.is_alive());
    }

    #[test]
    fn instance_state_is_audible() {
        assert!(!InstanceState::Loading.is_audible());
        assert!(!InstanceState::Ready.is_audible());
        assert!(InstanceState::Active.is_audible());
        assert!(InstanceState::Tailing.is_audible());
        assert!(!InstanceState::Unloaded.is_audible());
    }

    #[test]
    fn preset_readiness_progress() {
        assert!((PresetReadiness::Ready.progress() - 1.0).abs() < f64::EPSILON);
        assert!((PresetReadiness::Failed.progress()).abs() < f64::EPSILON);

        let loading = PresetReadiness::Loading {
            loaded: 3,
            total: 10,
        };
        assert!((loading.progress() - 0.3).abs() < f64::EPSILON);

        // Edge case: total = 0
        let zero = PresetReadiness::Loading {
            loaded: 0,
            total: 0,
        };
        assert!((zero.progress() - 1.0).abs() < f64::EPSILON);
    }

    #[test]
    fn preset_readiness_states() {
        assert!(PresetReadiness::Ready.is_ready());
        assert!(!PresetReadiness::Ready.is_loading());
        assert!(!PresetReadiness::Ready.is_failed());

        let loading = PresetReadiness::Loading {
            loaded: 1,
            total: 5,
        };
        assert!(!loading.is_ready());
        assert!(loading.is_loading());

        assert!(PresetReadiness::Failed.is_failed());
    }

    #[test]
    fn preset_load_handle_roundtrip() {
        let handle = PresetLoadHandle::from_raw(99);
        assert_eq!(handle.raw(), 99);
        assert_eq!(format!("{handle}"), "PresetLoad(99)");
    }

    #[test]
    fn slot_diff_module_type() {
        let target = ModuleTarget::new(ModuleType::Amp, ModulePresetId::new());
        let diff = SlotDiff::LoadAndActivate {
            module_type: ModuleType::Amp,
            target,
        };
        assert_eq!(diff.module_type(), ModuleType::Amp);
        assert!(diff.requires_load());
        assert!(!diff.is_noop());

        let noop = SlotDiff::NoChange {
            module_type: ModuleType::Eq,
        };
        assert!(noop.is_noop());
        assert!(!noop.requires_load());
    }

    #[test]
    fn switch_outcome_states() {
        assert!(SwitchOutcome::Completed.is_completed());
        assert!(!SwitchOutcome::Completed.is_pending());
        assert!(!SwitchOutcome::Completed.is_failed());

        let pending = SwitchOutcome::Pending {
            handle: PresetLoadHandle::from_raw(1),
            readiness: PresetReadiness::Loading {
                loaded: 0,
                total: 5,
            },
        };
        assert!(pending.is_pending());

        let failed = SwitchOutcome::Failed {
            reason: "test".into(),
        };
        assert!(failed.is_failed());
    }

    #[test]
    fn engine_error_display() {
        let err = EngineError::PluginNotFound {
            plugin_id: "com.test.fx".into(),
        };
        assert_eq!(format!("{err}"), "plugin not found: com.test.fx");

        let err = EngineError::SlotNotInitialized {
            module_type: ModuleType::Drive,
        };
        assert_eq!(format!("{err}"), "slot not initialized: Drive");

        let err = EngineError::InvalidState {
            expected: InstanceState::Ready,
            actual: InstanceState::Loading,
        };
        assert_eq!(
            format!("{err}"),
            "invalid state: expected Ready, got Loading"
        );

        let err = EngineError::Backend("connection lost".into());
        assert_eq!(format!("{err}"), "backend error: connection lost");
    }

    #[test]
    fn engine_error_is_std_error() {
        let err: Box<dyn std::error::Error> = Box::new(EngineError::Backend("test".into()));
        assert_eq!(format!("{err}"), "backend error: test");
    }

    #[test]
    fn preload_priority_ordering() {
        assert!(PreloadPriority::Critical < PreloadPriority::High);
        assert!(PreloadPriority::High < PreloadPriority::Medium);
        assert!(PreloadPriority::Medium < PreloadPriority::Low);
    }

    #[test]
    fn preload_request_creation() {
        let target = ModuleTarget::new(ModuleType::Amp, ModulePresetId::new());
        let request = PreloadRequest::new(PreloadPriority::Critical, vec![target.clone()]);

        assert_eq!(request.priority, PreloadPriority::Critical);
        assert_eq!(request.targets.len(), 1);
        assert_eq!(request.targets[0], target);
    }
}
