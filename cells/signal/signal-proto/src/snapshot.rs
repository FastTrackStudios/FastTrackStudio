//! Snapshot types — full state capture of parameters, modules, and rigs.
//!
//! These types capture the complete runtime state of a rig at a point in time,
//! suitable for persistence, undo/redo, A/B comparison, and VST state recall.
//!
//! **Not to be confused with** [`ModuleSnapshot`](crate::module_preset::ModuleSnapshot),
//! which captures parameter *variations* within a module preset (same blocks,
//! different knob positions). The types here capture *absolute* state.

use facet::Facet;

use crate::block::BlockType;
use crate::id::{ModuleId, RigId};
use crate::normalized::NormalizedF64;

// ─────────────────────────────────────────────────────────────────────────────
// ParameterSnapshot
// ─────────────────────────────────────────────────────────────────────────────

/// A captured parameter ID + value pair.
///
/// Unlike [`ParameterValue`](crate::parameter::ParameterValue) which uses a
/// numeric index, `ParameterSnapshot` uses the string `param_id` for
/// portability across plugin versions where indices may shift.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ParameterSnapshot {
    /// The parameter's string identifier (matches [`ParamSpec::id`](crate::parameter::ParamSpec)).
    pub param_id: String,
    /// Captured normalized value in [0.0, 1.0].
    pub value: NormalizedF64,
}

impl ParameterSnapshot {
    /// Create a new parameter snapshot, clamping `value` to [0.0, 1.0].
    pub fn new(param_id: impl Into<String>, value: f64) -> Self {
        Self {
            param_id: param_id.into(),
            value: NormalizedF64::new(value),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// RigModuleSnapshot
// ─────────────────────────────────────────────────────────────────────────────

/// Full state capture of a module within a rig.
///
/// Captures the module's identity, block type, all parameter values, and
/// bypass state. Named `RigModuleSnapshot` to avoid confusion with
/// [`ModuleSnapshot`](crate::module_preset::ModuleSnapshot) (parameter
/// variations within a preset).
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct RigModuleSnapshot {
    /// Which module this snapshot is for.
    pub module_id: ModuleId,
    /// The functional category of the module's primary block.
    pub block_type: BlockType,
    /// All captured parameter values.
    pub parameters: Vec<ParameterSnapshot>,
    /// Whether the module was bypassed when captured.
    pub bypassed: bool,
}

impl RigModuleSnapshot {
    /// Create a new module snapshot with no parameters.
    pub fn new(module_id: ModuleId, block_type: BlockType) -> Self {
        Self {
            module_id,
            block_type,
            parameters: Vec::new(),
            bypassed: false,
        }
    }

    /// Set the bypass state.
    #[must_use]
    pub fn with_bypassed(mut self, bypassed: bool) -> Self {
        self.bypassed = bypassed;
        self
    }

    /// Set the parameter list.
    #[must_use]
    pub fn with_parameters(mut self, parameters: Vec<ParameterSnapshot>) -> Self {
        self.parameters = parameters;
        self
    }

    /// Add a single parameter snapshot.
    pub fn add_parameter(&mut self, param: ParameterSnapshot) {
        self.parameters.push(param);
    }

    /// Find a captured parameter by ID.
    pub fn parameter(&self, param_id: &str) -> Option<&ParameterSnapshot> {
        self.parameters.iter().find(|p| p.param_id == param_id)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// RigSnapshot
// ─────────────────────────────────────────────────────────────────────────────

/// Full state capture of an entire rig.
///
/// Contains per-module snapshots plus metadata. Used for undo/redo,
/// A/B comparison, preset recall, and session persistence.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct RigSnapshot {
    /// Which rig this snapshot is for.
    pub rig_id: RigId,
    /// Human-readable label (e.g. `"Verse Clean"`, `"Pre-Solo"`, `"Autosave 14:32"`).
    pub name: String,
    /// Per-module state captures.
    pub modules: Vec<RigModuleSnapshot>,
    /// Optional description or notes.
    pub description: Option<String>,
    /// ISO-8601 timestamp of when this snapshot was taken (stored as string
    /// to avoid pulling in a datetime crate).
    pub timestamp: Option<String>,
}

impl RigSnapshot {
    /// Create a new rig snapshot with no modules.
    pub fn new(rig_id: RigId, name: impl Into<String>) -> Self {
        Self {
            rig_id,
            name: name.into(),
            modules: Vec::new(),
            description: None,
            timestamp: None,
        }
    }

    /// Set an optional description.
    #[must_use]
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    /// Set the capture timestamp.
    #[must_use]
    pub fn with_timestamp(mut self, timestamp: impl Into<String>) -> Self {
        self.timestamp = Some(timestamp.into());
        self
    }

    /// Set the module snapshots.
    #[must_use]
    pub fn with_modules(mut self, modules: Vec<RigModuleSnapshot>) -> Self {
        self.modules = modules;
        self
    }

    /// Add a single module snapshot.
    pub fn add_module(&mut self, module: RigModuleSnapshot) {
        self.modules.push(module);
    }

    /// Find a module snapshot by module ID.
    pub fn module(&self, module_id: ModuleId) -> Option<&RigModuleSnapshot> {
        self.modules.iter().find(|m| m.module_id == module_id)
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // ── ParameterSnapshot ────────────────────────────────────────────

    #[test]
    fn parameter_snapshot_creation() {
        // -- Exec
        let snap = ParameterSnapshot::new("drive", 0.75);

        // -- Check
        assert_eq!(snap.param_id, "drive");
        assert!((snap.value.get() - 0.75).abs() < f64::EPSILON);
    }

    #[test]
    fn parameter_snapshot_clamps_value() {
        // -- Exec
        let above = ParameterSnapshot::new("gain", 1.5);
        let below = ParameterSnapshot::new("gain", -0.5);

        // -- Check
        assert_eq!(above.value.get(), 1.0);
        assert_eq!(below.value.get(), 0.0);
    }

    // ── RigModuleSnapshot ────────────────────────────────────────────

    #[test]
    fn rig_module_snapshot_creation() {
        // -- Setup & Fixtures
        let module_id = ModuleId::new();

        // -- Exec
        let snap = RigModuleSnapshot::new(module_id, BlockType::Drive);

        // -- Check
        assert_eq!(snap.module_id, module_id);
        assert_eq!(snap.block_type, BlockType::Drive);
        assert!(snap.parameters.is_empty());
        assert!(!snap.bypassed);
    }

    #[test]
    fn rig_module_snapshot_builder() {
        // -- Setup & Fixtures
        let module_id = ModuleId::new();
        let params = vec![
            ParameterSnapshot::new("drive", 0.7),
            ParameterSnapshot::new("tone", 0.5),
        ];

        // -- Exec
        let snap = RigModuleSnapshot::new(module_id, BlockType::Drive)
            .with_bypassed(true)
            .with_parameters(params);

        // -- Check
        assert!(snap.bypassed);
        assert_eq!(snap.parameters.len(), 2);
        assert!((snap.parameter("drive").unwrap().value.get() - 0.7).abs() < f64::EPSILON);
        assert!((snap.parameter("tone").unwrap().value.get() - 0.5).abs() < f64::EPSILON);
        assert!(snap.parameter("nonexistent").is_none());
    }

    #[test]
    fn rig_module_snapshot_add_parameter() {
        // -- Setup & Fixtures
        let mut snap = RigModuleSnapshot::new(ModuleId::new(), BlockType::Eq);

        // -- Exec
        snap.add_parameter(ParameterSnapshot::new("low_freq", 0.3));
        snap.add_parameter(ParameterSnapshot::new("high_freq", 0.8));

        // -- Check
        assert_eq!(snap.parameters.len(), 2);
        assert_eq!(snap.parameters[0].param_id, "low_freq");
        assert_eq!(snap.parameters[1].param_id, "high_freq");
    }

    // ── RigSnapshot ──────────────────────────────────────────────────

    #[test]
    fn rig_snapshot_creation() {
        // -- Setup & Fixtures
        let rig_id = RigId::new();

        // -- Exec
        let snap = RigSnapshot::new(rig_id, "Verse Clean");

        // -- Check
        assert_eq!(snap.rig_id, rig_id);
        assert_eq!(snap.name, "Verse Clean");
        assert!(snap.modules.is_empty());
        assert!(snap.description.is_none());
        assert!(snap.timestamp.is_none());
    }

    #[test]
    fn rig_snapshot_builder() {
        // -- Setup & Fixtures
        let rig_id = RigId::new();
        let module_id = ModuleId::new();
        let module_snap = RigModuleSnapshot::new(module_id, BlockType::Amp)
            .with_parameters(vec![ParameterSnapshot::new("gain", 0.6)]);

        // -- Exec
        let snap = RigSnapshot::new(rig_id, "Pre-Solo")
            .with_description("State before solo section")
            .with_timestamp("2026-02-08T14:32:00Z")
            .with_modules(vec![module_snap]);

        // -- Check
        assert_eq!(snap.name, "Pre-Solo");
        assert_eq!(snap.description.as_deref(), Some("State before solo section"));
        assert_eq!(snap.timestamp.as_deref(), Some("2026-02-08T14:32:00Z"));
        assert_eq!(snap.modules.len(), 1);
        assert!(snap.module(module_id).is_some());
        assert!(snap.module(ModuleId::new()).is_none());
    }

    #[test]
    fn rig_snapshot_add_module() {
        // -- Setup & Fixtures
        let mut snap = RigSnapshot::new(RigId::new(), "Test");
        let id1 = ModuleId::new();
        let id2 = ModuleId::new();

        // -- Exec
        snap.add_module(RigModuleSnapshot::new(id1, BlockType::Drive));
        snap.add_module(RigModuleSnapshot::new(id2, BlockType::Reverb));

        // -- Check
        assert_eq!(snap.modules.len(), 2);
        assert_eq!(snap.module(id1).unwrap().block_type, BlockType::Drive);
        assert_eq!(snap.module(id2).unwrap().block_type, BlockType::Reverb);
    }
}
