//! RigDirector - Multi-rig coordination system
//!
//! The RigDirector manages multiple rigs of various instrument types
//! with their interconnections, role assignments, and shared sends module.

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::{InputAssignment, InstrumentType, Rig, Module, ModuleType, ModuleMacro, Role, RoleAssignment};

/// Coordinator for multiple rigs across all instrument types.
///
/// The RigDirector provides a single view to manage all band rigs,
/// their role assignments, and the shared Sends module.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct RigDirector {
    /// Unique identifier
    pub id: Uuid,
    /// Display name (e.g., "Full Band", "Vocals")
    pub name: String,
    /// All rigs managed by this director (vocals, guitars, bass, keys, drums)
    pub rigs: Vec<Rig>,
    /// Shared Sends module (Verb, Delay, Special)
    pub sends_engine: Module,
    /// Available roles
    pub roles: Vec<Role>,
    /// Role assignments (global and context-specific)
    pub role_assignments: Vec<RoleAssignment>,
}

/// View model for displaying a rig in the director UI.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct DirectorRigView {
    /// Rig ID
    pub rig_id: Uuid,
    /// Rig name
    pub rig_name: String,
    /// Type of instrument
    pub instrument_type: InstrumentType,
    /// Currently assigned role (resolved for current context)
    pub assigned_role: Option<Role>,
    /// Input assignment info
    pub input: Option<InputAssignment>,
    /// Current status
    pub status: RigStatus,
    /// State of each module
    pub module_states: Vec<ModuleState>,
    /// Output level
    pub output_level: f64,
    /// Pan position
    pub pan: f64,
    /// Whether the rig is enabled
    pub enabled: bool,
    /// Optional group name for UI organization
    pub group: Option<String>,
}

/// Current status of a rig.
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Facet)]
pub enum RigStatus {
    /// Rig is active and receiving signal
    Active,
    /// Rig is idle (no signal)
    Idle,
    /// Rig is muted
    Muted,
    /// Rig has a problem (clipping, no input, etc.)
    Warning,
    /// Rig is offline/disconnected
    Offline,
}

/// State of a single module for display.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct ModuleState {
    /// Module ID
    pub module_id: Uuid,
    /// Module type
    pub module_type: ModuleType,
    /// Whether the module is enabled
    pub enabled: bool,
    /// Output level
    pub level: f64,
    /// Macro values for quick display
    pub macros: Vec<ModuleMacro>,
}

/// Summary info about a rig director for listing.
#[derive(Debug, Clone, Serialize, Deserialize, Facet)]
pub struct DirectorInfo {
    /// RigDirector ID
    pub id: Uuid,
    /// RigDirector name
    pub name: String,
    /// Number of rigs
    pub rig_count: usize,
    /// Number of active rigs
    pub active_rig_count: usize,
    /// Counts by instrument type
    pub rig_counts_by_type: Vec<(InstrumentType, usize)>,
}

impl RigDirector {
    /// Create a new rig director
    pub fn new(name: impl Into<String>) -> Self {
        // Create default sends engine
        let sends_engine = Module::new("Sends", ModuleType::Sends);

        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            rigs: Vec::new(),
            sends_engine,
            roles: Vec::new(),
            role_assignments: Vec::new(),
        }
    }

    /// Add a rig to this director
    pub fn add_rig(&mut self, rig: Rig) {
        self.rigs.push(rig);
    }

    /// Add a role
    pub fn add_role(&mut self, role: Role) {
        self.roles.push(role);
    }

    /// Add a role assignment
    pub fn add_assignment(&mut self, assignment: RoleAssignment) {
        self.role_assignments.push(assignment);
    }

    /// Get a rig by ID
    pub fn get_rig(&self, id: Uuid) -> Option<&Rig> {
        self.rigs.iter().find(|r| r.id == id)
    }

    /// Get a mutable rig by ID
    pub fn get_rig_mut(&mut self, id: Uuid) -> Option<&mut Rig> {
        self.rigs.iter_mut().find(|r| r.id == id)
    }

    /// Get all rigs of a specific instrument type
    pub fn rigs_by_type(&self, instrument_type: InstrumentType) -> Vec<&Rig> {
        self.rigs
            .iter()
            .filter(|r| r.instrument_type == instrument_type)
            .collect()
    }

    /// Get a role by ID
    pub fn get_role(&self, id: Uuid) -> Option<&Role> {
        self.roles.iter().find(|r| r.id == id)
    }

    /// Resolve the effective role for a rig in a given context
    pub fn resolve_role(
        &self,
        rig_id: Uuid,
        song_id: Option<Uuid>,
        section_id: Option<Uuid>,
    ) -> Option<&Role> {
        // Find all assignments that apply to this rig and context
        let mut applicable: Vec<_> = self
            .role_assignments
            .iter()
            .filter(|a| a.rig_id == rig_id && a.context.applies_to(song_id, section_id))
            .collect();

        // Sort by priority (higher = more specific)
        applicable.sort_by(|a, b| b.priority.cmp(&a.priority));

        // Return the role for the highest priority assignment
        applicable
            .first()
            .and_then(|a| self.get_role(a.role_id))
    }

    /// Get view models for all rigs
    pub fn get_rig_views(
        &self,
        song_id: Option<Uuid>,
        section_id: Option<Uuid>,
    ) -> Vec<DirectorRigView> {
        self.rigs
            .iter()
            .map(|rig| self.build_rig_view(rig, song_id, section_id))
            .collect()
    }

    /// Build a view model for a single rig
    fn build_rig_view(
        &self,
        rig: &Rig,
        song_id: Option<Uuid>,
        section_id: Option<Uuid>,
    ) -> DirectorRigView {
        let assigned_role = self.resolve_role(rig.id, song_id, section_id).cloned();

        let module_states = rig
            .modules
            .iter()
            .map(|module| ModuleState {
                module_id: module.id,
                module_type: module.module_type,
                enabled: module.enabled,
                level: module.level,
                macros: module.macros.clone(),
            })
            .collect();

        let status = if !rig.enabled {
            RigStatus::Muted
        } else {
            RigStatus::Idle // Would be Active if receiving signal
        };

        DirectorRigView {
            rig_id: rig.id,
            rig_name: rig.name.clone(),
            instrument_type: rig.instrument_type,
            assigned_role,
            input: rig.input_assignment.clone(),
            status,
            module_states,
            output_level: rig.output_level,
            pan: rig.pan,
            enabled: rig.enabled,
            group: rig.group.clone(),
        }
    }

    /// Get director info summary
    pub fn info(&self) -> DirectorInfo {
        use std::collections::HashMap;

        let mut counts: HashMap<InstrumentType, usize> = HashMap::new();
        for rig in &self.rigs {
            *counts.entry(rig.instrument_type).or_insert(0) += 1;
        }

        let rig_counts_by_type: Vec<_> = counts.into_iter().collect();

        DirectorInfo {
            id: self.id,
            name: self.name.clone(),
            rig_count: self.rigs.len(),
            active_rig_count: self.rigs.iter().filter(|r| r.enabled).count(),
            rig_counts_by_type,
        }
    }

    /// Assign a role to a rig (global context)
    pub fn assign_role_global(&mut self, role_id: Uuid, rig_id: Uuid) {
        // Remove any existing global assignment for this rig
        self.role_assignments
            .retain(|a| !(a.rig_id == rig_id && a.context == super::AssignmentContext::Global));

        // Add new assignment
        self.role_assignments
            .push(RoleAssignment::global(role_id, rig_id));

        // Update the rig's current_role_id
        if let Some(rig) = self.get_rig_mut(rig_id) {
            rig.current_role_id = Some(role_id);
        }
    }
}

impl std::fmt::Display for RigStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RigStatus::Active => write!(f, "Active"),
            RigStatus::Idle => write!(f, "Idle"),
            RigStatus::Muted => write!(f, "Muted"),
            RigStatus::Warning => write!(f, "Warning"),
            RigStatus::Offline => write!(f, "Offline"),
        }
    }
}
