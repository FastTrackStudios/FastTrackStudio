//! DirectorService - ROAM service for Rig Director operations
//!
//! This service provides RPC access to the Rig Director for managing
//! multiple vocal rigs, role assignments, and shared sends.

use facet::Facet;
use roam::{Context, Tx};
use std::sync::Arc;
use uuid::Uuid;

use super::core::{
    AssignmentContext, DirectorInfo, DirectorRigView, Module,
    InputAssignment, ModuleState, ModuleType, ModuleMacro, Rig, RigDirector, RigStatus, Role,
    RoleAssignment, InstrumentType,
};

// region:    --- RPC Types

/// Simplified vocal rig info for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct RigInfo {
    /// Rig ID
    pub id: Uuid,
    /// Rig name
    pub name: String,
    /// Input index (if assigned)
    pub input_index: Option<u8>,
    /// Input display name
    pub input_name: Option<String>,
    /// Assigned role name
    pub role_name: Option<String>,
    /// Assigned role ID
    pub role_id: Option<Uuid>,
    /// Whether enabled
    pub enabled: bool,
    /// Output level
    pub output_level: f64,
    /// Status
    pub status: RigStatus,
}

/// Simplified engine info for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ModuleInfo {
    /// Module ID
    pub id: Uuid,
    /// Module name
    pub name: String,
    /// Module type
    pub module_type: ModuleType,
    /// Whether enabled
    pub enabled: bool,
    /// Output level
    pub level: f64,
    /// Number of blocks
    pub block_count: usize,
    /// Macro parameters
    pub macros: Vec<MacroInfo>,
}

/// Simplified macro info for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct MacroInfo {
    /// Macro ID
    pub id: Uuid,
    /// Macro name
    pub name: String,
    /// Block ID this macro controls
    pub block_id: Uuid,
    /// Parameter index
    pub param_index: u32,
    /// Display value
    pub display_value: String,
    /// Normalized value
    pub normalized: f64,
}

/// Simplified role info for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct RoleInfo {
    /// Role ID
    pub id: Uuid,
    /// Role name
    pub name: String,
    /// Role type display string
    pub role_type: String,
    /// Color (RGBA)
    pub color: Option<u32>,
    /// Abbreviation
    pub abbreviation: String,
}

/// Simplified send level info for RPC communication.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SendLevelInfo {
    /// Target block ID in sends engine
    pub sends_block_id: Uuid,
    /// Target block name
    pub sends_block_name: String,
    /// Send level
    pub level: f64,
    /// Whether pre-fader
    pub pre_fader: bool,
    /// Whether enabled
    pub enabled: bool,
}

/// Full rig view for detailed display.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct RigViewInfo {
    /// Rig ID
    pub rig_id: Uuid,
    /// Rig name
    pub rig_name: String,
    /// Instrument type (Vocal, Guitar, Bass, Keyboard, Drums)
    pub instrument_type: InstrumentType,
    /// Assigned role info
    pub assigned_role: Option<RoleInfo>,
    /// Input assignment
    pub input: Option<InputAssignmentInfo>,
    /// Status
    pub status: RigStatus,
    /// Module states
    pub engines: Vec<ModuleInfo>,
    /// Send levels
    pub send_levels: Vec<SendLevelInfo>,
    /// Output level
    pub output_level: f64,
    /// Pan position
    pub pan: f64,
    /// Whether enabled
    pub enabled: bool,
    /// Optional group name for UI organization
    pub group: Option<String>,
}

/// Input assignment info for RPC.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct InputAssignmentInfo {
    /// Input index
    pub input_index: u8,
    /// Display name
    pub display_name: String,
    /// Gain in dB
    pub gain_db: f64,
    /// Phantom power enabled
    pub phantom_power: bool,
}

// endregion: --- RPC Types

// region:    --- Commands

/// Commands that can be executed on the director service.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum DirectorCommand {
    // Role management
    /// Assign a role to a rig (global context)
    AssignRoleGlobal { role_id: Uuid, rig_id: Uuid } = 0,
    /// Assign a role to a rig for a specific song
    AssignRoleForSong {
        role_id: Uuid,
        rig_id: Uuid,
        song_id: Uuid,
    } = 1,
    /// Clear role assignment for a rig
    ClearRoleAssignment { rig_id: Uuid } = 2,

    // Rig control
    /// Enable/disable a rig
    SetRigEnabled { rig_id: Uuid, enabled: bool } = 3,
    /// Set rig output level
    SetRigLevel { rig_id: Uuid, level: f64 } = 4,
    /// Set rig pan position
    SetRigPan { rig_id: Uuid, pan: f64 } = 5,

    // Module control
    /// Enable/disable an engine
    SetModuleEnabled {
        rig_id: Uuid,
        module_type: ModuleType,
        enabled: bool,
    } = 6,
    /// Set engine level
    SetModuleLevel {
        rig_id: Uuid,
        module_type: ModuleType,
        level: f64,
    } = 7,

    // Macro control
    /// Set a macro value
    SetMacroValue {
        rig_id: Uuid,
        macro_id: Uuid,
        value: f64,
    } = 8,

    // Send control
    /// Set send level from a rig to a sends block
    SetSendLevel {
        rig_id: Uuid,
        sends_block_id: Uuid,
        level: f64,
    } = 9,
    /// Enable/disable a send
    SetSendEnabled {
        rig_id: Uuid,
        sends_block_id: Uuid,
        enabled: bool,
    } = 10,

    // Input control
    /// Reassign input for a rig
    ReassignInput { rig_id: Uuid, input_index: u8 } = 11,
    /// Set input gain
    SetInputGain { rig_id: Uuid, gain_db: f64 } = 12,
    /// Set phantom power
    SetPhantomPower { rig_id: Uuid, enabled: bool } = 13,

    // Shared sends engine control
    /// Enable/disable a block in the shared sends engine
    SetSendsBlockEnabled { block_id: Uuid, enabled: bool } = 14,
    /// Set a parameter in the shared sends engine
    SetSendsParameter {
        block_id: Uuid,
        param_index: u32,
        value: f64,
    } = 15,
}

// endregion: --- Commands

// region:    --- Events

/// Events emitted by the director service.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum DirectorEvent {
    /// Director was loaded/changed
    DirectorLoaded { info: DirectorInfo } = 0,
    /// A rig changed (enabled, level, etc.)
    RigChanged { rig_id: Uuid } = 1,
    /// Role assignment changed for a rig
    RoleAssignmentChanged { rig_id: Uuid, role_id: Option<Uuid> } = 2,
    /// An engine changed (enabled, level)
    ModuleChanged { rig_id: Uuid, module_type: ModuleType } = 3,
    /// A macro value changed
    MacroChanged {
        rig_id: Uuid,
        macro_id: Uuid,
        value: f64,
    } = 4,
    /// Send level changed
    SendLevelChanged {
        rig_id: Uuid,
        sends_block_id: Uuid,
        level: f64,
    } = 5,
    /// Input was reassigned
    InputReassigned { rig_id: Uuid, input_index: u8 } = 6,
    /// Shared sends engine changed
    SendsModuleChanged = 7,
}

// endregion: --- Events

// region:    --- Service Trait

/// DirectorService provides RPC access to rig director operations.
#[roam::service]
pub trait DirectorService {
    // Director queries
    /// Get the current director info
    async fn get_director(&self) -> Option<DirectorInfo>;
    /// Get all rig views with resolved roles
    async fn get_rig_views(&self) -> Vec<RigViewInfo>;
    /// Get a single rig view
    async fn get_rig_view(&self, rig_id: Uuid) -> Option<RigViewInfo>;
    /// Get the shared sends engine info
    async fn get_sends_engine(&self) -> Option<ModuleInfo>;

    // Role queries
    /// Get all available roles
    async fn get_roles(&self) -> Vec<RoleInfo>;
    /// Get role assignments
    async fn get_role_assignments(&self) -> Vec<RoleAssignment>;

    // Commands
    /// Execute a director command
    async fn execute(&self, cmd: DirectorCommand);

    // Subscriptions
    /// Subscribe to director events
    async fn subscribe(&self, events: Tx<DirectorEvent>);
}

// endregion: --- Service Trait

// region:    --- Type Conversions

impl RigInfo {
    /// Convert from domain Track to RPC RigInfo
    pub fn from_vocal_rig(rig: &Rig, role: Option<&Role>) -> Self {
        let status = if !rig.enabled {
            RigStatus::Muted
        } else {
            RigStatus::Idle
        };

        Self {
            id: rig.id,
            name: rig.name.clone(),
            input_index: rig.input_assignment.as_ref().map(|i| i.input_index),
            input_name: rig.input_assignment.as_ref().map(|i| i.display_name()),
            role_name: role.map(|r| r.name.clone()),
            role_id: role.map(|r| r.id),
            enabled: rig.enabled,
            output_level: rig.output_level,
            status,
        }
    }
}

impl ModuleInfo {
    /// Convert from domain Module to RPC ModuleInfo
    pub fn from_engine(e: &Module) -> Self {
        Self {
            id: e.id,
            name: e.name.clone(),
            module_type: e.module_type,
            enabled: e.enabled,
            level: e.level,
            block_count: e.blocks.len(),
            macros: e.macros.iter().map(MacroInfo::from_macro).collect(),
        }
    }
}

impl MacroInfo {
    /// Convert from domain ModuleMacro to RPC MacroInfo
    pub fn from_macro(m: &ModuleMacro) -> Self {
        Self {
            id: m.id,
            name: m.name.clone(),
            block_id: m.block_id,
            param_index: m.param_index,
            display_value: m.display_value.clone(),
            normalized: m.normalized,
        }
    }
}

impl RoleInfo {
    /// Convert from domain Role to RPC RoleInfo
    pub fn from_role(r: &Role) -> Self {
        Self {
            id: r.id,
            name: r.name.clone(),
            role_type: r.role_type.to_string(),
            color: r.color,
            abbreviation: r.display_abbrev(),
        }
    }
}

impl InputAssignmentInfo {
    /// Convert from domain InputAssignment to RPC InputAssignmentInfo
    pub fn from_input(i: &InputAssignment) -> Self {
        Self {
            input_index: i.input_index,
            display_name: i.display_name(),
            gain_db: i.gain_db,
            phantom_power: i.phantom_power,
        }
    }
}

impl RigViewInfo {
    /// Build from director and rig
    pub fn from_director_rig_view(view: &DirectorRigView, sends_engine: &Module) -> Self {
        Self {
            rig_id: view.rig_id,
            rig_name: view.rig_name.clone(),
            instrument_type: view.instrument_type,
            assigned_role: view.assigned_role.as_ref().map(RoleInfo::from_role),
            input: view.input.as_ref().map(InputAssignmentInfo::from_input),
            status: view.status,
            engines: view
                .module_states
                .iter()
                .map(|s| ModuleInfo {
                    id: s.module_id,
                    name: s.module_type.to_string(),
                    module_type: s.module_type,
                    enabled: s.enabled,
                    level: s.level,
                    block_count: 0, // Not available in ModuleState
                    macros: s.macros.iter().map(MacroInfo::from_macro).collect(),
                })
                .collect(),
            send_levels: Vec::new(), // TODO: populate from rig
            output_level: view.output_level,
            pan: view.pan,
            enabled: view.enabled,
            group: view.group.clone(),
        }
    }
}

// endregion: --- Type Conversions

// region:    --- Local Client

crate::define_local_client! {
    /// A local client for in-process DirectorService calls.
    ///
    /// Wraps any DirectorService implementation and provides direct async method calls.
    client: LocalDirectorClient,
    service: DirectorService,
    methods: {
        /// Get the current director info
        async fn get_director() -> Option<DirectorInfo>;

        /// Get all rig views
        async fn get_rig_views() -> Vec<RigViewInfo>;

        /// Get a single rig view
        async fn get_rig_view(rig_id: Uuid) -> Option<RigViewInfo>;

        /// Get the shared sends engine
        async fn get_sends_engine() -> Option<ModuleInfo>;

        /// Get all roles
        async fn get_roles() -> Vec<RoleInfo>;

        /// Get role assignments
        async fn get_role_assignments() -> Vec<RoleAssignment>;

        /// Execute a director command
        async fn execute(cmd: DirectorCommand) -> ();

        /// Subscribe to director events
        async fn subscribe(events: Tx<DirectorEvent>) -> ();
    }
}

// endregion: --- Local Client
