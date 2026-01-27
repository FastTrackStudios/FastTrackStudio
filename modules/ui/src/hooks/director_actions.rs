//! Hook providing director action callbacks
//!
//! This hook wraps the DirectorService.execute() method, providing
//! convenient callbacks for UI components to trigger director commands.

use crate::context::director::use_director_service;
use dioxus::prelude::*;
use fts::rig::{DirectorCommand, ModuleType, DIRECTOR_RIG_VIEWS};
use uuid::Uuid;

/// Collection of director action callbacks
#[derive(Clone)]
pub struct DirectorActions {
    // Role management
    /// Assign a role to a rig (global context)
    pub assign_role: Callback<(Uuid, Uuid)>,
    /// Clear role assignment for a rig
    pub clear_role: Callback<Uuid>,

    // Rig control
    /// Enable/disable a rig
    pub set_rig_enabled: Callback<(Uuid, bool)>,
    /// Set rig output level
    pub set_rig_level: Callback<(Uuid, f64)>,
    /// Set rig pan position
    pub set_rig_pan: Callback<(Uuid, f64)>,

    // Engine control
    /// Enable/disable an engine
    pub set_engine_enabled: Callback<(Uuid, ModuleType, bool)>,
    /// Set engine level
    pub set_engine_level: Callback<(Uuid, ModuleType, f64)>,

    // Macro control
    /// Set a macro value
    pub set_macro_value: Callback<(Uuid, Uuid, f64)>,

    // Send control
    /// Set send level from a rig to a sends block
    pub set_send_level: Callback<(Uuid, Uuid, f64)>,
    /// Enable/disable a send
    pub set_send_enabled: Callback<(Uuid, Uuid, bool)>,

    // Input control
    /// Reassign input for a rig
    pub reassign_input: Callback<(Uuid, u8)>,
    /// Set input gain
    pub set_input_gain: Callback<(Uuid, f64)>,
    /// Set phantom power
    pub set_phantom_power: Callback<(Uuid, bool)>,

    // Shared sends engine control
    /// Enable/disable a block in the shared sends engine
    pub set_sends_block_enabled: Callback<(Uuid, bool)>,
    /// Set a parameter in the shared sends engine
    pub set_sends_parameter: Callback<(Uuid, u32, f64)>,
}

/// Hook that provides director action callbacks
///
/// Uses the director service from context to execute commands via ROAM.
/// Commands are executed asynchronously using `spawn()`.
pub fn use_director_actions() -> DirectorActions {
    let ctx = use_director_service();

    DirectorActions {
        assign_role: {
            let client = ctx.client.clone();
            Callback::new(move |(role_id, rig_id): (Uuid, Uuid)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::AssignRoleGlobal { role_id, rig_id })
                        .await;
                    // Refresh rig views
                    let views = client.get_rig_views().await;
                    *DIRECTOR_RIG_VIEWS.write() = views;
                });
            })
        },

        clear_role: {
            let client = ctx.client.clone();
            Callback::new(move |rig_id: Uuid| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::ClearRoleAssignment { rig_id })
                        .await;
                    let views = client.get_rig_views().await;
                    *DIRECTOR_RIG_VIEWS.write() = views;
                });
            })
        },

        set_rig_enabled: {
            let client = ctx.client.clone();
            Callback::new(move |(rig_id, enabled): (Uuid, bool)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetRigEnabled { rig_id, enabled })
                        .await;
                });
            })
        },

        set_rig_level: {
            let client = ctx.client.clone();
            Callback::new(move |(rig_id, level): (Uuid, f64)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetRigLevel { rig_id, level })
                        .await;
                });
            })
        },

        set_rig_pan: {
            let client = ctx.client.clone();
            Callback::new(move |(rig_id, pan): (Uuid, f64)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetRigPan { rig_id, pan })
                        .await;
                });
            })
        },

        set_engine_enabled: {
            let client = ctx.client.clone();
            Callback::new(move |(rig_id, module_type, enabled): (Uuid, ModuleType, bool)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetModuleEnabled {
                            rig_id,
                            module_type,
                            enabled,
                        })
                        .await;
                });
            })
        },

        set_engine_level: {
            let client = ctx.client.clone();
            Callback::new(move |(rig_id, module_type, level): (Uuid, ModuleType, f64)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetModuleLevel {
                            rig_id,
                            module_type,
                            level,
                        })
                        .await;
                });
            })
        },

        set_macro_value: {
            let client = ctx.client.clone();
            Callback::new(move |(rig_id, macro_id, value): (Uuid, Uuid, f64)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetMacroValue {
                            rig_id,
                            macro_id,
                            value,
                        })
                        .await;
                });
            })
        },

        set_send_level: {
            let client = ctx.client.clone();
            Callback::new(move |(rig_id, sends_block_id, level): (Uuid, Uuid, f64)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetSendLevel {
                            rig_id,
                            sends_block_id,
                            level,
                        })
                        .await;
                });
            })
        },

        set_send_enabled: {
            let client = ctx.client.clone();
            Callback::new(move |(rig_id, sends_block_id, enabled): (Uuid, Uuid, bool)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetSendEnabled {
                            rig_id,
                            sends_block_id,
                            enabled,
                        })
                        .await;
                });
            })
        },

        reassign_input: {
            let client = ctx.client.clone();
            Callback::new(move |(rig_id, input_index): (Uuid, u8)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::ReassignInput { rig_id, input_index })
                        .await;
                });
            })
        },

        set_input_gain: {
            let client = ctx.client.clone();
            Callback::new(move |(rig_id, gain_db): (Uuid, f64)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetInputGain { rig_id, gain_db })
                        .await;
                });
            })
        },

        set_phantom_power: {
            let client = ctx.client.clone();
            Callback::new(move |(rig_id, enabled): (Uuid, bool)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetPhantomPower { rig_id, enabled })
                        .await;
                });
            })
        },

        set_sends_block_enabled: {
            let client = ctx.client.clone();
            Callback::new(move |(block_id, enabled): (Uuid, bool)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetSendsBlockEnabled { block_id, enabled })
                        .await;
                });
            })
        },

        set_sends_parameter: {
            let client = ctx.client.clone();
            Callback::new(move |(block_id, param_index, value): (Uuid, u32, f64)| {
                let client = client.clone();
                spawn(async move {
                    client
                        .execute(DirectorCommand::SetSendsParameter {
                            block_id,
                            param_index,
                            value,
                        })
                        .await;
                });
            })
        },
    }
}
