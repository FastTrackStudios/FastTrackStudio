//! Protocol messages for hub communication.
//!
//! These messages are serialized with postcard for efficient binary encoding.

use crate::types::*;
use serde::{Deserialize, Serialize};

/// Messages sent from plugin to hub.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PluginToHub {
    /// Register a new plugin instance with the hub.
    Register(PluginInfo),

    /// Unregister a plugin instance (on plugin unload).
    Unregister { plugin_id: PluginInstanceId },

    /// Report all parameters for this plugin.
    ReportParams {
        plugin_id: PluginInstanceId,
        params: Vec<ParamInfo>,
    },

    /// Parameter value changed (from plugin).
    ParamChanged {
        plugin_id: PluginInstanceId,
        param_id: ParamId,
        value: f32,
    },

    /// Meter data update (high frequency).
    MeterUpdate {
        plugin_id: PluginInstanceId,
        meters: MeterData,
    },

    /// Bypass state changed.
    BypassChanged {
        plugin_id: PluginInstanceId,
        bypassed: bool,
    },

    /// Heartbeat to indicate plugin is still alive.
    Heartbeat { plugin_id: PluginInstanceId },
}

/// Messages sent from hub to plugin.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HubToPlugin {
    /// Registration acknowledged with assigned ID.
    Registered { plugin_id: PluginInstanceId },

    /// Set a parameter value (from external UI).
    SetParam { param_id: ParamId, value: f32 },

    /// Begin parameter edit gesture (for automation).
    BeginEdit { param_id: ParamId },

    /// End parameter edit gesture.
    EndEdit { param_id: ParamId },

    /// Set bypass state.
    SetBypass { bypassed: bool },

    /// Request full state sync.
    RequestSync,

    /// Ping to check if plugin is alive.
    Ping,
}

/// Messages for external clients (browser, desktop app).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HubBroadcast {
    /// A new plugin was registered.
    PluginAdded(PluginInfo),

    /// A plugin was unregistered.
    PluginRemoved { plugin_id: PluginInstanceId },

    /// Full state sync for a plugin.
    PluginState(PluginState),

    /// Parameter changed on a plugin.
    ParamChanged {
        plugin_id: PluginInstanceId,
        param_id: ParamId,
        value: f32,
    },

    /// Meter data update (batched for efficiency).
    MeterUpdates(Vec<(PluginInstanceId, MeterData)>),

    /// Full channel strip state.
    ChannelStrip(ChannelStripState),

    /// List of all registered plugins.
    PluginList(Vec<PluginInfo>),
}

/// Messages from external clients to hub.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClientToHub {
    /// Subscribe to all updates.
    SubscribeAll,

    /// Subscribe to specific plugin.
    SubscribePlugin { plugin_id: PluginInstanceId },

    /// Subscribe to meter updates only (high frequency).
    SubscribeMeters,

    /// Request list of all plugins.
    ListPlugins,

    /// Set parameter on a plugin.
    SetParam {
        plugin_id: PluginInstanceId,
        param_id: ParamId,
        value: f32,
    },

    /// Begin parameter edit.
    BeginEdit {
        plugin_id: PluginInstanceId,
        param_id: ParamId,
    },

    /// End parameter edit.
    EndEdit {
        plugin_id: PluginInstanceId,
        param_id: ParamId,
    },

    /// Set bypass state.
    SetBypass {
        plugin_id: PluginInstanceId,
        bypassed: bool,
    },

    /// Request full state for a plugin.
    RequestState { plugin_id: PluginInstanceId },
}
