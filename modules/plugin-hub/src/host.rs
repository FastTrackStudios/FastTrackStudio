//! Host-side hub implementation.
//!
//! This module runs in the REAPER extension or standalone app and coordinates
//! communication between all FTS plugins.

use crate::protocol::*;
use crate::types::*;
use parking_lot::RwLock;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::broadcast;

/// The central hub that coordinates all plugin communication.
pub struct PluginHub {
    /// Registered plugins by ID
    plugins: Arc<RwLock<HashMap<PluginInstanceId, PluginState>>>,
    /// Broadcast channel for external clients
    broadcast_tx: broadcast::Sender<HubBroadcast>,
}

impl Default for PluginHub {
    fn default() -> Self {
        Self::new()
    }
}

impl PluginHub {
    /// Create a new plugin hub.
    #[must_use]
    pub fn new() -> Self {
        let (broadcast_tx, _) = broadcast::channel(256);
        Self {
            plugins: Arc::new(RwLock::new(HashMap::new())),
            broadcast_tx,
        }
    }

    /// Register a new plugin with the hub.
    pub fn register_plugin(&self, info: PluginInfo) -> PluginInstanceId {
        let id = info.id;
        let state = PluginState {
            info: info.clone(),
            params: Vec::new(),
            meters: MeterData::default(),
            bypassed: false,
        };

        self.plugins.write().insert(id, state);

        // Broadcast to external clients
        let _ = self.broadcast_tx.send(HubBroadcast::PluginAdded(info));

        id
    }

    /// Unregister a plugin from the hub.
    pub fn unregister_plugin(&self, plugin_id: PluginInstanceId) {
        self.plugins.write().remove(&plugin_id);
        let _ = self
            .broadcast_tx
            .send(HubBroadcast::PluginRemoved { plugin_id });
    }

    /// Update parameters for a plugin.
    pub fn update_params(&self, plugin_id: PluginInstanceId, params: Vec<ParamInfo>) {
        if let Some(state) = self.plugins.write().get_mut(&plugin_id) {
            state.params = params;
        }
    }

    /// Handle a parameter change from a plugin.
    pub fn handle_param_change(&self, plugin_id: PluginInstanceId, param_id: ParamId, value: f32) {
        // Update stored state
        if let Some(state) = self.plugins.write().get_mut(&plugin_id) {
            if let Some(param) = state.params.iter_mut().find(|p| p.id == param_id) {
                param.value = value;
            }
        }

        // Broadcast to external clients
        let _ = self.broadcast_tx.send(HubBroadcast::ParamChanged {
            plugin_id,
            param_id,
            value,
        });
    }

    /// Handle meter data from a plugin.
    pub fn handle_meter_update(&self, plugin_id: PluginInstanceId, meters: MeterData) {
        if let Some(state) = self.plugins.write().get_mut(&plugin_id) {
            state.meters = meters;
        }
    }

    /// Get a list of all registered plugins.
    #[must_use]
    pub fn list_plugins(&self) -> Vec<PluginInfo> {
        self.plugins
            .read()
            .values()
            .map(|s| s.info.clone())
            .collect()
    }

    /// Get the full state of a plugin.
    #[must_use]
    pub fn get_plugin_state(&self, plugin_id: PluginInstanceId) -> Option<PluginState> {
        self.plugins.read().get(&plugin_id).cloned()
    }

    /// Subscribe to all hub broadcasts.
    #[must_use]
    pub fn subscribe(&self) -> broadcast::Receiver<HubBroadcast> {
        self.broadcast_tx.subscribe()
    }

    /// Process a message from a plugin.
    pub fn handle_plugin_message(&self, msg: PluginToHub) {
        match msg {
            PluginToHub::Register(info) => {
                self.register_plugin(info);
            }
            PluginToHub::Unregister { plugin_id } => {
                self.unregister_plugin(plugin_id);
            }
            PluginToHub::ReportParams { plugin_id, params } => {
                self.update_params(plugin_id, params);
            }
            PluginToHub::ParamChanged {
                plugin_id,
                param_id,
                value,
            } => {
                self.handle_param_change(plugin_id, param_id, value);
            }
            PluginToHub::MeterUpdate { plugin_id, meters } => {
                self.handle_meter_update(plugin_id, meters);
            }
            PluginToHub::BypassChanged {
                plugin_id,
                bypassed,
            } => {
                if let Some(state) = self.plugins.write().get_mut(&plugin_id) {
                    state.bypassed = bypassed;
                }
            }
            PluginToHub::Heartbeat { .. } => {
                // TODO: Track last heartbeat for each plugin
            }
        }
    }

    /// Process a message from an external client.
    pub fn handle_client_message(&self, msg: ClientToHub) -> Option<HubBroadcast> {
        match msg {
            ClientToHub::ListPlugins => Some(HubBroadcast::PluginList(self.list_plugins())),
            ClientToHub::RequestState { plugin_id } => self
                .get_plugin_state(plugin_id)
                .map(HubBroadcast::PluginState),
            ClientToHub::SetParam {
                plugin_id,
                param_id,
                value,
            } => {
                // TODO: Forward to plugin
                tracing::debug!(
                    "Client requested param change: plugin={plugin_id}, param={param_id}, value={value}"
                );
                None
            }
            ClientToHub::BeginEdit {
                plugin_id,
                param_id,
            } => {
                tracing::debug!("Client began edit: plugin={plugin_id}, param={param_id}");
                None
            }
            ClientToHub::EndEdit {
                plugin_id,
                param_id,
            } => {
                tracing::debug!("Client ended edit: plugin={plugin_id}, param={param_id}");
                None
            }
            ClientToHub::SetBypass {
                plugin_id,
                bypassed,
            } => {
                tracing::debug!("Client set bypass: plugin={plugin_id}, bypassed={bypassed}");
                None
            }
            ClientToHub::SubscribeAll
            | ClientToHub::SubscribePlugin { .. }
            | ClientToHub::SubscribeMeters => {
                // Subscription is handled at the transport layer
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plugin_registration() {
        let hub = PluginHub::new();
        let info = PluginInfo {
            id: generate_instance_id(),
            plugin_type: PluginType::Gain,
            name: "Test Gain".to_string(),
            track_name: Some("Drums".to_string()),
            track_index: Some(0),
            fx_index: Some(0),
        };

        let id = hub.register_plugin(info.clone());
        assert_eq!(id, info.id);

        let plugins = hub.list_plugins();
        assert_eq!(plugins.len(), 1);
        assert_eq!(plugins[0].name, "Test Gain");
    }
}
