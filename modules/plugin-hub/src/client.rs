//! Client-side hub connection for plugins.
//!
//! This module is embedded in each FTS plugin and handles communication
//! with the central hub.

use crate::protocol::*;
use crate::types::*;
use parking_lot::RwLock;
use std::sync::Arc;
use tokio::sync::mpsc;

/// Connection to the plugin hub from a plugin.
pub struct HubClient {
    /// Plugin's unique instance ID
    plugin_id: PluginInstanceId,
    /// Plugin info
    info: PluginInfo,
    /// Channel to send messages to hub
    tx: mpsc::Sender<PluginToHub>,
    /// Pending parameter updates (batched for efficiency)
    pending_params: Arc<RwLock<Vec<(ParamId, f32)>>>,
}

impl HubClient {
    /// Create a new hub client for a plugin.
    ///
    /// Returns the client and a receiver for messages from the hub.
    #[must_use]
    pub fn new(info: PluginInfo, tx: mpsc::Sender<PluginToHub>) -> Self {
        Self {
            plugin_id: info.id,
            info,
            tx,
            pending_params: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Get the plugin's instance ID.
    #[must_use]
    pub fn plugin_id(&self) -> PluginInstanceId {
        self.plugin_id
    }

    /// Send registration message to hub.
    pub async fn register(&self) -> Result<(), HubClientError> {
        self.tx
            .send(PluginToHub::Register(self.info.clone()))
            .await
            .map_err(|_| HubClientError::Disconnected)
    }

    /// Send unregister message to hub.
    pub async fn unregister(&self) -> Result<(), HubClientError> {
        self.tx
            .send(PluginToHub::Unregister {
                plugin_id: self.plugin_id,
            })
            .await
            .map_err(|_| HubClientError::Disconnected)
    }

    /// Report all parameters to the hub.
    pub async fn report_params(&self, params: Vec<ParamInfo>) -> Result<(), HubClientError> {
        self.tx
            .send(PluginToHub::ReportParams {
                plugin_id: self.plugin_id,
                params,
            })
            .await
            .map_err(|_| HubClientError::Disconnected)
    }

    /// Notify hub of a parameter change.
    pub async fn param_changed(&self, param_id: ParamId, value: f32) -> Result<(), HubClientError> {
        self.tx
            .send(PluginToHub::ParamChanged {
                plugin_id: self.plugin_id,
                param_id,
                value,
            })
            .await
            .map_err(|_| HubClientError::Disconnected)
    }

    /// Send meter data to hub (should be called from audio thread via lock-free queue).
    pub async fn update_meters(&self, meters: MeterData) -> Result<(), HubClientError> {
        self.tx
            .send(PluginToHub::MeterUpdate {
                plugin_id: self.plugin_id,
                meters,
            })
            .await
            .map_err(|_| HubClientError::Disconnected)
    }

    /// Notify hub of bypass state change.
    pub async fn bypass_changed(&self, bypassed: bool) -> Result<(), HubClientError> {
        self.tx
            .send(PluginToHub::BypassChanged {
                plugin_id: self.plugin_id,
                bypassed,
            })
            .await
            .map_err(|_| HubClientError::Disconnected)
    }

    /// Send heartbeat to indicate plugin is alive.
    pub async fn heartbeat(&self) -> Result<(), HubClientError> {
        self.tx
            .send(PluginToHub::Heartbeat {
                plugin_id: self.plugin_id,
            })
            .await
            .map_err(|_| HubClientError::Disconnected)
    }

    /// Handle a message from the hub.
    ///
    /// Returns an action the plugin should take, if any.
    pub fn handle_hub_message(&self, msg: HubToPlugin) -> Option<HubAction> {
        match msg {
            HubToPlugin::Registered { plugin_id } => {
                tracing::info!("Plugin registered with ID: {plugin_id}");
                None
            }
            HubToPlugin::SetParam { param_id, value } => {
                Some(HubAction::SetParam { param_id, value })
            }
            HubToPlugin::BeginEdit { param_id } => Some(HubAction::BeginEdit { param_id }),
            HubToPlugin::EndEdit { param_id } => Some(HubAction::EndEdit { param_id }),
            HubToPlugin::SetBypass { bypassed } => Some(HubAction::SetBypass { bypassed }),
            HubToPlugin::RequestSync => Some(HubAction::SyncRequested),
            HubToPlugin::Ping => {
                // Should respond with heartbeat
                None
            }
        }
    }
}

/// Actions the plugin should take in response to hub messages.
#[derive(Debug, Clone)]
pub enum HubAction {
    /// Set a parameter value
    SetParam { param_id: ParamId, value: f32 },
    /// Begin parameter edit gesture
    BeginEdit { param_id: ParamId },
    /// End parameter edit gesture
    EndEdit { param_id: ParamId },
    /// Set bypass state
    SetBypass { bypassed: bool },
    /// Full state sync requested
    SyncRequested,
}

/// Errors that can occur in hub client operations.
#[derive(Debug, thiserror::Error)]
pub enum HubClientError {
    #[error("Disconnected from hub")]
    Disconnected,
    #[error("Connection failed: {0}")]
    ConnectionFailed(String),
    #[error("Serialization error: {0}")]
    Serialization(String),
}
