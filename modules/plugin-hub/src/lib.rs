//! Plugin Hub - Cross-plugin state sharing for FTS audio plugins.
//!
//! This module provides a hub-and-spoke architecture for sharing state between
//! FTS audio plugins running in a DAW. It uses shared memory for ultra-low latency
//! communication between plugins in the same process, and supports external clients
//! via WebSocket/TCP for browser and desktop apps.
//!
//! # Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────────┐
//! │                         DAW Process                              │
//! │                                                                  │
//! │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐              │
//! │  │ FTS Gain    │  │ FTS EQ      │  │ FTS Comp    │  (plugins)   │
//! │  │             │  │             │  │             │              │
//! │  │ HubClient   │  │ HubClient   │  │ HubClient   │              │
//! │  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘              │
//! │         │                │                │                      │
//! │         └────────────────┼────────────────┘                      │
//! │                          ▼                                       │
//! │              ┌─────────────────────────┐                         │
//! │              │     Plugin Hub          │                         │
//! │              │  (runs in extension)    │                         │
//! │              └────────────┬────────────┘                         │
//! └──────────────────────────┼───────────────────────────────────────┘
//!                            │
//!          ┌─────────────────┼─────────────────┐
//!          ▼                 ▼                 ▼
//!    ┌──────────┐     ┌──────────┐      ┌──────────┐
//!    │ Desktop  │     │ Browser  │      │ Mobile   │
//!    │ App      │     │ UI       │      │ App      │
//!    └──────────┘     └──────────┘      └──────────┘
//! ```
//!
//! # Usage
//!
//! ## Plugin Side (Client)
//!
//! ```ignore
//! use plugin_hub::client::HubClient;
//!
//! // In plugin initialization
//! let client = HubClient::connect().await?;
//! client.register_plugin(PluginInfo {
//!     id: generate_instance_id(),
//!     plugin_type: PluginType::Gain,
//!     track_name: "Drums".to_string(),
//! }).await?;
//!
//! // Push parameter updates
//! client.set_param(param_id, value).await?;
//!
//! // Push meter data (lock-free)
//! client.set_meter(MeterData { peak_l: 0.5, peak_r: 0.6 });
//! ```
//!
//! ## Host Side (Hub)
//!
//! ```ignore
//! use plugin_hub::host::PluginHub;
//!
//! // In REAPER extension initialization
//! let hub = PluginHub::new();
//! hub.start_websocket_server(9999).await?;
//!
//! // Subscribe to all plugin updates
//! let mut updates = hub.subscribe_all().await;
//! while let Some(update) = updates.next().await {
//!     // Forward to WebSocket clients, etc.
//! }
//! ```

pub mod protocol;
pub mod types;

#[cfg(feature = "client")]
pub mod client;

#[cfg(feature = "host")]
pub mod host;

pub use protocol::*;
pub use types::*;
