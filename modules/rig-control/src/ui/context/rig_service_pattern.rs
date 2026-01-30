//! RigService pattern - unified interface for all transport types
//!
//! This demonstrates how to structure the service to support:
//! - Local/in-process (audio plugins, testing)
//! - SHM (desktop ↔ REAPER)
//! - Network (web/mobile apps)
//!
//! All use the same service trait, just different transports.

use std::sync::Arc;
use crate::{RigControlService, MockRigControlService, PresetInfo};

/// RigService wraps different transport mechanisms
///
/// The key insight: All transports use the same `RigControlService` trait,
/// so your UI code doesn't need to know which transport is being used.
pub enum RigService {
    /// Local/in-process - just calls the service directly
    ///
    /// Use for:
    /// - Audio plugins (VST/CLAP) - need minimal latency
    /// - Unit tests - fast, deterministic
    /// - Single-process dev mode
    ///
    /// Performance: ~0.1-1 nanosecond overhead (essentially free)
    Local(Arc<dyn RigControlService + Send + Sync>),

    // Future: Add when implementing SHM
    // /// Shared memory transport
    // ///
    // /// Use for:
    // /// - Desktop app ↔ REAPER extension
    // /// - Crash isolation (plugin crash doesn't kill host)
    // /// - Hot reload without restart
    // ///
    // /// Performance: ~1-2 microseconds (cache-line copy)
    // Shm(roam_shm::ConnectionHandle),

    // Future: Add when implementing network control
    // /// Network transport (TCP/WebSocket)
    // ///
    // /// Use for:
    // /// - Web apps (via WebSocket bridge)
    // /// - Mobile apps (via TCP)
    // /// - Remote control
    // ///
    // /// Performance: ~50-200 microseconds (localhost)
    // Network(Arc<roam_reconnect::ReconnectingClient<TcpConnector>>),
}

impl RigService {
    /// Create a local service (for audio plugins, testing, dev)
    pub fn local<S>(service: S) -> Self
    where
        S: RigControlService + Send + Sync + 'static
    {
        Self::Local(Arc::new(service))
    }

    /// Create with mock service (convenience for testing/demo)
    pub fn mock() -> Self {
        Self::local(MockRigControlService::with_guitar_defaults())
    }

    // Future: Add when SHM is implemented
    // pub fn shm(connection: roam_shm::ConnectionHandle) -> Self {
    //     Self::Shm(connection)
    // }

    // Future: Add when network is implemented
    // pub fn network(client: Arc<roam_reconnect::ReconnectingClient<TcpConnector>>) -> Self {
    //     Self::Network(client)
    // }

    /// Get current preset - works regardless of transport
    pub async fn get_current_preset(&self) -> Option<PresetInfo> {
        match self {
            Self::Local(service) => {
                // For local calls, create a dummy context
                // The context is unused but required by the trait signature
                let cx = roam::Context::new(
                    Default::default(),
                    Default::default(),
                    Default::default(),
                    Default::default(),
                    vec![],
                );
                service.get_current_preset(&cx).await
            }

            // Future: SHM implementation
            // Self::Shm(handle) => {
            //     let client = RigControlClient::new(handle.clone());
            //     client.get_current_preset().await.ok().flatten()
            // }

            // Future: Network implementation
            // Self::Network(client) => {
            //     client.call(rig_control_method_id::get_current_preset(), &())
            //         .await
            //         .ok()
            // }
        }
    }

    // Add similar wrappers for all other service methods...
}

/// Example: How this would be used in different contexts
#[cfg(test)]
mod examples {
    use super::*;

    #[tokio::test]
    async fn audio_plugin_usage() {
        // In a VST/CLAP plugin - use local for zero overhead
        // (Would use LiveRigControlService in production)
        let service = RigService::mock();

        // This is essentially a direct function call
        let preset = service.get_current_preset().await;
        assert!(preset.is_some());
    }

    #[tokio::test]
    async fn desktop_app_usage() {
        // In desktop app - would use SHM for crash isolation
        // For now, use local in dev mode
        let service = RigService::mock();

        // Same call, different transport
        let preset = service.get_current_preset().await;
        assert!(preset.is_some());
    }

    #[tokio::test]
    async fn swap_implementations() {
        // Can easily swap between implementations
        let use_shm = false; // Config/environment flag

        let service = if use_shm {
            // Future: RigService::shm(connect_to_reaper()?)
            RigService::mock() // Placeholder
        } else {
            RigService::mock()
        };

        // UI code doesn't change!
        let preset = service.get_current_preset().await;
        assert!(preset.is_some());
    }
}

// --- Performance Comparison ---
//
// | Transport | Latency    | Use Case                    |
// |-----------|------------|-----------------------------|
// | Local     | ~0.1-1ns   | Audio plugins, tests        |
// | SHM       | ~1-2µs     | Desktop ↔ REAPER            |
// | Network   | ~50-200µs  | Web/mobile, remote control  |
//
// For rig control (preset switching, parameter updates), even network
// latency is totally acceptable. Audio DSP would use Local.
