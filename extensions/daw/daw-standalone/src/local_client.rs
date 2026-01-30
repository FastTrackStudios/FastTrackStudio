//! Local Client for DAW Standalone Service
//!
//! Provides a wrapper for calling DAW services directly in-process without RPC.
//! Similar to LocalSetlistClient pattern from dodeca.

use daw_proto::*;
use std::sync::Arc;
use super::DawStandaloneTransport;

/// Create a dummy ROAM context for in-process service calls.
fn local_context() -> roam_session::Context {
    use roam_wire::{ConnectionId, MethodId, RequestId};
    roam_session::Context::new(
        ConnectionId(0),
        RequestId(0),
        MethodId(0),
        vec![],
        vec![],
    )
}

/// Local client for direct DAW service calls (no RPC)
#[derive(Clone)]
pub struct LocalDawClient {
    transport: Arc<DawStandaloneTransport>,
}

impl LocalDawClient {
    /// Create a new local client wrapping a transport service
    pub fn new(transport: Arc<DawStandaloneTransport>) -> Self {
        Self { transport }
    }

    /// Play
    pub async fn play(&self) -> TransportResult {
        self.transport.play(&local_context()).await
    }

    /// Stop
    pub async fn stop(&self) -> TransportResult {
        self.transport.stop(&local_context()).await
    }

    /// Record
    pub async fn record(&self) -> TransportResult {
        self.transport.record(&local_context()).await
    }

    /// Get playback state
    pub async fn get_state(&self) -> PlaybackState {
        self.transport.get_state(&local_context()).await
    }

    /// Set playback position
    pub async fn set_position(&self, seconds: f64) -> TransportResult {
        self.transport.set_position(&local_context(), seconds).await
    }
}
