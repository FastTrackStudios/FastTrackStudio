//! Gateway protocol for external client connections.
//!
//! This crate defines the shared protocol used by all gateway cells (WebSocket, OSC, Iroh, etc.)
//! to coordinate with the host and handle handoff when a desktop app takes over.

use facet::Facet;

/// Gateway state (shared across all gateway types).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum GatewayState {
    /// Gateway is active and accepting connections.
    Active = 0,
    /// Gateway is suspended (desktop app took over).
    Suspended = 1,
}

/// Identifies a gateway type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum GatewayType {
    /// WebSocket gateway for browser clients.
    WebSocket = 0,
    /// Iroh P2P gateway for decentralized connections.
    Iroh = 1,
    /// OSC gateway for hardware controllers.
    Osc = 2,
}

/// Request to take over a specific gateway.
#[derive(Debug, Clone, Facet)]
pub struct TakeOverRequest {
    /// Which gateway type to take over.
    pub gateway_type: GatewayType,
    /// Where clients should reconnect (type-specific format).
    /// - WebSocket: "ws://host:port/path"
    /// - OSC: "host:port"
    /// - Iroh: node ID
    pub redirect_info: String,
}

/// Response to a takeover request.
#[derive(Debug, Clone, Facet)]
pub struct TakeOverResponse {
    /// Whether the takeover was successful.
    pub success: bool,
    /// Number of active connections that will be redirected.
    pub active_connections: u32,
}

/// State info for a single gateway.
#[derive(Debug, Clone, Facet)]
pub struct GatewayInfo {
    /// The gateway type.
    pub gateway_type: GatewayType,
    /// Current state.
    pub state: GatewayState,
    /// Number of active connections (if active).
    pub connection_count: u32,
}

/// Coordination protocol for gateway handoff.
///
/// The host implements this service. Desktop apps call it to take over
/// or release control of gateway cells.
#[allow(async_fn_in_trait)]
#[roam::service]
pub trait GatewayCoordinator {
    /// Request to take over a specific gateway type.
    ///
    /// The host will tell the gateway cell to suspend and redirect
    /// its connected clients to the provided redirect URL.
    async fn take_over(&self, request: TakeOverRequest) -> TakeOverResponse;

    /// Release control of a specific gateway type back to the host.
    ///
    /// The host will resume the gateway cell.
    async fn release(&self, gateway_type: GatewayType) -> ();

    /// Get the current state of all gateways.
    async fn get_states(&self) -> Vec<GatewayInfo>;
}

/// Events pushed from gateway to connected clients.
///
/// Each gateway type serializes these appropriately for its transport.
#[derive(Debug, Clone, Facet)]
#[repr(u8)]
pub enum GatewayEvent {
    /// Client should reconnect to a different endpoint.
    Redirect {
        /// New endpoint to connect to (format depends on gateway type).
        info: String,
    } = 0,
    /// Gateway is shutting down.
    Shutdown = 1,
}

/// Service that each gateway cell implements for the host to control it.
///
/// This allows the host to suspend/resume individual gateways.
#[allow(async_fn_in_trait)]
#[roam::service]
pub trait GatewayControl {
    /// Suspend this gateway, redirecting clients to the given endpoint.
    async fn suspend(&self, redirect_info: String) -> u32;

    /// Resume this gateway.
    async fn resume(&self) -> ();

    /// Get current state.
    async fn get_state(&self) -> GatewayInfo;
}
