//! Hello World Plugin Protocol Definitions
//!
//! Demonstrates bidirectional RPC:
//! - GuestService: Implemented by plugin, called by host
//! - HostService: Implemented by host, called by plugin

// ============================================================================
// Custom Result Enums (dodeca pattern - NOT Result<T>)
// ============================================================================

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, facet::Facet)]
#[repr(u8)]
pub enum GuestStatusResult {
    Success {
        uptime_secs: u64,
        events_processed: u64,
    },
    Error {
        message: String,
    },
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, facet::Facet)]
#[repr(u8)]
pub enum PingResult {
    Pong { message: String },
    Error { message: String },
}

// ============================================================================
// Guest Service (implemented by plugin, called by host)
// ============================================================================

#[roam::service]
pub trait GuestService {
    async fn ping(&self) -> PingResult;
    async fn get_status(&self) -> GuestStatusResult;
}

// ============================================================================
// Host Service (implemented by REAPER extension, called by plugin)
// ============================================================================

/// Result of forwarding an RPC call to another service
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, facet::Facet)]
#[repr(u8)]
pub enum ForwardCallResult {
    /// Call succeeded, returns the raw response bytes (postcard-encoded)
    Success { response_bytes: Vec<u8> },
    /// Service not found
    ServiceNotFound { service_name: String },
    /// RPC call failed
    CallFailed { error: String },
}

#[roam::service]
pub trait HostService {
    /// Log a message from an extension to the host's log
    async fn log_message(&self, level: String, message: String);

    /// Forward an RPC call to another registered service
    ///
    /// This allows extensions (like the HTTP gateway) to call other services
    /// through the host as a mediator.
    ///
    /// # Arguments
    /// * `service_name` - Name of the target service (e.g., "daw", "hello-world")
    /// * `method_id` - The roam method ID to call
    /// * `payload` - Postcard-encoded arguments
    async fn forward_call(
        &self,
        service_name: String,
        method_id: u64,
        payload: Vec<u8>,
    ) -> ForwardCallResult;
}
