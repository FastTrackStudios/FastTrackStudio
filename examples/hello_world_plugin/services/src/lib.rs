//! Hello World Plugin - ROAM Service Definitions
//!
//! These service traits define the API that clients can call.
//! Implementations can be hot-reloaded without changing these definitions.

use uuid::Uuid;

// ═══════════════════════════════════════════════════════════════════════════
// Greeter Service
// ═══════════════════════════════════════════════════════════════════════════

/// A simple greeting service that can be hot-reloaded
#[roam::service]
pub trait Greeter {
    /// Greet someone by name
    async fn greet(&self, name: String) -> String;

    /// Change the greeting prefix (e.g., "Hello" -> "Hola")
    async fn set_greeting(&self, greeting: String);

    /// Get the current greeting prefix
    async fn get_greeting(&self) -> String;
}

// ═══════════════════════════════════════════════════════════════════════════
// Counter Service
// ═══════════════════════════════════════════════════════════════════════════

/// A stateful counter service that preserves state across hot-reloads
#[roam::service]
pub trait Counter {
    /// Increment the counter and return the new value
    async fn increment(&self) -> i32;

    /// Decrement the counter and return the new value
    async fn decrement(&self) -> i32;

    /// Get the current counter value
    async fn get_count(&self) -> i32;

    /// Reset the counter to zero
    async fn reset(&self);
}

// ═══════════════════════════════════════════════════════════════════════════
// Session Service (Example of more complex service)
// ═══════════════════════════════════════════════════════════════════════════

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct SessionInfo {
    pub id: Uuid,
    pub name: String,
    pub created_at: i64,
}

/// Manages REAPER sessions
#[roam::service]
pub trait Session {
    /// Create a new session
    async fn create_session(&self, name: String) -> SessionInfo;

    /// Get the current session
    async fn get_current_session(&self) -> Option<SessionInfo>;

    /// List all sessions
    async fn list_sessions(&self) -> Vec<SessionInfo>;
}
