//! Session Protocol - Shared types and service definitions for Session cell

use roam::service;

// Re-export DefinesActions for convenience - session implements this
pub use actions_proto::{
    ActionCategory, ActionDefinition, ActionId, ActionResult, DefinesActions, DefinesActionsClient,
    DefinesActionsDispatcher,
};

/// Session service - provides session-specific functionality
#[service]
pub trait SessionService {
    /// Get session cell status
    async fn get_status(&self) -> String;
}
