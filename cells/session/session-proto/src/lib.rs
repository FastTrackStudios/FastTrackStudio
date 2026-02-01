//! Session Protocol - Shared types and service definitions for Session cell

use roam::service;

/// Session service - provides health check and info
#[service]
pub trait SessionService {
    /// Get session cell status
    async fn get_status(&self) -> String;
}
