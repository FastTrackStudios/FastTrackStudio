//! Sync Protocol — Shared types and service definitions for cloud sync authentication
//!
//! This crate provides:
//! - Domain types (`User`, `AuthToken`, `AuthProvider`, `AuthState`)
//! - Service trait definitions (`AuthService`, `SyncStatusService`)
//!
//! Note: Uses `async_trait` instead of `roam::service` to avoid upstream
//! `roam-session` breakage from `facet-path` API changes.

pub mod auth;
pub mod sync_status;
pub mod user;

pub use auth::{AuthProvider, AuthState, AuthToken, OAuthCallbackParams};
pub use sync_status::{SyncState, SyncStatusService};
pub use user::User;

// region: --- AuthService

/// Service for managing user authentication with OAuth2 providers.
///
/// Handles the full OAuth2 flow: initiating login, processing callbacks,
/// storing tokens, and managing session lifecycle.
#[async_trait::async_trait]
pub trait AuthService: Send + Sync {
    /// Begin the OAuth2 login flow for the given provider.
    /// Returns an authorization URL the user should open in their browser.
    async fn login(&self, provider: AuthProvider) -> Result<String, String>;

    /// Handle the OAuth2 callback after user authorization.
    /// Exchanges the auth code for tokens and creates/updates the user session.
    async fn handle_callback(&self, params: OAuthCallbackParams) -> Result<User, String>;

    /// Sign out the current user, clearing stored tokens.
    async fn logout(&self) -> Result<(), String>;

    /// Get the current authentication state.
    async fn get_auth_state(&self) -> AuthState;

    /// Get the currently authenticated user, if any.
    async fn get_current_user(&self) -> Option<User>;

    /// Attempt to restore a previous session from stored tokens.
    /// Returns the user if a valid session was restored.
    async fn restore_session(&self) -> Option<User>;
}

// endregion: --- AuthService
