//! AuthService implementation with OAuth2 and token storage

use std::sync::Arc;
use tokio::sync::Mutex;

use sync_proto::{AuthProvider, AuthState, AuthToken, OAuthCallbackParams, User};

use crate::error::{AuthError, AuthResult};
use crate::oauth::{self, AuthorizationRequest, OAuthConfig};
use crate::token_store::TokenStore;

/// Implementation of the `AuthService` trait
pub struct AuthServiceImpl<T: TokenStore> {
    token_store: Arc<T>,
    state: Mutex<AuthState>,
    current_user: Mutex<Option<User>>,
    /// Pending OAuth request (PKCE verifier + CSRF state + provider)
    pending_auth: Mutex<Option<PendingAuth>>,
    /// OAuth config per provider
    github_config: Option<OAuthConfig>,
    google_config: Option<OAuthConfig>,
}

struct PendingAuth {
    provider: AuthProvider,
    csrf_state: String,
    request: AuthorizationRequest,
}

impl<T: TokenStore> AuthServiceImpl<T> {
    pub fn new(token_store: Arc<T>) -> Self {
        Self {
            token_store,
            state: Mutex::new(AuthState::Unauthenticated),
            current_user: Mutex::new(None),
            pending_auth: Mutex::new(None),
            github_config: None,
            google_config: None,
        }
    }

    pub fn with_github(mut self, config: OAuthConfig) -> Self {
        self.github_config = Some(config);
        self
    }

    pub fn with_google(mut self, config: OAuthConfig) -> Self {
        self.google_config = Some(config);
        self
    }

    fn get_config(&self, provider: AuthProvider) -> AuthResult<&OAuthConfig> {
        match provider {
            AuthProvider::GitHub => self
                .github_config
                .as_ref()
                .ok_or_else(|| AuthError::Provider("GitHub OAuth not configured".into())),
            AuthProvider::Google => self
                .google_config
                .as_ref()
                .ok_or_else(|| AuthError::Provider("Google OAuth not configured".into())),
        }
    }

    /// Initiate OAuth2 login flow
    pub async fn login(&self, provider: AuthProvider) -> AuthResult<String> {
        let config = self.get_config(provider)?;
        let client = oauth::build_client(provider, config)?;
        let auth_request = oauth::authorize_url(&client, provider);

        let auth_url = auth_request.auth_url.clone();
        let csrf_state = auth_request.csrf_state.clone();

        *self.state.lock().await = AuthState::Authenticating;
        *self.pending_auth.lock().await = Some(PendingAuth {
            provider,
            csrf_state,
            request: auth_request,
        });

        tracing::info!(%provider, "OAuth2 login initiated");
        Ok(auth_url)
    }

    /// Handle OAuth2 callback
    pub async fn handle_callback(&self, params: OAuthCallbackParams) -> AuthResult<User> {
        let pending = self
            .pending_auth
            .lock()
            .await
            .take()
            .ok_or(AuthError::NoSession)?;

        // Validate CSRF state
        if params.state != pending.csrf_state {
            *self.state.lock().await = AuthState::Error("CSRF state mismatch".into());
            return Err(AuthError::InvalidState);
        }

        let config = self.get_config(pending.provider)?;
        let client = oauth::build_client(pending.provider, config)?;

        // Exchange code for tokens
        let token = oauth::exchange_code(&client, &params.code, pending.request.pkce_verifier).await?;

        // Fetch user profile from provider
        let user = oauth::fetch_user_profile(pending.provider, &token.access_token).await?;

        // Persist token and provider
        self.token_store.save_token(&token)?;
        self.token_store
            .save_provider(&pending.provider.to_string())?;

        *self.current_user.lock().await = Some(user.clone());
        *self.state.lock().await = AuthState::Authenticated;

        tracing::info!(
            provider = %pending.provider,
            email = %user.email,
            "User authenticated successfully"
        );

        Ok(user)
    }

    /// Sign out
    pub async fn logout(&self) -> AuthResult<()> {
        self.token_store.delete_token()?;
        *self.current_user.lock().await = None;
        *self.state.lock().await = AuthState::Unauthenticated;
        tracing::info!("User signed out");
        Ok(())
    }

    /// Get current auth state
    pub async fn get_auth_state(&self) -> AuthState {
        self.state.lock().await.clone()
    }

    /// Get current user
    pub async fn get_current_user(&self) -> Option<User> {
        self.current_user.lock().await.clone()
    }

    /// Try to restore a session from stored tokens
    pub async fn restore_session(&self) -> Option<User> {
        let token = match self.token_store.load_token() {
            Ok(Some(t)) => t,
            _ => return None,
        };

        let provider_str = match self.token_store.load_provider() {
            Ok(Some(p)) => p,
            _ => return None,
        };

        let provider = match provider_str.as_str() {
            "GitHub" => AuthProvider::GitHub,
            "Google" => AuthProvider::Google,
            _ => return None,
        };

        // Check if token is expired
        if token.is_expired() {
            // Try to refresh
            if let Some(refresh) = &token.refresh_token {
                let config = self.get_config(provider).ok()?;
                let client = oauth::build_client(provider, config).ok()?;
                match oauth::refresh_token(&client, refresh).await {
                    Ok(new_token) => {
                        let _ = self.token_store.save_token(&new_token);
                        match oauth::fetch_user_profile(provider, &new_token.access_token).await {
                            Ok(user) => {
                                *self.current_user.lock().await = Some(user.clone());
                                *self.state.lock().await = AuthState::Authenticated;
                                return Some(user);
                            }
                            Err(_) => return None,
                        }
                    }
                    Err(_) => {
                        let _ = self.token_store.delete_token();
                        return None;
                    }
                }
            }
            let _ = self.token_store.delete_token();
            return None;
        }

        // Token still valid — fetch profile
        match oauth::fetch_user_profile(provider, &token.access_token).await {
            Ok(user) => {
                *self.current_user.lock().await = Some(user.clone());
                *self.state.lock().await = AuthState::Authenticated;
                Some(user)
            }
            Err(_) => {
                let _ = self.token_store.delete_token();
                None
            }
        }
    }
}
