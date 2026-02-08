//! Authentication types for OAuth2 flows

/// Supported OAuth2 providers
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AuthProvider {
    GitHub,
    Google,
}

impl std::fmt::Display for AuthProvider {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::GitHub => write!(f, "GitHub"),
            Self::Google => write!(f, "Google"),
        }
    }
}

/// Current authentication state
#[repr(u8)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AuthState {
    /// No user is authenticated
    Unauthenticated,
    /// OAuth flow is in progress (waiting for callback)
    Authenticating,
    /// User is fully authenticated
    Authenticated,
    /// Authentication failed with an error
    Error(String),
}

impl Default for AuthState {
    fn default() -> Self {
        Self::Unauthenticated
    }
}

/// OAuth2 token pair (access + optional refresh)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AuthToken {
    /// OAuth2 access token
    pub access_token: String,
    /// OAuth2 refresh token (not all providers issue these)
    pub refresh_token: Option<String>,
    /// Token expiry as Unix timestamp (seconds)
    pub expires_at: Option<i64>,
}

impl AuthToken {
    /// Check whether the access token has expired
    pub fn is_expired(&self) -> bool {
        match self.expires_at {
            Some(expires) => chrono::Utc::now().timestamp() >= expires,
            None => false, // No expiry means it doesn't expire
        }
    }
}

/// Parameters from the OAuth2 callback redirect
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OAuthCallbackParams {
    /// Authorization code from the provider
    pub code: String,
    /// CSRF state parameter for validation
    pub state: String,
}
