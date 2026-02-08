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
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum AuthState {
    /// No user is authenticated
    #[default]
    Unauthenticated,
    /// OAuth flow is in progress (waiting for callback)
    Authenticating,
    /// User is fully authenticated
    Authenticated,
    /// Authentication failed with an error
    Error(String),
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

#[cfg(test)]
mod tests {
    use super::*;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn test_auth_provider_display() -> Result<()> {
        // -- Exec & Check
        assert_eq!(AuthProvider::GitHub.to_string(), "GitHub");
        assert_eq!(AuthProvider::Google.to_string(), "Google");
        Ok(())
    }

    #[test]
    fn test_auth_state_default() -> Result<()> {
        // -- Exec
        let state = AuthState::default();

        // -- Check
        assert_eq!(state, AuthState::Unauthenticated);
        Ok(())
    }

    #[test]
    fn test_auth_token_not_expired_when_no_expiry() -> Result<()> {
        // -- Setup
        let token = AuthToken {
            access_token: "abc".into(),
            refresh_token: None,
            expires_at: None,
        };

        // -- Check
        assert!(!token.is_expired());
        Ok(())
    }

    #[test]
    fn test_auth_token_expired_when_past() -> Result<()> {
        // -- Setup
        let token = AuthToken {
            access_token: "abc".into(),
            refresh_token: None,
            expires_at: Some(0), // Unix epoch — well in the past
        };

        // -- Check
        assert!(token.is_expired());
        Ok(())
    }

    #[test]
    fn test_auth_token_not_expired_when_future() -> Result<()> {
        // -- Setup
        let far_future = chrono::Utc::now().timestamp() + 3600;
        let token = AuthToken {
            access_token: "abc".into(),
            refresh_token: Some("refresh".into()),
            expires_at: Some(far_future),
        };

        // -- Check
        assert!(!token.is_expired());
        Ok(())
    }

    #[test]
    fn test_auth_provider_copy_clone() -> Result<()> {
        // -- Setup
        let provider = AuthProvider::GitHub;
        let copied = provider;

        // -- Check
        assert_eq!(provider, copied);
        Ok(())
    }
}
