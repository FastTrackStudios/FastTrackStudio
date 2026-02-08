//! Auth error types

pub type AuthResult<T> = Result<T, AuthError>;

#[derive(Debug, thiserror::Error)]
pub enum AuthError {
    #[error("OAuth2 error: {0}")]
    OAuth(String),

    #[error("token store error: {0}")]
    TokenStore(String),

    #[error("token expired and no refresh token available")]
    TokenExpired,

    #[error("invalid callback state: CSRF mismatch")]
    InvalidState,

    #[error("no active session")]
    NoSession,

    #[error("network error: {0}")]
    Network(String),

    #[error("provider error: {0}")]
    Provider(String),
}

impl From<reqwest::Error> for AuthError {
    fn from(e: reqwest::Error) -> Self {
        Self::Network(e.to_string())
    }
}
