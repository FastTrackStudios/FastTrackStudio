//! Secure token storage backed by the system keychain
//!
//! Uses the `keyring` crate for OS-native credential storage:
//! - macOS: Keychain Services
//! - Linux: Secret Service (GNOME Keyring / KWallet)
//! - Windows: Credential Manager

use crate::error::{AuthError, AuthResult};
use serde::{Deserialize, Serialize};
use sync_proto::AuthToken;

const SERVICE_NAME: &str = "com.fasttrackstudio.sync";
const TOKEN_KEY: &str = "auth_token";
const PROVIDER_KEY: &str = "auth_provider";

/// Serde-friendly mirror of `AuthToken` for JSON keychain storage
#[derive(Serialize, Deserialize)]
struct StoredToken {
    access_token: String,
    refresh_token: Option<String>,
    expires_at: Option<i64>,
}

impl From<&AuthToken> for StoredToken {
    fn from(t: &AuthToken) -> Self {
        Self {
            access_token: t.access_token.clone(),
            refresh_token: t.refresh_token.clone(),
            expires_at: t.expires_at,
        }
    }
}

impl From<StoredToken> for AuthToken {
    fn from(s: StoredToken) -> Self {
        Self {
            access_token: s.access_token,
            refresh_token: s.refresh_token,
            expires_at: s.expires_at,
        }
    }
}

/// Trait for storing and retrieving auth tokens
pub trait TokenStore: Send + Sync {
    fn save_token(&self, token: &AuthToken) -> AuthResult<()>;
    fn load_token(&self) -> AuthResult<Option<AuthToken>>;
    fn delete_token(&self) -> AuthResult<()>;
    fn save_provider(&self, provider: &str) -> AuthResult<()>;
    fn load_provider(&self) -> AuthResult<Option<String>>;
}

// region: --- KeychainTokenStore

/// Token storage backed by the OS keychain via `keyring`
pub struct KeychainTokenStore;

impl KeychainTokenStore {
    pub fn new() -> Self {
        Self
    }

    fn entry(key: &str) -> AuthResult<keyring::Entry> {
        keyring::Entry::new(SERVICE_NAME, key).map_err(|e| AuthError::TokenStore(e.to_string()))
    }
}

impl Default for KeychainTokenStore {
    fn default() -> Self {
        Self::new()
    }
}

impl TokenStore for KeychainTokenStore {
    fn save_token(&self, token: &AuthToken) -> AuthResult<()> {
        let stored = StoredToken::from(token);
        let json =
            serde_json::to_string(&stored).map_err(|e| AuthError::TokenStore(e.to_string()))?;
        let entry = Self::entry(TOKEN_KEY)?;
        entry
            .set_password(&json)
            .map_err(|e| AuthError::TokenStore(e.to_string()))
    }

    fn load_token(&self) -> AuthResult<Option<AuthToken>> {
        let entry = Self::entry(TOKEN_KEY)?;
        match entry.get_password() {
            Ok(json) => {
                let stored: StoredToken = serde_json::from_str(&json)
                    .map_err(|e| AuthError::TokenStore(e.to_string()))?;
                Ok(Some(AuthToken::from(stored)))
            }
            Err(keyring::Error::NoEntry) => Ok(None),
            Err(e) => Err(AuthError::TokenStore(e.to_string())),
        }
    }

    fn delete_token(&self) -> AuthResult<()> {
        let entry = Self::entry(TOKEN_KEY)?;
        match entry.delete_credential() {
            Ok(()) => Ok(()),
            Err(keyring::Error::NoEntry) => Ok(()), // Already gone
            Err(e) => Err(AuthError::TokenStore(e.to_string())),
        }
    }

    fn save_provider(&self, provider: &str) -> AuthResult<()> {
        let entry = Self::entry(PROVIDER_KEY)?;
        entry
            .set_password(provider)
            .map_err(|e| AuthError::TokenStore(e.to_string()))
    }

    fn load_provider(&self) -> AuthResult<Option<String>> {
        let entry = Self::entry(PROVIDER_KEY)?;
        match entry.get_password() {
            Ok(p) => Ok(Some(p)),
            Err(keyring::Error::NoEntry) => Ok(None),
            Err(e) => Err(AuthError::TokenStore(e.to_string())),
        }
    }
}

// endregion: --- KeychainTokenStore

// region: --- MockTokenStore

/// In-memory token store for testing
pub struct MockTokenStore {
    token: std::sync::Mutex<Option<String>>,
    provider: std::sync::Mutex<Option<String>>,
}

impl MockTokenStore {
    pub fn new() -> Self {
        Self {
            token: std::sync::Mutex::new(None),
            provider: std::sync::Mutex::new(None),
        }
    }
}

impl Default for MockTokenStore {
    fn default() -> Self {
        Self::new()
    }
}

impl TokenStore for MockTokenStore {
    fn save_token(&self, token: &AuthToken) -> AuthResult<()> {
        let stored = StoredToken::from(token);
        let json =
            serde_json::to_string(&stored).map_err(|e| AuthError::TokenStore(e.to_string()))?;
        *self.token.lock().unwrap() = Some(json);
        Ok(())
    }

    fn load_token(&self) -> AuthResult<Option<AuthToken>> {
        let guard = self.token.lock().unwrap();
        match guard.as_deref() {
            Some(json) => {
                let stored: StoredToken =
                    serde_json::from_str(json).map_err(|e| AuthError::TokenStore(e.to_string()))?;
                Ok(Some(AuthToken::from(stored)))
            }
            None => Ok(None),
        }
    }

    fn delete_token(&self) -> AuthResult<()> {
        *self.token.lock().unwrap() = None;
        *self.provider.lock().unwrap() = None;
        Ok(())
    }

    fn save_provider(&self, provider: &str) -> AuthResult<()> {
        *self.provider.lock().unwrap() = Some(provider.to_string());
        Ok(())
    }

    fn load_provider(&self) -> AuthResult<Option<String>> {
        Ok(self.provider.lock().unwrap().clone())
    }
}

// endregion: --- MockTokenStore

#[cfg(test)]
mod tests {
    use super::*;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    fn make_token() -> AuthToken {
        AuthToken {
            access_token: "access_123".into(),
            refresh_token: Some("refresh_456".into()),
            expires_at: Some(1_700_000_000),
        }
    }

    #[test]
    fn test_mock_token_store_save_and_load() -> Result<()> {
        // -- Setup
        let store = MockTokenStore::new();
        let token = make_token();

        // -- Exec
        store.save_token(&token)?;
        let loaded = store.load_token()?;

        // -- Check
        assert_eq!(loaded, Some(token));
        Ok(())
    }

    #[test]
    fn test_mock_token_store_load_empty() -> Result<()> {
        // -- Setup
        let store = MockTokenStore::new();

        // -- Exec & Check
        assert_eq!(store.load_token()?, None);
        Ok(())
    }

    #[test]
    fn test_mock_token_store_delete() -> Result<()> {
        // -- Setup
        let store = MockTokenStore::new();
        store.save_token(&make_token())?;

        // -- Exec
        store.delete_token()?;

        // -- Check
        assert_eq!(store.load_token()?, None);
        Ok(())
    }

    #[test]
    fn test_mock_token_store_provider_roundtrip() -> Result<()> {
        // -- Setup
        let store = MockTokenStore::new();

        // -- Exec
        store.save_provider("GitHub")?;

        // -- Check
        assert_eq!(store.load_provider()?, Some("GitHub".to_string()));
        Ok(())
    }

    #[test]
    fn test_mock_token_store_provider_empty() -> Result<()> {
        // -- Setup
        let store = MockTokenStore::new();

        // -- Check
        assert_eq!(store.load_provider()?, None);
        Ok(())
    }

    #[test]
    fn test_mock_token_store_delete_clears_provider() -> Result<()> {
        // -- Setup
        let store = MockTokenStore::new();
        store.save_token(&make_token())?;
        store.save_provider("Google")?;

        // -- Exec
        store.delete_token()?;

        // -- Check
        assert_eq!(store.load_token()?, None);
        assert_eq!(store.load_provider()?, None);
        Ok(())
    }

    #[test]
    fn test_mock_token_store_overwrite() -> Result<()> {
        // -- Setup
        let store = MockTokenStore::new();
        store.save_token(&make_token())?;

        let new_token = AuthToken {
            access_token: "new_access".into(),
            refresh_token: None,
            expires_at: None,
        };

        // -- Exec
        store.save_token(&new_token)?;

        // -- Check
        assert_eq!(store.load_token()?, Some(new_token));
        Ok(())
    }

    #[test]
    fn test_stored_token_roundtrip() -> Result<()> {
        // -- Setup
        let token = make_token();

        // -- Exec
        let stored = StoredToken::from(&token);
        let restored = AuthToken::from(stored);

        // -- Check
        assert_eq!(restored, token);
        Ok(())
    }
}
