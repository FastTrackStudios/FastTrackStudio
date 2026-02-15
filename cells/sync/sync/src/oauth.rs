//! OAuth2 client configuration for GitHub and Google providers
//!
//! Uses the `oauth2` v5 crate with PKCE for secure authorization code exchange.

use oauth2::basic::BasicClient;
use oauth2::{
    AuthUrl, ClientId, ClientSecret, CsrfToken, EndpointNotSet, EndpointSet, PkceCodeChallenge,
    PkceCodeVerifier, RedirectUrl, Scope, TokenResponse, TokenUrl,
};
use sync_proto::AuthProvider;

use crate::error::{AuthError, AuthResult};

/// OAuth2 configuration for a provider
pub struct OAuthConfig {
    pub client_id: String,
    pub client_secret: Option<String>,
    pub redirect_uri: String,
}

/// Result of initiating an OAuth2 login
pub struct AuthorizationRequest {
    /// URL the user should open in their browser
    pub auth_url: String,
    /// CSRF state token for callback validation
    pub csrf_state: String,
    /// PKCE verifier — must be kept until the callback
    pub pkce_verifier: PkceCodeVerifier,
}

/// Configured OAuth2 client with auth + token endpoints set.
///
/// oauth2 v5 uses typestate generics to track which endpoints are configured.
/// This alias captures the exact configuration produced by `build_client`:
/// - `EndpointSet` for AuthUrl (position 1) and TokenUrl (position 5)
/// - `EndpointNotSet` for DeviceAuth, Introspection, Revocation (positions 2-4)
pub type ConfiguredClient =
    BasicClient<EndpointSet, EndpointNotSet, EndpointNotSet, EndpointNotSet, EndpointSet>;

/// Build an OAuth2 client for the given provider and config
pub fn build_client(provider: AuthProvider, config: &OAuthConfig) -> AuthResult<ConfiguredClient> {
    let (auth_url, token_url) = match provider {
        AuthProvider::GitHub => (
            "https://github.com/login/oauth/authorize",
            "https://github.com/login/oauth/access_token",
        ),
        AuthProvider::Google => (
            "https://accounts.google.com/o/oauth2/v2/auth",
            "https://oauth2.googleapis.com/token",
        ),
    };

    let mut client = BasicClient::new(ClientId::new(config.client_id.clone()))
        .set_auth_uri(
            AuthUrl::new(auth_url.to_string()).map_err(|e| AuthError::OAuth(e.to_string()))?,
        )
        .set_token_uri(
            TokenUrl::new(token_url.to_string()).map_err(|e| AuthError::OAuth(e.to_string()))?,
        )
        .set_redirect_uri(
            RedirectUrl::new(config.redirect_uri.clone())
                .map_err(|e| AuthError::OAuth(e.to_string()))?,
        );

    if let Some(ref secret) = config.client_secret {
        client = client.set_client_secret(ClientSecret::new(secret.clone()));
    }

    Ok(client)
}

/// Generate an authorization URL with PKCE challenge
pub fn authorize_url(client: &ConfiguredClient, provider: AuthProvider) -> AuthorizationRequest {
    let (pkce_challenge, pkce_verifier) = PkceCodeChallenge::new_random_sha256();

    let scopes = match provider {
        AuthProvider::GitHub => vec!["user:email", "read:user"],
        AuthProvider::Google => vec!["openid", "email", "profile"],
    };

    let mut builder = client
        .authorize_url(CsrfToken::new_random)
        .set_pkce_challenge(pkce_challenge);

    for scope in scopes {
        builder = builder.add_scope(Scope::new(scope.to_string()));
    }

    let (auth_url, csrf_state) = builder.url();

    AuthorizationRequest {
        auth_url: auth_url.to_string(),
        csrf_state: csrf_state.secret().clone(),
        pkce_verifier,
    }
}

/// Exchange an authorization code for tokens
pub async fn exchange_code(
    client: &ConfiguredClient,
    code: &str,
    pkce_verifier: PkceCodeVerifier,
) -> AuthResult<sync_proto::AuthToken> {
    let http_client = reqwest::Client::new();

    let token_result = client
        .exchange_code(oauth2::AuthorizationCode::new(code.to_string()))
        .set_pkce_verifier(pkce_verifier)
        .request_async(&http_client)
        .await
        .map_err(|e| AuthError::OAuth(e.to_string()))?;

    let expires_at = token_result
        .expires_in()
        .map(|d: std::time::Duration| chrono::Utc::now().timestamp() + d.as_secs() as i64);

    Ok(sync_proto::AuthToken {
        access_token: token_result.access_token().secret().clone(),
        refresh_token: token_result
            .refresh_token()
            .map(|t: &oauth2::RefreshToken| t.secret().clone()),
        expires_at,
    })
}

/// Refresh an access token using a refresh token
pub async fn refresh_token(
    client: &ConfiguredClient,
    refresh_token_str: &str,
) -> AuthResult<sync_proto::AuthToken> {
    let http_client = reqwest::Client::new();

    let token_result = client
        .exchange_refresh_token(&oauth2::RefreshToken::new(refresh_token_str.to_string()))
        .request_async(&http_client)
        .await
        .map_err(|e| AuthError::OAuth(e.to_string()))?;

    let expires_at = token_result
        .expires_in()
        .map(|d: std::time::Duration| chrono::Utc::now().timestamp() + d.as_secs() as i64);

    Ok(sync_proto::AuthToken {
        access_token: token_result.access_token().secret().clone(),
        refresh_token: token_result
            .refresh_token()
            .map(|t: &oauth2::RefreshToken| t.secret().clone()),
        expires_at,
    })
}

/// Fetch user profile from the OAuth provider using the access token
pub async fn fetch_user_profile(
    provider: AuthProvider,
    access_token: &str,
) -> AuthResult<sync_proto::User> {
    let http_client = reqwest::Client::new();

    match provider {
        AuthProvider::GitHub => fetch_github_user(&http_client, access_token).await,
        AuthProvider::Google => fetch_google_user(&http_client, access_token).await,
    }
}

async fn fetch_github_user(
    client: &reqwest::Client,
    access_token: &str,
) -> AuthResult<sync_proto::User> {
    let resp: serde_json::Value = client
        .get("https://api.github.com/user")
        .header("Authorization", format!("Bearer {access_token}"))
        .header("User-Agent", "FastTrackStudio-Sync")
        .send()
        .await?
        .json()
        .await?;

    let email = resp["email"].as_str().unwrap_or("").to_string();
    let display_name = resp["name"]
        .as_str()
        .or_else(|| resp["login"].as_str())
        .unwrap_or("Unknown")
        .to_string();

    Ok(sync_proto::User::new(
        uuid::Uuid::new_v4(),
        email,
        display_name,
    ))
}

async fn fetch_google_user(
    client: &reqwest::Client,
    access_token: &str,
) -> AuthResult<sync_proto::User> {
    let resp: serde_json::Value = client
        .get("https://www.googleapis.com/oauth2/v2/userinfo")
        .header("Authorization", format!("Bearer {access_token}"))
        .send()
        .await?
        .json()
        .await?;

    let email = resp["email"].as_str().unwrap_or("").to_string();
    let display_name = resp["name"].as_str().unwrap_or("Unknown").to_string();

    Ok(sync_proto::User::new(
        uuid::Uuid::new_v4(),
        email,
        display_name,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use sync_proto::AuthProvider;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    fn test_config() -> OAuthConfig {
        OAuthConfig {
            client_id: "test-client-id".into(),
            client_secret: Some("test-secret".into()),
            redirect_uri: "http://localhost:8080/callback".into(),
        }
    }

    #[test]
    fn test_oauth_build_client_github() -> Result<()> {
        // -- Exec
        let client = build_client(AuthProvider::GitHub, &test_config())?;

        // -- Check: client was created (methods are available due to EndpointSet)
        let _auth_url = client.authorize_url(CsrfToken::new_random);
        Ok(())
    }

    #[test]
    fn test_oauth_build_client_google() -> Result<()> {
        // -- Exec
        let client = build_client(AuthProvider::Google, &test_config())?;

        // -- Check
        let _auth_url = client.authorize_url(CsrfToken::new_random);
        Ok(())
    }

    #[test]
    fn test_oauth_build_client_without_secret() -> Result<()> {
        // -- Setup
        let config = OAuthConfig {
            client_id: "public-client".into(),
            client_secret: None,
            redirect_uri: "http://localhost:8080/callback".into(),
        };

        // -- Exec & Check: no panic
        let _client = build_client(AuthProvider::GitHub, &config)?;
        Ok(())
    }

    #[test]
    fn test_oauth_build_client_invalid_redirect_uri() {
        // -- Setup
        let config = OAuthConfig {
            client_id: "test".into(),
            client_secret: None,
            redirect_uri: "not a valid url".into(),
        };

        // -- Exec
        let result = build_client(AuthProvider::GitHub, &config);

        // -- Check
        assert!(result.is_err());
    }

    #[test]
    fn test_oauth_authorize_url_github_scopes() -> Result<()> {
        // -- Setup
        let client = build_client(AuthProvider::GitHub, &test_config())?;

        // -- Exec
        let request = authorize_url(&client, AuthProvider::GitHub);

        // -- Check
        assert!(request.auth_url.contains("github.com"));
        assert!(request.auth_url.contains("user%3Aemail")); // URL-encoded "user:email"
        assert!(!request.csrf_state.is_empty());
        Ok(())
    }

    #[test]
    fn test_oauth_authorize_url_google_scopes() -> Result<()> {
        // -- Setup
        let client = build_client(AuthProvider::Google, &test_config())?;

        // -- Exec
        let request = authorize_url(&client, AuthProvider::Google);

        // -- Check
        assert!(request.auth_url.contains("accounts.google.com"));
        assert!(request.auth_url.contains("openid"));
        assert!(!request.csrf_state.is_empty());
        Ok(())
    }

    #[test]
    fn test_oauth_authorize_url_unique_csrf() -> Result<()> {
        // -- Setup
        let client = build_client(AuthProvider::GitHub, &test_config())?;

        // -- Exec
        let req1 = authorize_url(&client, AuthProvider::GitHub);
        let req2 = authorize_url(&client, AuthProvider::GitHub);

        // -- Check: each request gets a unique CSRF token
        assert_ne!(req1.csrf_state, req2.csrf_state);
        Ok(())
    }
}
