//! Auth live implementation — delegates to better-auth directly

use super::proto::*;
use better_auth::adapters::MemoryDatabaseAdapter;
use better_auth::plugins::{EmailPasswordPlugin, SessionManagementPlugin};
use better_auth::{AuthBuilder, AuthConfig, BetterAuth};
use better_auth_core::types::{AuthRequest, HttpMethod};
use std::collections::HashMap;
use std::sync::Arc;

pub type Auth = BetterAuth<MemoryDatabaseAdapter>;

#[derive(Clone)]
pub struct AuthServiceLive {
    auth: Arc<Auth>,
}

impl AuthServiceLive {
    pub async fn new() -> eyre::Result<Self> {
        let config = AuthConfig::new("playground-secret-key-must-be-at-least-32-chars-long!")
            .base_url("http://localhost")
            .password_min_length(6);

        let auth = AuthBuilder::new(config)
            .database(MemoryDatabaseAdapter::new())
            .plugin(EmailPasswordPlugin::new().enable_signup(true))
            .plugin(SessionManagementPlugin::new())
            .build()
            .await
            .map_err(|e| eyre::eyre!("Failed to build auth: {e:?}"))?;

        let auth = Arc::new(auth);

        // Seed demo user
        let resp = auth
            .handle_request(Self::make_request(
                HttpMethod::Post,
                "/sign-up/email",
                Some(serde_json::json!({
                    "email": "demo@example.com",
                    "password": "demo123",
                    "name": "Demo User",
                })),
            ))
            .await;

        match resp {
            Ok(r) if r.status == 200 => {
                tracing::info!("Seeded demo user: demo@example.com / demo123")
            }
            Ok(r) => tracing::warn!("Failed to seed demo user (status {})", r.status),
            Err(e) => tracing::warn!("Failed to seed demo user: {e:?}"),
        }

        Ok(Self { auth })
    }

    /// Expose auth instance for session validation by other services.
    pub fn auth_ref(&self) -> Arc<Auth> {
        self.auth.clone()
    }

    fn make_request(
        method: HttpMethod,
        path: &str,
        body: Option<serde_json::Value>,
    ) -> AuthRequest {
        AuthRequest {
            method,
            path: path.to_string(),
            headers: HashMap::from([("content-type".to_string(), "application/json".to_string())]),
            body: body.map(|b| serde_json::to_vec(&b).unwrap()),
            query: HashMap::new(),
        }
    }

    fn bearer_request(method: HttpMethod, path: &str, token: &str) -> AuthRequest {
        AuthRequest {
            method,
            path: path.to_string(),
            headers: HashMap::from([("authorization".to_string(), format!("Bearer {token}"))]),
            body: None,
            query: HashMap::new(),
        }
    }

    fn parse_session(body: &[u8]) -> Option<SessionToken> {
        let v: serde_json::Value = serde_json::from_slice(body).ok()?;
        Some(SessionToken {
            token: v["session"]["token"]
                .as_str()
                .or(v["token"].as_str())?
                .to_string(),
            user_id: v["user"]["id"].as_str()?.to_string(),
            user_email: v["user"]["email"].as_str().unwrap_or_default().to_string(),
            user_name: v["user"]["name"].as_str().unwrap_or_default().to_string(),
        })
    }
}

impl AuthService for AuthServiceLive {
    async fn sign_up(
        &self,
        ,
        email: String,
        password: String,
        name: String,
    ) -> AuthResult {
        let req = Self::make_request(
            HttpMethod::Post,
            "/sign-up/email",
            Some(serde_json::json!({ "email": email, "password": password, "name": name })),
        );
        match self.auth.handle_request(req).await {
            Ok(resp) if resp.status == 200 => match Self::parse_session(&resp.body) {
                Some(session) => AuthResult::Success { session },
                None => AuthResult::Failed {
                    message: "Failed to parse auth response".into(),
                },
            },
            Ok(resp) => {
                let msg = serde_json::from_slice::<serde_json::Value>(&resp.body)
                    .ok()
                    .and_then(|v| v["message"].as_str().map(String::from))
                    .unwrap_or_else(|| format!("Sign up failed (status {})", resp.status));
                AuthResult::Failed { message: msg }
            }
            Err(e) => AuthResult::Failed {
                message: format!("{e:?}"),
            },
        }
    }

    async fn sign_in(&self, _email: String, password: String) -> AuthResult {
        let req = Self::make_request(
            HttpMethod::Post,
            "/sign-in/email",
            Some(serde_json::json!({ "email": email, "password": password })),
        );
        match self.auth.handle_request(req).await {
            Ok(resp) if resp.status == 200 => match Self::parse_session(&resp.body) {
                Some(session) => AuthResult::Success { session },
                None => AuthResult::Failed {
                    message: "Failed to parse auth response".into(),
                },
            },
            Ok(resp) => {
                let msg = serde_json::from_slice::<serde_json::Value>(&resp.body)
                    .ok()
                    .and_then(|v| v["message"].as_str().map(String::from))
                    .unwrap_or_else(|| format!("Sign in failed (status {})", resp.status));
                AuthResult::Failed { message: msg }
            }
            Err(e) => AuthResult::Failed {
                message: format!("{e:?}"),
            },
        }
    }

    async fn validate_session(&self, _token: String) -> Option<SessionToken> {
        let req = Self::bearer_request(HttpMethod::Get, "/get-session", &token);
        match self.auth.handle_request(req).await {
            Ok(resp) if resp.status == 200 => {
                let v: serde_json::Value = serde_json::from_slice(&resp.body).ok()?;
                Some(SessionToken {
                    token,
                    user_id: v["user"]["id"].as_str()?.to_string(),
                    user_email: v["user"]["email"].as_str().unwrap_or_default().to_string(),
                    user_name: v["user"]["name"].as_str().unwrap_or_default().to_string(),
                })
            }
            _ => None,
        }
    }

    async fn sign_out(&self, _token: String) {
        let req = Self::bearer_request(HttpMethod::Post, "/sign-out", &token);
        let _ = self.auth.handle_request(req).await;
    }
}
