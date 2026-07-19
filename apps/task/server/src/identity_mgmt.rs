//! `IdentityService` server-side impl.
//!
//! Mounted at `/server/vox` (one endpoint per task-server
//! process, not per-org). Exposes the **home** org's identity
//! locker — the per-user set of `LinkedServer` rows holding
//! encrypted session tokens for other servers the user has
//! linked.
//!
//! ## Authorization
//!
//! Every method requires a `session_token` that validates
//! against the home org's `auth.sqlite` via
//! `ArchitectAuth::current_session`; the authenticated user's id
//! is the implicit `home_user_id` for the call. Unlike
//! [`crate::server_mgmt::OrgManagementImpl`], `local_trusted`
//! does **not** bypass the session check here: the store is
//! per-user keyed, so we always need a real `home_user_id` and
//! never forge one. `local_trusted` is retained only for
//! constructor symmetry with the other `/server/vox` services.

use std::sync::Arc;

use identity::{LinkRecord, Store};
use identity_proto::{IdentityService, IdentityServiceError, LinkServerRequest, LinkView};
use uuid::Uuid;

use crate::AppState;

/// Backend serving the home org's identity locker against a live
/// [`AppState`]. Holds an `Arc<AppState>` so it reads the same
/// orgs map the request handlers do.
#[derive(Clone, architect::HasDispatcher)]
pub struct IdentityServiceImpl {
    state: Arc<AppState>,
    /// Retained for constructor symmetry with the other
    /// `/server/vox` services. Does not relax auth — the locker is
    /// per-user, so a real `home_user_id` is always required (see
    /// module docs).
    #[allow(dead_code)]
    local_trusted: bool,
}

impl IdentityServiceImpl {
    #[must_use]
    pub fn new(state: AppState) -> Self {
        Self {
            state: Arc::new(state),
            local_trusted: false,
        }
    }

    /// In-process transport constructor. Note: unlike the org-mgmt
    /// service, this still validates the session token (see module
    /// docs) — a per-user locker can't run without a user id.
    #[must_use]
    pub fn new_local_trusted(state: AppState) -> Self {
        Self {
            state: Arc::new(state),
            local_trusted: true,
        }
    }

    /// Validate `session_token` against the home org's auth DB and
    /// return `(home identity store, home_user_id)`.
    fn resolve(&self, session_token: &str) -> Result<(Store, Uuid), IdentityServiceError> {
        let home_slug = self.state.home_slug().ok_or_else(|| {
            IdentityServiceError::Unauthorized("server has no home org".into())
        })?;
        if session_token.is_empty() {
            return Err(IdentityServiceError::Unauthorized(
                "missing session token".into(),
            ));
        }
        let home = self.state.org(&home_slug).ok_or_else(|| {
            IdentityServiceError::Unauthorized(format!(
                "home org `{home_slug}` not in live dispatcher"
            ))
        })?;
        let token = session_token.to_owned();
        let bundle = tokio::runtime::Handle::current()
            .block_on(async move {
                home.auth
                    .auth
                    .current_session(architect_auth::commands::CurrentSession { token })
                    .await
            })
            .map_err(|e| {
                IdentityServiceError::Unauthorized(format!("invalid session token: {e}"))
            })?;
        let home_user_id = bundle.user.id;

        let store = self
            .state
            .org(&home_slug)
            .and_then(|o| o.identity)
            .ok_or_else(|| {
                IdentityServiceError::Internal(
                    "home org has no identity locker".into(),
                )
            })?;
        Ok((store, home_user_id))
    }
}

fn record_to_view(rec: LinkRecord) -> LinkView {
    LinkView {
        id: rec.id,
        label: rec.label,
        remote_url: rec.remote_url,
        remote_slug: rec.remote_slug,
        remote_user_id: rec.remote_user_id,
        remote_email: rec.remote_email,
        token: rec.token,
        expires_at: rec.expires_at,
    }
}

impl IdentityService for IdentityServiceImpl {
    fn list_links(&self, session_token: String) -> Result<Vec<LinkView>, IdentityServiceError> {
        let (store, home_user_id) = self.resolve(&session_token)?;
        let rows = tokio::runtime::Handle::current()
            .block_on(async move { store.list_links(home_user_id).await })
            .map_err(|e| IdentityServiceError::Internal(e.to_string()))?;
        Ok(rows.into_iter().map(record_to_view).collect())
    }

    fn link_server(&self, req: LinkServerRequest) -> Result<LinkView, IdentityServiceError> {
        let (store, home_user_id) = self.resolve(&req.session_token)?;
        let rec = LinkRecord {
            id: Uuid::nil(),
            home_user_id,
            label: req.label,
            remote_url: req.remote_url,
            remote_slug: req.remote_slug,
            remote_user_id: req.remote_user_id,
            remote_email: req.remote_email,
            token: req.token,
            expires_at: req.expires_at,
        };
        let stored = tokio::runtime::Handle::current()
            .block_on(async move { store.upsert_link(rec).await })
            .map_err(|e| IdentityServiceError::Internal(e.to_string()))?;
        Ok(record_to_view(stored))
    }

    fn unlink_server(
        &self,
        session_token: String,
        id: Uuid,
    ) -> Result<(), IdentityServiceError> {
        let (store, home_user_id) = self.resolve(&session_token)?;
        tokio::runtime::Handle::current()
            .block_on(async move { store.delete_link(home_user_id, id).await })
            .map_err(|e| IdentityServiceError::Internal(e.to_string()))
    }
}
