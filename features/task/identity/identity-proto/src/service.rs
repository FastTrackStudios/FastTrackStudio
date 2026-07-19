//! Identity-locker RPC.
//!
//! [`IdentityService`] runs at the server-level `/server/vox`
//! endpoint (one endpoint per task-server process, not per-org).
//! It exposes the home org's identity locker — the per-user set
//! of [`crate::LinkedServer`] rows holding encrypted session
//! tokens for other servers the user has linked.
//!
//! ## Authorization
//!
//! Every method takes a `session_token` that must validate
//! against the **home** org's `auth.sqlite`. The authenticated
//! user's id is the implicit `home_user_id` for every operation
//! — a caller can only ever see and mutate their own links.
//!
//! See `plans/federated-task-platform.md`.

use facet::Facet;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use uuid::Uuid;

/// Trait-boundary error type. Variants stay flat so the same
/// enum travels cleanly over vox. Mirrors
/// `org_proto::OrgManagementError`'s derive set.
#[derive(Debug, Clone, PartialEq, Eq, Facet, Error)]
#[repr(u8)]
pub enum IdentityServiceError {
    /// Caller's session token isn't valid against the home org's
    /// `auth.sqlite`, or the home org has no identity locker.
    #[error("unauthorized: {0}")]
    Unauthorized(String),
    /// The referenced link row doesn't exist (or isn't owned by
    /// the caller).
    #[error("not found: {0}")]
    NotFound(String),
    /// Store / DB failure on the server side. Strings the source
    /// so callers don't pull in the server's error types.
    #[error("internal: {0}")]
    Internal(String),
}

/// The decrypted view of one linked-server row, returned to its
/// authenticated owner. No `home_user_id` — it's implicit (the
/// caller). `token` is the plaintext session token; `None` when
/// the row was stored without one.
#[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize)]
#[repr(C)]
pub struct LinkView {
    pub id: Uuid,
    pub label: String,
    pub remote_url: String,
    pub remote_slug: String,
    pub remote_user_id: Option<Uuid>,
    pub remote_email: Option<String>,
    pub token: Option<String>,
    pub expires_at: Option<i64>,
}

/// Create-or-update a linked server for the authenticated user.
/// Keyed server-side on `(home_user_id, remote_url, remote_slug)`
/// — repeating a link updates it in place. `token` is the
/// plaintext session token to store (encrypted at rest); `None`
/// leaves any existing token untouched.
#[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize)]
#[repr(C)]
pub struct LinkServerRequest {
    /// Session token from the home org — authenticates the caller.
    pub session_token: String,
    pub label: String,
    pub remote_url: String,
    pub remote_slug: String,
    pub remote_user_id: Option<Uuid>,
    pub remote_email: Option<String>,
    pub token: Option<String>,
    pub expires_at: Option<i64>,
}

/// Identity-locker surface. Mounted at `/server/vox`.
#[architect::rpc]
pub trait IdentityService {
    /// Every linked server owned by the authenticated caller,
    /// tokens decrypted.
    fn list_links(&self, session_token: String) -> Result<Vec<LinkView>, IdentityServiceError>;

    /// Link (or re-link) a remote server for the authenticated
    /// caller; returns the stored row.
    fn link_server(&self, req: LinkServerRequest) -> Result<LinkView, IdentityServiceError>;

    /// Remove a linked server by id, scoped to the authenticated
    /// caller so a user can't delete another user's link.
    fn unlink_server(
        &self,
        session_token: String,
        id: Uuid,
    ) -> Result<(), IdentityServiceError>;
}
