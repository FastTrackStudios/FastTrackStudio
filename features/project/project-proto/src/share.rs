//! `ShareService` — admin-side surface for project share links.
//!
//! Phase 4 of `plans/decentralized-foundation.md`. Lets a project
//! admin mint a capability token scoped to a single project + ttl,
//! list active tokens, and revoke them. Revocation propagates into
//! the capability middleware: a revoked token's next sub/apply
//! returns `Forbidden`.
//!
//! Anonymous redemption goes through the existing `?cap=<token>`
//! URL — no special endpoint. The token's signed scope is what
//! grants access.

use facet::Facet;
use thiserror::Error;
use uuid::Uuid;

/// Scope of access a share link grants. Subset of the more general
/// capability shape — share links are always single-project,
/// always anonymous, sometimes RW.
#[derive(Debug, Clone, Facet)]
pub struct ShareLinkScope {
    /// Doc id the share link grants access to. Conventionally
    /// `project/<uuid>` but free-form to support whole-vault
    /// invitations later.
    pub doc_id: String,
    pub can_write: bool,
    /// TTL in seconds from `now`. The server converts to an
    /// absolute `expires_unix` at issue time.
    pub ttl_seconds: i64,
    /// Optional human label ("Acme review", "client onboarding").
    pub label: Option<String>,
}

/// Result of creating a share link.
#[derive(Debug, Clone, Facet)]
pub struct ShareLinkCreated {
    pub token_id: Uuid,
    pub token: String,
    pub expires_unix: i64,
}

/// One row of `ShareService::list`.
#[derive(Debug, Clone, Facet)]
pub struct ShareLinkMeta {
    pub token_id: Uuid,
    pub doc_id: String,
    pub can_write: bool,
    pub expires_unix: i64,
    pub label: Option<String>,
    pub revoked: bool,
}

/// Listing response — wrapped in a struct so vox's wire codec has
/// a named type to bind to.
#[derive(Debug, Clone, Facet)]
pub struct ShareLinkList {
    pub items: Vec<ShareLinkMeta>,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for ShareLinkScope {
    type Ref<'a> = ShareLinkScope;
}
#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for ShareLinkCreated {
    type Ref<'a> = ShareLinkCreated;
}
#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for ShareLinkMeta {
    type Ref<'a> = ShareLinkMeta;
}
#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for ShareLinkList {
    type Ref<'a> = ShareLinkList;
}

#[derive(Debug, Clone, PartialEq, Eq, Facet, Error)]
#[repr(u8)]
pub enum ShareError {
    #[error("forbidden")]
    Forbidden,
    #[error("not found")]
    NotFound,
    #[error("invalid scope: {0}")]
    InvalidScope(String),
    #[error("internal: {0}")]
    Internal(String),
}

/// Token id wrapper — vox method args need a single typed value.
#[derive(Debug, Clone, Facet)]
pub struct TokenIdArg {
    pub token_id: Uuid,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for TokenIdArg {
    type Ref<'a> = TokenIdArg;
}

/// Doc id wrapper for `list`.
#[derive(Debug, Clone, Facet)]
pub struct DocIdArg {
    pub doc_id: String,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for DocIdArg {
    type Ref<'a> = DocIdArg;
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait ShareService {
    /// Mint a new share-link token. Returns the encoded token
    /// string for inclusion in `?cap=...`.
    async fn create(&self, scope: ShareLinkScope) -> Result<ShareLinkCreated, ShareError>;

    /// List share links for a given doc.
    async fn list(&self, doc: DocIdArg) -> Result<ShareLinkList, ShareError>;

    /// Revoke a share link. Idempotent — revoking an already-revoked
    /// token is a no-op; revoking an unknown token returns `NotFound`.
    async fn revoke(&self, arg: TokenIdArg) -> Result<(), ShareError>;
}
