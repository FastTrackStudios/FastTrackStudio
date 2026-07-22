//! Wire contract for the `share` feature — sharing a group of notes via
//! tracked, individually-configurable links (Samply-style: nothing is
//! fire-and-forget; every link can be edited, disabled, or deleted after
//! creation and the change is retroactive).
//!
//! Dev-preview surface (collaboration-sharing.md S2 slice): link CRUD on
//! the ORG lane. The scoped guest lane (`/org/{slug}/share/{token}/vox`)
//! and named email grants arrive next; capabilities are already carried on
//! every link so the landing page can honor them.

/// One share link, as the panel and the Links registry render it.
#[derive(Clone, Debug, PartialEq, ::facet::Facet)]
pub struct ShareLinkInfo {
    /// The unguessable URL token (also the link's id).
    pub token: String,
    /// Human label ("band link", "orchestra desk").
    pub label: String,
    /// The note the share was created from.
    pub note_path: String,
    /// `view` | `comment` (edit is invite-only, never link-based).
    pub capability: String,
    /// Reversible off-switch — a disabled link 410s without being deleted.
    pub disabled: bool,
    /// Absolute URL to hand out (server composes it from its public base).
    pub url: String,
    /// RFC3339 creation stamp.
    pub created_at: String,
}

#[derive(Clone, Debug, PartialEq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum ShareError {
    #[error("share not found")]
    NotFound,
    #[error("share storage error: {0}")]
    Storage(String),
    #[error("invalid request: {0}")]
    Invalid(String),
}

#[architect::rpc]
pub trait ShareService {
    /// Mint a new link for `note_path`. `capability` is `view` or
    /// `comment`.
    async fn create_link(
        &self,
        note_path: String,
        label: String,
        capability: String,
    ) -> Result<ShareLinkInfo, ShareError>;

    /// Every link in the org (the Links registry), newest first.
    async fn list_links(&self) -> Result<Vec<ShareLinkInfo>, ShareError>;

    /// Links for ONE note (the note's Share panel).
    async fn links_for_note(&self, note_path: String) -> Result<Vec<ShareLinkInfo>, ShareError>;

    /// Disable (reversible) or re-enable a link — retroactive: a disabled
    /// link stops resolving immediately.
    async fn set_link_disabled(&self, token: String, disabled: bool) -> Result<(), ShareError>;

    /// Delete a link permanently.
    async fn delete_link(&self, token: String) -> Result<(), ShareError>;
}
