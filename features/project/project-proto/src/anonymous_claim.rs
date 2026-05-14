//! `AnonymousClaim` — Phase 8 service for claiming share-link
//! authored edits.
//!
//! Every share-link token issued by Phase 4's `ShareService`
//! carries a stable `peer_id = "share-link-<token_id>"`. When the
//! anonymous user later signs in (via architect-auth), they can
//! claim the edits made under that peer id. The server records
//! `(peer_id, user_id, claimed_at)` so future renders can
//! substitute the friendly name for the share-link peer.
//!
//! Loro's history is immutable — we don't rewrite the peer in the
//! doc. The claim is metadata applied at render time. First-claim-
//! wins; subsequent claimers are told the session is already
//! attributed.

use facet::Facet;
use thiserror::Error;
use uuid::Uuid;

#[derive(Debug, Clone, Facet)]
pub struct ClaimRequest {
    /// The `token_id` of the share link whose edits should be
    /// claimed. The client knows this from the
    /// `ShareLinkCreated.token_id` it received earlier, or by
    /// decoding its own stored capability scope.
    pub token_id: Uuid,
}

#[derive(Debug, Clone, Facet)]
pub struct ClaimSummary {
    /// `share-link-<token_id>` — what appears in the doc's
    /// history.
    pub peer_id: String,
    /// The authenticated user the peer now resolves to.
    pub user_id: Uuid,
    pub claimed_at_unix: i64,
    /// True iff this call was the first to claim. False = already
    /// claimed by some other user. The summary still returns the
    /// original claimer's user id.
    pub first_claim: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Facet, Error)]
#[repr(u8)]
pub enum ClaimError {
    #[error("forbidden")]
    Forbidden,
    #[error("not signed in")]
    NotSignedIn,
    #[error("unknown token")]
    UnknownToken,
    #[error("internal: {0}")]
    Internal(String),
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for ClaimRequest {
    type Ref<'a> = ClaimRequest;
}
#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for ClaimSummary {
    type Ref<'a> = ClaimSummary;
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait AnonymousClaim {
    async fn claim_anonymous_session(&self, req: ClaimRequest) -> Result<ClaimSummary, ClaimError>;
}
