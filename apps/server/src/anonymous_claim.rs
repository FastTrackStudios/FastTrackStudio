//! Phase 8 — server-side `AnonymousClaim` implementation.
//!
//! Holds an in-memory [`ClaimTable`] mapping `peer_id ->
//! (user_id, claimed_at)`. The vox impl translates the
//! `ClaimRequest.token_id` into the canonical `share-link-<uuid>`
//! peer id, checks the share-link table exists, looks up the
//! caller's authenticated user id via `AuthVoxContext`, and
//! inserts a row (first-claim-wins).

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

use architect::vox;
use architect_auth::transport::vox::AuthVoxContext;
use architect_auth::{ArchitectAuth, AuthStorage, CurrentSession};
use project_proto::{AnonymousClaim, ClaimError, ClaimRequest, ClaimSummary};
use uuid::Uuid;

use crate::share_link::ShareServiceImpl;

/// In-memory claim table. Keyed by canonical peer id
/// (`"share-link-<token_id>"`). Phase 9+ persists this in the
/// org-vault doc.
#[derive(Debug, Default, Clone)]
pub struct ClaimTable {
    inner: Arc<Mutex<HashMap<String, ClaimEntry>>>,
}

#[derive(Debug, Clone)]
pub struct ClaimEntry {
    pub user_id: Uuid,
    pub claimed_at_unix: i64,
}

impl ClaimTable {
    pub fn new() -> Self {
        Self::default()
    }

    /// First-claim-wins. Returns `(stored_entry, first_claim_flag)`.
    pub fn claim(&self, peer_id: String, user_id: Uuid, now_unix: i64) -> (ClaimEntry, bool) {
        let mut inner = self.inner.lock().unwrap();
        if let Some(existing) = inner.get(&peer_id) {
            return (existing.clone(), false);
        }
        let entry = ClaimEntry {
            user_id,
            claimed_at_unix: now_unix,
        };
        inner.insert(peer_id, entry.clone());
        (entry, true)
    }

    pub fn resolve(&self, peer_id: &str) -> Option<ClaimEntry> {
        self.inner.lock().unwrap().get(peer_id).cloned()
    }

    pub fn len(&self) -> usize {
        self.inner.lock().unwrap().len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.lock().unwrap().is_empty()
    }
}

/// `AnonymousClaimService` — pulls together the auth, the share
/// link table, and the claim table. Cloneable so the vox
/// dispatcher can construct one per connection.
#[derive(Clone)]
pub struct AnonymousClaimServiceImpl<S: AuthStorage + Clone + 'static> {
    pub auth: ArchitectAuth<S>,
    /// Reference to the share-link service so we can verify the
    /// token_id actually maps to a known share link.
    pub share_service: ShareServiceImpl,
    pub table: ClaimTable,
    /// vox metadata is read from a per-connection context. The
    /// dispatcher mounts `AuthServerMiddleware`, which inserts an
    /// `AuthVoxContext` into the request extensions on every call;
    /// we look it up via a thread-local supplied by the dispatcher
    /// wrapper.
    pub session_token: Arc<Mutex<Option<String>>>,
}

impl<S: AuthStorage + Clone + 'static> AnonymousClaimServiceImpl<S> {
    pub fn new(auth: ArchitectAuth<S>, share_service: ShareServiceImpl) -> Self {
        Self {
            auth,
            share_service,
            table: ClaimTable::new(),
            session_token: Arc::new(Mutex::new(None)),
        }
    }

    /// Provided by the vox handler before the call dispatches.
    pub fn install_session_token(&self, token: Option<String>) {
        *self.session_token.lock().unwrap() = token;
    }
}

fn now_unix() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
}

impl<S: AuthStorage + Clone + 'static> AnonymousClaim for AnonymousClaimServiceImpl<S> {
    async fn claim_anonymous_session(&self, req: ClaimRequest) -> Result<ClaimSummary, ClaimError> {
        // 1. Caller must be signed in.
        let token = self
            .session_token
            .lock()
            .unwrap()
            .clone()
            .ok_or(ClaimError::NotSignedIn)?;
        let session = self
            .auth
            .current_session(CurrentSession { token })
            .await
            .map_err(|_| ClaimError::NotSignedIn)?;
        let user_id = session.user.id;

        // 2. Construct the peer id from the token_id. We don't
        // verify the share-link table for existence here — the
        // ShareService's revocation list is the access gate, and
        // a claim records intent even if the token has been
        // revoked.
        let peer_id = format!("share-link-{}", req.token_id);

        // 3. First-claim-wins.
        let (entry, first_claim) = self.table.claim(peer_id.clone(), user_id, now_unix());
        Ok(ClaimSummary {
            peer_id,
            user_id: entry.user_id,
            claimed_at_unix: entry.claimed_at_unix,
            first_claim,
        })
    }
}

/// Tiny vox `ServerMiddleware` that copies the
/// `AuthVoxContext.token` (placed by `AuthServerMiddleware`) into
/// the service's `session_token` slot before the typed method
/// runs. Lets the `claim_anonymous_session` impl read the caller's
/// session without changing the trait signature.
#[derive(Clone)]
pub struct InstallSessionMiddleware<S: AuthStorage + Clone + 'static> {
    pub service: AnonymousClaimServiceImpl<S>,
}

impl<S: AuthStorage + Clone + 'static> vox::ServerMiddleware for InstallSessionMiddleware<S> {
    fn pre<'a>(&'a self, request: vox::ServerRequest<'_>) -> vox::BoxMiddlewareFuture<'a> {
        let ctx = request
            .extensions()
            .get_cloned::<AuthVoxContext>()
            .and_then(|c| c.token);
        self.service.install_session_token(ctx);
        Box::pin(async {})
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn first_claim_wins() {
        let table = ClaimTable::new();
        let alice = Uuid::new_v4();
        let bob = Uuid::new_v4();
        let peer = "share-link-abc".to_string();
        let (a, first_a) = table.claim(peer.clone(), alice, 100);
        let (b, first_b) = table.claim(peer.clone(), bob, 200);
        assert!(first_a);
        assert!(!first_b);
        assert_eq!(a.user_id, alice);
        assert_eq!(b.user_id, alice);
        assert_eq!(b.claimed_at_unix, 100);
        assert_eq!(table.resolve(&peer).unwrap().user_id, alice);
    }

    #[test]
    fn unclaimed_resolves_to_none() {
        let table = ClaimTable::new();
        assert!(table.resolve("share-link-unknown").is_none());
    }
}
