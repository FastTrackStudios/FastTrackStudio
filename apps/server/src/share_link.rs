//! Phase 4 — `ShareService` implementation.
//!
//! Issues capability tokens scoped to a single doc, tracks active
//! tokens in an in-memory map, and supports revocation. Revoked
//! tokens fail capability verification at the WS-handler layer
//! before they reach a service method.
//!
//! The persistence story is intentionally in-memory for Phase 4 —
//! a server restart drops the revocation list. Phase 5 will give
//! `ShareService` a `share_links` root container on the project
//! doc (per `decentralized-foundation.md` §14.5) so it survives
//! restarts as a normal CRDT subtree.

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

use project_proto::{
    DocIdArg, ShareError, ShareLinkCreated, ShareLinkList, ShareLinkMeta, ShareLinkScope,
    ShareService, TokenIdArg,
};
use uuid::Uuid;

use crate::capability::{CapabilityScope, ServerKeypair};

/// Revocation list shared between `ShareService` and the WS handler.
/// Cheap to clone; reads are O(1).
#[derive(Debug, Clone, Default)]
pub struct RevocationList {
    inner: Arc<Mutex<HashSet<Uuid>>>,
}

impl RevocationList {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn revoke(&self, token_id: Uuid) {
        self.inner.lock().unwrap().insert(token_id);
    }
    pub fn is_revoked(&self, token_id: &Uuid) -> bool {
        self.inner.lock().unwrap().contains(token_id)
    }
}

/// One row in the in-memory share-link table.
#[derive(Debug, Clone)]
struct Entry {
    meta: ShareLinkMeta,
}

#[derive(Clone)]
pub struct ShareServiceImpl {
    keypair: ServerKeypair,
    revocations: RevocationList,
    entries: Arc<Mutex<HashMap<Uuid, Entry>>>,
}

impl ShareServiceImpl {
    pub fn new(keypair: ServerKeypair, revocations: RevocationList) -> Self {
        Self {
            keypair,
            revocations,
            entries: Arc::new(Mutex::new(HashMap::new())),
        }
    }
}

fn now_unix() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
}

impl ShareService for ShareServiceImpl {
    async fn create(&self, scope: ShareLinkScope) -> Result<ShareLinkCreated, ShareError> {
        if scope.doc_id.is_empty() {
            return Err(ShareError::InvalidScope("doc_id required".into()));
        }
        if scope.ttl_seconds <= 0 {
            return Err(ShareError::InvalidScope("ttl_seconds must be > 0".into()));
        }
        let token_id = Uuid::new_v4();
        let expires_unix = now_unix().saturating_add(scope.ttl_seconds);
        let cap_scope = CapabilityScope {
            expires_unix,
            can_write: scope.can_write,
            doc_ids: vec![project_proto::DocId::new(scope.doc_id.clone())],
            token_id: Some(token_id),
            peer_id: Some(format!("share-link-{token_id}")),
            attachments_only: false,
        };
        let token = self.keypair.issue(&cap_scope);
        let meta = ShareLinkMeta {
            token_id,
            doc_id: scope.doc_id.clone(),
            can_write: scope.can_write,
            expires_unix,
            label: scope.label.clone(),
            revoked: false,
        };
        self.entries
            .lock()
            .unwrap()
            .insert(token_id, Entry { meta });
        Ok(ShareLinkCreated {
            token_id,
            token,
            expires_unix,
        })
    }

    async fn list(&self, doc: DocIdArg) -> Result<ShareLinkList, ShareError> {
        let entries = self.entries.lock().unwrap();
        let items = entries
            .values()
            .filter(|e| e.meta.doc_id == doc.doc_id)
            .map(|e| {
                let mut m = e.meta.clone();
                m.revoked = self.revocations.is_revoked(&m.token_id);
                m
            })
            .collect();
        Ok(ShareLinkList { items })
    }

    async fn revoke(&self, arg: TokenIdArg) -> Result<(), ShareError> {
        let entries = self.entries.lock().unwrap();
        if !entries.contains_key(&arg.token_id) {
            return Err(ShareError::NotFound);
        }
        drop(entries);
        self.revocations.revoke(arg.token_id);
        tracing::info!(token_id = %arg.token_id, "share link revoked");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn create_list_revoke_round_trip() {
        let kp = ServerKeypair::generate_ephemeral();
        let revs = RevocationList::new();
        let svc = ShareServiceImpl::new(kp.clone(), revs.clone());

        let scope = ShareLinkScope {
            doc_id: "project/x".into(),
            can_write: false,
            ttl_seconds: 3600,
            label: Some("test".into()),
        };
        let created = svc.create(scope).await.expect("create");
        assert!(!created.token.is_empty());
        let cap = kp.verify(&created.token, now_unix()).expect("verify");
        assert_eq!(cap.token_id, Some(created.token_id));
        assert_eq!(
            cap.peer_id.as_deref(),
            Some(format!("share-link-{}", created.token_id)).as_deref()
        );

        let list = svc
            .list(DocIdArg {
                doc_id: "project/x".into(),
            })
            .await
            .expect("list");
        assert_eq!(list.items.len(), 1);
        assert!(!list.items[0].revoked);

        // Other doc: empty list.
        let other = svc
            .list(DocIdArg {
                doc_id: "project/y".into(),
            })
            .await
            .expect("list other");
        assert_eq!(other.items.len(), 0);

        // Revoke.
        svc.revoke(TokenIdArg {
            token_id: created.token_id,
        })
        .await
        .expect("revoke");
        assert!(revs.is_revoked(&created.token_id));

        let after = svc
            .list(DocIdArg {
                doc_id: "project/x".into(),
            })
            .await
            .expect("list after");
        assert!(after.items[0].revoked);

        // Revoke unknown → NotFound.
        let bad = svc
            .revoke(TokenIdArg {
                token_id: Uuid::new_v4(),
            })
            .await;
        assert!(matches!(bad, Err(ShareError::NotFound)));
    }

    #[tokio::test]
    async fn invalid_scope_rejected() {
        let svc = ShareServiceImpl::new(ServerKeypair::generate_ephemeral(), RevocationList::new());
        assert!(matches!(
            svc.create(ShareLinkScope {
                doc_id: String::new(),
                can_write: false,
                ttl_seconds: 60,
                label: None
            })
            .await,
            Err(ShareError::InvalidScope(_))
        ));
        assert!(matches!(
            svc.create(ShareLinkScope {
                doc_id: "project/x".into(),
                can_write: false,
                ttl_seconds: 0,
                label: None
            })
            .await,
            Err(ShareError::InvalidScope(_))
        ));
    }
}
