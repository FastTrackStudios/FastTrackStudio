//! Items live implementation — business logic + auth validation
//!
//! Delegates all data access to ItemRepo. Never touches SeaORM directly.

use super::proto::*;
use super::repo::ItemRepo;
use crate::example::auth::live::Auth;
use crate::example::auth::proto::SessionToken;
use better_auth_core::types::{AuthRequest, HttpMethod};
use vox::Tx;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

#[derive(Clone)]
pub struct ItemServiceLive {
    repo: Arc<ItemRepo>,
    auth: Arc<Auth>,
    subscribers: Arc<RwLock<Vec<(String, Arc<Tx<ItemEvent>>)>>>,
}

impl ItemServiceLive {
    pub fn new(repo: ItemRepo, auth: Arc<Auth>) -> Self {
        Self {
            repo: Arc::new(repo),
            auth,
            subscribers: Arc::new(RwLock::new(Vec::new())),
        }
    }

    async fn validate(&self, token: &SessionToken) -> Option<String> {
        let req = AuthRequest {
            method: HttpMethod::Get,
            path: "/get-session".to_string(),
            headers: HashMap::from([(
                "authorization".to_string(),
                format!("Bearer {}", token.token),
            )]),
            body: None,
            query: HashMap::new(),
        };

        match self.auth.handle_request(req).await {
            Ok(resp) if resp.status == 200 => {
                let v: serde_json::Value = serde_json::from_slice(&resp.body).ok()?;
                let user_id = v["user"]["id"].as_str().map(String::from);
                tracing::debug!("validate: status=200 user_id={:?}", user_id);
                user_id
            }
            Ok(resp) => {
                let body = String::from_utf8_lossy(&resp.body);
                tracing::warn!("validate: status={} body={}", resp.status, body);
                None
            }
            Err(e) => {
                tracing::error!("validate: error={:?}", e);
                None
            }
        }
    }

    async fn read_items(&self, owner_id: &str) -> Vec<ItemInfo> {
        self.repo
            .find_by_owner(owner_id)
            .await
            .into_iter()
            .map(|m| ItemInfo {
                id: m.id,
                name: m.name,
                done: m.done,
            })
            .collect()
    }

    async fn broadcast_change(&self, owner_id: &str) {
        let items = self.read_items(owner_id).await;
        tracing::debug!(
            "broadcast_change: {} items for owner {}",
            items.len(),
            owner_id
        );
        let event = ItemEvent::ListChanged { items };
        let subs = self.subscribers.read().unwrap().clone();
        tracing::debug!("broadcast_change: {} subscribers", subs.len());
        for (uid, tx) in subs {
            if uid == owner_id {
                let event = event.clone();
                tokio::spawn(async move {
                    let _ = tx.send(&event).await;
                });
            }
        }
    }
}

impl ItemService for ItemServiceLive {
    async fn list_items(&self, _token: SessionToken) -> Vec<ItemInfo> {
        match self.validate(&token).await {
            Some(user_id) => self.read_items(&user_id).await,
            None => Vec::new(),
        }
    }

    async fn create_item(&self, _token: SessionToken, name: String) -> String {
        let Some(user_id) = self.validate(&token).await else {
            return String::new();
        };
        let id = uuid::Uuid::new_v4().to_string();
        if self
            .repo
            .insert(id.clone(), user_id.clone(), name)
            .await
            .is_ok()
        {
            self.broadcast_change(&user_id).await;
        }
        id
    }

    async fn toggle_item(&self, _token: SessionToken, id: String) {
        let Some(user_id) = self.validate(&token).await else {
            return;
        };
        if self.repo.toggle(&id, &user_id).await {
            self.broadcast_change(&user_id).await;
        }
    }

    async fn delete_item(&self, _token: SessionToken, id: String) {
        let Some(user_id) = self.validate(&token).await else {
            return;
        };
        if self.repo.delete(&id, &user_id).await {
            self.broadcast_change(&user_id).await;
        }
    }

    async fn subscribe(&self, _token: SessionToken, tx: Tx<ItemEvent>) {
        if let Some(user_id) = self.validate(&token).await {
            let items = self.read_items(&user_id).await;
            let _ = tx.send(&ItemEvent::ListChanged { items }).await;
            self.subscribers
                .write()
                .unwrap()
                .push((user_id, Arc::new(tx)));
        }
    }
}
