//! [`ProductBackend`] — `EmailProduct` impl + the delivery poller.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use architect::vox;
use email_proto::{
    Draft, EmailChange, EmailEvent, EmailProduct, EmailSync, EmailSyncError, OutboxEntry,
    OutboxStatus,
};
use email_store::{Store, StoreError};

/// One account the product layer serves: the account id (must
/// match the `EmailSync` backend's) and the account root the
/// store lives under (`<root>/index.db`).
pub struct ProductAccount {
    pub id: String,
    pub root: PathBuf,
}

/// Store-backed `EmailProduct` backend + delivery poller. Cheap
/// to `Clone` — internals are `Arc`'d.
#[derive(Clone, architect::HasDispatcher)]
pub struct ProductBackend {
    inner: Arc<Inner>,
}

struct Inner {
    /// Per-account product store (outbox + derivation + notify
    /// tables in the account's `index.db`).
    stores: HashMap<String, Mutex<Store>>,
    /// The mounted mailbox backend — delivery calls its `send`,
    /// so the Sent copy + `NewMessage` event come for free.
    sync: Arc<dyn EmailSync + Send + Sync>,
    /// The `EmailChange` hub the `EmailSync` stream serves —
    /// cloned from the sync backend so product events reach the
    /// same subscribers.
    hub: architect::PubSub<EmailChange>,
    /// Poller wake-up — `approve` pokes it so delivery starts
    /// promptly instead of waiting out the interval.
    wake: tokio::sync::Notify,
}

/// Base delay before retrying a failed delivery; doubles per
/// retry (30s, 1m, 2m, 4m, 8m) up to [`MAX_BACKOFF`].
const BASE_BACKOFF: Duration = Duration::from_secs(30);
const MAX_BACKOFF: Duration = Duration::from_secs(3600);

/// Entries delivered per account per poller pass — keeps one
/// pass bounded (mirrors the triage pass budget).
const DRAIN_BUDGET: u32 = 5;

impl ProductBackend {
    /// Open the per-account stores and build the backend.
    ///
    /// `sync` is the mounted `EmailSync` backend delivery goes
    /// through; `hub` is that backend's `EmailChange` hub
    /// (`EmailSyncStreamSource::changes_hub(&b).clone()`), so
    /// outbox events interleave with mailbox events on the one
    /// stream subscribers already hold.
    pub fn new<I>(
        accounts: I,
        sync: Arc<dyn EmailSync + Send + Sync>,
        hub: architect::PubSub<EmailChange>,
    ) -> Result<Self, StoreError>
    where
        I: IntoIterator<Item = ProductAccount>,
    {
        let mut stores = HashMap::new();
        for acct in accounts {
            stores.insert(acct.id, Mutex::new(Store::open(acct.root)?));
        }
        Ok(Self {
            inner: Arc::new(Inner {
                stores,
                sync,
                hub,
                wake: tokio::sync::Notify::new(),
            }),
        })
    }

    fn store(&self, account: &str) -> Result<&Mutex<Store>, EmailSyncError> {
        self.inner
            .stores
            .get(account)
            .ok_or(EmailSyncError::UnknownAccount)
    }

    /// Publish an outbox transition on the shared stream.
    fn publish_outbox(&self, account: &str, entry: &OutboxEntry) {
        self.inner.hub.publish(EmailChange {
            account: account.to_string(),
            event: EmailEvent::OutboxChanged {
                id: entry.id,
                status: entry.status,
            },
        });
    }

    /// Start the delivery poller: wakes every `interval` (or
    /// immediately on approval), claims due `Approved`/retryable
    /// `Failed` entries, and delivers them through
    /// `EmailSync::send`. Abort the returned handle to stop.
    pub fn spawn_poller(&self, interval: Duration) -> tokio::task::JoinHandle<()> {
        let backend = self.clone();
        tokio::spawn(async move {
            loop {
                tokio::select! {
                    () = backend.inner.wake.notified() => {}
                    () = tokio::time::sleep(interval) => {}
                }
                backend.drain_outbox_once().await;
            }
        })
    }

    /// One delivery pass over every account. Public so tests (and
    /// a future explicit "flush now" surface) can drive it
    /// without the poller task.
    pub async fn drain_outbox_once(&self) {
        let accounts: Vec<String> = self.inner.stores.keys().cloned().collect();
        for account in accounts {
            if let Err(err) = self.drain_account(&account).await {
                tracing::warn!(account, %err, "outbox drain failed");
            }
        }
    }

    async fn drain_account(&self, account: &str) -> Result<(), EmailSyncError> {
        // Claim (flips to Sending atomically).
        let claimed = {
            let backend = self.clone();
            let account = account.to_string();
            tokio::task::spawn_blocking(move || -> Result<Vec<OutboxEntry>, EmailSyncError> {
                let store = backend.store(&account)?;
                let mut store = store.lock().expect("store mutex");
                store
                    .outbox_claim_due(&account, now_ms(), DRAIN_BUDGET)
                    .map_err(map_store)
            })
            .await
            .map_err(|e| EmailSyncError::Internal(e.to_string()))??
        };

        for entry in claimed {
            self.publish_outbox(account, &entry);
            let outcome = {
                let sync = self.inner.sync.clone();
                let account = account.to_string();
                let draft = entry.draft.clone();
                tokio::task::spawn_blocking(move || sync.send(&account, draft))
                    .await
                    .map_err(|e| EmailSyncError::Internal(e.to_string()))?
            };

            let backend = self.clone();
            let account_owned = account.to_string();
            let id = entry.id;
            let retries = entry.retries;
            let updated = tokio::task::spawn_blocking(move || -> Result<OutboxEntry, EmailSyncError> {
                let store = backend.store(&account_owned)?;
                let mut store = store.lock().expect("store mutex");
                match outcome {
                    Ok(message_id) => store
                        .outbox_mark_sent(&account_owned, id, &message_id, now_ms())
                        .map_err(map_store),
                    Err(err) => {
                        let backoff = BASE_BACKOFF
                            .saturating_mul(2u32.saturating_pow(retries))
                            .min(MAX_BACKOFF);
                        store
                            .outbox_mark_failed(
                                &account_owned,
                                id,
                                &err.to_string(),
                                now_ms(),
                                now_ms() + backoff.as_millis() as i64,
                            )
                            .map_err(map_store)
                    }
                }
            })
            .await
            .map_err(|e| EmailSyncError::Internal(e.to_string()))??;

            if updated.status == OutboxStatus::Failed {
                tracing::warn!(
                    account,
                    id = updated.id,
                    retries = updated.retries,
                    error = updated.last_error.as_deref().unwrap_or(""),
                    "outbox delivery failed"
                );
            }
            self.publish_outbox(account, &updated);
        }
        Ok(())
    }
}

impl EmailProduct for ProductBackend {
    fn list_outbox(&self, account: &str) -> Result<Vec<OutboxEntry>, EmailSyncError> {
        let store = self.store(account)?.lock().expect("store mutex");
        store.outbox_list(account).map_err(map_store)
    }

    fn submit_draft(
        &self,
        account: &str,
        draft: Draft,
        origin: &str,
    ) -> Result<OutboxEntry, EmailSyncError> {
        if draft.to.is_empty() && draft.cc.is_empty() && draft.bcc.is_empty() {
            return Err(EmailSyncError::Protocol("draft has no recipients".into()));
        }
        let entry = {
            let mut store = self.store(account)?.lock().expect("store mutex");
            store
                .outbox_submit(account, &draft, origin, now_ms())
                .map_err(map_store)?
        };
        self.publish_outbox(account, &entry);
        Ok(entry)
    }

    fn approve(&self, account: &str, id: u64) -> Result<OutboxEntry, EmailSyncError> {
        let entry = {
            let mut store = self.store(account)?.lock().expect("store mutex");
            store.outbox_approve(account, id, now_ms()).map_err(map_store)?
        };
        self.publish_outbox(account, &entry);
        // Deliver promptly — don't wait out the poller interval.
        self.inner.wake.notify_one();
        Ok(entry)
    }

    fn cancel(&self, account: &str, id: u64) -> Result<OutboxEntry, EmailSyncError> {
        let entry = {
            let mut store = self.store(account)?.lock().expect("store mutex");
            store.outbox_cancel(account, id, now_ms()).map_err(map_store)?
        };
        self.publish_outbox(account, &entry);
        Ok(entry)
    }
}

fn now_ms() -> i64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0)
}

fn map_store(err: StoreError) -> EmailSyncError {
    match err {
        StoreError::OutboxNotFound(_) => EmailSyncError::NotFound,
        e @ StoreError::OutboxTransition { .. } => EmailSyncError::Protocol(e.to_string()),
        e => EmailSyncError::Internal(e.to_string()),
    }
}
