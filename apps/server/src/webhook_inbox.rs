//! In-memory webhook audit log.
//!
//! Every inbound webhook (GitHub, Hermes, …) inserts a row BEFORE
//! verifying the signature so failed-auth attempts stay visible.
//! After dispatch we update the row with `processed_at` + optional
//! `error`.
//!
//! **v1 storage**: a `Mutex<VecDeque<WebhookInboxRow>>` capped at
//! 1,024 rows (oldest evicted). Promoted to a SeaORM table in a
//! follow-up — the schema below mirrors what the table will look
//! like so the migration is mechanical.
//!
//! ```ignore
//! CREATE TABLE webhook_inbox (
//!     id            UUID PRIMARY KEY,
//!     integration   TEXT NOT NULL,
//!     received_at   TIMESTAMPTZ NOT NULL,
//!     headers_json  TEXT NOT NULL,
//!     body          BLOB NOT NULL,
//!     signature_ok  BOOLEAN NOT NULL,
//!     processed_at  TIMESTAMPTZ,
//!     error         TEXT
//! );
//! ```

use std::collections::VecDeque;
use std::sync::Arc;

use chrono::{DateTime, Utc};
use tokio::sync::Mutex;
use uuid::Uuid;

const CAP: usize = 1024;

#[derive(Clone, Debug)]
pub struct WebhookInboxRow {
    pub id: Uuid,
    pub integration: String,
    pub received_at: DateTime<Utc>,
    pub headers_json: String,
    pub body: Vec<u8>,
    pub signature_ok: bool,
    pub processed_at: Option<DateTime<Utc>>,
    pub error: Option<String>,
}

#[derive(Clone, Default)]
pub struct WebhookInbox {
    inner: Arc<Mutex<VecDeque<WebhookInboxRow>>>,
}

impl WebhookInbox {
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a fresh row before signature verification. Returns the
    /// generated row id so the caller can update it later.
    pub async fn insert(
        &self,
        integration: &str,
        headers_json: String,
        body: Vec<u8>,
        signature_ok: bool,
    ) -> Uuid {
        let row = WebhookInboxRow {
            id: Uuid::new_v4(),
            integration: integration.to_string(),
            received_at: Utc::now(),
            headers_json,
            body,
            signature_ok,
            processed_at: None,
            error: None,
        };
        let id = row.id;
        let mut q = self.inner.lock().await;
        if q.len() >= CAP {
            q.pop_front();
        }
        q.push_back(row);
        id
    }

    /// Stamp the row with `processed_at` + optional error after dispatch.
    pub async fn finish(&self, id: Uuid, error: Option<String>) {
        let mut q = self.inner.lock().await;
        if let Some(row) = q.iter_mut().find(|r| r.id == id) {
            row.processed_at = Some(Utc::now());
            row.error = error;
        }
    }

    /// Snapshot of recent rows (most-recent last). For debug pages.
    pub async fn recent(&self) -> Vec<WebhookInboxRow> {
        let q = self.inner.lock().await;
        q.iter().cloned().collect()
    }
}
