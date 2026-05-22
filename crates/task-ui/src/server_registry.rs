//! Phase 8 — client-side server registry.
//!
//! Persists across reloads via `localStorage` on web; in-memory
//! only on native (the desktop launcher carries its own state).
//! Stored as JSON under the key
//! [`STORAGE_KEY`](Self::STORAGE_KEY). The registry exposes a
//! `Signal<Vec<ServerEntry>>` so Dioxus components re-render when
//! entries change.
//!
//! Phase 8 keeps the shape small: each entry is `(label,
//! server_url, session_token, my_user_id)`. The token is what
//! Phase 2's `AuthService::sign_in_email_password` returned; the
//! federated view forwards it via `AuthClientMiddleware` so the
//! server knows who's asking.

use std::collections::BTreeMap;

use dioxus::prelude::*;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

pub const STORAGE_KEY: &str = "task-architect.servers.v1";

/// One row of the registry. `label` is user-chosen ("work",
/// "studio"); `server_url` is the WS endpoint root
/// (`ws://host:port/vox`); `session_token` is the bearer from
/// Phase 2; `my_user_id` is the user-id Phase 2 returned at
/// sign-in (used by the federated view to disambiguate).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ServerEntry {
    pub id: Uuid,
    pub label: String,
    pub server_url: String,
    pub session_token: Option<String>,
    pub my_user_id: Option<Uuid>,
    pub my_email: Option<String>,
}

impl ServerEntry {
    pub fn new(label: impl Into<String>, server_url: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            label: label.into(),
            server_url: server_url.into(),
            session_token: None,
            my_user_id: None,
            my_email: None,
        }
    }
}

/// Clone-cheap handle over the registry. Wraps a Dioxus `Signal`
/// so consumers re-render on changes.
#[derive(Debug, Clone, Copy)]
pub struct ServerRegistry {
    entries: Signal<BTreeMap<Uuid, ServerEntry>>,
}

impl ServerRegistry {
    /// Build a registry, loading any persisted entries from
    /// localStorage on wasm. On native, starts empty.
    #[must_use]
    pub fn new() -> Self {
        let initial = load_from_storage().unwrap_or_default();
        Self {
            entries: Signal::new(initial),
        }
    }

    /// Snapshot of current entries, sorted by label.
    #[must_use]
    pub fn list(&self) -> Vec<ServerEntry> {
        let mut out: Vec<ServerEntry> = self.entries.read().values().cloned().collect();
        out.sort_by(|a, b| a.label.cmp(&b.label));
        out
    }

    #[must_use]
    pub fn get(&self, id: Uuid) -> Option<ServerEntry> {
        self.entries.read().get(&id).cloned()
    }

    pub fn upsert(&mut self, entry: ServerEntry) {
        self.entries.with_mut(|map| {
            map.insert(entry.id, entry);
        });
        self.persist();
    }

    pub fn remove(&mut self, id: Uuid) {
        self.entries.with_mut(|map| {
            map.remove(&id);
        });
        self.persist();
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.entries.read().len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.entries.read().is_empty()
    }

    fn persist(&self) {
        let snapshot: BTreeMap<Uuid, ServerEntry> = self.entries.read().clone();
        save_to_storage(&snapshot);
    }
}

impl Default for ServerRegistry {
    fn default() -> Self {
        Self::new()
    }
}

// ── localStorage I/O (wasm-only) ────────────────────────────────

#[cfg(target_arch = "wasm32")]
fn load_from_storage() -> Option<BTreeMap<Uuid, ServerEntry>> {
    let storage = web_sys::window()?.local_storage().ok()??;
    let json = storage.get_item(STORAGE_KEY).ok()??;
    serde_json::from_str(&json).ok()
}

#[cfg(target_arch = "wasm32")]
fn save_to_storage(snapshot: &BTreeMap<Uuid, ServerEntry>) {
    let Some(window) = web_sys::window() else {
        return;
    };
    let Ok(Some(storage)) = window.local_storage() else {
        return;
    };
    let Ok(json) = serde_json::to_string(snapshot) else {
        return;
    };
    let _ = storage.set_item(STORAGE_KEY, &json);
}

#[cfg(not(target_arch = "wasm32"))]
fn load_from_storage() -> Option<BTreeMap<Uuid, ServerEntry>> {
    None
}

#[cfg(not(target_arch = "wasm32"))]
fn save_to_storage(_snapshot: &BTreeMap<Uuid, ServerEntry>) {
    // Native persistence (servers.json) follows in a later
    // commit. Phase 8 ships wasm-only.
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_serialization() {
        let mut map: BTreeMap<Uuid, ServerEntry> = BTreeMap::new();
        let entry = ServerEntry {
            id: Uuid::new_v4(),
            label: "work".into(),
            server_url: "ws://example.test:9090/vox".into(),
            session_token: Some("tok-123".into()),
            my_user_id: Some(Uuid::new_v4()),
            my_email: Some("alice@example.test".into()),
        };
        map.insert(entry.id, entry.clone());
        let json = serde_json::to_string(&map).expect("serialize");
        let back: BTreeMap<Uuid, ServerEntry> = serde_json::from_str(&json).expect("deserialize");
        assert_eq!(back, map);
    }
}
