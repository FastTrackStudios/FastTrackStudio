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
use fts_ui::prelude::*;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

pub const STORAGE_KEY: &str = "task-architect.servers.v1";
/// Storage key / filename stem for the active-server pointer.
pub const ACTIVE_KEY: &str = "task-architect.active.v1";

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

/// Clone-cheap handle over the registry. Wraps Dioxus `Signal`s so
/// consumers re-render on changes. `active` names the entry the app
/// currently connects to (`None` = fall back to env/same-origin).
#[derive(Debug, Clone, Copy)]
pub struct ServerRegistry {
    entries: Signal<BTreeMap<Uuid, ServerEntry>>,
    active: Signal<Option<Uuid>>,
}

impl ServerRegistry {
    /// Build a registry, loading any persisted entries + active
    /// selection from storage (localStorage on wasm, a JSON file on
    /// native).
    #[must_use]
    pub fn new() -> Self {
        let initial = load_from_storage().unwrap_or_default();
        // Drop a stale active pointer if its entry is gone.
        let active = load_active().filter(|id| initial.contains_key(id));
        Self {
            entries: Signal::new(initial),
            active: Signal::new(active),
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
        if *self.active.peek() == Some(id) {
            self.set_active(None);
        }
        self.persist();
    }

    /// The active entry's id (the reactive signal — reads subscribe).
    #[must_use]
    pub fn active_id(&self) -> Option<Uuid> {
        *self.active.read()
    }

    /// The active entry, if any.
    #[must_use]
    pub fn active_entry(&self) -> Option<ServerEntry> {
        self.active_id().and_then(|id| self.get(id))
    }

    /// Point the app at `id` (or clear back to the env/same-origin default).
    pub fn set_active(&mut self, id: Option<Uuid>) {
        self.active.set(id);
        save_active(id);
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

/// Normalize a user-typed server address into a vox base URL. Bare
/// hosts default to `wss://` (production over TLS); an explicit scheme
/// is respected. `http(s)` is mapped to `ws(s)`.
fn normalize_url(raw: &str) -> String {
    let t = raw.trim().trim_end_matches('/');
    if let Some(rest) = t.strip_prefix("https://") {
        format!("wss://{rest}")
    } else if let Some(rest) = t.strip_prefix("http://") {
        format!("ws://{rest}")
    } else if t.contains("://") {
        t.to_owned()
    } else {
        format!("wss://{t}")
    }
}

/// Host portion of a normalized URL, for a default label.
fn host_of(url: &str) -> String {
    url.split("://")
        .nth(1)
        .unwrap_or(url)
        .split('/')
        .next()
        .unwrap_or(url)
        .to_owned()
}

/// Add / select / remove the servers the app can connect to. Reads the
/// [`ServerRegistry`] from context; selecting an entry re-points the app
/// (org discovery + connection) at that server via the app root's holder
/// sync. Sign-in to the selected server happens through the login form
/// (which dials the active server).
#[component]
pub fn ServersPanel() -> Element {
    let mut registry = use_context::<ServerRegistry>();
    let mut label = use_signal(String::new);
    let mut url = use_signal(String::new);
    // Why discovery didn't resolve for the active server, if it failed.
    let discovery_err = use_context::<crate::orgs::DiscoveryError>().0;

    let servers = registry.list();
    let active = registry.active_id();

    let add = move |_| {
        let raw = url.peek().trim().to_owned();
        if raw.is_empty() {
            return;
        }
        let normalized = normalize_url(&raw);
        let chosen = label.peek().trim().to_owned();
        let entry_label = if chosen.is_empty() {
            host_of(&normalized)
        } else {
            chosen
        };
        let entry = ServerEntry::new(entry_label, normalized);
        let id = entry.id;
        registry.upsert(entry);
        registry.set_active(Some(id));
        label.set(String::new());
        url.set(String::new());
    };

    rsx! {
        div { class: "flex flex-col gap-3",
            if let Some(err) = discovery_err.read().as_ref() {
                div { class: "rounded-lg border border-destructive/40 bg-destructive/10 px-2 py-2 text-xs text-destructive",
                    div { class: "font-medium", "Couldn't reach this server" }
                    div { class: "mt-0.5 break-words opacity-90", "{err}" }
                }
            }
            if servers.is_empty() {
                div { class: "px-1 text-xs text-muted-foreground",
                    "No servers yet. Add one below (e.g. task.starcommand.live)."
                }
            } else {
                div { class: "flex flex-col gap-1",
                    for s in servers {
                        div {
                            key: "{s.id}",
                            class: "flex items-center gap-2 rounded-lg border border-border px-2 py-2",
                            button {
                                r#type: "button",
                                class: "flex min-w-0 flex-1 flex-col text-left",
                                onclick: {
                                    let id = s.id;
                                    move |_| registry.set_active(Some(id))
                                },
                                span { class: "truncate text-sm font-medium text-foreground", "{s.label}" }
                                span { class: "truncate text-xs text-muted-foreground", "{s.server_url}" }
                            }
                            if Some(s.id) == active {
                                span { class: "text-xs text-primary", "● active" }
                            }
                            button {
                                r#type: "button",
                                class: "text-xs text-muted-foreground hover:text-destructive",
                                onclick: {
                                    let id = s.id;
                                    move |_| registry.remove(id)
                                },
                                "Remove"
                            }
                        }
                    }
                }
            }

            div { class: "flex flex-col gap-2",
                Input { value: label, placeholder: "Label (optional)" }
                Input {
                    value: url,
                    input_type: "url".to_string(),
                    placeholder: "Server URL (task.starcommand.live)",
                }
                Button {
                    variant: ButtonVariant::Secondary,
                    size: ButtonSize::Medium,
                    on_click: add,
                    class: "w-full",
                    "Add server"
                }
            }
        }
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

#[cfg(target_arch = "wasm32")]
fn load_active() -> Option<Uuid> {
    let storage = web_sys::window()?.local_storage().ok()??;
    let raw = storage.get_item(ACTIVE_KEY).ok()??;
    Uuid::parse_str(raw.trim()).ok()
}

#[cfg(target_arch = "wasm32")]
fn save_active(id: Option<Uuid>) {
    let Some(window) = web_sys::window() else {
        return;
    };
    let Ok(Some(storage)) = window.local_storage() else {
        return;
    };
    match id {
        Some(id) => {
            let _ = storage.set_item(ACTIVE_KEY, &id.to_string());
        }
        None => {
            let _ = storage.remove_item(ACTIVE_KEY);
        }
    }
}

// Native (desktop/mobile): persist to `$XDG_DATA_HOME/task/servers.json`
// (stays inside the iOS/macOS app container). Mirrors the token store's
// data-dir choice in `auth.rs`.
#[cfg(not(target_arch = "wasm32"))]
fn servers_path() -> Option<std::path::PathBuf> {
    let base = std::env::var_os("XDG_DATA_HOME")
        .map(std::path::PathBuf::from)
        .filter(|p| !p.as_os_str().is_empty())
        .or_else(|| {
            std::env::var_os("HOME")
                .map(|h| std::path::PathBuf::from(h).join(".local").join("share"))
        })?;
    Some(base.join("task").join("servers.json"))
}

#[cfg(not(target_arch = "wasm32"))]
fn load_from_storage() -> Option<BTreeMap<Uuid, ServerEntry>> {
    let json = std::fs::read_to_string(servers_path()?).ok()?;
    serde_json::from_str(&json).ok()
}

#[cfg(not(target_arch = "wasm32"))]
fn save_to_storage(snapshot: &BTreeMap<Uuid, ServerEntry>) {
    let Some(path) = servers_path() else {
        return;
    };
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    if let Ok(json) = serde_json::to_string_pretty(snapshot) {
        let _ = std::fs::write(path, json);
    }
}

#[cfg(not(target_arch = "wasm32"))]
fn active_path() -> Option<std::path::PathBuf> {
    servers_path().map(|p| p.with_file_name("active-server"))
}

#[cfg(not(target_arch = "wasm32"))]
fn load_active() -> Option<Uuid> {
    let raw = std::fs::read_to_string(active_path()?).ok()?;
    Uuid::parse_str(raw.trim()).ok()
}

#[cfg(not(target_arch = "wasm32"))]
fn save_active(id: Option<Uuid>) {
    let Some(path) = active_path() else {
        return;
    };
    match id {
        Some(id) => {
            if let Some(parent) = path.parent() {
                let _ = std::fs::create_dir_all(parent);
            }
            let _ = std::fs::write(path, id.to_string());
        }
        None => {
            let _ = std::fs::remove_file(path);
        }
    }
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
