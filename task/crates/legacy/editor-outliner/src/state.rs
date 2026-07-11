//! Top-level shared state. Mirrors Logseq's `frontend.state`
//! namespace — a flat collection of signals other modules read.
//!
//! Every signal lives in [`AppState`], which is provided once
//! via `use_context_provider` at the [`crate::EditorApp`] root.
//! Descendant components call `use_context::<AppState>()` to
//! read or mutate.

use dioxus::prelude::*;
use std::path::PathBuf;
use uuid::Uuid;

use crate::db::Vault;

/// Vault root directory (`~/Task/` by default). Computed once
/// at launch — None on wasm where we can't touch the FS.
pub fn default_vault_root() -> Option<PathBuf> {
    #[cfg(target_arch = "wasm32")]
    {
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let home = std::env::var_os("HOME").map(PathBuf::from)?;
        Some(home.join("Task"))
    }
}

/// Which top-level pane the user is looking at. The outliner is
/// the default; settings/cards/graph come later.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Pane {
    /// Single-page outliner (Logseq's "Journals" / "All pages"
    /// merge into one — we navigate via active_page).
    Outliner,
    /// Settings panel.
    Settings,
    /// Reference / graph view (placeholder).
    Graph,
}

/// One bag of every signal the editor cares about. We keep it in
/// one struct so passing it through context is cheap (Signal is
/// Copy) and adding a new global doesn't churn every component
/// signature.
#[derive(Clone, Copy)]
pub struct AppState {
    /// The vault — pages and blocks loaded from disk.
    pub vault: Signal<Vault>,
    /// Currently visible page. `None` until the vault loads.
    pub active_page: Signal<Option<Uuid>>,
    /// Block currently in edit mode. `None` when no block is being
    /// edited (the static render is shown for every block).
    pub editing_block: Signal<Option<Uuid>>,
    /// Top-level pane.
    pub pane: Signal<Pane>,
    /// Vault root path. Read at startup; the writer/watcher loops
    /// read this to know where to read + write.
    pub vault_root: Signal<Option<PathBuf>>,
    /// Bumped after every successful disk reload — descendants
    /// that snapshot the vault can subscribe.
    pub vault_revision: Signal<u64>,
    /// Slash-menu state. `Some({ block, query, slash_start })` when
    /// the menu is open inside a block; `None` when closed.
    pub slash: Signal<Option<SlashState>>,
}

/// What the slash-menu needs to know about: which block hosts
/// the open menu, the substring after `/` the user has typed so
/// far, and the byte offset of the `/` in the block content
/// (so the runner can replace `/query` with the command result).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SlashState {
    pub block_id: Uuid,
    pub query: String,
    pub slash_start: usize,
    /// Highlighted row in the filtered list (0-based).
    pub selected: usize,
}

impl AppState {
    /// Construct the state with empty defaults. Called once via
    /// `use_context_provider` at the `EditorApp` root.
    pub fn new() -> Self {
        Self {
            vault: Signal::new(Vault::default()),
            active_page: Signal::new(None),
            editing_block: Signal::new(None),
            pane: Signal::new(Pane::Outliner),
            vault_root: Signal::new(default_vault_root()),
            vault_revision: Signal::new(0),
            slash: Signal::new(None),
        }
    }
}

impl Default for AppState {
    fn default() -> Self {
        Self::new()
    }
}
