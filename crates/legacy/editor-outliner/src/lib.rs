//! Logseq-style outliner editor.
//!
//! Module layout mirrors the way Logseq organizes its frontend
//! (see `~/Development/research/logseq/src/main/frontend/` for the
//! reference shape):
//!
//! | crate module      | Logseq namespace              | role                          |
//! |-------------------|-------------------------------|-------------------------------|
//! | [`state`]         | `frontend.state`              | top-level signals + contexts  |
//! | [`db`]            | `frontend.db.*`               | in-memory page/block store    |
//! | [`format`]        | `frontend.format.*`           | markdown parse + serialize    |
//! | [`handler`]       | `frontend.handler.*`          | user-facing actions           |
//! | [`components`]    | `frontend.components.*`       | Dioxus UI                     |
//!
//! ## Source of truth
//!
//! Files on disk in `~/Task/{pages,journals}/*.md` are
//! authoritative. The in-memory store ([`db::Vault`]) rebuilds
//! from disk on launch and reconciles on external change. No
//! CRDT layer — that lives behind a future sync feature.
//!
//! ## Mount point
//!
//! Apps embed [`components::EditorApp`] under their root
//! component. That component:
//!
//! 1. Resolves the default vault root (`~/Task/`),
//! 2. Loads pages + blocks from disk into [`state::AppState`],
//! 3. Renders the shell + sidebar + page view,
//! 4. Mirrors edits back to disk on a debounced timer.

pub mod components;
pub mod db;
pub mod format;
pub mod handler;
pub mod state;

pub use components::EditorApp;
pub use state::{AppState, default_vault_root};
