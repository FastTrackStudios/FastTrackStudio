//! Knowledge feature UI.
//!
//! Phase 5c — first Knowledge route. A live wrapper
//! [`KnowledgeLive`] subscribes to the server's `vault/org` doc via
//! `WorkspaceSync`, lists pages, and shows the blocks for whichever
//! page is selected. Block content edits commit locally and
//! propagate to peers via `apply_update` — the same pattern
//! `project-ui::TasksByProjectLive` uses.

pub mod live;

pub use live::{KnowledgeLive, KnowledgeSnapshot, KnowledgeView};
