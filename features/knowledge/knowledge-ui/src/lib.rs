//! Knowledge feature UI.
//!
//! Phase 5c — first Knowledge route. A live wrapper
//! [`KnowledgeLive`] subscribes to the server's `vault/org` doc via
//! `WorkspaceSync`, lists pages, and shows the blocks for whichever
//! page is selected. Block content edits commit locally and
//! propagate to peers via `apply_update` — the same pattern
//! `project-ui::TasksByProjectLive` uses.

pub mod awareness;
pub mod graph_loader;
pub mod graph_writer;
#[cfg(target_arch = "wasm32")]
pub mod idb_persistence;
pub mod inline_md;
pub mod live;
pub mod logseq_shell;
pub mod outliner;
pub mod properties_pane;
pub mod query;
pub mod seed;
pub mod tasks_kanban;
pub mod up_tree;
pub mod view_mode;
pub mod views;

pub use live::{KnowledgeLive, KnowledgeSnapshot, KnowledgeView};
pub use logseq_shell::LogseqShell;
pub use properties_pane::PropertiesPane;
pub use tasks_kanban::TasksKanbanLive;
pub use views::{KindCalendar, KindGallery, KindKanban, KindList};
