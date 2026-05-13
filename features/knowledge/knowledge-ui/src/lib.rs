//! Knowledge feature UI — dumb Dioxus components. Data + callbacks
//! in, RSX out. No Loro client, no service wiring; `task-ui` does
//! the runtime construction.
//!
//! Submodules:
//!
//! - [`canvas`] — Logseq-style 2D whiteboard. Blocks live as live
//!                references (not JSON-copies). Pan/zoom/drag,
//!                connectors, sticky notes. See `canvas::Whiteboard`.
//! - [`editor`] — span-level live-preview block editor (Phase B).
//! - [`views`]  — full-page surrounding UI (`PageView`).
//! - [`embed`]  — embeddable variants (outliner / block / page) used
//!                by other features.
//! - [`backlinks`], [`quickswitcher`], [`vault_picker`],
//!   [`page_preview`], [`tag_explorer`], [`properties_panel`] —
//!   standalone Phase B-surround components.

pub mod bases;
pub mod canvas;
pub mod common;
pub mod editor;

pub mod backlinks;
pub mod embed;
pub mod page_preview;
pub mod properties_panel;
pub mod quickswitcher;
pub mod tag_explorer;
pub mod vault_picker;
pub mod views;

pub use common::{BlockBrief, PageBrief, PageKind};

// Re-export the main components for `use knowledge_ui::*` ergonomics.
pub use backlinks::BacklinksPanel;
pub use embed::{BlockEmbed, EmbedDensity, OutlinerEmbed, PageEmbed};
pub use page_preview::PagePreview;
pub use properties_panel::PropertiesPanel;
pub use quickswitcher::{Quickswitcher, QuickswitcherPick};
pub use tag_explorer::TagExplorer;
pub use vault_picker::VaultPicker;
pub use views::PageView;

pub use bases::{BaseResultSet, BaseTableView, BaseView, ColumnSpec, PageRow, ResultRow, run_base};
