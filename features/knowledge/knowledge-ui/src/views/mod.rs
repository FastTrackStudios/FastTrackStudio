//! Full-page surrounding views.
//!
//! [`PageView`] is the main view: title row, frontmatter editor,
//! outliner editor (or whiteboard, when `ext == "canvas"`), and the
//! Backlinks / Outgoing / Tags / Unresolved tabs along the bottom.

pub mod page_view;

pub use page_view::PageView;
