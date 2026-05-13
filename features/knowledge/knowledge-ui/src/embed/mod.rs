//! Embeddable variants of the knowledge UI used by other features.
//!
//! - [`OutlinerEmbed`] — full outliner editor in a sized container, for
//!   Task / Project note panels.
//! - [`BlockEmbed`] — single-block reference renderer for chat
//!   transcripts and task descriptions.
//! - [`PageEmbed`] — Obsidian-style `![[Page]]` transclusion.

pub mod block_embed;
pub mod outliner_embed;
pub mod page_embed;

pub use block_embed::BlockEmbed;
pub use outliner_embed::{EmbedDensity, OutlinerEmbed};
pub use page_embed::PageEmbed;
