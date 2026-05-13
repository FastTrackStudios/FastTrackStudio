//! `PageEmbed` — Obsidian-style `![[Page]]` transclusion.
//!
//! Reuses [`crate::page_preview::PagePreview`] in `full_height` mode so
//! the embedding context controls overflow (which is what Obsidian's
//! transclusion does).

use crate::page_preview::PagePreview;
use dioxus::prelude::*;
use knowledge_proto::{Block, Page};
use uuid::Uuid;

#[component]
pub fn PageEmbed(
    page: Page,
    top_blocks: Vec<Block>,
    /// `1` = top-level blocks only, `2` = include children, etc. Today
    /// we render whatever the caller provides; the depth is passed
    /// through for future hierarchy-aware rendering.
    expand_depth: u32,
    on_open: EventHandler<Uuid>,
) -> Element {
    let _ = expand_depth;
    rsx! {
        PagePreview {
            page,
            top_blocks,
            on_open,
            full_height: true,
        }
    }
}
