//! `OutlinerEmbed` — embeddable outliner editor.
//!
//! Wraps Phase B-editor's `OutlinerEditor` in a sized container with
//! density-aware styling. No frontmatter editor and no backlinks tabs;
//! those live in the full `PageView`.

use crate::common::{BlockBrief, PageBrief};
use crate::editor::{BlockUpdate, OutlinerEditor, StructuralOp};
use dioxus::prelude::*;
use knowledge_proto::Block;
use uuid::Uuid;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EmbedDensity {
    Compact,
    Comfortable,
}

#[component]
pub fn OutlinerEmbed(
    page_id: Uuid,
    blocks: Vec<Block>,
    density: EmbedDensity,
    max_height: Option<String>,
    available_pages: Vec<PageBrief>,
    available_blocks: Vec<BlockBrief>,
    on_block_patch: EventHandler<(Uuid, BlockUpdate)>,
    on_structural: EventHandler<StructuralOp>,
    on_open_page: EventHandler<Uuid>,
    on_open_block: EventHandler<Uuid>,
) -> Element {
    let density_class = match density {
        EmbedDensity::Compact => "text-sm gap-1",
        EmbedDensity::Comfortable => "",
    };

    let body = rsx! {
        OutlinerEditor {
            page_id,
            blocks,
            available_pages,
            available_blocks,
            on_block_patch,
            on_structural,
            on_open_page,
            on_open_block,
        }
    };

    match max_height {
        Some(h) => rsx! {
            div {
                class: "overflow-y-auto rounded-md border border-border bg-card p-2 {density_class}",
                style: "max-height: {h}",
                {body}
            }
        },
        None => rsx! {
            div {
                class: "rounded-md border border-border bg-card p-2 {density_class}",
                {body}
            }
        },
    }
}
