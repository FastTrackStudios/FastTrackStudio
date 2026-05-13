//! `BlockEmbed` — inline single-block reference renderer.
//!
//! Used by chat / task descriptions when a message references a
//! specific block. The block content renders read-only via
//! [`crate::common::render_block_readonly`]; the whole card is
//! clickable and bubbles `on_open(block.id)` to the caller.

use crate::common::render_block_readonly;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::FileText;
use knowledge_proto::Block;
use uuid::Uuid;

#[component]
pub fn BlockEmbed(
    block: Block,
    /// When true, show the source page title as a chip on top of the
    /// card. The caller resolves the title (we don't take a `Page`).
    show_page_breadcrumb: bool,
    /// Optional source-page display title (only shown when
    /// `show_page_breadcrumb` is true).
    #[props(default)]
    page_title: Option<String>,
    on_open: EventHandler<Uuid>,
) -> Element {
    let id = block.id;
    let breadcrumb = if show_page_breadcrumb {
        page_title.clone()
    } else {
        None
    };

    let body = render_block_readonly(&block);

    rsx! {
        div {
            class: "rounded-md border border-border bg-card p-2 cursor-pointer hover:bg-accent/30 transition-colors",
            onclick: move |_| on_open.call(id),
            if let Some(title) = breadcrumb {
                div { class: "mb-1 flex items-center gap-1 text-xs text-muted-foreground",
                    FileText { size: 12 }
                    span { class: "truncate", "{title}" }
                }
            }
            {body}
        }
    }
}
