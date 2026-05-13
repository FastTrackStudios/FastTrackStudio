//! Live PageRef card. Renders a small clickable card with the
//! target page's title + a short preview pulled from the page's
//! top-level Blocks. Like `BlockRefCard` this is a *reference* —
//! the underlying Page can be edited elsewhere and the card stays
//! in sync on the next render.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::FileText;
use knowledge_proto::canvas::CanvasNode;
use knowledge_proto::{Block, Page};
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Props, Clone, PartialEq)]
pub struct PageRefCardProps {
    pub block_id: Uuid,
    pub node: CanvasNode,
    pub target_page_id: Uuid,
    pub pages_by_id: HashMap<Uuid, Page>,
    /// Top-level (parentless) blocks per page id. The renderer joins
    /// the first 2–3 `content` lines for a teaser. Callers populate
    /// this from the parent component's block index.
    pub top_blocks_by_page: HashMap<Uuid, Vec<Block>>,
    pub selected: bool,
    pub on_open_page: EventHandler<Uuid>,
}

#[component]
pub fn PageRefCard(props: PageRefCardProps) -> Element {
    let target = props.pages_by_id.get(&props.target_page_id).cloned();
    let ring = if props.selected {
        "outline outline-2 outline-primary"
    } else {
        ""
    };
    let style = format!(
        "left: {x}px; top: {y}px; width: {w}px; height: {h}px; transform: rotate({rot}deg); transform-origin: center;",
        x = props.node.x,
        y = props.node.y,
        w = props.node.w,
        h = props.node.h,
        rot = props.node.rotation,
    );
    let id = props.target_page_id;

    let preview = props
        .top_blocks_by_page
        .get(&id)
        .map(|bs| {
            bs.iter()
                .take(3)
                .map(|b| b.content.lines().next().unwrap_or("").to_string())
                .collect::<Vec<_>>()
                .join("\n")
        })
        .unwrap_or_default();

    rsx! {
        div {
            class: "absolute rounded-md border border-border bg-card text-card-foreground shadow-sm overflow-hidden flex flex-col cursor-pointer {ring}",
            style: "{style}",
            onclick: move |_| props.on_open_page.call(id),
            div { class: "flex items-center gap-1.5 px-2 py-1 text-xs text-muted-foreground border-b border-border",
                FileText { size: 12 }
                match target.as_ref() {
                    Some(p) => rsx! { span { class: "truncate font-medium text-foreground", "{p.basename}" } },
                    None => rsx! { span { class: "italic", "missing page" } },
                }
            }
            div { class: "flex-1 px-2 py-1.5 overflow-hidden text-xs text-muted-foreground whitespace-pre-wrap break-words",
                "{preview}"
            }
        }
    }
}
