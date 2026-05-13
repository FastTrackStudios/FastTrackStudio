//! Live BlockRef card. Pulls the target Block from a
//! `blocks_by_id` map and renders its `content` read-only. Edits to
//! the source block flow back here on the next render — this is the
//! "reference, not copy" property that distinguishes Logseq-style
//! whiteboards from Obsidian Canvas.
//!
//! Click → emits `on_open_block(target_block_id)` so the route can
//! open the source block for editing in a side sheet.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::ExternalLink;
use knowledge_proto::Block;
use knowledge_proto::canvas::{BlockRefView, CanvasNode};
use std::collections::HashMap;
use uuid::Uuid;

#[derive(Props, Clone, PartialEq)]
pub struct BlockRefCardProps {
    pub block_id: Uuid,
    pub node: CanvasNode,
    pub target_block_id: Uuid,
    pub view: BlockRefView,
    pub blocks_by_id: HashMap<Uuid, Block>,
    pub selected: bool,
    pub on_open_block: EventHandler<Uuid>,
}

#[component]
pub fn BlockRefCard(props: BlockRefCardProps) -> Element {
    let target = props.blocks_by_id.get(&props.target_block_id).cloned();
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

    let id = props.target_block_id;
    rsx! {
        div {
            class: "absolute rounded-md border border-border bg-card text-card-foreground shadow-sm overflow-hidden flex flex-col {ring}",
            style: "{style}",
            div { class: "flex items-center justify-between px-2 py-1 text-xs text-muted-foreground border-b border-border",
                span { "Block ref" }
                button {
                    class: "p-0.5 rounded hover:bg-muted",
                    onclick: move |_| props.on_open_block.call(id),
                    ExternalLink { size: 12 }
                }
            }
            div { class: "flex-1 px-2 py-1.5 overflow-hidden text-sm",
                match (target.as_ref(), props.view) {
                    (None, _) => rsx! {
                        div { class: "italic text-muted-foreground", "missing block" }
                    },
                    (Some(b), BlockRefView::Compact) => rsx! {
                        BlockRefCompact { content: b.content.clone() }
                    },
                    (Some(b), BlockRefView::Full) => rsx! {
                        BlockRefFull { content: b.content.clone() }
                    },
                    (Some(b), BlockRefView::Outline) => rsx! {
                        BlockRefOutline { content: b.content.clone(), target_id: id, blocks_by_id: props.blocks_by_id.clone() }
                    },
                }
            }
        }
    }
}

#[component]
fn BlockRefCompact(content: String) -> Element {
    let first_line = content.lines().next().unwrap_or("").to_string();
    let has_more = content.lines().count() > 1;
    rsx! {
        div { class: "truncate", "{first_line}" }
        if has_more {
            div { class: "text-xs text-muted-foreground mt-0.5", "more…" }
        }
    }
}

#[component]
fn BlockRefFull(content: String) -> Element {
    rsx! {
        div { class: "whitespace-pre-wrap break-words", "{content}" }
    }
}

#[component]
fn BlockRefOutline(
    content: String,
    target_id: Uuid,
    blocks_by_id: HashMap<Uuid, Block>,
) -> Element {
    let children: Vec<Block> = blocks_by_id
        .values()
        .filter(|b| b.parent_block_id == Some(target_id))
        .cloned()
        .collect();
    rsx! {
        div { class: "whitespace-pre-wrap break-words", "{content}" }
        if !children.is_empty() {
            ul { class: "mt-1 pl-4 list-disc text-xs text-muted-foreground",
                for c in children {
                    li { key: "{c.id}", "{c.content}" }
                }
            }
        }
    }
}
