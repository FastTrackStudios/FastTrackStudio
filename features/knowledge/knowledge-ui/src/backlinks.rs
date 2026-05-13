//! `BacklinksPanel` — list of source blocks referencing the current
//! page, grouped by source page.
//!
//! Dumb component: caller resolves the backlinks (typically by scanning
//! all blocks' `refs_json` for links pointing at the current page) and
//! hands the resulting [`BlockBrief`] list in.

use crate::common::BlockBrief;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::FileText;
use fts_ui::prelude::*;
use std::collections::BTreeMap;
use uuid::Uuid;

#[component]
pub fn BacklinksPanel(backlinks: Vec<BlockBrief>, on_open_block: EventHandler<Uuid>) -> Element {
    if backlinks.is_empty() {
        return rsx! {
            EmptyState {
                message: "No backlinks yet. Reference this page from another page with `[[Page name]]`.".to_string(),
            }
        };
    }

    // Group by source page.
    let mut groups: BTreeMap<String, Vec<BlockBrief>> = BTreeMap::new();
    for bl in backlinks.into_iter() {
        groups.entry(bl.page_basename.clone()).or_default().push(bl);
    }

    rsx! {
        div { class: "flex flex-col gap-2",
            for (page_title, items) in groups.into_iter() {
                BacklinkGroup {
                    key: "{page_title}",
                    page_title: page_title.clone(),
                    items: items,
                    on_open_block: on_open_block,
                }
            }
        }
    }
}

#[component]
fn BacklinkGroup(
    page_title: String,
    items: Vec<BlockBrief>,
    on_open_block: EventHandler<Uuid>,
) -> Element {
    let count = items.len();
    rsx! {
        Card { class: "p-0".to_string(),
            CardHeader { class: "px-3 py-2".to_string(),
                CardTitle { class: "text-sm flex items-center gap-2".to_string(),
                    FileText { size: 14 }
                    "{page_title}"
                    span { class: "ml-auto text-xs text-muted-foreground", "{count}" }
                }
            }
            CardContent { class: "px-2 pb-2 pt-0 space-y-1".to_string(),
                for bl in items.iter().cloned() {
                    BacklinkRow {
                        key: "{bl.id}",
                        block: bl,
                        on_open_block: on_open_block,
                    }
                }
            }
        }
    }
}

#[component]
fn BacklinkRow(block: BlockBrief, on_open_block: EventHandler<Uuid>) -> Element {
    let id = block.id;
    let icon = block_kind_icon(&block.kind);
    rsx! {
        div {
            class: "flex items-start gap-2 rounded-md px-2 py-1.5 cursor-pointer hover:bg-accent/40",
            onclick: move |_| on_open_block.call(id),
            div { class: "mt-0.5 text-muted-foreground [&_svg]:size-3.5", {icon} }
            div { class: "flex-1 min-w-0",
                div { class: "text-sm truncate", "{block.preview}" }
                div { class: "text-xs text-muted-foreground", "{block.kind}" }
            }
        }
    }
}

fn block_kind_icon(kind: &str) -> Element {
    use fts_ui::lucide_dioxus::{Code, Hash, Heading1, ListTodo, Quote};
    match kind {
        "heading" => rsx! { Heading1 { size: 14 } },
        "code" => rsx! { Code { size: 14 } },
        "blockquote" | "callout" => rsx! { Quote { size: 14 } },
        "list_item" => rsx! { ListTodo { size: 14 } },
        _ => rsx! { Hash { size: 14 } },
    }
}
