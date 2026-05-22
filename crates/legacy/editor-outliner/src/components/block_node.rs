//! One outline bullet. Static render until clicked, then swaps
//! to [`super::EditableBlock`]. Recursively renders children.
//! Mirrors `frontend.components.block`.

use dioxus::prelude::*;
use uuid::Uuid;

use crate::components::EditableBlock;
use crate::format::{Inlines, parse_inline};
use crate::handler::enter_edit;
use crate::state::AppState;

#[component]
pub fn BlockNode(block_id: Uuid) -> Element {
    let state = use_context::<AppState>();
    let vault = state.vault.read().clone();
    let Some(block) = vault.blocks.iter().find(|b| b.id == block_id).cloned() else {
        return rsx! { Fragment {} };
    };
    let editing = *state.editing_block.read() == Some(block_id);
    let children = vault
        .children(block_id)
        .into_iter()
        .cloned()
        .collect::<Vec<_>>();

    let click_state = state;
    let on_static_click = move |_: Event<MouseData>| {
        enter_edit(click_state, block_id);
    };

    rsx! {
        div { class: "editor-block",
            div { class: "editor-bullet" }
            div { style: "flex: 1; min-width: 0;",
                if editing {
                    EditableBlock { block_id }
                } else {
                    {
                        let is_empty = block.content.trim().is_empty();
                        let cls = if is_empty {
                            "editor-block-body empty"
                        } else {
                            "editor-block-body"
                        };
                        // Static render: parse the raw markdown
                        // through the inline tokenizer and emit
                        // rich elements (wikilinks, bold, code,
                        // tags, …). Mirrors Logseq's
                        // `inline-text` → `map-inline` →
                        // `inline` dispatch path.
                        let nodes = parse_inline(&block.content);
                        rsx! {
                            div { class: "{cls}",
                                onclick: on_static_click,
                                if is_empty {
                                    "—"
                                } else {
                                    Inlines { nodes }
                                }
                            }
                        }
                    }
                }
                if !children.is_empty() {
                    div { class: "editor-block-children",
                        for c in children {
                            BlockNode { key: "{c.id}", block_id: c.id }
                        }
                    }
                }
            }
        }
    }
}
