//! Root dock layout component.
//!
//! Reads the global `DOCK_LAYOUT` signal and recursively renders
//! the split/tile tree. Handles maximized panels and global drag state.

use crate::components::split_pane::SplitPane;
use crate::components::tile_pane::TilePane;
use crate::context::use_dock_context;
use crate::prelude::*;
use crate::signals::*;
use dock_proto::{FlatNode, NodeId};

/// Root dock layout component.
#[component]
pub fn DockRoot() -> Element {
    let layout = DOCK_LAYOUT.read();
    let maximized = *DOCK_MAXIMIZED_PANEL.read();
    let is_dragging = DOCK_DRAG_STATE.read().is_dragging();
    let is_resizing = DOCK_RESIZING.read().is_some();

    // Global classes during drag/resize operations
    let drag_class = if is_dragging || is_resizing {
        "select-none"
    } else {
        ""
    };

    // If a panel is maximized, render it full-screen
    if let Some(panel_id) = maximized {
        let ctx = use_dock_context();
        return rsx! {
            div {
                class: "h-full w-full relative",
                {ctx.render_panel.render(panel_id)}
                // Restore button (top-right corner)
                button {
                    class: "absolute top-2 right-2 z-50 px-2.5 py-1 bg-muted/90 text-muted-foreground rounded-md text-xs hover:bg-accent hover:text-foreground transition-colors shadow-lg backdrop-blur-sm",
                    onclick: move |_| {
                        *DOCK_MAXIMIZED_PANEL.write() = None;
                    },
                    "\u{25A3} Restore" // dotted square + text
                }
            }
        };
    }

    // Normal mode: render the tree
    rsx! {
        div {
            class: "h-full w-full {drag_class}",
            if let Some(root_id) = layout.root() {
                DockNodeRenderer { node_id: root_id }
            } else {
                div { class: "h-full w-full flex items-center justify-center text-muted-foreground",
                    "Empty layout — add a panel to get started"
                }
            }
        }
    }
}

/// Recursive node renderer — dispatches to SplitPane or TilePane.
#[component]
fn DockNodeRenderer(node_id: NodeId) -> Element {
    let layout = DOCK_LAYOUT.read();

    match layout.get_node(node_id) {
        Some(FlatNode::Split {
            direction,
            ratio,
            first,
            second,
        }) => {
            let direction = *direction;
            let ratio = *ratio;
            let first = *first;
            let second = *second;
            rsx! {
                SplitPane {
                    node_id,
                    direction,
                    ratio,
                    first: rsx! { DockNodeRenderer { node_id: first } },
                    second: rsx! { DockNodeRenderer { node_id: second } },
                }
            }
        }
        Some(FlatNode::Tile { tabs, .. }) => {
            let tabs = tabs.clone();
            rsx! { TilePane { node_id, tabs } }
        }
        None => rsx! {
            div { class: "h-full w-full bg-background flex items-center justify-center text-muted-foreground",
                "Missing node"
            }
        },
    }
}
