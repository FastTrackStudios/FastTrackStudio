//! Right-click context menu for modules and nodes.
//!
//! Contains the menu state types, the popup component, and the menu-item helper.

use crate::prelude::*;
use crate::signals::RIG_NODE_GRAPH;
use uuid::Uuid;

// ── Context Menu State ───────────────────────────────────────────────

/// Describes an open context menu: screen position + target entity.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ContextMenu {
    pub x: f64,
    pub y: f64,
    pub target: ContextMenuTarget,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum ContextMenuTarget {
    Module(Uuid),
    Node(Uuid),
    Canvas,
}

// ── Context Menu Popup ───────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub(crate) struct ContextMenuPopupProps {
    pub menu: ContextMenu,
    pub on_close: Callback<()>,
    pub on_bypass: Callback<Uuid>,
    pub on_delete: Callback<Uuid>,
    pub on_duplicate: Callback<Uuid>,
}

#[component]
pub(crate) fn ContextMenuPopup(props: ContextMenuPopupProps) -> Element {
    let x = props.menu.x;
    let y = props.menu.y;

    let entity_id = match &props.menu.target {
        ContextMenuTarget::Module(id) | ContextMenuTarget::Node(id) => *id,
        ContextMenuTarget::Canvas => return rsx! {},
    };

    let is_module = matches!(props.menu.target, ContextMenuTarget::Module(_));

    // Check current bypass state
    let is_bypassed = {
        let graph = RIG_NODE_GRAPH.read();
        match &props.menu.target {
            ContextMenuTarget::Module(id) => graph.find_module(*id).map_or(false, |m| m.bypassed),
            ContextMenuTarget::Node(id) => graph.find_node(*id).map_or(false, |n| n.bypassed),
            _ => false,
        }
    };

    let bypass_label = if is_bypassed { "Enable" } else { "Bypass" };

    let on_close = props.on_close.clone();
    let on_bypass = props.on_bypass.clone();
    let on_delete = props.on_delete.clone();
    let on_duplicate = props.on_duplicate.clone();

    rsx! {
        // Backdrop to close menu
        div {
            class: "fixed inset-0 z-40",
            onclick: move |_| on_close.call(()),
            oncontextmenu: move |evt| {
                evt.prevent_default();
                on_close.call(());
            },
        }

        // Menu popup
        div {
            class: "fixed z-50 py-1 rounded-lg shadow-xl border border-zinc-700 min-w-[160px]",
            style: "left: {x}px; top: {y}px; \
                    background-color: #1c1c2e; \
                    backdrop-filter: blur(12px);",

            // Bypass
            ContextMenuItem {
                label: bypass_label,
                shortcut: "B",
                on_click: move |_| on_bypass.call(entity_id),
            }

            // Duplicate (modules only)
            if is_module {
                ContextMenuItem {
                    label: "Duplicate",
                    shortcut: "",
                    on_click: move |_| on_duplicate.call(entity_id),
                }
            }

            // Separator
            div { class: "my-1 border-t border-zinc-700" }

            // Delete
            ContextMenuItem {
                label: "Delete",
                shortcut: "Del",
                danger: true,
                on_click: move |_| on_delete.call(entity_id),
            }
        }
    }
}

// ── Context Menu Item ────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct ContextMenuItemProps {
    label: &'static str,
    #[props(default)]
    shortcut: &'static str,
    #[props(default)]
    danger: bool,
    on_click: EventHandler<()>,
}

#[component]
fn ContextMenuItem(props: ContextMenuItemProps) -> Element {
    let text_class = if props.danger {
        "text-red-400 hover:text-red-300"
    } else {
        "text-zinc-300 hover:text-white"
    };

    rsx! {
        button {
            class: "w-full flex items-center justify-between px-3 py-1.5 text-xs \
                    hover:bg-zinc-700/50 transition-colors {text_class}",
            onclick: move |_| props.on_click.call(()),
            span { "{props.label}" }
            if !props.shortcut.is_empty() {
                span { class: "text-zinc-500 text-[10px] ml-4", "{props.shortcut}" }
            }
        }
    }
}
