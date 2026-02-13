//! Module container component for the node graph view.
//!
//! Renders a module as an absolutely positioned container with:
//! - Semi-transparent tinted background (cyan glow when selected)
//! - Title bar with module name, collapse toggle, and bypass badge (draggable)
//! - Port circles on left/right edges (module-level I/O, wire-draggable)
//! - Child NodeBlock components positioned inside
//! - Collapse/expand support: collapsed modules show only title bar + node count badge

use crate::callback_types::{PortDragStart, PortHoverEvent};
use crate::prelude::*;
use uuid::Uuid;

use super::block_colors::block_type_color;
use super::node_graph::GraphModule;
use super::node_graph_node::NodeBlock;

/// Title bar height in pixels (normal mode).
const TITLE_BAR_HEIGHT: f64 = 40.0;
/// Title bar height in pixels (compact mode).
const TITLE_BAR_HEIGHT_COMPACT: f64 = 28.0;
/// Height of a collapsed module (title bar + summary badge area).
const COLLAPSED_HEIGHT: f64 = 56.0;
/// Height of a collapsed module in compact mode.
const COLLAPSED_HEIGHT_COMPACT: f64 = 40.0;

/// Props for a module container.
#[derive(Props, Clone, PartialEq)]
pub(crate) struct ModuleContainerProps {
    /// The module data.
    pub module: GraphModule,
    /// Compact mode: no widget visualizations, just labels.
    #[props(default)]
    pub compact: bool,
    /// Performance mode: reduce chrome and hide module-edge ports while keeping node widgets.
    #[props(default)]
    pub performance_mode: bool,
    /// Whether this module is currently selected.
    #[props(default)]
    pub is_selected: bool,
    /// Callback when the title bar is mousedown'd (for dragging).
    #[props(default)]
    pub on_title_drag_start: Option<EventHandler<MouseEvent>>,
    /// Callback when the module is clicked (for selection).
    #[props(default)]
    pub on_select: Option<Callback<Uuid>>,
    /// Callback when a port circle is mousedown'd.
    #[props(default)]
    pub on_port_drag_start: Option<Callback<PortDragStart>>,
    /// Callback when a port is hovered.
    #[props(default)]
    pub on_port_hover: Option<Callback<PortHoverEvent>>,
    /// Callback when port hover ends.
    #[props(default)]
    pub on_port_hover_end: Option<Callback<()>>,
    /// Callback when the module is right-clicked (for context menu).
    #[props(default)]
    pub on_context_menu: Option<EventHandler<MouseEvent>>,
    /// Callback when an internal node is double-clicked (opens parameter editor).
    #[props(default)]
    pub on_node_double_click: Option<Callback<Uuid>>,
    /// Callback when the module collapse/expand toggle is clicked.
    #[props(default)]
    pub on_toggle_collapse: Option<Callback<Uuid>>,
    /// Whether a wire draft is currently active (enables port hover highlighting).
    #[props(default)]
    pub wire_draft_active: bool,
    /// Currently hovered port (entity_id, port_id, is_input) for highlighting.
    #[props(default)]
    pub hovered_port: Option<(Uuid, String, bool)>,
}

/// Renders a module container on the canvas.
#[component]
pub(crate) fn ModuleContainer(props: ModuleContainerProps) -> Element {
    let module = &props.module;
    let x = module.position.x;
    let y = module.position.y;
    let w = module.size.width;
    let is_collapsed = module.collapsed;

    let color = block_type_color(module.block_type);
    let opacity = if module.bypassed { "0.4" } else { "1.0" };

    // Port circle sizing
    let port_size = 12.0;

    let on_title_drag = props.on_title_drag_start.clone();
    let on_select = props.on_select.clone();
    let on_context_menu = props.on_context_menu.clone();
    let on_toggle_collapse = props.on_toggle_collapse.clone();
    let module_id = module.id;
    let node_count = module.nodes.len();

    // Selection glow
    let selection_border = if props.is_selected {
        "border-color: #22d3ee; box-shadow: 0 0 12px 2px rgba(34,211,238,0.4);"
    } else {
        ""
    };

    let title_cursor = if props.wire_draft_active {
        "crosshair"
    } else {
        "grab"
    };
    let title_height = if props.compact || props.performance_mode {
        TITLE_BAR_HEIGHT_COMPACT
    } else {
        TITLE_BAR_HEIGHT
    };
    let port_size = if props.compact { 8.0 } else { port_size };

    // Compute rendered height: collapsed modules shrink to title bar + badge
    let collapsed_h = if props.compact || props.performance_mode {
        COLLAPSED_HEIGHT_COMPACT
    } else {
        COLLAPSED_HEIGHT
    };
    let h = if is_collapsed {
        collapsed_h
    } else {
        module.size.height
    };

    // Chevron indicator: right-pointing when collapsed, down-pointing when expanded
    let chevron = if is_collapsed { "\u{25B6}" } else { "\u{25BC}" };

    rsx! {
        div {
            class: if props.compact { "absolute rounded-lg overflow-visible" } else { "absolute rounded-xl overflow-visible" },
            style: "left: {x}px; top: {y}px; width: {w}px; height: {h}px; opacity: {opacity}; \
                    transition: height 0.3s ease;",

            // Click on module body to select (but don't start pan)
            onmousedown: move |evt| {
                evt.stop_propagation();
                if let Some(ref cb) = on_select {
                    cb.call(module_id);
                }
            },

            // Right-click context menu
            oncontextmenu: move |evt: MouseEvent| {
                evt.prevent_default();
                evt.stop_propagation();
                if let Some(ref handler) = on_context_menu {
                    handler.call(evt);
                }
            },

            // Background with tinted fill
            div {
                class: if props.compact { "absolute inset-0 rounded-lg border transition-all duration-150" } else { "absolute inset-0 rounded-xl border-2 transition-all duration-150" },
                style: "background-color: {color.bg}15; \
                        border-color: {color.bg}40; \
                        backdrop-filter: blur(4px); \
                        {selection_border}",
            }

            // Title bar -- draggable, with collapse toggle
            div {
                class: "relative flex items-center select-none",
                class: if props.compact { "px-2 rounded-t-lg" } else { "px-3 rounded-t-xl" },
                style: "height: {title_height}px; \
                        background-color: {color.bg}30; \
                        border-bottom: 1px solid {color.bg}40; \
                        cursor: {title_cursor};",
                onmousedown: {
                    let on_title_drag = on_title_drag.clone();
                    move |evt: MouseEvent| {
                        evt.stop_propagation();
                        if let Some(ref handler) = on_title_drag {
                            handler.call(evt);
                        }
                    }
                },

                // Collapse/expand chevron toggle (not in performance mode)
                if !props.performance_mode {
                    button {
                        class: "flex items-center justify-center flex-shrink-0 mr-1.5",
                        style: "width: 18px; height: 18px; \
                                color: {color.fg}90; \
                                cursor: pointer; \
                                font-size: 8px; \
                                border: none; \
                                background: transparent; \
                                transition: transform 0.3s ease;",
                        title: if is_collapsed { "Expand module" } else { "Collapse module" },
                        onmousedown: move |evt: MouseEvent| {
                            // Prevent the title bar drag from starting
                            evt.stop_propagation();
                        },
                        onclick: move |evt| {
                            evt.stop_propagation();
                            if let Some(ref cb) = on_toggle_collapse {
                                cb.call(module_id);
                            }
                        },
                        "{chevron}"
                    }
                }

                // Module name
                span {
                    class: if props.compact { "text-[10px] font-semibold flex-1" } else { "text-sm font-semibold tracking-wide flex-1" },
                    style: "color: {color.fg};",
                    "{module.name}"
                }

                // Right side: bypass badge and collapsed node count badge
                div { class: "flex items-center gap-1",
                    if is_collapsed && node_count > 0 {
                        span {
                            class: "text-[9px] px-1.5 py-0.5 rounded-full font-medium",
                            style: "background-color: {color.bg}40; color: {color.fg};",
                            {
                                let label = if node_count == 1 {
                                    "1 block".to_string()
                                } else {
                                    format!("{node_count} blocks")
                                };
                                label
                            }
                        }
                    }
                    if module.bypassed {
                        span {
                            class: "text-[9px] px-1 py-0.5 rounded",
                            style: "background-color: {color.bg}30; color: {color.fg}80;",
                            "BYP"
                        }
                    }
                }
            }

            // Collapsed summary area (below title bar)
            if is_collapsed && !props.performance_mode {
                div {
                    class: "relative flex items-center justify-center select-none",
                    style: "height: {collapsed_h - title_height}px; \
                            overflow: hidden; \
                            opacity: 0.7;",
                }
            }

            // Child nodes -- hidden when collapsed (positioned relative to module content area)
            if !is_collapsed {
                for node in &module.nodes {
                    NodeBlock {
                        key: "{node.id}",
                        node: node.clone(),
                        offset_x: 0.0,
                        offset_y: title_height,
                        compact: props.compact,
                        on_double_click: props.on_node_double_click.clone(),
                        on_port_drag_start: props.on_port_drag_start.clone(),
                        on_port_hover: props.on_port_hover.clone(),
                        on_port_hover_end: props.on_port_hover_end.clone(),
                        wire_draft_active: props.wire_draft_active,
                        hovered_port: props.hovered_port.clone(),
                    }
                }
            }

            // Module-level input ports (left edge) -- hidden in compact/collapsed mode
            if !props.compact && !props.performance_mode && !is_collapsed {
            for (idx, port) in module.inputs.iter().enumerate() {
                {
                    let spacing = h / (module.inputs.len() + 1) as f64;
                    let port_y = spacing * (idx + 1) as f64 - port_size / 2.0;
                    let port_x = -port_size / 2.0;
                    let port_color = port.color.as_deref().unwrap_or(color.fg);

                    let is_hovered = props.hovered_port.as_ref().map_or(false, |(eid, pid, _)| {
                        *eid == module.id && pid == &port.id
                    });
                    let glow = if is_hovered {
                        "0 0 8px 3px #22d3ee"
                    } else if props.wire_draft_active {
                        "0 0 4px 1px rgba(255,255,255,0.3)"
                    } else {
                        "none"
                    };
                    let highlight_size = if is_hovered { port_size + 4.0 } else { port_size };
                    let highlight_offset = if is_hovered { -2.0 } else { 0.0 };

                    let port_id = port.id.clone();
                    let module_id = module.id;
                    let on_port_start = props.on_port_drag_start.clone();
                    let on_hover = props.on_port_hover.clone();
                    let on_hover_end = props.on_port_hover_end.clone();
                    let wire_active = props.wire_draft_active;

                    rsx! {
                        div {
                            key: "in-{port.id}",
                            class: "absolute rounded-full border-2 transition-all duration-150",
                            style: "left: {port_x + highlight_offset}px; top: {port_y + highlight_offset}px; \
                                    width: {highlight_size}px; height: {highlight_size}px; \
                                    background-color: {port_color}; \
                                    border-color: {color.bg}; \
                                    box-shadow: {glow}; \
                                    cursor: crosshair; \
                                    pointer-events: auto;",
                            title: "{port.label}",
                            onmousedown: {
                                let port_id = port_id.clone();
                                move |evt: MouseEvent| {
                                    evt.stop_propagation();
                                    if let Some(ref cb) = on_port_start {
                                        cb.call(PortDragStart { port_name: port_id.clone(), is_output: false }); // input port
                                    }
                                }
                            },
                            onmouseenter: {
                                let port_id = port_id.clone();
                                move |_| {
                                    if wire_active {
                                        if let Some(ref cb) = on_hover {
                                            cb.call(PortHoverEvent { node_id: module_id, port_name: port_id.clone(), is_hovering: true });
                                        }
                                    }
                                }
                            },
                            onmouseleave: move |_| {
                                if let Some(ref cb) = on_hover_end {
                                    cb.call(());
                                }
                            },
                        }
                    }
                }
            }

            // Module-level output ports (right edge)
            for (idx, port) in module.outputs.iter().enumerate() {
                {
                    let spacing = h / (module.outputs.len() + 1) as f64;
                    let port_y = spacing * (idx + 1) as f64 - port_size / 2.0;
                    let port_x = w - port_size / 2.0;
                    let port_color = port.color.as_deref().unwrap_or(color.fg);

                    let is_hovered = props.hovered_port.as_ref().map_or(false, |(eid, pid, _)| {
                        *eid == module.id && pid == &port.id
                    });
                    let glow = if is_hovered {
                        "0 0 8px 3px #22d3ee"
                    } else if props.wire_draft_active {
                        "0 0 4px 1px rgba(255,255,255,0.3)"
                    } else {
                        "none"
                    };
                    let highlight_size = if is_hovered { port_size + 4.0 } else { port_size };
                    let highlight_offset = if is_hovered { -2.0 } else { 0.0 };

                    let port_id = port.id.clone();
                    let module_id = module.id;
                    let on_port_start = props.on_port_drag_start.clone();
                    let on_hover = props.on_port_hover.clone();
                    let on_hover_end = props.on_port_hover_end.clone();
                    let wire_active = props.wire_draft_active;

                    rsx! {
                        div {
                            key: "out-{port.id}",
                            class: "absolute rounded-full border-2 transition-all duration-150",
                            style: "left: {port_x + highlight_offset}px; top: {port_y + highlight_offset}px; \
                                    width: {highlight_size}px; height: {highlight_size}px; \
                                    background-color: {port_color}; \
                                    border-color: {color.bg}; \
                                    box-shadow: {glow}; \
                                    cursor: crosshair; \
                                    pointer-events: auto;",
                            title: "{port.label}",
                            onmousedown: {
                                let port_id = port_id.clone();
                                move |evt: MouseEvent| {
                                    evt.stop_propagation();
                                    if let Some(ref cb) = on_port_start {
                                        cb.call(PortDragStart { port_name: port_id.clone(), is_output: true }); // output port
                                    }
                                }
                            },
                            onmouseenter: {
                                let port_id = port_id.clone();
                                move |_| {
                                    if wire_active {
                                        if let Some(ref cb) = on_hover {
                                            cb.call(PortHoverEvent { node_id: module_id, port_name: port_id.clone(), is_hovering: false });
                                        }
                                    }
                                }
                            },
                            onmouseleave: move |_| {
                                if let Some(ref cb) = on_hover_end {
                                    cb.call(());
                                }
                            },
                        }
                    }
                }
            }
            } // end if !props.compact && !props.performance_mode && !is_collapsed (ports)
        }
    }
}
