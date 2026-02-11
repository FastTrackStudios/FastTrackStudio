//! Node block component for the node graph view.
//!
//! Renders a single processing node as an absolutely positioned div with:
//! - Color-coded rounded rect background (cyan glow when selected)
//! - Header with name and bypass indicator (draggable)
//! - Content area with widget visualization
//! - Port circles on left (inputs) and right (outputs) edges (wire-draggable)

use crate::prelude::*;
use uuid::Uuid;

use super::block_colors::block_type_style;
use super::node_graph::{Node, NodePort, NodeWidget};
use super::signal_flow_grid::{
    AmpCabBlock, CompressorGraphBlock, DriveBlock, EqGraphBlock, GateGraphBlock, LooperBlock,
    ModulationBlock, TimeEffectBlock, TunerBlock,
};

/// Props for a node block.
#[derive(Props, Clone, PartialEq)]
pub(crate) struct NodeBlockProps {
    /// The node data.
    pub node: Node,
    /// X offset (e.g., module position.x for nodes inside modules).
    #[props(default)]
    pub offset_x: f64,
    /// Y offset (e.g., module position.y + title bar height).
    #[props(default)]
    pub offset_y: f64,
    /// Compact mode: no widget visualizations, just colored block with label.
    #[props(default)]
    pub compact: bool,
    /// Whether this node is currently selected.
    #[props(default)]
    pub is_selected: bool,
    /// Callback when the node is clicked (for selection).
    #[props(default)]
    pub on_select: Option<Callback<Uuid>>,
    /// Callback when the header is mousedown'd (for dragging standalone nodes).
    #[props(default)]
    pub on_header_drag_start: Option<EventHandler<MouseEvent>>,
    /// Callback when a port circle is mousedown'd: (port_id, is_output).
    #[props(default)]
    pub on_port_drag_start: Option<Callback<(String, bool)>>,
    /// Callback when a port is hovered: (entity_id, port_id, is_input).
    #[props(default)]
    pub on_port_hover: Option<Callback<(Uuid, String, bool)>>,
    /// Callback when port hover ends.
    #[props(default)]
    pub on_port_hover_end: Option<Callback<()>>,
    /// Callback when the node is double-clicked (opens parameter editor).
    #[props(default)]
    pub on_double_click: Option<Callback<Uuid>>,
    /// Whether a wire draft is currently active (enables port hover highlighting).
    #[props(default)]
    pub wire_draft_active: bool,
    /// Currently hovered port (entity_id, port_id, is_input) for highlighting.
    #[props(default)]
    pub hovered_port: Option<(Uuid, String, bool)>,
}

/// Renders a node block on the canvas.
#[component]
pub(crate) fn NodeBlock(props: NodeBlockProps) -> Element {
    let node = &props.node;
    let x = node.position.x + props.offset_x;
    let y = node.position.y + props.offset_y;
    let w = node.size.width;
    let h = node.size.height;

    let style_str = block_type_style(node.block_type);
    let opacity = if node.bypassed { "0.5" } else { "1.0" };

    // In compact mode, the header IS the entire node
    let header_h = if props.compact { h } else { 28.0 };
    let content_h = if props.compact { 0 } else { (h - 28.0) as u32 };
    let content_w = w as u32;

    let on_header_drag = props.on_header_drag_start.clone();
    let on_select = props.on_select.clone();
    let on_dbl_click = props.on_double_click.clone();
    let node_id = node.id;
    let header_cursor = if props.wire_draft_active {
        "crosshair"
    } else {
        "grab"
    };

    // Selection glow
    let selection_style = if props.is_selected {
        "box-shadow: 0 0 12px 2px rgba(34,211,238,0.5); border-color: #22d3ee !important;"
    } else {
        ""
    };

    let node_class = if node.is_placeholder {
        if props.compact {
            "absolute rounded overflow-hidden border border-dashed transition-shadow duration-150 opacity-50"
        } else {
            "absolute rounded-lg overflow-hidden shadow-md border-2 border-dashed transition-shadow duration-150 opacity-50"
        }
    } else if props.compact {
        "absolute rounded overflow-hidden border transition-shadow duration-150"
    } else {
        "absolute rounded-lg overflow-hidden shadow-md border-2 transition-shadow duration-150"
    };

    let header_class = if props.compact {
        "flex items-center justify-between px-1.5 text-[9px] font-semibold select-none truncate"
    } else {
        "flex items-center justify-between px-2 py-1 text-[11px] font-semibold select-none"
    };

    rsx! {
        div {
            class: "{node_class}",
            style: "left: {x}px; top: {y}px; width: {w}px; height: {h}px; \
                    {style_str} opacity: {opacity}; {selection_style}",
            // Click on node to select (but don't start pan)
            onmousedown: move |evt| {
                evt.stop_propagation();
                if let Some(ref cb) = on_select {
                    cb.call(node_id);
                }
            },
            // Double-click to open parameter editor
            ondblclick: move |evt| {
                evt.stop_propagation();
                if let Some(ref cb) = on_dbl_click {
                    cb.call(node_id);
                }
            },

            // Header — draggable for standalone nodes
            div {
                class: "{header_class}",
                style: "height: {header_h}px; cursor: {header_cursor};",
                onmousedown: {
                    move |evt: MouseEvent| {
                        evt.stop_propagation();
                        if let Some(ref handler) = on_header_drag {
                            handler.call(evt);
                        }
                    }
                },
                span { class: "truncate", "{node.name}" }
                if node.bypassed {
                    span { class: "text-[8px] opacity-50 flex-shrink-0 ml-1", "BYP" }
                }
            }

            // Content area — in compact mode, show nothing (just the header)
            if !props.compact {
                div {
                    class: "flex-1 overflow-hidden",
                    style: "height: {content_h}px;",
                    NodeWidgetContent {
                        widget: node.widget,
                        width: content_w,
                        height: content_h,
                        bypassed: node.bypassed,
                    }
                }
            }

            // Ports — hide in compact mode (too small to be useful)
            if !props.compact {
                // Input ports (left edge)
                for (idx, port) in node.inputs.iter().enumerate() {
                    PortCircle {
                        key: "{port.id}",
                        port: port.clone(),
                        index: idx,
                        total: node.inputs.len(),
                        node_height: h,
                        is_input: true,
                        entity_id: node.id,
                        on_port_drag_start: props.on_port_drag_start.clone(),
                        on_port_hover: props.on_port_hover.clone(),
                        on_port_hover_end: props.on_port_hover_end.clone(),
                        wire_draft_active: props.wire_draft_active,
                        is_hovered: props.hovered_port.as_ref().map_or(false, |(eid, pid, _)| {
                            *eid == node.id && pid == &port.id
                        }),
                    }
                }

                // Output ports (right edge)
                for (idx, port) in node.outputs.iter().enumerate() {
                    PortCircle {
                        key: "{port.id}",
                        port: port.clone(),
                        index: idx,
                        total: node.outputs.len(),
                        node_height: h,
                        node_width: w,
                        is_input: false,
                        entity_id: node.id,
                        on_port_drag_start: props.on_port_drag_start.clone(),
                        on_port_hover: props.on_port_hover.clone(),
                        on_port_hover_end: props.on_port_hover_end.clone(),
                        wire_draft_active: props.wire_draft_active,
                        is_hovered: props.hovered_port.as_ref().map_or(false, |(eid, pid, _)| {
                            *eid == node.id && pid == &port.id
                        }),
                    }
                }
            }
        }
    }
}

/// Props for a port circle.
#[derive(Props, Clone, PartialEq)]
struct PortCircleProps {
    port: NodePort,
    index: usize,
    total: usize,
    node_height: f64,
    #[props(default)]
    node_width: f64,
    is_input: bool,
    entity_id: Uuid,
    #[props(default)]
    on_port_drag_start: Option<Callback<(String, bool)>>,
    #[props(default)]
    on_port_hover: Option<Callback<(Uuid, String, bool)>>,
    #[props(default)]
    on_port_hover_end: Option<Callback<()>>,
    #[props(default)]
    wire_draft_active: bool,
    #[props(default)]
    is_hovered: bool,
}

/// A small circle representing an input or output port on a node edge.
#[component]
fn PortCircle(props: PortCircleProps) -> Element {
    let base_size = 10.0;
    let port_size = if props.is_hovered {
        base_size + 4.0
    } else {
        base_size
    };
    let size_offset = if props.is_hovered { -2.0 } else { 0.0 };

    let spacing = props.node_height / (props.total + 1) as f64;
    let port_y = spacing * (props.index + 1) as f64 - base_size / 2.0 + size_offset;

    let port_x = if props.is_input {
        -base_size / 2.0 + size_offset
    } else {
        props.node_width - base_size / 2.0 + size_offset
    };

    let color = props.port.color.as_deref().unwrap_or("#ffffff");

    let glow = if props.is_hovered {
        "0 0 8px 3px #22d3ee"
    } else if props.wire_draft_active {
        "0 0 4px 1px rgba(255,255,255,0.3)"
    } else {
        "none"
    };

    let on_port_start = props.on_port_drag_start.clone();
    let on_hover = props.on_port_hover.clone();
    let on_hover_end = props.on_port_hover_end.clone();
    let port_id = props.port.id.clone();
    let entity_id = props.entity_id;
    let is_input = props.is_input;
    let wire_active = props.wire_draft_active;

    rsx! {
        div {
            class: "absolute rounded-full border border-white/50 transition-all duration-150",
            style: "left: {port_x}px; top: {port_y}px; \
                    width: {port_size}px; height: {port_size}px; \
                    background-color: {color}; \
                    box-shadow: {glow}; \
                    cursor: crosshair; \
                    pointer-events: auto;",
            title: "{props.port.label}",
            onmousedown: {
                let port_id = port_id.clone();
                move |evt: MouseEvent| {
                    evt.stop_propagation();
                    if let Some(ref cb) = on_port_start {
                        cb.call((port_id.clone(), !is_input));
                    }
                }
            },
            onmouseenter: {
                let port_id = port_id.clone();
                move |_| {
                    if wire_active {
                        if let Some(ref cb) = on_hover {
                            cb.call((entity_id, port_id.clone(), is_input));
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

/// Routes to the appropriate widget for the node's content area.
#[derive(Props, Clone, PartialEq)]
struct NodeWidgetContentProps {
    widget: NodeWidget,
    width: u32,
    height: u32,
    bypassed: bool,
}

#[component]
fn NodeWidgetContent(props: NodeWidgetContentProps) -> Element {
    if props.bypassed {
        return rsx! {
            div { class: "flex items-center justify-center h-full text-xs opacity-30",
                "BYPASSED"
            }
        };
    }

    let has_room = props.width >= 80 && props.height >= 50;

    match props.widget {
        NodeWidget::EqGraph if has_room => rsx! { EqGraphBlock {} },
        NodeWidget::CompressorGraph if has_room => rsx! { CompressorGraphBlock {} },
        NodeWidget::GateGraph if has_room => rsx! { GateGraphBlock {} },
        NodeWidget::AmpCab if has_room => rsx! { AmpCabBlock {} },
        NodeWidget::DelayGraph if has_room => rsx! { TimeEffectBlock { label: "DLY" } },
        NodeWidget::ReverbGraph if has_room => rsx! { TimeEffectBlock { label: "REV" } },
        NodeWidget::ModulationGraph if has_room => rsx! { ModulationBlock {} },
        NodeWidget::DriveGraph if has_room => rsx! { DriveBlock {} },
        NodeWidget::Tuner => rsx! { TunerBlock {} },
        NodeWidget::Looper => rsx! { LooperBlock {} },
        _ => rsx! {},
    }
}
