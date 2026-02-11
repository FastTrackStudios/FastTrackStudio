//! Wire rendering utilities for the node graph view.
//!
//! Provides SVG cubic bezier path generation, wire endpoint resolution,
//! clickable hit areas, and draft wire rendering.

use crate::prelude::*;
use uuid::Uuid;

use super::block_colors::block_type_color;
use super::node_graph::{GraphModule, Node, NodeGraph, NodePosition, Wire};

/// Title bar height in the module container (px) — normal mode.
const MODULE_TITLE_BAR_HEIGHT: f64 = 40.0;
/// Title bar height in the module container (px) — compact mode.
const MODULE_TITLE_BAR_HEIGHT_COMPACT: f64 = 28.0;

/// A wire with resolved absolute canvas coordinates.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct ResolvedWire {
    pub from: NodePosition,
    pub to: NodePosition,
    pub color: String,
    /// The wire's UUID (for selection/deletion).
    pub wire_id: Uuid,
}

/// Generate an SVG path `d` attribute for a cubic bezier wire.
///
/// Creates a smooth curve from `from` to `to` with horizontal tangents.
/// Handles both left-to-right and right-to-left connections with
/// adaptive control point offsets.
pub(crate) fn wire_path_d(from: &NodePosition, to: &NodePosition) -> String {
    let dx = to.x - from.x;
    let abs_dx = dx.abs();
    let abs_dy = (to.y - from.y).abs();

    // Adaptive offset: larger for distant connections, minimum 60px
    let offset = abs_dx.max(abs_dy * 0.5).max(60.0) * 0.45;

    // When going right-to-left (backwards), push control points further out
    let (cp1x, cp2x) = if dx >= 0.0 {
        (from.x + offset, to.x - offset)
    } else {
        // Backwards connection: use larger offsets to create a nice loop
        let back_offset = offset + abs_dy * 0.3;
        (from.x + back_offset, to.x - back_offset)
    };

    format!(
        "M {},{} C {},{} {},{} {},{}",
        from.x, from.y,
        cp1x, from.y,
        cp2x, to.y,
        to.x, to.y
    )
}

/// Resolve the absolute canvas position of a node port inside a module.
fn resolve_node_port_in_module(
    module: &GraphModule,
    node: &Node,
    port_id: &str,
    is_input: bool,
    title_bar_h: f64,
) -> Option<NodePosition> {
    let port_pos = node.port_position(port_id, is_input)?;
    // Node positions are relative to the module content area
    Some(NodePosition::new(
        module.position.x + port_pos.x,
        module.position.y + title_bar_h + port_pos.y,
    ))
}

/// Resolve all wires in a NodeGraph to absolute canvas coordinates.
///
/// Handles:
/// 1. **Inter-module wires** (graph.wires): module-level port positions
/// 2. **Internal wires** (module.internal_wires): node port positions + module offset
///
/// The `compact` flag controls the title bar height used for node offset calculations.
pub(crate) fn resolve_all_wires(graph: &NodeGraph, compact: bool) -> Vec<ResolvedWire> {
    let title_bar_h = if compact {
        MODULE_TITLE_BAR_HEIGHT_COMPACT
    } else {
        MODULE_TITLE_BAR_HEIGHT
    };

    let mut resolved = Vec::new();

    // 1. Resolve inter-module wires
    for wire in &graph.wires {
        let from = resolve_module_or_node_port(graph, wire.from_node, &wire.from_port, false);
        let to = resolve_module_or_node_port(graph, wire.to_node, &wire.to_port, true);

        if let (Some(from_pos), Some(to_pos)) = (from, to) {
            let color = wire_color_for_inter_module(wire, graph);
            resolved.push(ResolvedWire {
                from: from_pos,
                to: to_pos,
                color,
                wire_id: wire.id,
            });
        }
    }

    // 2. Resolve internal wires within each module.
    // Compact mode still renders internal wires; only geometry differs.
    for module in &graph.modules {
        for wire in &module.internal_wires {
            let from_node = module.find_node(wire.from_node);
            let to_node = module.find_node(wire.to_node);

            if let (Some(from_n), Some(to_n)) = (from_node, to_node) {
                let from_pos =
                    resolve_node_port_in_module(module, from_n, &wire.from_port, false, title_bar_h);
                let to_pos =
                    resolve_node_port_in_module(module, to_n, &wire.to_port, true, title_bar_h);

                if let (Some(from_p), Some(to_p)) = (from_pos, to_pos) {
                    let color = wire
                        .color
                        .clone()
                        .unwrap_or_else(|| {
                            block_type_color(from_n.block_type).bg.to_string()
                        });

                    resolved.push(ResolvedWire {
                        from: from_p,
                        to: to_p,
                        color,
                        wire_id: wire.id,
                    });
                }
            }
        }
    }

    resolved
}

/// Resolve port position for an entity that could be a module or standalone node.
fn resolve_module_or_node_port(
    graph: &NodeGraph,
    entity_id: Uuid,
    port_id: &str,
    is_input: bool,
) -> Option<NodePosition> {
    // Check modules first
    if let Some(module) = graph.find_module(entity_id) {
        return module.port_position(port_id, is_input);
    }
    // Check standalone nodes
    if let Some(node) = graph.nodes.iter().find(|n| n.id == entity_id) {
        return node.port_position(port_id, is_input);
    }
    None
}

/// Get wire color for an inter-module wire.
fn wire_color_for_inter_module(wire: &Wire, graph: &NodeGraph) -> String {
    if let Some(ref color) = wire.color {
        return color.clone();
    }
    // Use source module/node block type color
    if let Some(module) = graph.find_module(wire.from_node) {
        return block_type_color(module.block_type).bg.to_string();
    }
    if let Some(node) = graph.nodes.iter().find(|n| n.id == wire.from_node) {
        return block_type_color(node.block_type).bg.to_string();
    }
    "#666666".to_string()
}

/// Props for a single wire path with click detection.
#[derive(Props, Clone, PartialEq)]
pub(crate) struct WirePathProps {
    pub from: NodePosition,
    pub to: NodePosition,
    pub color: String,
    /// Wire UUID for selection.
    #[props(default)]
    pub wire_id: Option<Uuid>,
    /// Whether this wire is currently selected.
    #[props(default)]
    pub is_selected: bool,
    /// Callback when wire is clicked.
    #[props(default)]
    pub on_click: Option<Callback<Uuid>>,
}

/// Renders a single SVG bezier wire path with an invisible hit area for click detection.
#[component]
pub(crate) fn WirePath(props: WirePathProps) -> Element {
    let d = wire_path_d(&props.from, &props.to);

    let (stroke_color, stroke_width, stroke_opacity) = if props.is_selected {
        ("#22d3ee".to_string(), "4.0", "1.0")
    } else {
        (props.color.clone(), "2.5", "0.8")
    };

    let glow_filter = if props.is_selected {
        "drop-shadow(0 0 4px #22d3ee)"
    } else {
        "none"
    };

    let wire_id = props.wire_id;
    let on_click = props.on_click.clone();

    rsx! {
        // Invisible wider hit area for click detection
        if wire_id.is_some() && on_click.is_some() {
            path {
                d: "{d}",
                fill: "none",
                stroke: "transparent",
                stroke_width: "14",
                style: "pointer-events: stroke; cursor: pointer;",
                onclick: move |evt| {
                    evt.stop_propagation();
                    if let (Some(id), Some(ref cb)) = (wire_id, &on_click) {
                        cb.call(id);
                    }
                },
            }
        }

        // Visible wire path
        path {
            d: "{d}",
            fill: "none",
            stroke: "{stroke_color}",
            stroke_width: "{stroke_width}",
            stroke_opacity: "{stroke_opacity}",
            stroke_linecap: "round",
            style: "filter: {glow_filter};",
        }
    }
}
