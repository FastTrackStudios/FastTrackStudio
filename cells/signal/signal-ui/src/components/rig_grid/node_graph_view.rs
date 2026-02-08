//! Node graph view — main canvas component with pan/zoom and full interactions.
//!
//! Features:
//! - Module containers with child node blocks (HTML layer)
//! - SVG bezier wire connections (SVG layer)
//! - Pan by dragging the background
//! - Zoom with scroll wheel (CSS `zoom` for vector crisp rendering)
//! - Module/node dragging with snap-to-grid
//! - Interactive wire creation with validation
//! - Selection system: click to select module/node/wire, Delete to remove
//! - Right-click context menus for modules/nodes (bypass, delete, duplicate)
//! - Minimap navigation overlay
//! - Keyboard shortcuts: Delete, Escape, F (fit), G (snap), B (bypass)
//!
//! ## Graph Storage
//!
//! The graph lives in `RIG_NODE_GRAPH` (GlobalSignal). This allows the module
//! browser, rig layout, and other components to share the same graph state.

use crate::prelude::*;
use crate::signals::{GRAPH_HISTORY, RIG_NODE_GRAPH};
use dioxus::prelude::dioxus_elements::geometry::WheelDelta;
use uuid::Uuid;

use super::block_colors::block_type_color;
use super::node_graph::{NodeGraph, NodePosition};
use super::node_graph_module::ModuleContainer;
use super::node_graph_node::NodeBlock;
use super::node_graph_wire::{resolve_all_wires, wire_path_d, WirePath};

// ── Selection ────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
enum Selection {
    None,
    Module(Uuid),
    Node(Uuid),
    Wire(Uuid),
}

// ── Drag ─────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
enum DragMode {
    None,
    Pan {
        start_mouse_x: f64,
        start_mouse_y: f64,
        start_pan_x: f64,
        start_pan_y: f64,
    },
    Module {
        module_id: Uuid,
        start_mouse_x: f64,
        start_mouse_y: f64,
        start_module_x: f64,
        start_module_y: f64,
    },
    Node {
        node_id: Uuid,
        start_mouse_x: f64,
        start_mouse_y: f64,
        start_node_x: f64,
        start_node_y: f64,
    },
}

// ── Wire Draft ───────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
struct WireDraft {
    from_entity: Uuid,
    from_port: String,
    from_pos: NodePosition,
    is_from_output: bool,
    mouse_pos: NodePosition,
}

// ── Context Menu ─────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
struct ContextMenu {
    /// Screen position for the menu popup.
    x: f64,
    y: f64,
    /// What was right-clicked.
    target: ContextMenuTarget,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ContextMenuTarget {
    Module(Uuid),
    Node(Uuid),
    Canvas,
}

// ── Snap to Grid ─────────────────────────────────────────────────────

const GRID_SNAP: f64 = 20.0;

fn snap_to_grid(val: f64) -> f64 {
    (val / GRID_SNAP).round() * GRID_SNAP
}

// ── Props ────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct NodeGraphViewProps {
    #[props(default)]
    pub compact: bool,
}

// ── Component ────────────────────────────────────────────────────────

#[component]
pub fn NodeGraphView(props: NodeGraphViewProps) -> Element {
    // ── Viewport state ───────────────────────────────────────────
    let mut pan_x = use_signal(|| 0.0f64);
    let mut pan_y = use_signal(|| 0.0f64);
    let mut zoom = use_signal(|| 1.0f64);
    let mut has_fitted = use_signal(|| false);

    // ── Interaction state ────────────────────────────────────────
    let mut drag_mode = use_signal(|| DragMode::None);
    let mut wire_draft = use_signal(|| Option::<WireDraft>::None);
    let mut hovered_port = use_signal(|| Option::<(Uuid, String, bool)>::None);
    let mut selection = use_signal(|| Selection::None);
    let mut snap_enabled = use_signal(|| true);
    let mut context_menu = use_signal(|| Option::<ContextMenu>::None);

    let compact = props.compact;

    // ── Read graph from global signal ────────────────────────────
    // Compact mode uses the same full-size graph, just auto-zoomed to fit.
    let current_graph = RIG_NODE_GRAPH.read().clone();
    let wires = resolve_all_wires(&current_graph, false);
    let (canvas_w, canvas_h) = calculate_canvas_bounds(&current_graph);

    // ── Auto-fit on first render or when compact mode changes ────
    // Compact view always auto-fits the full-size graph to fill the viewport.
    let mut last_compact = use_signal(|| compact);
    let should_fit = !has_fitted() || last_compact() != compact;
    if should_fit {
        has_fitted.set(true);
        last_compact.set(compact);
        let vp_w = 1200.0;
        let vp_h = 700.0;
        let (fit_zoom, fit_pan_x, fit_pan_y) = calculate_fit(canvas_w, canvas_h, vp_w, vp_h);
        zoom.set(fit_zoom);
        pan_x.set(fit_pan_x);
        pan_y.set(fit_pan_y);
    }

    let cw = canvas_w;
    let ch = canvas_h;

    let is_dragging_anything = !matches!(drag_mode(), DragMode::None);
    let has_wire_draft = wire_draft().is_some();
    let cursor = if has_wire_draft {
        "crosshair"
    } else if is_dragging_anything {
        "grabbing"
    } else {
        "grab"
    };

    rsx! {
        div {
            class: "relative w-full h-full overflow-hidden select-none",
            style: "background-color: #0a0a0f; \
                    background-image: radial-gradient(circle, #1a1a2e 1px, transparent 1px); \
                    background-size: 20px 20px; \
                    cursor: {cursor};",
            tabindex: "0",

            // ── Keyboard ─────────────────────────────────────
            onkeydown: move |evt| {
                // Close context menu on any key
                context_menu.set(None);

                let key = evt.key();
                let modifiers = evt.modifiers();
                let has_cmd_or_ctrl = modifiers.meta() || modifiers.ctrl();

                // Undo: Cmd+Z / Ctrl+Z (without Shift)
                if has_cmd_or_ctrl && !modifiers.shift() && matches!(&key, Key::Character(c) if c == "z" || c == "Z") {
                    let mut history = GRAPH_HISTORY.write();
                    let mut graph = RIG_NODE_GRAPH.write();
                    history.undo(&mut graph);
                    return;
                }
                // Redo: Cmd+Shift+Z / Ctrl+Y
                if (has_cmd_or_ctrl && modifiers.shift() && matches!(&key, Key::Character(c) if c == "z" || c == "Z"))
                    || (has_cmd_or_ctrl && matches!(&key, Key::Character(c) if c == "y" || c == "Y"))
                {
                    let mut history = GRAPH_HISTORY.write();
                    let mut graph = RIG_NODE_GRAPH.write();
                    history.redo(&mut graph);
                    return;
                }

                match key {
                    Key::Delete | Key::Backspace => {
                        match selection() {
                            Selection::Wire(wire_id) => {
                                RIG_NODE_GRAPH.write().disconnect(wire_id);
                                selection.set(Selection::None);
                            }
                            Selection::Module(module_id) => {
                                RIG_NODE_GRAPH.write().remove_module(module_id);
                                selection.set(Selection::None);
                            }
                            Selection::Node(node_id) => {
                                RIG_NODE_GRAPH.write().remove_node(node_id);
                                selection.set(Selection::None);
                            }
                            Selection::None => {}
                        }
                    }
                    Key::Escape => {
                        selection.set(Selection::None);
                        wire_draft.set(None);
                        hovered_port.set(None);
                    }
                    Key::Character(ref c) if c == "f" || c == "F" => {
                        let g = RIG_NODE_GRAPH.read();
                        let (cw, ch) = calculate_canvas_bounds(&g);
                        let (fit_zoom, fit_pan_x, fit_pan_y) =
                            calculate_fit(cw, ch, 1200.0, 700.0);
                        zoom.set(fit_zoom);
                        pan_x.set(fit_pan_x);
                        pan_y.set(fit_pan_y);
                    }
                    Key::Character(ref c) if c == "g" || c == "G" => {
                        snap_enabled.set(!snap_enabled());
                    }
                    Key::Character(ref c) if c == "b" || c == "B" => {
                        // Toggle bypass on selected module/node
                        match selection() {
                            Selection::Module(module_id) => {
                                RIG_NODE_GRAPH.write().find_module_mut(module_id).map(|m| {
                                    m.bypassed = !m.bypassed;
                                });
                            }
                            Selection::Node(node_id) => {
                                RIG_NODE_GRAPH.write().find_node_mut(node_id).map(|n| {
                                    n.bypassed = !n.bypassed;
                                });
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            },

            // ── Mouse: pan start ─────────────────────────────
            onmousedown: move |evt| {
                evt.prevent_default();
                context_menu.set(None);
                selection.set(Selection::None);
                drag_mode.set(DragMode::Pan {
                    start_mouse_x: evt.client_coordinates().x,
                    start_mouse_y: evt.client_coordinates().y,
                    start_pan_x: pan_x(),
                    start_pan_y: pan_y(),
                });
            },

            // ── Right-click on canvas background ─────────────
            oncontextmenu: move |evt| {
                evt.prevent_default();
                evt.stop_propagation();
                // No context menu for background (could add "paste module" later)
                context_menu.set(None);
            },

            // ── Mouse: move ──────────────────────────────────
            onmousemove: move |evt| {
                let mx = evt.client_coordinates().x;
                let my = evt.client_coordinates().y;
                let current_zoom = zoom();

                match drag_mode() {
                    DragMode::Pan { start_mouse_x, start_mouse_y, start_pan_x, start_pan_y } => {
                        let dx = mx - start_mouse_x;
                        let dy = my - start_mouse_y;
                        pan_x.set(start_pan_x + dx);
                        pan_y.set(start_pan_y + dy);
                    }
                    DragMode::Module { module_id, start_mouse_x, start_mouse_y, start_module_x, start_module_y } => {
                        let dx = (mx - start_mouse_x) / current_zoom;
                        let dy = (my - start_mouse_y) / current_zoom;
                        let mut new_x = start_module_x + dx;
                        let mut new_y = start_module_y + dy;
                        if snap_enabled() {
                            new_x = snap_to_grid(new_x);
                            new_y = snap_to_grid(new_y);
                        }
                        new_x = new_x.max(0.0);
                        new_y = new_y.max(0.0);
                        RIG_NODE_GRAPH.write().find_module_mut(module_id).map(|m| {
                            m.position.x = new_x;
                            m.position.y = new_y;
                        });
                    }
                    DragMode::Node { node_id, start_mouse_x, start_mouse_y, start_node_x, start_node_y } => {
                        let dx = (mx - start_mouse_x) / current_zoom;
                        let dy = (my - start_mouse_y) / current_zoom;
                        let mut new_x = start_node_x + dx;
                        let mut new_y = start_node_y + dy;
                        if snap_enabled() {
                            new_x = snap_to_grid(new_x);
                            new_y = snap_to_grid(new_y);
                        }
                        new_x = new_x.max(0.0);
                        new_y = new_y.max(0.0);
                        RIG_NODE_GRAPH.write().find_node_mut(node_id).map(|n| {
                            n.position.x = new_x;
                            n.position.y = new_y;
                        });
                    }
                    DragMode::None => {}
                }

                // Update wire draft endpoint
                if let Some(mut draft) = wire_draft() {
                    let canvas_x = (mx - pan_x()) / current_zoom;
                    let canvas_y = (my - pan_y()) / current_zoom;
                    draft.mouse_pos = NodePosition::new(canvas_x, canvas_y);
                    wire_draft.set(Some(draft));
                }
            },

            // ── Mouse: up ────────────────────────────────────
            onmouseup: move |_| {
                drag_mode.set(DragMode::None);

                if let Some(draft) = wire_draft() {
                    if let Some((target_entity, target_port, target_is_input)) = hovered_port() {
                        if draft.is_from_output && target_is_input {
                            RIG_NODE_GRAPH.write().try_connect(
                                draft.from_entity,
                                &draft.from_port,
                                target_entity,
                                &target_port,
                            );
                        } else if !draft.is_from_output && !target_is_input {
                            RIG_NODE_GRAPH.write().try_connect(
                                target_entity,
                                &target_port,
                                draft.from_entity,
                                &draft.from_port,
                            );
                        }
                    }
                    wire_draft.set(None);
                    hovered_port.set(None);
                }
            },

            onmouseleave: move |_| {
                drag_mode.set(DragMode::None);
                wire_draft.set(None);
                hovered_port.set(None);
            },

            // ── Zoom ─────────────────────────────────────────
            onwheel: move |evt| {
                let delta = evt.delta();
                let dy = match delta {
                    WheelDelta::Pixels(p) => p.y,
                    WheelDelta::Lines(l) => l.y * 40.0,
                    WheelDelta::Pages(p) => p.y * 400.0,
                };
                let zoom_factor = if dy < 0.0 { 1.08 } else { 1.0 / 1.08 };
                let new_zoom = (zoom() * zoom_factor).clamp(0.1, 3.0);
                zoom.set(new_zoom);
            },

            // ── Canvas Layer ─────────────────────────────────
            div {
                style: "position: absolute; left: {pan_x()}px; top: {pan_y()}px; \
                        zoom: {zoom()};",

                // HTML layer: modules and standalone nodes
                div {
                    style: "position: relative; \
                            width: {canvas_w}px; height: {canvas_h}px;",

                    for module in &current_graph.modules {
                        ModuleContainer {
                            key: "{module.id}",
                            module: module.clone(),
                            is_selected: matches!(selection(), Selection::Module(id) if id == module.id),
                            on_select: {
                                let module_id = module.id;
                                move |_: Uuid| {
                                    context_menu.set(None);
                                    selection.set(Selection::Module(module_id));
                                }
                            },
                            on_title_drag_start: {
                                let module_id = module.id;
                                let module_x = module.position.x;
                                let module_y = module.position.y;
                                move |evt: MouseEvent| {
                                    if compact { return; } // No drag in compact mode
                                    evt.stop_propagation();
                                    context_menu.set(None);
                                    selection.set(Selection::Module(module_id));
                                    drag_mode.set(DragMode::Module {
                                        module_id,
                                        start_mouse_x: evt.client_coordinates().x,
                                        start_mouse_y: evt.client_coordinates().y,
                                        start_module_x: module_x,
                                        start_module_y: module_y,
                                    });
                                }
                            },
                            on_context_menu: {
                                let module_id = module.id;
                                move |evt: MouseEvent| {
                                    if compact { return; }
                                    selection.set(Selection::Module(module_id));
                                    context_menu.set(Some(ContextMenu {
                                        x: evt.client_coordinates().x,
                                        y: evt.client_coordinates().y,
                                        target: ContextMenuTarget::Module(module_id),
                                    }));
                                }
                            },
                            on_port_drag_start: {
                                let module_clone = module.clone();
                                move |(port_id, is_output): (String, bool)| {
                                    if compact { return; }
                                    let is_input = !is_output;
                                    if let Some(pos) = module_clone.port_position(&port_id, is_input) {
                                        wire_draft.set(Some(WireDraft {
                                            from_entity: module_clone.id,
                                            from_port: port_id,
                                            from_pos: pos,
                                            is_from_output: is_output,
                                            mouse_pos: pos,
                                        }));
                                    }
                                }
                            },
                            on_port_hover: {
                                move |(entity_id, port_id, is_input): (Uuid, String, bool)| {
                                    if compact { return; }
                                    if wire_draft().is_some() {
                                        hovered_port.set(Some((entity_id, port_id, is_input)));
                                    }
                                }
                            },
                            on_port_hover_end: {
                                move |_: ()| {
                                    hovered_port.set(None);
                                }
                            },
                            wire_draft_active: has_wire_draft && !compact,
                            hovered_port: hovered_port(),
                        }
                    }

                    for node in &current_graph.nodes {
                        NodeBlock {
                            key: "{node.id}",
                            node: node.clone(),
                            is_selected: matches!(selection(), Selection::Node(id) if id == node.id),
                            on_select: {
                                let node_id = node.id;
                                move |_: Uuid| {
                                    context_menu.set(None);
                                    selection.set(Selection::Node(node_id));
                                }
                            },
                            on_header_drag_start: {
                                let node_id = node.id;
                                let node_x = node.position.x;
                                let node_y = node.position.y;
                                move |evt: MouseEvent| {
                                    evt.stop_propagation();
                                    context_menu.set(None);
                                    selection.set(Selection::Node(node_id));
                                    drag_mode.set(DragMode::Node {
                                        node_id,
                                        start_mouse_x: evt.client_coordinates().x,
                                        start_mouse_y: evt.client_coordinates().y,
                                        start_node_x: node_x,
                                        start_node_y: node_y,
                                    });
                                }
                            },
                            on_port_drag_start: {
                                let node_clone = node.clone();
                                move |(port_id, is_output): (String, bool)| {
                                    let is_input = !is_output;
                                    if let Some(pos) = node_clone.port_position(&port_id, is_input) {
                                        wire_draft.set(Some(WireDraft {
                                            from_entity: node_clone.id,
                                            from_port: port_id,
                                            from_pos: pos,
                                            is_from_output: is_output,
                                            mouse_pos: pos,
                                        }));
                                    }
                                }
                            },
                            on_port_hover: {
                                move |(entity_id, port_id, is_input): (Uuid, String, bool)| {
                                    if wire_draft().is_some() {
                                        hovered_port.set(Some((entity_id, port_id, is_input)));
                                    }
                                }
                            },
                            on_port_hover_end: {
                                move |_: ()| {
                                    hovered_port.set(None);
                                }
                            },
                            wire_draft_active: has_wire_draft,
                            hovered_port: hovered_port(),
                        }
                    }
                }

                // SVG wire layer
                svg {
                    style: "position: absolute; top: 0; left: 0; \
                            pointer-events: none; overflow: visible;",
                    width: "{canvas_w}",
                    height: "{canvas_h}",

                    for wire in &wires {
                        WirePath {
                            key: "{wire.wire_id}",
                            from: wire.from,
                            to: wire.to,
                            color: wire.color.clone(),
                            wire_id: wire.wire_id,
                            is_selected: matches!(selection(), Selection::Wire(id) if id == wire.wire_id),
                            on_click: {
                                move |wire_id: Uuid| {
                                    context_menu.set(None);
                                    selection.set(Selection::Wire(wire_id));
                                }
                            },
                        }
                    }

                    // Draft wire
                    if let Some(draft) = wire_draft() {
                        {
                            let (from, to) = if draft.is_from_output {
                                (draft.from_pos, draft.mouse_pos)
                            } else {
                                (draft.mouse_pos, draft.from_pos)
                            };
                            let d = wire_path_d(&from, &to);
                            let draft_color = if hovered_port().is_some() {
                                "#22d3ee"
                            } else {
                                "#ffffff"
                            };
                            rsx! {
                                path {
                                    d: "{d}",
                                    fill: "none",
                                    stroke: "{draft_color}",
                                    stroke_width: "2.5",
                                    stroke_opacity: "0.8",
                                    stroke_linecap: "round",
                                    stroke_dasharray: "8 4",
                                }
                            }
                        }
                    }
                }
            }

            // ── Context Menu Popup ───────────────────────────
            if let Some(menu) = context_menu() {
                ContextMenuPopup {
                    menu: menu.clone(),
                    on_close: move |_: ()| context_menu.set(None),
                    on_bypass: move |id: Uuid| {
                        context_menu.set(None);
                        match menu.target {
                            ContextMenuTarget::Module(_) => {
                                RIG_NODE_GRAPH.write().find_module_mut(id).map(|m| {
                                    m.bypassed = !m.bypassed;
                                });
                            }
                            ContextMenuTarget::Node(_) => {
                                RIG_NODE_GRAPH.write().find_node_mut(id).map(|n| {
                                    n.bypassed = !n.bypassed;
                                });
                            }
                            _ => {}
                        }
                    },
                    on_delete: move |id: Uuid| {
                        context_menu.set(None);
                        selection.set(Selection::None);
                        match menu.target {
                            ContextMenuTarget::Module(_) => {
                                RIG_NODE_GRAPH.write().remove_module(id);
                            }
                            ContextMenuTarget::Node(_) => {
                                RIG_NODE_GRAPH.write().remove_node(id);
                            }
                            _ => {}
                        }
                    },
                    on_duplicate: move |id: Uuid| {
                        context_menu.set(None);
                        match menu.target {
                            ContextMenuTarget::Module(_) => {
                                let graph = RIG_NODE_GRAPH.read();
                                if let Some(original) = graph.find_module(id) {
                                    let mut dup = original.clone();
                                    dup.id = Uuid::new_v4();
                                    // Offset position so it doesn't overlap
                                    dup.position.x += 40.0;
                                    dup.position.y += 40.0;
                                    // Give new IDs to all internal nodes and wires
                                    for node in &mut dup.nodes {
                                        node.id = Uuid::new_v4();
                                    }
                                    dup.internal_wires.clear(); // Wires reference old node IDs
                                    drop(graph);
                                    let new_id = RIG_NODE_GRAPH.write().add_module(dup);
                                    selection.set(Selection::Module(new_id));
                                }
                            }
                            _ => {}
                        }
                    },
                }
            }

            // ── Minimap (bottom-left) ────────────────────────
            if !compact {
                Minimap {
                    graph: current_graph.clone(),
                    canvas_w: canvas_w,
                    canvas_h: canvas_h,
                    pan_x: pan_x(),
                    pan_y: pan_y(),
                    zoom: zoom(),
                    on_pan: move |(new_px, new_py): (f64, f64)| {
                        pan_x.set(new_px);
                        pan_y.set(new_py);
                    },
                }
            }

            // ── Controls overlay (bottom-right) ──────────────
            if !compact {
                div {
                    class: "absolute bottom-4 right-4 flex items-center gap-2 select-none",
                    onmousedown: move |evt| evt.stop_propagation(),

                    button {
                        class: "px-3 py-1.5 rounded-lg text-xs font-medium transition-colors",
                        style: if snap_enabled() {
                            "background-color: rgba(34,211,238,0.2); color: #22d3ee; backdrop-filter: blur(8px);"
                        } else {
                            "background-color: rgba(0,0,0,0.6); color: #a1a1aa; backdrop-filter: blur(8px);"
                        },
                        title: "Toggle snap to grid (G)",
                        onclick: move |_| snap_enabled.set(!snap_enabled()),
                        "Grid"
                    }

                    button {
                        class: "px-3 py-1.5 rounded-lg text-xs font-medium \
                                text-zinc-300 hover:text-white transition-colors",
                        style: "background-color: rgba(0,0,0,0.6); backdrop-filter: blur(8px);",
                        title: "Fit all modules in view (F)",
                        onclick: move |_| {
                            let (fit_zoom, fit_pan_x, fit_pan_y) =
                                calculate_fit(cw, ch, 1200.0, 700.0);
                            zoom.set(fit_zoom);
                            pan_x.set(fit_pan_x);
                            pan_y.set(fit_pan_y);
                        },
                        "Fit"
                    }

                    button {
                        class: "px-2 py-1.5 rounded-lg text-xs font-medium \
                                text-zinc-300 hover:text-white transition-colors",
                        style: "background-color: rgba(0,0,0,0.6); backdrop-filter: blur(8px);",
                        onclick: move |_| {
                            zoom.set((zoom() / 1.2).clamp(0.1, 3.0));
                        },
                        "-"
                    }

                    div {
                        class: "px-3 py-1.5 rounded-lg text-xs font-mono text-zinc-400",
                        style: "background-color: rgba(0,0,0,0.6); backdrop-filter: blur(8px); \
                                min-width: 48px; text-align: center;",
                        "{(zoom() * 100.0) as i32}%"
                    }

                    button {
                        class: "px-2 py-1.5 rounded-lg text-xs font-medium \
                                text-zinc-300 hover:text-white transition-colors",
                        style: "background-color: rgba(0,0,0,0.6); backdrop-filter: blur(8px);",
                        onclick: move |_| {
                            zoom.set((zoom() * 1.2).clamp(0.1, 3.0));
                        },
                        "+"
                    }
                }
            }

            // ── Selection info bar ───────────────────────────
            {
                let info_text = match selection() {
                    Selection::Module(id) => {
                        current_graph.find_module(id)
                            .map(|m| {
                                let bypass_status = if m.bypassed { " [BYPASSED]" } else { "" };
                                format!("{}{} — Del: remove, B: bypass, Esc: deselect", m.name, bypass_status)
                            })
                            .unwrap_or_default()
                    }
                    Selection::Node(id) => {
                        current_graph.find_node(id)
                            .map(|n| {
                                let bypass_status = if n.bypassed { " [BYPASSED]" } else { "" };
                                format!("{}{} — Del: remove, B: bypass, Esc: deselect", n.name, bypass_status)
                            })
                            .unwrap_or_default()
                    }
                    Selection::Wire(_) => "Wire — Del: remove, Esc: deselect".to_string(),
                    Selection::None => String::new(),
                };
                if !info_text.is_empty() {
                    rsx! {
                        div {
                            class: "absolute top-4 left-1/2 -translate-x-1/2 px-4 py-2 rounded-lg \
                                    text-xs text-zinc-300 select-none pointer-events-none",
                            style: "background-color: rgba(0,0,0,0.7); backdrop-filter: blur(8px);",
                            "{info_text}"
                        }
                    }
                } else {
                    rsx! {}
                }
            }
        }
    }
}

// ── Context Menu Popup ───────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct ContextMenuPopupProps {
    menu: ContextMenu,
    on_close: Callback<()>,
    on_bypass: Callback<Uuid>,
    on_delete: Callback<Uuid>,
    on_duplicate: Callback<Uuid>,
}

#[component]
fn ContextMenuPopup(props: ContextMenuPopupProps) -> Element {
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

// ── Minimap Component ────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct MinimapProps {
    graph: NodeGraph,
    canvas_w: f64,
    canvas_h: f64,
    pan_x: f64,
    pan_y: f64,
    zoom: f64,
    on_pan: Callback<(f64, f64)>,
}

#[component]
fn Minimap(props: MinimapProps) -> Element {
    let minimap_w = 180.0;
    let minimap_h = 120.0;

    if props.canvas_w <= 0.0 || props.canvas_h <= 0.0 {
        return rsx! {};
    }

    let scale_x = minimap_w / props.canvas_w;
    let scale_y = minimap_h / props.canvas_h;
    let scale = scale_x.min(scale_y) * 0.9;

    let offset_x = (minimap_w - props.canvas_w * scale) / 2.0;
    let offset_y = (minimap_h - props.canvas_h * scale) / 2.0;

    let vp_x = offset_x + (-props.pan_x / props.zoom) * scale;
    let vp_y = offset_y + (-props.pan_y / props.zoom) * scale;
    let vp_w = (1200.0 / props.zoom) * scale;
    let vp_h = (700.0 / props.zoom) * scale;

    let on_pan = props.on_pan.clone();
    let current_zoom = props.zoom;

    rsx! {
        div {
            class: "absolute bottom-4 left-4 rounded-lg overflow-hidden select-none",
            style: "width: {minimap_w}px; height: {minimap_h}px; \
                    background-color: rgba(0,0,0,0.75); \
                    border: 1px solid rgba(255,255,255,0.1); \
                    backdrop-filter: blur(4px);",
            onmousedown: move |evt| {
                evt.stop_propagation();
                let rect_x = evt.element_coordinates().x;
                let rect_y = evt.element_coordinates().y;
                let canvas_x = (rect_x - offset_x) / scale;
                let canvas_y = (rect_y - offset_y) / scale;
                let new_pan_x = -(canvas_x - 600.0 / current_zoom) * current_zoom;
                let new_pan_y = -(canvas_y - 350.0 / current_zoom) * current_zoom;
                on_pan.call((new_pan_x, new_pan_y));
            },

            for module in &props.graph.modules {
                {
                    let mx = offset_x + module.position.x * scale;
                    let my = offset_y + module.position.y * scale;
                    let mw = module.size.width * scale;
                    let mh = module.size.height * scale;
                    let color = block_type_color(module.block_type);
                    rsx! {
                        div {
                            key: "mm-{module.id}",
                            class: "absolute rounded-sm",
                            style: "left: {mx}px; top: {my}px; \
                                    width: {mw}px; height: {mh}px; \
                                    background-color: {color.bg}60; \
                                    border: 1px solid {color.bg}80;",
                        }
                    }
                }
            }

            div {
                class: "absolute rounded-sm",
                style: "left: {vp_x}px; top: {vp_y}px; \
                        width: {vp_w}px; height: {vp_h}px; \
                        border: 1.5px solid rgba(255,255,255,0.5); \
                        background-color: rgba(255,255,255,0.05);",
            }
        }
    }
}

// ── Compact Graph Projection ────────────────────────────────────────

// ── Helpers ──────────────────────────────────────────────────────────

fn calculate_canvas_bounds(graph: &NodeGraph) -> (f64, f64) {
    let mut max_x = 0.0f64;
    let mut max_y = 0.0f64;

    for module in &graph.modules {
        max_x = max_x.max(module.position.x + module.size.width);
        max_y = max_y.max(module.position.y + module.size.height);
    }

    for node in &graph.nodes {
        max_x = max_x.max(node.position.x + node.size.width);
        max_y = max_y.max(node.position.y + node.size.height);
    }

    (max_x + 100.0, max_y + 100.0)
}

fn calculate_fit(
    canvas_w: f64,
    canvas_h: f64,
    viewport_w: f64,
    viewport_h: f64,
) -> (f64, f64, f64) {
    if canvas_w <= 0.0 || canvas_h <= 0.0 {
        return (1.0, 0.0, 0.0);
    }

    let padding = 20.0;

    let available_w = viewport_w - padding * 2.0;
    let available_h = viewport_h - padding * 2.0;

    let zoom_x = available_w / canvas_w;
    let zoom_y = available_h / canvas_h;
    let fit_zoom = zoom_x.min(zoom_y).clamp(0.1, 2.0);

    let scaled_w = canvas_w * fit_zoom;
    let scaled_h = canvas_h * fit_zoom;
    let pan_x = (viewport_w - scaled_w) / 2.0;
    let pan_y = (viewport_h - scaled_h) / 2.0;

    (fit_zoom, pan_x, pan_y)
}
