//! Node graph view -- main canvas component with pan/zoom and full interactions.
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

mod context_menu;
mod drag_handler;
mod minimap;
mod performance_layout;
mod wire_renderer;

use crate::prelude::*;
use crate::signals::{SelectedEntity, RIG_NODE_GRAPH, RIG_SELECTED_ENTITY};
use dioxus::prelude::dioxus_elements::geometry::WheelDelta;
use serde_json::Value;
use tokio::time::{sleep, Duration};
use uuid::Uuid;

use super::node_graph::{NodeGraph, NodePosition};
use super::node_graph_module::ModuleContainer;
use super::node_graph_node::NodeBlock;
use super::node_graph_wire::resolve_all_wires;

use context_menu::{ContextMenu, ContextMenuPopup, ContextMenuTarget};
use drag_handler::{
    calculate_canvas_bounds, calculate_fit, snap_to_grid, CanvasViewMode, DragMode, Selection,
};
use minimap::Minimap;
use performance_layout::build_performance_graph;
use wire_renderer::{WireDraft, WireLayer};

// ── Props ────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct NodeGraphViewProps {
    #[props(default)]
    pub compact: bool,
}

// ── Component ────────────────────────────────────────────────────────

#[component]
pub fn NodeGraphView(props: NodeGraphViewProps) -> Element {
    const ROOT_ID: &str = "rig-node-graph-root";

    // ── Viewport state ───────────────────────────────────────────
    let mut pan_x = use_signal(|| 0.0f64);
    let mut pan_y = use_signal(|| 0.0f64);
    let mut zoom = use_signal(|| 1.0f64);
    let mut has_fitted = use_signal(|| false);
    let mut viewport_left = use_signal(|| 0.0f64);
    let mut viewport_top = use_signal(|| 0.0f64);
    let mut viewport_w = use_signal(|| 1200.0f64);
    let mut viewport_h = use_signal(|| 700.0f64);

    // ── Interaction state ────────────────────────────────────────
    let mut drag_mode = use_signal(|| DragMode::None);
    let mut wire_draft = use_signal(|| Option::<WireDraft>::None);
    let mut hovered_port = use_signal(|| Option::<(Uuid, String, bool)>::None);
    let mut selection = use_signal(|| Selection::None);
    let mut snap_enabled = use_signal(|| true);
    let mut context_menu = use_signal(|| Option::<ContextMenu>::None);
    let mut canvas_mode = use_signal(|| {
        if props.compact {
            CanvasViewMode::Performance
        } else {
            CanvasViewMode::Node
        }
    });

    let compact = props.compact;
    let performance_mode = canvas_mode() == CanvasViewMode::Performance;

    // ── Sync local selection -> global signal ────────────────────
    use_effect(move || {
        let entity = match selection() {
            Selection::Module(id) => Some(SelectedEntity::Module(id)),
            Selection::Node(id) => Some(SelectedEntity::Node(id)),
            _ => None,
        };
        *RIG_SELECTED_ENTITY.write() = entity;
    });

    // ── Read graph from global signal ────────────────────────────
    let current_graph = RIG_NODE_GRAPH.read().clone();
    let render_graph = if performance_mode {
        build_performance_graph(
            &current_graph,
            viewport_w().max(320.0),
            viewport_h().max(220.0),
        )
    } else {
        current_graph.clone()
    };
    let wires = resolve_all_wires(&render_graph, performance_mode || compact);
    let (canvas_w, canvas_h) = calculate_canvas_bounds(&render_graph);

    // Keep viewport rect in sync for accurate fit/zoom math inside docked panels.
    use_future(move || async move {
        loop {
            let js = format!(
                r#"(function() {{
                    const el = document.getElementById('{ROOT_ID}');
                    if (!el) return "null";
                    const r = el.getBoundingClientRect();
                    return JSON.stringify({{
                        left: r.left,
                        top: r.top,
                        width: el.clientWidth,
                        height: el.clientHeight
                    }});
                }})();"#
            );

            if let Ok(value) = document::eval(&js).await {
                let raw = value
                    .as_str()
                    .map(|s| s.to_string())
                    .unwrap_or_else(|| value.to_string());
                if raw != "null" && raw != "\"null\"" {
                    if let Ok(parsed) = serde_json::from_str::<Value>(&raw) {
                        if let Some(v) = parsed.get("left").and_then(Value::as_f64) {
                            viewport_left.set(v);
                        }
                        if let Some(v) = parsed.get("top").and_then(Value::as_f64) {
                            viewport_top.set(v);
                        }
                        if let Some(v) = parsed.get("width").and_then(Value::as_f64) {
                            viewport_w.set(v.max(1.0));
                        }
                        if let Some(v) = parsed.get("height").and_then(Value::as_f64) {
                            viewport_h.set(v.max(1.0));
                        }
                    }
                }
            }

            sleep(Duration::from_millis(250)).await;
        }
    });

    // ── Auto-fit on first render or when compact mode changes ────
    let mut last_compact = use_signal(|| compact);
    let mut last_mode = use_signal(|| canvas_mode());
    let mut last_compact_viewport = use_signal(|| (0.0f64, 0.0f64));
    let viewport_changed = {
        let (last_w, last_h) = last_compact_viewport();
        (last_w - viewport_w()).abs() > 1.0 || (last_h - viewport_h()).abs() > 1.0
    };
    let should_fit = !has_fitted()
        || last_compact() != compact
        || last_mode() != canvas_mode()
        || (compact && viewport_changed);
    if should_fit {
        has_fitted.set(true);
        last_compact.set(compact);
        last_mode.set(canvas_mode());
        last_compact_viewport.set((viewport_w(), viewport_h()));
        let vp_w = viewport_w();
        let vp_h = viewport_h();
        if compact {
            let (fit_zoom, fit_pan_x, fit_pan_y) = calculate_fit(canvas_w, canvas_h, vp_w, vp_h);
            zoom.set(fit_zoom);
            pan_x.set(fit_pan_x);
            pan_y.set(fit_pan_y);
        } else if performance_mode {
            zoom.set(1.0);
            pan_x.set(0.0);
            pan_y.set(0.0);
        }
    }

    let cw = canvas_w;
    let ch = canvas_h;

    let is_dragging_anything = !matches!(drag_mode(), DragMode::None);
    let has_wire_draft = wire_draft().is_some();
    let cursor = if performance_mode {
        "default"
    } else if has_wire_draft {
        "crosshair"
    } else if is_dragging_anything {
        "grabbing"
    } else {
        "grab"
    };

    // Pre-compute selected wire id for the wire layer
    let selected_wire_id = match selection() {
        Selection::Wire(id) => Some(id),
        _ => None,
    };

    rsx! {
        div {
            id: "{ROOT_ID}",
            class: if performance_mode {
                "relative w-full h-full overflow-y-auto overflow-x-hidden select-none"
            } else {
                "relative w-full h-full overflow-hidden select-none"
            },
            style: "background-color: #0a0a0f; \
                    background-image: radial-gradient(circle, #1a1a2e 1px, transparent 1px); \
                    background-size: 20px 20px; \
                    cursor: {cursor};",
            tabindex: "0",

            // ── Keyboard ─────────────────────────────────────
            onkeydown: move |evt| {
                context_menu.set(None);
                let key = evt.key();
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
                        if performance_mode { return; }
                        let g = RIG_NODE_GRAPH.read();
                        let (cw, ch) = calculate_canvas_bounds(&g);
                        let (fit_zoom, fit_pan_x, fit_pan_y) =
                            calculate_fit(cw, ch, viewport_w(), viewport_h());
                        zoom.set(fit_zoom);
                        pan_x.set(fit_pan_x);
                        pan_y.set(fit_pan_y);
                    }
                    Key::Character(ref c) if c == "g" || c == "G" => {
                        snap_enabled.set(!snap_enabled());
                    }
                    Key::Character(ref c) if c == "b" || c == "B" => {
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
                if performance_mode { return; }
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
                    let canvas_x = (mx - viewport_left() - pan_x()) / current_zoom;
                    let canvas_y = (my - viewport_top() - pan_y()) / current_zoom;
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
                if performance_mode { return; }
                evt.prevent_default();
                let delta = evt.delta();
                let dy = match delta {
                    WheelDelta::Pixels(p) => p.y,
                    WheelDelta::Lines(l) => l.y * 40.0,
                    WheelDelta::Pages(p) => p.y * 400.0,
                };
                let old_zoom = zoom();
                let zoom_factor = if dy < 0.0 { 1.08 } else { 1.0 / 1.08 };
                let new_zoom = (old_zoom * zoom_factor).clamp(0.1, 3.0);

                let local_x = evt.client_coordinates().x - viewport_left();
                let local_y = evt.client_coordinates().y - viewport_top();
                let canvas_x = (local_x - pan_x()) / old_zoom;
                let canvas_y = (local_y - pan_y()) / old_zoom;
                pan_x.set(local_x - canvas_x * new_zoom);
                pan_y.set(local_y - canvas_y * new_zoom);
                zoom.set(new_zoom);
            },

            // ── Canvas Mode Toggles ─────────────────────────
            if !compact {
                div {
                    class: "absolute top-3 right-3 z-30 flex gap-2 select-none",
                    onmousedown: move |evt| evt.stop_propagation(),

                    button {
                        class: if canvas_mode() == CanvasViewMode::Node {
                            "px-3 py-1.5 rounded-lg text-xs font-medium bg-blue-600 text-white"
                        } else {
                            "px-3 py-1.5 rounded-lg text-xs font-medium text-zinc-300 hover:text-white"
                        },
                        style: if canvas_mode() == CanvasViewMode::Node {
                            "backdrop-filter: blur(8px);"
                        } else {
                            "background-color: rgba(0,0,0,0.6); backdrop-filter: blur(8px);"
                        },
                        onclick: move |_| canvas_mode.set(CanvasViewMode::Node),
                        "Node View"
                    }

                    button {
                        class: if canvas_mode() == CanvasViewMode::Performance {
                            "px-3 py-1.5 rounded-lg text-xs font-medium bg-blue-600 text-white"
                        } else {
                            "px-3 py-1.5 rounded-lg text-xs font-medium text-zinc-300 hover:text-white"
                        },
                        style: if canvas_mode() == CanvasViewMode::Performance {
                            "backdrop-filter: blur(8px);"
                        } else {
                            "background-color: rgba(0,0,0,0.6); backdrop-filter: blur(8px);"
                        },
                        onclick: move |_| canvas_mode.set(CanvasViewMode::Performance),
                        "Performance View"
                    }
                }
            }

            // ── Canvas Layer ─────────────────────────────────
            div {
                style: if performance_mode {
                    "position: relative; left: 0px; top: 0px; zoom: 1; width: 100%;"
                } else {
                    "position: absolute; left: {pan_x()}px; top: {pan_y()}px; \
                            zoom: {zoom()};"
                },

                // HTML layer: modules and standalone nodes
                div {
                    style: if performance_mode {
                        "position: relative; width: 100%; height: {canvas_h}px;"
                    } else {
                        "position: relative; width: {canvas_w}px; height: {canvas_h}px;"
                    },

                    for module in &render_graph.modules {
                        ModuleContainer {
                            key: "{module.id}",
                            module: module.clone(),
                            performance_mode: performance_mode,
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
                                    if compact || performance_mode { return; }
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
                                    if compact || performance_mode { return; }
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
                                move |evt: crate::callback_types::PortDragStart| {
                                    if compact || performance_mode { return; }
                                    let is_input = !evt.is_output;
                                    if let Some(pos) = module_clone.port_position(&evt.port_name, is_input) {
                                        wire_draft.set(Some(WireDraft {
                                            from_entity: module_clone.id,
                                            from_port: evt.port_name,
                                            from_pos: pos,
                                            is_from_output: evt.is_output,
                                            mouse_pos: pos,
                                        }));
                                    }
                                }
                            },
                            on_port_hover: {
                                move |evt: crate::callback_types::PortHoverEvent| {
                                    if compact || performance_mode { return; }
                                    if wire_draft().is_some() {
                                        hovered_port.set(Some((evt.node_id, evt.port_name, evt.is_hovering)));
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

                    for node in &render_graph.nodes {
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
                                    if performance_mode { return; }
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
                                move |evt: crate::callback_types::PortDragStart| {
                                    if performance_mode { return; }
                                    let is_input = !evt.is_output;
                                    if let Some(pos) = node_clone.port_position(&evt.port_name, is_input) {
                                        wire_draft.set(Some(WireDraft {
                                            from_entity: node_clone.id,
                                            from_port: evt.port_name,
                                            from_pos: pos,
                                            is_from_output: evt.is_output,
                                            mouse_pos: pos,
                                        }));
                                    }
                                }
                            },
                            on_port_hover: {
                                move |evt: crate::callback_types::PortHoverEvent| {
                                    if wire_draft().is_some() {
                                        hovered_port.set(Some((evt.node_id, evt.port_name, evt.is_hovering)));
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

                // SVG wire layer (hidden in performance mode)
                if !performance_mode {
                    WireLayer {
                        canvas_w: canvas_w,
                        canvas_h: canvas_h,
                        wires: wires.clone(),
                        wire_draft: wire_draft(),
                        hovered_port: hovered_port(),
                        selected_wire_id: selected_wire_id,
                        on_wire_click: move |wire_id: Uuid| {
                            context_menu.set(None);
                            selection.set(Selection::Wire(wire_id));
                        },
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
                                    dup.position.x += 40.0;
                                    dup.position.y += 40.0;
                                    for node in &mut dup.nodes {
                                        node.id = Uuid::new_v4();
                                    }
                                    dup.internal_wires.clear();
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
            if !compact && !performance_mode {
                Minimap {
                    graph: render_graph.clone(),
                    canvas_w: canvas_w,
                    canvas_h: canvas_h,
                    viewport_w: viewport_w(),
                    viewport_h: viewport_h(),
                    pan_x: pan_x(),
                    pan_y: pan_y(),
                    zoom: zoom(),
                    on_pan: move |offset: crate::callback_types::PanOffset| {
                        pan_x.set(offset.x);
                        pan_y.set(offset.y);
                    },
                }
            }

            // ── Controls overlay (bottom-right) ──────────────
            if !compact && !performance_mode {
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
                                calculate_fit(cw, ch, viewport_w(), viewport_h());
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
                        render_graph.find_module(id)
                            .map(|m| {
                                let bypass_status = if m.bypassed { " [BYPASSED]" } else { "" };
                                format!("{}{} -- Del: remove, B: bypass, Esc: deselect", m.name, bypass_status)
                            })
                            .unwrap_or_default()
                    }
                    Selection::Node(id) => {
                        render_graph.find_node(id)
                            .map(|n| {
                                let bypass_status = if n.bypassed { " [BYPASSED]" } else { "" };
                                format!("{}{} -- Del: remove, B: bypass, Esc: deselect", n.name, bypass_status)
                            })
                            .unwrap_or_default()
                    }
                    Selection::Wire(_) => "Wire -- Del: remove, Esc: deselect".to_string(),
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
