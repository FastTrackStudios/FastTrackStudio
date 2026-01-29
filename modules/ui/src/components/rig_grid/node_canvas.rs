//! Node canvas with pan/zoom interaction.
//!
//! Renders a `NodeGraph` on an SVG canvas with:
//! - Mouse drag to pan
//! - Mouse wheel to zoom (towards cursor)
//! - Drag nodes to reposition them (title bar only)
//! - Double-click title bar for focus mode
//! - Bezier curves for wire routing

use dioxus::prelude::*;
use uuid::Uuid;

use super::block_colors::block_type_color;
use super::node_focus_dialog::NodeFocusDialog;
use super::node_graph::{Module, Node, NodeGraph, NodePosition, NodeSize, NodeWidget, Wire};
use super::performance_view::PerformanceView;

// Import audio-controls widgets
use audio_controls::widgets::{CompressorGraph, CompressorMode, CompressorParams, DbRange, EqBand, EqBandShape, EqGraph};

/// Props for the node canvas.
#[derive(Props, Clone, PartialEq)]
pub struct NodeCanvasProps {
    /// The node graph to render.
    pub graph: Signal<NodeGraph>,
    /// Callback when a node is clicked.
    #[props(default)]
    pub on_node_click: Option<Callback<Uuid>>,
    /// Callback when a node's bypass is toggled.
    #[props(default)]
    pub on_toggle_bypass: Option<Callback<Uuid>>,
    /// Callback when a node is moved.
    #[props(default)]
    pub on_node_move: Option<Callback<(Uuid, NodePosition)>>,
    /// Callback when a node is double-clicked (for focus mode).
    #[props(default)]
    pub on_node_focus: Option<Callback<Uuid>>,
}

/// View mode for the canvas.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CanvasViewMode {
    Node,        // Node-based view with pan/zoom
    Performance, // Performance view with maximized layout
}

/// Node canvas component with pan/zoom and node dragging.
#[component]
pub fn NodeCanvas(props: NodeCanvasProps) -> Element {
    // Copy the graph signal (Signal is Copy)
    let mut graph = props.graph;

    // View mode state - default to Performance view
    let mut view_mode = use_signal(|| CanvasViewMode::Performance);

    // Transform state for pan/zoom
    let mut transform_x = use_signal(|| 0.0);
    let mut transform_y = use_signal(|| 0.0);
    let mut scale = use_signal(|| 1.0);

    // Mouse interaction state for panning
    let mut is_panning = use_signal(|| false);
    let mut last_mouse_x = use_signal(|| 0.0);
    let mut last_mouse_y = use_signal(|| 0.0);

    // Module/Node dragging state
    let mut dragging_module: Signal<Option<Uuid>> = use_signal(|| None);
    let mut dragging_node: Signal<Option<Uuid>> = use_signal(|| None);
    let mut drag_offset_x = use_signal(|| 0.0);
    let mut drag_offset_y = use_signal(|| 0.0);

    // Selected module/node for highlighting
    let mut selected_module: Signal<Option<Uuid>> = use_signal(|| None);
    let mut selected_node: Signal<Option<Uuid>> = use_signal(|| None);

    // Focus mode state (for expanded dialog view)
    let mut focused_node: Signal<Option<Uuid>> = use_signal(|| None);

    // Resize handle dragging
    let mut resizing_module: Signal<Option<Uuid>> = use_signal(|| None);
    let mut resize_start_size = use_signal(|| NodeSize::default());
    let mut resize_original_nodes: Signal<Vec<(Uuid, NodePosition, NodeSize)>> = use_signal(Vec::new);
    let mut resizing_node: Signal<Option<Uuid>> = use_signal(|| None);
    let mut resizing_node_module: Signal<Option<Uuid>> = use_signal(|| None);

    // Module focus mode
    let mut focused_module: Signal<Option<Uuid>> = use_signal(|| None);

    // Compute cursor style
    let cursor_style = if *is_panning.read() {
        "grabbing"
    } else if resizing_module.read().is_some() || resizing_node.read().is_some() {
        "nwse-resize"
    } else if dragging_module.read().is_some() {
        "grabbing"
    } else {
        "default"
    };

    rsx! {
        div {
            class: "w-full h-full relative bg-zinc-950 overflow-hidden select-none",

            // View mode toggle button
            div {
                class: "absolute top-3 right-3 z-10 flex gap-2",
                button {
                    class: if view_mode() == CanvasViewMode::Node {
                        "px-3 py-1.5 rounded-lg bg-blue-600 text-white text-xs font-medium"
                    } else {
                        "px-3 py-1.5 rounded-lg bg-zinc-800 hover:bg-zinc-700 text-zinc-300 text-xs font-medium"
                    },
                    onclick: move |_| view_mode.set(CanvasViewMode::Node),
                    "Node View"
                }
                button {
                    class: if view_mode() == CanvasViewMode::Performance {
                        "px-3 py-1.5 rounded-lg bg-blue-600 text-white text-xs font-medium"
                    } else {
                        "px-3 py-1.5 rounded-lg bg-zinc-800 hover:bg-zinc-700 text-zinc-300 text-xs font-medium"
                    },
                    onclick: move |_| view_mode.set(CanvasViewMode::Performance),
                    "Performance View"
                }
            }

            // Render appropriate view based on mode
            if view_mode() == CanvasViewMode::Performance {
                PerformanceView {
                    graph: graph,
                }
            } else {
                // SVG canvas (no fixed viewBox - use actual coordinates)
                svg {
                class: "w-full h-full",
                style: "cursor: {cursor_style};",
                xmlns: "http://www.w3.org/2000/svg",

                // Mouse events for pan/zoom
                onmousedown: move |evt| {
                    // Use element coordinates (relative to SVG element)
                    let element_x = evt.element_coordinates().x;
                    let element_y = evt.element_coordinates().y;
                    // Check which mouse button triggered the event
                    let trigger_button = evt.data.trigger_button();
                    // MouseButton enum: Primary = left, Secondary = right, Auxiliary = middle
                    let is_middle_button = matches!(trigger_button, Some(dioxus::html::input_data::MouseButton::Auxiliary));

                    let graph_read = graph.read();

                    // Transform element coordinates to canvas coordinates
                    // Account for pan (translate) and zoom (scale)
                    let canvas_x = (element_x - transform_x()) / scale();
                    let canvas_y = (element_y - transform_y()) / scale();

                    // Middle mouse button always starts panning
                    if is_middle_button {
                        is_panning.set(true);
                        last_mouse_x.set(element_x);
                        last_mouse_y.set(element_y);
                        drop(graph_read);
                        return;
                    }

                    // Left mouse button: check for module/node interactions
                    let is_left_button = matches!(trigger_button, Some(dioxus::html::input_data::MouseButton::Primary));
                    if is_left_button {
                        // Check for resize handle click (bottom-right corner of modules)
                        let mut resize_module = None;
                        for module in graph_read.modules.iter().rev() {
                            let handle_x = module.position.x + module.size.width - 20.0;
                            let handle_y = module.position.y + module.size.height - 20.0;
                            let in_handle = canvas_x >= handle_x && canvas_x <= handle_x + 20.0
                                && canvas_y >= handle_y && canvas_y <= handle_y + 20.0;

                            if in_handle {
                                resize_module = Some(module.clone());
                                break;
                            }
                        }

                        if let Some(module) = resize_module {
                            // Start resizing - store original node positions/sizes for proportional scaling
                            let original_nodes: Vec<(Uuid, NodePosition, NodeSize)> = module.nodes.iter()
                                .map(|n| (n.id, n.position, n.size))
                                .collect();

                            resizing_module.set(Some(module.id));
                            resize_start_size.set(module.size);
                            resize_original_nodes.set(original_nodes);
                            selected_module.set(Some(module.id));
                            drop(graph_read);
                            return;
                        }

                        // Check for module title bar click (for dragging)
                        let mut clicked_module = None;
                        for module in graph_read.modules.iter().rev() {
                            if module.title_bar_contains(canvas_x, canvas_y) {
                                clicked_module = Some(module.clone());
                                break;
                            }
                        }

                        if let Some(module) = clicked_module {
                            // Start dragging module
                            dragging_module.set(Some(module.id));
                            drag_offset_x.set(canvas_x - module.position.x);
                            drag_offset_y.set(canvas_y - module.position.y);
                            selected_module.set(Some(module.id));
                            drop(graph_read);
                            return;
                        }

                        // Check if clicking inside any module (for node interaction)
                        let mut clicked_inside_module = false;
                        for module in graph_read.modules.iter().rev() {
                            if module.contains_point(canvas_x, canvas_y) {
                                clicked_inside_module = true;
                                selected_module.set(Some(module.id));
                                break;
                            }
                        }

                        drop(graph_read);

                        if clicked_inside_module {
                            // Clicked inside a module - do nothing, let module handle it
                            // This allows for future node selection/interaction
                            return;
                        }

                        // Clicked on empty background - start panning
                        is_panning.set(true);
                        last_mouse_x.set(element_x);
                        last_mouse_y.set(element_y);
                        selected_module.set(None);
                        selected_node.set(None);
                    }
                },
                onmousemove: move |evt| {
                    // Use element coordinates (relative to SVG element)
                    let element_x = evt.element_coordinates().x;
                    let element_y = evt.element_coordinates().y;

                    let canvas_x = (element_x - transform_x()) / scale();
                    let canvas_y = (element_y - transform_y()) / scale();

                    if let Some(module_id) = *resizing_module.read() {
                        // Resize module with proportional scaling of internal nodes
                        let mut graph = graph.write();
                        if let Some(module) = graph.find_module_mut(module_id) {
                            let new_width = (canvas_x - module.position.x).max(300.0);
                            let new_height = (canvas_y - module.position.y).max(150.0);

                            // Calculate scaling factors
                            let old_size = resize_start_size();
                            let scale_x = new_width / old_size.width;
                            let scale_y = new_height / old_size.height;

                            // Apply proportional scaling to all internal nodes
                            let original_nodes = resize_original_nodes();
                            for (node_id, original_pos, original_size) in &original_nodes {
                                if let Some(node) = module.find_node_mut(*node_id) {
                                    // Scale position and size proportionally
                                    node.position = NodePosition::new(
                                        original_pos.x * scale_x,
                                        original_pos.y * scale_y,
                                    );
                                    node.size = NodeSize::new(
                                        original_size.width * scale_x,
                                        original_size.height * scale_y,
                                    );
                                }
                            }

                            module.size = NodeSize::new(new_width, new_height);
                        }
                    } else if let Some(module_id) = *dragging_module.read() {
                        // Drag module
                        let new_x = canvas_x - drag_offset_x();
                        let new_y = canvas_y - drag_offset_y();

                        // Update module position
                        let mut graph = graph.write();
                        if let Some(module) = graph.find_module_mut(module_id) {
                            module.position = NodePosition::new(new_x, new_y);
                        }
                    } else if let Some(node_id) = *dragging_node.read() {
                        // Drag standalone node
                        let new_x = canvas_x - drag_offset_x();
                        let new_y = canvas_y - drag_offset_y();

                        // Update node position
                        let mut graph = graph.write();
                        if let Some(node) = graph.find_node_mut(node_id) {
                            node.position = NodePosition::new(new_x, new_y);
                        }

                        // Call callback
                        if let Some(ref cb) = props.on_node_move {
                            cb.call((node_id, NodePosition::new(new_x, new_y)));
                        }
                    } else if *is_panning.read() {
                        // Pan canvas
                        let dx = element_x - last_mouse_x();
                        let dy = element_y - last_mouse_y();
                        transform_x.set(transform_x() + dx);
                        transform_y.set(transform_y() + dy);
                        last_mouse_x.set(element_x);
                        last_mouse_y.set(element_y);
                    }
                },
                onmouseup: move |_| {
                    is_panning.set(false);
                    dragging_module.set(None);
                    dragging_node.set(None);
                    resizing_module.set(None);
                    resizing_node.set(None);
                    resizing_node_module.set(None);
                },
                onwheel: move |evt| {
                    evt.prevent_default();

                    let delta = -evt.delta().strip_units().y / 500.0;
                    let old_scale = scale();
                    let new_scale = (old_scale * (1.0 + delta)).clamp(0.25, 4.0);

                    // Zoom towards mouse position (use element coordinates)
                    let mouse_x = evt.element_coordinates().x;
                    let mouse_y = evt.element_coordinates().y;

                    let scale_change = new_scale / old_scale;
                    let new_tx = mouse_x - (mouse_x - transform_x()) * scale_change;
                    let new_ty = mouse_y - (mouse_y - transform_y()) * scale_change;

                    transform_x.set(new_tx);
                    transform_y.set(new_ty);
                    scale.set(new_scale);
                },

                // Group with transform
                g {
                    transform: "translate({transform_x()}, {transform_y()}) scale({scale()})",

                    // Render inter-module wires first (behind everything)
                    for wire in &graph.read().wires {
                        WireRenderer {
                            key: "{wire.id}",
                            wire: wire.clone(),
                            graph: graph,
                        }
                    }

                    // Render modules (which contain nodes)
                    for module in &graph.read().modules {
                        {
                            let module_id = module.id;
                            rsx! {
                                ModuleRenderer {
                                    key: "{module.id}",
                                    module: module.clone(),
                                    is_selected: selected_module().map(|id| id == module.id).unwrap_or(false),
                                    focused_node: focused_node,
                                    on_module_focus: Some(Callback::new(move |id| {
                                        focused_module.set(Some(id));
                                    })),
                                    resizing_node: resizing_node,
                                    resizing_node_module: resizing_node_module,
                                }
                            }
                        }
                    }

                    // Render standalone nodes (if any)
                    for node in &graph.read().nodes {
                        {
                            let node_id = node.id;
                            rsx! {
                                NodeRenderer {
                                    key: "{node.id}",
                                    node: node.clone(),
                                    is_selected: selected_node().map(|id| id == node.id).unwrap_or(false),
                                    on_click: props.on_node_click.clone(),
                                    on_toggle_bypass: props.on_toggle_bypass.clone(),
                                    on_focus: Some(Callback::new(move |id| {
                                        focused_node.set(Some(id));
                                        if let Some(ref cb) = props.on_node_focus {
                                            cb.call(id);
                                        }
                                    })),
                                }
                            }
                        }
                    }
                }
            }

                // Info overlay (zoom level) - only in Node view
                div {
                    class: "absolute bottom-3 right-3 text-xs text-zinc-500 pointer-events-none bg-zinc-900/80 px-2 py-1 rounded",
                    "Zoom: {scale() * 100.0:.0}%"
                }

                // Help overlay - only in Node view
                div {
                    class: "absolute top-3 left-3 text-xs text-zinc-500 bg-zinc-900/80 px-3 py-2 rounded space-y-1",
                    div { "🖱️ Middle-click or click empty space to pan" }
                    div { "🔍 Scroll to zoom" }
                    div { "✋ Drag title bar to move modules" }
                    div { "↔️ Drag module corner to resize (scales nodes)" }
                    div { "↔️ Drag node corner to resize individual node" }
                    div { "🖱️ Double-click module title for module focus" }
                    div { "🖱️ Double-click node for node focus mode" }
                }
            }

            // Node focus mode dialog - works in both view modes
            if let Some(node_id) = focused_node() {
                {
                    let graph_read = graph.read();
                    let node = graph_read.find_node(node_id).cloned();
                    drop(graph_read);

                    rsx! {
                        NodeFocusDialog {
                            node: node,
                            on_close: move |_| focused_node.set(None),
                        }
                    }
                }
            }

            // Module focus mode dialog
            if let Some(module_id) = focused_module() {
                {
                    let graph_read = graph.read();
                    let module = graph_read.find_module(module_id).cloned();
                    drop(graph_read);

                    rsx! {
                        ModuleFocusDialog {
                            module: module,
                            on_close: move |_| focused_module.set(None),
                        }
                    }
                }
            }
        }
    }
}

/// Props for module focus dialog.
#[derive(Props, Clone, PartialEq)]
struct ModuleFocusDialogProps {
    module: Option<Module>,
    on_close: Callback<()>,
}

/// Full-screen focus dialog for a module showing all internal nodes at larger scale.
#[component]
fn ModuleFocusDialog(props: ModuleFocusDialogProps) -> Element {
    let Some(module) = props.module else {
        return rsx! {};
    };

    rsx! {
        // Backdrop
        div {
            class: "fixed inset-0 z-50 flex items-center justify-center bg-black/80 backdrop-blur-sm",
            onclick: move |_| props.on_close.call(()),

            // Dialog container
            div {
                class: "bg-zinc-900 rounded-xl border border-zinc-700 shadow-2xl w-[90vw] h-[85vh] \
                        flex flex-col overflow-hidden animate-in fade-in zoom-in duration-200",
                onclick: |e| e.stop_propagation(),

                // Header
                div { class: "flex items-center justify-between px-6 py-4 border-b border-zinc-800 bg-zinc-850",
                    div {
                        h2 { class: "text-xl font-semibold text-zinc-200", "{module.name}" }
                        p { class: "text-sm text-zinc-500 mt-0.5", "Module Internal View" }
                    }
                    button {
                        class: "p-2 rounded-lg hover:bg-zinc-800 text-zinc-400 hover:text-zinc-200 transition-colors",
                        onclick: move |_| props.on_close.call(()),
                        // Close icon
                        svg {
                            class: "w-6 h-6",
                            fill: "none",
                            stroke: "currentColor",
                            stroke_width: "2",
                            view_box: "0 0 24 24",
                            path {
                                stroke_linecap: "round",
                                stroke_linejoin: "round",
                                d: "M6 18L18 6M6 6l12 12",
                            }
                        }
                    }
                }

                // Content area - render module internals at 2x scale
                div { class: "flex-1 overflow-auto bg-zinc-950",
                    ModuleFocusContent { module: module.clone() }
                }

                // Footer
                div { class: "px-6 py-4 border-t border-zinc-800 bg-zinc-850 flex items-center justify-between",
                    div { class: "flex items-center gap-2",
                        // Bypass toggle
                        button {
                            class: if module.bypassed {
                                "px-4 py-2 rounded-lg bg-red-600 hover:bg-red-500 text-white text-sm font-medium transition-colors"
                            } else {
                                "px-4 py-2 rounded-lg bg-green-600 hover:bg-green-500 text-white text-sm font-medium transition-colors"
                            },
                            if module.bypassed { "Bypassed" } else { "Active" }
                        }
                    }
                    button {
                        class: "px-6 py-2 rounded-lg bg-zinc-700 hover:bg-zinc-600 text-white text-sm font-medium transition-colors",
                        onclick: move |_| props.on_close.call(()),
                        "Close"
                    }
                }
            }
        }
    }
}

/// Props for module focus content.
#[derive(Props, Clone, PartialEq)]
struct ModuleFocusContentProps {
    module: Module,
}

/// Renders module internals at larger scale with node system.
#[component]
fn ModuleFocusContent(props: ModuleFocusContentProps) -> Element {
    let module = &props.module;
    let scale = 2.0; // Scale up nodes for easier viewing

    rsx! {
        div { class: "w-full h-full p-8",
            svg {
                class: "w-full h-full",
                xmlns: "http://www.w3.org/2000/svg",

                // Render internal wires
                for wire in &module.internal_wires {
                    {
                        let from_node = module.find_node(wire.from_node);
                        let to_node = module.find_node(wire.to_node);

                        if let (Some(from), Some(to)) = (from_node, to_node) {
                            let from_pos = from.port_position(&wire.from_port, false);
                            let to_pos = to.port_position(&wire.to_port, true);

                            if let (Some(fp), Some(tp)) = (from_pos, to_pos) {
                                let control_offset = ((tp.x - fp.x).abs() * 0.5).min(100.0);
                                let cp1_x = (fp.x + control_offset) * scale;
                                let cp1_y = fp.y * scale;
                                let cp2_x = (tp.x - control_offset) * scale;
                                let cp2_y = tp.y * scale;
                                let wire_color = wire.color.as_deref().unwrap_or("#60A5FA");

                                rsx! {
                                    path {
                                        key: "{wire.id}",
                                        d: "M {fp.x * scale} {fp.y * scale} C {cp1_x} {cp1_y}, {cp2_x} {cp2_y}, {tp.x * scale} {tp.y * scale}",
                                        fill: "none",
                                        stroke: "{wire_color}",
                                        stroke_width: "3",
                                        opacity: "0.7",
                                    }
                                }
                            } else {
                                rsx! {}
                            }
                        } else {
                            rsx! {}
                        }
                    }
                }

                // Render nodes at 2x scale
                for node in &module.nodes {
                    {
                        let node_id = node.id;
                        let color = block_type_color(node.block_type);
                        let bg_color = if node.bypassed {
                            format!("{}30", color.bg)
                        } else {
                            color.bg.to_string()
                        };
                        let border_color = if node.bypassed {
                            format!("{}60", color.border)
                        } else {
                            color.border.to_string()
                        };

                        rsx! {
                            g {
                                key: "{node.id}",
                                transform: "translate({node.position.x * scale}, {node.position.y * scale})",

                                // Node background
                                rect {
                                    width: "{node.size.width * scale}",
                                    height: "{node.size.height * scale}",
                                    rx: "8",
                                    fill: "{bg_color}",
                                    stroke: "{border_color}",
                                    stroke_width: "2",
                                }

                                // Node header
                                rect {
                                    width: "{node.size.width * scale}",
                                    height: "{24.0 * scale}",
                                    rx: "8",
                                    fill: "rgba(0, 0, 0, 0.3)",
                                }

                                // Node title
                                text {
                                    x: "{node.size.width * scale / 2.0}",
                                    y: "{16.0 * scale}",
                                    fill: "#FFFFFF",
                                    font_size: "{11.0 * scale}",
                                    font_weight: "600",
                                    text_anchor: "middle",
                                    "{node.name}"
                                }

                                // Input ports
                                for (idx, port) in node.inputs.iter().enumerate() {
                                    {
                                        let port_y = node.size.height / (node.inputs.len() + 1) as f64 * (idx + 1) as f64 * scale;
                                        rsx! {
                                            circle {
                                                cx: "0",
                                                cy: "{port_y}",
                                                r: "{4.0 * scale}",
                                                fill: port.color.as_deref().unwrap_or("#3B82F6"),
                                                stroke: "#000000",
                                                stroke_width: "1",
                                            }
                                        }
                                    }
                                }

                                // Output ports
                                for (idx, port) in node.outputs.iter().enumerate() {
                                    {
                                        let port_y = node.size.height / (node.outputs.len() + 1) as f64 * (idx + 1) as f64 * scale;
                                        rsx! {
                                            circle {
                                                cx: "{node.size.width * scale}",
                                                cy: "{port_y}",
                                                r: "{4.0 * scale}",
                                                fill: port.color.as_deref().unwrap_or("#3B82F6"),
                                                stroke: "#000000",
                                                stroke_width: "1",
                                            }
                                        }
                                    }
                                }

                                // Widget content
                                if node.size.height > 40.0 {
                                    {
                                        let content_y = 30.0 * scale;
                                        let content_width = node.size.width * scale;
                                        let content_height = (node.size.height - 36.0) * scale;

                                        match node.widget {
                                            NodeWidget::EqGraph | NodeWidget::CompressorGraph => rsx! {
                                                foreignObject {
                                                    x: "0",
                                                    y: "{content_y}",
                                                    width: "{content_width}",
                                                    height: "{content_height}",
                                                    div {
                                                        class: "w-full h-full flex items-center justify-center text-zinc-400",
                                                        "{node.name} Widget"
                                                    }
                                                }
                                            },
                                            _ => rsx! {
                                                text {
                                                    x: "{node.size.width * scale / 2.0}",
                                                    y: "{content_y + content_height / 2.0}",
                                                    fill: "#71717A",
                                                    font_size: "{10.0 * scale}",
                                                    text_anchor: "middle",
                                                    "{node.short_label.as_deref().unwrap_or(&node.name)}"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Props for wire renderer.
#[derive(Props, Clone, PartialEq)]
struct WireRendererProps {
    wire: Wire,
    graph: Signal<NodeGraph>,
}

/// Renders a single wire connection with bezier curve.
#[component]
fn WireRenderer(props: WireRendererProps) -> Element {
    let graph = props.graph.read();

    // Find source and destination nodes
    let Some(from_node) = graph.find_node(props.wire.from_node) else {
        return rsx! {};
    };
    let Some(to_node) = graph.find_node(props.wire.to_node) else {
        return rsx! {};
    };

    // Get port positions
    let Some(from_pos) = from_node.port_position(&props.wire.from_port, false) else {
        return rsx! {};
    };
    let Some(to_pos) = to_node.port_position(&props.wire.to_port, true) else {
        return rsx! {};
    };

    // Calculate bezier control points (horizontal flow)
    let control_offset = ((to_pos.x - from_pos.x).abs() * 0.5).min(150.0);
    let cp1_x = from_pos.x + control_offset;
    let cp1_y = from_pos.y;
    let cp2_x = to_pos.x - control_offset;
    let cp2_y = to_pos.y;

    let wire_color = props.wire.color.as_deref().unwrap_or("#3B82F6");

    rsx! {
        path {
            d: "M {from_pos.x} {from_pos.y} C {cp1_x} {cp1_y}, {cp2_x} {cp2_y}, {to_pos.x} {to_pos.y}",
            fill: "none",
            stroke: "{wire_color}",
            stroke_width: "2",
            opacity: "0.6",
        }
    }
}

/// Props for module renderer.
#[derive(Props, Clone, PartialEq)]
struct ModuleRendererProps {
    module: Module,
    is_selected: bool,
    focused_node: Signal<Option<Uuid>>,
    on_module_focus: Option<Callback<Uuid>>,
    resizing_node: Signal<Option<Uuid>>,
    resizing_node_module: Signal<Option<Uuid>>,
}

/// Renders a module container with nodes inside.
#[component]
fn ModuleRenderer(props: ModuleRendererProps) -> Element {
    let module = &props.module;
    let module_id = module.id; // Extract ID for closures
    let color = block_type_color(module.block_type);

    let bg_color = if module.bypassed {
        format!("{}20", color.bg)
    } else {
        format!("{}40", color.bg)
    };

    let border_color = if props.is_selected {
        "#60A5FA".to_string()
    } else if module.bypassed {
        format!("{}60", color.border)
    } else {
        color.border.to_string()
    };

    rsx! {
        g {
            transform: "translate({module.position.x}, {module.position.y})",

            // Module background container
            rect {
                width: "{module.size.width}",
                height: "{module.size.height}",
                rx: "12",
                fill: "{bg_color}",
                stroke: "{border_color}",
                stroke_width: if props.is_selected { "3" } else { "2" },
            }

            // Module header/title bar
            rect {
                width: "{module.size.width}",
                height: "40",
                rx: "12",
                fill: "rgba(0, 0, 0, 0.5)",
                cursor: "move",
                ondblclick: move |evt| {
                    evt.stop_propagation();
                    if let Some(ref cb) = props.on_module_focus {
                        cb.call(module_id);
                    }
                },
            }

            // Module title
            text {
                x: "{module.size.width / 2.0}",
                y: "25",
                fill: "#FFFFFF",
                font_size: "16",
                font_weight: "700",
                text_anchor: "middle",
                pointer_events: "none",
                "{module.name}"
            }

            // Bypass indicator
            if module.bypassed {
                circle {
                    cx: "{module.size.width - 20.0}",
                    cy: "20",
                    r: "5",
                    fill: "#EF4444",
                }
            }

            // Module-level input ports
            for (idx, port) in module.inputs.iter().enumerate() {
                {
                    let port_y = module.size.height / (module.inputs.len() + 1) as f64 * (idx + 1) as f64;
                    rsx! {
                        circle {
                            cx: "0",
                            cy: "{port_y}",
                            r: "8",
                            fill: port.color.as_deref().unwrap_or("#3B82F6"),
                            stroke: "#000000",
                            stroke_width: "2",
                        }
                    }
                }
            }

            // Module-level output ports
            for (idx, port) in module.outputs.iter().enumerate() {
                {
                    let port_y = module.size.height / (module.outputs.len() + 1) as f64 * (idx + 1) as f64;
                    rsx! {
                        circle {
                            cx: "{module.size.width}",
                            cy: "{port_y}",
                            r: "8",
                            fill: port.color.as_deref().unwrap_or("#3B82F6"),
                            stroke: "#000000",
                            stroke_width: "2",
                        }
                    }
                }
            }

            // Render internal wires (within module)
            for wire in &module.internal_wires {
                InternalWireRenderer {
                    key: "{wire.id}",
                    wire: wire.clone(),
                    module: module.clone(),
                }
            }

            // Render nodes inside module
            if module.nodes.len() == 1 {
                // Single node - render fullscreen without node chrome
                {
                    let node = &module.nodes[0];
                    let content_y = 50.0; // After module title bar
                    let content_width = module.size.width - 20.0;
                    let content_height = module.size.height - 70.0;

                    match node.widget {
                        NodeWidget::EqGraph => {
                            let mut bands = use_signal(|| vec![
                                EqBand {
                                    index: 0,
                                    used: true,
                                    enabled: true,
                                    frequency: 100.0,
                                    gain: 3.0,
                                    q: 0.7,
                                    shape: EqBandShape::LowShelf,
                                    ..Default::default()
                                },
                                EqBand {
                                    index: 1,
                                    used: true,
                                    enabled: true,
                                    frequency: 800.0,
                                    gain: -2.0,
                                    q: 1.5,
                                    shape: EqBandShape::Bell,
                                    ..Default::default()
                                },
                                EqBand {
                                    index: 2,
                                    used: true,
                                    enabled: true,
                                    frequency: 4000.0,
                                    gain: 2.5,
                                    q: 1.0,
                                    shape: EqBandShape::HighShelf,
                                    ..Default::default()
                                },
                            ]);

                            rsx! {
                                foreignObject {
                                    x: "10",
                                    y: "{content_y}",
                                    width: "{content_width}",
                                    height: "{content_height}",
                                    div {
                                        class: "w-full h-full",
                                        style: "pointer-events: auto;",
                                        EqGraph {
                                            bands: bands,
                                            db_range: 12.0,
                                            show_grid: true,
                                            fill_curve: true,
                                            disabled: false,
                                            on_band_change: move |(idx, band): (usize, EqBand)| {
                                                bands.write()[idx] = band;
                                            },
                                            on_band_add: move |band: EqBand| {
                                                let mut all_bands = bands();
                                                all_bands.push(band);
                                                bands.set(all_bands);
                                            },
                                            on_band_remove: move |idx: usize| {
                                                let mut all_bands = bands();
                                                all_bands.remove(idx);
                                                bands.set(all_bands);
                                            },
                                        }
                                    }
                                }
                            }
                        }
                        NodeWidget::CompressorGraph => {
                            let params = CompressorParams {
                                threshold: -20.0,
                                ratio: 4.0,
                                attack: 10.0,
                                release: 100.0,
                                knee: 6.0,
                                makeup: 0.0,
                                ..Default::default()
                            };

                            rsx! {
                                foreignObject {
                                    x: "10",
                                    y: "{content_y}",
                                    width: "{content_width}",
                                    height: "{content_height}",
                                    div {
                                        class: "w-full h-full",
                                        style: "pointer-events: auto;",
                                        CompressorGraph {
                                            params: params,
                                            db_range: DbRange::default(),
                                            show_grid: true,
                                        }
                                    }
                                }
                            }
                        }
                        _ => rsx! {
                            text {
                                x: "{module.size.width / 2.0}",
                                y: "{module.size.height / 2.0}",
                                fill: "#71717A",
                                font_size: "14",
                                text_anchor: "middle",
                                "{node.short_label.as_deref().unwrap_or(&node.name)}"
                            }
                        }
                    }
                }
            } else {
                // Multiple nodes - render as before
                for node in &module.nodes {
                    {
                        let node_id = node.id;
                        let focused_id = *props.focused_node.read();
                        let mut focused_node_signal = props.focused_node;
                        let resizing_node_id = *props.resizing_node.read();
                        let is_resizing = resizing_node_id == Some(node_id);
                        rsx! {
                            g {
                                transform: "translate({node.position.x}, {node.position.y})",

                                InternalNodeRenderer {
                                    key: "{node.id}",
                                    node: node.clone(),
                                    is_focused: focused_id.map(|id| id == node.id).unwrap_or(false),
                                    on_focus: Some(Callback::new(move |id| {
                                        focused_node_signal.set(Some(id));
                                    })),
                                    is_resizing: is_resizing,
                                    module_id: module_id,
                                }
                            }
                        }
                    }
                }
            }

            // Resize handle (bottom-right corner)
            rect {
                x: "{module.size.width - 20.0}",
                y: "{module.size.height - 20.0}",
                width: "20",
                height: "20",
                fill: "rgba(255, 255, 255, 0.3)",
                cursor: "nwse-resize",
                rx: "3",
            }
            // Resize icon (small diagonal lines)
            line {
                x1: "{module.size.width - 15.0}",
                y1: "{module.size.height - 5.0}",
                x2: "{module.size.width - 5.0}",
                y2: "{module.size.height - 15.0}",
                stroke: "rgba(255, 255, 255, 0.6)",
                stroke_width: "2",
                pointer_events: "none",
            }
            line {
                x1: "{module.size.width - 10.0}",
                y1: "{module.size.height - 5.0}",
                x2: "{module.size.width - 5.0}",
                y2: "{module.size.height - 10.0}",
                stroke: "rgba(255, 255, 255, 0.6)",
                stroke_width: "2",
                pointer_events: "none",
            }
        }
    }
}

/// Props for internal wire renderer (wires within a module).
#[derive(Props, Clone, PartialEq)]
struct InternalWireRendererProps {
    wire: Wire,
    module: Module,
}

/// Renders internal wires within a module.
#[component]
fn InternalWireRenderer(props: InternalWireRendererProps) -> Element {
    let module = &props.module;

    // Find source and destination nodes
    let Some(from_node) = module.find_node(props.wire.from_node) else {
        return rsx! {};
    };
    let Some(to_node) = module.find_node(props.wire.to_node) else {
        return rsx! {};
    };

    // Get port positions (relative to module, not canvas)
    let Some(from_pos) = from_node.port_position(&props.wire.from_port, false) else {
        return rsx! {};
    };
    let Some(to_pos) = to_node.port_position(&props.wire.to_port, true) else {
        return rsx! {};
    };

    // Calculate bezier control points
    let control_offset = ((to_pos.x - from_pos.x).abs() * 0.5).min(100.0);
    let cp1_x = from_pos.x + control_offset;
    let cp1_y = from_pos.y;
    let cp2_x = to_pos.x - control_offset;
    let cp2_y = to_pos.y;

    let wire_color = props.wire.color.as_deref().unwrap_or("#60A5FA");

    rsx! {
        path {
            d: "M {from_pos.x} {from_pos.y} C {cp1_x} {cp1_y}, {cp2_x} {cp2_y}, {to_pos.x} {to_pos.y}",
            fill: "none",
            stroke: "{wire_color}",
            stroke_width: "2",
            opacity: "0.7",
        }
    }
}

/// Props for internal node renderer (nodes within a module).
#[derive(Props, Clone, PartialEq)]
struct InternalNodeRendererProps {
    node: Node,
    is_focused: bool,
    #[props(default)]
    on_focus: Option<Callback<Uuid>>,
    is_resizing: bool,
    module_id: Uuid,
}

/// Renders a node inside a module.
#[component]
fn InternalNodeRenderer(props: InternalNodeRendererProps) -> Element {
    let node = &props.node;
    let node_id = node.id; // Extract ID for closure
    let color = block_type_color(node.block_type);

    let bg_color = if node.bypassed {
        format!("{}30", color.bg)
    } else {
        color.bg.to_string()
    };

    let border_color = if props.is_focused {
        "#F59E0B".to_string() // Orange for focused
    } else if node.bypassed {
        format!("{}60", color.border)
    } else {
        color.border.to_string()
    };

    rsx! {
        // Node background
        rect {
            width: "{node.size.width}",
            height: "{node.size.height}",
            rx: "6",
            fill: "{bg_color}",
            stroke: "{border_color}",
            stroke_width: if props.is_focused { "2" } else { "1" },
        }

        // Node header/title bar
        rect {
            width: "{node.size.width}",
            height: "24",
            rx: "6",
            fill: "rgba(0, 0, 0, 0.3)",
            cursor: "pointer",
            ondblclick: move |evt| {
                evt.stop_propagation();
                if let Some(ref cb) = props.on_focus {
                    cb.call(node_id);
                }
            },
        }

        // Node title
        text {
            x: "{node.size.width / 2.0}",
            y: "16",
            fill: "#FFFFFF",
            font_size: "11",
            font_weight: "600",
            text_anchor: "middle",
            pointer_events: "none",
            "{node.name}"
        }

        // Bypass indicator
        if node.bypassed {
            circle {
                cx: "{node.size.width - 10.0}",
                cy: "12",
                r: "3",
                fill: "#EF4444",
            }
        }

        // Input ports
        for (idx, port) in node.inputs.iter().enumerate() {
            {
                let port_y = node.size.height / (node.inputs.len() + 1) as f64 * (idx + 1) as f64;
                rsx! {
                    circle {
                        cx: "0",
                        cy: "{port_y}",
                        r: "4",
                        fill: port.color.as_deref().unwrap_or("#3B82F6"),
                        stroke: "#000000",
                        stroke_width: "1",
                    }
                }
            }
        }

        // Output ports
        for (idx, port) in node.outputs.iter().enumerate() {
            {
                let port_y = node.size.height / (node.outputs.len() + 1) as f64 * (idx + 1) as f64;
                rsx! {
                    circle {
                        cx: "{node.size.width}",
                        cy: "{port_y}",
                        r: "4",
                        fill: port.color.as_deref().unwrap_or("#3B82F6"),
                        stroke: "#000000",
                        stroke_width: "1",
                    }
                }
            }
        }

        // Widget content area
        if node.size.height > 40.0 {
            NodeWidgetContent {
                node: node.clone(),
            }
        }

        // Resize handle (bottom-right corner) for individual nodes
        rect {
            x: "{node.size.width - 15.0}",
            y: "{node.size.height - 15.0}",
            width: "15",
            height: "15",
            fill: "rgba(255, 255, 255, 0.2)",
            cursor: "nwse-resize",
            rx: "2",
        }
        // Resize icon
        line {
            x1: "{node.size.width - 10.0}",
            y1: "{node.size.height - 3.0}",
            x2: "{node.size.width - 3.0}",
            y2: "{node.size.height - 10.0}",
            stroke: "rgba(255, 255, 255, 0.5)",
            stroke_width: "1",
            pointer_events: "none",
        }
        line {
            x1: "{node.size.width - 6.0}",
            y1: "{node.size.height - 3.0}",
            x2: "{node.size.width - 3.0}",
            y2: "{node.size.height - 6.0}",
            stroke: "rgba(255, 255, 255, 0.5)",
            stroke_width: "1",
            pointer_events: "none",
        }
    }
}

/// Props for node renderer.
#[derive(Props, Clone, PartialEq)]
struct NodeRendererProps {
    node: Node,
    is_selected: bool,
    on_click: Option<Callback<Uuid>>,
    on_toggle_bypass: Option<Callback<Uuid>>,
    on_focus: Option<Callback<Uuid>>,
}

/// Renders a single node on the canvas.
#[component]
fn NodeRenderer(props: NodeRendererProps) -> Element {
    let node = &props.node;
    let color = block_type_color(node.block_type);

    let bg_color = if node.bypassed {
        format!("{}30", color.bg)
    } else {
        color.bg.to_string()
    };

    let border_color = if props.is_selected {
        "#60A5FA".to_string() // Blue for selected
    } else if node.bypassed {
        format!("{}60", color.border)
    } else {
        color.border.to_string()
    };

    let node_id = node.id;

    rsx! {
        g {
            transform: "translate({node.position.x}, {node.position.y})",

            // Node background
            rect {
                width: "{node.size.width}",
                height: "{node.size.height}",
                rx: "8",
                fill: "{bg_color}",
                stroke: "{border_color}",
                stroke_width: if props.is_selected { "3" } else { "2" },
                cursor: "move",
            }

            // Node header (title bar)
            rect {
                width: "{node.size.width}",
                height: "32",
                rx: "8",
                fill: "rgba(0, 0, 0, 0.3)",
                cursor: "move",
                ondblclick: move |evt| {
                    evt.stop_propagation();
                    if let Some(ref cb) = props.on_focus {
                        cb.call(node_id);
                    }
                },
            }

            // Node title
            text {
                x: "{node.size.width / 2.0}",
                y: "20",
                fill: "#FFFFFF",
                font_size: "14",
                font_weight: "600",
                text_anchor: "middle",
                pointer_events: "none",
                "{node.name}"
            }

            // Bypass indicator
            if node.bypassed {
                circle {
                    cx: "{node.size.width - 16.0}",
                    cy: "16",
                    r: "4",
                    fill: "#EF4444",
                }
            }

            // Input ports
            for (idx, port) in node.inputs.iter().enumerate() {
                {
                    let port_y = node.size.height / (node.inputs.len() + 1) as f64 * (idx + 1) as f64;
                    rsx! {
                        circle {
                            cx: "0",
                            cy: "{port_y}",
                            r: "6",
                            fill: port.color.as_deref().unwrap_or("#3B82F6"),
                            stroke: "#000000",
                            stroke_width: "2",
                        }
                    }
                }
            }

            // Output ports
            for (idx, port) in node.outputs.iter().enumerate() {
                {
                    let port_y = node.size.height / (node.outputs.len() + 1) as f64 * (idx + 1) as f64;
                    rsx! {
                        circle {
                            cx: "{node.size.width}",
                            cy: "{port_y}",
                            r: "6",
                            fill: port.color.as_deref().unwrap_or("#3B82F6"),
                            stroke: "#000000",
                            stroke_width: "2",
                        }
                    }
                }
            }

            // Widget content area
            if node.size.height > 60.0 {
                NodeWidgetContent {
                    node: node.clone(),
                }
            }
        }
    }
}

/// Props for node widget content.
#[derive(Props, Clone, PartialEq)]
struct NodeWidgetContentProps {
    node: Node,
}

/// Renders the widget content inside a node.
#[component]
fn NodeWidgetContent(props: NodeWidgetContentProps) -> Element {
    let node = &props.node;
    // Adjust for smaller title bar in internal nodes (24px vs 32px)
    let content_y = 30.0;
    let content_width = node.size.width;
    let content_height = node.size.height - 36.0;

    match node.widget {
        NodeWidget::Label => rsx! {},
        NodeWidget::EqGraph => {
            // Render actual EQ widget
            let mut bands = use_signal(|| vec![
                EqBand {
                    index: 0,
                    used: true,
                    enabled: true,
                    frequency: 100.0,
                    gain: 3.0,
                    q: 0.7,
                    shape: EqBandShape::LowShelf,
                    ..Default::default()
                },
                EqBand {
                    index: 1,
                    used: true,
                    enabled: true,
                    frequency: 800.0,
                    gain: -2.0,
                    q: 1.5,
                    shape: EqBandShape::Bell,
                    ..Default::default()
                },
                EqBand {
                    index: 2,
                    used: true,
                    enabled: true,
                    frequency: 4000.0,
                    gain: 2.5,
                    q: 1.0,
                    shape: EqBandShape::HighShelf,
                    ..Default::default()
                },
            ]);

            rsx! {
                foreignObject {
                    x: "0",
                    y: "{content_y}",
                    width: "{content_width}",
                    height: "{content_height}",
                    div {
                        class: "w-full h-full",
                        style: "pointer-events: auto;",
                        EqGraph {
                            bands: bands,
                            db_range: 12.0,
                            show_grid: true,
                            fill_curve: true,
                            disabled: false,
                            on_band_change: move |(idx, band): (usize, EqBand)| {
                                bands.write()[idx] = band;
                            },
                            on_band_add: move |band: EqBand| {
                                let mut all_bands = bands();
                                all_bands.push(band);
                                bands.set(all_bands);
                            },
                            on_band_remove: move |idx: usize| {
                                let mut all_bands = bands();
                                all_bands.remove(idx);
                                bands.set(all_bands);
                            },
                        }
                    }
                }
            }
        }
        NodeWidget::CompressorGraph => {
            // Render actual compressor widget
            let params = CompressorParams {
                threshold: -20.0,
                ratio: 4.0,
                attack: 10.0,
                release: 100.0,
                knee: 6.0,
                makeup: 0.0,
                ..Default::default()
            };

            rsx! {
                foreignObject {
                    x: "0",
                    y: "{content_y}",
                    width: "{content_width}",
                    height: "{content_height}",
                    div {
                        class: "w-full h-full",
                        style: "pointer-events: auto;",
                        CompressorGraph {
                            params: params,
                            db_range: DbRange::default(),
                            show_grid: true,
                        }
                    }
                }
            }
        }
        _ => rsx! {
            text {
                x: "{node.size.width / 2.0}",
                y: "{content_y + content_height / 2.0}",
                fill: "#71717A",
                font_size: "10",
                text_anchor: "middle",
                "{node.short_label.as_deref().unwrap_or(&node.name)}"
            }
        }
    }
}
