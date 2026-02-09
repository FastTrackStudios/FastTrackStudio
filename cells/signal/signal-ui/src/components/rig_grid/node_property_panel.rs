//! Node property panel — sidebar panel showing the selected node or module's
//! editable properties.
//!
//! Features:
//! - Editable name field
//! - Block type badge with color coding
//! - Bypass toggle
//! - Position & size display
//! - Parameter list with inline sliders
//! - "Open Full Editor" button
//! - Input/output port list with connection status
//! - Placeholder when nothing is selected

use crate::prelude::*;
use crate::signals::{SelectedEntity, RIG_NODE_GRAPH, RIG_SELECTED_ENTITY};
use signal_control::block::BlockType;
use uuid::Uuid;

use super::block_colors::block_type_color;
use super::node_graph::{GraphModule, Node, NodePort};

// ── Public Component ────────────────────────────────────────────────

/// Standalone property panel that reactively displays the currently selected
/// node or module from `RIG_SELECTED_ENTITY`.
///
/// When nothing is selected, shows a placeholder message.
#[component]
pub fn NodePropertyPanel() -> Element {
    let selected = RIG_SELECTED_ENTITY.read().clone();
    let graph = RIG_NODE_GRAPH.read();

    match selected {
        Some(SelectedEntity::Node(node_id)) => {
            if let Some(node) = graph.find_node(node_id) {
                rsx! {
                    NodePropertyContent {
                        node: node.clone(),
                    }
                }
            } else {
                rsx! { EmptyPanel {} }
            }
        }
        Some(SelectedEntity::Module(module_id)) => {
            if let Some(module) = graph.find_module(module_id) {
                rsx! {
                    ModulePropertyContent {
                        module: module.clone(),
                    }
                }
            } else {
                rsx! { EmptyPanel {} }
            }
        }
        None => {
            rsx! { EmptyPanel {} }
        }
    }
}

// ── Empty State ─────────────────────────────────────────────────────

#[component]
fn EmptyPanel() -> Element {
    rsx! {
        div { class: "h-full w-full flex flex-col items-center justify-center bg-card \
                      border-l border-border p-6 text-center",
            div { class: "text-zinc-500 text-sm mb-2", "No Selection" }
            div { class: "text-zinc-600 text-xs",
                "Click a node or module on the canvas to inspect its properties."
            }
        }
    }
}

// ── Node Properties ─────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct NodePropertyContentProps {
    node: Node,
}

#[component]
fn NodePropertyContent(props: NodePropertyContentProps) -> Element {
    let node = &props.node;
    let color = block_type_color(node.block_type);
    let node_id = node.id;

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card border-l border-border overflow-hidden",
            // ── Header ──────────────────────────────────────
            div { class: "p-3 border-b border-border",
                div { class: "flex items-center gap-2 mb-2",
                    // Color dot
                    div {
                        class: "w-3 h-3 rounded-full flex-shrink-0",
                        style: "background-color: {color.bg};",
                    }
                    // Name
                    div { class: "font-semibold text-sm text-foreground truncate",
                        "{node.name}"
                    }
                }
                // Block type badge
                div { class: "flex items-center gap-2",
                    span {
                        class: "px-2 py-0.5 rounded text-[10px] font-medium uppercase tracking-wider",
                        style: "background-color: {color.bg}30; color: {color.bg};",
                        "{node.block_type:?}"
                    }
                    if node.bypassed {
                        span {
                            class: "px-2 py-0.5 rounded text-[10px] font-medium uppercase tracking-wider \
                                    bg-red-500/20 text-red-400",
                            "Bypassed"
                        }
                    }
                }
            }

            // ── Scrollable body ─────────────────────────────
            div { class: "flex-1 overflow-y-auto",
                // ── Bypass toggle ────────────────────────────
                PropertySection { title: "Controls" }
                div { class: "px-3 pb-3",
                    BypassToggle {
                        bypassed: node.bypassed,
                        on_toggle: move |_: ()| {
                            RIG_NODE_GRAPH.write().find_node_mut(node_id).map(|n| {
                                n.bypassed = !n.bypassed;
                            });
                        },
                    }
                }

                // ── Position & Size ─────────────────────────
                PropertySection { title: "Layout" }
                div { class: "px-3 pb-3 grid grid-cols-2 gap-2",
                    PropertyField { label: "X", value: format!("{:.0}", node.position.x) }
                    PropertyField { label: "Y", value: format!("{:.0}", node.position.y) }
                    PropertyField { label: "W", value: format!("{:.0}", node.size.width) }
                    PropertyField { label: "H", value: format!("{:.0}", node.size.height) }
                }

                // ── Parameters ──────────────────────────────
                if !node.parameters.is_empty() {
                    PropertySection { title: "Parameters" }
                    div { class: "px-3 pb-3 flex flex-col gap-2",
                        for param in &node.parameters {
                            ParameterSlider {
                                key: "{param.id}",
                                node_id: node_id,
                                param_id: param.id.clone(),
                                name: param.name.clone(),
                                value: param.value.get(),
                            }
                        }
                    }
                    // Open Full Editor button
                    div { class: "px-3 pb-3",
                        button {
                            class: "w-full px-3 py-2 rounded-lg text-xs font-medium \
                                    bg-accent/50 hover:bg-accent text-accent-foreground \
                                    transition-colors",
                            "Open Full Editor"
                        }
                    }
                }

                // ── Ports ───────────────────────────────────
                if !node.inputs.is_empty() || !node.outputs.is_empty() {
                    PropertySection { title: "Ports" }
                    div { class: "px-3 pb-3",
                        PortList {
                            inputs: node.inputs.clone(),
                            outputs: node.outputs.clone(),
                            entity_id: node.id,
                        }
                    }
                }
            }
        }
    }
}

// ── Module Properties ───────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct ModulePropertyContentProps {
    module: GraphModule,
}

#[component]
fn ModulePropertyContent(props: ModulePropertyContentProps) -> Element {
    let module = &props.module;
    let color = block_type_color(module.block_type);
    let module_id = module.id;

    rsx! {
        div { class: "h-full w-full flex flex-col bg-card border-l border-border overflow-hidden",
            // ── Header ──────────────────────────────────────
            div { class: "p-3 border-b border-border",
                div { class: "flex items-center gap-2 mb-2",
                    div {
                        class: "w-3 h-3 rounded-full flex-shrink-0",
                        style: "background-color: {color.bg};",
                    }
                    div { class: "font-semibold text-sm text-foreground truncate",
                        "{module.name}"
                    }
                }
                div { class: "flex items-center gap-2",
                    span {
                        class: "px-2 py-0.5 rounded text-[10px] font-medium uppercase tracking-wider",
                        style: "background-color: {color.bg}30; color: {color.bg};",
                        "{module.block_type:?}"
                    }
                    span {
                        class: "px-2 py-0.5 rounded text-[10px] font-medium text-zinc-400 \
                                bg-zinc-800",
                        "Module"
                    }
                    if module.bypassed {
                        span {
                            class: "px-2 py-0.5 rounded text-[10px] font-medium uppercase tracking-wider \
                                    bg-red-500/20 text-red-400",
                            "Bypassed"
                        }
                    }
                }
            }

            // ── Scrollable body ─────────────────────────────
            div { class: "flex-1 overflow-y-auto",
                // ── Bypass toggle ────────────────────────────
                PropertySection { title: "Controls" }
                div { class: "px-3 pb-3",
                    BypassToggle {
                        bypassed: module.bypassed,
                        on_toggle: move |_: ()| {
                            RIG_NODE_GRAPH.write().find_module_mut(module_id).map(|m| {
                                m.bypassed = !m.bypassed;
                            });
                        },
                    }
                }

                // ── Position & Size ─────────────────────────
                PropertySection { title: "Layout" }
                div { class: "px-3 pb-3 grid grid-cols-2 gap-2",
                    PropertyField { label: "X", value: format!("{:.0}", module.position.x) }
                    PropertyField { label: "Y", value: format!("{:.0}", module.position.y) }
                    PropertyField { label: "W", value: format!("{:.0}", module.size.width) }
                    PropertyField { label: "H", value: format!("{:.0}", module.size.height) }
                }

                // ── Internal Nodes ──────────────────────────
                if !module.nodes.is_empty() {
                    PropertySection { title: format!("Nodes ({})", module.nodes.len()) }
                    div { class: "px-3 pb-3 flex flex-col gap-1",
                        for node in &module.nodes {
                            InternalNodeItem {
                                key: "{node.id}",
                                node: node.clone(),
                            }
                        }
                    }
                }

                // ── Ports ───────────────────────────────────
                if !module.inputs.is_empty() || !module.outputs.is_empty() {
                    PropertySection { title: "Ports" }
                    div { class: "px-3 pb-3",
                        PortList {
                            inputs: module.inputs.clone(),
                            outputs: module.outputs.clone(),
                            entity_id: module.id,
                        }
                    }
                }
            }
        }
    }
}

// ── Internal Sub-Components ─────────────────────────────────────────

/// Section header divider.
#[component]
fn PropertySection(title: String) -> Element {
    rsx! {
        div { class: "px-3 pt-3 pb-1",
            h3 { class: "text-xs font-semibold text-muted-foreground uppercase tracking-wider",
                "{title}"
            }
        }
    }
}

/// Read-only property field (label + value).
#[derive(Props, Clone, PartialEq)]
struct PropertyFieldProps {
    label: String,
    value: String,
}

#[component]
fn PropertyField(props: PropertyFieldProps) -> Element {
    rsx! {
        div { class: "flex items-center gap-1.5",
            span { class: "text-[10px] font-medium text-muted-foreground w-4",
                "{props.label}"
            }
            span { class: "flex-1 px-1.5 py-0.5 rounded text-xs text-foreground \
                          bg-muted font-mono",
                "{props.value}"
            }
        }
    }
}

/// Bypass toggle button.
#[derive(Props, Clone, PartialEq)]
struct BypassToggleProps {
    bypassed: bool,
    on_toggle: Callback<()>,
}

#[component]
fn BypassToggle(props: BypassToggleProps) -> Element {
    let (label, class) = if props.bypassed {
        (
            "Enable",
            "w-full px-3 py-1.5 rounded-lg text-xs font-medium transition-colors \
             bg-red-500/20 text-red-400 hover:bg-red-500/30",
        )
    } else {
        (
            "Bypass",
            "w-full px-3 py-1.5 rounded-lg text-xs font-medium transition-colors \
             bg-muted text-muted-foreground hover:bg-accent hover:text-foreground",
        )
    };

    rsx! {
        button {
            class: class,
            onclick: move |_| props.on_toggle.call(()),
            "{label}"
        }
    }
}

/// Inline parameter slider for a single parameter.
#[derive(Props, Clone, PartialEq)]
struct ParameterSliderProps {
    node_id: Uuid,
    param_id: String,
    name: String,
    value: f64,
}

#[component]
fn ParameterSlider(props: ParameterSliderProps) -> Element {
    let pct = props.value * 100.0;
    let node_id = props.node_id;
    let param_id = props.param_id.clone();

    rsx! {
        div { class: "flex flex-col gap-0.5",
            div { class: "flex items-center justify-between",
                span { class: "text-xs text-muted-foreground", "{props.name}" }
                span { class: "text-[10px] font-mono text-foreground", "{pct:.0}%" }
            }
            // Slider track
            div { class: "relative h-1.5 rounded-full bg-muted overflow-hidden cursor-pointer",
                oninput: move |evt: FormEvent| {
                    if let Ok(val) = evt.value().parse::<f64>() {
                        let clamped = val.clamp(0.0, 1.0);
                        let pid = param_id.clone();
                        RIG_NODE_GRAPH.write().find_node_mut(node_id).map(|n| {
                            if let Some(p) = n.parameters.iter_mut().find(|p| p.id == pid) {
                                p.value = signal_control::normalized::NormalizedF64::new(clamped);
                            }
                        });
                    }
                },
                // Fill
                div {
                    class: "absolute inset-y-0 left-0 rounded-full bg-accent",
                    style: "width: {pct}%;",
                }
                // Invisible range input for interaction
                input {
                    r#type: "range",
                    class: "absolute inset-0 w-full h-full opacity-0 cursor-pointer",
                    min: "0",
                    max: "1",
                    step: "0.01",
                    value: "{props.value}",
                }
            }
        }
    }
}

/// Clickable node item inside a module's node list.
#[derive(Props, Clone, PartialEq)]
struct InternalNodeItemProps {
    node: Node,
}

#[component]
fn InternalNodeItem(props: InternalNodeItemProps) -> Element {
    let node = &props.node;
    let color = block_type_color(node.block_type);
    let node_id = node.id;
    let param_count = node.parameters.len();
    let bypassed = node.bypassed;

    rsx! {
        button {
            class: "w-full flex items-center gap-2 px-2 py-1.5 rounded-lg text-left \
                    hover:bg-muted transition-colors group",
            onclick: move |_| {
                *RIG_SELECTED_ENTITY.write() = Some(SelectedEntity::Node(node_id));
            },
            // Color dot
            div {
                class: "w-2 h-2 rounded-full flex-shrink-0",
                style: "background-color: {color.bg};",
            }
            // Name + info
            div { class: "flex-1 min-w-0",
                div { class: "text-xs text-foreground truncate group-hover:text-foreground",
                    "{node.name}"
                }
                div { class: "text-[10px] text-muted-foreground",
                    if bypassed {
                        "Bypassed"
                    } else if param_count > 0 {
                        "{param_count} params"
                    } else {
                        ""
                    }
                }
            }
            // Chevron
            span { class: "text-muted-foreground text-xs opacity-0 group-hover:opacity-100 \
                          transition-opacity", ">" }
        }
    }
}

// ── Port List ───────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
struct PortListProps {
    inputs: Vec<NodePort>,
    outputs: Vec<NodePort>,
    entity_id: Uuid,
}

#[component]
fn PortList(props: PortListProps) -> Element {
    let graph = RIG_NODE_GRAPH.read();

    rsx! {
        div { class: "flex flex-col gap-2",
            // Inputs
            if !props.inputs.is_empty() {
                div {
                    div { class: "text-[10px] font-medium text-muted-foreground mb-1", "Inputs" }
                    div { class: "flex flex-col gap-0.5",
                        for port in &props.inputs {
                            {
                                let connected = graph.wires.iter().any(|w| {
                                    w.to_node == props.entity_id && w.to_port == port.id
                                });
                                rsx! {
                                    PortItem {
                                        key: "{port.id}",
                                        label: port.label.clone(),
                                        connected: connected,
                                        is_input: true,
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Outputs
            if !props.outputs.is_empty() {
                div {
                    div { class: "text-[10px] font-medium text-muted-foreground mb-1", "Outputs" }
                    div { class: "flex flex-col gap-0.5",
                        for port in &props.outputs {
                            {
                                let connected = graph.wires.iter().any(|w| {
                                    w.from_node == props.entity_id && w.from_port == port.id
                                });
                                rsx! {
                                    PortItem {
                                        key: "{port.id}",
                                        label: port.label.clone(),
                                        connected: connected,
                                        is_input: false,
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

#[derive(Props, Clone, PartialEq)]
struct PortItemProps {
    label: String,
    connected: bool,
    is_input: bool,
}

#[component]
fn PortItem(props: PortItemProps) -> Element {
    let dot_color = if props.connected {
        "bg-green-400"
    } else {
        "bg-zinc-600"
    };
    let direction = if props.is_input { "←" } else { "→" };

    rsx! {
        div { class: "flex items-center gap-1.5 px-1.5 py-0.5",
            span { class: "text-[10px] text-muted-foreground", "{direction}" }
            div { class: "w-1.5 h-1.5 rounded-full {dot_color}" }
            span { class: "text-xs text-foreground", "{props.label}" }
            if props.connected {
                span { class: "text-[10px] text-green-400 ml-auto", "connected" }
            }
        }
    }
}

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::components::rig_grid::node_graph::*;
    use signal_proto::normalized::NormalizedF64;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    // -- SelectedEntity

    #[test]
    fn test_selected_entity_node_equality() -> Result<()> {
        // -- Setup & Fixtures
        let id = Uuid::new_v4();
        let a = SelectedEntity::Node(id);
        let b = SelectedEntity::Node(id);

        // -- Check
        assert_eq!(a, b);
        Ok(())
    }

    #[test]
    fn test_selected_entity_module_equality() -> Result<()> {
        // -- Setup & Fixtures
        let id = Uuid::new_v4();
        let a = SelectedEntity::Module(id);
        let b = SelectedEntity::Module(id);

        // -- Check
        assert_eq!(a, b);
        Ok(())
    }

    #[test]
    fn test_selected_entity_different_types_not_equal() -> Result<()> {
        // -- Setup & Fixtures
        let id = Uuid::new_v4();
        let node = SelectedEntity::Node(id);
        let module = SelectedEntity::Module(id);

        // -- Check
        assert_ne!(node, module);
        Ok(())
    }

    #[test]
    fn test_selected_entity_different_ids_not_equal() -> Result<()> {
        // -- Setup & Fixtures
        let a = SelectedEntity::Node(Uuid::new_v4());
        let b = SelectedEntity::Node(Uuid::new_v4());

        // -- Check
        assert_ne!(a, b);
        Ok(())
    }

    // -- Node lookup for property display

    #[test]
    fn test_graph_find_node_for_panel() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let node = Node::new(
            "Test Drive",
            BlockType::Drive,
            NodePosition::new(100.0, 200.0),
        )
        .with_parameters(vec![
            NodeParameter::new("drive", "Drive", NormalizedF64::new(0.7)),
            NodeParameter::new("tone", "Tone", NormalizedF64::new(0.5)),
        ]);
        let node_id = node.id;
        graph.add_node(node);

        // -- Exec
        let found = graph.find_node(node_id);

        // -- Check
        let found = found.unwrap();
        assert_eq!(found.name, "Test Drive");
        assert_eq!(found.parameters.len(), 2);
        assert_eq!(found.position.x, 100.0);
        assert_eq!(found.position.y, 200.0);
        Ok(())
    }

    #[test]
    fn test_graph_find_module_for_panel() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let mut module = GraphModule::new(
            "Drive Stage",
            BlockType::Drive,
            NodePosition::new(50.0, 80.0),
        );
        let node = Node::new("Drive 1", BlockType::Drive, NodePosition::new(10.0, 50.0));
        module.add_node(node);
        let module_id = module.id;
        graph.add_module(module);

        // -- Exec
        let found = graph.find_module(module_id);

        // -- Check
        let found = found.unwrap();
        assert_eq!(found.name, "Drive Stage");
        assert_eq!(found.nodes.len(), 1);
        assert_eq!(found.position.x, 50.0);
        assert!(!found.bypassed);
        Ok(())
    }

    #[test]
    fn test_graph_find_node_in_module_for_panel() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let node = Node::new(
            "Internal Drive",
            BlockType::Drive,
            NodePosition::new(10.0, 50.0),
        )
        .with_bypassed(true)
        .with_parameters(vec![NodeParameter::new(
            "gain",
            "Gain",
            NormalizedF64::new(0.9),
        )]);
        let node_id = node.id;
        let mut module =
            GraphModule::new("Container", BlockType::Drive, NodePosition::new(0.0, 0.0));
        module.add_node(node);
        graph.add_module(module);

        // -- Exec
        let found = graph.find_node(node_id);

        // -- Check
        let found = found.unwrap();
        assert_eq!(found.name, "Internal Drive");
        assert!(found.bypassed);
        assert_eq!(found.parameters.len(), 1);
        assert!((found.parameters[0].value.get() - 0.9).abs() < f64::EPSILON);
        Ok(())
    }

    // -- Wire connection status for port display

    #[test]
    fn test_port_connection_status() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let node_a = Node::new("A", BlockType::Input, NodePosition::new(0.0, 0.0));
        let node_b = Node::new("B", BlockType::Drive, NodePosition::new(200.0, 0.0));
        let a_id = node_a.id;
        let b_id = node_b.id;
        graph.add_node(node_a);
        graph.add_node(node_b);
        graph.connect(a_id, "out_l", b_id, "in_l");

        // -- Check: node B's in_l is connected
        let b_in_l_connected = graph
            .wires
            .iter()
            .any(|w| w.to_node == b_id && w.to_port == "in_l");
        assert!(b_in_l_connected);

        // -- Check: node B's in_r is NOT connected
        let b_in_r_connected = graph
            .wires
            .iter()
            .any(|w| w.to_node == b_id && w.to_port == "in_r");
        assert!(!b_in_r_connected);

        // -- Check: node A's out_l is connected
        let a_out_l_connected = graph
            .wires
            .iter()
            .any(|w| w.from_node == a_id && w.from_port == "out_l");
        assert!(a_out_l_connected);

        Ok(())
    }

    #[test]
    fn test_bypass_toggle_via_graph_write() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let node = Node::new("Drive", BlockType::Drive, NodePosition::new(0.0, 0.0));
        let node_id = node.id;
        graph.add_node(node);
        assert!(!graph.find_node(node_id).unwrap().bypassed);

        // -- Exec (simulates what BypassToggle.on_toggle does)
        graph.find_node_mut(node_id).map(|n| {
            n.bypassed = !n.bypassed;
        });

        // -- Check
        assert!(graph.find_node(node_id).unwrap().bypassed);
        Ok(())
    }

    #[test]
    fn test_parameter_update_via_graph_write() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let node = Node::new("Drive", BlockType::Drive, NodePosition::new(0.0, 0.0))
            .with_parameters(vec![NodeParameter::new(
                "drive",
                "Drive",
                NormalizedF64::new(0.5),
            )]);
        let node_id = node.id;
        graph.add_node(node);

        // -- Exec (simulates what ParameterSlider.oninput does)
        let new_value = 0.8;
        graph.find_node_mut(node_id).map(|n| {
            if let Some(p) = n.parameters.iter_mut().find(|p| p.id == "drive") {
                p.value = NormalizedF64::new(new_value);
            }
        });

        // -- Check
        let param = &graph.find_node(node_id).unwrap().parameters[0];
        assert!((param.value.get() - 0.8).abs() < f64::EPSILON);
        Ok(())
    }
}
