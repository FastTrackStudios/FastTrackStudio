//! Node-based signal flow model.
//!
//! Represents audio processing nodes positioned on an infinite 2D canvas
//! with wire connections between them. Similar to Gig Performer, Reaktor, etc.

use crate::block::BlockType;
use uuid::Uuid;

/// 2D position on the canvas (in canvas coordinates).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NodePosition {
    pub x: f64,
    pub y: f64,
}

impl NodePosition {
    pub const fn new(x: f64, y: f64) -> Self {
        Self { x, y }
    }
}

/// Size of a node (in canvas coordinates).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct NodeSize {
    pub width: f64,
    pub height: f64,
}

impl NodeSize {
    pub const fn new(width: f64, height: f64) -> Self {
        Self { width, height }
    }

    /// Small node (160x80).
    pub const fn small() -> Self {
        Self::new(160.0, 80.0)
    }

    /// Medium node (220x120).
    pub const fn medium() -> Self {
        Self::new(220.0, 120.0)
    }

    /// Large node with widget visualization (320x180).
    pub const fn large() -> Self {
        Self::new(320.0, 180.0)
    }

    /// Extra large for detailed widgets (400x220).
    pub const fn xlarge() -> Self {
        Self::new(400.0, 220.0)
    }
}

impl Default for NodeSize {
    fn default() -> Self {
        Self::medium()
    }
}

/// Widget type to render inside a node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum NodeWidget {
    /// Simple label/text display.
    #[default]
    Label,
    /// EQ graph visualization.
    EqGraph,
    /// Compressor graph visualization.
    CompressorGraph,
    /// Gate graph visualization.
    GateGraph,
    /// Delay graph/visualization.
    DelayGraph,
    /// Reverb visualization.
    ReverbGraph,
    /// Amp/cab visualization.
    AmpCab,
    /// Drive/saturation visualization.
    DriveGraph,
    /// Modulation visualization (chorus, phaser, etc.).
    ModulationGraph,
    /// Tuner display.
    Tuner,
    /// Looper controls.
    Looper,
}

/// Input/output port on a node.
#[derive(Debug, Clone, PartialEq)]
pub struct NodePort {
    /// Port identifier (unique within the node).
    pub id: String,
    /// Display label.
    pub label: String,
    /// Whether this is an input (true) or output (false).
    pub is_input: bool,
    /// Port color for visual grouping (optional).
    pub color: Option<String>,
}

impl NodePort {
    pub fn input(id: impl Into<String>, label: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            label: label.into(),
            is_input: true,
            color: None,
        }
    }

    pub fn output(id: impl Into<String>, label: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            label: label.into(),
            is_input: false,
            color: None,
        }
    }

    pub fn with_color(mut self, color: impl Into<String>) -> Self {
        self.color = Some(color.into());
        self
    }
}

/// A signal processing node on the canvas.
#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    /// Unique node ID.
    pub id: Uuid,
    /// Node name for display.
    pub name: String,
    /// Short label (optional, for compact display).
    pub short_label: Option<String>,
    /// Block type for color coding and behavior.
    pub block_type: BlockType,
    /// Position on the canvas.
    pub position: NodePosition,
    /// Node size.
    pub size: NodeSize,
    /// Widget to render inside.
    pub widget: NodeWidget,
    /// Whether the node is bypassed.
    pub bypassed: bool,
    /// Input ports.
    pub inputs: Vec<NodePort>,
    /// Output ports.
    pub outputs: Vec<NodePort>,
}

impl Node {
    pub fn new(
        name: impl Into<String>,
        block_type: BlockType,
        position: NodePosition,
    ) -> Self {
        let name = name.into();
        Self {
            id: Uuid::new_v4(),
            name: name.clone(),
            short_label: None,
            block_type,
            position,
            size: NodeSize::default(),
            widget: NodeWidget::Label,
            bypassed: false,
            // Default stereo in/out
            inputs: vec![
                NodePort::input("in_l", "In L"),
                NodePort::input("in_r", "In R"),
            ],
            outputs: vec![
                NodePort::output("out_l", "Out L"),
                NodePort::output("out_r", "Out R"),
            ],
        }
    }

    pub fn with_size(mut self, size: NodeSize) -> Self {
        self.size = size;
        self
    }

    pub fn with_widget(mut self, widget: NodeWidget) -> Self {
        self.widget = widget;
        self
    }

    pub fn with_bypassed(mut self, bypassed: bool) -> Self {
        self.bypassed = bypassed;
        self
    }

    pub fn with_short_label(mut self, label: impl Into<String>) -> Self {
        self.short_label = Some(label.into());
        self
    }

    pub fn with_ports(mut self, inputs: Vec<NodePort>, outputs: Vec<NodePort>) -> Self {
        self.inputs = inputs;
        self.outputs = outputs;
        self
    }

    /// Check if a point (in canvas coordinates) is inside this node.
    pub fn contains_point(&self, x: f64, y: f64) -> bool {
        x >= self.position.x
            && x <= self.position.x + self.size.width
            && y >= self.position.y
            && y <= self.position.y + self.size.height
    }

    /// Get the center position of this node.
    pub fn center(&self) -> NodePosition {
        NodePosition::new(
            self.position.x + self.size.width / 2.0,
            self.position.y + self.size.height / 2.0,
        )
    }

    /// Get the position of a port (for wire connection).
    pub fn port_position(&self, port_id: &str, is_input: bool) -> Option<NodePosition> {
        let ports = if is_input { &self.inputs } else { &self.outputs };
        let port_index = ports.iter().position(|p| p.id == port_id)?;
        let port_count = ports.len();

        // Distribute ports evenly along the left (input) or right (output) edge
        let port_spacing = self.size.height / (port_count + 1) as f64;
        let port_y = self.position.y + port_spacing * (port_index + 1) as f64;
        let port_x = if is_input {
            self.position.x // Left edge
        } else {
            self.position.x + self.size.width // Right edge
        };

        Some(NodePosition::new(port_x, port_y))
    }
}

/// Wire connection between two node ports.
#[derive(Debug, Clone, PartialEq)]
pub struct Wire {
    /// Unique wire ID.
    pub id: Uuid,
    /// Source node ID.
    pub from_node: Uuid,
    /// Source port ID.
    pub from_port: String,
    /// Destination node ID.
    pub to_node: Uuid,
    /// Destination port ID.
    pub to_port: String,
    /// Wire color (optional, overrides default).
    pub color: Option<String>,
}

impl Wire {
    pub fn new(
        from_node: Uuid,
        from_port: impl Into<String>,
        to_node: Uuid,
        to_port: impl Into<String>,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            from_node,
            from_port: from_port.into(),
            to_node,
            to_port: to_port.into(),
            color: None,
        }
    }

    pub fn with_color(mut self, color: impl Into<String>) -> Self {
        self.color = Some(color.into());
        self
    }
}

/// A module container that groups multiple nodes.
#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    /// Unique module ID.
    pub id: Uuid,
    /// Module name for display.
    pub name: String,
    /// Block type for color coding.
    pub block_type: BlockType,
    /// Position on the canvas.
    pub position: NodePosition,
    /// Module size (container bounds).
    pub size: NodeSize,
    /// Whether the module is bypassed.
    pub bypassed: bool,
    /// Nodes inside this module.
    pub nodes: Vec<Node>,
    /// Wires connecting nodes inside this module.
    pub internal_wires: Vec<Wire>,
    /// Input ports (module-level).
    pub inputs: Vec<NodePort>,
    /// Output ports (module-level).
    pub outputs: Vec<NodePort>,
}

impl Module {
    pub fn new(
        name: impl Into<String>,
        block_type: BlockType,
        position: NodePosition,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            block_type,
            position,
            size: NodeSize::new(500.0, 300.0), // Default module size
            bypassed: false,
            nodes: Vec::new(),
            internal_wires: Vec::new(),
            inputs: vec![
                NodePort::input("in_l", "In L"),
                NodePort::input("in_r", "In R"),
            ],
            outputs: vec![
                NodePort::output("out_l", "Out L"),
                NodePort::output("out_r", "Out R"),
            ],
        }
    }

    pub fn with_size(mut self, size: NodeSize) -> Self {
        self.size = size;
        self
    }

    pub fn with_bypassed(mut self, bypassed: bool) -> Self {
        self.bypassed = bypassed;
        self
    }

    pub fn with_ports(mut self, inputs: Vec<NodePort>, outputs: Vec<NodePort>) -> Self {
        self.inputs = inputs;
        self.outputs = outputs;
        self
    }

    pub fn add_node(&mut self, node: Node) -> Uuid {
        let id = node.id;
        self.nodes.push(node);
        id
    }

    pub fn add_wire(&mut self, wire: Wire) {
        self.internal_wires.push(wire);
    }

    /// Check if a point (in canvas coordinates) is inside this module.
    pub fn contains_point(&self, x: f64, y: f64) -> bool {
        x >= self.position.x
            && x <= self.position.x + self.size.width
            && y >= self.position.y
            && y <= self.position.y + self.size.height
    }

    /// Check if a point is in the title bar (for dragging).
    pub fn title_bar_contains(&self, x: f64, y: f64) -> bool {
        x >= self.position.x
            && x <= self.position.x + self.size.width
            && y >= self.position.y
            && y <= self.position.y + 40.0 // Title bar height
    }

    /// Get the position of a port (for wire connection).
    pub fn port_position(&self, port_id: &str, is_input: bool) -> Option<NodePosition> {
        let ports = if is_input { &self.inputs } else { &self.outputs };
        let port_index = ports.iter().position(|p| p.id == port_id)?;
        let port_count = ports.len();

        let port_spacing = self.size.height / (port_count + 1) as f64;
        let port_y = self.position.y + port_spacing * (port_index + 1) as f64;
        let port_x = if is_input {
            self.position.x
        } else {
            self.position.x + self.size.width
        };

        Some(NodePosition::new(port_x, port_y))
    }

    /// Find a node inside this module by ID.
    pub fn find_node(&self, id: Uuid) -> Option<&Node> {
        self.nodes.iter().find(|n| n.id == id)
    }

    /// Find a node inside this module by ID (mutable).
    pub fn find_node_mut(&mut self, id: Uuid) -> Option<&mut Node> {
        self.nodes.iter_mut().find(|n| n.id == id)
    }

    /// Calculate and set the module size to fit all internal nodes with padding.
    pub fn auto_size(&mut self, padding: f64) {
        if self.nodes.is_empty() {
            return;
        }

        // Find bounding box of all nodes
        let mut min_x = f64::MAX;
        let mut min_y = f64::MAX;
        let mut max_x = f64::MIN;
        let mut max_y = f64::MIN;

        for node in &self.nodes {
            min_x = min_x.min(node.position.x);
            min_y = min_y.min(node.position.y);
            max_x = max_x.max(node.position.x + node.size.width);
            max_y = max_y.max(node.position.y + node.size.height);
        }

        // Calculate module size with padding
        // Account for title bar height (40px) in the top padding
        let content_width = max_x - min_x + (padding * 2.0);
        let content_height = max_y - min_y + padding + 50.0; // Extra padding at bottom, account for title

        self.size = NodeSize::new(content_width, content_height);
    }
}

/// The complete node graph.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct NodeGraph {
    /// Modules in the graph.
    pub modules: Vec<Module>,
    /// Standalone nodes (not in any module).
    pub nodes: Vec<Node>,
    /// Wires connecting modules/nodes.
    pub wires: Vec<Wire>,
}

impl NodeGraph {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a module to the graph.
    pub fn add_module(&mut self, module: Module) -> Uuid {
        let id = module.id;
        self.modules.push(module);
        id
    }

    /// Find a module by ID.
    pub fn find_module(&self, id: Uuid) -> Option<&Module> {
        self.modules.iter().find(|m| m.id == id)
    }

    /// Find a module by ID (mutable).
    pub fn find_module_mut(&mut self, id: Uuid) -> Option<&mut Module> {
        self.modules.iter_mut().find(|m| m.id == id)
    }

    /// Find a module at a given position.
    pub fn module_at(&self, x: f64, y: f64) -> Option<&Module> {
        // Search in reverse order (top module first)
        self.modules.iter().rev().find(|m| m.contains_point(x, y))
    }

    /// Add a node to the graph (standalone, not in a module).
    pub fn add_node(&mut self, node: Node) -> Uuid {
        let id = node.id;
        self.nodes.push(node);
        id
    }

    /// Remove a node by ID.
    pub fn remove_node(&mut self, id: Uuid) {
        self.nodes.retain(|n| n.id != id);
        // Remove all wires connected to this node
        self.wires.retain(|w| w.from_node != id && w.to_node != id);
    }

    /// Find a node by ID (searches both standalone nodes and nodes in modules).
    pub fn find_node(&self, id: Uuid) -> Option<&Node> {
        // First check standalone nodes
        if let Some(node) = self.nodes.iter().find(|n| n.id == id) {
            return Some(node);
        }
        // Then check nodes inside modules
        for module in &self.modules {
            if let Some(node) = module.find_node(id) {
                return Some(node);
            }
        }
        None
    }

    /// Find a node by ID (mutable, searches both standalone nodes and nodes in modules).
    pub fn find_node_mut(&mut self, id: Uuid) -> Option<&mut Node> {
        // First check standalone nodes
        if let Some(node) = self.nodes.iter_mut().find(|n| n.id == id) {
            return Some(node);
        }
        // Then check nodes inside modules
        for module in &mut self.modules {
            if let Some(node) = module.find_node_mut(id) {
                return Some(node);
            }
        }
        None
    }

    /// Find a node at a given position.
    pub fn node_at(&self, x: f64, y: f64) -> Option<&Node> {
        // Search in reverse order (top node first)
        self.nodes.iter().rev().find(|n| n.contains_point(x, y))
    }

    /// Add a wire between two nodes.
    pub fn connect(
        &mut self,
        from_node: Uuid,
        from_port: impl Into<String>,
        to_node: Uuid,
        to_port: impl Into<String>,
    ) -> Uuid {
        let wire = Wire::new(from_node, from_port, to_node, to_port);
        let id = wire.id;
        self.wires.push(wire);
        id
    }

    /// Remove a wire by ID.
    pub fn disconnect(&mut self, id: Uuid) {
        self.wires.retain(|w| w.id != id);
    }

    /// Create a comprehensive guitar rig node graph with all modules.
    pub fn sample_guitar_rig() -> Self {
        let mut graph = Self::new();

        let mut y_offset = 100.0;

        // === SOURCE MODULE (contains Guitar Input, Input Gate, Input Volume) ===
        let mut source_module = Module::new("Source", BlockType::Input, NodePosition::new(50.0, y_offset))
            .with_size(NodeSize::new(300.0, 280.0));

        let input = Node::new("Guitar Input", BlockType::Input, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::small())
            .with_short_label("IN");
        let input_id = source_module.add_node(input);

        let input_gate = Node::new("Gate", BlockType::Gate, NodePosition::new(20.0, 140.0))
            .with_size(NodeSize::small());
        let input_gate_id = source_module.add_node(input_gate);

        let input_vol = Node::new("Volume", BlockType::Volume, NodePosition::new(20.0, 210.0))
            .with_size(NodeSize::small());
        let input_vol_id = source_module.add_node(input_vol);

        // Internal routing within source module
        source_module.add_wire(Wire::new(input_id, "out_l", input_gate_id, "in_l"));
        source_module.add_wire(Wire::new(input_id, "out_r", input_gate_id, "in_r"));
        source_module.add_wire(Wire::new(input_gate_id, "out_l", input_vol_id, "in_l"));
        source_module.add_wire(Wire::new(input_gate_id, "out_r", input_vol_id, "in_r"));

        source_module.auto_size(20.0); // 20px padding
        let source_id = graph.add_module(source_module);

        // === EQ BLOCK (standalone module) ===
        let mut eq_module = Module::new("EQ", BlockType::Eq, NodePosition::new(380.0, y_offset));
        let eq = Node::new("EQ", BlockType::Eq, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::xlarge())
            .with_widget(NodeWidget::EqGraph);
        eq_module.add_node(eq);
        eq_module.auto_size(20.0);
        let eq_id = graph.add_module(eq_module);

        // === DYNAMICS MODULE (standalone) ===
        let mut dynamics_module = Module::new("Dynamics", BlockType::Compressor, NodePosition::new(830.0, y_offset));
        let comp = Node::new("Compressor", BlockType::Compressor, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::large())
            .with_widget(NodeWidget::CompressorGraph);
        dynamics_module.add_node(comp);
        dynamics_module.auto_size(20.0);
        let dynamics_id = graph.add_module(dynamics_module);

        // === SPECIAL MODULE (contains Envelope, Wah, Pitch, Doubler) ===
        y_offset += 280.0;
        let mut special_module = Module::new("Special", BlockType::Modulation, NodePosition::new(50.0, y_offset))
            .with_size(NodeSize::new(950.0, 150.0));

        let envelope = Node::new("Envelope", BlockType::Modulation, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::medium());
        let envelope_id = special_module.add_node(envelope);

        let wah = Node::new("Wah", BlockType::Modulation, NodePosition::new(250.0, 50.0))
            .with_size(NodeSize::medium());
        let wah_id = special_module.add_node(wah);

        let pitch = Node::new("Pitch", BlockType::Pitch, NodePosition::new(480.0, 50.0))
            .with_size(NodeSize::medium());
        let pitch_id = special_module.add_node(pitch);

        let doubler = Node::new("Doubler", BlockType::Modulation, NodePosition::new(710.0, 50.0))
            .with_size(NodeSize::medium());
        let doubler_id = special_module.add_node(doubler);

        // Internal routing (serial chain)
        special_module.add_wire(Wire::new(envelope_id, "out_l", wah_id, "in_l"));
        special_module.add_wire(Wire::new(envelope_id, "out_r", wah_id, "in_r"));
        special_module.add_wire(Wire::new(wah_id, "out_l", pitch_id, "in_l"));
        special_module.add_wire(Wire::new(wah_id, "out_r", pitch_id, "in_r"));
        special_module.add_wire(Wire::new(pitch_id, "out_l", doubler_id, "in_l"));
        special_module.add_wire(Wire::new(pitch_id, "out_r", doubler_id, "in_r"));

        special_module.auto_size(20.0);
        let special_id = graph.add_module(special_module);

        // === DRIVE MODULE (contains Boost, Drive 1, Drive 2, Drive 3) ===
        y_offset += 180.0;
        let mut drive_module = Module::new("Drive", BlockType::Drive, NodePosition::new(50.0, y_offset))
            .with_size(NodeSize::new(1100.0, 180.0));

        let boost = Node::new("Boost", BlockType::Drive, NodePosition::new(20.0, 60.0))
            .with_size(NodeSize::small());
        let boost_id = drive_module.add_node(boost);

        let drive1 = Node::new("Drive 1", BlockType::Drive, NodePosition::new(200.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::DriveGraph);
        let drive1_id = drive_module.add_node(drive1);

        let drive2 = Node::new("Drive 2", BlockType::Drive, NodePosition::new(450.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::DriveGraph);
        let drive2_id = drive_module.add_node(drive2);

        let drive3 = Node::new("Drive 3", BlockType::Drive, NodePosition::new(700.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::DriveGraph);
        let drive3_id = drive_module.add_node(drive3);

        // Internal routing (serial chain)
        drive_module.add_wire(Wire::new(boost_id, "out_l", drive1_id, "in_l"));
        drive_module.add_wire(Wire::new(boost_id, "out_r", drive1_id, "in_r"));
        drive_module.add_wire(Wire::new(drive1_id, "out_l", drive2_id, "in_l"));
        drive_module.add_wire(Wire::new(drive1_id, "out_r", drive2_id, "in_r"));
        drive_module.add_wire(Wire::new(drive2_id, "out_l", drive3_id, "in_l"));
        drive_module.add_wire(Wire::new(drive2_id, "out_r", drive3_id, "in_r"));

        drive_module.auto_size(20.0);
        let drive_id = graph.add_module(drive_module);

        // === VOLUME PEDAL (standalone) ===
        y_offset += 210.0;
        let mut vol_pedal_module = Module::new("Volume", BlockType::Volume, NodePosition::new(50.0, y_offset));
        let vol_pedal = Node::new("Volume", BlockType::Volume, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::small());
        vol_pedal_module.add_node(vol_pedal);
        vol_pedal_module.auto_size(20.0);
        let vol_pedal_id = graph.add_module(vol_pedal_module);

        // === PRE-FX MODULE (contains Pre Delay, Spring Verb) ===
        let mut prefx_module = Module::new("Pre-FX", BlockType::Delay, NodePosition::new(260.0, y_offset));

        let pre_delay = Node::new("Delay", BlockType::Delay, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::large())
            .with_widget(NodeWidget::DelayGraph);
        let pre_delay_id = prefx_module.add_node(pre_delay);

        let spring_verb = Node::new("Spring", BlockType::Reverb, NodePosition::new(350.0, 50.0))
            .with_size(NodeSize::large())
            .with_widget(NodeWidget::ReverbGraph);
        let spring_verb_id = prefx_module.add_node(spring_verb);

        // Internal routing
        prefx_module.add_wire(Wire::new(pre_delay_id, "out_l", spring_verb_id, "in_l"));
        prefx_module.add_wire(Wire::new(pre_delay_id, "out_r", spring_verb_id, "in_r"));

        prefx_module.auto_size(20.0);
        let prefx_id = graph.add_module(prefx_module);

        // === AMP/CAB MODULE (contains 2 Amps, 2 Cabinets, Room Send) ===
        // Layout: Parallel paths shown vertically
        //   Amp1  Cab1 ↘
        //                Room
        //   Amp2  Cab2 ↗
        y_offset += 230.0;
        let mut ampcab_module = Module::new("Amp/Cab", BlockType::Amp, NodePosition::new(50.0, y_offset))
            .with_size(NodeSize::new(870.0, 300.0));

        // Parallel path 1 (top): Amp1 -> Cab1
        let amp1 = Node::new("Amp 1", BlockType::Amp, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::AmpCab);
        let amp1_id = ampcab_module.add_node(amp1);

        let cab1 = Node::new("Cab 1", BlockType::Cabinet, NodePosition::new(260.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::AmpCab);
        let cab1_id = ampcab_module.add_node(cab1);

        // Parallel path 2 (bottom): Amp2 -> Cab2
        let amp2 = Node::new("Amp 2", BlockType::Amp, NodePosition::new(20.0, 170.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::AmpCab);
        let amp2_id = ampcab_module.add_node(amp2);

        let cab2 = Node::new("Cab 2", BlockType::Cabinet, NodePosition::new(260.0, 170.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::AmpCab);
        let cab2_id = ampcab_module.add_node(cab2);

        // Room send receives both cab outputs (centered vertically)
        let room_send = Node::new("Room", BlockType::Send, NodePosition::new(510.0, 110.0))
            .with_size(NodeSize::medium());
        let room_send_id = ampcab_module.add_node(room_send);

        // Internal routing: Amp1 -> Cab1 -> Room
        ampcab_module.add_wire(Wire::new(amp1_id, "out_l", cab1_id, "in_l"));
        ampcab_module.add_wire(Wire::new(amp1_id, "out_r", cab1_id, "in_r"));
        ampcab_module.add_wire(Wire::new(cab1_id, "out_l", room_send_id, "in_l"));
        ampcab_module.add_wire(Wire::new(cab1_id, "out_r", room_send_id, "in_r"));

        // Internal routing: Amp2 -> Cab2 -> Room
        ampcab_module.add_wire(Wire::new(amp2_id, "out_l", cab2_id, "in_l"));
        ampcab_module.add_wire(Wire::new(amp2_id, "out_r", cab2_id, "in_r"));
        ampcab_module.add_wire(Wire::new(cab2_id, "out_l", room_send_id, "in_l"));
        ampcab_module.add_wire(Wire::new(cab2_id, "out_r", room_send_id, "in_r"));

        ampcab_module.auto_size(20.0);
        let ampcab_id = graph.add_module(ampcab_module);

        // === POST EQ (standalone) ===
        y_offset += 210.0;
        let mut post_eq_module = Module::new("Post EQ", BlockType::Eq, NodePosition::new(50.0, y_offset));
        let post_eq = Node::new("EQ", BlockType::Eq, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::xlarge())
            .with_widget(NodeWidget::EqGraph);
        post_eq_module.add_node(post_eq);
        post_eq_module.auto_size(20.0);
        let post_eq_id = graph.add_module(post_eq_module);

        // === MODULATION MODULE (contains Chorus, Flanger, Phaser) ===
        y_offset += 260.0;
        let mut mod_module = Module::new("Modulation", BlockType::Modulation, NodePosition::new(50.0, y_offset))
            .with_size(NodeSize::new(730.0, 160.0));

        let chorus = Node::new("Chorus", BlockType::Modulation, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::ModulationGraph);
        let chorus_id = mod_module.add_node(chorus);

        let flanger = Node::new("Flanger", BlockType::Modulation, NodePosition::new(260.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::ModulationGraph);
        let flanger_id = mod_module.add_node(flanger);

        let phaser = Node::new("Phaser", BlockType::Modulation, NodePosition::new(500.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::ModulationGraph);
        let phaser_id = mod_module.add_node(phaser);

        // Internal routing (serial chain)
        mod_module.add_wire(Wire::new(chorus_id, "out_l", flanger_id, "in_l"));
        mod_module.add_wire(Wire::new(chorus_id, "out_r", flanger_id, "in_r"));
        mod_module.add_wire(Wire::new(flanger_id, "out_l", phaser_id, "in_l"));
        mod_module.add_wire(Wire::new(flanger_id, "out_r", phaser_id, "in_r"));

        mod_module.auto_size(20.0);
        let mod_id = graph.add_module(mod_module);

        // === TIME MODULE (contains Delay, Reverb, Freeze) ===
        y_offset += 190.0;
        let mut time_module = Module::new("Time", BlockType::Delay, NodePosition::new(50.0, y_offset));

        let delay = Node::new("Delay", BlockType::Delay, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::large())
            .with_widget(NodeWidget::DelayGraph);
        let delay_id = time_module.add_node(delay);

        let reverb = Node::new("Reverb", BlockType::Reverb, NodePosition::new(360.0, 50.0))
            .with_size(NodeSize::large())
            .with_widget(NodeWidget::ReverbGraph);
        let reverb_id = time_module.add_node(reverb);

        let freeze = Node::new("Freeze", BlockType::Freeze, NodePosition::new(710.0, 60.0))
            .with_size(NodeSize::medium());
        let freeze_id = time_module.add_node(freeze);

        // Internal routing
        time_module.add_wire(Wire::new(delay_id, "out_l", reverb_id, "in_l"));
        time_module.add_wire(Wire::new(delay_id, "out_r", reverb_id, "in_r"));
        time_module.add_wire(Wire::new(reverb_id, "out_l", freeze_id, "in_l"));
        time_module.add_wire(Wire::new(reverb_id, "out_r", freeze_id, "in_r"));

        time_module.auto_size(20.0);
        let time_id = graph.add_module(time_module);

        // === MOTION MODULE (contains Tremolo, Vibrato, Rotary) ===
        y_offset += 230.0;
        let mut motion_module = Module::new("Motion", BlockType::Tremolo, NodePosition::new(50.0, y_offset))
            .with_size(NodeSize::new(730.0, 160.0));

        let tremolo = Node::new("Tremolo", BlockType::Tremolo, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::ModulationGraph);
        let tremolo_id = motion_module.add_node(tremolo);

        let vibrato = Node::new("Vibrato", BlockType::Modulation, NodePosition::new(260.0, 50.0))
            .with_size(NodeSize::medium());
        let vibrato_id = motion_module.add_node(vibrato);

        let rotary = Node::new("Rotary", BlockType::Modulation, NodePosition::new(500.0, 50.0))
            .with_size(NodeSize::medium());
        let rotary_id = motion_module.add_node(rotary);

        // Internal routing
        motion_module.add_wire(Wire::new(tremolo_id, "out_l", vibrato_id, "in_l"));
        motion_module.add_wire(Wire::new(tremolo_id, "out_r", vibrato_id, "in_r"));
        motion_module.add_wire(Wire::new(vibrato_id, "out_l", rotary_id, "in_l"));
        motion_module.add_wire(Wire::new(vibrato_id, "out_r", rotary_id, "in_r"));

        motion_module.auto_size(20.0);
        let motion_id = graph.add_module(motion_module);

        // === MASTER MODULE (contains Master EQ, Multiband Comp, Output) ===
        y_offset += 190.0;
        let mut master_module = Module::new("Master", BlockType::Eq, NodePosition::new(50.0, y_offset));

        let master_eq = Node::new("Master EQ", BlockType::Eq, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::xlarge())
            .with_widget(NodeWidget::EqGraph);
        let master_eq_id = master_module.add_node(master_eq);

        let master_comp = Node::new("Multiband", BlockType::Compressor, NodePosition::new(440.0, 50.0))
            .with_size(NodeSize::large())
            .with_widget(NodeWidget::CompressorGraph);
        let master_comp_id = master_module.add_node(master_comp);

        let output = Node::new("Output", BlockType::Volume, NodePosition::new(790.0, 90.0))
            .with_size(NodeSize::small())
            .with_short_label("OUT")
            .with_ports(
                vec![
                    NodePort::input("in_l", "L"),
                    NodePort::input("in_r", "R"),
                ],
                vec![],
            );
        let output_id = master_module.add_node(output);

        // Internal routing
        master_module.add_wire(Wire::new(master_eq_id, "out_l", master_comp_id, "in_l"));
        master_module.add_wire(Wire::new(master_eq_id, "out_r", master_comp_id, "in_r"));
        master_module.add_wire(Wire::new(master_comp_id, "out_l", output_id, "in_l"));
        master_module.add_wire(Wire::new(master_comp_id, "out_r", output_id, "in_r"));

        master_module.auto_size(20.0);
        let master_id = graph.add_module(master_module);

        // === CONNECTIONS (Inter-Module Signal Chain) ===
        // Source -> EQ -> Dynamics
        graph.connect(source_id, "out_l", eq_id, "in_l");
        graph.connect(source_id, "out_r", eq_id, "in_r");
        graph.connect(eq_id, "out_l", dynamics_id, "in_l");
        graph.connect(eq_id, "out_r", dynamics_id, "in_r");

        // Dynamics -> Drive Module
        graph.connect(dynamics_id, "out_l", drive_id, "in_l");
        graph.connect(dynamics_id, "out_r", drive_id, "in_r");

        // Drive -> Volume Pedal -> Pre-FX
        graph.connect(drive_id, "out_l", vol_pedal_id, "in_l");
        graph.connect(drive_id, "out_r", vol_pedal_id, "in_r");
        graph.connect(vol_pedal_id, "out_l", prefx_id, "in_l");
        graph.connect(vol_pedal_id, "out_r", prefx_id, "in_r");

        // Pre-FX -> Amp/Cab -> Post EQ
        graph.connect(prefx_id, "out_l", ampcab_id, "in_l");
        graph.connect(prefx_id, "out_r", ampcab_id, "in_r");
        graph.connect(ampcab_id, "out_l", post_eq_id, "in_l");
        graph.connect(ampcab_id, "out_r", post_eq_id, "in_r");

        // Post EQ -> Modulation -> Time -> Motion
        graph.connect(post_eq_id, "out_l", mod_id, "in_l");
        graph.connect(post_eq_id, "out_r", mod_id, "in_r");
        graph.connect(mod_id, "out_l", time_id, "in_l");
        graph.connect(mod_id, "out_r", time_id, "in_r");
        graph.connect(time_id, "out_l", motion_id, "in_l");
        graph.connect(time_id, "out_r", motion_id, "in_r");

        // Motion -> Master
        graph.connect(motion_id, "out_l", master_id, "in_l");
        graph.connect(motion_id, "out_r", master_id, "in_r");

        graph
    }
}
