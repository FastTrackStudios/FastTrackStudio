//! Node-based signal flow model.
//!
//! Represents audio processing nodes positioned on an infinite 2D canvas
//! with wire connections between them. Similar to Gig Performer, Reaktor, etc.

use signal_control::block::BlockType;
use signal_proto::normalized::NormalizedF64;
use uuid::Uuid;

// ─────────────────────────────────────────────────────────────────────────────
// NodeParameter
// ─────────────────────────────────────────────────────────────────────────────

/// A parameter attached to a processing node.
///
/// Mirrors [`ParameterOverride`](signal_proto::preset::block_override::ParameterOverride)
/// but lives in the UI layer so node graphs can carry current parameter state
/// for snapshot capture and display.
#[derive(Debug, Clone, PartialEq)]
pub struct NodeParameter {
    /// Unique parameter identifier (e.g. "drive", "eq_low_freq").
    pub id: String,
    /// Human-readable name.
    pub name: String,
    /// Current normalized value in [0.0, 1.0].
    pub value: NormalizedF64,
}

impl NodeParameter {
    /// Create a new node parameter.
    pub fn new(id: impl Into<String>, name: impl Into<String>, value: NormalizedF64) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            value,
        }
    }
}

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
    /// Current parameter values for this node.
    pub parameters: Vec<NodeParameter>,
    /// Input ports.
    pub inputs: Vec<NodePort>,
    /// Output ports.
    pub outputs: Vec<NodePort>,
}

impl Node {
    pub fn new(name: impl Into<String>, block_type: BlockType, position: NodePosition) -> Self {
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
            parameters: Vec::new(),
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

    pub fn with_parameters(mut self, parameters: Vec<NodeParameter>) -> Self {
        self.parameters = parameters;
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
        let ports = if is_input {
            &self.inputs
        } else {
            &self.outputs
        };
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

/// A module container that groups multiple nodes on the node graph canvas.
///
/// Renamed from `Module` to avoid collision with `signal_control::module::Module`.
#[derive(Debug, Clone, PartialEq)]
pub struct GraphModule {
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

impl GraphModule {
    pub fn new(name: impl Into<String>, block_type: BlockType, position: NodePosition) -> Self {
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
        let ports = if is_input {
            &self.inputs
        } else {
            &self.outputs
        };
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
    ///
    /// Node positions are in module-local coordinates where (0,0) is the
    /// top-left of the content area (below the 40px title bar). The module
    /// height must include the title bar + the max node bottom edge + padding.
    pub fn auto_size(&mut self, padding: f64) {
        if self.nodes.is_empty() {
            return;
        }

        // Find the rightmost and bottommost edges of all nodes
        let mut max_x = 0.0f64;
        let mut max_y = 0.0f64;

        for node in &self.nodes {
            max_x = max_x.max(node.position.x + node.size.width);
            max_y = max_y.max(node.position.y + node.size.height);
        }

        // Width: max right edge + right padding
        let content_width = max_x + padding;
        // Height: title bar (40px) + max bottom edge + bottom padding
        let content_height = 40.0 + max_y + padding;

        self.size = NodeSize::new(content_width, content_height);
    }
}

/// The complete node graph.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct NodeGraph {
    /// Modules in the graph.
    pub modules: Vec<GraphModule>,
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
    pub fn add_module(&mut self, module: GraphModule) -> Uuid {
        let id = module.id;
        self.modules.push(module);
        id
    }

    /// Find a module by ID.
    pub fn find_module(&self, id: Uuid) -> Option<&GraphModule> {
        self.modules.iter().find(|m| m.id == id)
    }

    /// Find a module by ID (mutable).
    pub fn find_module_mut(&mut self, id: Uuid) -> Option<&mut GraphModule> {
        self.modules.iter_mut().find(|m| m.id == id)
    }

    /// Find a module at a given position.
    pub fn module_at(&self, x: f64, y: f64) -> Option<&GraphModule> {
        // Search in reverse order (top module first)
        self.modules.iter().rev().find(|m| m.contains_point(x, y))
    }

    /// Add a node to the graph (standalone, not in a module).
    pub fn add_node(&mut self, node: Node) -> Uuid {
        let id = node.id;
        self.nodes.push(node);
        id
    }

    /// Remove a module by ID and all its connected wires.
    pub fn remove_module(&mut self, id: Uuid) {
        self.modules.retain(|m| m.id != id);
        self.wires.retain(|w| w.from_node != id && w.to_node != id);
    }

    /// Remove a node by ID.
    pub fn remove_node(&mut self, id: Uuid) {
        self.nodes.retain(|n| n.id != id);
        // Remove all wires connected to this node
        self.wires.retain(|w| w.from_node != id && w.to_node != id);
    }

    /// Check if a wire already exists between these two ports.
    pub fn has_wire(&self, from_node: Uuid, from_port: &str, to_node: Uuid, to_port: &str) -> bool {
        self.wires.iter().any(|w| {
            w.from_node == from_node
                && w.from_port == from_port
                && w.to_node == to_node
                && w.to_port == to_port
        })
    }

    /// Validate and add a wire. Returns None if the wire is invalid.
    ///
    /// Rejects: self-loops, duplicate wires.
    pub fn try_connect(
        &mut self,
        from_node: Uuid,
        from_port: impl Into<String>,
        to_node: Uuid,
        to_port: impl Into<String>,
    ) -> Option<Uuid> {
        // Reject self-loops
        if from_node == to_node {
            return None;
        }
        let from_port = from_port.into();
        let to_port = to_port.into();
        // Reject duplicates
        if self.has_wire(from_node, &from_port, to_node, &to_port) {
            return None;
        }
        let wire = Wire::new(from_node, from_port, to_node, to_port);
        let id = wire.id;
        self.wires.push(wire);
        Some(id)
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

    /// Automatically arrange modules vertically with proper spacing.
    ///
    /// Groups modules that share a similar y position into "rows",
    /// then repositions each row so modules don't overlap vertically.
    /// Modules within the same row keep their relative x positions.
    pub fn compact_layout(&mut self, gap: f64) {
        if self.modules.is_empty() {
            return;
        }

        // Sort module indices by y position
        let mut indices: Vec<usize> = (0..self.modules.len()).collect();
        indices.sort_by(|a, b| {
            self.modules[*a]
                .position
                .y
                .partial_cmp(&self.modules[*b].position.y)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        // Group into rows: modules within 50px of each other are on the same row
        let row_threshold = 50.0;
        let mut rows: Vec<Vec<usize>> = Vec::new();

        for idx in indices {
            let this_y = self.modules[idx].position.y;
            let same_row = rows.last().map_or(false, |row| {
                let row_y = self.modules[row[0]].position.y;
                (this_y - row_y).abs() < row_threshold
            });

            if same_row {
                rows.last_mut().unwrap().push(idx);
            } else {
                rows.push(vec![idx]);
            }
        }

        // Reposition each row with proper vertical spacing
        let mut y = 50.0;
        for row in &rows {
            // Set all modules in this row to the same y
            for &idx in row {
                self.modules[idx].position.y = y;
            }
            // Advance y by the tallest module in this row + gap
            let max_height = row
                .iter()
                .map(|&idx| self.modules[idx].size.height)
                .fold(0.0f64, f64::max);
            y += max_height + gap;
        }
    }

    /// Get the appropriate widget and size for a block type.
    pub fn widget_for_block_type(block_type: BlockType) -> (NodeWidget, NodeSize) {
        match block_type {
            BlockType::Eq => (NodeWidget::EqGraph, NodeSize::xlarge()),
            BlockType::Compressor => (NodeWidget::CompressorGraph, NodeSize::large()),
            BlockType::Gate => (NodeWidget::GateGraph, NodeSize::medium()),
            BlockType::Delay => (NodeWidget::DelayGraph, NodeSize::large()),
            BlockType::Reverb => (NodeWidget::ReverbGraph, NodeSize::large()),
            BlockType::Drive | BlockType::Saturator => (NodeWidget::DriveGraph, NodeSize::medium()),
            BlockType::Modulation | BlockType::Tremolo | BlockType::Pitch => {
                (NodeWidget::ModulationGraph, NodeSize::medium())
            }
            BlockType::Amp | BlockType::Cabinet => (NodeWidget::AmpCab, NodeSize::medium()),
            BlockType::Tuner => (NodeWidget::Tuner, NodeSize::small()),
            BlockType::Freeze => (NodeWidget::Label, NodeSize::medium()),
            _ => (NodeWidget::Label, NodeSize::small()),
        }
    }

    /// Create a new module with a single node for a given block type.
    ///
    /// Automatically assigns the correct widget, size, and port configuration.
    /// The module is positioned at `position` and auto-sized to fit its node.
    pub fn create_module_for_block_type(
        name: impl Into<String>,
        block_type: BlockType,
        position: NodePosition,
    ) -> GraphModule {
        let name = name.into();
        let (widget, size) = Self::widget_for_block_type(block_type);

        let node = Node::new(&name, block_type, NodePosition::new(10.0, 50.0))
            .with_size(size)
            .with_widget(widget);

        let mut module = GraphModule::new(&name, block_type, position);
        module.add_node(node);
        module.auto_size(20.0);
        module
    }

    /// Find an open position to place a new module, avoiding overlap with
    /// existing modules. Searches below and to the right of existing content.
    pub fn find_open_position(&self) -> NodePosition {
        if self.modules.is_empty() && self.nodes.is_empty() {
            return NodePosition::new(100.0, 100.0);
        }

        // Find the bottom-most module
        let mut max_bottom = 0.0f64;
        let mut leftmost_x = f64::MAX;

        for module in &self.modules {
            let bottom = module.position.y + module.size.height;
            max_bottom = max_bottom.max(bottom);
            leftmost_x = leftmost_x.min(module.position.x);
        }

        for node in &self.nodes {
            let bottom = node.position.y + node.size.height;
            max_bottom = max_bottom.max(bottom);
            leftmost_x = leftmost_x.min(node.position.x);
        }

        // Place below existing content with some gap
        let x = if leftmost_x == f64::MAX {
            100.0
        } else {
            leftmost_x
        };
        NodePosition::new(x, max_bottom + 40.0)
    }

    /// Create a comprehensive guitar rig node graph with all modules.
    pub fn sample_guitar_rig() -> Self {
        let mut graph = Self::new();

        let mut y_offset = 100.0;

        // === SOURCE MODULE (contains Guitar Input, Input Gate, Input Volume) ===
        let mut source_module = GraphModule::new(
            "Source",
            BlockType::Input,
            NodePosition::new(50.0, y_offset),
        )
        .with_size(NodeSize::new(300.0, 280.0));

        let input = Node::new(
            "Guitar Input",
            BlockType::Input,
            NodePosition::new(20.0, 50.0),
        )
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
        let mut eq_module =
            GraphModule::new("EQ", BlockType::Eq, NodePosition::new(380.0, y_offset));
        let eq = Node::new("EQ", BlockType::Eq, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::xlarge())
            .with_widget(NodeWidget::EqGraph);
        eq_module.add_node(eq);
        eq_module.auto_size(20.0);
        let eq_id = graph.add_module(eq_module);

        // === DYNAMICS MODULE (standalone) ===
        let mut dynamics_module = GraphModule::new(
            "Dynamics",
            BlockType::Compressor,
            NodePosition::new(830.0, y_offset),
        );
        let comp = Node::new(
            "Compressor",
            BlockType::Compressor,
            NodePosition::new(10.0, 50.0),
        )
        .with_size(NodeSize::large())
        .with_widget(NodeWidget::CompressorGraph);
        dynamics_module.add_node(comp);
        dynamics_module.auto_size(20.0);
        let dynamics_id = graph.add_module(dynamics_module);

        // === SPECIAL MODULE (contains Envelope, Wah, Pitch, Doubler) ===
        y_offset += 280.0;
        let mut special_module = GraphModule::new(
            "Special",
            BlockType::Modulation,
            NodePosition::new(50.0, y_offset),
        )
        .with_size(NodeSize::new(950.0, 150.0));

        let envelope = Node::new(
            "Envelope",
            BlockType::Modulation,
            NodePosition::new(20.0, 50.0),
        )
        .with_size(NodeSize::medium());
        let envelope_id = special_module.add_node(envelope);

        let wah = Node::new("Wah", BlockType::Modulation, NodePosition::new(250.0, 50.0))
            .with_size(NodeSize::medium());
        let wah_id = special_module.add_node(wah);

        let pitch = Node::new("Pitch", BlockType::Pitch, NodePosition::new(480.0, 50.0))
            .with_size(NodeSize::medium());
        let pitch_id = special_module.add_node(pitch);

        let doubler = Node::new(
            "Doubler",
            BlockType::Modulation,
            NodePosition::new(710.0, 50.0),
        )
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
        let mut drive_module =
            GraphModule::new("Drive", BlockType::Drive, NodePosition::new(50.0, y_offset))
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
        let mut vol_pedal_module = GraphModule::new(
            "Volume",
            BlockType::Volume,
            NodePosition::new(50.0, y_offset),
        );
        let vol_pedal = Node::new("Volume", BlockType::Volume, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::small());
        vol_pedal_module.add_node(vol_pedal);
        vol_pedal_module.auto_size(20.0);
        let vol_pedal_id = graph.add_module(vol_pedal_module);

        // === PRE-FX MODULE (contains Pre Delay, Spring Verb) ===
        let mut prefx_module = GraphModule::new(
            "Pre-FX",
            BlockType::Delay,
            NodePosition::new(260.0, y_offset),
        );

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
        let mut ampcab_module =
            GraphModule::new("Amp/Cab", BlockType::Amp, NodePosition::new(50.0, y_offset))
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
        let mut post_eq_module =
            GraphModule::new("Post EQ", BlockType::Eq, NodePosition::new(50.0, y_offset));
        let post_eq = Node::new("EQ", BlockType::Eq, NodePosition::new(10.0, 50.0))
            .with_size(NodeSize::xlarge())
            .with_widget(NodeWidget::EqGraph);
        post_eq_module.add_node(post_eq);
        post_eq_module.auto_size(20.0);
        let post_eq_id = graph.add_module(post_eq_module);

        // === MODULATION MODULE (contains Chorus, Flanger, Phaser) ===
        y_offset += 260.0;
        let mut mod_module = GraphModule::new(
            "Modulation",
            BlockType::Modulation,
            NodePosition::new(50.0, y_offset),
        )
        .with_size(NodeSize::new(730.0, 160.0));

        let chorus = Node::new(
            "Chorus",
            BlockType::Modulation,
            NodePosition::new(20.0, 50.0),
        )
        .with_size(NodeSize::medium())
        .with_widget(NodeWidget::ModulationGraph);
        let chorus_id = mod_module.add_node(chorus);

        let flanger = Node::new(
            "Flanger",
            BlockType::Modulation,
            NodePosition::new(260.0, 50.0),
        )
        .with_size(NodeSize::medium())
        .with_widget(NodeWidget::ModulationGraph);
        let flanger_id = mod_module.add_node(flanger);

        let phaser = Node::new(
            "Phaser",
            BlockType::Modulation,
            NodePosition::new(500.0, 50.0),
        )
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
        let mut time_module =
            GraphModule::new("Time", BlockType::Delay, NodePosition::new(50.0, y_offset));

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
        let mut motion_module = GraphModule::new(
            "Motion",
            BlockType::Tremolo,
            NodePosition::new(50.0, y_offset),
        )
        .with_size(NodeSize::new(730.0, 160.0));

        let tremolo = Node::new("Tremolo", BlockType::Tremolo, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::medium())
            .with_widget(NodeWidget::ModulationGraph);
        let tremolo_id = motion_module.add_node(tremolo);

        let vibrato = Node::new(
            "Vibrato",
            BlockType::Modulation,
            NodePosition::new(260.0, 50.0),
        )
        .with_size(NodeSize::medium());
        let vibrato_id = motion_module.add_node(vibrato);

        let rotary = Node::new(
            "Rotary",
            BlockType::Modulation,
            NodePosition::new(500.0, 50.0),
        )
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
        let mut master_module =
            GraphModule::new("Master", BlockType::Eq, NodePosition::new(50.0, y_offset));

        let master_eq = Node::new("Master EQ", BlockType::Eq, NodePosition::new(20.0, 50.0))
            .with_size(NodeSize::xlarge())
            .with_widget(NodeWidget::EqGraph);
        let master_eq_id = master_module.add_node(master_eq);

        let master_comp = Node::new(
            "Multiband",
            BlockType::Compressor,
            NodePosition::new(440.0, 50.0),
        )
        .with_size(NodeSize::large())
        .with_widget(NodeWidget::CompressorGraph);
        let master_comp_id = master_module.add_node(master_comp);

        let output = Node::new("Output", BlockType::Volume, NodePosition::new(790.0, 90.0))
            .with_size(NodeSize::small())
            .with_short_label("OUT")
            .with_ports(
                vec![NodePort::input("in_l", "L"), NodePort::input("in_r", "R")],
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

        // Auto-arrange modules so nothing overlaps
        graph.compact_layout(30.0);

        graph
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Snapshot Types
// ─────────────────────────────────────────────────────────────────────────────

/// Captured parameter state for a single node.
#[derive(Debug, Clone, PartialEq)]
pub struct NodeSnapshot {
    /// The node this snapshot was captured from.
    pub node_id: Uuid,
    /// Node name at capture time (for display).
    pub node_name: String,
    /// Captured parameter values as (param_id, normalized_value) pairs.
    pub parameters: Vec<(String, f64)>,
}

/// Captured state of all nodes within a single module.
#[derive(Debug, Clone, PartialEq)]
pub struct ModuleSnapshot {
    /// The module this snapshot was captured from.
    pub module_id: Uuid,
    /// Module name at capture time.
    pub module_name: String,
    /// Captured state for each node in the module.
    pub nodes: Vec<NodeSnapshot>,
}

/// Captured state of the entire rig (all modules and standalone nodes).
#[derive(Debug, Clone, PartialEq)]
pub struct RigSnapshot {
    /// Unique snapshot ID.
    pub id: Uuid,
    /// User-assigned name for this snapshot.
    pub name: String,
    /// Captured state for each module.
    pub modules: Vec<ModuleSnapshot>,
    /// Captured state for standalone nodes (not in any module).
    pub standalone_nodes: Vec<NodeSnapshot>,
}

impl RigSnapshot {
    /// Create a new rig snapshot with the given name.
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            name: name.into(),
            modules: Vec::new(),
            standalone_nodes: Vec::new(),
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Capture Functions
// ─────────────────────────────────────────────────────────────────────────────

/// Capture the current parameter values from a single node.
///
/// Returns a list of `(param_id, normalized_value)` pairs — one per parameter
/// on the node. Returns an empty vec if the node has no parameters.
pub fn capture_node_parameters(graph: &NodeGraph, node_id: Uuid) -> Vec<(Uuid, f64)> {
    graph
        .find_node(node_id)
        .map(|node| {
            node.parameters
                .iter()
                .map(|p| (node.id, p.value.get()))
                .collect()
        })
        .unwrap_or_default()
}

/// Capture a snapshot of all node parameters within a module.
///
/// Returns `None` if the module is not found.
pub fn capture_module_snapshot(graph: &NodeGraph, module_id: Uuid) -> Option<ModuleSnapshot> {
    let module = graph.find_module(module_id)?;
    let nodes = module
        .nodes
        .iter()
        .map(|node| NodeSnapshot {
            node_id: node.id,
            node_name: node.name.clone(),
            parameters: node
                .parameters
                .iter()
                .map(|p| (p.id.clone(), p.value.get()))
                .collect(),
        })
        .collect();

    Some(ModuleSnapshot {
        module_id: module.id,
        module_name: module.name.clone(),
        nodes,
    })
}

/// Capture a snapshot of the entire rig — all modules and standalone nodes.
pub fn capture_rig_snapshot(graph: &NodeGraph, name: impl Into<String>) -> RigSnapshot {
    let modules = graph
        .modules
        .iter()
        .map(|module| ModuleSnapshot {
            module_id: module.id,
            module_name: module.name.clone(),
            nodes: module
                .nodes
                .iter()
                .map(|node| NodeSnapshot {
                    node_id: node.id,
                    node_name: node.name.clone(),
                    parameters: node
                        .parameters
                        .iter()
                        .map(|p| (p.id.clone(), p.value.get()))
                        .collect(),
                })
                .collect(),
        })
        .collect();

    let standalone_nodes = graph
        .nodes
        .iter()
        .map(|node| NodeSnapshot {
            node_id: node.id,
            node_name: node.name.clone(),
            parameters: node
                .parameters
                .iter()
                .map(|p| (p.id.clone(), p.value.get()))
                .collect(),
        })
        .collect();

    RigSnapshot {
        id: Uuid::new_v4(),
        name: name.into(),
        modules,
        standalone_nodes,
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Undo / Redo
// ─────────────────────────────────────────────────────────────────────────────

/// Maximum number of operations stored in the undo history.
const MAX_HISTORY: usize = 50;

/// A reversible graph operation.
///
/// Each variant stores enough data to both apply **and** reverse the change.
/// Operations that mutate the `NodeGraph` are recorded here so `GraphHistory`
/// can replay them in either direction.
#[derive(Debug, Clone, PartialEq)]
pub enum GraphOperation {
    /// A module was moved from `old_pos` to `new_pos`.
    MoveModule {
        module_id: Uuid,
        old_pos: NodePosition,
        new_pos: NodePosition,
    },
    /// A standalone node was moved.
    MoveNode {
        node_id: Uuid,
        old_pos: NodePosition,
        new_pos: NodePosition,
    },
    /// A wire was added to the graph.
    AddWire { wire: Wire },
    /// A wire was removed from the graph.
    RemoveWire { wire: Wire },
    /// A module was added.
    AddModule { module: GraphModule },
    /// A module (and its connected wires) was removed.
    RemoveModule {
        module: GraphModule,
        connected_wires: Vec<Wire>,
    },
    /// A standalone node was added.
    AddNode { node: Node },
    /// A standalone node (and its connected wires) was removed.
    RemoveNode {
        node: Node,
        connected_wires: Vec<Wire>,
    },
    /// A module's or node's bypass state was toggled.
    BypassToggle {
        entity_id: Uuid,
        /// `true` if the entity is a module, `false` if standalone node.
        is_module: bool,
    },
    /// A parameter value was changed.
    ParameterChange {
        node_id: Uuid,
        param_id: String,
        old_value: f64,
        new_value: f64,
    },
}

impl GraphOperation {
    /// Apply this operation to the graph (redo direction).
    pub fn apply(&self, graph: &mut NodeGraph) {
        match self {
            GraphOperation::MoveModule {
                module_id, new_pos, ..
            } => {
                if let Some(m) = graph.find_module_mut(*module_id) {
                    m.position = *new_pos;
                }
            }
            GraphOperation::MoveNode {
                node_id, new_pos, ..
            } => {
                if let Some(n) = graph.find_node_mut(*node_id) {
                    n.position = *new_pos;
                }
            }
            GraphOperation::AddWire { wire } => {
                graph.wires.push(wire.clone());
            }
            GraphOperation::RemoveWire { wire } => {
                graph.wires.retain(|w| w.id != wire.id);
            }
            GraphOperation::AddModule { module } => {
                graph.modules.push(module.clone());
            }
            GraphOperation::RemoveModule { module, .. } => {
                graph.modules.retain(|m| m.id != module.id);
                graph
                    .wires
                    .retain(|w| w.from_node != module.id && w.to_node != module.id);
            }
            GraphOperation::AddNode { node } => {
                graph.nodes.push(node.clone());
            }
            GraphOperation::RemoveNode { node, .. } => {
                graph.nodes.retain(|n| n.id != node.id);
                graph
                    .wires
                    .retain(|w| w.from_node != node.id && w.to_node != node.id);
            }
            GraphOperation::BypassToggle {
                entity_id,
                is_module,
            } => {
                if *is_module {
                    if let Some(m) = graph.find_module_mut(*entity_id) {
                        m.bypassed = !m.bypassed;
                    }
                } else if let Some(n) = graph.find_node_mut(*entity_id) {
                    n.bypassed = !n.bypassed;
                }
            }
            GraphOperation::ParameterChange {
                node_id,
                param_id,
                new_value,
                ..
            } => {
                if let Some(n) = graph.find_node_mut(*node_id) {
                    if let Some(p) = n.parameters.iter_mut().find(|p| p.id == *param_id) {
                        p.value = NormalizedF64::new(*new_value);
                    }
                }
            }
        }
    }

    /// Reverse this operation on the graph (undo direction).
    pub fn undo(&self, graph: &mut NodeGraph) {
        match self {
            GraphOperation::MoveModule {
                module_id, old_pos, ..
            } => {
                if let Some(m) = graph.find_module_mut(*module_id) {
                    m.position = *old_pos;
                }
            }
            GraphOperation::MoveNode {
                node_id, old_pos, ..
            } => {
                if let Some(n) = graph.find_node_mut(*node_id) {
                    n.position = *old_pos;
                }
            }
            GraphOperation::AddWire { wire } => {
                graph.wires.retain(|w| w.id != wire.id);
            }
            GraphOperation::RemoveWire { wire } => {
                graph.wires.push(wire.clone());
            }
            GraphOperation::AddModule { module } => {
                graph.modules.retain(|m| m.id != module.id);
            }
            GraphOperation::RemoveModule {
                module,
                connected_wires,
            } => {
                graph.modules.push(module.clone());
                for wire in connected_wires {
                    graph.wires.push(wire.clone());
                }
            }
            GraphOperation::AddNode { node } => {
                graph.nodes.retain(|n| n.id != node.id);
            }
            GraphOperation::RemoveNode {
                node,
                connected_wires,
            } => {
                graph.nodes.push(node.clone());
                for wire in connected_wires {
                    graph.wires.push(wire.clone());
                }
            }
            GraphOperation::BypassToggle {
                entity_id,
                is_module,
            } => {
                // Toggle is its own inverse.
                if *is_module {
                    if let Some(m) = graph.find_module_mut(*entity_id) {
                        m.bypassed = !m.bypassed;
                    }
                } else if let Some(n) = graph.find_node_mut(*entity_id) {
                    n.bypassed = !n.bypassed;
                }
            }
            GraphOperation::ParameterChange {
                node_id,
                param_id,
                old_value,
                ..
            } => {
                if let Some(n) = graph.find_node_mut(*node_id) {
                    if let Some(p) = n.parameters.iter_mut().find(|p| p.id == *param_id) {
                        p.value = NormalizedF64::new(*old_value);
                    }
                }
            }
        }
    }
}

/// Bounded undo/redo history for graph operations.
///
/// Stores up to [`MAX_HISTORY`] operations. Pushing a new operation clears
/// the redo stack (standard branching-discard behaviour).
#[derive(Debug, Clone, Default)]
pub struct GraphHistory {
    undo_stack: Vec<GraphOperation>,
    redo_stack: Vec<GraphOperation>,
}

impl GraphHistory {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record a new operation. Clears the redo stack and enforces the capacity
    /// limit by removing the oldest operation when full.
    pub fn push_operation(&mut self, op: GraphOperation) {
        self.redo_stack.clear();
        if self.undo_stack.len() >= MAX_HISTORY {
            self.undo_stack.remove(0);
        }
        self.undo_stack.push(op);
    }

    /// Undo the most recent operation, applying the inverse to `graph`.
    ///
    /// Returns `true` if an operation was undone.
    pub fn undo(&mut self, graph: &mut NodeGraph) -> bool {
        if let Some(op) = self.undo_stack.pop() {
            op.undo(graph);
            self.redo_stack.push(op);
            true
        } else {
            false
        }
    }

    /// Redo the most recently undone operation, re-applying it to `graph`.
    ///
    /// Returns `true` if an operation was redone.
    pub fn redo(&mut self, graph: &mut NodeGraph) -> bool {
        if let Some(op) = self.redo_stack.pop() {
            op.apply(graph);
            self.undo_stack.push(op);
            true
        } else {
            false
        }
    }

    /// Whether the undo stack is non-empty.
    pub fn can_undo(&self) -> bool {
        !self.undo_stack.is_empty()
    }

    /// Whether the redo stack is non-empty.
    pub fn can_redo(&self) -> bool {
        !self.redo_stack.is_empty()
    }

    /// Number of operations on the undo stack.
    pub fn undo_len(&self) -> usize {
        self.undo_stack.len()
    }

    /// Number of operations on the redo stack.
    pub fn redo_len(&self) -> usize {
        self.redo_stack.len()
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    // -- Helpers

    fn sample_node_with_params(name: &str, position: NodePosition) -> Node {
        Node::new(name, BlockType::Drive, position).with_parameters(vec![
            NodeParameter::new("drive", "Drive", NormalizedF64::new(0.7)),
            NodeParameter::new("tone", "Tone", NormalizedF64::new(0.5)),
            NodeParameter::new("level", "Level", NormalizedF64::new(0.8)),
        ])
    }

    // -- NodeParameter

    #[test]
    fn test_node_parameter_new() -> Result<()> {
        // -- Setup & Fixtures
        let param = NodeParameter::new("drive", "Drive", NormalizedF64::new(0.7));

        // -- Check
        assert_eq!(param.id, "drive");
        assert_eq!(param.name, "Drive");
        assert!((param.value.get() - 0.7).abs() < f64::EPSILON);
        Ok(())
    }

    #[test]
    fn test_node_parameter_value_clamped() -> Result<()> {
        // -- Setup & Fixtures
        let param = NodeParameter::new("gain", "Gain", NormalizedF64::new(1.5));

        // -- Check
        assert_eq!(param.value.get(), 1.0);
        Ok(())
    }

    // -- Node with parameters

    #[test]
    fn test_node_with_parameters() -> Result<()> {
        // -- Setup & Fixtures
        let node = sample_node_with_params("Drive", NodePosition::new(0.0, 0.0));

        // -- Check
        assert_eq!(node.parameters.len(), 3);
        assert_eq!(node.parameters[0].id, "drive");
        assert_eq!(node.parameters[1].id, "tone");
        assert_eq!(node.parameters[2].id, "level");
        Ok(())
    }

    #[test]
    fn test_node_default_has_no_parameters() -> Result<()> {
        // -- Setup & Fixtures
        let node = Node::new("Test", BlockType::Input, NodePosition::new(0.0, 0.0));

        // -- Check
        assert!(node.parameters.is_empty());
        Ok(())
    }

    // -- capture_node_parameters

    #[test]
    fn test_capture_node_parameters_found() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let node = sample_node_with_params("Drive", NodePosition::new(0.0, 0.0));
        let node_id = node.id;
        graph.add_node(node);

        // -- Exec
        let captured = capture_node_parameters(&graph, node_id);

        // -- Check
        assert_eq!(captured.len(), 3);
        for (id, _value) in &captured {
            assert_eq!(*id, node_id);
        }
        assert!((captured[0].1 - 0.7).abs() < f64::EPSILON);
        Ok(())
    }

    #[test]
    fn test_capture_node_parameters_not_found() -> Result<()> {
        // -- Setup & Fixtures
        let graph = NodeGraph::new();

        // -- Exec
        let captured = capture_node_parameters(&graph, Uuid::new_v4());

        // -- Check
        assert!(captured.is_empty());
        Ok(())
    }

    #[test]
    fn test_capture_node_parameters_in_module() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let node = sample_node_with_params("Drive", NodePosition::new(10.0, 50.0));
        let node_id = node.id;
        let mut module =
            GraphModule::new("Test Module", BlockType::Drive, NodePosition::new(0.0, 0.0));
        module.add_node(node);
        graph.add_module(module);

        // -- Exec
        let captured = capture_node_parameters(&graph, node_id);

        // -- Check
        assert_eq!(captured.len(), 3);
        Ok(())
    }

    // -- capture_module_snapshot

    #[test]
    fn test_capture_module_snapshot_found() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let node1 = sample_node_with_params("Drive 1", NodePosition::new(10.0, 50.0));
        let node2 = sample_node_with_params("Drive 2", NodePosition::new(200.0, 50.0));
        let mut module =
            GraphModule::new("Drive Stage", BlockType::Drive, NodePosition::new(0.0, 0.0));
        module.add_node(node1);
        module.add_node(node2);
        let module_id = module.id;
        graph.add_module(module);

        // -- Exec
        let snapshot = capture_module_snapshot(&graph, module_id);

        // -- Check
        let snapshot = snapshot.unwrap();
        assert_eq!(snapshot.module_id, module_id);
        assert_eq!(snapshot.module_name, "Drive Stage");
        assert_eq!(snapshot.nodes.len(), 2);
        assert_eq!(snapshot.nodes[0].parameters.len(), 3);
        assert_eq!(snapshot.nodes[1].parameters.len(), 3);
        Ok(())
    }

    #[test]
    fn test_capture_module_snapshot_not_found() -> Result<()> {
        // -- Setup & Fixtures
        let graph = NodeGraph::new();

        // -- Exec
        let snapshot = capture_module_snapshot(&graph, Uuid::new_v4());

        // -- Check
        assert!(snapshot.is_none());
        Ok(())
    }

    // -- capture_rig_snapshot

    #[test]
    fn test_capture_rig_snapshot_empty_graph() -> Result<()> {
        // -- Setup & Fixtures
        let graph = NodeGraph::new();

        // -- Exec
        let snapshot = capture_rig_snapshot(&graph, "Empty Rig");

        // -- Check
        assert_eq!(snapshot.name, "Empty Rig");
        assert!(snapshot.modules.is_empty());
        assert!(snapshot.standalone_nodes.is_empty());
        Ok(())
    }

    #[test]
    fn test_capture_rig_snapshot_full_graph() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();

        // Add a module with nodes
        let node1 = sample_node_with_params("Drive", NodePosition::new(10.0, 50.0));
        let mut module = GraphModule::new(
            "Drive Module",
            BlockType::Drive,
            NodePosition::new(0.0, 0.0),
        );
        module.add_node(node1);
        graph.add_module(module);

        // Add a standalone node
        let standalone = sample_node_with_params("Standalone EQ", NodePosition::new(500.0, 0.0));
        graph.add_node(standalone);

        // -- Exec
        let snapshot = capture_rig_snapshot(&graph, "Full Rig Capture");

        // -- Check
        assert_eq!(snapshot.name, "Full Rig Capture");
        assert_eq!(snapshot.modules.len(), 1);
        assert_eq!(snapshot.modules[0].module_name, "Drive Module");
        assert_eq!(snapshot.modules[0].nodes.len(), 1);
        assert_eq!(snapshot.standalone_nodes.len(), 1);
        assert_eq!(snapshot.standalone_nodes[0].node_name, "Standalone EQ");
        Ok(())
    }

    #[test]
    fn test_capture_rig_snapshot_preserves_parameter_values() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let node =
            Node::new("Custom", BlockType::Eq, NodePosition::new(0.0, 0.0)).with_parameters(vec![
                NodeParameter::new("freq", "Frequency", NormalizedF64::new(0.3)),
                NodeParameter::new("gain", "Gain", NormalizedF64::new(0.9)),
            ]);
        graph.add_node(node);

        // -- Exec
        let snapshot = capture_rig_snapshot(&graph, "Value Test");

        // -- Check
        let node_snap = &snapshot.standalone_nodes[0];
        assert_eq!(node_snap.parameters.len(), 2);
        assert_eq!(node_snap.parameters[0].0, "freq");
        assert!((node_snap.parameters[0].1 - 0.3).abs() < f64::EPSILON);
        assert_eq!(node_snap.parameters[1].0, "gain");
        assert!((node_snap.parameters[1].1 - 0.9).abs() < f64::EPSILON);
        Ok(())
    }

    #[test]
    fn test_rig_snapshot_unique_ids() -> Result<()> {
        // -- Setup & Fixtures
        let graph = NodeGraph::new();

        // -- Exec
        let snap1 = capture_rig_snapshot(&graph, "Snap 1");
        let snap2 = capture_rig_snapshot(&graph, "Snap 2");

        // -- Check
        assert_ne!(snap1.id, snap2.id);
        Ok(())
    }

    // region: --- GraphHistory

    #[test]
    fn test_history_new_is_empty() -> Result<()> {
        // -- Setup & Fixtures
        let history = GraphHistory::new();

        // -- Check
        assert!(!history.can_undo());
        assert!(!history.can_redo());
        assert_eq!(history.undo_len(), 0);
        assert_eq!(history.redo_len(), 0);
        Ok(())
    }

    #[test]
    fn test_history_push_enables_undo() -> Result<()> {
        // -- Setup & Fixtures
        let mut history = GraphHistory::new();
        let module = GraphModule::new("Test", BlockType::Drive, NodePosition::new(0.0, 0.0));
        let op = GraphOperation::MoveModule {
            module_id: module.id,
            old_pos: NodePosition::new(0.0, 0.0),
            new_pos: NodePosition::new(100.0, 200.0),
        };

        // -- Exec
        history.push_operation(op);

        // -- Check
        assert!(history.can_undo());
        assert!(!history.can_redo());
        assert_eq!(history.undo_len(), 1);
        Ok(())
    }

    #[test]
    fn test_history_undo_move_module() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let module = GraphModule::new("Test", BlockType::Drive, NodePosition::new(0.0, 0.0));
        let module_id = module.id;
        graph.add_module(module);

        // Move the module in the graph
        graph.find_module_mut(module_id).unwrap().position = NodePosition::new(100.0, 200.0);

        let mut history = GraphHistory::new();
        history.push_operation(GraphOperation::MoveModule {
            module_id,
            old_pos: NodePosition::new(0.0, 0.0),
            new_pos: NodePosition::new(100.0, 200.0),
        });

        // -- Exec
        let undone = history.undo(&mut graph);

        // -- Check
        assert!(undone);
        let m = graph.find_module(module_id).unwrap();
        assert_eq!(m.position, NodePosition::new(0.0, 0.0));
        assert!(history.can_redo());
        assert!(!history.can_undo());
        Ok(())
    }

    #[test]
    fn test_history_redo_move_module() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let module = GraphModule::new("Test", BlockType::Drive, NodePosition::new(0.0, 0.0));
        let module_id = module.id;
        graph.add_module(module);

        graph.find_module_mut(module_id).unwrap().position = NodePosition::new(100.0, 200.0);

        let mut history = GraphHistory::new();
        history.push_operation(GraphOperation::MoveModule {
            module_id,
            old_pos: NodePosition::new(0.0, 0.0),
            new_pos: NodePosition::new(100.0, 200.0),
        });
        history.undo(&mut graph);

        // -- Exec
        let redone = history.redo(&mut graph);

        // -- Check
        assert!(redone);
        let m = graph.find_module(module_id).unwrap();
        assert_eq!(m.position, NodePosition::new(100.0, 200.0));
        assert!(history.can_undo());
        assert!(!history.can_redo());
        Ok(())
    }

    #[test]
    fn test_history_push_clears_redo() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let module = GraphModule::new("Test", BlockType::Drive, NodePosition::new(0.0, 0.0));
        let module_id = module.id;
        graph.add_module(module);

        let mut history = GraphHistory::new();
        history.push_operation(GraphOperation::MoveModule {
            module_id,
            old_pos: NodePosition::new(0.0, 0.0),
            new_pos: NodePosition::new(50.0, 50.0),
        });
        history.undo(&mut graph);
        assert!(history.can_redo());

        // -- Exec: push a new op after undo
        history.push_operation(GraphOperation::MoveModule {
            module_id,
            old_pos: NodePosition::new(0.0, 0.0),
            new_pos: NodePosition::new(200.0, 200.0),
        });

        // -- Check: redo is gone
        assert!(!history.can_redo());
        assert_eq!(history.undo_len(), 1);
        Ok(())
    }

    #[test]
    fn test_history_capacity_limit() -> Result<()> {
        // -- Setup & Fixtures
        let mut history = GraphHistory::new();
        let module_id = Uuid::new_v4();

        // -- Exec: push more than MAX_HISTORY operations
        for i in 0..=MAX_HISTORY {
            history.push_operation(GraphOperation::MoveModule {
                module_id,
                old_pos: NodePosition::new(i as f64, 0.0),
                new_pos: NodePosition::new(i as f64 + 1.0, 0.0),
            });
        }

        // -- Check: capacity is capped at MAX_HISTORY
        assert_eq!(history.undo_len(), MAX_HISTORY);
        Ok(())
    }

    #[test]
    fn test_history_undo_empty_returns_false() -> Result<()> {
        // -- Setup & Fixtures
        let mut history = GraphHistory::new();
        let mut graph = NodeGraph::new();

        // -- Exec
        let undone = history.undo(&mut graph);

        // -- Check
        assert!(!undone);
        Ok(())
    }

    #[test]
    fn test_history_redo_empty_returns_false() -> Result<()> {
        // -- Setup & Fixtures
        let mut history = GraphHistory::new();
        let mut graph = NodeGraph::new();

        // -- Exec
        let redone = history.redo(&mut graph);

        // -- Check
        assert!(!redone);
        Ok(())
    }

    #[test]
    fn test_history_add_remove_wire_roundtrip() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let node_a = Node::new("A", BlockType::Input, NodePosition::new(0.0, 0.0));
        let node_b = Node::new("B", BlockType::Eq, NodePosition::new(200.0, 0.0));
        let a_id = graph.add_node(node_a);
        let b_id = graph.add_node(node_b);

        let wire = Wire::new(a_id, "out_l", b_id, "in_l");
        let wire_id = wire.id;
        graph.wires.push(wire.clone());

        let mut history = GraphHistory::new();
        history.push_operation(GraphOperation::AddWire { wire });

        // -- Exec: undo should remove the wire
        history.undo(&mut graph);

        // -- Check
        assert!(!graph.wires.iter().any(|w| w.id == wire_id));

        // -- Exec: redo should re-add it
        history.redo(&mut graph);
        assert!(graph.wires.iter().any(|w| w.id == wire_id));
        Ok(())
    }

    #[test]
    fn test_history_bypass_toggle_is_self_inverse() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let module = GraphModule::new("Amp", BlockType::Amp, NodePosition::new(0.0, 0.0));
        let module_id = module.id;
        graph.add_module(module);
        assert!(!graph.find_module(module_id).unwrap().bypassed);

        let mut history = GraphHistory::new();
        // Record a bypass toggle (forward: bypass ON)
        graph.find_module_mut(module_id).unwrap().bypassed = true;
        history.push_operation(GraphOperation::BypassToggle {
            entity_id: module_id,
            is_module: true,
        });

        // -- Exec: undo toggles back to OFF
        history.undo(&mut graph);

        // -- Check
        assert!(!graph.find_module(module_id).unwrap().bypassed);

        // -- Exec: redo toggles back to ON
        history.redo(&mut graph);
        assert!(graph.find_module(module_id).unwrap().bypassed);
        Ok(())
    }

    #[test]
    fn test_history_parameter_change_roundtrip() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let node = sample_node_with_params("Drive", NodePosition::new(0.0, 0.0));
        let node_id = node.id;
        graph.add_node(node);

        let mut history = GraphHistory::new();
        // Change "drive" param from 0.7 → 0.3
        graph
            .find_node_mut(node_id)
            .unwrap()
            .parameters
            .iter_mut()
            .find(|p| p.id == "drive")
            .unwrap()
            .value = NormalizedF64::new(0.3);

        history.push_operation(GraphOperation::ParameterChange {
            node_id,
            param_id: "drive".to_string(),
            old_value: 0.7,
            new_value: 0.3,
        });

        // -- Exec: undo restores 0.7
        history.undo(&mut graph);

        // -- Check
        let val = graph
            .find_node(node_id)
            .unwrap()
            .parameters
            .iter()
            .find(|p| p.id == "drive")
            .unwrap()
            .value
            .get();
        assert!((val - 0.7).abs() < f64::EPSILON);

        // -- Exec: redo applies 0.3 again
        history.redo(&mut graph);
        let val = graph
            .find_node(node_id)
            .unwrap()
            .parameters
            .iter()
            .find(|p| p.id == "drive")
            .unwrap()
            .value
            .get();
        assert!((val - 0.3).abs() < f64::EPSILON);
        Ok(())
    }

    #[test]
    fn test_history_remove_module_restores_wires() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let module = GraphModule::new("EQ", BlockType::Eq, NodePosition::new(0.0, 0.0));
        let module_id = module.id;
        graph.add_module(module.clone());

        let other = GraphModule::new("Comp", BlockType::Compressor, NodePosition::new(300.0, 0.0));
        let other_id = other.id;
        graph.add_module(other);

        let wire = Wire::new(module_id, "out_l", other_id, "in_l");
        graph.wires.push(wire.clone());

        // Record removal with connected wires
        let connected_wires = graph
            .wires
            .iter()
            .filter(|w| w.from_node == module_id || w.to_node == module_id)
            .cloned()
            .collect::<Vec<_>>();

        let mut history = GraphHistory::new();
        history.push_operation(GraphOperation::RemoveModule {
            module: module.clone(),
            connected_wires,
        });
        // Actually remove it
        graph.remove_module(module_id);

        // -- Check: module and wire are gone
        assert!(graph.find_module(module_id).is_none());
        assert!(graph.wires.is_empty());

        // -- Exec: undo should restore both
        history.undo(&mut graph);
        assert!(graph.find_module(module_id).is_some());
        assert_eq!(graph.wires.len(), 1);
        Ok(())
    }

    #[test]
    fn test_history_multiple_undo_redo_sequence() -> Result<()> {
        // -- Setup & Fixtures
        let mut graph = NodeGraph::new();
        let module = GraphModule::new("Test", BlockType::Drive, NodePosition::new(0.0, 0.0));
        let module_id = module.id;
        graph.add_module(module);

        let mut history = GraphHistory::new();

        // Op 1: move to (100, 100)
        graph.find_module_mut(module_id).unwrap().position = NodePosition::new(100.0, 100.0);
        history.push_operation(GraphOperation::MoveModule {
            module_id,
            old_pos: NodePosition::new(0.0, 0.0),
            new_pos: NodePosition::new(100.0, 100.0),
        });

        // Op 2: move to (200, 200)
        graph.find_module_mut(module_id).unwrap().position = NodePosition::new(200.0, 200.0);
        history.push_operation(GraphOperation::MoveModule {
            module_id,
            old_pos: NodePosition::new(100.0, 100.0),
            new_pos: NodePosition::new(200.0, 200.0),
        });

        // -- Exec: undo twice
        history.undo(&mut graph);
        assert_eq!(
            graph.find_module(module_id).unwrap().position,
            NodePosition::new(100.0, 100.0)
        );

        history.undo(&mut graph);
        assert_eq!(
            graph.find_module(module_id).unwrap().position,
            NodePosition::new(0.0, 0.0)
        );

        // -- Exec: redo twice
        history.redo(&mut graph);
        assert_eq!(
            graph.find_module(module_id).unwrap().position,
            NodePosition::new(100.0, 100.0)
        );

        history.redo(&mut graph);
        assert_eq!(
            graph.find_module(module_id).unwrap().position,
            NodePosition::new(200.0, 200.0)
        );
        Ok(())
    }

    // endregion: --- GraphHistory
}
