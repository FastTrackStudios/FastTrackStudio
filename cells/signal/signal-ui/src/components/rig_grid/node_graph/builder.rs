//! Factory and builder methods for [`NodeGraph`].
//!
//! Contains `widget_for_block_type`, `create_module_for_block_type`,
//! `find_open_position`, and `build_from_modules`.

use signal_control::block::BlockType;
use uuid::Uuid;

use super::models::{GraphModule, Node, NodeGraph, NodePosition, NodeSize, NodeWidget, Wire};

impl NodeGraph {
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

        let x = if leftmost_x == f64::MAX {
            100.0
        } else {
            leftmost_x
        };
        NodePosition::new(x, max_bottom + 40.0)
    }

    /// Build a node graph from DB-backed module data.
    ///
    /// Creates a `GraphModule` for each `Module` in the list, with child
    /// `Node`s for each block. Modules are laid out vertically with auto-sized
    /// containers. Internal wires chain blocks in signal flow order.
    pub fn build_from_modules(modules: &[signal_control::module::Module]) -> Self {
        use signal_control::block::BlockType;
        use signal_control::module::ModuleType;

        fn module_type_to_block_type(mt: ModuleType) -> BlockType {
            match mt {
                ModuleType::Drive => BlockType::Drive,
                ModuleType::Amp => BlockType::Amp,
                ModuleType::Eq | ModuleType::PostEq => BlockType::Eq,
                ModuleType::Dynamics => BlockType::Compressor,
                ModuleType::Modulation | ModuleType::VocalModulation => BlockType::Modulation,
                ModuleType::Time => BlockType::Delay,
                ModuleType::Motion => BlockType::Tremolo,
                ModuleType::Special | ModuleType::PreFx => BlockType::Special,
                ModuleType::Master => BlockType::Volume,
                _ => BlockType::Special,
            }
        }

        let mut graph = Self::new();
        let mut y_offset = 80.0;
        let module_x = 50.0;
        let module_width = 400.0;
        let node_height = 60.0;
        let node_gap = 10.0;
        let header_height = 40.0;

        let mut prev_module_id: Option<Uuid> = None;

        for module in modules {
            let bt = module_type_to_block_type(module.module_type);
            let block_count = module.blocks.len();
            let content_height =
                header_height + (block_count as f64) * (node_height + node_gap) + 20.0;
            let module_height = content_height.max(120.0);

            let mut gm = GraphModule::new(
                module.name.clone(),
                bt,
                NodePosition::new(module_x, y_offset),
            )
            .with_size(NodeSize::new(module_width, module_height));

            let mut prev_node_id: Option<Uuid> = None;
            for (i, mb) in module.blocks.iter().enumerate() {
                let mut node = Node::new(
                    mb.block.display_name().to_string(),
                    bt,
                    NodePosition::new(20.0, header_height + (i as f64) * (node_height + node_gap)),
                )
                .with_size(NodeSize::new(module_width - 40.0, node_height));
                node.is_placeholder = mb.block.is_placeholder();
                node.description = mb.block.description.clone();
                let node_id = gm.add_node(node);

                if let Some(prev) = prev_node_id {
                    gm.add_wire(Wire::new(prev, "out_l", node_id, "in_l"));
                }
                prev_node_id = Some(node_id);
            }

            let module_id = graph.add_module(gm);

            if let Some(prev_mid) = prev_module_id {
                graph.connect(prev_mid, "out_l", module_id, "in_l");
            }
            prev_module_id = Some(module_id);

            y_offset += module_height + 30.0;
        }

        graph
    }
}
