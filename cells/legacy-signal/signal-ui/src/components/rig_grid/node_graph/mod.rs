//! Node-based signal flow model.
//!
//! Represents audio processing nodes positioned on an infinite 2D canvas
//! with wire connections between them. Similar to Gig Performer, Reaktor, etc.

mod builder;
pub mod models;
mod sample_rig;
pub mod snapshot;

// Re-export all public types at the module level so existing
// `use super::node_graph::{Node, Wire, ...}` imports keep working.
pub use models::{
    GraphModule, Node, NodeGraph, NodeParameter, NodePort, NodePosition, NodeSize, NodeWidget,
    ParameterType, Wire,
};
pub use snapshot::{
    capture_module_snapshot, capture_node_parameters, capture_rig_snapshot, ModuleSnapshot,
    NodeSnapshot, RigSnapshot,
};

use uuid::Uuid;

use models::*;

// ---------------------------------------------------------------------------
// Core NodeGraph impl (new, add/remove/find, connect, disconnect, layout)
// ---------------------------------------------------------------------------

impl NodeGraph {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_module(&mut self, module: GraphModule) -> Uuid {
        let id = module.id;
        self.modules.push(module);
        id
    }

    pub fn find_module(&self, id: Uuid) -> Option<&GraphModule> {
        self.modules.iter().find(|m| m.id == id)
    }

    pub fn find_module_mut(&mut self, id: Uuid) -> Option<&mut GraphModule> {
        self.modules.iter_mut().find(|m| m.id == id)
    }

    pub fn module_at(&self, x: f64, y: f64) -> Option<&GraphModule> {
        self.modules.iter().rev().find(|m| m.contains_point(x, y))
    }

    pub fn add_node(&mut self, node: Node) -> Uuid {
        let id = node.id;
        self.nodes.push(node);
        id
    }

    pub fn remove_module(&mut self, id: Uuid) {
        self.modules.retain(|m| m.id != id);
        self.wires.retain(|w| w.from_node != id && w.to_node != id);
    }

    pub fn remove_node(&mut self, id: Uuid) {
        self.nodes.retain(|n| n.id != id);
        self.wires.retain(|w| w.from_node != id && w.to_node != id);
    }

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
        if from_node == to_node {
            return None;
        }
        let from_port = from_port.into();
        let to_port = to_port.into();
        if self.has_wire(from_node, &from_port, to_node, &to_port) {
            return None;
        }
        let wire = Wire::new(from_node, from_port, to_node, to_port);
        let id = wire.id;
        self.wires.push(wire);
        Some(id)
    }

    pub fn find_node(&self, id: Uuid) -> Option<&Node> {
        if let Some(node) = self.nodes.iter().find(|n| n.id == id) {
            return Some(node);
        }
        for module in &self.modules {
            if let Some(node) = module.find_node(id) {
                return Some(node);
            }
        }
        None
    }

    pub fn find_node_mut(&mut self, id: Uuid) -> Option<&mut Node> {
        if let Some(node) = self.nodes.iter_mut().find(|n| n.id == id) {
            return Some(node);
        }
        for module in &mut self.modules {
            if let Some(node) = module.find_node_mut(id) {
                return Some(node);
            }
        }
        None
    }

    pub fn node_at(&self, x: f64, y: f64) -> Option<&Node> {
        self.nodes.iter().rev().find(|n| n.contains_point(x, y))
    }

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

    pub fn disconnect(&mut self, id: Uuid) {
        self.wires.retain(|w| w.id != id);
    }

    /// Automatically arrange modules vertically with proper spacing.
    pub fn compact_layout(&mut self, gap: f64) {
        if self.modules.is_empty() {
            return;
        }

        let mut indices: Vec<usize> = (0..self.modules.len()).collect();
        indices.sort_by(|a, b| {
            self.modules[*a]
                .position
                .y
                .partial_cmp(&self.modules[*b].position.y)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

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

        let mut y = 50.0;
        for row in &rows {
            for &idx in row {
                self.modules[idx].position.y = y;
            }
            let max_height = row
                .iter()
                .map(|&idx| self.modules[idx].size.height)
                .fold(0.0f64, f64::max);
            y += max_height + gap;
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use signal_control::block::BlockType;
    use signal_control::normalized::NormalizedF64;

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    fn sample_node_with_params(name: &str, position: NodePosition) -> Node {
        Node::new(name, BlockType::Drive, position).with_parameters(vec![
            NodeParameter::new("drive", "Drive", NormalizedF64::new(0.7)),
            NodeParameter::new("tone", "Tone", NormalizedF64::new(0.5)),
            NodeParameter::new("level", "Level", NormalizedF64::new(0.8)),
        ])
    }

    #[test]
    fn test_node_parameter_new() -> Result<()> {
        let param = NodeParameter::new("drive", "Drive", NormalizedF64::new(0.7));
        assert_eq!(param.id, "drive");
        assert_eq!(param.name, "Drive");
        assert!((param.value.get() - 0.7).abs() < f64::EPSILON);
        Ok(())
    }

    #[test]
    fn test_node_parameter_value_clamped() -> Result<()> {
        let param = NodeParameter::new("gain", "Gain", NormalizedF64::new(1.5));
        assert_eq!(param.value.get(), 1.0);
        Ok(())
    }

    #[test]
    fn test_node_with_parameters() -> Result<()> {
        let node = sample_node_with_params("Drive", NodePosition::new(0.0, 0.0));
        assert_eq!(node.parameters.len(), 3);
        assert_eq!(node.parameters[0].id, "drive");
        assert_eq!(node.parameters[1].id, "tone");
        assert_eq!(node.parameters[2].id, "level");
        Ok(())
    }

    #[test]
    fn test_node_default_has_no_parameters() -> Result<()> {
        let node = Node::new("Test", BlockType::Input, NodePosition::new(0.0, 0.0));
        assert!(node.parameters.is_empty());
        Ok(())
    }

    #[test]
    fn test_capture_node_parameters_found() -> Result<()> {
        let mut graph = NodeGraph::new();
        let node = sample_node_with_params("Drive", NodePosition::new(0.0, 0.0));
        let node_id = node.id;
        graph.add_node(node);
        let captured = capture_node_parameters(&graph, node_id);
        assert_eq!(captured.len(), 3);
        for (id, _value) in &captured {
            assert_eq!(*id, node_id);
        }
        assert!((captured[0].1 - 0.7).abs() < f64::EPSILON);
        Ok(())
    }

    #[test]
    fn test_capture_node_parameters_not_found() -> Result<()> {
        let graph = NodeGraph::new();
        let captured = capture_node_parameters(&graph, Uuid::new_v4());
        assert!(captured.is_empty());
        Ok(())
    }

    #[test]
    fn test_capture_node_parameters_in_module() -> Result<()> {
        let mut graph = NodeGraph::new();
        let node = sample_node_with_params("Drive", NodePosition::new(10.0, 50.0));
        let node_id = node.id;
        let mut module =
            GraphModule::new("Test Module", BlockType::Drive, NodePosition::new(0.0, 0.0));
        module.add_node(node);
        graph.add_module(module);
        let captured = capture_node_parameters(&graph, node_id);
        assert_eq!(captured.len(), 3);
        Ok(())
    }

    #[test]
    fn test_capture_module_snapshot_found() -> Result<()> {
        let mut graph = NodeGraph::new();
        let node1 = sample_node_with_params("Drive 1", NodePosition::new(10.0, 50.0));
        let node2 = sample_node_with_params("Drive 2", NodePosition::new(200.0, 50.0));
        let mut module =
            GraphModule::new("Drive Stage", BlockType::Drive, NodePosition::new(0.0, 0.0));
        module.add_node(node1);
        module.add_node(node2);
        let module_id = module.id;
        graph.add_module(module);
        let snapshot = capture_module_snapshot(&graph, module_id);
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
        let graph = NodeGraph::new();
        let snapshot = capture_module_snapshot(&graph, Uuid::new_v4());
        assert!(snapshot.is_none());
        Ok(())
    }

    #[test]
    fn test_capture_rig_snapshot_empty_graph() -> Result<()> {
        let graph = NodeGraph::new();
        let snapshot = capture_rig_snapshot(&graph, "Empty Rig");
        assert_eq!(snapshot.name, "Empty Rig");
        assert!(snapshot.modules.is_empty());
        assert!(snapshot.standalone_nodes.is_empty());
        Ok(())
    }

    #[test]
    fn test_capture_rig_snapshot_full_graph() -> Result<()> {
        let mut graph = NodeGraph::new();
        let node1 = sample_node_with_params("Drive", NodePosition::new(10.0, 50.0));
        let mut module = GraphModule::new(
            "Drive Module",
            BlockType::Drive,
            NodePosition::new(0.0, 0.0),
        );
        module.add_node(node1);
        graph.add_module(module);
        let standalone = sample_node_with_params("Standalone EQ", NodePosition::new(500.0, 0.0));
        graph.add_node(standalone);
        let snapshot = capture_rig_snapshot(&graph, "Full Rig Capture");
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
        let mut graph = NodeGraph::new();
        let node =
            Node::new("Custom", BlockType::Eq, NodePosition::new(0.0, 0.0)).with_parameters(vec![
                NodeParameter::new("freq", "Frequency", NormalizedF64::new(0.3)),
                NodeParameter::new("gain", "Gain", NormalizedF64::new(0.9)),
            ]);
        graph.add_node(node);
        let snapshot = capture_rig_snapshot(&graph, "Value Test");
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
        let graph = NodeGraph::new();
        let snap1 = capture_rig_snapshot(&graph, "Snap 1");
        let snap2 = capture_rig_snapshot(&graph, "Snap 2");
        assert_ne!(snap1.id, snap2.id);
        Ok(())
    }
}
