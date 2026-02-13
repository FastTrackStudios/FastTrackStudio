//! Snapshot types and capture functions for rig state persistence.
//!
//! Provides [`NodeSnapshot`], [`ModuleSnapshot`], [`RigSnapshot`] and the
//! free functions [`capture_node_parameters`], [`capture_module_snapshot`],
//! and [`capture_rig_snapshot`].

use facet::Facet;
use uuid::Uuid;

use super::{GraphModule, Node, NodeGraph};

// ---------------------------------------------------------------------------
// Snapshot Types
// ---------------------------------------------------------------------------

/// Captured parameter state for a single node.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct NodeSnapshot {
    /// The node this snapshot was captured from.
    pub node_id: Uuid,
    /// Node name at capture time (for display).
    pub node_name: String,
    /// Captured parameter values as (param_id, normalized_value) pairs.
    pub parameters: Vec<(String, f64)>,
}

/// Captured state of all nodes within a single module.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ModuleSnapshot {
    /// The module this snapshot was captured from.
    pub module_id: Uuid,
    /// Module name at capture time.
    pub module_name: String,
    /// Captured state for each node in the module.
    pub nodes: Vec<NodeSnapshot>,
}

/// Captured state of the entire rig (all modules and standalone nodes).
#[derive(Debug, Clone, PartialEq, Facet)]
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

// ---------------------------------------------------------------------------
// Capture Functions
// ---------------------------------------------------------------------------

/// Capture the current parameter values from a single node.
///
/// Returns a list of `(param_id, normalized_value)` pairs -- one per parameter
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

/// Capture a snapshot of the entire rig -- all modules and standalone nodes.
pub fn capture_rig_snapshot(graph: &NodeGraph, name: impl Into<String>) -> RigSnapshot {
    fn capture_node_snapshot(node: &Node) -> NodeSnapshot {
        NodeSnapshot {
            node_id: node.id,
            node_name: node.name.clone(),
            parameters: node
                .parameters
                .iter()
                .map(|p| (p.id.clone(), p.value.get()))
                .collect(),
        }
    }

    fn capture_module(module: &GraphModule) -> ModuleSnapshot {
        ModuleSnapshot {
            module_id: module.id,
            module_name: module.name.clone(),
            nodes: module.nodes.iter().map(capture_node_snapshot).collect(),
        }
    }

    RigSnapshot {
        id: Uuid::new_v4(),
        name: name.into(),
        modules: graph.modules.iter().map(capture_module).collect(),
        standalone_nodes: graph.nodes.iter().map(capture_node_snapshot).collect(),
    }
}
