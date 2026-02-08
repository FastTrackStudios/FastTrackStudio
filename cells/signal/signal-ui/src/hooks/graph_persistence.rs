//! Hook for auto-persisting the node graph to `.signal/graph.json`.
//!
//! Watches `RIG_NODE_GRAPH` for changes and saves to disk automatically.

use crate::components::rig_grid::node_graph::FileGraphPersistence;
use crate::components::rig_grid::node_graph::GraphPersistence;
use crate::prelude::*;
use crate::signals::RIG_NODE_GRAPH;

/// Hook that watches the node graph signal and persists changes to disk.
///
/// Call once at app startup (e.g. alongside `use_rig_subscription()`).
/// Uses `FileGraphPersistence` to write `.signal/graph.json` whenever
/// the `RIG_NODE_GRAPH` global signal is modified.
pub fn use_graph_persistence() {
    use_effect(move || {
        let graph = RIG_NODE_GRAPH.read();
        let persistence = FileGraphPersistence::new();
        if let Err(e) = persistence.save(&graph) {
            tracing::warn!("Failed to persist node graph: {e}");
        }
    });
}
