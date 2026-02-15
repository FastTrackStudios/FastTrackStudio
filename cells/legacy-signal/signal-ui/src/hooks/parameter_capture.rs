//! Hook for capturing rig parameter snapshots.
//!
//! Provides `use_parameter_capture()` which reads the current node graph
//! from `RIG_NODE_GRAPH`, captures all parameter values, and stores the
//! resulting `RigSnapshot` in `RIG_SNAPSHOTS`.

use crate::components::rig_grid::node_graph::{capture_rig_snapshot, RigSnapshot};
use crate::prelude::*;
use crate::signals::{RIG_NODE_GRAPH, RIG_SNAPSHOTS};

/// Hook that returns a callback for saving rig snapshots.
///
/// When called with a snapshot name, it reads all current parameters from
/// the node graph and stores a new `RigSnapshot` in `RIG_SNAPSHOTS`.
///
/// # Example
///
/// ```ignore
/// let save_snapshot = use_parameter_capture();
/// save_snapshot.call("My Verse Sound".to_string());
/// ```
pub fn use_parameter_capture() -> Callback<String> {
    Callback::new(move |name: String| {
        let graph = RIG_NODE_GRAPH.read();
        let snapshot = capture_rig_snapshot(&graph, name);
        tracing::info!(
            "Captured rig snapshot '{}' (id={})",
            snapshot.name,
            snapshot.id
        );
        RIG_SNAPSHOTS.write().push(snapshot);
    })
}

/// Get the current list of saved snapshots.
///
/// Returns a clone of the snapshots vec — use sparingly for display.
pub fn get_saved_snapshots() -> Vec<RigSnapshot> {
    RIG_SNAPSHOTS.read().clone()
}
