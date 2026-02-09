//! Hook for auto-persisting the node graph and snapshots to SQLite.
//!
//! Watches `RIG_NODE_GRAPH` and `RIG_SNAPSHOTS` for changes and saves
//! to the local SQLite database via [`signal_storage::SqliteBackend`].
//! On mount, loads any previously saved state.

use crate::components::rig_grid::node_graph::{NodeGraph, RigSnapshot};
use crate::prelude::*;
use crate::signals::{RIG_NODE_GRAPH, RIG_SNAPSHOTS};
use signal_storage::{load_value, save_value, SqliteBackend};

const GRAPH_KEY: &str = "rig:node_graph";
const SNAPSHOTS_KEY: &str = "rig:snapshots";

/// Hook that persists the node graph and snapshots to SQLite.
///
/// - **On mount**: loads saved graph and snapshots from SQLite (falls back
///   to the existing in-memory state if nothing is saved yet).
/// - **On change**: saves the current graph/snapshots to SQLite.
///
/// Call once in the top-level rig layout component.
pub fn use_graph_persistence() {
    // On mount: load saved state from SQLite
    use_effect(move || {
        spawn(async move {
            let backend = match SqliteBackend::from_default_path().await {
                Ok(b) => b,
                Err(e) => {
                    tracing::warn!("Failed to open persistence backend: {e}");
                    return;
                }
            };

            // Load graph
            match load_value::<NodeGraph>(&backend, GRAPH_KEY).await {
                Ok(Some(graph)) => {
                    tracing::info!("Loaded persisted node graph");
                    *RIG_NODE_GRAPH.write() = graph;
                }
                Ok(None) => {
                    tracing::debug!("No persisted graph found, using default");
                }
                Err(e) => {
                    tracing::warn!("Failed to load persisted graph: {e}");
                }
            }

            // Load snapshots
            match load_value::<Vec<RigSnapshot>>(&backend, SNAPSHOTS_KEY).await {
                Ok(Some(snaps)) => {
                    tracing::info!("Loaded {} persisted snapshots", snaps.len());
                    *RIG_SNAPSHOTS.write() = snaps;
                }
                Ok(None) => {
                    tracing::debug!("No persisted snapshots found");
                }
                Err(e) => {
                    tracing::warn!("Failed to load persisted snapshots: {e}");
                }
            }
        });
    });

    // On graph change: save to SQLite
    // Reading inside use_effect tracks the signal for re-runs.
    use_effect(move || {
        let graph = RIG_NODE_GRAPH.read().clone();
        spawn(async move {
            let backend = match SqliteBackend::from_default_path().await {
                Ok(b) => b,
                Err(e) => {
                    tracing::warn!("Failed to open persistence backend for save: {e}");
                    return;
                }
            };
            if let Err(e) = save_value(&backend, GRAPH_KEY, &graph).await {
                tracing::warn!("Failed to persist node graph: {e}");
            }
        });
    });

    // On snapshots change: save to SQLite
    use_effect(move || {
        let snaps = RIG_SNAPSHOTS.read().clone();
        spawn(async move {
            let backend = match SqliteBackend::from_default_path().await {
                Ok(b) => b,
                Err(e) => {
                    tracing::warn!("Failed to open persistence backend for save: {e}");
                    return;
                }
            };
            if let Err(e) = save_value(&backend, SNAPSHOTS_KEY, &snaps).await {
                tracing::warn!("Failed to persist snapshots: {e}");
            }
        });
    });
}
