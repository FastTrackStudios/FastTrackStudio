//! Hook for auto-persisting the node graph and snapshots to SQLite.
//!
//! Watches `RIG_NODE_GRAPH` and `RIG_SNAPSHOTS` for changes and saves
//! to the local SQLite database via [`signal_storage::SqliteBackend`].
//! On mount, loads any previously saved state.
//!
//! Uses a shared `SqliteBackend` (opened once on mount) connected to the
//! same `~/Music/FastTrackStudio/signal.db` as the main DB connection in
//! `RIG_SERVICE`. The backend operates on the `kv_store` table which is
//! separate from the SeaORM entity tables.

use crate::components::rig_grid::node_graph::{NodeGraph, RigSnapshot};
use crate::prelude::*;
use crate::signals::{RIG_NODE_GRAPH, RIG_SNAPSHOTS};
use signal_storage::{load_value, save_value, SqliteBackend};
use std::sync::Arc;

const GRAPH_KEY: &str = "rig:node_graph";
const SNAPSHOTS_KEY: &str = "rig:snapshots";

/// Shared persistence backend — opened once and reused for all saves/loads.
static KV_BACKEND: GlobalSignal<Option<Arc<SqliteBackend>>> = Signal::global(|| None);

/// Hook that persists the node graph and snapshots to SQLite.
///
/// - **On mount**: opens a shared `SqliteBackend`, loads saved graph and
///   snapshots from SQLite (falls back to in-memory defaults if nothing saved).
/// - **On change**: saves the current graph/snapshots to SQLite.
///
/// Call once in the top-level rig layout component.
pub fn use_graph_persistence() {
    // On mount: open the shared backend and load saved state
    use_effect(move || {
        spawn(async move {
            // Open or reuse the shared backend
            let backend = if let Some(existing) = KV_BACKEND.read().clone() {
                existing
            } else {
                match SqliteBackend::from_default_path().await {
                    Ok(b) => {
                        let shared = Arc::new(b);
                        *KV_BACKEND.write() = Some(shared.clone());
                        shared
                    }
                    Err(e) => {
                        tracing::warn!("Failed to open persistence backend: {e}");
                        return;
                    }
                }
            };

            // Load graph
            match load_value::<NodeGraph>(backend.as_ref(), GRAPH_KEY).await {
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
            match load_value::<Vec<RigSnapshot>>(backend.as_ref(), SNAPSHOTS_KEY).await {
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

    // On graph change: save to SQLite using shared backend
    use_effect(move || {
        let graph = RIG_NODE_GRAPH.read().clone();
        spawn(async move {
            let Some(backend) = KV_BACKEND.read().clone() else {
                return;
            };
            if let Err(e) = save_value(backend.as_ref(), GRAPH_KEY, &graph).await {
                tracing::warn!("Failed to persist node graph: {e}");
            }
        });
    });

    // On snapshots change: save to SQLite using shared backend
    use_effect(move || {
        let snaps = RIG_SNAPSHOTS.read().clone();
        spawn(async move {
            let Some(backend) = KV_BACKEND.read().clone() else {
                return;
            };
            if let Err(e) = save_value(backend.as_ref(), SNAPSHOTS_KEY, &snaps).await {
                tracing::warn!("Failed to persist snapshots: {e}");
            }
        });
    });
}
