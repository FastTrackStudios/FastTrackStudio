//! Hook for undo/redo on the node graph.
//!
//! Provides `use_graph_history()` which exposes undo/redo callbacks
//! and reactively tracks whether each stack is non-empty so toolbar
//! buttons can show the correct disabled state.

use crate::components::rig_grid::node_graph::GraphOperation;
use crate::prelude::*;
use crate::signals::{GRAPH_HISTORY, RIG_NODE_GRAPH};

/// Undo/redo handle returned by [`use_graph_history`].
#[derive(Clone)]
pub struct GraphHistoryHandle {
    /// Undo the last graph operation.
    pub undo: Callback<()>,
    /// Redo the last undone operation.
    pub redo: Callback<()>,
    /// Whether there is anything to undo.
    pub can_undo: bool,
    /// Whether there is anything to redo.
    pub can_redo: bool,
}

/// Hook that provides undo/redo capabilities for the node graph.
///
/// Reads `GRAPH_HISTORY` reactively so the caller re-renders whenever the
/// stack state changes (e.g. a button's `disabled` attribute updates).
///
/// # Example
///
/// ```ignore
/// let history = use_graph_history();
/// rsx! {
///     button { disabled: !history.can_undo, onclick: move |_| history.undo.call(()), "Undo" }
///     button { disabled: !history.can_redo, onclick: move |_| history.redo.call(()), "Redo" }
/// }
/// ```
pub fn use_graph_history() -> GraphHistoryHandle {
    let history = GRAPH_HISTORY.read();
    let can_undo = history.can_undo();
    let can_redo = history.can_redo();

    GraphHistoryHandle {
        undo: Callback::new(move |_: ()| {
            let mut history = GRAPH_HISTORY.write();
            let mut graph = RIG_NODE_GRAPH.write();
            history.undo(&mut graph);
        }),
        redo: Callback::new(move |_: ()| {
            let mut history = GRAPH_HISTORY.write();
            let mut graph = RIG_NODE_GRAPH.write();
            history.redo(&mut graph);
        }),
        can_undo,
        can_redo,
    }
}

/// Record a graph operation in the undo history.
///
/// Call this from any component after mutating `RIG_NODE_GRAPH` so the
/// operation can be undone later.
pub fn record_graph_op(op: GraphOperation) {
    GRAPH_HISTORY.write().push_operation(op);
}
