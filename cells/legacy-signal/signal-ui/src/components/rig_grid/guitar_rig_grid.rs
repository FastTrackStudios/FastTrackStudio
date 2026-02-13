//! Guitar rig grid - main content component.
//!
//! Renders the node graph view in either full or compact mode.
//! The graph is stored in the `RIG_NODE_GRAPH` global signal.

use crate::prelude::*;

use super::node_graph_view::NodeGraphView;
use super::view_mode::ModuleViewMode;

/// Props for the guitar rig grid.
#[derive(Props, Clone, PartialEq)]
pub struct GuitarRigGridProps {
    /// Current view mode (Flow or FlowCompact).
    pub view_mode: ModuleViewMode,
}

/// Guitar rig content area — renders the node graph view.
#[component]
pub fn GuitarRigGrid(props: GuitarRigGridProps) -> Element {
    let compact = props.view_mode == ModuleViewMode::FlowCompact;

    rsx! {
        div { class: "h-full w-full overflow-hidden",
            NodeGraphView {
                compact: compact,
            }
        }
    }
}
