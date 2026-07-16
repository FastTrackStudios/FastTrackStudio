//! Global signals + the service handle.

use std::collections::HashMap;
use std::sync::Arc;

use dioxus::prelude::*;
use patchbay_proto::{
    ApplyReport, ClockInfo, DanteStatus, GraphEvent, GraphSnapshot, MediaKind,
    PatchbayServiceClient, RoutingPreset,
};

/// The service client, provided via context by the shell.
#[derive(Clone)]
pub struct PatchbayHandle(pub Arc<PatchbayServiceClient>);

/// Convenience accessor for components.
pub fn use_patchbay() -> PatchbayHandle {
    use_context::<PatchbayHandle>()
}

// ─── Data mirrors ───────────────────────────────────────────────────────

pub static GRAPH: GlobalSignal<GraphSnapshot> = Signal::global(GraphSnapshot::default);
/// `target → alias` (`node.name` or `node.name:port.name`).
pub static ALIASES: GlobalSignal<HashMap<String, String>> = Signal::global(HashMap::new);
pub static PRESETS: GlobalSignal<Vec<RoutingPreset>> = Signal::global(Vec::new);
pub static CLOCK: GlobalSignal<ClockInfo> = Signal::global(ClockInfo::default);
pub static DANTE: GlobalSignal<DanteStatus> = Signal::global(DanteStatus::default);

// ─── View state ─────────────────────────────────────────────────────────

/// Output port armed for connecting (click an input port to link).
pub static SELECTED_OUTPUT: GlobalSignal<Option<u32>> = Signal::global(|| None);
/// Node id whose inspector is open.
pub static SELECTED_NODE: GlobalSignal<Option<u32>> = Signal::global(|| None);
pub static SEARCH: GlobalSignal<String> = Signal::global(String::new);
/// Media kinds currently visible.
pub static KIND_FILTER: GlobalSignal<Vec<MediaKind>> = Signal::global(|| {
    vec![
        MediaKind::Audio,
        MediaKind::Video,
        MediaKind::Midi,
        MediaKind::Other,
    ]
});
pub static HIDE_UNCONNECTED: GlobalSignal<bool> = Signal::global(|| false);
/// Port-group expansion (`node.name/direction/prefix` → expanded).
/// Groups default to collapsed — that's the whole point with 128-channel
/// Inferno nodes.
pub static EXPANDED_GROUPS: GlobalSignal<HashMap<String, bool>> = Signal::global(HashMap::new);
/// Outcome of the last preset apply, for the status line.
pub static LAST_REPORT: GlobalSignal<Option<(String, ApplyReport)>> = Signal::global(|| None);

// ─── Mutations ──────────────────────────────────────────────────────────

/// Fold one engine event into the graph mirror (idempotent — replayed
/// events after a snapshot fetch are harmless).
pub fn apply_graph_event(ev: &GraphEvent) {
    let mut g = GRAPH.write();
    match ev {
        GraphEvent::NodeAdded(n) => {
            g.nodes.retain(|x| x.id != n.id);
            g.nodes.push(n.clone());
            g.nodes.sort_by_key(|x| x.id);
        }
        GraphEvent::NodeRemoved { id } => g.nodes.retain(|x| x.id != *id),
        GraphEvent::PortAdded(p) => {
            g.ports.retain(|x| x.id != p.id);
            g.ports.push(p.clone());
            g.ports.sort_by_key(|x| x.id);
        }
        GraphEvent::PortRemoved { id, .. } => g.ports.retain(|x| x.id != *id),
        GraphEvent::LinkAdded(l) => {
            g.links.retain(|x| x.id != l.id);
            g.links.push(l.clone());
            g.links.sort_by_key(|x| x.id);
        }
        GraphEvent::LinkStateChanged { id, active } => {
            if let Some(l) = g.links.iter_mut().find(|x| x.id == *id) {
                l.active = *active;
            }
        }
        GraphEvent::LinkRemoved { id } => g.links.retain(|x| x.id != *id),
    }
}

/// Fetch everything renderable (initial mount + manual refresh).
pub async fn refresh_all(handle: &PatchbayHandle) {
    match handle.0.graph().await {
        Ok(snap) => *GRAPH.write() = snap,
        Err(e) => tracing::warn!("graph fetch failed: {e:?}"),
    }
    refresh_meta(handle).await;
}

/// The cheap non-graph state (aliases, presets, clock, dante).
pub async fn refresh_meta(handle: &PatchbayHandle) {
    if let Ok(aliases) = handle.0.aliases().await {
        *ALIASES.write() = aliases.into_iter().map(|a| (a.target, a.alias)).collect();
    }
    if let Ok(presets) = handle.0.list_presets().await {
        *PRESETS.write() = presets;
    }
    if let Ok(clock) = handle.0.clock().await {
        *CLOCK.write() = clock;
    }
    if let Ok(dante) = handle.0.dante_status().await {
        *DANTE.write() = dante;
    }
}

/// Connect-or-disconnect between an output and input port (helvum's
/// toggle semantics). The graph mirror updates via the event stream.
pub fn toggle_link(handle: PatchbayHandle, output_port: u32, input_port: u32) {
    let existing = GRAPH
        .peek()
        .links
        .iter()
        .find(|l| l.output_port == output_port && l.input_port == input_port)
        .map(|l| l.id);
    spawn(async move {
        let res = match existing {
            Some(id) => handle.0.destroy_link(id).await,
            None => handle.0.create_link(output_port, input_port).await,
        };
        if let Err(e) = res {
            tracing::warn!("link toggle failed: {e:?}");
        }
    });
}

/// Display name for a node (alias wins).
pub fn node_label(name: &str, label: &str) -> String {
    ALIASES
        .read()
        .get(name)
        .cloned()
        .unwrap_or_else(|| label.to_string())
}

/// Display name for a port (alias wins).
pub fn port_label(node_name: &str, port_name: &str) -> String {
    ALIASES
        .read()
        .get(&format!("{node_name}:{port_name}"))
        .cloned()
        .unwrap_or_else(|| port_name.to_string())
}
