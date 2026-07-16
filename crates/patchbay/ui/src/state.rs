//! Global signals + the service handle.

use std::collections::HashMap;
use std::sync::Arc;

use dioxus::prelude::*;
use patchbay_proto::{
    ApplyReport, ClockInfo, DanteDevice, DanteStatus, GraphEvent, GraphSnapshot, MediaKind,
    PatchbayServiceClient, RoutingPreset, ServiceStatus,
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
pub static SERVICES: GlobalSignal<Vec<ServiceStatus>> = Signal::global(Vec::new);
pub static LATENCY_RULES: GlobalSignal<Vec<patchbay_proto::LatencyRule>> = Signal::global(Vec::new);
pub static CLOCK_DEFAULTS: GlobalSignal<patchbay_proto::ClockDefaults> =
    Signal::global(patchbay_proto::ClockDefaults::default);
pub static DANTE_DEVICES: GlobalSignal<Vec<DanteDevice>> = Signal::global(Vec::new);
/// Dante grid fetch in flight.
pub static DANTE_LOADING: GlobalSignal<bool> = Signal::global(|| false);
/// Last dante grid error (empty = fine).
pub static DANTE_ERROR: GlobalSignal<String> = Signal::global(String::new);

/// Which main view is showing.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum View {
    Patchbay,
    Dante,
}
pub static VIEW: GlobalSignal<View> = Signal::global(|| View::Patchbay);

// ─── View state ─────────────────────────────────────────────────────────

/// Output port armed for connecting (click an input port to link).
pub static SELECTED_OUTPUT: GlobalSignal<Option<u32>> = Signal::global(|| None);
/// Node id whose inspector is open.
pub static SELECTED_NODE: GlobalSignal<Option<u32>> = Signal::global(|| None);
pub static SEARCH: GlobalSignal<String> = Signal::global(String::new);
/// Which media domain the graph shows (Audio | MIDI | Video tabs).
/// `Other`-kind ports ride along in the Audio tab.
pub static MEDIA_TAB: GlobalSignal<MediaKind> = Signal::global(|| MediaKind::Audio);
/// Canvas zoom factor.
pub static ZOOM: GlobalSignal<f64> = Signal::global(|| 1.0);
/// Canvas pan offset (px, pre-zoom screen space).
pub static PAN: GlobalSignal<(f64, f64)> = Signal::global(|| (0.0, 0.0));
pub static HIDE_UNCONNECTED: GlobalSignal<bool> = Signal::global(|| false);
/// Drop sinks' monitor ports from the canvas entirely.
pub static HIDE_MONITORS: GlobalSignal<bool> = Signal::global(|| false);
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
        GraphEvent::Reset => *g = GraphSnapshot::default(),
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

/// Replace the graph mirror wholesale (periodic reconcile — the event
/// stream can drop under burst; the snapshot is always authoritative).
/// Skips the signal write when nothing changed so it never causes
/// re-renders on a quiet graph.
pub fn replace_graph(snap: GraphSnapshot) {
    if *GRAPH.peek() != snap {
        *GRAPH.write() = snap;
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
    if let Ok(services) = handle.0.services().await {
        *SERVICES.write() = services;
    }
    if let Ok(rules) = handle.0.latency_rules().await {
        *LATENCY_RULES.write() = rules;
    }
    if let Ok(defaults) = handle.0.clock_defaults().await {
        *CLOCK_DEFAULTS.write() = defaults;
    }
}

/// Re-scan the Dante network (mDNS + per-device ARC — seconds).
pub async fn refresh_dante(handle: &PatchbayHandle) {
    *DANTE_LOADING.write() = true;
    match handle.0.dante_network().await {
        Ok(devices) => {
            *DANTE_DEVICES.write() = devices;
            DANTE_ERROR.write().clear();
        }
        Err(e) => *DANTE_ERROR.write() = format!("dante scan failed: {e}"),
    }
    *DANTE_LOADING.write() = false;
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

/// Portable async sleep (tokio on native, gloo on wasm).
pub(crate) async fn sleep_secs(secs: u64) {
    #[cfg(not(target_arch = "wasm32"))]
    tokio::time::sleep(std::time::Duration::from_secs(secs)).await;
    #[cfg(target_arch = "wasm32")]
    gloo_timers::future::TimeoutFuture::new((secs * 1000) as u32).await;
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
