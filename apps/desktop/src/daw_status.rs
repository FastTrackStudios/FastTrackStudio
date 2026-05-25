//! Live DAW status fed by the REAPER-hosted fts-extensions EventBus.
//!
//! Mirrors the fts-cli dashboard: one `EventBus::subscribe(BusFilter::all())`
//! multiplexes transport / track / marker / region / tempo / project events
//! over a single Vox channel. Counts are seeded once and re-fetched when their
//! domain emits an event; transport state/tempo update live from push events.
//! The result drives a read-only [`DawStatusPanel`].

use dioxus::prelude::*;

use daw_proto::ProjectContext;
use daw_proto::event_bus::{BusFilter, DawEvent, EventBusClient};
use daw_proto::marker::MarkersClient;
use daw_proto::project::ProjectsClient;
use daw_proto::region::RegionsClient;
use daw_proto::tempo_map::TempoMapClient;
use daw_proto::track::TracksClient;
use daw_proto::transport::TransportEvent;

#[derive(Clone, PartialEq, Default)]
pub struct DawStatus {
    /// True once the EventBus subscription is live.
    pub subscribed: bool,
    pub project: Option<String>,
    pub play_state: Option<String>,
    pub tempo: Option<f64>,
    pub tracks: Option<u32>,
    pub markers: Option<u32>,
    pub regions: Option<u32>,
    pub tempo_points: Option<u32>,
    pub event_count: u64,
    pub last_event: Option<String>,
}

pub static DAW_STATUS: GlobalSignal<DawStatus> = Signal::global(DawStatus::default);

/// Re-fetch every count + the current project (used to seed at startup and on
/// a project switch). Each RPC `await` completes before the (short) signal
/// write, so no signal guard is held across an await.
async fn refetch_all(
    tracks: &TracksClient,
    markers: &MarkersClient,
    regions: &RegionsClient,
    tempo: &TempoMapClient,
    projects: &ProjectsClient,
) {
    let t = tracks.count(ProjectContext::Current).await.ok();
    let m = markers.count(ProjectContext::Current).await.ok();
    let r = regions.count(ProjectContext::Current).await.ok();
    let tp = tempo.tempo_point_count(ProjectContext::Current).await.ok();
    let proj = projects.current().await.ok().flatten();

    let mut s = DAW_STATUS.write();
    s.tracks = t;
    s.markers = m;
    s.regions = r;
    s.tempo_points = tp;
    s.project = proj.map(|p| p.name.to_string());
}

fn apply_transport(te: &TransportEvent) {
    let mut s = DAW_STATUS.write();
    match te {
        TransportEvent::PlayStateChanged { play_state, .. } => {
            s.play_state = Some(format!("{play_state:?}"));
        }
        TransportEvent::TempoChanged { tempo, .. } => {
            s.tempo = Some(tempo.bpm());
        }
        TransportEvent::Snapshot { state, .. } => {
            s.play_state = Some(format!("{:?}", state.play_state));
            s.tempo = Some(state.tempo.bpm());
        }
        _ => {}
    }
}

fn variant_label(event: &DawEvent) -> &'static str {
    match event {
        DawEvent::Track(_) => "track",
        DawEvent::Marker(_) => "marker",
        DawEvent::Region(_) => "region",
        DawEvent::TempoMap(_) => "tempo",
        DawEvent::TransportState(_) => "transport",
        DawEvent::TransportPosition(_) => "position",
        DawEvent::Project(_) => "project",
    }
}

/// Subscribe to the DAW EventBus and keep [`DAW_STATUS`] current. Returns when
/// the stream closes/errors (the caller retries). Must run inside the Dioxus
/// runtime (a `use_future`/`spawn`) so the signal writes are valid.
pub async fn run_daw_event_bus(caller: vox::Caller) {
    let bus = EventBusClient::new(caller.clone());
    let tracks = TracksClient::new(caller.clone());
    let markers = MarkersClient::new(caller.clone());
    let regions = RegionsClient::new(caller.clone());
    let tempo = TempoMapClient::new(caller.clone());
    let projects = ProjectsClient::new(caller.clone());

    refetch_all(&tracks, &markers, &regions, &tempo, &projects).await;

    let (tx, mut rx) = vox::channel::<DawEvent>();
    if let Err(e) = bus.subscribe(BusFilter::all(), tx).await {
        tracing::error!("DAW EventBus subscribe failed: {e:?}");
        return;
    }
    DAW_STATUS.write().subscribed = true;
    tracing::info!("Subscribed to DAW EventBus");

    while let Ok(Some(event_ref)) = rx.recv().await {
        let event = event_ref.get();
        let label = variant_label(event);

        // Live transport state (no await; short write).
        if let DawEvent::TransportState(te) = event {
            apply_transport(te);
        }

        {
            let mut s = DAW_STATUS.write();
            s.event_count = s.event_count.wrapping_add(1);
            s.last_event = Some(label.to_string());
        }

        // Structural domains: re-fetch the affected count off the lock.
        match event {
            DawEvent::Track(_) => {
                if let Ok(v) = tracks.count(ProjectContext::Current).await {
                    DAW_STATUS.write().tracks = Some(v);
                }
            }
            DawEvent::Marker(_) => {
                if let Ok(v) = markers.count(ProjectContext::Current).await {
                    DAW_STATUS.write().markers = Some(v);
                }
            }
            DawEvent::Region(_) => {
                if let Ok(v) = regions.count(ProjectContext::Current).await {
                    DAW_STATUS.write().regions = Some(v);
                }
            }
            DawEvent::TempoMap(_) => {
                if let Ok(v) = tempo.tempo_point_count(ProjectContext::Current).await {
                    DAW_STATUS.write().tempo_points = Some(v);
                }
            }
            DawEvent::Project(_) => {
                // Project switch / open / close — counts are per-project, so
                // re-seed everything against the new current project.
                refetch_all(&tracks, &markers, &regions, &tempo, &projects).await;
            }
            _ => {}
        }
    }

    DAW_STATUS.write().subscribed = false;
    tracing::info!("DAW EventBus stream ended");
}

fn count_str(v: Option<u32>) -> String {
    v.map(|n| n.to_string()).unwrap_or_else(|| "—".to_string())
}

#[component]
pub fn DawStatusPanel() -> Element {
    let s = DAW_STATUS();

    let project = s.project.clone().unwrap_or_else(|| "—".to_string());
    let transport = s.play_state.clone().unwrap_or_else(|| "—".to_string());
    let tempo = s
        .tempo
        .map(|t| format!("{t:.2} BPM"))
        .unwrap_or_else(|| "—".to_string());
    let tracks = count_str(s.tracks);
    let markers = count_str(s.markers);
    let regions = count_str(s.regions);
    let tempo_points = count_str(s.tempo_points);
    let event_count = s.event_count;
    let last_event = s.last_event.clone();

    rsx! {
        div { class: "p-6 max-w-2xl mx-auto flex flex-col gap-4",
            h1 { class: "text-2xl font-bold", "DAW Status" }
            div { class: "border border-neutral-300 rounded-lg p-4 flex flex-col gap-2 text-sm",
                if !s.subscribed {
                    p { class: "text-yellow-600", "Waiting for REAPER / fts-extensions…" }
                }
                Row { label: "Project", value: project }
                Row { label: "Transport", value: transport }
                Row { label: "Tempo", value: tempo }
                Row { label: "Tracks", value: tracks }
                Row { label: "Markers", value: markers }
                Row { label: "Regions", value: regions }
                Row { label: "Tempo points", value: tempo_points }
                div { class: "flex items-center justify-between py-1",
                    span { class: "opacity-60", "Events" }
                    span { class: "font-mono",
                        "{event_count}"
                        if let Some(e) = last_event {
                            span { class: "opacity-50 ml-2", "(last: {e})" }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn Row(label: &'static str, value: String) -> Element {
    rsx! {
        div { class: "flex items-center justify-between py-1 border-b border-neutral-100",
            span { class: "opacity-60", "{label}" }
            span { class: "font-mono", "{value}" }
        }
    }
}
