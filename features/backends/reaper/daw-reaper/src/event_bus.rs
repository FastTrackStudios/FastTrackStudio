//! Reaper backend impl for [`daw_proto::event_bus::EventBus`].
//!
//! Multiplexes the per-domain hub broadcasters into one `Tx<DawEvent>`
//! based on the caller's `BusFilter`. Each subscription spawns one
//! forwarder task driven by `tokio::select!` — disabled domains never
//! subscribe a broadcast receiver, so they cost nothing.

use daw_proto::event_bus::{BusFilter, DawEvent, EventBus};
use daw_proto::marker::MarkerStreamEvent;
use daw_proto::region::RegionStreamEvent;
use daw_proto::tempo_map::TempoMapStreamEvent;
use daw_proto::track::TrackStreamEvent;
use daw_proto::transport::TransportEvent;
use tokio::sync::broadcast::error::RecvError;
use vox::Tx;

impl EventBus for crate::Reaper {
    async fn subscribe(&self, filter: BusFilter, tx: Tx<DawEvent>) {
        if !filter.any() {
            return;
        }
        let hub = crate::event_hub::hub();
        let mut track_rx = filter.tracks.then(|| hub.subscribe_tracks());
        let mut marker_rx = filter.markers.then(|| hub.subscribe_markers());
        let mut region_rx = filter.regions.then(|| hub.subscribe_regions());
        let mut tempo_rx = filter.tempo_map.then(|| hub.subscribe_tempo_map());
        let mut state_rx = filter
            .transport_state
            .then(|| hub.subscribe_transport_state());
        let mut position_rx = filter.transport_position.then(|| hub.subscribe_position());
        let mut project_rx = filter.projects.then(|| hub.subscribe_projects());

        tokio::task::spawn(async move {
            loop {
                tokio::select! {
                    biased;
                    res = async { track_rx.as_mut().unwrap().recv().await }, if track_rx.is_some() => {
                        if !forward(&tx, res, &filter, "tracks", track_guid, DawEvent::Track, &mut track_rx).await {
                            return;
                        }
                    }
                    res = async { marker_rx.as_mut().unwrap().recv().await }, if marker_rx.is_some() => {
                        if !forward(&tx, res, &filter, "markers", marker_guid, DawEvent::Marker, &mut marker_rx).await {
                            return;
                        }
                    }
                    res = async { region_rx.as_mut().unwrap().recv().await }, if region_rx.is_some() => {
                        if !forward(&tx, res, &filter, "regions", region_guid, DawEvent::Region, &mut region_rx).await {
                            return;
                        }
                    }
                    res = async { tempo_rx.as_mut().unwrap().recv().await }, if tempo_rx.is_some() => {
                        if !forward(&tx, res, &filter, "tempo_map", tempo_guid, DawEvent::TempoMap, &mut tempo_rx).await {
                            return;
                        }
                    }
                    res = async { state_rx.as_mut().unwrap().recv().await }, if state_rx.is_some() => {
                        if !forward(&tx, res, &filter, "transport_state", transport_guid, DawEvent::TransportState, &mut state_rx).await {
                            return;
                        }
                    }
                    res = async { project_rx.as_mut().unwrap().recv().await }, if project_rx.is_some() => {
                        // Project events ignore project_guid filter —
                        // subscribers need them to know when to swap
                        // their scoped guid.
                        if !forward(&tx, res, &BusFilter::default(), "projects", |_| "", DawEvent::Project, &mut project_rx).await {
                            return;
                        }
                    }
                    res = async { position_rx.as_mut().unwrap().recv().await }, if position_rx.is_some() => {
                        // Position is continuous — drop on lag without
                        // logging, otherwise treat like the others.
                        match res {
                            Ok(tick) => {
                                if filter.project_rejects(&tick.project_guid) {
                                    // not interesting for this subscriber
                                } else if tx.send(DawEvent::TransportPosition(tick)).await.is_err() {
                                    return;
                                }
                            }
                            Err(RecvError::Closed) => { position_rx = None; }
                            Err(RecvError::Lagged(_)) => {}
                        }
                    }
                }
                if track_rx.is_none()
                    && marker_rx.is_none()
                    && region_rx.is_none()
                    && tempo_rx.is_none()
                    && state_rx.is_none()
                    && position_rx.is_none()
                    && project_rx.is_none()
                {
                    return;
                }
            }
        });
    }
}

// ── Per-domain guid extractors ──────────────────────────────────────
//
// Each domain's stream-event carries `project_guid` somewhere; the
// extractor signature is `fn(&T) -> &str` so `forward` can check the
// caller's filter without knowing the event shape.

fn track_guid(e: &TrackStreamEvent) -> &str {
    &e.project_guid
}
fn marker_guid(e: &MarkerStreamEvent) -> &str {
    &e.project_guid
}
fn region_guid(e: &RegionStreamEvent) -> &str {
    &e.project_guid
}
fn tempo_guid(e: &TempoMapStreamEvent) -> &str {
    &e.project_guid
}
fn transport_guid(e: &TransportEvent) -> &str {
    match e {
        TransportEvent::PlayStateChanged { project_guid, .. }
        | TransportEvent::RecordModeChanged { project_guid, .. }
        | TransportEvent::LoopingChanged { project_guid, .. }
        | TransportEvent::MetronomeChanged { project_guid, .. }
        | TransportEvent::LoopRegionChanged { project_guid, .. }
        | TransportEvent::TimeSelectionChanged { project_guid, .. }
        | TransportEvent::TempoChanged { project_guid, .. }
        | TransportEvent::PlayrateChanged { project_guid, .. }
        | TransportEvent::Snapshot { project_guid, .. } => project_guid,
    }
}

/// Forwarder helper for occasional channels — log on lag, drop slot on
/// close, drop events that don't match the caller's project filter,
/// return false when the subscriber `tx` has disconnected.
async fn forward<T, R>(
    tx: &Tx<DawEvent>,
    res: Result<T, RecvError>,
    filter: &BusFilter,
    domain: &'static str,
    extract_guid: fn(&T) -> &str,
    wrap: impl FnOnce(T) -> DawEvent,
    slot: &mut Option<R>,
) -> bool {
    match res {
        Ok(event) => {
            if filter.project_rejects(extract_guid(&event)) {
                return true; // drop silently, keep loop alive
            }
            tx.send(wrap(event)).await.is_ok()
        }
        Err(RecvError::Closed) => {
            *slot = None;
            true
        }
        Err(RecvError::Lagged(skipped)) => {
            tracing::warn!(skipped, domain, "event_bus subscriber lagged");
            true
        }
    }
}
