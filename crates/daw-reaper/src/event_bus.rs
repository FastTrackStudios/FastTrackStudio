//! Reaper backend impl for [`daw_proto::event_bus::EventBus`].
//!
//! Multiplexes the per-domain hub broadcasters into one `Tx<DawEvent>`
//! based on the caller's `BusFilter`. Each subscription spawns one
//! forwarder task driven by `tokio::select!` — disabled domains never
//! subscribe a broadcast receiver, so they cost nothing.

use daw_proto::event_bus::{BusFilter, DawEvent, EventBus};
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
                        if !forward(&tx, res, "tracks", DawEvent::Track, &mut track_rx).await {
                            return;
                        }
                    }
                    res = async { marker_rx.as_mut().unwrap().recv().await }, if marker_rx.is_some() => {
                        if !forward(&tx, res, "markers", DawEvent::Marker, &mut marker_rx).await {
                            return;
                        }
                    }
                    res = async { region_rx.as_mut().unwrap().recv().await }, if region_rx.is_some() => {
                        if !forward(&tx, res, "regions", DawEvent::Region, &mut region_rx).await {
                            return;
                        }
                    }
                    res = async { tempo_rx.as_mut().unwrap().recv().await }, if tempo_rx.is_some() => {
                        if !forward(&tx, res, "tempo_map", DawEvent::TempoMap, &mut tempo_rx).await {
                            return;
                        }
                    }
                    res = async { state_rx.as_mut().unwrap().recv().await }, if state_rx.is_some() => {
                        if !forward(&tx, res, "transport_state", DawEvent::TransportState, &mut state_rx).await {
                            return;
                        }
                    }
                    res = async { project_rx.as_mut().unwrap().recv().await }, if project_rx.is_some() => {
                        if !forward(&tx, res, "projects", DawEvent::Project, &mut project_rx).await {
                            return;
                        }
                    }
                    res = async { position_rx.as_mut().unwrap().recv().await }, if position_rx.is_some() => {
                        // Position is continuous — drop on lag without
                        // logging, otherwise treat like the others.
                        match res {
                            Ok(tick) => {
                                if tx.send(DawEvent::TransportPosition(tick)).await.is_err() {
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

/// Forwarder helper for occasional channels — log on lag, drop slot on
/// close, return false when the subscriber `tx` has disconnected.
async fn forward<T, R>(
    tx: &Tx<DawEvent>,
    res: Result<T, RecvError>,
    domain: &'static str,
    wrap: impl FnOnce(T) -> DawEvent,
    slot: &mut Option<R>,
) -> bool {
    match res {
        Ok(event) => tx.send(wrap(event)).await.is_ok(),
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
