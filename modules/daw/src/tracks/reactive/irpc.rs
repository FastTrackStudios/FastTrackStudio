//! IRPC protocol for track reactive service
//!
//! Exposes all track reactive streams over IRPC so they can be reactive over the network.

use crate::tracks::Track;
use irpc::{
    channel::mpsc,
    rpc::RemoteService,
    rpc_requests, Client, WithChannels,
};
use serde::{Deserialize, Serialize};
use uuid::Uuid;
use iroh::{protocol::ProtocolHandler, Endpoint, EndpointAddr};
use irpc_iroh::{IrohLazyRemoteConnection, IrohProtocol};
use rxrust::prelude::*;
use tokio::sync::broadcast;

/// Tracks changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TracksChangedMessage {
    pub project_id: Uuid,
    pub tracks: Vec<Track>,
}

/// Track changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackChangedMessage {
    pub project_id: Uuid,
    pub track_index: usize,
    pub track: Track,
}

/// Track added message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackAddedMessage {
    pub project_id: Uuid,
    pub track_index: usize,
    pub track: Track,
}

/// Track removed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackRemovedMessage {
    pub project_id: Uuid,
    pub track_index: usize,
}

/// All track update messages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TrackUpdateMessage {
    TracksChanged(TracksChangedMessage),
    TrackChanged(TrackChangedMessage),
    TrackAdded(TrackAddedMessage),
    TrackRemoved(TrackRemovedMessage),
}

/// Request to subscribe to track updates
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeTracks;

/// IRPC protocol for track service
#[rpc_requests(message = TrackMessage)]
#[derive(Serialize, Deserialize, Debug)]
pub enum TrackProtocol {
    /// Subscribe to track updates (server streaming)
    #[rpc(tx = mpsc::Sender<TrackUpdateMessage>)]
    SubscribeTracks(SubscribeTracks),
}

/// Track API client
pub struct TrackApi {
    inner: Client<TrackProtocol>,
}

impl TrackApi {
    pub const ALPN: &[u8] = b"irpc-iroh/track/1";

    /// Create a new track reactive API (server-side)
    pub fn spawn(service: Box<dyn crate::tracks::reactive::TrackReactiveService>) -> Self {
        let (tx, rx) = mpsc::channel(16);
        let streams = service.streams().clone();

        // Create broadcast channels for each stream type
        let (tracks_broadcast, _) = broadcast::channel(1000);
        let (track_changed_broadcast, _) = broadcast::channel(1000);
        let (track_added_broadcast, _) = broadcast::channel(1000);
        let (track_removed_broadcast, _) = broadcast::channel(1000);

        // Subscribe to reactive streams synchronously (on current thread)
        // These subscriptions will forward to broadcast channels
        {
            let tracks_subject = streams.tracks_changed.borrow().clone();
            let tx = tracks_broadcast.clone();
            tracks_subject.subscribe(move |(project_id, tracks)| {
                let _ = tx.send(TrackUpdateMessage::TracksChanged(TracksChangedMessage {
                    project_id,
                    tracks,
                }));
            });
        }

        {
            let track_changed_subject = streams.track_changed.borrow().clone();
            let tx = track_changed_broadcast.clone();
            track_changed_subject.subscribe(move |(project_id, track_index, track)| {
                let _ = tx.send(TrackUpdateMessage::TrackChanged(TrackChangedMessage {
                    project_id,
                    track_index,
                    track,
                }));
            });
        }

        {
            let track_added_subject = streams.track_added.borrow().clone();
            let tx = track_added_broadcast.clone();
            track_added_subject.subscribe(move |(project_id, track_index, track)| {
                let _ = tx.send(TrackUpdateMessage::TrackAdded(TrackAddedMessage {
                    project_id,
                    track_index,
                    track,
                }));
            });
        }

        {
            let track_removed_subject = streams.track_removed.borrow().clone();
            let tx = track_removed_broadcast.clone();
            track_removed_subject.subscribe(move |(project_id, track_index)| {
                let _ = tx.send(TrackUpdateMessage::TrackRemoved(TrackRemovedMessage {
                    project_id,
                    track_index,
                }));
            });
        }

        // Spawn async task to handle IRPC messages (only moves Send types)
        tokio::task::spawn(async move {
            let mut rx = rx;
            while let Ok(Some(msg)) = rx.recv().await {
                match msg {
                    TrackMessage::SubscribeTracks(WithChannels { tx, .. }) => {
                        // Subscribe to all broadcast channels and merge them
                        let mut tracks_rx = tracks_broadcast.subscribe();
                        let mut track_changed_rx = track_changed_broadcast.subscribe();
                        let mut track_added_rx = track_added_broadcast.subscribe();
                        let mut track_removed_rx = track_removed_broadcast.subscribe();

                        // Forward from broadcast channels to IRPC
                        tokio::spawn(async move {
                            loop {
                                tokio::select! {
                                    Ok(msg) = tracks_rx.recv() => {
                                        if tx.send(msg).await.is_err() {
                                            break;
                                        }
                                    }
                                    Ok(msg) = track_changed_rx.recv() => {
                                        if tx.send(msg).await.is_err() {
                                            break;
                                        }
                                    }
                                    Ok(msg) = track_added_rx.recv() => {
                                        if tx.send(msg).await.is_err() {
                                            break;
                                        }
                                    }
                                    Ok(msg) = track_removed_rx.recv() => {
                                        if tx.send(msg).await.is_err() {
                                            break;
                                        }
                                    }
                                    else => break,
                                }
                            }
                        });
                    }
                }
            }
        });

        TrackApi {
            inner: Client::local(tx),
        }
    }

    /// Connect to a remote track service
    pub fn connect(
        endpoint: Endpoint,
        addr: impl Into<EndpointAddr>,
    ) -> Result<Self, anyhow::Error> {
        let conn = IrohLazyRemoteConnection::new(endpoint, addr.into(), Self::ALPN.to_vec());
        Ok(TrackApi {
            inner: Client::boxed(conn),
        })
    }

    /// Expose this service as a protocol handler for IROH router
    pub fn expose(&self) -> Result<impl ProtocolHandler, anyhow::Error> {
        use anyhow::Context;
        let local = self
            .inner
            .as_local()
            .context("cannot listen on remote service")?;
        Ok(IrohProtocol::new(TrackProtocol::remote_handler(local)))
    }

    /// Subscribe to track updates
    pub async fn subscribe(&self) -> irpc::Result<mpsc::Receiver<TrackUpdateMessage>> {
        self.inner.server_streaming(SubscribeTracks, 32).await
    }
}

