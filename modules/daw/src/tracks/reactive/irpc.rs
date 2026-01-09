//! IRPC protocol for track reactive service
//!
//! Exposes all track reactive streams over IRPC so they can be reactive over the network.

use crate::tracks::Track;
use async_trait::async_trait;
use iroh::{Endpoint, EndpointAddr, protocol::ProtocolHandler};
use irpc::{
    Client, Request, WithChannels,
    channel::{mpsc, oneshot},
    rpc::RemoteService,
    rpc_requests,
};
use irpc_iroh::{IrohLazyRemoteConnection, IrohProtocol};
use rxrust::prelude::*;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::broadcast;

/// Tracks changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TracksChangedMessage {
    pub project_id: String,
    pub tracks: Vec<Track>,
}

/// Track changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackChangedMessage {
    pub project_id: String,
    pub track_index: usize,
    pub track: Track,
}

/// Track added message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackAddedMessage {
    pub project_id: String,
    pub track_index: usize,
    pub track: Track,
}

/// Track removed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TrackRemovedMessage {
    pub project_id: String,
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

/// Set track mute command
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetTrackMute {
    pub project_id: String,
    pub track_index: usize,
    pub muted: bool,
}

/// Set track solo command
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetTrackSolo {
    pub project_id: String,
    pub track_index: usize,
    pub solo_mode: crate::tracks::api::solo::SoloMode,
}

/// IRPC protocol for track service
#[rpc_requests(message = TrackMessage)]
#[derive(Serialize, Deserialize, Debug)]
pub enum TrackProtocol {
    /// Subscribe to track updates (server streaming)
    #[rpc(tx = mpsc::Sender<TrackUpdateMessage>)]
    SubscribeTracks(SubscribeTracks),
    /// Set track mute (request/response)
    #[rpc(tx = oneshot::Sender<Result<(), String>>)]
    SetTrackMute(SetTrackMute),
    /// Set track solo (request/response)
    #[rpc(tx = oneshot::Sender<Result<(), String>>)]
    SetTrackSolo(SetTrackSolo),
}

/// Trait for backends that can handle track commands
///
/// This allows the track API to execute commands
/// on different backends (REAPER, other DAWs, etc.)
#[async_trait]
pub trait TrackCommandHandler: Send + Sync {
    /// Set track mute
    async fn set_track_mute(
        &self,
        project_id: String,
        track_index: usize,
        muted: bool,
    ) -> Result<(), String>;

    /// Set track solo
    async fn set_track_solo(
        &self,
        project_id: String,
        track_index: usize,
        solo_mode: crate::tracks::api::solo::SoloMode,
    ) -> Result<(), String>;
}

/// Track API client
pub struct TrackApi {
    inner: Client<TrackProtocol>,
    pub(crate) handler_data: Option<(
        mpsc::Receiver<TrackMessage>,
        broadcast::Sender<TrackUpdateMessage>,
        broadcast::Sender<TrackUpdateMessage>,
        broadcast::Sender<TrackUpdateMessage>,
        broadcast::Sender<TrackUpdateMessage>,
    )>,
    pub(crate) command_handler: Option<Arc<dyn TrackCommandHandler>>,
}

impl TrackApi {
    pub const ALPN: &[u8] = b"irpc-iroh/track/1";

    /// Create a new track reactive API (server-side)
    pub fn spawn(service: Box<dyn crate::tracks::reactive::TrackReactiveService>) -> Self {
        Self::spawn_with_handler(service, None)
    }

    /// Create a new track reactive API (server-side) with command handler
    pub fn spawn_with_handler(
        service: Box<dyn crate::tracks::reactive::TrackReactiveService>,
        command_handler: Option<Arc<dyn TrackCommandHandler>>,
    ) -> Self {
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
                use tracing::info;
                info!(
                    project_id = %project_id,
                    track_index,
                    track_name = %track.name,
                    track_color = ?track.color,
                    "🎵 [Tracks Service] Track changed - emitting to API"
                );
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

        // Store channels for later spawning in tokio runtime
        let handler_data = (
            rx,
            tracks_broadcast,
            track_changed_broadcast,
            track_added_broadcast,
            track_removed_broadcast,
        );

        TrackApi {
            inner: Client::local(tx),
            handler_data: Some(handler_data),
            command_handler,
        }
    }

    /// Extract handler data for spawning in tokio runtime
    pub fn take_handler_data(
        &mut self,
    ) -> Option<(
        mpsc::Receiver<TrackMessage>,
        broadcast::Sender<TrackUpdateMessage>,
        broadcast::Sender<TrackUpdateMessage>,
        broadcast::Sender<TrackUpdateMessage>,
        broadcast::Sender<TrackUpdateMessage>,
    )> {
        self.handler_data.take()
    }

    /// Connect to a remote track service
    pub fn connect(
        endpoint: Endpoint,
        addr: impl Into<EndpointAddr>,
    ) -> Result<Self, anyhow::Error> {
        let conn = IrohLazyRemoteConnection::new(endpoint, addr.into(), Self::ALPN.to_vec());
        Ok(TrackApi {
            inner: Client::boxed(conn),
            handler_data: None,
            command_handler: None,
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

    /// Set track mute
    pub async fn set_track_mute(
        &self,
        project_id: String,
        track_index: usize,
        muted: bool,
    ) -> irpc::Result<Result<(), String>> {
        let msg = SetTrackMute {
            project_id,
            track_index,
            muted,
        };
        let rx = match self.inner.request().await? {
            Request::Local(request) => {
                let (tx, rx) = oneshot::channel();
                request.send((msg, tx)).await?;
                rx
            }
            Request::Remote(request) => {
                let (_tx, rx) = request.write(msg).await?;
                rx.into()
            }
        };
        match rx.await {
            Ok(result) => Ok(result),
            Err(e) => Ok(Err(format!("Request failed: {}", e))),
        }
    }

    /// Set track solo
    pub async fn set_track_solo(
        &self,
        project_id: String,
        track_index: usize,
        solo_mode: crate::tracks::api::solo::SoloMode,
    ) -> irpc::Result<Result<(), String>> {
        let msg = SetTrackSolo {
            project_id,
            track_index,
            solo_mode,
        };
        let rx = match self.inner.request().await? {
            Request::Local(request) => {
                let (tx, rx) = oneshot::channel();
                request.send((msg, tx)).await?;
                rx
            }
            Request::Remote(request) => {
                let (_tx, rx) = request.write(msg).await?;
                rx.into()
            }
        };
        match rx.await {
            Ok(result) => Ok(result),
            Err(e) => Ok(Err(format!("Request failed: {}", e))),
        }
    }
}
