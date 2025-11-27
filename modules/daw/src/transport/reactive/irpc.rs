//! IRPC protocol for transport reactive service
//!
//! Exposes all transport reactive streams over IRPC so they can be reactive over the network.

use crate::transport::{Transport, PlayState, Tempo};
use irpc::{
    channel::mpsc,
    rpc::RemoteService,
    rpc_requests, Client, WithChannels,
};
use serde::{Deserialize, Serialize};
use iroh::{protocol::ProtocolHandler, Endpoint, EndpointAddr};
use irpc_iroh::{IrohLazyRemoteConnection, IrohProtocol};
use rxrust::prelude::*;
use tokio::sync::{mpsc as tokio_mpsc, broadcast};

/// Transport changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransportChangedMessage {
    pub project_id: String,
    pub transport: Transport,
}

/// Play state changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlayStateChangedMessage {
    pub project_id: String,
    pub play_state: PlayState,
}

/// Tempo changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TempoChangedMessage {
    pub project_id: String,
    pub tempo: Tempo,
}

/// Position changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PositionChangedMessage {
    pub project_id: String,
    pub position: f64,
}

/// All transport reactive update messages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TransportUpdateMessage {
    TransportChanged(TransportChangedMessage),
    PlayStateChanged(PlayStateChangedMessage),
    TempoChanged(TempoChangedMessage),
    PositionChanged(PositionChangedMessage),
}

/// Request to subscribe to transport updates
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeTransport;

/// IRPC protocol for transport service
#[rpc_requests(message = TransportMessage)]
#[derive(Serialize, Deserialize, Debug)]
pub enum TransportProtocol {
    /// Subscribe to transport updates (server streaming)
    #[rpc(tx = mpsc::Sender<TransportUpdateMessage>)]
    SubscribeTransport(SubscribeTransport),
}

/// Transport API client
pub struct TransportApi {
    inner: Client<TransportProtocol>,
    pub(crate) handler_data: Option<(mpsc::Receiver<TransportMessage>, broadcast::Sender<TransportUpdateMessage>, broadcast::Sender<TransportUpdateMessage>, broadcast::Sender<TransportUpdateMessage>, broadcast::Sender<TransportUpdateMessage>)>,
}

impl TransportApi {
    pub const ALPN: &[u8] = b"irpc-iroh/transport/1";

    /// Create a new transport reactive API (server-side)
    pub fn spawn(service: Box<dyn crate::transport::reactive::TransportReactiveService>) -> Self {
        let (tx, rx) = mpsc::channel(16);
        let streams = service.streams().clone();

        // Create broadcast channels for each stream type
        let (transport_broadcast, _) = broadcast::channel(1000);
        let (play_state_broadcast, _) = broadcast::channel(1000);
        let (tempo_broadcast, _) = broadcast::channel(1000);
        let (position_broadcast, _) = broadcast::channel(1000);

        // Subscribe to reactive streams synchronously (on current thread)
        // These subscriptions will forward to broadcast channels
        {
            let transport_subject = streams.transport_changed.borrow().clone();
            let tx = transport_broadcast.clone();
            transport_subject.subscribe(move |(project_id, transport)| {
                let _ = tx.send(TransportUpdateMessage::TransportChanged(TransportChangedMessage {
                    project_id,
                    transport,
                }));
            });
        }

        {
            let play_state_subject = streams.play_state_changed.borrow().clone();
            let tx = play_state_broadcast.clone();
            play_state_subject.subscribe(move |(project_id, play_state)| {
                let _ = tx.send(TransportUpdateMessage::PlayStateChanged(PlayStateChangedMessage {
                    project_id,
                    play_state,
                }));
            });
        }

        {
            let tempo_subject = streams.tempo_changed.borrow().clone();
            let tx = tempo_broadcast.clone();
            tempo_subject.subscribe(move |(project_id, tempo)| {
                let _ = tx.send(TransportUpdateMessage::TempoChanged(TempoChangedMessage {
                    project_id,
                    tempo,
                }));
            });
        }

        {
            let position_subject = streams.position_changed.borrow().clone();
            let tx = position_broadcast.clone();
            position_subject.subscribe(move |(project_id, position)| {
                let _ = tx.send(TransportUpdateMessage::PositionChanged(PositionChangedMessage {
                    project_id,
                    position,
                }));
            });
        }

        // Store channels for later spawning in tokio runtime
        // The handler will be spawned when start_handler() is called from the IROH server
        let handler_data = (rx, transport_broadcast, play_state_broadcast, tempo_broadcast, position_broadcast);

        TransportApi {
            inner: Client::local(tx),
            handler_data: Some(handler_data),
        }
    }

    /// Extract handler data for spawning in tokio runtime
    pub fn take_handler_data(&mut self) -> Option<(mpsc::Receiver<TransportMessage>, broadcast::Sender<TransportUpdateMessage>, broadcast::Sender<TransportUpdateMessage>, broadcast::Sender<TransportUpdateMessage>, broadcast::Sender<TransportUpdateMessage>)> {
        self.handler_data.take()
    }

    /// Start the message handler (must be called from within a tokio runtime)
    pub fn start_handler(&mut self) {
        if let Some((rx, transport_broadcast, play_state_broadcast, tempo_broadcast, position_broadcast)) = self.handler_data.take() {
            let transport_broadcast_clone = transport_broadcast.clone();
            let play_state_broadcast_clone = play_state_broadcast.clone();
            let tempo_broadcast_clone = tempo_broadcast.clone();
            let position_broadcast_clone = position_broadcast.clone();
            
            tokio::spawn(async move {
                let mut rx = rx;
                while let Ok(Some(msg)) = rx.recv().await {
                    match msg {
                        TransportMessage::SubscribeTransport(WithChannels { tx, .. }) => {
                            // Subscribe to all broadcast channels and merge them
                            let mut transport_rx = transport_broadcast_clone.subscribe();
                            let mut play_state_rx = play_state_broadcast_clone.subscribe();
                            let mut tempo_rx = tempo_broadcast_clone.subscribe();
                            let mut position_rx = position_broadcast_clone.subscribe();

                            // Forward from broadcast channels to IRPC
                            tokio::spawn(async move {
                                loop {
                                    tokio::select! {
                                        Ok(msg) = transport_rx.recv() => {
                                            if tx.send(msg).await.is_err() {
                                                break;
                                            }
                                        }
                                        Ok(msg) = play_state_rx.recv() => {
                                            if tx.send(msg).await.is_err() {
                                                break;
                                            }
                                        }
                                        Ok(msg) = tempo_rx.recv() => {
                                            if tx.send(msg).await.is_err() {
                                                break;
                                            }
                                        }
                                        Ok(msg) = position_rx.recv() => {
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
        }
    }

    /// Connect to a remote transport service
    pub fn connect(
        endpoint: Endpoint,
        addr: impl Into<EndpointAddr>,
    ) -> Result<Self, anyhow::Error> {
        let conn = IrohLazyRemoteConnection::new(endpoint, addr.into(), Self::ALPN.to_vec());
        Ok(TransportApi {
            inner: Client::boxed(conn),
            handler_data: None,
        })
    }

    /// Expose this service as a protocol handler for IROH router
    pub fn expose(&self) -> Result<impl ProtocolHandler, anyhow::Error> {
        use anyhow::Context;
        let local = self
            .inner
            .as_local()
            .context("cannot listen on remote service")?;
        Ok(IrohProtocol::new(TransportProtocol::remote_handler(local)))
    }

    /// Subscribe to transport updates
    pub async fn subscribe(&self) -> irpc::Result<mpsc::Receiver<TransportUpdateMessage>> {
        self.inner.server_streaming(SubscribeTransport, 32).await
    }
}

