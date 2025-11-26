//! Transport Stream IRPC Protocol
//!
//! Defines the irpc protocol for streaming transport state updates.
//! This protocol is backend-agnostic and can be implemented by any DAW backend.

use crate::transport::core::transport::Transport;
use irpc::{
    channel::mpsc,
    rpc::RemoteService,
    rpc_requests, Client, WithChannels,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::time;
use tracing::{info, warn};

/// Transport update message sent over the irpc stream
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransportUpdateMessage {
    pub transport: Transport,
    /// The name of the active project (used to determine which song is active)
    pub project_name: Option<String>,
}

impl From<TransportUpdateMessage> for crate::transport::infra::stream::TransportUpdate {
    fn from(msg: TransportUpdateMessage) -> Self {
        crate::transport::infra::stream::TransportUpdate {
            transport: msg.transport,
        }
    }
}

impl From<crate::transport::infra::stream::TransportUpdate> for TransportUpdateMessage {
    fn from(update: crate::transport::infra::stream::TransportUpdate) -> Self {
        TransportUpdateMessage {
            transport: update.transport,
            project_name: None, // Will be set by the backend implementation
        }
    }
}

/// Request to subscribe to transport updates
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeTransport;

/// IRPC protocol for transport stream service
#[rpc_requests(message = TransportStreamMessage)]
#[derive(Serialize, Deserialize, Debug)]
pub enum TransportStreamProtocol {
    /// Subscribe to transport state updates (server streaming)
    #[rpc(tx=mpsc::Sender<TransportUpdateMessage>)]
    SubscribeTransport(SubscribeTransport),
}

/// Trait for backends that can provide transport state
/// 
/// This allows the transport stream actor to get transport state
/// from different sources (REAPER, other DAWs, etc.)
#[async_trait::async_trait]
pub trait TransportStateProvider: Send + Sync {
    /// Get the current transport state
    async fn get_transport(&self) -> Result<Transport, String>;
    
    /// Get the current project name (optional, for determining active song)
    async fn get_project_name(&self) -> Result<Option<String>, String> {
        Ok(None) // Default implementation returns None
    }
}

/// Transport stream actor that handles client subscriptions and broadcasts updates
struct TransportStreamActor {
    recv: tokio::sync::mpsc::Receiver<TransportStreamMessage>,
    /// Broadcast channel for transport updates
    /// The polling loop sends updates here, and subscriber tasks forward them to their clients
    broadcast_tx: tokio::sync::broadcast::Sender<TransportUpdateMessage>,
    /// Provider for getting transport state
    state_provider: Arc<dyn TransportStateProvider>,
}

impl TransportStreamActor {
    pub fn spawn(state_provider: Arc<dyn TransportStateProvider>) -> TransportStreamApi {
        let (tx, rx) = tokio::sync::mpsc::channel(16);
        // Create a broadcast channel with a large buffer (1000 messages) to handle bursts
        let (broadcast_tx, _) = tokio::sync::broadcast::channel(1000);

        let actor = Self {
            recv: rx,
            broadcast_tx: broadcast_tx.clone(),
            state_provider: state_provider.clone(),
        };

        // Spawn the actor to handle messages
        tokio::task::spawn(actor.run());

        // Spawn the polling task that reads transport state and broadcasts to all subscribers
        let state_provider_for_polling = state_provider.clone();
        tokio::task::spawn(async move {
            Self::poll_and_broadcast(broadcast_tx, state_provider_for_polling).await;
        });

        TransportStreamApi {
            inner: Client::local(tx),
        }
    }

    async fn run(mut self) {
        while let Some(msg) = self.recv.recv().await {
            self.handle(msg).await;
        }
    }

    async fn handle(&mut self, msg: TransportStreamMessage) {
        match msg {
            TransportStreamMessage::SubscribeTransport(sub) => {
                info!("[Transport Stream] Client subscribed to transport updates");
                let WithChannels { tx, .. } = sub;
                
                // CRITICAL: In irpc server streaming, the tx from WithChannels must stay alive
                // for the connection to remain open. We spawn a forwarding task that:
                // 1. Keeps the original tx alive
                // 2. Receives from the broadcast channel
                // 3. Forwards messages to the client's tx
                
                // Create a receiver for this subscriber from the broadcast channel
                let mut broadcast_rx = self.broadcast_tx.subscribe();
                
                // Get current transport state for initial update
                let state_provider_for_initial = self.state_provider.clone();
                let tx_for_initial = tx.clone();
                
                // Spawn a forwarding task for this subscriber
                // This task keeps the original tx alive and forwards messages
                tokio::task::spawn(async move {
                    info!("[Transport Stream] 🚀 Started forwarding task for subscriber");
                    
                    // Send an initial update immediately with current state
                    match state_provider_for_initial.get_transport().await {
                        Ok(transport) => {
                            let project_name = state_provider_for_initial.get_project_name().await.ok().flatten();
                            let initial_update = TransportUpdateMessage { 
                                transport,
                                project_name,
                            };
                            match tx_for_initial.send(initial_update).await {
                                Ok(_) => {
                                    info!("[Transport Stream] ✅ Sent initial transport update to new subscriber");
                                }
                                Err(e) => {
                                    warn!("[Transport Stream] ❌ Failed to send initial transport update: {}", e);
                                    return; // Client disconnected
                                }
                            }
                        }
                        Err(e) => {
                            warn!("[Transport Stream] Failed to get initial transport state: {}", e);
                            // Continue anyway - we'll get updates from the broadcast channel
                        }
                    }
                    
                    // Forward all subsequent updates from the broadcast channel
                    loop {
                        match broadcast_rx.recv().await {
                            Ok(update) => {
                                // Forward to client - this will block if the client isn't consuming
                                // but that's okay, we want backpressure
                                if tx.send(update).await.is_err() {
                                    info!("[Transport Stream] Client disconnected (tx.send failed), stopping forwarder");
                                    break;
                                }
                            }
                            Err(tokio::sync::broadcast::error::RecvError::Closed) => {
                                info!("[Transport Stream] Broadcast channel closed, stopping forwarder");
                                break;
                            }
                            Err(tokio::sync::broadcast::error::RecvError::Lagged(skipped)) => {
                                warn!("[Transport Stream] Subscriber lagged, skipped {} messages", skipped);
                                // Continue - we'll send the next available message
                            }
                        }
                    }
                    
                    // Keep the original tx alive until the task ends
                    // When tx is dropped, the connection closes
                    info!("[Transport Stream] Forwarding task ended, connection will close");
                });
                
                info!("[Transport Stream] ✅ Subscriber forwarding task spawned");
            }
        }
    }

    /// Poll transport state and broadcast to all subscribers via broadcast channel
    async fn poll_and_broadcast(
        broadcast_tx: tokio::sync::broadcast::Sender<TransportUpdateMessage>,
        state_provider: Arc<dyn TransportStateProvider>,
    ) {
        let mut interval = time::interval(time::Duration::from_millis(33)); // ~30Hz

        loop {
            interval.tick().await;
            
            // Get current transport state and project name from the provider
            if let Ok(transport) = state_provider.get_transport().await {
                let project_name = state_provider.get_project_name().await.ok().flatten();
                let update = TransportUpdateMessage { 
                    transport,
                    project_name,
                };
                // Broadcast to all subscribers (non-blocking)
                let _ = broadcast_tx.send(update);
            }
        }
    }
}

/// Transport stream API for clients
#[derive(Debug)]
pub struct TransportStreamApi {
    inner: Client<TransportStreamProtocol>,
}

impl TransportStreamApi {
    pub const ALPN: &[u8] = b"irpc-iroh/transport-stream/1";

    /// Create a new transport stream API with a state provider
    /// 
    /// This is called by the server (e.g., REAPER extension) to create
    /// a transport stream service. The state provider implementation
    /// (e.g., ReaperTransportStateProvider) is specific to the backend.
    pub fn spawn(state_provider: Arc<dyn TransportStateProvider>) -> Self {
        TransportStreamActor::spawn(state_provider)
    }

    /// Connect to a remote transport stream service using endpoint ID
    /// 
    /// This is called by clients (e.g., playground app) to connect to
    /// a transport stream service. Clients don't need to know about
    /// the backend implementation - they just connect via irpc.
    pub fn connect(
        endpoint: iroh::Endpoint,
        addr: impl Into<iroh::EndpointAddr>,
    ) -> Result<Self, anyhow::Error> {
        use irpc_iroh::IrohLazyRemoteConnection;
        let conn = IrohLazyRemoteConnection::new(endpoint, addr.into(), Self::ALPN.to_vec());
        Ok(TransportStreamApi {
            inner: Client::boxed(conn),
        })
    }

    /// Expose this service as a protocol handler for IROH router
    pub fn expose(&self) -> Result<impl iroh::protocol::ProtocolHandler, anyhow::Error> {
        use anyhow::Context;
        use irpc_iroh::IrohProtocol;
        let local = self
            .inner
            .as_local()
            .context("cannot listen on remote service")?;
        Ok(IrohProtocol::new(TransportStreamProtocol::remote_handler(local)))
    }

    /// Subscribe to transport state updates
    /// Returns a receiver that will receive TransportUpdateMessage messages
    pub async fn subscribe(&self) -> irpc::Result<mpsc::Receiver<TransportUpdateMessage>> {
        self.inner.server_streaming(SubscribeTransport, 32).await
    }
}
