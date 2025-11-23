//! Setlist Stream IRPC Protocol
//!
//! Defines the irpc protocol for streaming setlist state updates.
//! This protocol is backend-agnostic and can be implemented by any DAW backend.

use crate::core::{Setlist, SetlistApi};
use anyhow::Result;
use async_trait::async_trait;
use iroh::{Endpoint, EndpointAddr};
use irpc::{
    channel::mpsc,
    rpc::RemoteService,
    rpc_requests, Client, WithChannels,
};
use irpc_iroh::IrohLazyRemoteConnection;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::time;
use tracing::{info, warn};

/// Setlist update message sent over the irpc stream
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetlistUpdateMessage {
    pub setlist_api: SetlistApi,
}

/// Request to subscribe to setlist updates
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSetlist;

/// IRPC protocol for setlist stream service
#[rpc_requests(message = SetlistStreamMessage)]
#[derive(Serialize, Deserialize, Debug)]
pub enum SetlistStreamProtocol {
    /// Subscribe to setlist state updates (server streaming)
    #[rpc(tx=mpsc::Sender<SetlistUpdateMessage>)]
    SubscribeSetlist(SubscribeSetlist),
}

/// Trait for backends that can provide setlist state
/// 
/// This allows the setlist stream actor to get setlist state
/// from different sources (REAPER, other DAWs, etc.)
#[async_trait]
pub trait SetlistStateProvider: Send + Sync {
    /// Get the current setlist API state (includes computed fields like active song)
    async fn get_setlist_api(&self) -> Result<SetlistApi, String>;
    
    /// Get the current setlist API state with transport info for all projects
    /// Note: Transport info is now part of each song in the setlist, so this just returns the setlist API
    async fn get_setlist_api_with_transport(&self) -> Result<SetlistApi, String> {
        // Transport info is now embedded in each song's transport_info field
        self.get_setlist_api().await
    }
}

/// Setlist stream actor that handles client subscriptions and broadcasts updates
struct SetlistStreamActor {
    recv: tokio::sync::mpsc::Receiver<SetlistStreamMessage>,
    /// Broadcast channel for setlist updates
    /// The polling loop sends updates here, and subscriber tasks forward them to their clients
    broadcast_tx: tokio::sync::broadcast::Sender<SetlistUpdateMessage>,
    /// Provider for getting setlist state
    state_provider: Arc<dyn SetlistStateProvider>,
}

impl SetlistStreamActor {
    pub fn spawn(state_provider: Arc<dyn SetlistStateProvider>) -> SetlistStreamApi {
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

        // Spawn the polling task that reads setlist state and broadcasts to all subscribers
        let state_provider_for_polling = state_provider.clone();
        tokio::task::spawn(async move {
            Self::poll_and_broadcast(broadcast_tx, state_provider_for_polling).await;
        });

        SetlistStreamApi {
            inner: Client::local(tx),
        }
    }

    async fn run(mut self) {
        while let Some(msg) = self.recv.recv().await {
            self.handle(msg).await;
        }
    }

    async fn handle(&mut self, msg: SetlistStreamMessage) {
        match msg {
            SetlistStreamMessage::SubscribeSetlist(sub) => {
                let WithChannels { tx, .. } = sub;
                
                // Create a receiver for this subscriber from the broadcast channel
                let mut broadcast_rx = self.broadcast_tx.subscribe();
                
                // Get current setlist state for initial update
                let state_provider_for_initial = self.state_provider.clone();
                let tx_for_initial = tx.clone();
                
                // Spawn a forwarding task for this subscriber
                tokio::task::spawn(async move {
                    // Send an initial update immediately with current state
                    // Transport info is now embedded in each song's transport_info field
                    if let Ok(setlist_api) = state_provider_for_initial.get_setlist_api().await {
                        let initial_update = SetlistUpdateMessage { 
                            setlist_api,
                        };
                        if tx_for_initial.send(initial_update).await.is_err() {
                            return; // Client disconnected
                        }
                    }
                    
                    // Forward all subsequent updates from the broadcast channel
                    loop {
                        match broadcast_rx.recv().await {
                            Ok(update) => {
                                if tx.send(update).await.is_err() {
                                    break;
                                }
                            }
                            Err(tokio::sync::broadcast::error::RecvError::Closed) => {
                                break;
                            }
                            Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => {
                                // Continue - we'll send the next available message
                            }
                        }
                    }
                });
            }
        }
    }

    /// Poll setlist state and broadcast to all subscribers via broadcast channel
    async fn poll_and_broadcast(
        broadcast_tx: tokio::sync::broadcast::Sender<SetlistUpdateMessage>,
        state_provider: Arc<dyn SetlistStateProvider>,
    ) {
        use std::sync::atomic::{AtomicU64, Ordering};
        use std::time::Instant;
        use std::sync::OnceLock;
        
        static SEND_COUNT: AtomicU64 = AtomicU64::new(0);
        static LAST_LOG: OnceLock<std::sync::Mutex<Instant>> = OnceLock::new();
        
        // Poll as fast as possible - update whenever state changes
        // No rate limiting - update on every state change for maximum responsiveness
        let mut interval = time::interval(time::Duration::from_millis(8)); // ~120Hz polling for immediate updates

        loop {
            interval.tick().await;
            
            // Get current setlist API state from the provider
            // Transport info is now embedded in each song's transport_info field
            // Use try_get_setlist_api if available to avoid blocking on stale data
            if let Ok(setlist_api) = state_provider.get_setlist_api().await {
                let update = SetlistUpdateMessage { 
                    setlist_api,
                };
                // Broadcast to all subscribers (non-blocking)
                // If channel is full, skip this update to avoid blocking
                if broadcast_tx.send(update).is_ok() {
                    SEND_COUNT.fetch_add(1, Ordering::Relaxed);
                }
                
                // Log performance metrics every 10 seconds
                let last_log = LAST_LOG.get_or_init(|| std::sync::Mutex::new(Instant::now()));
                if let Ok(mut last_log_guard) = last_log.lock() {
                    if last_log_guard.elapsed().as_secs() >= 10 {
                        let send_rate = SEND_COUNT.swap(0, Ordering::Relaxed) / 10;
                        tracing::info!("[Performance] IRPC send: {} msg/s", send_rate);
                        *last_log_guard = Instant::now();
                    }
                }
            }
        }
    }
}

/// Setlist stream API for clients
#[derive(Debug)]
pub struct SetlistStreamApi {
    inner: Client<SetlistStreamProtocol>,
}

impl SetlistStreamApi {
    // Use REAPER_ALPN for all connections - single ALPN for the entire app
    // This matches the ALPN used by the REAPER extension router
    pub const ALPN: &[u8] = b"fasttrackstudio/reaper/1";

    /// Create a new setlist stream API with a state provider
    /// 
    /// This is called by the server (e.g., REAPER extension) to create
    /// a setlist stream service. The state provider implementation
    /// (e.g., ReaperSetlistStateProvider) is specific to the backend.
    pub fn spawn(state_provider: Arc<dyn SetlistStateProvider>) -> Self {
        SetlistStreamActor::spawn(state_provider)
    }

    /// Connect to a remote setlist stream service using endpoint ID
    /// 
    /// This is called by clients (e.g., playground app) to connect to
    /// a setlist stream service. Clients don't need to know about
    /// the backend implementation - they just connect via irpc.
    pub fn connect(
        endpoint: Endpoint,
        addr: impl Into<EndpointAddr>,
    ) -> Result<Self, anyhow::Error> {
        let conn = IrohLazyRemoteConnection::new(endpoint, addr.into(), Self::ALPN.to_vec());
        Ok(SetlistStreamApi {
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
        Ok(IrohProtocol::new(SetlistStreamProtocol::remote_handler(local)))
    }

    /// Subscribe to setlist state updates
    /// Returns a receiver that will receive SetlistUpdateMessage messages
    pub async fn subscribe(&self) -> irpc::Result<mpsc::Receiver<SetlistUpdateMessage>> {
        self.inner.server_streaming(SubscribeSetlist, 32).await
    }
}

