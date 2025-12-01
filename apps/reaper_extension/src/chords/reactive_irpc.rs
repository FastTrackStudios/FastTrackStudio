//! IRPC protocol for chords reactive service
//!
//! Exposes chords reactive streams over IRPC so they can be reactive over the network.

use crate::chords::types::ChordsData;
use irpc::{
    channel::mpsc,
    rpc::RemoteService,
    rpc_requests, Client, WithChannels,
};
use serde::{Deserialize, Serialize};
use iroh::{protocol::ProtocolHandler, Endpoint, EndpointAddr};
use irpc_iroh::{IrohLazyRemoteConnection, IrohProtocol};
use rxrust::prelude::*;
use tokio::sync::broadcast;

/// Chords changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChordsChangedMessage {
    pub project_name: String,
    pub chords: ChordsData,
}

/// All chords update messages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ChordsUpdateMessage {
    ChordsChanged(ChordsChangedMessage),
}

/// Request to subscribe to chords updates
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeChords;

/// IRPC protocol for chords service
#[rpc_requests(message = ChordsMessage)]
#[derive(Serialize, Deserialize, Debug)]
pub enum ChordsProtocol {
    /// Subscribe to chords updates (server streaming)
    #[rpc(tx = mpsc::Sender<ChordsUpdateMessage>)]
    SubscribeChords(SubscribeChords),
}

/// Chords API client
pub struct ChordsApi {
    inner: Client<ChordsProtocol>,
    pub(crate) handler_data: Option<(mpsc::Receiver<ChordsMessage>, broadcast::Sender<ChordsUpdateMessage>)>,
}

impl ChordsApi {
    pub const ALPN: &[u8] = b"irpc-iroh/chords/1";

    /// Create a new chords reactive API (server-side)
    pub fn spawn(service: Box<dyn crate::chords::reactive::ChordsReactiveService>) -> Self {
        let (tx, rx) = mpsc::channel(16);
        let streams = service.streams().clone();

        // Create broadcast channel for chords stream
        let (chords_broadcast, _) = broadcast::channel(1000);

        // Subscribe to reactive streams synchronously (on current thread)
        // This subscription will forward to broadcast channel
        {
            let chords_subject = streams.chords_changed.borrow().clone();
            let tx = chords_broadcast.clone();
            chords_subject.subscribe(move |(project_name, chords)| {
                let _ = tx.send(ChordsUpdateMessage::ChordsChanged(ChordsChangedMessage {
                    project_name,
                    chords,
                }));
            });
        }

        // Store channels for later spawning in tokio runtime
        let handler_data = (rx, chords_broadcast);

        ChordsApi {
            inner: Client::local(tx),
            handler_data: Some(handler_data),
        }
    }

    /// Extract handler data for spawning in tokio runtime
    pub fn take_handler_data(&mut self) -> Option<(mpsc::Receiver<ChordsMessage>, broadcast::Sender<ChordsUpdateMessage>)> {
        self.handler_data.take()
    }

    /// Connect to a remote chords service
    pub fn connect(
        endpoint: Endpoint,
        addr: impl Into<EndpointAddr>,
    ) -> Result<Self, anyhow::Error> {
        let conn = IrohLazyRemoteConnection::new(endpoint, addr.into(), Self::ALPN.to_vec());
        Ok(ChordsApi {
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
        Ok(IrohProtocol::new(ChordsProtocol::remote_handler(local)))
    }

    /// Subscribe to chords updates
    pub async fn subscribe(&self) -> irpc::Result<mpsc::Receiver<ChordsUpdateMessage>> {
        self.inner.server_streaming(SubscribeChords, 32).await
    }
}

