//! IRPC protocol for chart reactive service
//!
//! Exposes chart reactive streams over IRPC so they can be reactive over the network.
//! The Chart includes chords, sections, tempo, time signatures, and all other chart data.

use keyflow::Chart;
use crate::chords::reactive::ChordsReactiveService;
use irpc::{
    channel::mpsc,
    rpc::RemoteService,
    rpc_requests, Client ,
};
use serde::{Deserialize, Serialize};
use iroh::{protocol::ProtocolHandler, Endpoint, EndpointAddr};
use irpc_iroh::{IrohLazyRemoteConnection, IrohProtocol};
use tokio::sync::broadcast;

/// Chart changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChartChangedMessage {
    pub project_name: String,
    pub chart: Chart,
}

/// All chart update messages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ChartUpdateMessage {
    ChartChanged(ChartChangedMessage),
}

/// Request to subscribe to chart updates
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeChart;

/// IRPC protocol for chart service
#[rpc_requests(message = ChartMessage)]
#[derive(Serialize, Deserialize, Debug)]
pub enum ChartProtocol {
    /// Subscribe to chart updates (server streaming)
    #[rpc(tx = mpsc::Sender<ChartUpdateMessage>)]
    SubscribeChart(SubscribeChart),
}

/// Chart API client
pub struct ChartApi {
    inner: Client<ChartProtocol>,
    pub(crate) handler_data: Option<(mpsc::Receiver<ChartMessage>, broadcast::Sender<ChartUpdateMessage>)>,
}

impl ChartApi {
    pub const ALPN: &[u8] = b"irpc-iroh/chart/1";

    /// Create a new chart reactive API (server-side)
    /// 
    /// Note: This currently uses ChordsReactiveService, but in the future
    /// we should create a ChartReactiveService that directly streams Chart updates.
    /// For now, we'll need to convert ChordsData to Chart or extend the service.
    pub fn spawn(_service: Box<dyn ChordsReactiveService>) -> Self {
        let (tx, rx) = mpsc::channel(16);

        // Create broadcast channel for chart stream
        let (chart_broadcast, _) = broadcast::channel(1000);

        // Subscribe to reactive streams synchronously (on current thread)
        // This subscription will forward to broadcast channel
        // TODO: When we have ChartReactiveService, subscribe to chart_changed instead
        // For now, we'll need to handle the conversion from ChordsData to Chart
        // or extend the service to also stream Chart updates
        {
            // Placeholder: We'll need to implement chart streaming
            // For now, this is a placeholder that will need to be updated
            // when we have Chart data available from the service
        }

        // Store channels for later spawning in tokio runtime
        let handler_data = (rx, chart_broadcast);

        ChartApi {
            inner: Client::local(tx),
            handler_data: Some(handler_data),
        }
    }

    /// Extract handler data for spawning in tokio runtime
    pub fn take_handler_data(&mut self) -> Option<(mpsc::Receiver<ChartMessage>, broadcast::Sender<ChartUpdateMessage>)> {
        self.handler_data.take()
    }

    /// Connect to a remote chart service
    pub fn connect(
        endpoint: Endpoint,
        addr: impl Into<EndpointAddr>,
    ) -> Result<Self, anyhow::Error> {
        let conn = IrohLazyRemoteConnection::new(endpoint, addr.into(), Self::ALPN.to_vec());
        Ok(ChartApi {
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
        Ok(IrohProtocol::new(ChartProtocol::remote_handler(local)))
    }

    /// Subscribe to chart updates
    pub async fn subscribe(&self) -> irpc::Result<mpsc::Receiver<ChartUpdateMessage>> {
        self.inner.server_streaming(SubscribeChart, 32).await
    }
}
