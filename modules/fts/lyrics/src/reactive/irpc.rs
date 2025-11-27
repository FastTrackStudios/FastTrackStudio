//! IRPC protocol for lyrics reactive service
//!
//! Exposes all lyrics reactive streams over IRPC so they can be reactive over the network.

use crate::core::Lyrics;
use crate::source::LyricsAnnotations;
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

/// Lyrics changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LyricsChangedMessage {
    pub song_name: String,
    pub lyrics: Lyrics,
}

/// Active slide index changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActiveSlideIndexChangedMessage {
    pub song_name: String,
    pub slide_index: Option<usize>,
}

/// Lyrics annotations changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LyricsAnnotationsChangedMessage {
    pub song_name: String,
    pub annotations: LyricsAnnotations,
}

/// All lyrics update messages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LyricsUpdateMessage {
    LyricsChanged(LyricsChangedMessage),
    ActiveSlideIndexChanged(ActiveSlideIndexChangedMessage),
    LyricsAnnotationsChanged(LyricsAnnotationsChangedMessage),
}

/// Request to subscribe to lyrics updates
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeLyrics;

/// IRPC protocol for lyrics service
#[rpc_requests(message = LyricsMessage)]
#[derive(Serialize, Deserialize, Debug)]
pub enum LyricsProtocol {
    /// Subscribe to lyrics updates (server streaming)
    #[rpc(tx = mpsc::Sender<LyricsUpdateMessage>)]
    SubscribeLyrics(SubscribeLyrics),
}

/// Lyrics API client
pub struct LyricsApi {
    inner: Client<LyricsProtocol>,
    pub(crate) handler_data: Option<(mpsc::Receiver<LyricsMessage>, broadcast::Sender<LyricsUpdateMessage>, broadcast::Sender<LyricsUpdateMessage>, broadcast::Sender<LyricsUpdateMessage>)>,
}

impl LyricsApi {
    pub const ALPN: &[u8] = b"irpc-iroh/lyrics/1";

    /// Create a new lyrics reactive API (server-side)
    pub fn spawn(service: Box<dyn crate::reactive::LyricsReactiveService>) -> Self {
        let (tx, rx) = mpsc::channel(16);
        let streams = service.streams().clone();

        // Create broadcast channels for each stream type
        let (lyrics_broadcast, _) = broadcast::channel(1000);
        let (active_slide_broadcast, _) = broadcast::channel(1000);
        let (annotations_broadcast, _) = broadcast::channel(1000);

        // Subscribe to reactive streams synchronously (on current thread)
        // These subscriptions will forward to broadcast channels
        {
            let lyrics_subject = streams.lyrics_changed.borrow().clone();
            let tx = lyrics_broadcast.clone();
            lyrics_subject.subscribe(move |(song_name, lyrics)| {
                let _ = tx.send(LyricsUpdateMessage::LyricsChanged(LyricsChangedMessage {
                    song_name,
                    lyrics,
                }));
            });
        }

        {
            let active_slide_subject = streams.active_slide_index_changed.borrow().clone();
            let tx = active_slide_broadcast.clone();
            active_slide_subject.subscribe(move |(song_name, slide_index)| {
                let _ = tx.send(LyricsUpdateMessage::ActiveSlideIndexChanged(ActiveSlideIndexChangedMessage {
                    song_name,
                    slide_index,
                }));
            });
        }

        {
            let annotations_subject = streams.lyrics_annotations_changed.borrow().clone();
            let tx = annotations_broadcast.clone();
            annotations_subject.subscribe(move |(song_name, annotations)| {
                let _ = tx.send(LyricsUpdateMessage::LyricsAnnotationsChanged(LyricsAnnotationsChangedMessage {
                    song_name,
                    annotations,
                }));
            });
        }

        // Store channels for later spawning in tokio runtime
        let handler_data = (rx, lyrics_broadcast, active_slide_broadcast, annotations_broadcast);

        LyricsApi {
            inner: Client::local(tx),
            handler_data: Some(handler_data),
        }
    }

    /// Extract handler data for spawning in tokio runtime
    pub fn take_handler_data(&mut self) -> Option<(mpsc::Receiver<LyricsMessage>, broadcast::Sender<LyricsUpdateMessage>, broadcast::Sender<LyricsUpdateMessage>, broadcast::Sender<LyricsUpdateMessage>)> {
        self.handler_data.take()
    }

    /// Connect to a remote lyrics service
    pub fn connect(
        endpoint: Endpoint,
        addr: impl Into<EndpointAddr>,
    ) -> Result<Self, anyhow::Error> {
        let conn = IrohLazyRemoteConnection::new(endpoint, addr.into(), Self::ALPN.to_vec());
        Ok(LyricsApi {
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
        Ok(IrohProtocol::new(LyricsProtocol::remote_handler(local)))
    }

    /// Subscribe to lyrics updates
    pub async fn subscribe(&self) -> irpc::Result<mpsc::Receiver<LyricsUpdateMessage>> {
        self.inner.server_streaming(SubscribeLyrics, 32).await
    }
}

