//! IRPC protocol for setlist reactive service
//!
//! Exposes all setlist reactive streams over IRPC so they can be reactive over the network.

use crate::core::{Setlist, Song, Section};
use lyrics::{Lyrics, LyricsAnnotations};
use irpc::{
    channel::mpsc,
    rpc::RemoteService,
    rpc_requests, Client, WithChannels,
};
use serde::{Deserialize, Serialize};

#[cfg(not(target_arch = "wasm32"))]
use iroh::{protocol::ProtocolHandler, Endpoint, EndpointAddr};
#[cfg(not(target_arch = "wasm32"))]
use irpc_iroh::{IrohLazyRemoteConnection, IrohProtocol};

/// Setlist structure changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetlistStructureChangedMessage {
    pub setlist: Setlist,
}

/// Song added message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SongAddedMessage {
    pub song_index: usize,
    pub song: Song,
}

/// Song removed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SongRemovedMessage {
    pub song_index: usize,
}

/// Songs reordered message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SongsReorderedMessage {
    pub setlist: Setlist,
}

/// Song changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SongChangedMessage {
    pub song_index: usize,
    pub song: Song,
}

/// Section added message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SectionAddedMessage {
    pub song_index: usize,
    pub section_index: usize,
    pub section: Section,
}

/// Section removed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SectionRemovedMessage {
    pub song_index: usize,
    pub section_index: usize,
}

/// Section changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SectionChangedMessage {
    pub song_index: usize,
    pub section_index: usize,
    pub section: Section,
}

/// Lyrics changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LyricsChangedMessage {
    pub song_index: usize,
    pub lyrics: Lyrics,
}

/// Lyrics annotations changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LyricsAnnotationsChangedMessage {
    pub song_index: usize,
    pub annotations: LyricsAnnotations,
}

/// Active indices changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActiveIndicesChangedMessage {
    pub song_index: Option<usize>,
    pub section_index: Option<usize>,
    pub slide_index: Option<usize>,
}

/// Active song index changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActiveSongIndexChangedMessage {
    pub song_index: Option<usize>,
}

/// Active section index changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActiveSectionIndexChangedMessage {
    pub section_index: Option<usize>,
}

/// Active slide index changed message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActiveSlideIndexChangedMessage {
    pub slide_index: Option<usize>,
}

/// Subscribe requests
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSetlistStructure;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSongAdded;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSongRemoved;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSongsReordered;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSongChanged;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSectionAdded;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSectionRemoved;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSectionChanged;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeLyrics;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeLyricsAnnotations;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeActiveIndices;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeActiveSongIndex;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeActiveSectionIndex;
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeActiveSlideIndex;

/// IRPC protocol for setlist reactive service
#[rpc_requests(message = SetlistReactiveMessage)]
#[derive(Serialize, Deserialize, Debug)]
pub enum SetlistReactiveProtocol {
    /// Subscribe to setlist structure changes (server streaming)
    #[rpc(tx=mpsc::Sender<SetlistStructureChangedMessage>)]
    SubscribeSetlistStructure(SubscribeSetlistStructure),
    
    /// Subscribe to song added (server streaming)
    #[rpc(tx=mpsc::Sender<SongAddedMessage>)]
    SubscribeSongAdded(SubscribeSongAdded),
    
    /// Subscribe to song removed (server streaming)
    #[rpc(tx=mpsc::Sender<SongRemovedMessage>)]
    SubscribeSongRemoved(SubscribeSongRemoved),
    
    /// Subscribe to songs reordered (server streaming)
    #[rpc(tx=mpsc::Sender<SongsReorderedMessage>)]
    SubscribeSongsReordered(SubscribeSongsReordered),
    
    /// Subscribe to song changed (server streaming)
    #[rpc(tx=mpsc::Sender<SongChangedMessage>)]
    SubscribeSongChanged(SubscribeSongChanged),
    
    /// Subscribe to section added (server streaming)
    #[rpc(tx=mpsc::Sender<SectionAddedMessage>)]
    SubscribeSectionAdded(SubscribeSectionAdded),
    
    /// Subscribe to section removed (server streaming)
    #[rpc(tx=mpsc::Sender<SectionRemovedMessage>)]
    SubscribeSectionRemoved(SubscribeSectionRemoved),
    
    /// Subscribe to section changed (server streaming)
    #[rpc(tx=mpsc::Sender<SectionChangedMessage>)]
    SubscribeSectionChanged(SubscribeSectionChanged),
    
    /// Subscribe to lyrics changed (server streaming)
    #[rpc(tx=mpsc::Sender<LyricsChangedMessage>)]
    SubscribeLyrics(SubscribeLyrics),
    
    /// Subscribe to lyrics annotations changed (server streaming)
    #[rpc(tx=mpsc::Sender<LyricsAnnotationsChangedMessage>)]
    SubscribeLyricsAnnotations(SubscribeLyricsAnnotations),
    
    /// Subscribe to active indices changed (server streaming)
    #[rpc(tx=mpsc::Sender<ActiveIndicesChangedMessage>)]
    SubscribeActiveIndices(SubscribeActiveIndices),
    
    /// Subscribe to active song index changed (server streaming)
    #[rpc(tx=mpsc::Sender<ActiveSongIndexChangedMessage>)]
    SubscribeActiveSongIndex(SubscribeActiveSongIndex),
    
    /// Subscribe to active section index changed (server streaming)
    #[rpc(tx=mpsc::Sender<ActiveSectionIndexChangedMessage>)]
    SubscribeActiveSectionIndex(SubscribeActiveSectionIndex),
    
    /// Subscribe to active slide index changed (server streaming)
    #[rpc(tx=mpsc::Sender<ActiveSlideIndexChangedMessage>)]
    SubscribeActiveSlideIndex(SubscribeActiveSlideIndex),
}

/// Client API for setlist reactive service
pub struct SetlistReactiveApi {
    inner: Client<SetlistReactiveProtocol>,
}

impl SetlistReactiveApi {
    pub const ALPN: &[u8] = b"irpc-iroh/setlist-reactive/1";

    /// Create a new setlist reactive API (server-side)
    pub fn spawn(service: crate::reactive::SetlistReactiveService) -> Self {
        // TODO: Implement actor that bridges reactive streams to IRPC
        todo!("Implement SetlistReactiveActor")
    }

    /// Connect to a remote setlist reactive service (client-side)
    pub fn connect(
        endpoint: iroh::Endpoint,
        addr: impl Into<iroh::EndpointAddr>,
    ) -> Result<Self, anyhow::Error> {
        use irpc_iroh::IrohLazyRemoteConnection;
        let conn = IrohLazyRemoteConnection::new(endpoint, addr.into(), Self::ALPN.to_vec());
        Ok(SetlistReactiveApi {
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
        Ok(IrohProtocol::new(SetlistReactiveProtocol::remote_handler(local)))
    }

    /// Subscribe to setlist structure changes
    pub async fn subscribe_setlist_structure(&self) -> irpc::Result<mpsc::Receiver<SetlistStructureChangedMessage>> {
        self.inner.server_streaming(SubscribeSetlistStructure, 32).await
    }

    /// Subscribe to song added
    pub async fn subscribe_song_added(&self) -> irpc::Result<mpsc::Receiver<SongAddedMessage>> {
        self.inner.server_streaming(SubscribeSongAdded, 32).await
    }

    /// Subscribe to song removed
    pub async fn subscribe_song_removed(&self) -> irpc::Result<mpsc::Receiver<SongRemovedMessage>> {
        self.inner.server_streaming(SubscribeSongRemoved, 32).await
    }

    /// Subscribe to songs reordered
    pub async fn subscribe_songs_reordered(&self) -> irpc::Result<mpsc::Receiver<SongsReorderedMessage>> {
        self.inner.server_streaming(SubscribeSongsReordered, 32).await
    }

    /// Subscribe to song changed
    pub async fn subscribe_song_changed(&self) -> irpc::Result<mpsc::Receiver<SongChangedMessage>> {
        self.inner.server_streaming(SubscribeSongChanged, 32).await
    }

    /// Subscribe to section added
    pub async fn subscribe_section_added(&self) -> irpc::Result<mpsc::Receiver<SectionAddedMessage>> {
        self.inner.server_streaming(SubscribeSectionAdded, 32).await
    }

    /// Subscribe to section removed
    pub async fn subscribe_section_removed(&self) -> irpc::Result<mpsc::Receiver<SectionRemovedMessage>> {
        self.inner.server_streaming(SubscribeSectionRemoved, 32).await
    }

    /// Subscribe to section changed
    pub async fn subscribe_section_changed(&self) -> irpc::Result<mpsc::Receiver<SectionChangedMessage>> {
        self.inner.server_streaming(SubscribeSectionChanged, 32).await
    }

    /// Subscribe to lyrics changed
    pub async fn subscribe_lyrics(&self) -> irpc::Result<mpsc::Receiver<LyricsChangedMessage>> {
        self.inner.server_streaming(SubscribeLyrics, 32).await
    }

    /// Subscribe to lyrics annotations changed
    pub async fn subscribe_lyrics_annotations(&self) -> irpc::Result<mpsc::Receiver<LyricsAnnotationsChangedMessage>> {
        self.inner.server_streaming(SubscribeLyricsAnnotations, 32).await
    }

    /// Subscribe to active indices changed
    pub async fn subscribe_active_indices(&self) -> irpc::Result<mpsc::Receiver<ActiveIndicesChangedMessage>> {
        self.inner.server_streaming(SubscribeActiveIndices, 32).await
    }

    /// Subscribe to active song index changed
    pub async fn subscribe_active_song_index(&self) -> irpc::Result<mpsc::Receiver<ActiveSongIndexChangedMessage>> {
        self.inner.server_streaming(SubscribeActiveSongIndex, 32).await
    }

    /// Subscribe to active section index changed
    pub async fn subscribe_active_section_index(&self) -> irpc::Result<mpsc::Receiver<ActiveSectionIndexChangedMessage>> {
        self.inner.server_streaming(SubscribeActiveSectionIndex, 32).await
    }

    /// Subscribe to active slide index changed
    pub async fn subscribe_active_slide_index(&self) -> irpc::Result<mpsc::Receiver<ActiveSlideIndexChangedMessage>> {
        self.inner.server_streaming(SubscribeActiveSlideIndex, 32).await
    }
}

