//! Setlist Stream IRPC Protocol
//!
//! Defines the irpc protocol for streaming setlist state updates.
//! This protocol is backend-agnostic and can be implemented by any DAW backend.

use crate::core::{Setlist, SetlistApi};
use anyhow::Result;
use async_trait::async_trait;
use iroh::{Endpoint, EndpointAddr};
use irpc::{
    channel::{mpsc, oneshot},
    rpc::RemoteService,
    rpc_requests, Client, Request, WithChannels,
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

/// Transport control commands
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TransportCommand {
    Play,
    Pause,
    Stop,
    TogglePlayPause,
}

/// Navigation commands
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum NavigationCommand {
    NextSectionOrSong,
    PreviousSectionOrSong,
}

/// Seek to a specific section
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeekToSection {
    pub song_index: usize,
    pub section_index: usize,
}

/// Seek to a specific song (switches to that song's tab and moves cursor to beginning)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeekToSong {
    pub song_index: usize,
}

/// Seek to a specific time position within a song
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeekToTime {
    pub song_index: usize,
    pub time_seconds: f64, // Time position relative to song start
}

/// Toggle loop for current song
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToggleLoop;

/// Advance to the next syllable and assign it to the next MIDI note at edit cursor
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdvanceSyllable;

/// Get current lyrics state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GetLyricsState;

/// Response for GetLyricsState
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LyricsState {
    pub current_line_index: Option<usize>,
    pub current_syllable_index: Option<usize>,
    pub total_syllables: usize,
    pub line_text: Option<String>,
    pub has_lyrics: bool,
}

/// Assign syllable to MIDI note at edit cursor position
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AssignSyllableToNote {
    pub syllable_text: String,
}

/// Update lyrics for a song
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UpdateLyrics {
    pub song_index: usize,
    pub lyrics: lyrics::Lyrics,
}

/// IRPC protocol for setlist stream service
#[rpc_requests(message = SetlistStreamMessage)]
#[derive(Serialize, Deserialize, Debug)]
pub enum SetlistStreamProtocol {
    /// Subscribe to setlist state updates (server streaming)
    #[rpc(tx=mpsc::Sender<SetlistUpdateMessage>)]
    SubscribeSetlist(SubscribeSetlist),
    /// Transport control (play, pause, stop, toggle)
    #[rpc(tx=oneshot::Sender<Result<(), String>>)]
    TransportCommand(TransportCommand),
    /// Navigation (next/previous section or song)
    #[rpc(tx=oneshot::Sender<Result<(), String>>)]
    NavigationCommand(NavigationCommand),
    /// Seek to a specific section
    #[rpc(tx=oneshot::Sender<Result<(), String>>)]
    SeekToSection(SeekToSection),
    /// Seek to a specific song
    #[rpc(tx=oneshot::Sender<Result<(), String>>)]
    SeekToSong(SeekToSong),
    /// Seek to a specific time position within a song
    #[rpc(tx=oneshot::Sender<Result<(), String>>)]
    SeekToTime(SeekToTime),
    /// Toggle loop for current song
    #[rpc(tx=oneshot::Sender<Result<(), String>>)]
    ToggleLoop(ToggleLoop),
    /// Advance to next syllable and assign to next MIDI note
    #[rpc(tx=oneshot::Sender<Result<LyricsState, String>>)]
    AdvanceSyllable(AdvanceSyllable),
    /// Get current lyrics state
    #[rpc(tx=oneshot::Sender<Result<LyricsState, String>>)]
    GetLyricsState(GetLyricsState),
    /// Assign a specific syllable to the MIDI note at edit cursor
    #[rpc(tx=oneshot::Sender<Result<(), String>>)]
    AssignSyllableToNote(AssignSyllableToNote),
    /// Update lyrics for a song
    #[rpc(tx=oneshot::Sender<Result<(), String>>)]
    UpdateLyrics(UpdateLyrics),
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

/// Trait for backends that can handle setlist commands
/// 
/// This allows the setlist stream actor to execute commands
/// on different backends (REAPER, other DAWs, etc.)
#[async_trait]
pub trait SetlistCommandHandler: Send + Sync {
    /// Execute a transport command
    async fn execute_transport_command(&self, command: TransportCommand) -> Result<(), String>;
    
    /// Execute a navigation command
    async fn execute_navigation_command(&self, command: NavigationCommand) -> Result<(), String>;
    
    /// Seek to a specific section
    async fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String>;
    
    /// Seek to a specific song (switches to that song's tab and moves cursor to beginning)
    async fn seek_to_song(&self, song_index: usize) -> Result<(), String>;
    
    /// Seek to a specific time position within a song
    async fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> Result<(), String>;
    
    /// Toggle loop for current song
    async fn toggle_loop(&self) -> Result<(), String>;
    
    /// Advance to the next syllable and assign it to the next MIDI note at edit cursor
    async fn advance_syllable(&self) -> Result<LyricsState, String>;
    
    /// Get current lyrics state
    async fn get_lyrics_state(&self) -> Result<LyricsState, String>;
    
    /// Assign a specific syllable to the MIDI note at edit cursor position
    async fn assign_syllable_to_note(&self, syllable_text: String) -> Result<(), String>;
    
    /// Update lyrics for a song
    async fn update_lyrics(&self, song_index: usize, lyrics: lyrics::Lyrics) -> Result<(), String>;
}

/// Setlist stream actor that handles client subscriptions and broadcasts updates
struct SetlistStreamActor {
    recv: tokio::sync::mpsc::Receiver<SetlistStreamMessage>,
    /// Broadcast channel for setlist updates
    /// The polling loop sends updates here, and subscriber tasks forward them to their clients
    broadcast_tx: tokio::sync::broadcast::Sender<SetlistUpdateMessage>,
    /// Provider for getting setlist state
    state_provider: Arc<dyn SetlistStateProvider>,
    /// Handler for executing commands (optional)
    command_handler: Option<Arc<dyn SetlistCommandHandler>>,
}

impl SetlistStreamActor {
    pub fn spawn(state_provider: Arc<dyn SetlistStateProvider>) -> SetlistStreamApi {
        Self::spawn_with_handler(state_provider, None)
    }
    
    pub fn spawn_with_handler(
        state_provider: Arc<dyn SetlistStateProvider>,
        command_handler: Option<Arc<dyn SetlistCommandHandler>>,
    ) -> SetlistStreamApi {
        let (tx, rx) = tokio::sync::mpsc::channel(16);
        // Create a broadcast channel with a large buffer (1000 messages) to handle bursts
        let (broadcast_tx, _) = tokio::sync::broadcast::channel(1000);

        let actor = Self {
            recv: rx,
            broadcast_tx: broadcast_tx.clone(),
            state_provider: state_provider.clone(),
            command_handler: command_handler.clone(),
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
            SetlistStreamMessage::TransportCommand(cmd) => {
                let WithChannels { tx, inner, span, .. } = cmd;
                let handler = self.command_handler.clone();
                tokio::task::spawn(async move {
                    let _entered = span.enter();
                    let result = if let Some(handler) = handler {
                        handler.execute_transport_command(inner).await
                    } else {
                        Err("Command handler not available".to_string())
                    };
                    if let Err(e) = tx.send(result).await {
                        warn!("Failed to send transport command response: {}", e);
                    }
                });
            }
            SetlistStreamMessage::NavigationCommand(cmd) => {
                let WithChannels { tx, inner, span, .. } = cmd;
                let handler = self.command_handler.clone();
                tokio::task::spawn(async move {
                    let _entered = span.enter();
                    let result = if let Some(handler) = handler {
                        handler.execute_navigation_command(inner).await
                    } else {
                        Err("Command handler not available".to_string())
                    };
                    if let Err(e) = tx.send(result).await {
                        warn!("Failed to send navigation command response: {}", e);
                    }
                });
            }
            SetlistStreamMessage::SeekToSection(cmd) => {
                let WithChannels { tx, inner, span, .. } = cmd;
                let handler = self.command_handler.clone();
                tokio::task::spawn(async move {
                    let _entered = span.enter();
                    let result = if let Some(handler) = handler {
                        handler.seek_to_section(inner.song_index, inner.section_index).await
                    } else {
                        Err("Command handler not available".to_string())
                    };
                    if let Err(e) = tx.send(result).await {
                        warn!("Failed to send seek to section response: {}", e);
                    }
                });
            }
            SetlistStreamMessage::SeekToSong(cmd) => {
                let WithChannels { tx, inner, span, .. } = cmd;
                let handler = self.command_handler.clone();
                tokio::task::spawn(async move {
                    let _entered = span.enter();
                    let result = if let Some(handler) = handler {
                        handler.seek_to_song(inner.song_index).await
                    } else {
                        Err("Command handler not available".to_string())
                    };
                    if let Err(e) = tx.send(result).await {
                        warn!("Failed to send seek to song response: {}", e);
                    }
                });
            }
            SetlistStreamMessage::SeekToTime(cmd) => {
                let WithChannels { tx, inner, span, .. } = cmd;
                let handler = self.command_handler.clone();
                tokio::task::spawn(async move {
                    let _entered = span.enter();
                    let result = if let Some(handler) = handler {
                        handler.seek_to_time(inner.song_index, inner.time_seconds).await
                    } else {
                        Err("Command handler not available".to_string())
                    };
                    if let Err(e) = tx.send(result).await {
                        warn!("Failed to send seek to time response: {}", e);
                    }
                });
            }
            SetlistStreamMessage::ToggleLoop(cmd) => {
                let WithChannels { tx, span, .. } = cmd;
                let handler = self.command_handler.clone();
                tokio::task::spawn(async move {
                    let _entered = span.enter();
                    let result = if let Some(handler) = handler {
                        handler.toggle_loop().await
                    } else {
                        Err("Command handler not available".to_string())
                    };
                    if let Err(e) = tx.send(result).await {
                        warn!("Failed to send toggle loop response: {}", e);
                    }
                });
            }
            SetlistStreamMessage::AdvanceSyllable(cmd) => {
                let WithChannels { tx, inner, span, .. } = cmd;
                let handler = self.command_handler.clone();
                tokio::task::spawn(async move {
                    let _entered = span.enter();
                    let result = if let Some(handler) = handler {
                        handler.advance_syllable().await
                    } else {
                        Err("Command handler not available".to_string())
                    };
                    if let Err(e) = tx.send(result).await {
                        warn!("Failed to send advance syllable response: {}", e);
                    }
                });
            }
            SetlistStreamMessage::GetLyricsState(cmd) => {
                let WithChannels { tx, inner, span, .. } = cmd;
                let handler = self.command_handler.clone();
                tokio::task::spawn(async move {
                    let _entered = span.enter();
                    let result = if let Some(handler) = handler {
                        handler.get_lyrics_state().await
                    } else {
                        Err("Command handler not available".to_string())
                    };
                    if let Err(e) = tx.send(result).await {
                        warn!("Failed to send get lyrics state response: {}", e);
                    }
                });
            }
            SetlistStreamMessage::AssignSyllableToNote(cmd) => {
                let WithChannels { tx, inner, span, .. } = cmd;
                let handler = self.command_handler.clone();
                tokio::task::spawn(async move {
                    let _entered = span.enter();
                    let result = if let Some(handler) = handler {
                        handler.assign_syllable_to_note(inner.syllable_text).await
                    } else {
                        Err("Command handler not available".to_string())
                    };
                    if let Err(e) = tx.send(result).await {
                        warn!("Failed to send assign syllable to note response: {}", e);
                    }
                });
            }
            SetlistStreamMessage::UpdateLyrics(cmd) => {
                let WithChannels { tx, inner, span, .. } = cmd;
                let handler = self.command_handler.clone();
                tokio::task::spawn(async move {
                    let _entered = span.enter();
                    let result = if let Some(handler) = handler {
                        handler.update_lyrics(inner.song_index, inner.lyrics).await
                    } else {
                        Err("Command handler not available".to_string())
                    };
                    if let Err(e) = tx.send(result).await {
                        warn!("Failed to send update lyrics response: {}", e);
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
            match state_provider.get_setlist_api().await {
                Ok(setlist_api) => {
                    let update = SetlistUpdateMessage { 
                        setlist_api,
                    };
                    // Broadcast to all subscribers (non-blocking)
                    // If channel is full, skip this update to avoid blocking
                    if broadcast_tx.send(update).is_ok() {
                        SEND_COUNT.fetch_add(1, Ordering::Relaxed);
                    }
                    
                    // Log periodically to verify updates are being sent
                    let count = SEND_COUNT.load(Ordering::Relaxed);
                    if count % 1000 == 0 && count > 0 {
                        tracing::debug!(
                            send_count = count,
                            "Setlist stream: broadcasting updates ({} messages sent)",
                            count
                        );
                    }
                }
                Err(e) => {
                    // Log error periodically to avoid spam
                    static ERROR_COUNT: AtomicU64 = AtomicU64::new(0);
                    let error_count = ERROR_COUNT.fetch_add(1, Ordering::Relaxed);
                    if error_count % 1000 == 0 {
                        tracing::warn!(
                            error_count,
                            error = %e,
                            "Setlist stream: failed to get setlist API state"
                        );
                    }
                }
            }
        }
    }
}

/// Setlist stream API for clients
#[derive(Debug, Clone)]
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
    
    /// Create a new setlist stream API with a state provider and command handler
    /// 
    /// This is called by the server (e.g., REAPER extension) to create
    /// a setlist stream service with command execution capabilities.
    pub fn spawn_with_handler(
        state_provider: Arc<dyn SetlistStateProvider>,
        command_handler: Option<Arc<dyn SetlistCommandHandler>>,
    ) -> Self {
        SetlistStreamActor::spawn_with_handler(state_provider, command_handler)
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
    
    /// Execute a transport command
    pub async fn transport_command(&self, command: TransportCommand) -> irpc::Result<Result<(), String>> {
        let msg = command;
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
    
    /// Execute a navigation command
    pub async fn navigation_command(&self, command: NavigationCommand) -> irpc::Result<Result<(), String>> {
        let msg = command;
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
    
    /// Seek to a specific section
    pub async fn seek_to_section(&self, song_index: usize, section_index: usize) -> irpc::Result<Result<(), String>> {
        let msg = SeekToSection { song_index, section_index };
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
    
    /// Seek to a specific song (switches to that song's tab and moves cursor to beginning)
    pub async fn seek_to_song(&self, song_index: usize) -> irpc::Result<Result<(), String>> {
        let msg = SeekToSong { song_index };
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
    
    /// Seek to a specific time position within a song
    pub async fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> irpc::Result<Result<(), String>> {
        let msg = SeekToTime { song_index, time_seconds };
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
    
    /// Toggle loop for current song
    pub async fn toggle_loop(&self) -> irpc::Result<Result<(), String>> {
        let msg = ToggleLoop;
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
    
    /// Advance to next syllable and assign to next MIDI note
    pub async fn advance_syllable(&self) -> irpc::Result<Result<LyricsState, String>> {
        let msg = AdvanceSyllable;
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
    
    /// Get current lyrics state
    pub async fn get_lyrics_state(&self) -> irpc::Result<Result<LyricsState, String>> {
        let msg = GetLyricsState;
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
    
    /// Assign syllable to MIDI note at edit cursor
    pub async fn assign_syllable_to_note(&self, syllable_text: String) -> irpc::Result<Result<(), String>> {
        let msg = AssignSyllableToNote { syllable_text };
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
    
    /// Update lyrics for a song
    pub async fn update_lyrics(&self, song_index: usize, lyrics: lyrics::Lyrics) -> irpc::Result<Result<(), String>> {
        let msg = UpdateLyrics { song_index, lyrics };
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

