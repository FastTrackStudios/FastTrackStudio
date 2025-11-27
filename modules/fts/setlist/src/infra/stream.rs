//! Setlist Stream IRPC Protocol
//!
//! Defines the irpc protocol for streaming setlist state updates.
//! This protocol is backend-agnostic and can be implemented by any DAW backend.

use crate::{Setlist, SetlistApi, Song};
use anyhow::{Context, Result};
use async_trait::async_trait;
use daw::transport::Transport;
use iroh::{protocol::ProtocolHandler, Endpoint, EndpointAddr};
use irpc::{
    channel::{mpsc, oneshot},
    rpc::RemoteService,
    rpc_requests, Client, Request, WithChannels,
};
use irpc_iroh::{IrohLazyRemoteConnection, IrohProtocol};
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc, OnceLock,
    },
    time::Instant,
};
use tokio::sync::broadcast;
use tokio::time;
use tracing::{debug, error, info, warn};


/// Granular setlist update message sent over the irpc stream
/// 
/// Instead of sending the entire setlist on every update, we send only what changed.
/// This reduces payload size and prevents unnecessary rerenders in the desktop app.
/// 
/// Note: Uses externally tagged enum (default) for PostCard compatibility.
/// PostCard does not support internally tagged enums.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SetlistUpdateMessage {
    /// Full setlist update (sent on initial connection or major changes)
    /// Contains the complete setlist structure (songs, sections, metadata) but NOT tracks/transport
    FullSetlist {
                        setlist: crate::Setlist,
        active_song_index: Option<usize>,
        active_section_index: Option<usize>,
        active_slide_index: Option<usize>,
    },
    
    /// Update tracks for a specific song
    /// Only sent when tracks change for that song
    SongTracks {
        song_index: usize,
        tracks: Vec<daw::tracks::Track>,
    },
    
    /// Update transport info for a specific song
    /// Only sent when transport state changes for that song
    SongTransport {
        song_index: usize,
        transport: Transport,
    },
    
    /// Update active indices (which song/section/slide is currently active)
    /// Sent frequently as playback progresses
    ActiveIndices {
        active_song_index: Option<usize>,
        active_section_index: Option<usize>,
        active_slide_index: Option<usize>,
    },
    
    /// Update song metadata (name, sections, lyrics structure, etc.)
    /// Only sent when song structure changes (sections added/removed, name changed, etc.)
    SongMetadata {
        song_index: usize,
                        song: crate::Song,
    },
    
    /// Add a new song to the setlist
    SongAdded {
        song_index: usize,
                        song: crate::Song,
    },
    
    /// Remove a song from the setlist
    SongRemoved {
        song_index: usize,
    },
    
    /// Songs reordered in the setlist
    SongsReordered {
                        setlist: crate::Setlist,
    },
}

/// Request to subscribe to setlist structure updates (songs, sections, metadata)
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSetlistStructure;

/// Request to subscribe to active indices updates (which song/section/slide is active)
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeActiveIndices;

/// Request to subscribe to song tracks updates
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSongTracks;

/// Request to subscribe to song transport updates
#[derive(Debug, Serialize, Deserialize)]
pub struct SubscribeSongTransport;

/// Request to subscribe to all setlist updates (legacy, for backward compatibility)
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
    /// Subscribe to setlist structure updates (songs, sections, metadata) - server streaming
    #[rpc(tx=mpsc::Sender<SetlistUpdateMessage>)]
    SubscribeSetlistStructure(SubscribeSetlistStructure),
    /// Subscribe to active indices updates (which song/section/slide is active) - server streaming
    #[rpc(tx=mpsc::Sender<SetlistUpdateMessage>)]
    SubscribeActiveIndices(SubscribeActiveIndices),
    /// Subscribe to song tracks updates - server streaming
    #[rpc(tx=mpsc::Sender<SetlistUpdateMessage>)]
    SubscribeSongTracks(SubscribeSongTracks),
    /// Subscribe to song transport updates - server streaming
    #[rpc(tx=mpsc::Sender<SetlistUpdateMessage>)]
    SubscribeSongTransport(SubscribeSongTransport),
    /// Subscribe to all setlist updates (legacy, for backward compatibility) - server streaming
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

/// Register the broadcast sender for reactive change detection
/// This allows change detection middleware to broadcast updates reactively
fn register_broadcast_sender(
    broadcast_tx: broadcast::Sender<SetlistUpdateMessage>,
    state_provider: Arc<dyn SetlistStateProvider>,
) {
    static BROADCAST_SENDER: OnceLock<broadcast::Sender<SetlistUpdateMessage>> = OnceLock::new();
    static STATE_PROVIDER: OnceLock<Arc<dyn SetlistStateProvider>> = OnceLock::new();
    
    BROADCAST_SENDER.set(broadcast_tx).ok();
    STATE_PROVIDER.set(state_provider).ok();
    
    info!("[Setlist Stream] Registered broadcast sender for reactive change detection");
}

/// Get the registered broadcast sender (for use by change detection)
pub fn get_broadcast_sender() -> Option<broadcast::Sender<SetlistUpdateMessage>> {
    static BROADCAST_SENDER: OnceLock<broadcast::Sender<SetlistUpdateMessage>> = OnceLock::new();
    BROADCAST_SENDER.get().cloned()
}

/// Get the registered state provider (for use by change detection)
pub fn get_state_provider() -> Option<Arc<dyn SetlistStateProvider>> {
    static STATE_PROVIDER: OnceLock<Arc<dyn SetlistStateProvider>> = OnceLock::new();
    STATE_PROVIDER.get().cloned()
}

/// Setlist stream actor that handles client subscriptions and broadcasts updates
pub(crate) struct SetlistStreamActor {
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

        // Register the broadcast sender so change detection can use it
        register_broadcast_sender(broadcast_tx.clone(), state_provider.clone());

        let actor = Self {
            recv: rx,
            broadcast_tx: broadcast_tx.clone(),
            state_provider: state_provider.clone(),
            command_handler: command_handler.clone(),
        };

        // Spawn the actor to handle messages
        // Try to spawn if runtime exists, otherwise store for later
        let (actor_opt, broadcast_tx_opt, state_provider_opt) = if let Ok(handle) = tokio::runtime::Handle::try_current() {
            handle.spawn(actor.run());
            (None, None, None)
        } else {
            tracing::warn!("No tokio runtime available when creating SetlistStreamApi - actor will be spawned in tokio runtime thread");
            (Some(actor), Some(broadcast_tx.clone()), Some(state_provider.clone()))
        };

        // Spawn the polling task that reads setlist state and broadcasts to all subscribers
        let state_provider_for_polling = state_provider.clone();
        if let Ok(handle) = tokio::runtime::Handle::try_current() {
            handle.spawn(async move {
                Self::poll_and_broadcast(broadcast_tx, state_provider_for_polling).await;
            });
        } else {
            tracing::warn!("No tokio runtime available when creating SetlistStreamApi - polling task will be spawned in tokio runtime thread");
        }

        SetlistStreamApi {
            inner: Client::local(tx),
            actor: actor_opt,
            broadcast_tx: broadcast_tx_opt,
            state_provider: state_provider_opt,
        }
    }
    
    /// Spawn the actor in the current tokio runtime
    /// This should be called from within a tokio runtime context
    pub fn spawn_actor_in_runtime(actor: Self) {
        tokio::spawn(actor.run());
    }

    pub(crate) async fn run(mut self) {
        while let Some(msg) = self.recv.recv().await {
            self.handle(msg).await;
        }
    }

    /// Handle setlist structure subscription (songs, sections, metadata)
    async fn handle_setlist_structure_subscription(
        &self,
        sub: WithChannels<SubscribeSetlistStructure, SetlistStreamProtocol>,
    ) {
        let WithChannels { tx, .. } = sub;
        
        // CRITICAL: Send initial message synchronously before spawning task
        // This ensures the connection is established before the handler returns
        let state_provider_for_initial = self.state_provider.clone();
        let initial_update = match state_provider_for_initial.get_setlist_api().await {
            Ok(setlist_api) => {
                debug!("[Setlist Stream] Got setlist API for SetlistStructure, sending initial update with {} songs", setlist_api.get_setlist().songs.len());
                let mut setlist_without_tracks = setlist_api.get_setlist().clone();
                // Clear tracks from each song's project
                for song in &mut setlist_without_tracks.songs {
                    if let Some(ref mut project) = song.project {
                        project.set_tracks(Vec::new());
                    }
                }
                SetlistUpdateMessage::FullSetlist {
                    setlist: setlist_without_tracks,
                    active_song_index: setlist_api.active_song_index(),
                    active_section_index: setlist_api.active_section_index(),
                    active_slide_index: setlist_api.active_slide_index(),
                }
            },
            Err(e) => {
                warn!("[Setlist Stream] Setlist state not available yet for SetlistStructure: {}, sending empty message", e);
                let empty_setlist = Setlist::new("Loading...".to_string())
                    .unwrap_or_else(|_| Setlist {
                        id: None,
                        name: "Loading...".to_string(),
                        songs: Vec::new(),
                        metadata: std::collections::HashMap::new(),
                    });
                SetlistUpdateMessage::FullSetlist {
                    setlist: empty_setlist,
                    active_song_index: None,
                    active_section_index: None,
                    active_slide_index: None,
                }
            }
        };
        
        // Send initial update synchronously to establish connection
        match tx.send(initial_update).await {
            Ok(_) => {
                info!("[Setlist Stream] ✅ Sent initial SetlistStructure update to new subscriber");
            }
            Err(e) => {
                error!(
                    error = %e,
                    error_debug = ?e,
                    "[Setlist Stream] ❌ Failed to send initial SetlistStructure update, client disconnected"
                );
                return; // Client disconnected, don't spawn forwarding task
            }
        }
        
        // Now spawn task to forward subsequent updates
        let mut broadcast_rx = self.broadcast_tx.subscribe();
        tokio::task::spawn(async move {
            info!("[Setlist Stream] 🚀 Started forwarding task for SetlistStructure subscriber");
            
            // Forward all subsequent updates from the broadcast channel that are structure-related
            // Keep the original tx alive - when tx is dropped, the connection closes
            loop {
                match broadcast_rx.recv().await {
                    Ok(update) => {
                        // Only forward structure-related messages
                        if matches!(update, SetlistUpdateMessage::FullSetlist { .. } | 
                                          SetlistUpdateMessage::SongMetadata { .. } |
                                          SetlistUpdateMessage::SongAdded { .. } |
                                          SetlistUpdateMessage::SongRemoved { .. } |
                                          SetlistUpdateMessage::SongsReordered { .. }) {
                            // Forward to client - this will block if the client isn't consuming
                            // but that's okay, we want backpressure
                            if let Err(e) = tx.send(update).await {
                                error!(
                                    error = %e,
                                    error_debug = ?e,
                                    "[Setlist Stream] Client disconnected (tx.send failed) for SetlistStructure, stopping forwarder"
                                );
                                break;
                            }
                        }
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Closed) => {
                        info!("[Setlist Stream] Broadcast channel closed for SetlistStructure, stopping forwarder");
                        break;
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Lagged(skipped)) => {
                        warn!("[Setlist Stream] SetlistStructure subscriber lagged, skipped {} messages", skipped);
                        // Continue - we'll send the next available message
                    }
                }
            }
            
            // Keep the original tx alive until the task ends
            // When tx is dropped, the connection closes
            info!("[Setlist Stream] Forwarding task ended for SetlistStructure, connection will close");
        });
    }

    /// Handle active indices subscription (which song/section/slide is active)
    async fn handle_active_indices_subscription(
        &self,
        sub: WithChannels<SubscribeActiveIndices, SetlistStreamProtocol>,
    ) {
        let WithChannels { tx, .. } = sub;
        
        // CRITICAL: Send initial message synchronously before spawning task
        let state_provider_for_initial = self.state_provider.clone();
        let initial_update = match state_provider_for_initial.get_setlist_api().await {
            Ok(setlist_api) => {
                debug!("[Setlist Stream] Got setlist API for ActiveIndices, sending initial update");
                SetlistUpdateMessage::ActiveIndices {
                    active_song_index: setlist_api.active_song_index(),
                    active_section_index: setlist_api.active_section_index(),
                    active_slide_index: setlist_api.active_slide_index(),
                }
            },
            Err(e) => {
                warn!("[Setlist Stream] Setlist state not available yet for ActiveIndices: {}, sending empty message", e);
                SetlistUpdateMessage::ActiveIndices {
                    active_song_index: None,
                    active_section_index: None,
                    active_slide_index: None,
                }
            }
        };
        
        // Send initial update synchronously to establish connection
        match tx.send(initial_update).await {
            Ok(_) => {
                info!("[Setlist Stream] ✅ Sent initial ActiveIndices update to new subscriber");
            }
            Err(e) => {
                error!(
                    error = %e,
                    error_debug = ?e,
                    "[Setlist Stream] ❌ Failed to send initial ActiveIndices update, client disconnected"
                );
                return; // Client disconnected, don't spawn forwarding task
            }
        }
        
        // Now spawn task to forward subsequent updates
        let mut broadcast_rx = self.broadcast_tx.subscribe();
        tokio::task::spawn(async move {
            info!("[Setlist Stream] 🚀 Started forwarding task for ActiveIndices subscriber");
            
            // Forward all subsequent ActiveIndices updates
            loop {
                match broadcast_rx.recv().await {
                    Ok(update) => {
                        if matches!(update, SetlistUpdateMessage::ActiveIndices { .. }) {
                            if let Err(e) = tx.send(update).await {
                                error!(
                                    error = %e,
                                    error_debug = ?e,
                                    "[Setlist Stream] Client disconnected for ActiveIndices, stopping forwarder"
                                );
                                break;
                            }
                        }
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Closed) => {
                        info!("[Setlist Stream] Broadcast channel closed for ActiveIndices, stopping forwarder");
                        break;
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Lagged(skipped)) => {
                        warn!("[Setlist Stream] ActiveIndices subscriber lagged, skipped {} messages", skipped);
                    }
                }
            }
            
            info!("[Setlist Stream] Forwarding task ended for ActiveIndices, connection will close");
        });
    }

    /// Handle song tracks subscription
    async fn handle_song_tracks_subscription(
        &self,
        sub: WithChannels<SubscribeSongTracks, SetlistStreamProtocol>,
    ) {
        let WithChannels { tx, .. } = sub;
        
        // CRITICAL: Send initial message synchronously before spawning task
        let state_provider_for_initial = self.state_provider.clone();
        let initial_update = match state_provider_for_initial.get_setlist_api().await {
            Ok(setlist_api) => {
                debug!("[Setlist Stream] Got setlist API for SongTracks, sending initial update");
                // Send tracks for active song as initial update
                if let Some(active_song_index) = setlist_api.active_song_index() {
                    if let Some(active_song) = setlist_api.get_setlist().songs.get(active_song_index) {
                        if let Some(project) = &active_song.project {
                            debug!("[Setlist Stream] Sending initial tracks for active song {}", active_song_index);
                            SetlistUpdateMessage::SongTracks {
                                song_index: active_song_index,
                                tracks: project.tracks().to_vec(),
                            }
                        } else {
                            debug!("[Setlist Stream] Active song {} has no project, sending empty tracks", active_song_index);
                            SetlistUpdateMessage::SongTracks {
                                song_index: active_song_index,
                                tracks: Vec::new(),
                            }
                        }
                    } else {
                        warn!("[Setlist Stream] Active song index {} out of bounds, sending empty tracks", active_song_index);
                        SetlistUpdateMessage::SongTracks {
                            song_index: active_song_index,
                            tracks: Vec::new(),
                        }
                    }
                } else {
                    // No active song, send empty tracks for song 0 as fallback
                    debug!("[Setlist Stream] No active song, sending empty tracks for song 0");
                    SetlistUpdateMessage::SongTracks {
                        song_index: 0,
                        tracks: Vec::new(),
                    }
                }
            },
            Err(e) => {
                warn!("[Setlist Stream] Setlist state not available yet for SongTracks: {}, sending empty message", e);
                SetlistUpdateMessage::SongTracks {
                    song_index: 0,
                    tracks: Vec::new(),
                }
            }
        };
        
        // Send initial update synchronously to establish connection
        match tx.send(initial_update).await {
            Ok(_) => {
                info!("[Setlist Stream] ✅ Sent initial SongTracks update to new subscriber");
            }
            Err(e) => {
                error!(
                    error = %e,
                    error_debug = ?e,
                    "[Setlist Stream] ❌ Failed to send initial SongTracks update, client disconnected"
                );
                return; // Client disconnected, don't spawn forwarding task
            }
        }
        
        // Now spawn task to forward subsequent updates
        let mut broadcast_rx = self.broadcast_tx.subscribe();
        tokio::task::spawn(async move {
            info!("[Setlist Stream] 🚀 Started forwarding task for SongTracks subscriber");
            
            // Forward all subsequent SongTracks updates
            loop {
                match broadcast_rx.recv().await {
                    Ok(update) => {
                        if matches!(update, SetlistUpdateMessage::SongTracks { .. }) {
                            if let Err(e) = tx.send(update).await {
                                error!(
                                    error = %e,
                                    error_debug = ?e,
                                    "[Setlist Stream] Client disconnected for SongTracks, stopping forwarder"
                                );
                                break;
                            }
                        }
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Closed) => {
                        info!("[Setlist Stream] Broadcast channel closed for SongTracks, stopping forwarder");
                        break;
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Lagged(skipped)) => {
                        warn!("[Setlist Stream] SongTracks subscriber lagged, skipped {} messages", skipped);
                    }
                }
            }
            
            info!("[Setlist Stream] Forwarding task ended for SongTracks, connection will close");
        });
    }

    /// Handle song transport subscription
    async fn handle_song_transport_subscription(
        &self,
        sub: WithChannels<SubscribeSongTransport, SetlistStreamProtocol>,
    ) {
        let WithChannels { tx, .. } = sub;
        
        // CRITICAL: Send initial message synchronously before spawning task
        let state_provider_for_initial = self.state_provider.clone();
        let initial_update = match state_provider_for_initial.get_setlist_api().await {
            Ok(setlist_api) => {
                debug!("[Setlist Stream] Got setlist API for SongTransport, sending initial update");
                // Send transport for active song as initial update
                if let Some(active_song_index) = setlist_api.active_song_index() {
                    if let Some(active_song) = setlist_api.get_setlist().songs.get(active_song_index) {
                        if let Some(project) = &active_song.project {
                            debug!("[Setlist Stream] Sending initial transport for active song {}", active_song_index);
                            SetlistUpdateMessage::SongTransport {
                                song_index: active_song_index,
                                transport: project.transport().clone(),
                            }
                        } else {
                            debug!("[Setlist Stream] Active song {} has no project, sending default transport", active_song_index);
                            SetlistUpdateMessage::SongTransport {
                                song_index: active_song_index,
                                transport: Transport::default(),
                            }
                        }
                    } else {
                        warn!("[Setlist Stream] Active song index {} out of bounds, sending default transport", active_song_index);
                        SetlistUpdateMessage::SongTransport {
                            song_index: active_song_index,
                            transport: Transport::default(),
                        }
                    }
                } else {
                    // No active song, send default transport for song 0 as fallback
                    debug!("[Setlist Stream] No active song, sending default transport for song 0");
                    SetlistUpdateMessage::SongTransport {
                        song_index: 0,
                        transport: Transport::default(),
                    }
                }
            },
            Err(e) => {
                warn!("[Setlist Stream] Setlist state not available yet for SongTransport: {}, sending empty message", e);
                SetlistUpdateMessage::SongTransport {
                    song_index: 0,
                    transport: Transport::default(),
                }
            }
        };
        
        // Send initial update synchronously to establish connection
        match tx.send(initial_update).await {
            Ok(_) => {
                info!("[Setlist Stream] ✅ Sent initial SongTransport update to new subscriber");
            }
            Err(e) => {
                error!(
                    error = %e,
                    error_debug = ?e,
                    "[Setlist Stream] ❌ Failed to send initial SongTransport update, client disconnected"
                );
                return; // Client disconnected, don't spawn forwarding task
            }
        }
        
        // Now spawn task to forward subsequent updates
        let mut broadcast_rx = self.broadcast_tx.subscribe();
        tokio::task::spawn(async move {
            info!("[Setlist Stream] 🚀 Started forwarding task for SongTransport subscriber");
            
            // Forward all subsequent SongTransport updates
            loop {
                match broadcast_rx.recv().await {
                    Ok(update) => {
                        if matches!(update, SetlistUpdateMessage::SongTransport { .. }) {
                            if let Err(e) = tx.send(update).await {
                                error!(
                                    error = %e,
                                    error_debug = ?e,
                                    "[Setlist Stream] Client disconnected for SongTransport, stopping forwarder"
                                );
                                break;
                            }
                        }
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Closed) => {
                        info!("[Setlist Stream] Broadcast channel closed for SongTransport, stopping forwarder");
                        break;
                    }
                    Err(tokio::sync::broadcast::error::RecvError::Lagged(skipped)) => {
                        warn!("[Setlist Stream] SongTransport subscriber lagged, skipped {} messages", skipped);
                    }
                }
            }
            
            info!("[Setlist Stream] Forwarding task ended for SongTransport, connection will close");
        });
    }

    async fn handle(&mut self, msg: SetlistStreamMessage) {
        match msg {
            SetlistStreamMessage::SubscribeSetlistStructure(sub) => {
                self.handle_setlist_structure_subscription(sub).await;
            }
            SetlistStreamMessage::SubscribeActiveIndices(sub) => {
                self.handle_active_indices_subscription(sub).await;
            }
            SetlistStreamMessage::SubscribeSongTracks(sub) => {
                self.handle_song_tracks_subscription(sub).await;
            }
            SetlistStreamMessage::SubscribeSongTransport(sub) => {
                self.handle_song_transport_subscription(sub).await;
            }
            SetlistStreamMessage::SubscribeSetlist(sub) => {
                let WithChannels { tx, .. } = sub;
                
                // CRITICAL: Send initial message synchronously before spawning task
                // This ensures the connection is established before the handler returns
                // The connection stays alive as long as tx is alive in the spawned task
                let state_provider_for_initial = self.state_provider.clone();
                let initial_update = match state_provider_for_initial.get_setlist_api().await {
                    Ok(setlist_api) => {
                        debug!("[Setlist Stream] Got setlist API, sending initial update with {} songs", setlist_api.get_setlist().songs.len());
                        SetlistUpdateMessage::FullSetlist {
                            setlist: setlist_api.get_setlist().clone(),
                            active_song_index: setlist_api.active_song_index(),
                            active_section_index: setlist_api.active_section_index(),
                            active_slide_index: setlist_api.active_slide_index(),
                        }
                    },
                    Err(e) => {
                        warn!("[Setlist Stream] Setlist state not available yet: {}, sending empty setlist", e);
                        let empty_setlist = Setlist::new("Loading...".to_string())
                            .unwrap_or_else(|_| Setlist {
                                id: None,
                                name: "Loading...".to_string(),
                                songs: Vec::new(),
                                metadata: std::collections::HashMap::new(),
                            });
                        SetlistUpdateMessage::FullSetlist {
                            setlist: empty_setlist,
                            active_song_index: None,
                            active_section_index: None,
                            active_slide_index: None,
                        }
                    }
                };
                
        // Send initial update synchronously to establish connection
        match tx.send(initial_update).await {
            Ok(_) => {
                info!("[Setlist Stream] ✅ Sent initial setlist update to new subscriber");
            }
            Err(e) => {
                error!(
                    error = %e,
                    error_debug = ?e,
                    "[Setlist Stream] ❌ Failed to send initial setlist update, client disconnected"
                );
                return; // Client disconnected, don't spawn forwarding task
            }
        }
                
                // Now spawn task to forward subsequent updates
                // The tx is moved into the task to keep the connection alive
                let mut broadcast_rx = self.broadcast_tx.subscribe();
                tokio::task::spawn(async move {
                    info!("[Setlist Stream] 🚀 Started forwarding task for subscriber");
                    
                    // Forward all subsequent updates from the broadcast channel
                    // Keep the original tx alive - when tx is dropped, the connection closes
                    loop {
                        match broadcast_rx.recv().await {
                            Ok(update) => {
                                // Forward to client - this will block if the client isn't consuming
                                // but that's okay, we want backpressure
                                if let Err(e) = tx.send(update).await {
                                    error!(
                                        error = %e,
                                        error_debug = ?e,
                                        "[Setlist Stream] Client disconnected (tx.send failed), stopping forwarder"
                                    );
                                    break;
                                }
                            }
                            Err(tokio::sync::broadcast::error::RecvError::Closed) => {
                                info!("[Setlist Stream] Broadcast channel closed, stopping forwarder");
                                break;
                            }
                            Err(tokio::sync::broadcast::error::RecvError::Lagged(skipped)) => {
                                warn!("[Setlist Stream] Subscriber lagged, skipped {} messages", skipped);
                                // Continue - we'll send the next available message
                            }
                        }
                    }
                    
                    // Keep the original tx alive until the task ends
                    // When tx is dropped, the connection closes
                    info!("[Setlist Stream] Forwarding task ended, connection will close");
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

    /// Poll setlist state and broadcast granular updates to all subscribers
    /// 
    /// Tracks previous state and only sends updates for what changed:
    /// - ActiveIndices: Sent frequently as playback progresses
    /// - SongTransport: Sent when transport changes for a specific song
    /// - SongTracks: Sent when tracks change for a specific song
    /// - FullSetlist: Sent on initial connection or major structure changes
    pub(crate) async fn poll_and_broadcast(
        broadcast_tx: tokio::sync::broadcast::Sender<SetlistUpdateMessage>,
        state_provider: Arc<dyn SetlistStateProvider>,
    ) {
        
        static SEND_COUNT: AtomicU64 = AtomicU64::new(0);
        static LAST_LOG: OnceLock<std::sync::Mutex<Instant>> = OnceLock::new();
        static FIRST_UPDATE: OnceLock<std::sync::Mutex<bool>> = OnceLock::new();
        
        // Track previous state to detect changes
        let mut prev_setlist_structure: Option<crate::Setlist> = None;
        let mut prev_tracks: HashMap<usize, Vec<daw::tracks::Track>> = HashMap::new();
        let mut prev_transport: HashMap<usize, Transport> = HashMap::new();
        let mut prev_active_indices: (Option<usize>, Option<usize>, Option<usize>) = (None, None, None);
        
        // Poll as fast as possible - update whenever state changes
        let mut interval = time::interval(time::Duration::from_millis(8)); // ~120Hz polling

        loop {
            interval.tick().await;
            
            // Get current setlist API state from the provider
            match state_provider.get_setlist_api().await {
                Ok(setlist_api) => {
                    let setlist = setlist_api.get_setlist();
                    let current_active_indices = (
                        setlist_api.active_song_index(),
                        setlist_api.active_section_index(),
                        setlist_api.active_slide_index(),
                    );
                    
                    // Check if this is the first update - send FullSetlist
                    let is_first = FIRST_UPDATE.get_or_init(|| std::sync::Mutex::new(true));
                    let mut first_guard = is_first.lock().unwrap();
                    if *first_guard {
                        *first_guard = false;
                        drop(first_guard);
                        
                        // Send full setlist structure (without tracks/transport)
                        let mut setlist_without_tracks = setlist.clone();
                        // Clear tracks from each song's project
                        for song in &mut setlist_without_tracks.songs {
                            if let Some(ref mut project) = song.project {
                                project.set_tracks(Vec::new());
                            }
                        }
                        
                        let update = SetlistUpdateMessage::FullSetlist {
                            setlist: setlist_without_tracks,
                            active_song_index: current_active_indices.0,
                            active_section_index: current_active_indices.1,
                            active_slide_index: current_active_indices.2,
                        };
                        if broadcast_tx.send(update).is_ok() {
                            SEND_COUNT.fetch_add(1, Ordering::Relaxed);
                        }
                        
                        // Send tracks and transport for each song
                        for (song_index, song) in setlist.songs.iter().enumerate() {
                            if let Some(project) = &song.project {
                                // Send tracks if available
                                if !project.tracks().is_empty() {
                                    let tracks = project.tracks().to_vec();
                                    let update = SetlistUpdateMessage::SongTracks {
                                        song_index,
                                        tracks: tracks.clone(),
                                    };
                                    if broadcast_tx.send(update).is_ok() {
                                        SEND_COUNT.fetch_add(1, Ordering::Relaxed);
                                    }
                                    prev_tracks.insert(song_index, tracks);
                                }
                                
                                // Send transport if available
                                let transport = project.transport().clone();
                                let update = SetlistUpdateMessage::SongTransport {
                                    song_index,
                                    transport: transport.clone(),
                                };
                                if broadcast_tx.send(update).is_ok() {
                                    SEND_COUNT.fetch_add(1, Ordering::Relaxed);
                                }
                                prev_transport.insert(song_index, transport);
                            }
                        }
                        
                        // Store initial state
                        prev_setlist_structure = Some(setlist.clone());
                        prev_active_indices = current_active_indices;
                        
                        tracing::info!("[Setlist Stream] Sent initial full setlist update with {} songs", setlist.songs.len());
                        continue;
                    }
                    
                    // Check for structure changes (songs added/removed/reordered)
                    let structure_changed = prev_setlist_structure.as_ref()
                        .map(|prev| {
                            prev.songs.len() != setlist.songs.len() ||
                            prev.songs.iter().enumerate().any(|(i, prev_song)| {
                                setlist.songs.get(i)
                                    .map(|curr_song| {
                                        prev_song.name != curr_song.name ||
                                        prev_song.sections.len() != curr_song.sections.len()
                                    })
                                    .unwrap_or(true)
                            })
                        })
                        .unwrap_or(true);
                    
                    if structure_changed {
                        // Major structure change - send FullSetlist
                        let mut setlist_without_tracks = setlist.clone();
                        for song in &mut setlist_without_tracks.songs {
                            if let Some(ref mut project) = song.project {
                                project.set_tracks(Vec::new());
                            }
                        }
                        
                        let update = SetlistUpdateMessage::FullSetlist {
                            setlist: setlist_without_tracks,
                            active_song_index: current_active_indices.0,
                            active_section_index: current_active_indices.1,
                            active_slide_index: current_active_indices.2,
                        };
                        if broadcast_tx.send(update).is_ok() {
                            SEND_COUNT.fetch_add(1, Ordering::Relaxed);
                        }
                        prev_setlist_structure = Some(setlist.clone());
                    }
                    
                    // Check for active indices changes (frequent updates during playback)
                    if prev_active_indices != current_active_indices {
                        let update = SetlistUpdateMessage::ActiveIndices {
                            active_song_index: current_active_indices.0,
                            active_section_index: current_active_indices.1,
                            active_slide_index: current_active_indices.2,
                        };
                        if broadcast_tx.send(update).is_ok() {
                            SEND_COUNT.fetch_add(1, Ordering::Relaxed);
                        }
                        prev_active_indices = current_active_indices;
                    }
                    
                    // Check for track changes per song
                    for (song_index, song) in setlist.songs.iter().enumerate() {
                        if let Some(project) = &song.project {
                            let current_tracks = project.tracks().to_vec();
                            let prev_tracks_for_song = prev_tracks.get(&song_index);
                            
                            // Check if tracks changed (compare by length and names for efficiency)
                            let tracks_changed = prev_tracks_for_song
                                .map(|prev| {
                                    prev.len() != current_tracks.len() ||
                                    prev.iter().zip(current_tracks.iter()).any(|(p, c)| {
                                        p.name != c.name || p.index != c.index
                                    })
                                })
                                .unwrap_or(true);
                            
                            if tracks_changed && !current_tracks.is_empty() {
                                let update = SetlistUpdateMessage::SongTracks {
                                    song_index,
                                    tracks: current_tracks.clone(),
                                };
                                if broadcast_tx.send(update).is_ok() {
                                    SEND_COUNT.fetch_add(1, Ordering::Relaxed);
                                }
                                prev_tracks.insert(song_index, current_tracks);
                            }
                            
                            // Check for transport changes (compare key fields)
                            let current_transport = project.transport();
                            let prev_transport_for_song = prev_transport.get(&song_index);
                            
                            let transport_changed = prev_transport_for_song
                                .map(|prev| {
                                    prev.play_state != current_transport.play_state ||
                                    (prev.playhead_position.time.to_seconds() - current_transport.playhead_position.time.to_seconds()).abs() > 0.01 ||
                                    (prev.tempo.bpm - current_transport.tempo.bpm).abs() > 0.1
                                })
                                .unwrap_or(true);
                            
                            if transport_changed {
                                let update = SetlistUpdateMessage::SongTransport {
                                    song_index,
                                    transport: current_transport.clone(),
                                };
                                if broadcast_tx.send(update).is_ok() {
                                    SEND_COUNT.fetch_add(1, Ordering::Relaxed);
                                }
                                prev_transport.insert(song_index, current_transport.clone());
                            }
                        }
                    }
                    
                    // Clean up tracks/transport for removed songs
                    let current_song_count = setlist.songs.len();
                    prev_tracks.retain(|&idx, _| idx < current_song_count);
                    prev_transport.retain(|&idx, _| idx < current_song_count);
                    
                    // Log periodically to verify updates are being sent
                    let count = SEND_COUNT.load(Ordering::Relaxed);
                    if count % 1000 == 0 && count > 0 {
                        tracing::debug!(
                            send_count = count,
                            "Setlist stream: broadcasting granular updates ({} messages sent)",
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
pub struct SetlistStreamApi {
    inner: Client<SetlistStreamProtocol>,
    /// Actor to process messages (stored for deferred spawning)
    actor: Option<SetlistStreamActor>,
    /// Broadcast channel for polling task
    broadcast_tx: Option<tokio::sync::broadcast::Sender<SetlistUpdateMessage>>,
    /// State provider for polling task
    state_provider: Option<Arc<dyn SetlistStateProvider>>,
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
            actor: None,
            broadcast_tx: None,
            state_provider: None,
        })
    }

    /// Expose this service as a protocol handler for IROH router
    pub fn expose(&self) -> Result<impl ProtocolHandler, anyhow::Error> {
        let local = self
            .inner
            .as_local()
            .context("cannot listen on remote service")?;
        Ok(IrohProtocol::new(SetlistStreamProtocol::remote_handler(local)))
    }
    
    /// Spawn deferred tasks in the current tokio runtime
    /// This should be called from within a tokio runtime context
    /// Returns true if any tasks were spawned
    pub fn spawn_deferred_tasks(&mut self) -> bool {
        let mut spawned = false;
        if let Some(actor) = self.actor.take() {
            tokio::spawn(async move {
                actor.run().await;
            });
            spawned = true;
        }
        if let (Some(broadcast_tx), Some(state_provider)) = (self.broadcast_tx.take(), self.state_provider.take()) {
            tokio::spawn(async move {
                SetlistStreamActor::poll_and_broadcast(broadcast_tx, state_provider).await;
            });
            spawned = true;
        }
        spawned
    }

    /// Subscribe to setlist state updates (legacy, subscribes to all updates)
    /// Returns a receiver that will receive SetlistUpdateMessage messages
    pub async fn subscribe(&self) -> irpc::Result<mpsc::Receiver<SetlistUpdateMessage>> {
        self.inner.server_streaming(SubscribeSetlist, 32).await
    }
    
    /// Subscribe to setlist structure updates (songs, sections, metadata)
    /// Returns a receiver that will receive SetlistUpdateMessage messages
    pub async fn subscribe_structure(&self) -> irpc::Result<mpsc::Receiver<SetlistUpdateMessage>> {
        self.inner.server_streaming(SubscribeSetlistStructure, 32).await
    }
    
    /// Subscribe to active indices updates (which song/section/slide is active)
    /// Returns a receiver that will receive SetlistUpdateMessage messages
    pub async fn subscribe_active_indices(&self) -> irpc::Result<mpsc::Receiver<SetlistUpdateMessage>> {
        self.inner.server_streaming(SubscribeActiveIndices, 32).await
    }
    
    /// Subscribe to song tracks updates
    /// Returns a receiver that will receive SetlistUpdateMessage messages
    pub async fn subscribe_tracks(&self) -> irpc::Result<mpsc::Receiver<SetlistUpdateMessage>> {
        self.inner.server_streaming(SubscribeSongTracks, 32).await
    }
    
    /// Subscribe to song transport updates
    /// Returns a receiver that will receive SetlistUpdateMessage messages
    pub async fn subscribe_transport(&self) -> irpc::Result<mpsc::Receiver<SetlistUpdateMessage>> {
        self.inner.server_streaming(SubscribeSongTransport, 32).await
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

