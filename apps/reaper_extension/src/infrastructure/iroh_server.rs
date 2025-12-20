//! IROH Server Infrastructure
//!
//! Manages the IROH server setup for peer-to-peer communication.
//! Composes multiple IRPC APIs (Transport, Tracks, Setlist) into a single IROH router.

use std::sync::Arc;
use crate::services::{StreamService, SetlistService};
use crate::infrastructure::reactive_app_state::ReactiveAppStateService;
use tracing::{info, error, warn};

/// ALPN strings for each IRPC protocol
/// 
/// These must match the ALPNs defined in the API structs:
/// - TransportApi::ALPN
/// - TrackApi::ALPN
/// - SetlistStreamApi::ALPN
/// - LyricsApi::ALPN
pub mod alpn {
    /// ALPN for transport protocol (matches TransportApi::ALPN)
    pub const TRANSPORT: &[u8] = daw::transport::TransportApi::ALPN;
    /// ALPN for tracks protocol (matches TrackApi::ALPN)
    pub const TRACKS: &[u8] = daw::tracks::TrackApi::ALPN;
    /// ALPN for setlist protocol (matches SetlistStreamApi::ALPN)
    pub const SETLIST: &[u8] = fts::setlist::SetlistStreamApi::ALPN;
    /// ALPN for lyrics protocol (matches LyricsApi::ALPN)
    #[cfg(feature = "lyrics")]
    pub const LYRICS: &[u8] = fts::lyrics::reactive::irpc::LyricsApi::ALPN;
    /// ALPN for chart protocol (matches ChartApi::ALPN)
    #[cfg(feature = "keyflow")]
    pub const CHART: &[u8] = fts::chords::reactive::irpc::ChartApi::ALPN;
}

/// Start the IROH server with all IRPC APIs (Transport, Tracks, Setlist)
/// 
/// Note: This must be called from the main thread because reactive services
/// use `Rc<RefCell<...>>` which is not `Send`. The IRPC APIs are created
/// on the main thread, then the tokio runtime is spawned in a separate thread
/// to handle the IROH router.
pub fn start_iroh_server(
    stream_service: Arc<StreamService>,
    reactive_state: Arc<ReactiveAppStateService>,
    setlist_service: Arc<SetlistService>,
) {
    // Create all IRPC APIs on the main thread (where streams live)
    // Use REAPER implementations - create new instances that share the same streams
    let transport_streams = reactive_state.transport_service.streams().clone();
    let track_streams = reactive_state.track_service.streams().clone();
    #[cfg(feature = "lyrics")]
    let lyrics_streams = reactive_state.lyrics_service.streams().clone();
    
    // Create transport API using REAPER implementation
    let transport_service_box: Box<dyn daw::transport::reactive::TransportReactiveService> = 
        Box::new(crate::infrastructure::reaper_transport_reactive::ReaperTransportReactiveService::new(
            transport_streams,
            setlist_service.clone(),
        ));
    let mut transport_api = daw::transport::TransportApi::spawn(transport_service_box);
    
    // Create tracks API using REAPER implementation
    let track_service_box: Box<dyn daw::tracks::reactive::TrackReactiveService> = 
        Box::new(crate::infrastructure::reaper_track_reactive::ReaperTrackReactiveService::new(
            track_streams,
            setlist_service.clone(),
        ));
    
    // Create track command handler
    // We need TaskSupport to dispatch work to the main thread
    // Get it from the module-level static
    use crate::infrastructure::task_support::get_task_support;
    
    let task_support = get_task_support()
        .expect("TaskSupport not initialized - this should be set up in plugin_main");
    
    // Initialize track reactive service static for command handler
    // Use the same instance that's in reactive_state
    if let Some(reaper_track_service) = &reactive_state.reaper_track_service {
        crate::infrastructure::reaper_track_command::init_track_reactive_service(reaper_track_service.clone());
    }
    
    let track_command_handler: Arc<dyn daw::tracks::reactive::irpc::TrackCommandHandler> = 
        Arc::new(crate::infrastructure::reaper_track_command::ReaperTrackCommandHandler::new(
            setlist_service.clone(),
            task_support,
        ));
    
    let mut tracks_api = daw::tracks::TrackApi::spawn_with_handler(
        track_service_box,
        Some(track_command_handler.clone()),
    );
    
    // Create lyrics API using REAPER implementation (if lyrics feature is enabled)
    #[cfg(feature = "lyrics")]
    let mut lyrics_api = {
        let lyrics_service_box: Box<dyn fts::lyrics::reactive::LyricsReactiveService> = 
            Box::new(crate::infrastructure::reaper_lyrics_reactive::ReaperLyricsReactiveService::new(
                lyrics_streams,
            ));
        fts::lyrics::reactive::irpc::LyricsApi::spawn(lyrics_service_box)
    };
    
    // Create chords API using REAPER implementation (if keyflow feature is enabled)
    #[cfg(feature = "keyflow")]
    let mut chart_api = {
        let chords_streams = fts::chords::reactive::ChordsStreams::new();
        let chords_service_box: Box<dyn fts::chords::reactive::ChordsReactiveService> = 
            Box::new(crate::infrastructure::reaper_chords_reactive::ReaperChordsReactiveService::new(
                chords_streams,
            ));
        fts::chords::reactive::irpc::ChartApi::spawn(chords_service_box)
    };
    
    // Create setlist stream API (on main thread)
    let setlist_api = match stream_service.create_stream_api() {
        Ok(api) => api,
        Err(e) => {
            warn!("Failed to create setlist stream service: {}", e);
            return;
        }
    };
    
    // Extract handler data before exposing (expose consumes the API)
    let transport_handler_data = transport_api.take_handler_data();
    let tracks_handler_data = tracks_api.take_handler_data();
    #[cfg(feature = "lyrics")]
    let lyrics_handler_data = lyrics_api.take_handler_data();
    #[cfg(feature = "keyflow")]
    let chart_handler_data = chart_api.take_handler_data();
    
    // Expose all handlers (on main thread)
    let transport_handler = match transport_api.expose() {
        Ok(handler) => handler,
        Err(e) => {
            warn!("Failed to expose transport service: {}", e);
            return;
        }
    };
    
    let tracks_handler = match tracks_api.expose() {
        Ok(handler) => handler,
        Err(e) => {
            warn!("Failed to expose tracks service: {}", e);
            return;
        }
    };
    
    // Move setlist_api into closure so we can spawn deferred tasks in tokio runtime
    let mut setlist_api_for_spawn = setlist_api;
    let setlist_handler = match setlist_api_for_spawn.expose() {
        Ok(handler) => handler,
        Err(e) => {
            warn!("Failed to expose setlist stream service: {}", e);
            return;
        }
    };
    
    #[cfg(feature = "lyrics")]
    let lyrics_handler = match lyrics_api.expose() {
        Ok(handler) => handler,
        Err(e) => {
            warn!("Failed to expose lyrics service: {}", e);
            return;
        }
    };
    
    #[cfg(feature = "keyflow")]
    let chart_handler = match chart_api.expose() {
        Ok(handler) => handler,
        Err(e) => {
            warn!("Failed to expose chart service: {}", e);
            return;
        }
    };
    
    // Now spawn the tokio runtime in a separate thread to run the IROH router
    // The handlers are already created and are Send, so we can move them
    // We also need to start the message handlers for transport, tracks, and lyrics
    std::thread::spawn(move || {
        let rt = tokio::runtime::Runtime::new().expect("Failed to create tokio runtime");
        
        rt.block_on(async {
            info!("Starting IROH router with all IRPC protocols in tokio runtime...");
            
            // Spawn deferred setlist actor and polling task now that we're in a tokio runtime
            if setlist_api_for_spawn.spawn_deferred_tasks() {
                info!("[Setlist Handler] Spawned deferred actor and polling task");
            }
            
            // Start message handlers now that we're in a tokio runtime
            if let Some((rx, transport_broadcast, play_state_broadcast, tempo_broadcast, position_broadcast)) = transport_handler_data {
                let transport_broadcast_clone = transport_broadcast.clone();
                let play_state_broadcast_clone = play_state_broadcast.clone();
                let tempo_broadcast_clone = tempo_broadcast.clone();
                let position_broadcast_clone = position_broadcast.clone();
                
                tokio::spawn(async move {
                    use daw::transport::reactive::irpc::TransportMessage;
                    use irpc::WithChannels;
                    let mut rx = rx;
                    info!("[Transport Handler] Started listening for transport subscription requests");
                    while let Ok(msg_opt) = rx.recv().await {
                        match msg_opt {
                            Some(msg) => {
                                info!("[Transport Handler] Received message: {:?}", std::mem::discriminant(&msg));
                                match msg {
                                    TransportMessage::SubscribeTransport(WithChannels { tx, .. }) => {
                                        info!("[Transport Handler] ✅ Client subscribed to transport stream");
                                        let mut transport_rx = transport_broadcast_clone.subscribe();
                                        let mut play_state_rx = play_state_broadcast_clone.subscribe();
                                        let mut tempo_rx = tempo_broadcast_clone.subscribe();
                                        let mut position_rx = position_broadcast_clone.subscribe();

                                        tokio::spawn(async move {
                                            info!("[Transport Handler] Started forwarding transport updates to client");
                                            loop {
                                                tokio::select! {
                                                    Ok(msg) = transport_rx.recv() => {
                                                        if tx.send(msg).await.is_err() { 
                                                            warn!("[Transport Handler] Client disconnected (transport_rx)");
                                                            break; 
                                                        }
                                                    }
                                                    Ok(msg) = play_state_rx.recv() => {
                                                        if tx.send(msg).await.is_err() { 
                                                            warn!("[Transport Handler] Client disconnected (play_state_rx)");
                                                            break; 
                                                        }
                                                    }
                                                    Ok(msg) = tempo_rx.recv() => {
                                                        if tx.send(msg).await.is_err() { 
                                                            warn!("[Transport Handler] Client disconnected (tempo_rx)");
                                                            break; 
                                                        }
                                                    }
                                                    Ok(msg) = position_rx.recv() => {
                                                        if tx.send(msg).await.is_err() { 
                                                            warn!("[Transport Handler] Client disconnected (position_rx)");
                                                            break; 
                                                        }
                                                    }
                                                    else => break,
                                                }
                                            }
                                            info!("[Transport Handler] Stopped forwarding transport updates to client");
                                        });
                                    }
                                }
                            }
                            None => {
                                warn!("[Transport Handler] Message channel closed");
                                break;
                            }
                        }
                    }
                    warn!("[Transport Handler] Handler loop ended");
                });
            }
            
            if let Some((rx, tracks_broadcast, track_changed_broadcast, track_added_broadcast, track_removed_broadcast)) = tracks_handler_data {
                let tracks_broadcast_clone = tracks_broadcast.clone();
                let track_changed_broadcast_clone = track_changed_broadcast.clone();
                let track_added_broadcast_clone = track_added_broadcast.clone();
                let track_removed_broadcast_clone = track_removed_broadcast.clone();
                let track_command_handler_clone = track_command_handler.clone();
                
                tokio::spawn(async move {
                    use daw::tracks::reactive::irpc::TrackMessage;
                    use irpc::WithChannels;
                    let mut rx = rx;
                    info!("[Tracks Handler] Started listening for track subscription requests");
                    while let Ok(msg_opt) = rx.recv().await {
                        match msg_opt {
                            Some(msg) => {
                                info!("[Tracks Handler] Received message: {:?}", std::mem::discriminant(&msg));
                                match msg {
                                    TrackMessage::SubscribeTracks(WithChannels { tx, .. }) => {
                                        info!("[Tracks Handler] ✅ Client subscribed to tracks stream");
                                        let mut tracks_rx = tracks_broadcast_clone.subscribe();
                                        let mut track_changed_rx = track_changed_broadcast_clone.subscribe();
                                        let mut track_added_rx = track_added_broadcast_clone.subscribe();
                                        let mut track_removed_rx = track_removed_broadcast_clone.subscribe();

                                        tokio::spawn(async move {
                                            info!("[Tracks Handler] Started forwarding track updates to client");
                                            loop {
                                                tokio::select! {
                                                    Ok(msg) = tracks_rx.recv() => {
                                                        if tx.send(msg).await.is_err() { 
                                                            warn!("[Tracks Handler] Client disconnected (tracks_rx)");
                                                            break; 
                                                        }
                                                    }
                                                    Ok(msg) = track_changed_rx.recv() => {
                                                        if tx.send(msg).await.is_err() { 
                                                            warn!("[Tracks Handler] Client disconnected (track_changed_rx)");
                                                            break; 
                                                        }
                                                    }
                                                    Ok(msg) = track_added_rx.recv() => {
                                                        if tx.send(msg).await.is_err() { 
                                                            warn!("[Tracks Handler] Client disconnected (track_added_rx)");
                                                            break; 
                                                        }
                                                    }
                                                    Ok(msg) = track_removed_rx.recv() => {
                                                        if tx.send(msg).await.is_err() { 
                                                            warn!("[Tracks Handler] Client disconnected (track_removed_rx)");
                                                            break; 
                                                        }
                                                    }
                                                    else => break,
                                                }
                                            }
                                            info!("[Tracks Handler] Stopped forwarding track updates to client");
                                        });
                                    }
                                    TrackMessage::SetTrackMute(WithChannels { tx, inner, .. }) => {
                                        let handler = track_command_handler_clone.clone();
                                        tokio::spawn(async move {
                                            let result = handler.set_track_mute(inner.project_id, inner.track_index, inner.muted).await;
                                            if let Err(e) = tx.send(result).await {
                                                warn!("[Tracks Handler] Failed to send set_track_mute response: {}", e);
                                            }
                                        });
                                    }
                                    TrackMessage::SetTrackSolo(WithChannels { tx, inner, .. }) => {
                                        let handler = track_command_handler_clone.clone();
                                        tokio::spawn(async move {
                                            let result = handler.set_track_solo(inner.project_id, inner.track_index, inner.solo_mode).await;
                                            if let Err(e) = tx.send(result).await {
                                                warn!("[Tracks Handler] Failed to send set_track_solo response: {}", e);
                                            }
                                        });
                                    }
                                }
                            }
                            None => {
                                warn!("[Tracks Handler] Message channel closed");
                                break;
                            }
                        }
                    }
                    warn!("[Tracks Handler] Handler loop ended");
                });
            }
            
            #[cfg(feature = "lyrics")]
            {
                if let Some((rx, lyrics_broadcast, active_slide_broadcast, annotations_broadcast)) = lyrics_handler_data {
                    let lyrics_broadcast_clone = lyrics_broadcast.clone();
                    let active_slide_broadcast_clone = active_slide_broadcast.clone();
                    let annotations_broadcast_clone = annotations_broadcast.clone();
                    
                    tokio::spawn(async move {
                        use fts::lyrics::reactive::irpc::LyricsMessage;
                        use irpc::WithChannels;
                        let mut rx = rx;
                        info!("[Lyrics Handler] Started listening for lyrics subscription requests");
                        while let Ok(msg_opt) = rx.recv().await {
                            match msg_opt {
                                Some(msg) => {
                                    info!("[Lyrics Handler] Received message: {:?}", std::mem::discriminant(&msg));
                                    match msg {
                                        LyricsMessage::SubscribeLyrics(WithChannels { tx, .. }) => {
                                            info!("[Lyrics Handler] ✅ Client subscribed to lyrics stream");
                                            let mut lyrics_rx = lyrics_broadcast_clone.subscribe();
                                            let mut active_slide_rx = active_slide_broadcast_clone.subscribe();
                                            let mut annotations_rx = annotations_broadcast_clone.subscribe();

                                            tokio::spawn(async move {
                                                info!("[Lyrics Handler] Started forwarding lyrics updates to client");
                                                loop {
                                                    tokio::select! {
                                                        Ok(msg) = lyrics_rx.recv() => {
                                                            if tx.send(msg).await.is_err() { 
                                                                warn!("[Lyrics Handler] Client disconnected (lyrics_rx)");
                                                                break; 
                                                            }
                                                        }
                                                        Ok(msg) = active_slide_rx.recv() => {
                                                            if tx.send(msg).await.is_err() { 
                                                                warn!("[Lyrics Handler] Client disconnected (active_slide_rx)");
                                                                break; 
                                                            }
                                                        }
                                                        Ok(msg) = annotations_rx.recv() => {
                                                            if tx.send(msg).await.is_err() { 
                                                                warn!("[Lyrics Handler] Client disconnected (annotations_rx)");
                                                                break; 
                                                            }
                                                        }
                                                        else => break,
                                                    }
                                                }
                                                info!("[Lyrics Handler] Stopped forwarding lyrics updates to client");
                                            });
                                        }
                                    }
                                }
                                None => {
                                    warn!("[Lyrics Handler] Message channel closed");
                                    break;
                                }
                            }
                        }
                        warn!("[Lyrics Handler] Handler loop ended");
                    });
                }
            }
            
            #[cfg(feature = "keyflow")]
            {
                if let Some((rx, chart_broadcast)) = chart_handler_data {
                    let chart_broadcast_clone = chart_broadcast.clone();
                    
                    tokio::spawn(async move {
                        use fts::chords::reactive::irpc::ChartUpdateMessage;
                        use irpc::WithChannels;
                        let mut rx = rx;
                        info!("[Chart Handler] Started listening for chart subscription requests");
                        while let Ok(msg_opt) = rx.recv().await {
                            match msg_opt {
                                Some(fts::chords::reactive::irpc::ChartMessage::SubscribeChart(WithChannels { tx, .. })) => {
                                    info!("[Chart Handler] ✅ Client subscribed to chart stream");
                                    let mut chart_rx = chart_broadcast_clone.subscribe();

                                    tokio::spawn(async move {
                                        info!("[Chart Handler] Started forwarding chart updates to client");
                                        loop {
                                            tokio::select! {
                                                Ok(msg) = chart_rx.recv() => {
                                                    if tx.send(msg).await.is_err() { 
                                                        warn!("[Chart Handler] Client disconnected (chart_rx)");
                                                        break; 
                                                    }
                                                }
                                                else => break,
                                            }
                                        }
                                        info!("[Chart Handler] Stopped forwarding chart updates to client");
                                    });
                                }
                                None => {
                                    warn!("[Chart Handler] Message channel closed");
                                    break;
                                }
                            }
                        }
                        warn!("[Chart Handler] Handler loop ended");
                    });
                }
            }
            
            // Create IROH endpoint
            let endpoint = match iroh::Endpoint::bind().await {
                Ok(ep) => ep,
                Err(e) => {
                    error!("Failed to create IROH endpoint: {}", e);
                    return;
                }
            };
            let endpoint_id = endpoint.id();
                    
            // Build router with all protocols - each has its own ALPN
            let mut router_builder = iroh::protocol::Router::builder(endpoint.clone())
                .accept(alpn::TRANSPORT, transport_handler)
                .accept(alpn::TRACKS, tracks_handler)
                .accept(alpn::SETLIST, setlist_handler);
            
            #[cfg(feature = "lyrics")]
            {
                router_builder = router_builder.accept(alpn::LYRICS, lyrics_handler);
            }
            
            #[cfg(feature = "keyflow")]
            {
                router_builder = router_builder.accept(alpn::CHART, chart_handler);
            }
            
            let _router = router_builder.spawn();
            
            // Store endpoint ID for client discovery
            if let Err(e) = peer_2_peer::iroh_connection::store_endpoint_id(endpoint_id) {
                warn!("Failed to store endpoint ID: {}", e);
            }
            
            info!("IROH router started successfully");
            info!("Router endpoint ID: {}", endpoint_id);
            info!("Transport ALPN: {:?}", String::from_utf8_lossy(alpn::TRANSPORT));
            info!("Tracks ALPN: {:?}", String::from_utf8_lossy(alpn::TRACKS));
            info!("Setlist ALPN: {:?}", String::from_utf8_lossy(alpn::SETLIST));
            #[cfg(feature = "lyrics")]
            info!("Lyrics ALPN: {:?}", String::from_utf8_lossy(alpn::LYRICS));
            #[cfg(feature = "keyflow")]
            info!("Chart ALPN: {:?}", String::from_utf8_lossy(alpn::CHART));
                    
            // Keep router alive - it handles all connections
            // The router will run until shutdown is called
            loop {
                tokio::time::sleep(tokio::time::Duration::from_secs(60)).await;
            }
        });
    });
}

