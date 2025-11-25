use dioxus::prelude::*;
use dioxus_router::{Routable, Router, Outlet};
use setlist::{Setlist, Song, Section, SectionType, SETLIST, TransportCommand, NavigationCommand};
use marker_region::{Marker, application::TempoTimePoint};
use primitives::Position;
use ui::components::*;
// REAPER connection via REAPER_ALPN is disabled - all data comes through setlist stream
// #[cfg(not(target_arch = "wasm32"))]
// use crate::reaper_connection::ReaperConnection;
// #[cfg(not(target_arch = "wasm32"))]
// use peer_2_peer::reaper_api::ReaperStateUpdate;
use tracing::{info, warn};
#[cfg(not(target_arch = "wasm32"))]
use crate::setlist_connection::get_setlist_api;

mod utils;
#[cfg(not(target_arch = "wasm32"))]
mod reaper_connection;
#[cfg(not(target_arch = "wasm32"))]
mod setlist_connection;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    // Initialize tracing subscriber for logging (only for desktop builds)
    #[cfg(feature = "desktop")]
    #[cfg(not(target_arch = "wasm32"))]
    {
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::EnvFilter;
        
        let filter = if std::env::var("RUST_LOG").is_ok() {
            // User specified RUST_LOG - use it directly
            EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into())
        } else {
            // Default filter with IROH warning suppression
            EnvFilter::new("info")
                // Suppress harmless IROH warnings about IPv6 relays and discovery pongs
                .add_directive("iroh::magicsock=error".parse().unwrap())
                .add_directive("iroh_quinn_udp=error".parse().unwrap())
                .add_directive("iroh::protocol::router=error".parse().unwrap())
                // Suppress protocol mismatch warnings (error 120: peer doesn't support any known protocol)
                .add_directive("iroh::protocol=error".parse().unwrap())
        };
        
        tracing_subscriber::registry()
            .with(filter)
            .with(tracing_subscriber::fmt::layer())
            .init();
    }
    
    dioxus::launch(App);
}

#[derive(Routable, Clone, PartialEq, Debug)]
enum Route {
    #[layout(AppLayout)]
    #[route("/")]
    Performance {},
    #[route("/lyrics")]
    Lyrics {},
}

#[component]
fn App() -> Element {
    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: MAIN_CSS }
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        Router::<Route> {}
    }
}

#[component]
fn AppLayout() -> Element {
    // Track connection status
    let is_connected = use_signal(|| false);
    
    #[cfg(not(target_arch = "wasm32"))]
    {
        use_effect({
            let mut is_connected = is_connected;
            move || {
            spawn(async move {
                    // Initialize connection first
                if let Err(e) = setlist_connection::connect_to_reaper_setlist().await {
                        tracing::warn!("Failed to initialize REAPER setlist connection: {}", e);
                        return;
                    }
                    
                    // Wait a bit for the connection status channel to be initialized
                    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
                    
                    // Get connection status receiver
                    let mut connection_rx = match setlist_connection::get_connection_status_receiver() {
                        Some(rx) => rx,
                        None => {
                            warn!("Connection status receiver not available");
                            return;
                        }
                    };
                    
                    // Set initial status
                    is_connected.set(*connection_rx.borrow());
                    
                    // Watch for status changes
                    while connection_rx.changed().await.is_ok() {
                        let connected = *connection_rx.borrow();
                        is_connected.set(connected);
                        if connected {
                            info!("[Desktop] Connection status: Connected");
                        } else {
                            warn!("[Desktop] Connection status: Disconnected");
                        }
                }
            });
            }
        });
    }
    
    #[cfg(target_arch = "wasm32")]
    let is_connected = use_signal(|| false);
    
    // REAPER connection via REAPER_ALPN is disabled - all data comes through setlist stream
    // The setlist stream includes both setlist and transport state, so no separate connection needed
    // TODO: Re-enable if/when we need bidirectional REAPER commands (play/pause, etc.)
    /*
    #[cfg(not(target_arch = "wasm32"))]
    {
        use_effect(move || {
            let (reaper_conn, _connected_rx, _command_sender) = ReaperConnection::new();
            
            // Set up handler for state updates
            let (tx, mut rx) = tokio::sync::mpsc::unbounded_channel::<ReaperStateUpdate>();
            
            let reaper_conn_for_handler = reaper_conn.clone();
            let handler_tx = tx.clone();
            spawn(async move {
                reaper_conn_for_handler.set_state_update_handler(Arc::new(move |update| {
                    if let Err(e) = handler_tx.send(update) {
                        warn!("[DESKTOP] Failed to forward state update to channel: {}", e);
                    }
                })).await;
    });
    
            // Start connection
            spawn(async move {
                if let Err(e) = reaper_conn.start().await {
                    error!("[DESKTOP] Failed to start connection manager: {}", e);
        }
            });
            
            // Process state updates (transport info is embedded in SETLIST)
            spawn(async move {
                while let Some(update) = rx.recv().await {
                    match update {
                        ReaperStateUpdate::Heartbeat => {
                            info!("[DESKTOP] 💓 Received heartbeat");
                        }
                        ReaperStateUpdate::SetlistState(_) => {
                            // Ignored - setlist is handled via SETLIST global signal
                        }
                        ReaperStateUpdate::TransportState(state) => {
                            info!("[DESKTOP] Received TransportState update - playing: {}, position: {:.2}s", 
                                state.is_playing, state.position_seconds);
                            // Transport state is now embedded in each song's transport_info via SETLIST
                            // No need to update a separate TRANSPORT_INFO signal
                        }
                    }
                }
            });
        });
    }
    */
    
    // Derive current song and section index from SETLIST global signal
    let current_song_index = use_memo(move || {
        SETLIST.read().as_ref().and_then(|api| api.active_song_index())
    });
    
    let current_section_index = use_memo(move || {
        SETLIST.read().as_ref().and_then(|api| api.active_section_index())
    });
    
                
    // Mode toggle callback - simplified for now
    let on_toggle_mode = Callback::new(move |_: ()| {
        // TODO: Implement mode toggle if needed
    });
    
    // Callbacks
    #[cfg(not(target_arch = "wasm32"))]
    let on_song_click = Callback::new(move |song_idx: usize| {
        // Seek to the clicked song (switches to that song's tab)
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.seek_to_song(song_idx).await {
                    Ok(Ok(())) => {
                        info!("Successfully sought to song {}", song_idx);
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to seek to song: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error seeking to song: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for song seek");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_song_click = Callback::new(move |_idx: usize| {
        // No-op for wasm32
    });
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_section_click = Callback::new(move |(song_idx, section_idx): (usize, usize)| {
        // Seek to the clicked section
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.seek_to_section(song_idx, section_idx).await {
                    Ok(Ok(())) => {
                        info!("Successfully sought to song {} section {}", song_idx, section_idx);
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to seek to section: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error seeking to section: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for section seek");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_section_click = Callback::new(move |(_song_idx, _section_idx): (usize, usize)| {
        // No-op for wasm32
    });
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_play_pause = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.transport_command(TransportCommand::TogglePlayPause).await {
                    Ok(Ok(())) => {
                        info!("Successfully toggled play/pause");
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to toggle play/pause: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error toggling play/pause: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for play/pause");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_play_pause = Callback::new(move |_: ()| {
        // No-op for wasm32
    });
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_loop_toggle = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.toggle_loop().await {
                    Ok(Ok(())) => {
                        info!("Successfully toggled loop");
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to toggle loop: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error toggling loop: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for loop toggle");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_loop_toggle = Callback::new(move |_: ()| {
        // No-op for wasm32
    });
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_back = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.navigation_command(NavigationCommand::PreviousSectionOrSong).await {
                    Ok(Ok(())) => {
                        info!("Successfully navigated to previous section/song");
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to navigate back: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error navigating back: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for navigation");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_back = Callback::new(move |_: ()| {
        // No-op for wasm32
    });
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_forward = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.navigation_command(NavigationCommand::NextSectionOrSong).await {
                    Ok(Ok(())) => {
                        info!("Successfully navigated to next section/song");
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to navigate forward: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error navigating forward: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for navigation");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_forward = Callback::new(move |_: ()| {
        // No-op for wasm32
    });
    
    // Keyboard shortcuts
    #[cfg(not(target_arch = "wasm32"))]
    let on_keydown = {
        let on_play_pause = on_play_pause.clone();
        let on_back = on_back.clone();
        let on_forward = on_forward.clone();
        let current_song_index = current_song_index.clone();
        
        Callback::new(move |evt: Event<KeyboardData>| {
            use dioxus::prelude::Key;
            
            match evt.data.key() {
                Key::Character(c) if c == " " => {
                    // Spacebar: Play/Pause
                    on_play_pause.call(());
                }
                Key::ArrowLeft => {
                    // Left arrow: Previous section/song
                    on_back.call(());
                }
                Key::ArrowRight => {
                    // Right arrow: Next section/song
                    on_forward.call(());
                }
                Key::ArrowUp => {
                    // Up arrow: Previous song
                    if let Some(api) = get_setlist_api() {
                        if let Some(current_idx) = current_song_index() {
                            if current_idx > 0 {
                                let prev_song_idx = current_idx - 1;
                                spawn(async move {
                                    match api.seek_to_song(prev_song_idx).await {
                                        Ok(Ok(())) => {
                                            info!("Successfully navigated to previous song {}", prev_song_idx);
                                        }
                                        Ok(Err(e)) => {
                                            warn!("Failed to navigate to previous song: {}", e);
                                        }
                                        Err(e) => {
                                            warn!("RPC error navigating to previous song: {}", e);
                                        }
                                    }
                                });
                            }
                        }
                    }
                }
                Key::ArrowDown => {
                    // Down arrow: Next song
                    if let Some(api) = get_setlist_api() {
                        if let Some(current_idx) = current_song_index() {
                            // Get total song count from SETLIST
                            if let Some(setlist_api) = SETLIST.read().as_ref() {
                                let setlist = setlist_api.get_setlist();
                                let song_count = setlist.songs.len();
                                if current_idx < song_count - 1 {
                                    let next_song_idx = current_idx + 1;
                                    spawn(async move {
                                        match api.seek_to_song(next_song_idx).await {
                                            Ok(Ok(())) => {
                                                info!("Successfully navigated to next song {}", next_song_idx);
                                            }
                                            Ok(Err(e)) => {
                                                warn!("Failed to navigate to next song: {}", e);
                                            }
                                            Err(e) => {
                                                warn!("RPC error navigating to next song: {}", e);
                                            }
                                        }
                                    });
                                }
                            }
                        }
                    }
                }
                _ => {
                    // Ignore other keys
                }
            }
        })
    };
    
    #[cfg(target_arch = "wasm32")]
    let on_keydown = Callback::new(move |_evt: Event<KeyboardData>| {
        // No-op for wasm32
    });

    // Derive current song and section index from SETLIST global signal
    let current_song_index = use_memo(move || {
        SETLIST.read().as_ref().and_then(|api| api.active_song_index())
    });
    
    let current_section_index = use_memo(move || {
        SETLIST.read().as_ref().and_then(|api| api.active_section_index())
    });
    
    // Mode toggle callback - simplified for now
    let on_toggle_mode = Callback::new(move |_: ()| {
        // TODO: Implement mode toggle if needed
    });
    
    // Callbacks
    #[cfg(not(target_arch = "wasm32"))]
    let on_song_click = Callback::new(move |song_idx: usize| {
        // Seek to the clicked song (switches to that song's tab)
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.seek_to_song(song_idx).await {
                    Ok(Ok(())) => {
                        info!("Successfully sought to song {}", song_idx);
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to seek to song: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error seeking to song: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for song seek");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_song_click = Callback::new(move |_idx: usize| {
        // No-op for wasm32
    });
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_section_click = Callback::new(move |(song_idx, section_idx): (usize, usize)| {
        // Seek to the clicked section
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.seek_to_section(song_idx, section_idx).await {
                    Ok(Ok(())) => {
                        info!("Successfully sought to song {} section {}", song_idx, section_idx);
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to seek to section: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error seeking to section: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for section seek");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_section_click = Callback::new(move |(_song_idx, _section_idx): (usize, usize)| {
        // No-op for wasm32
    });
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_play_pause = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.transport_command(TransportCommand::TogglePlayPause).await {
                    Ok(Ok(())) => {
                        info!("Successfully toggled play/pause");
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to toggle play/pause: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error toggling play/pause: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for play/pause");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_play_pause = Callback::new(move |_: ()| {
        // No-op for wasm32
    });
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_loop_toggle = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.toggle_loop().await {
                    Ok(Ok(())) => {
                        info!("Successfully toggled loop");
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to toggle loop: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error toggling loop: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for loop toggle");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_loop_toggle = Callback::new(move |_: ()| {
        // No-op for wasm32
    });
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_back = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.navigation_command(NavigationCommand::PreviousSectionOrSong).await {
                    Ok(Ok(())) => {
                        info!("Successfully navigated to previous section/song");
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to navigate back: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error navigating back: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for navigation");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_back = Callback::new(move |_: ()| {
        // No-op for wasm32
    });
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_forward = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.navigation_command(NavigationCommand::NextSectionOrSong).await {
                    Ok(Ok(())) => {
                        info!("Successfully navigated to next section/song");
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to navigate forward: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error navigating forward: {}", e);
                    }
                }
            } else {
                warn!("Setlist API not available for navigation");
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_forward = Callback::new(move |_: ()| {
        // No-op for wasm32
    });
    
    // Keyboard shortcuts
    #[cfg(not(target_arch = "wasm32"))]
    let on_keydown = {
        let on_play_pause = on_play_pause.clone();
        let on_back = on_back.clone();
        let on_forward = on_forward.clone();
        let current_song_index = current_song_index.clone();
        
        Callback::new(move |evt: Event<KeyboardData>| {
            use dioxus::prelude::Key;
            
            match evt.data.key() {
                Key::Character(c) if c == " " => {
                    // Spacebar: Play/Pause
                    on_play_pause.call(());
                }
                Key::ArrowLeft => {
                    // Left arrow: Previous section/song
                    on_back.call(());
                }
                Key::ArrowRight => {
                    // Right arrow: Next section/song
                    on_forward.call(());
                }
                Key::ArrowUp => {
                    // Up arrow: Previous song
                    if let Some(api) = get_setlist_api() {
                        if let Some(current_idx) = current_song_index() {
                            if current_idx > 0 {
                                let prev_song_idx = current_idx - 1;
                                spawn(async move {
                                    match api.seek_to_song(prev_song_idx).await {
                                        Ok(Ok(())) => {
                                            info!("Successfully navigated to previous song {}", prev_song_idx);
                                        }
                                        Ok(Err(e)) => {
                                            warn!("Failed to navigate to previous song: {}", e);
                                        }
                                        Err(e) => {
                                            warn!("RPC error navigating to previous song: {}", e);
                                        }
                                    }
                                });
                            }
                        }
                    }
                }
                Key::ArrowDown => {
                    // Down arrow: Next song
                    if let Some(api) = get_setlist_api() {
                        if let Some(current_idx) = current_song_index() {
                            // Get total song count from SETLIST
                            if let Some(setlist_api) = SETLIST.read().as_ref() {
                                let setlist = setlist_api.get_setlist();
                                let song_count = setlist.songs.len();
                                if current_idx < song_count - 1 {
                                    let next_song_idx = current_idx + 1;
                                    spawn(async move {
                                        match api.seek_to_song(next_song_idx).await {
                                            Ok(Ok(())) => {
                                                info!("Successfully navigated to next song {}", next_song_idx);
                                            }
                                            Ok(Err(e)) => {
                                                warn!("Failed to navigate to next song: {}", e);
                                            }
                                            Err(e) => {
                                                warn!("RPC error navigating to next song: {}", e);
                                            }
                                        }
                                    });
                                }
                            }
                        }
                    }
                }
                _ => {
                    // Ignore other keys
                }
            }
        })
    };
    
    #[cfg(target_arch = "wasm32")]
    let on_keydown = Callback::new(move |_evt: Event<KeyboardData>| {
        // No-op for wasm32
    });

    rsx! {
        div {
            class: "h-screen w-screen flex flex-col overflow-hidden bg-background",
            tabindex: "0",
            onkeydown: on_keydown,
            TopBar {
                is_connected: is_connected,
                is_server_mode: false,
                on_toggle_mode: Some(on_toggle_mode),
            }
            
            Outlet::<Route> {}
        }
    }
}

#[component]
fn Performance() -> Element {
    // Derive current song and section index from SETLIST global signal
    let current_song_index = use_memo(move || {
        SETLIST.read().as_ref().and_then(|api| api.active_song_index())
    });
    
    let current_section_index = use_memo(move || {
        SETLIST.read().as_ref().and_then(|api| api.active_section_index())
    });
    
    // Callbacks
    #[cfg(not(target_arch = "wasm32"))]
    let on_song_click = Callback::new(move |song_idx: usize| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.seek_to_song(song_idx).await {
                    Ok(Ok(())) => {
                        info!("Successfully sought to song {}", song_idx);
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to seek to song: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error seeking to song: {}", e);
                    }
                }
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_song_click = Callback::new(move |_idx: usize| {});
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_section_click = Callback::new(move |(song_idx, section_idx): (usize, usize)| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                match api.seek_to_section(song_idx, section_idx).await {
                    Ok(Ok(())) => {
                        info!("Successfully sought to song {} section {}", song_idx, section_idx);
                    }
                    Ok(Err(e)) => {
                        warn!("Failed to seek to section: {}", e);
                    }
                    Err(e) => {
                        warn!("RPC error seeking to section: {}", e);
                    }
                }
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_section_click = Callback::new(move |(_song_idx, _section_idx): (usize, usize)| {});
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_play_pause = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                api.transport_command(TransportCommand::TogglePlayPause).await.ok();
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_play_pause = Callback::new(move |_: ()| {});
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_loop_toggle = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                api.toggle_loop().await.ok();
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_loop_toggle = Callback::new(move |_: ()| {});
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_back = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                api.navigation_command(NavigationCommand::PreviousSectionOrSong).await.ok();
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_back = Callback::new(move |_: ()| {});
    
    #[cfg(not(target_arch = "wasm32"))]
    let on_forward = Callback::new(move |_: ()| {
        spawn(async move {
            if let Some(api) = get_setlist_api() {
                api.navigation_command(NavigationCommand::NextSectionOrSong).await.ok();
            }
        });
    });
    
    #[cfg(target_arch = "wasm32")]
    let on_forward = Callback::new(move |_: ()| {});

    rsx! {
            div {
                class: "flex-1 flex overflow-hidden",
                Sidebar {
                    current_song_index: Signal::new(current_song_index()),
                    current_section_index: Signal::new(current_section_index()),
                    is_playing: Signal::new(false), // Will be derived from SETLIST in component
                    on_song_click: on_song_click,
                    on_section_click: on_section_click,
                }
                
                MainContent {
                    current_song_index: Signal::new(current_song_index()),
                    current_section_index: Signal::new(current_section_index()),
                    on_section_click: Some(Callback::new(move |idx: usize| {
                        if let Some(song_idx) = current_song_index() {
                            on_section_click.call((song_idx, idx));
                        }
                    })),
                is_playing: Signal::new(false),
                is_looping: Signal::new(false),
                    on_play_pause: on_play_pause,
                    on_loop_toggle: on_loop_toggle,
                    on_back: on_back,
                    on_forward: on_forward,
                }
            }
        }
}

#[component]
fn Lyrics() -> Element {
    rsx! {
        LyricsView {}
    }
}

/// Create sample setlist with real Setlist data
fn create_sample_setlist() -> Setlist {
    let mut setlist = Setlist::new("Sample Setlist".to_string()).unwrap();
    let measure_duration = 2.0; // 2 seconds per measure at 120 BPM
    
    // Song 1: "Midnight Dreams" - Complex structure with bridge
    let mut song1 = Song::new("Midnight Dreams".to_string()).unwrap();
    song1.metadata.insert("project_name".to_string(), "Project A".to_string());
    
    // Set song color (blue: rgb(59, 130, 246))
    let song1_color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    let song1_start_marker = Marker::new_full(
        None,
        Position::from_seconds(0.0),
        "SONGSTART".to_string(),
        song1_color,
        None,
        None,
        None,
    );
    song1.set_start_marker(song1_start_marker);
    let song1_region_marker = Marker::new_full(
        None,
        Position::from_seconds(0.0),
        "Midnight Dreams_REGION_START".to_string(),
        song1_color,
        None,
        None,
        None,
    );
    song1.set_song_region_start_marker(song1_region_marker);
    
    let mut current_time = 0.0;
    // Intro - blue
    let mut intro = Section::from_seconds(SectionType::Intro, current_time, current_time + 4.0 * measure_duration, "Intro".to_string(), None).unwrap();
    intro.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song1.sections.push(intro);
    current_time += 4.0 * measure_duration;
    // Verse 1 - green
    let mut verse1 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 1".to_string(), Some(1)).unwrap();
    verse1.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song1.sections.push(verse1);
    current_time += 8.0 * measure_duration;
    // Chorus 1 - purple
    let mut chorus1 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 1".to_string(), Some(1)).unwrap();
    chorus1.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song1.sections.push(chorus1);
    current_time += 8.0 * measure_duration;
    // Verse 2 - green
    let mut verse2 = Section::from_seconds(SectionType::Verse, current_time, current_time + 4.0 * measure_duration, "Verse 2".to_string(), Some(2)).unwrap();
    verse2.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song1.sections.push(verse2);
    current_time += 4.0 * measure_duration;
    // Chorus 2 - purple
    let mut chorus2 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 6.0 * measure_duration, "Chorus 2".to_string(), Some(2)).unwrap();
    chorus2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song1.sections.push(chorus2);
    current_time += 6.0 * measure_duration;
    // Bridge - red
    let mut bridge = Section::from_seconds(SectionType::Bridge, current_time, current_time + 8.0 * measure_duration, "Bridge".to_string(), None).unwrap();
    bridge.color = Some((239u32 << 16) | (68u32 << 8) | 68u32);
    song1.sections.push(bridge);
    current_time += 8.0 * measure_duration;
    // Chorus 3 - purple
    let mut chorus3 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 3".to_string(), Some(3)).unwrap();
    chorus3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song1.sections.push(chorus3);
    current_time += 8.0 * measure_duration;
    // Outro - blue
    let mut outro = Section::from_seconds(SectionType::Outro, current_time, current_time + 8.0 * measure_duration, "Outro".to_string(), None).unwrap();
    outro.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song1.sections.push(outro);
    
    // Add tempo and time signature changes for Song 1
    song1.tempo_time_sig_changes.push(TempoTimePoint::new_full(
        0.0, 120.0, None, Some((4, 4)), None, None, None,
    ));
    song1.tempo_time_sig_changes.push(TempoTimePoint::new_full(
        36.0, 120.0, None, Some((6, 8)), None, None, None,
    ));
    song1.tempo_time_sig_changes.push(TempoTimePoint::new_full(
        60.0, 100.0, None, Some((4, 4)), None, None, None,
    ));
    song1.tempo_time_sig_changes.push(TempoTimePoint::new_full(
        76.0, 140.0, None, None, None, None, None,
    ));
    
    setlist.add_song(song1).unwrap();
    
    // Song 2: "Electric Nights" - Simple verse-chorus structure (same project as Song 1)
    let mut song2 = Song::new("Electric Nights".to_string()).unwrap();
    song2.metadata.insert("project_name".to_string(), "Project A".to_string());
    
    let song2_color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    let song2_start_marker = Marker::new_full(
        None,
        Position::from_seconds(100.0),
        "SONGSTART".to_string(),
        song2_color,
        None,
        None,
        None,
    );
    song2.set_start_marker(song2_start_marker);
    let song2_region_marker = Marker::new_full(
        None,
        Position::from_seconds(100.0),
        "Electric Nights_REGION_START".to_string(),
        song2_color,
        None,
        None,
        None,
    );
    song2.set_song_region_start_marker(song2_region_marker);
    
    current_time = 100.0;
    let mut intro2 = Section::from_seconds(SectionType::Intro, current_time, current_time + 2.0 * measure_duration, "Intro".to_string(), None).unwrap();
    intro2.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song2.sections.push(intro2);
    current_time += 2.0 * measure_duration;
    let mut verse1_2 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 1".to_string(), Some(1)).unwrap();
    verse1_2.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song2.sections.push(verse1_2);
    current_time += 8.0 * measure_duration;
    let mut chorus1_2 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 1".to_string(), Some(1)).unwrap();
    chorus1_2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song2.sections.push(chorus1_2);
    current_time += 8.0 * measure_duration;
    let mut verse2_2 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 2".to_string(), Some(2)).unwrap();
    verse2_2.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song2.sections.push(verse2_2);
    current_time += 8.0 * measure_duration;
    let mut chorus2_2 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 2".to_string(), Some(2)).unwrap();
    chorus2_2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song2.sections.push(chorus2_2);
    current_time += 8.0 * measure_duration;
    let mut chorus3_2 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 3".to_string(), Some(3)).unwrap();
    chorus3_2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song2.sections.push(chorus3_2);
    current_time += 8.0 * measure_duration;
    let mut outro2 = Section::from_seconds(SectionType::Outro, current_time, current_time + 4.0 * measure_duration, "Outro".to_string(), None).unwrap();
    outro2.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song2.sections.push(outro2);
    
    song2.tempo_time_sig_changes.push(TempoTimePoint::new_full(0.0, 120.0, None, Some((4, 4)), None, None, None));
    song2.tempo_time_sig_changes.push(TempoTimePoint::new_full(8.0, 130.0, None, None, None, None, None));
    song2.tempo_time_sig_changes.push(TempoTimePoint::new_full(24.0, 130.0, None, Some((3, 4)), None, None, None));
    song2.tempo_time_sig_changes.push(TempoTimePoint::new_full(40.0, 130.0, None, Some((5, 4)), None, None, None));
    song2.tempo_time_sig_changes.push(TempoTimePoint::new_full(56.0, 130.0, None, Some((4, 4)), None, None, None));
    
    setlist.add_song(song2).unwrap();
    
    // Song 3: "City Lights" - Verse-heavy with instrumental break (different project)
    let mut song3 = Song::new("City Lights".to_string()).unwrap();
    song3.metadata.insert("project_name".to_string(), "Project B".to_string());
    
    let song3_color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    let song3_start_marker = Marker::new_full(
        None,
        Position::from_seconds(0.0),
        "SONGSTART".to_string(),
        song3_color,
        None,
        None,
        None,
    );
    song3.set_start_marker(song3_start_marker);
    let song3_region_marker = Marker::new_full(
        None,
        Position::from_seconds(0.0),
        "City Lights_REGION_START".to_string(),
        song3_color,
        None,
        None,
        None,
    );
    song3.set_song_region_start_marker(song3_region_marker);
    
    current_time = 0.0;
    let mut intro3 = Section::from_seconds(SectionType::Intro, current_time, current_time + 6.0 * measure_duration, "Intro".to_string(), None).unwrap();
    intro3.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song3.sections.push(intro3);
    current_time += 6.0 * measure_duration;
    let mut verse1_3 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 1".to_string(), Some(1)).unwrap();
    verse1_3.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song3.sections.push(verse1_3);
    current_time += 8.0 * measure_duration;
    let mut chorus1_3 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 1".to_string(), Some(1)).unwrap();
    chorus1_3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song3.sections.push(chorus1_3);
    current_time += 8.0 * measure_duration;
    let mut verse2_3 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 2".to_string(), Some(2)).unwrap();
    verse2_3.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song3.sections.push(verse2_3);
    current_time += 8.0 * measure_duration;
    let mut chorus2_3 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 2".to_string(), Some(2)).unwrap();
    chorus2_3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song3.sections.push(chorus2_3);
    current_time += 8.0 * measure_duration;
    let mut instrumental = Section::from_seconds(SectionType::Instrumental, current_time, current_time + 12.0 * measure_duration, "Instrumental".to_string(), None).unwrap();
    instrumental.color = Some((251u32 << 16) | (191u32 << 8) | 36u32);
    song3.sections.push(instrumental);
    current_time += 12.0 * measure_duration;
    let mut verse3_3 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 3".to_string(), Some(3)).unwrap();
    verse3_3.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song3.sections.push(verse3_3);
    current_time += 8.0 * measure_duration;
    let mut chorus3_3 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 3".to_string(), Some(3)).unwrap();
    chorus3_3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song3.sections.push(chorus3_3);
    current_time += 8.0 * measure_duration;
    let mut outro3 = Section::from_seconds(SectionType::Outro, current_time, current_time + 6.0 * measure_duration, "Outro".to_string(), None).unwrap();
    outro3.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song3.sections.push(outro3);
    
    song3.tempo_time_sig_changes.push(TempoTimePoint::new_full(0.0, 120.0, None, Some((4, 4)), None, None, None));
    song3.tempo_time_sig_changes.push(TempoTimePoint::new_full(28.0, 120.0, None, Some((3, 4)), None, None, None));
    song3.tempo_time_sig_changes.push(TempoTimePoint::new_full(44.0, 120.0, None, Some((7, 8)), None, None, None));
    song3.tempo_time_sig_changes.push(TempoTimePoint::new_full(68.0, 140.0, None, Some((4, 4)), None, None, None));
    
    setlist.add_song(song3).unwrap();
    
    setlist
}
