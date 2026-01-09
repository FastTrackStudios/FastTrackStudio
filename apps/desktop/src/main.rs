use daw::marker_region::{Marker, application::TempoTimePoint};
use daw::primitives::Position;
use dioxus::prelude::*;
use dioxus_router::{Outlet, Routable, Router};
use fts::setlist::{
    ACTIVE_INDICES, NavigationCommand, SETLIST, SETLIST_STRUCTURE, SONG_TRACKS, SONG_TRANSPORT,
    Section, SectionType, Setlist, Song, TransportCommand,
};
use ui::components::chord_tab::ChordTabView;
use ui::components::chords::ChordsView;
use ui::components::*;
// REAPER connection via REAPER_ALPN is disabled - all data comes through setlist stream
// #[cfg(not(target_arch = "wasm32"))]
// use crate::reaper_connection::ReaperConnection;
// #[cfg(not(target_arch = "wasm32"))]
// use peer_2_peer::reaper_api::ReaperStateUpdate;
#[cfg(not(target_arch = "wasm32"))]
use crate::setlist_commands::{
    seek_to_section, seek_to_song, seek_to_time, send_navigation_command, send_transport_command,
    toggle_loop,
};
use tracing::{info, warn};

#[cfg(not(target_arch = "wasm32"))]
mod chart_connection;
#[cfg(not(target_arch = "wasm32"))]
mod iroh_connection_manager;
#[cfg(not(target_arch = "wasm32"))]
mod reaper_connection;
#[cfg(not(target_arch = "wasm32"))]
mod setlist_commands;
#[cfg(not(target_arch = "wasm32"))]
mod setlist_connection;
#[cfg(not(target_arch = "wasm32"))]
mod track_commands;
#[cfg(not(target_arch = "wasm32"))]
mod tracks_connection;
mod utils;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    // Initialize tracing subscriber for logging (only for desktop builds)
    #[cfg(feature = "desktop")]
    #[cfg(not(target_arch = "wasm32"))]
    {
        use tracing_subscriber::EnvFilter;
        use tracing_subscriber::prelude::*;

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
    #[route("/lyrics/edit")]
    LyricsEdit {},
    #[route("/lyrics/performance")]
    LyricsPerformance {},
    #[route("/arrangement")]
    Arrangement {},
    #[route("/chords")]
    Chords {},
    #[route("/chart")]
    Chart {},
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

    // Global edit mode
    let mut edit_mode = use_signal(|| false);

    // Edit view mode for context-specific navigation (e.g., Slides/Sync in lyrics edit)
    let mut edit_view_mode = use_signal(|| None::<ui::components::layout::EditViewMode>);

    // Get current route path
    let current_route = use_route::<Route>();
    let route_clone = current_route.clone();
    let current_path = use_memo(move || match route_clone.clone() {
        Route::Performance {} => Some("/".to_string()),
        Route::Lyrics {} => Some("/lyrics".to_string()),
        Route::LyricsEdit {} => Some("/lyrics/edit".to_string()),
        Route::LyricsPerformance {} => Some("/lyrics/performance".to_string()),
        Route::Arrangement {} => Some("/arrangement".to_string()),
        Route::Chords {} => Some("/chords".to_string()),
        Route::Chart {} => Some("/chart".to_string()),
    });

    // Sync edit mode with route
    let route_for_effect = current_route.clone();
    use_effect(move || match route_for_effect.clone() {
        Route::LyricsEdit {} => {
            if !edit_mode() {
                edit_mode.set(true);
            }
        }
        Route::Lyrics {} => {
            if edit_mode() {
                edit_mode.set(false);
            }
        }
        _ => {}
    });

    #[cfg(not(target_arch = "wasm32"))]
    {
        use_effect({
            let mut is_connected = is_connected;
            move || {
                spawn(async move {
                    // Initialize connections first
                    if let Err(e) = iroh_connection_manager::init_shared_endpoint().await {
                        tracing::warn!("Failed to initialize shared IROH endpoint: {}", e);
                        return;
                    }

                    if let Err(e) = setlist_connection::connect_to_reaper_setlist().await {
                        tracing::warn!("Failed to initialize REAPER setlist connection: {}", e);
                        return;
                    }

                    if let Err(e) = tracks_connection::connect_to_reaper_tracks().await {
                        tracing::warn!("Failed to initialize REAPER tracks connection: {}", e);
                        return;
                    }

                    if let Err(e) = chart_connection::connect_to_reaper_chart().await {
                        tracing::warn!("Failed to initialize REAPER chart connection: {}", e);
                        return;
                    }

                    // Wait a bit for the connection status channel to be initialized
                    tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;

                    // Get connection status receiver
                    let mut connection_rx =
                        match setlist_connection::get_connection_status_receiver() {
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
                            // Transport state is now embedded in each song's project.transport() via SETLIST
                            // No need to update a separate TRANSPORT_INFO signal
                        }
                    }
                }
            });
        });
    }
    */

    // Derive current song and section index from ACTIVE_INDICES granular signal
    // This only rerenders when active indices change, not when tracks/transport change
    let current_song_index = use_memo(move || ACTIVE_INDICES.read().0);

    let current_section_index = use_memo(move || ACTIVE_INDICES.read().1);

    // Get setlist structure (songs, sections) - only rerenders when structure changes
    let setlist_structure = use_memo(move || SETLIST_STRUCTURE.read().clone());

    // Mode toggle callback - simplified for now
    let on_toggle_mode = Callback::new(move |_: ()| {
        // TODO: Implement mode toggle if needed
    });

    // Callbacks
    #[cfg(not(target_arch = "wasm32"))]
    let on_song_click = Callback::new(move |song_idx: usize| {
        // Seek to the clicked song (switches to that song's tab)
        spawn(async move {
            if let Err(e) = seek_to_song(song_idx).await {
                warn!("Failed to seek to song {}: {}", song_idx, e);
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
            if let Err(e) = seek_to_section(song_idx, section_idx).await {
                warn!(
                    "Failed to seek to song {} section {}: {}",
                    song_idx, section_idx, e
                );
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
            if let Err(e) = send_transport_command(TransportCommand::TogglePlayPause).await {
                warn!("Failed to toggle play/pause: {}", e);
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
            if let Err(e) = toggle_loop().await {
                warn!("Failed to toggle loop: {}", e);
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
            if let Err(e) = send_navigation_command(NavigationCommand::PreviousSectionOrSong).await
            {
                warn!("Failed to navigate back: {}", e);
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
            if let Err(e) = send_navigation_command(NavigationCommand::NextSectionOrSong).await {
                warn!("Failed to navigate forward: {}", e);
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
                    if let Some(current_idx) = current_song_index() {
                        if current_idx > 0 {
                            let prev_song_idx = current_idx - 1;
                            spawn(async move {
                                if let Err(e) = seek_to_song(prev_song_idx).await {
                                    warn!(
                                        "Failed to navigate to previous song {}: {}",
                                        prev_song_idx, e
                                    );
                                }
                            });
                        }
                    }
                }
                Key::ArrowDown => {
                    // Down arrow: Next song
                    if let Some(current_idx) = current_song_index() {
                        // Get total song count from SETLIST_STRUCTURE
                        if let Some(setlist) = SETLIST_STRUCTURE.read().as_ref() {
                            let song_count = setlist.songs.len();
                            if current_idx < song_count - 1 {
                                let next_song_idx = current_idx + 1;
                                spawn(async move {
                                    if let Err(e) = seek_to_song(next_song_idx).await {
                                        warn!(
                                            "Failed to navigate to next song {}: {}",
                                            next_song_idx, e
                                        );
                                    }
                                });
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

    // Callbacks (duplicate definitions removed - using ones defined earlier)
    #[cfg(not(target_arch = "wasm32"))]
    let on_song_click = Callback::new(move |song_idx: usize| {
        // Seek to the clicked song (switches to that song's tab)
        spawn(async move {
            if let Err(e) = seek_to_song(song_idx).await {
                warn!("Failed to seek to song {}: {}", song_idx, e);
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
            if let Err(e) = seek_to_section(song_idx, section_idx).await {
                warn!(
                    "Failed to seek to song {} section {}: {}",
                    song_idx, section_idx, e
                );
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
            if let Err(e) = send_transport_command(TransportCommand::TogglePlayPause).await {
                warn!("Failed to toggle play/pause: {}", e);
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
            if let Err(e) = toggle_loop().await {
                warn!("Failed to toggle loop: {}", e);
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
            if let Err(e) = send_navigation_command(NavigationCommand::PreviousSectionOrSong).await
            {
                warn!("Failed to navigate back: {}", e);
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
            if let Err(e) = send_navigation_command(NavigationCommand::NextSectionOrSong).await {
                warn!("Failed to navigate forward: {}", e);
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
                    if let Some(current_idx) = current_song_index() {
                        if current_idx > 0 {
                            let prev_song_idx = current_idx - 1;
                            spawn(async move {
                                if let Err(e) = seek_to_song(prev_song_idx).await {
                                    warn!(
                                        "Failed to navigate to previous song {}: {}",
                                        prev_song_idx, e
                                    );
                                }
                            });
                        }
                    }
                }
                Key::ArrowDown => {
                    // Down arrow: Next song
                    if let Some(current_idx) = current_song_index() {
                        // Get total song count from SETLIST_STRUCTURE
                        if let Some(setlist) = SETLIST_STRUCTURE.read().as_ref() {
                            let song_count = setlist.songs.len();
                            if current_idx < song_count - 1 {
                                let next_song_idx = current_idx + 1;
                                spawn(async move {
                                    if let Err(e) = seek_to_song(next_song_idx).await {
                                        warn!(
                                            "Failed to navigate to next song {}: {}",
                                            next_song_idx, e
                                        );
                                    }
                                });
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
                edit_mode: edit_mode,
                on_toggle_edit: {
                    let mut edit_mode = edit_mode;
                    let navigator = dioxus_router::use_navigator();
                    let route_for_toggle = current_route.clone();
                    Callback::new(move |_| {
                        let new_edit_mode = !edit_mode();
                        edit_mode.set(new_edit_mode);

                        // Navigate based on edit mode and current route
                        match route_for_toggle.clone() {
                            Route::Lyrics {} if new_edit_mode => {
                                navigator.push(Route::LyricsEdit {});
                            }
                            Route::LyricsEdit {} if !new_edit_mode => {
                                navigator.push(Route::Lyrics {});
                            }
                            _ => {}
                        }
                    })
                },
                edit_view_mode: Some(edit_view_mode),
                on_edit_view_change: Some(Callback::new(move |mode| {
                    edit_view_mode.set(Some(mode));
                })),
                current_route_path: current_path(),
            }

            // Provide edit mode context to child routes
            EditModeContext {
                edit_mode: edit_mode,
                edit_view_mode: edit_view_mode,
                on_edit_view_change: {
                    let mut edit_view_mode = edit_view_mode;
                    Callback::new(move |mode| {
                        edit_view_mode.set(Some(mode));
                    })
                },
                Outlet::<Route> {}
            }
        }
    }
}

/// Context provider for edit mode state
#[component]
fn EditModeContext(
    edit_mode: Signal<bool>,
    edit_view_mode: Signal<Option<ui::components::layout::EditViewMode>>,
    on_edit_view_change: Callback<ui::components::layout::EditViewMode>,
    children: Element,
) -> Element {
    use_context_provider(move || ui::components::lyrics::EditModeCtx {
        edit_mode,
        edit_view_mode,
        on_edit_view_change,
    });

    rsx! {
        {children}
    }
}

#[component]
fn Performance() -> Element {
    // Derive current song and section index from ACTIVE_INDICES granular signal
    // This only rerenders when active indices change, not when tracks/transport change
    let current_song_index = use_memo(move || ACTIVE_INDICES.read().0);

    let current_section_index = use_memo(move || ACTIVE_INDICES.read().1);

    // Create reactive signals that sync with the memos
    // This ensures the signals update when ACTIVE_INDICES changes
    //
    #[allow(clippy::redundant_closure)]
    let mut reactive_song_index = use_signal(|| current_song_index());
    #[allow(clippy::redundant_closure)]
    let mut reactive_section_index = use_signal(|| current_section_index());

    // Sync signals with memos when they change
    use_effect(move || {
        let new_song_idx = current_song_index();
        let new_section_idx = current_section_index();
        reactive_song_index.set(new_song_idx);
        reactive_section_index.set(new_section_idx);
    });

    // Callbacks
    #[cfg(not(target_arch = "wasm32"))]
    let on_song_click = Callback::new(move |song_idx: usize| {
        spawn(async move {
            if let Err(e) = seek_to_song(song_idx).await {
                warn!("Failed to seek to song {}: {}", song_idx, e);
            }
        });
    });

    #[cfg(target_arch = "wasm32")]
    let on_song_click = Callback::new(move |_idx: usize| {});

    #[cfg(not(target_arch = "wasm32"))]
    let on_section_click = Callback::new(move |(song_idx, section_idx): (usize, usize)| {
        spawn(async move {
            if let Err(e) = seek_to_section(song_idx, section_idx).await {
                warn!(
                    "Failed to seek to song {} section {}: {}",
                    song_idx, section_idx, e
                );
            }
        });
    });

    #[cfg(target_arch = "wasm32")]
    let on_section_click = Callback::new(move |(_song_idx, _section_idx): (usize, usize)| {});

    #[cfg(not(target_arch = "wasm32"))]
    let on_play_pause = Callback::new(move |_: ()| {
        spawn(async move {
            send_transport_command(TransportCommand::TogglePlayPause)
                .await
                .ok();
        });
    });

    #[cfg(target_arch = "wasm32")]
    let on_play_pause = Callback::new(move |_: ()| {});

    #[cfg(not(target_arch = "wasm32"))]
    let on_loop_toggle = Callback::new(move |_: ()| {
        spawn(async move {
            toggle_loop().await.ok();
        });
    });

    #[cfg(target_arch = "wasm32")]
    let on_loop_toggle = Callback::new(move |_: ()| {});

    #[cfg(not(target_arch = "wasm32"))]
    let on_back = Callback::new(move |_: ()| {
        spawn(async move {
            send_navigation_command(NavigationCommand::PreviousSectionOrSong)
                .await
                .ok();
        });
    });

    #[cfg(target_arch = "wasm32")]
    let on_back = Callback::new(move |_: ()| {});

    #[cfg(not(target_arch = "wasm32"))]
    let on_forward = Callback::new(move |_: ()| {
        spawn(async move {
            send_navigation_command(NavigationCommand::NextSectionOrSong)
                .await
                .ok();
        });
    });

    #[cfg(target_arch = "wasm32")]
    let on_forward = Callback::new(move |_: ()| {});

    #[cfg(not(target_arch = "wasm32"))]
    let on_seek_to_time = Callback::new(move |(song_idx, time_seconds): (usize, f64)| {
        spawn(async move {
            if let Err(e) = seek_to_time(song_idx, time_seconds).await {
                warn!(
                    "Failed to seek to song {} at time {}s: {}",
                    song_idx, time_seconds, e
                );
            }
        });
    });

    #[cfg(target_arch = "wasm32")]
    let on_seek_to_time = Callback::new(move |(_song_idx, _time_seconds): (usize, f64)| {});

    #[cfg(not(target_arch = "wasm32"))]
    let on_seek_to_musical_position = {
        use crate::setlist_commands::seek_to_musical_position;
        Callback::new(
            move |(song_idx, musical_pos): (usize, daw::primitives::MusicalPosition)| {
                let musical_pos_for_log = musical_pos.clone();
                spawn(async move {
                    if let Err(e) = seek_to_musical_position(song_idx, musical_pos).await {
                        warn!(
                            "Failed to seek to song {} at musical position {}: {}",
                            song_idx, musical_pos_for_log, e
                        );
                    }
                });
            },
        )
    };

    #[cfg(target_arch = "wasm32")]
    let on_seek_to_musical_position = Callback::new(
        move |(_song_idx, _musical_pos): (usize, daw::primitives::MusicalPosition)| {},
    );

    rsx! {
        div {
            class: "flex-1 flex overflow-hidden",
            Sidebar {
                current_song_index: reactive_song_index,
                current_section_index: reactive_section_index,
                is_playing: Signal::new(false), // Will be derived from SETLIST in component
                on_song_click: on_song_click,
                on_section_click: on_section_click,
            }

            MainContent {
                current_song_index: reactive_song_index,
                current_section_index: reactive_section_index,
                on_section_click: Some(Callback::new(move |idx: usize| {
                    if let Some(song_idx) = reactive_song_index() {
                        on_section_click.call((song_idx, idx));
                    }
                })),
            is_playing: Signal::new(false),
            is_looping: Signal::new(false),
                on_play_pause: on_play_pause,
                on_loop_toggle: on_loop_toggle,
                on_back: on_back,
                on_forward: on_forward,
                on_seek_to_time: Some(on_seek_to_time),
                on_seek_to_musical_position: Some(on_seek_to_musical_position),
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

#[component]
fn LyricsEdit() -> Element {
    rsx! {
        LyricsEditView {}
    }
}

#[component]
fn LyricsPerformance() -> Element {
    rsx! {
        PerformancePreview {}
    }
}

#[component]
fn Chords() -> Element {
    rsx! {
        ChordTabView {
            chart: None, // Uses test chart data
        }
    }
}

#[component]
fn Chart() -> Element {
    use pdf::{PdfPageData, RotationAngle, get_pdf_info, render_pdf_page_with_text};
    use std::collections::HashMap;
    use std::path::PathBuf;

    // PDF file path state management
    let mut pdf_path = use_signal(|| -> Option<PathBuf> { None });

    #[allow(clippy::redundant_closure)]
    let mut page_cache = use_signal(|| HashMap::<usize, PdfPageData>::new());

    #[allow(clippy::redundant_closure)]
    let mut is_loading = use_signal(|| false);
    #[allow(clippy::redundant_closure)]
    let mut error_message = use_signal(|| String::new());

    #[allow(clippy::redundant_closure)]
    let mut loaded_pdf_path = use_signal(|| -> Option<PathBuf> { None });
    #[allow(clippy::redundant_closure)]
    let mut page_rotations = use_signal(|| HashMap::<usize, RotationAngle>::new());

    // PDF file information
    let (total_pages, pdf_info) = use_memo(move || {
        if let Some(path) = pdf_path() {
            match get_pdf_info(&path.to_string_lossy()) {
                Ok((pages, title)) => {
                    tracing::info!(
                        "Successfully loaded PDF: {} ({} pages)",
                        path.display(),
                        pages
                    );
                    (pages, title)
                }
                Err(e) => {
                    let error_msg = format!("Failed to load PDF: {}", e);
                    tracing::error!("{}", error_msg);
                    error_message.set(error_msg.clone());
                    (0, error_msg)
                }
            }
        } else {
            (0, "No PDF file selected".to_string())
        }
    })();

    // PDF loading effect
    use_effect(move || {
        if let Some(path) = pdf_path() {
            let should_load = loaded_pdf_path().as_ref() != Some(&path);

            if total_pages > 0 && !is_loading() && should_load {
                is_loading.set(true);
                page_cache.write().clear();
                error_message.set(String::new());

                let rotations = page_rotations();
                let path_clone = path.clone();

                spawn(async move {
                    tracing::info!("Starting to load PDF pages from: {}", path_clone.display());

                    // Load first 3 pages with priority
                    for page_idx in 0..3.min(total_pages) {
                        let rotation = rotations
                            .get(&page_idx)
                            .copied()
                            .unwrap_or(RotationAngle::None);
                        match render_pdf_page_with_text(
                            &path_clone.to_string_lossy(),
                            page_idx,
                            rotation,
                        ) {
                            Ok(page_data) => {
                                if page_data.page_index != page_idx {
                                    tracing::error!(
                                        "CRITICAL: Page data contamination detected! Expected page {}, got page {}",
                                        page_idx,
                                        page_data.page_index
                                    );
                                    continue;
                                }
                                tracing::debug!("Successfully loaded page {}", page_idx);
                                page_cache.write().insert(page_idx, page_data);
                            }
                            Err(e) => {
                                let error_msg =
                                    format!("Failed to render page {}: {}", page_idx, e);
                                tracing::error!("{}", error_msg);
                                error_message.set(error_msg);
                            }
                        }
                    }

                    // Load remaining pages in parallel
                    let chunk_size = std::thread::available_parallelism()
                        .map(|n| n.get())
                        .unwrap_or(4)
                        .min(8);
                    for chunk_start in (3..total_pages).step_by(chunk_size) {
                        let chunk_end = (chunk_start + chunk_size).min(total_pages);
                        let batch_futures: Vec<_> = (chunk_start..chunk_end)
                            .map(|page_idx| {
                                let path_clone = path_clone.clone();
                                let rotation = rotations
                                    .get(&page_idx)
                                    .copied()
                                    .unwrap_or(RotationAngle::None);
                                Box::pin(async move {
                                    match render_pdf_page_with_text(
                                        &path_clone.to_string_lossy(),
                                        page_idx,
                                        rotation,
                                    ) {
                                        Ok(data) => Some((page_idx, data)),
                                        Err(e) => {
                                            tracing::warn!(
                                                "Failed to render page {}: {}",
                                                page_idx,
                                                e
                                            );
                                            None
                                        }
                                    }
                                })
                            })
                            .collect();

                        let results = futures::future::join_all(batch_futures).await;
                        for (page_idx, page_data) in results.into_iter().flatten() {
                            if page_data.page_index != page_idx {
                                tracing::error!(
                                    "CRITICAL: Page data contamination detected! Expected page {}, got page {}",
                                    page_idx,
                                    page_data.page_index
                                );
                                continue;
                            }
                            tracing::debug!("Successfully loaded page {}", page_idx);
                            page_cache.write().insert(page_idx, page_data);
                        }

                        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
                    }

                    is_loading.set(false);
                    loaded_pdf_path.set(Some(path_clone));
                    tracing::info!(
                        "Finished loading PDF pages. Total loaded: {}",
                        page_cache.read().len()
                    );
                });
            }
        }
    });

    // Get rendered pages list
    let rendered_pages = use_memo(move || {
        let cache = page_cache();
        let mut pages = Vec::new();
        tracing::debug!(
            "Rendered pages memo: total_pages={}, cache_size={}",
            total_pages,
            cache.len()
        );
        for page_idx in 0..total_pages {
            if let Some(page_data) = cache.get(&page_idx) {
                // Contamination check: verify page index matches
                if page_data.page_index != page_idx {
                    tracing::warn!(
                        "Page data mismatch detected! Expected page {}, got page {}",
                        page_idx,
                        page_data.page_index
                    );
                    continue;
                }
                pages.push((page_idx, page_data.clone()));
            }
        }
        tracing::debug!("Rendered pages memo: returning {} pages", pages.len());
        pages
    });

    rsx! {
        div {
            class: "flex-1 flex flex-col overflow-hidden bg-background",
            div {
                class: "p-4 border-b border-border bg-card",
                div {
                    class: "flex items-center gap-4",
                    button {
                        class: "px-4 py-2 rounded-md bg-primary text-primary-foreground hover:bg-primary/90",
                        onclick: move |_| {
                            spawn(async move {
                                if let Some(file_handle) = rfd::AsyncFileDialog::new()
                                    .add_filter("PDF files", &["pdf"])
                                    .set_title("Select PDF file")
                                    .pick_file()
                                    .await
                                {
                                    let selected_path = file_handle.path().to_path_buf();
                                    tracing::info!("User selected PDF file: {}", selected_path.display());
                                    pdf_path.set(Some(selected_path));
                                    page_cache.write().clear();
                                    loaded_pdf_path.set(None);
                                    is_loading.set(false);
                                    error_message.set(String::new());
                                } else {
                                    tracing::debug!("User cancelled PDF file selection");
                                }
                            });
                        },
                        "📁 Open PDF"
                    }
                    if pdf_path().is_some() {
                        button {
                            class: "px-4 py-2 rounded-md bg-destructive text-destructive-foreground hover:bg-destructive/90",
                            onclick: move |_| {
                                pdf_path.set(None);
                                page_cache.write().clear();
                                loaded_pdf_path.set(None);
                                is_loading.set(false);
                                error_message.set(String::new());
                            },
                            "❌ Close"
                        }
                    }
                    if pdf_path().is_some() && total_pages > 0 {
                        div {
                            class: "text-sm text-muted-foreground",
                            if is_loading() {
                                "Loading {total_pages} pages... ({rendered_pages().len()}/{total_pages})"
                            } else {
                                "Loaded {total_pages} pages"
                            }
                        }
                    }
                }
            }

            if !error_message().is_empty() {
                div {
                    class: "p-4 bg-destructive/10 text-destructive",
                    "{error_message()}"
                }
            }

            if pdf_path().is_none() {
                div {
                    class: "flex-1 flex items-center justify-center",
                    div {
                        class: "text-center text-muted-foreground",
                        h2 { class: "text-2xl font-bold mb-2", "PDF Viewer" }
                        p { "Click 'Open PDF' to select a PDF file to view." }
                    }
                }
            } else if total_pages == 0 {
                div {
                    class: "flex-1 flex items-center justify-center",
                    div {
                        class: "text-center",
                        div {
                            class: "p-4 bg-destructive/10 text-destructive rounded-md mb-4 max-w-2xl",
                            h3 { class: "font-bold mb-2", "Failed to Load PDF" }
                            p { "{pdf_info}" }
                            if !error_message().is_empty() {
                                div {
                                    class: "mt-2 text-sm",
                                    "Details: {error_message()}"
                                }
                            }
                        }
                    }
                }
            } else {
                div {
                    class: "flex-1 flex flex-col overflow-hidden bg-background",
                    div {
                        class: "flex-1 overflow-y-auto overflow-x-hidden p-4 bg-background",
                        style: "gap: 20px; min-height: 100%;",
                        // Debug: show cache status
                        if rendered_pages().is_empty() && !page_cache.read().is_empty() {
                            div {
                                class: "text-center py-8 text-muted-foreground",
                                "Pages in cache: {page_cache.read().len()}, Total pages: {total_pages}, Rendered: {rendered_pages().len()}"
                            }
                        }
                        for (page_idx, page_data) in rendered_pages() {
                            div {
                                key: "{page_idx}",
                                class: "flex flex-col items-center mb-8",
                                div {
                                    class: "flex items-center justify-center gap-2 mb-2",
                                    span {
                                        class: "font-semibold text-foreground",
                                        "Page {page_idx + 1}"
                                    }
                                    button {
                                        class: "px-2 py-1 rounded text-xs bg-secondary hover:bg-secondary/80",
                                        onclick: {
                                            move |_| {
                                                if let Some(path) = pdf_path() {
                                                    let current_rotation = page_rotations().get(&page_idx).copied().unwrap_or(RotationAngle::None);
                                                    let new_rotation = current_rotation.next();

                                                    page_rotations.write().insert(page_idx, new_rotation);

                                                    let path_clone = path.clone();
                                                    spawn(async move {
                                                        if let Ok(page_data) = render_pdf_page_with_text(&path_clone.to_string_lossy(), page_idx, new_rotation) {
                                                            page_cache.write().insert(page_idx, page_data);
                                                        }
                                                    });
                                                }
                                            }
                                        },
                                        "🔄 Rotate"
                                    }
                                }
                                div {
                                    class: "relative",
                                    style: "position: relative; display: block; width: 100%; max-width: 800px; margin-bottom: 20px; isolation: isolate;",
                                    img {
                                        src: "{page_data.image_data}",
                                        alt: "PDF Page {page_idx + 1}",
                                        class: "pdf-page",
                                        style: "display: block; width: 100%; max-width: 800px; height: auto; border: 1px solid #ddd; border-radius: 4px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); background-color: white;",
                                        onerror: move |_| {
                                            tracing::error!("Failed to load image for page {}", page_idx);
                                        }
                                    }
                                    div {
                                        class: "text-overlay",
                                        style: "position: absolute; top: 0; left: 0; right: 0; bottom: 0; pointer-events: none; border-radius: 4px; z-index: 1; overflow: hidden;",
                                        for (text_idx, text_elem) in page_data.text_elements.iter().enumerate() {
                                            span {
                                                key: "p{page_idx}t{text_idx}",
                                                class: "selectable-text",
                                                style: "position: absolute;
                                                       left: {text_elem.bounds.x / page_data.page_width * 100.0}%; 
                                                       top: {text_elem.bounds.y / page_data.page_height * 100.0}%;
                                                       width: {text_elem.bounds.width / page_data.page_width * 100.0}%;
                                                       height: {text_elem.bounds.height / page_data.page_height * 100.0}%;
                                                       font-size: {(text_elem.font_size / page_data.page_height * 100.0).max(0.8)}%;
                                                       color: transparent;
                                                       pointer-events: auto;
                                                       user-select: text;
                                                       cursor: text;
                                                       font-family: monospace;
                                                       line-height: 1;
                                                       overflow: hidden;
                                                       white-space: nowrap;
                                                       z-index: 2;",
                                                "{text_elem.text} "
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        if is_loading() && rendered_pages().len() < total_pages {
                            div {
                                class: "text-center py-8 text-muted-foreground italic",
                                "Loading remaining {total_pages - rendered_pages().len()} pages..."
                            }
                        } else if !is_loading() && rendered_pages().is_empty() && pdf_path().is_some() && total_pages > 0 {
                            div {
                                class: "text-center py-8 text-destructive",
                                "No pages could be rendered. Check logs for errors."
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn Arrangement() -> Element {
    #[cfg(not(target_arch = "wasm32"))]
    let on_seek_to_time = Callback::new(move |(song_idx, time_seconds): (usize, f64)| {
        spawn(async move {
            if let Err(e) = seek_to_time(song_idx, time_seconds).await {
                warn!(
                    "Failed to seek to song {} at time {}s: {}",
                    song_idx, time_seconds, e
                );
            }
        });
    });

    #[cfg(target_arch = "wasm32")]
    let on_seek_to_time = Callback::new(move |(_song_idx, _time_seconds): (usize, f64)| {});

    #[cfg(not(target_arch = "wasm32"))]
    let on_seek_to_musical_position = {
        use crate::setlist_commands::seek_to_musical_position;
        Callback::new(
            move |(song_idx, musical_pos): (usize, daw::primitives::MusicalPosition)| {
                let musical_pos_for_log = musical_pos.clone();
                spawn(async move {
                    if let Err(e) = seek_to_musical_position(song_idx, musical_pos).await {
                        warn!(
                            "Failed to seek to song {} at musical position {}: {}",
                            song_idx, musical_pos_for_log, e
                        );
                    }
                });
            },
        )
    };

    #[cfg(target_arch = "wasm32")]
    let on_seek_to_musical_position = Callback::new(
        move |(_song_idx, _musical_pos): (usize, daw::primitives::MusicalPosition)| {},
    );

    #[cfg(not(target_arch = "wasm32"))]
    let on_track_mute = {
        use crate::track_commands::set_track_mute;
        use dioxus::prelude::spawn;
        Callback::new(
            move |(project_name, track_index, muted): (String, usize, bool)| {
                spawn(async move {
                    if let Err(e) = set_track_mute(project_name, track_index, muted).await {
                        tracing::warn!("Failed to set track mute: {}", e);
                    }
                });
            },
        )
    };

    #[cfg(target_arch = "wasm32")]
    let on_track_mute = Callback::new(|_| {});

    #[cfg(not(target_arch = "wasm32"))]
    let on_track_solo = {
        use crate::track_commands::set_track_solo;
        use dioxus::prelude::spawn;
        Callback::new(
            move |(project_name, track_index, solo_mode): (
                String,
                usize,
                daw::tracks::api::solo::SoloMode,
            )| {
                spawn(async move {
                    if let Err(e) = set_track_solo(project_name, track_index, solo_mode).await {
                        tracing::warn!("Failed to set track solo: {}", e);
                    }
                });
            },
        )
    };

    #[cfg(target_arch = "wasm32")]
    let on_track_solo = Callback::new(|_| {});

    rsx! {
        ArrangementView {
            on_seek_to_time: Some(on_seek_to_time),
            on_track_mute: Some(on_track_mute),
            on_track_solo: Some(on_track_solo),
        }
    }
}

// Testing component and Mosaic test page removed - was using libs/ui which is no longer used

/// Create sample setlist with real Setlist data
fn create_sample_setlist() -> Setlist {
    let mut setlist = Setlist::new("Sample Setlist".to_string()).unwrap();
    let measure_duration = 2.0; // 2 seconds per measure at 120 BPM

    // Song 1: "Midnight Dreams" - Complex structure with bridge
    let mut song1 = Song::new("Midnight Dreams".to_string()).unwrap();
    song1
        .metadata
        .insert("project_name".to_string(), "Project A".to_string());

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
    let mut intro = Section::from_seconds(
        SectionType::Intro,
        current_time,
        current_time + 4.0 * measure_duration,
        "Intro".to_string(),
        None,
    )
    .unwrap();
    intro.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song1.sections.push(intro);
    current_time += 4.0 * measure_duration;
    // Verse 1 - green
    let mut verse1 = Section::from_seconds(
        SectionType::Verse,
        current_time,
        current_time + 8.0 * measure_duration,
        "Verse 1".to_string(),
        Some(1),
    )
    .unwrap();
    verse1.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song1.sections.push(verse1);
    current_time += 8.0 * measure_duration;
    // Chorus 1 - purple
    let mut chorus1 = Section::from_seconds(
        SectionType::Chorus,
        current_time,
        current_time + 8.0 * measure_duration,
        "Chorus 1".to_string(),
        Some(1),
    )
    .unwrap();
    chorus1.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song1.sections.push(chorus1);
    current_time += 8.0 * measure_duration;
    // Verse 2 - green
    let mut verse2 = Section::from_seconds(
        SectionType::Verse,
        current_time,
        current_time + 4.0 * measure_duration,
        "Verse 2".to_string(),
        Some(2),
    )
    .unwrap();
    verse2.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song1.sections.push(verse2);
    current_time += 4.0 * measure_duration;
    // Chorus 2 - purple
    let mut chorus2 = Section::from_seconds(
        SectionType::Chorus,
        current_time,
        current_time + 6.0 * measure_duration,
        "Chorus 2".to_string(),
        Some(2),
    )
    .unwrap();
    chorus2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song1.sections.push(chorus2);
    current_time += 6.0 * measure_duration;
    // Bridge - red
    let mut bridge = Section::from_seconds(
        SectionType::Bridge,
        current_time,
        current_time + 8.0 * measure_duration,
        "Bridge".to_string(),
        None,
    )
    .unwrap();
    bridge.color = Some((239u32 << 16) | (68u32 << 8) | 68u32);
    song1.sections.push(bridge);
    current_time += 8.0 * measure_duration;
    // Chorus 3 - purple
    let mut chorus3 = Section::from_seconds(
        SectionType::Chorus,
        current_time,
        current_time + 8.0 * measure_duration,
        "Chorus 3".to_string(),
        Some(3),
    )
    .unwrap();
    chorus3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song1.sections.push(chorus3);
    current_time += 8.0 * measure_duration;
    // Outro - blue
    let mut outro = Section::from_seconds(
        SectionType::Outro,
        current_time,
        current_time + 8.0 * measure_duration,
        "Outro".to_string(),
        None,
    )
    .unwrap();
    outro.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song1.sections.push(outro);

    // Add tempo and time signature changes for Song 1
    song1
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            0.0,
            120.0,
            None,
            Some((4, 4)),
            None,
            None,
            None,
        ));
    song1
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            36.0,
            120.0,
            None,
            Some((6, 8)),
            None,
            None,
            None,
        ));
    song1
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            60.0,
            100.0,
            None,
            Some((4, 4)),
            None,
            None,
            None,
        ));
    song1
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            76.0, 140.0, None, None, None, None, None,
        ));

    setlist.add_song(song1).unwrap();

    // Song 2: "Electric Nights" - Simple verse-chorus structure (same project as Song 1)
    let mut song2 = Song::new("Electric Nights".to_string()).unwrap();
    song2
        .metadata
        .insert("project_name".to_string(), "Project A".to_string());

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
    let mut intro2 = Section::from_seconds(
        SectionType::Intro,
        current_time,
        current_time + 2.0 * measure_duration,
        "Intro".to_string(),
        None,
    )
    .unwrap();
    intro2.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song2.sections.push(intro2);
    current_time += 2.0 * measure_duration;
    let mut verse1_2 = Section::from_seconds(
        SectionType::Verse,
        current_time,
        current_time + 8.0 * measure_duration,
        "Verse 1".to_string(),
        Some(1),
    )
    .unwrap();
    verse1_2.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song2.sections.push(verse1_2);
    current_time += 8.0 * measure_duration;
    let mut chorus1_2 = Section::from_seconds(
        SectionType::Chorus,
        current_time,
        current_time + 8.0 * measure_duration,
        "Chorus 1".to_string(),
        Some(1),
    )
    .unwrap();
    chorus1_2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song2.sections.push(chorus1_2);
    current_time += 8.0 * measure_duration;
    let mut verse2_2 = Section::from_seconds(
        SectionType::Verse,
        current_time,
        current_time + 8.0 * measure_duration,
        "Verse 2".to_string(),
        Some(2),
    )
    .unwrap();
    verse2_2.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song2.sections.push(verse2_2);
    current_time += 8.0 * measure_duration;
    let mut chorus2_2 = Section::from_seconds(
        SectionType::Chorus,
        current_time,
        current_time + 8.0 * measure_duration,
        "Chorus 2".to_string(),
        Some(2),
    )
    .unwrap();
    chorus2_2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song2.sections.push(chorus2_2);
    current_time += 8.0 * measure_duration;
    let mut chorus3_2 = Section::from_seconds(
        SectionType::Chorus,
        current_time,
        current_time + 8.0 * measure_duration,
        "Chorus 3".to_string(),
        Some(3),
    )
    .unwrap();
    chorus3_2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song2.sections.push(chorus3_2);
    current_time += 8.0 * measure_duration;
    let mut outro2 = Section::from_seconds(
        SectionType::Outro,
        current_time,
        current_time + 4.0 * measure_duration,
        "Outro".to_string(),
        None,
    )
    .unwrap();
    outro2.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song2.sections.push(outro2);

    song2
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            0.0,
            120.0,
            None,
            Some((4, 4)),
            None,
            None,
            None,
        ));
    song2
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            8.0, 130.0, None, None, None, None, None,
        ));
    song2
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            24.0,
            130.0,
            None,
            Some((3, 4)),
            None,
            None,
            None,
        ));
    song2
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            40.0,
            130.0,
            None,
            Some((5, 4)),
            None,
            None,
            None,
        ));
    song2
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            56.0,
            130.0,
            None,
            Some((4, 4)),
            None,
            None,
            None,
        ));

    setlist.add_song(song2).unwrap();

    // Song 3: "City Lights" - Verse-heavy with instrumental break (different project)
    let mut song3 = Song::new("City Lights".to_string()).unwrap();
    song3
        .metadata
        .insert("project_name".to_string(), "Project B".to_string());

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
    let mut intro3 = Section::from_seconds(
        SectionType::Intro,
        current_time,
        current_time + 6.0 * measure_duration,
        "Intro".to_string(),
        None,
    )
    .unwrap();
    intro3.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song3.sections.push(intro3);
    current_time += 6.0 * measure_duration;
    let mut verse1_3 = Section::from_seconds(
        SectionType::Verse,
        current_time,
        current_time + 8.0 * measure_duration,
        "Verse 1".to_string(),
        Some(1),
    )
    .unwrap();
    verse1_3.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song3.sections.push(verse1_3);
    current_time += 8.0 * measure_duration;
    let mut chorus1_3 = Section::from_seconds(
        SectionType::Chorus,
        current_time,
        current_time + 8.0 * measure_duration,
        "Chorus 1".to_string(),
        Some(1),
    )
    .unwrap();
    chorus1_3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song3.sections.push(chorus1_3);
    current_time += 8.0 * measure_duration;
    let mut verse2_3 = Section::from_seconds(
        SectionType::Verse,
        current_time,
        current_time + 8.0 * measure_duration,
        "Verse 2".to_string(),
        Some(2),
    )
    .unwrap();
    verse2_3.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song3.sections.push(verse2_3);
    current_time += 8.0 * measure_duration;
    let mut chorus2_3 = Section::from_seconds(
        SectionType::Chorus,
        current_time,
        current_time + 8.0 * measure_duration,
        "Chorus 2".to_string(),
        Some(2),
    )
    .unwrap();
    chorus2_3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song3.sections.push(chorus2_3);
    current_time += 8.0 * measure_duration;
    let mut instrumental = Section::from_seconds(
        SectionType::Instrumental,
        current_time,
        current_time + 12.0 * measure_duration,
        "Instrumental".to_string(),
        None,
    )
    .unwrap();
    instrumental.color = Some((251u32 << 16) | (191u32 << 8) | 36u32);
    song3.sections.push(instrumental);
    current_time += 12.0 * measure_duration;
    let mut verse3_3 = Section::from_seconds(
        SectionType::Verse,
        current_time,
        current_time + 8.0 * measure_duration,
        "Verse 3".to_string(),
        Some(3),
    )
    .unwrap();
    verse3_3.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
    song3.sections.push(verse3_3);
    current_time += 8.0 * measure_duration;
    let mut chorus3_3 = Section::from_seconds(
        SectionType::Chorus,
        current_time,
        current_time + 8.0 * measure_duration,
        "Chorus 3".to_string(),
        Some(3),
    )
    .unwrap();
    chorus3_3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
    song3.sections.push(chorus3_3);
    current_time += 8.0 * measure_duration;
    let mut outro3 = Section::from_seconds(
        SectionType::Outro,
        current_time,
        current_time + 6.0 * measure_duration,
        "Outro".to_string(),
        None,
    )
    .unwrap();
    outro3.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
    song3.sections.push(outro3);

    song3
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            0.0,
            120.0,
            None,
            Some((4, 4)),
            None,
            None,
            None,
        ));
    song3
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            28.0,
            120.0,
            None,
            Some((3, 4)),
            None,
            None,
            None,
        ));
    song3
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            44.0,
            120.0,
            None,
            Some((7, 8)),
            None,
            None,
            None,
        ));
    song3
        .tempo_time_sig_changes
        .push(TempoTimePoint::new_full_from_seconds(
            68.0,
            140.0,
            None,
            Some((4, 4)),
            None,
            None,
            None,
        ));

    setlist.add_song(song3).unwrap();

    setlist
}
