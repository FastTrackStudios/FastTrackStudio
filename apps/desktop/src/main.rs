use dioxus::prelude::*;
use setlist::{Setlist, Song, Section, SectionType, SETLIST};
use marker_region::{Marker, application::TempoTimePoint};
use primitives::Position;
use ui::components::*;
#[cfg(not(target_arch = "wasm32"))]
use crate::reaper_connection::ReaperConnection;
#[cfg(not(target_arch = "wasm32"))]
use peer_2_peer::reaper_api::ReaperStateUpdate;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::{info, warn, error};

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
        };
        
        tracing_subscriber::registry()
            .with(filter)
            .with(tracing_subscriber::fmt::layer())
            .init();
    }
    
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    // Connect to setlist stream when app starts
    #[cfg(not(target_arch = "wasm32"))]
    {
        use_effect(move || {
            spawn(async move {
                if let Err(e) = setlist_connection::connect_to_reaper_setlist().await {
                    tracing::warn!("Failed to connect to REAPER setlist: {}", e);
                }
            });
        });
    }
    
    // Set up REAPER connection (transport info is embedded in SETLIST)
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
    
    // Derive current song and section index from SETLIST global signal
    let current_song_index = use_memo(move || {
        SETLIST.read().as_ref().and_then(|api| api.active_song_index())
    });
    
    let current_section_index = use_memo(move || {
        SETLIST.read().as_ref().and_then(|api| api.active_section_index())
    });
    
                
    // Connection status - for now always true when REAPER connection exists
    let is_connected = use_signal(|| false);
    
    // Mode toggle callback - simplified for now
    let on_toggle_mode = Callback::new(move |_| {
        // TODO: Implement mode toggle if needed
    });
    
    // Callbacks
    let on_song_click = Callback::new(move |_idx: usize| {
        // No-op: song selection is automatic from REAPER state via setlist signal
    });
    
    let on_section_click = Callback::new(move |(_song_idx, _section_idx): (usize, usize)| {
        // No-op: section selection is automatic from REAPER state via setlist signal
    });
    
    let on_transport_action = Callback::new(move |action: String| {
        // TODO: Implement transport actions via REAPER connection
        match action.as_str() {
            "play_pause" => {
                // TODO: Send play/pause command to REAPER
            }
            "loop_toggle" => {
                // TODO: Send loop toggle command to REAPER
            }
            "back" | "advance" => {
                // TODO: Implement navigation
            }
            _ => {}
        }
    });

    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: MAIN_CSS }
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        
        div {
            class: "h-screen w-screen flex flex-col overflow-hidden bg-background",
            tabindex: "0",
            TopBar {
                is_connected: is_connected,
                is_server_mode: false,
                on_toggle_mode: Some(on_toggle_mode),
            }
            
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
                    is_playing: Signal::new(false), // Will be derived from SETLIST in component
                    is_looping: Signal::new(false), // Will be derived from SETLIST in component
                    on_play_pause: Callback::new(move |_| {
                        on_transport_action.call("play_pause".to_string());
                    }),
                    on_loop_toggle: Callback::new(move |_| {
                        on_transport_action.call("loop_toggle".to_string());
                    }),
                    on_back: Callback::new(move |_| {
                        on_transport_action.call("back".to_string());
                    }),
                    on_forward: Callback::new(move |_| {
                        on_transport_action.call("advance".to_string());
                    }),
                }
            }
        }
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
