use dioxus::prelude::*;
use setlist::{Setlist, Song, Section, SectionType};
use marker_region::{Marker, application::TempoTimePoint};
use primitives::Position;
use ui::components::*;
use ui::components::sidebar_items::{SongItemData, SectionItem};
use ui::components::progress::{ProgressSection, TempoMarker};
use ui::components::layout::DetailBadge;
use crate::utils::{get_project_name, get_song_color_bright, get_song_color_muted, get_section_color_bright, get_section_color_muted, calculate_song_progress, get_tempo_at_position, format_time_position, get_time_signature_at_position, format_musical_position};
use crate::utils::sidebar_helpers::{get_song_progress_for_sidebar, get_section_progress_for_sidebar};
use crate::utils::navigation::handle_keyboard_navigation;
use crate::state::provider::{StateProvider, use_app_state};
use crate::state::StateManagerMode;
use std::collections::HashMap;

mod state;
mod utils;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    // Initialize tracing subscriber for logging (only for desktop builds)
    // Note: tracing_subscriber is commented out in Cargo.toml - will be re-added with server features
    // For now, tracing will use default output
    #[cfg(feature = "desktop")]
    {
        // tracing_subscriber commented out - will be re-added incrementally
        // use tracing_subscriber::prelude::*;
        // tracing_subscriber::registry()
        //     .with(
        //         tracing_subscriber::EnvFilter::try_from_default_env()
        //             .unwrap_or_else(|_| "info".into())
        //     )
        //     .with(tracing_subscriber::fmt::layer())
        //     .init();
    }
    
    dioxus::launch(Root);
}

/// Root component that wraps the app with state provider
#[component]
fn Root() -> Element {
    let initial_setlist = create_sample_setlist();
    let initial_mode = StateManagerMode::Local; // Can be swapped to Server, Reaper, Phone, etc.
    
    rsx! {
        StateProvider {
            initial_setlist: initial_setlist,
            initial_mode: initial_mode,
            App {}
        }
    }
}

#[component]
fn App() -> Element {
    // Get all state from centralized provider
    let state = use_app_state();
    
    // Extract individual signals for easier access
    let setlist = state.setlist;
    let mut transport_positions = state.transport_positions;
    let mut song_positions = state.song_positions;
    let mut current_song_index = state.current_song_index;
    let mut current_section_index = state.current_section_index;
    let mut is_playing = state.is_playing;
    let mut is_looping = state.is_looping;
    let is_connected = state.connection_status;
    let mode = state.mode;
    
    // Convert mode to boolean for UI
    let is_server_mode = use_memo(move || {
        matches!(mode(), StateManagerMode::Server { .. })
    });
    
    // Mode toggle callback - disabled for now (server features removed)
    // Will be re-enabled when server features are re-added incrementally
    let on_toggle_mode = Callback::new(move |_| {
        // Server mode switching disabled - local only for now
        tracing::info!("Mode toggle disabled - local mode only");
    });
    
    // Log current setlist for debugging
    {
        let setlist_ref = setlist.read();
        let song_names: Vec<_> = setlist_ref.songs.iter().map(|s| s.name.as_str()).collect();
        tracing::debug!("📊 APP RENDER: Current setlist has {} songs: {:?}", setlist_ref.songs.len(), song_names);
    }
    
    // Convert Setlist to UI component data
    let sidebar_song_items: Vec<SongItemData> = setlist.read().songs.iter().enumerate().map(|(idx, song)| {
        let current_song_idx = current_song_index();
        let progress = get_song_progress_for_sidebar(song, idx, current_song_idx, &transport_positions.read());
        
        SongItemData {
            label: song.name.clone(),
            bright_color: get_song_color_bright(song),
            muted_color: get_song_color_muted(song),
            progress,
            sections: song.sections.iter().enumerate().map(|(sec_idx, section)| {
                let section_progress = get_section_progress_for_sidebar(section, song, idx, current_song_idx, &transport_positions.read());
                SectionItem {
                    label: section.display_name(),
                    bright_color: get_section_color_bright(section),
                    muted_color: get_section_color_muted(section),
                    progress: section_progress,
                }
            }).collect(),
        }
    }).collect();
    
    // Get current song data for main content (reactive - recalculates when song index or transport positions change)
    let current_song_data = use_memo(move || {
        current_song_index().and_then(|idx| {
            setlist.read().songs.get(idx).map(|song| {
                let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                let current_pos = transport_positions.read().get(&project_name).copied().unwrap_or(0.0);
                (song.clone(), current_pos)
            })
        })
    });
    
    let song_title = current_song_data().as_ref()
        .map(|(song, _)| song.name.clone())
        .unwrap_or_else(|| "No song selected".to_string());
    
    // Create progress sections and tempo markers from current song (reactive)
    let mut progress_sections = use_signal(|| Vec::<ProgressSection>::new());
    let mut tempo_markers = use_signal(|| Vec::<TempoMarker>::new());
    {
        let mut progress_sections = progress_sections.clone();
        let mut tempo_markers = tempo_markers.clone();
        use_effect(move || {
            if let Some((song, _current_pos)) = current_song_data().as_ref() {
                let song_start = if song.effective_start() > 0.0 {
                    song.effective_start()
                } else {
                    song.sections.first()
                        .map(|s| s.start_seconds())
                        .unwrap_or(0.0)
                };
                let song_end = if song.effective_end() > 0.0 {
                    song.effective_end()
                } else {
                    song.sections.last()
                        .map(|s| s.end_seconds())
                        .unwrap_or(0.0)
                };
                let song_duration = song_end - song_start;
                
                // Calculate progress sections
                let mut sections = Vec::new();
                if song_duration > 0.0 {
                    for section in &song.sections {
                        let section_start = section.start_position.time.to_seconds();
                        let section_end = section.end_position.time.to_seconds();
                        let start_percent = ((section_start - song_start) / song_duration) * 100.0;
                        let end_percent = ((section_end - song_start) / song_duration) * 100.0;
                        
                        sections.push(ProgressSection {
                            start_percent: start_percent.max(0.0).min(100.0),
                            end_percent: end_percent.max(0.0).min(100.0),
                            color: get_section_color_bright(section),
                            name: section.display_name(),
                        });
                    }
                }
                progress_sections.set(sections);
                
                // Calculate tempo markers (filter redundant ones, similar to dioxus-test)
                let mut markers = Vec::new();
                if song_duration > 0.0 {
                    // Sort tempo changes by position
                    let mut sorted_changes: Vec<&TempoTimePoint> = song.tempo_time_sig_changes.iter().collect();
                    sorted_changes.sort_by(|a, b| a.position.partial_cmp(&b.position).unwrap_or(std::cmp::Ordering::Equal));
                    
                    // Track previous values to only show actual changes
                    let mut prev_tempo: Option<f64> = None;
                    let mut prev_time_sig: Option<(i32, i32)> = None;
                    let mut prev_tempo_for_label: Option<f64> = None;
                    let mut prev_time_sig_for_label: Option<(i32, i32)> = None;
                    
                    // Establish baseline from first marker
                    if let Some(first_point) = sorted_changes.first() {
                        if first_point.tempo > 0.0 {
                            prev_tempo_for_label = Some(first_point.tempo);
                        }
                        if let Some(time_sig) = first_point.time_signature {
                            prev_time_sig_for_label = Some(time_sig);
                        }
                    }
                    
                    for point in sorted_changes.iter() {
                        let current_tempo = if point.tempo > 0.0 { Some(point.tempo) } else { None };
                        let current_time_sig = point.time_signature;
                        
                        // Check if tempo or time signature changed from previous
                        let tempo_changed = match (prev_tempo, current_tempo) {
                            (Some(prev), Some(curr)) => (prev - curr).abs() > 0.01,
                            _ => false,
                        };
                        
                        let time_sig_changed = match (prev_time_sig, current_time_sig) {
                            (Some(prev), Some(curr)) => prev != curr,
                            _ => false,
                        };
                        
                        // Only include if something actually changed
                        if tempo_changed || time_sig_changed {
                            let marker_position_seconds = point.position;
                            let marker_percent = ((marker_position_seconds - song_start) / song_duration * 100.0).max(0.0).min(100.0);
                            
                            // Build label based on what changed
                            let mut label_parts = Vec::new();
                            
                            // Check if time signature changed
                            if let Some((n, d)) = current_time_sig {
                                if let Some(prev_sig) = prev_time_sig_for_label {
                                    if prev_sig != (n, d) {
                                        label_parts.push(format!("{}/{}", n, d));
                                    }
                                }
                            }
                            
                            // Check if tempo changed
                            if let Some(tempo) = current_tempo {
                                if let Some(prev) = prev_tempo_for_label {
                                    if (prev - tempo).abs() > 0.01 {
                                        label_parts.push(format!("{:.0} bpm", tempo));
                                    }
                                }
                            }
                            
                            let label = label_parts.join(" ");
                            if !label.is_empty() {
                                markers.push(TempoMarker {
                                    position_percent: marker_percent,
                                    label,
                                });
                            }
                            
                            // Update previous values for next iteration
                            if let Some(tempo) = current_tempo {
                                prev_tempo_for_label = Some(tempo);
                            }
                            if let Some(time_sig) = current_time_sig {
                                prev_time_sig_for_label = Some(time_sig);
                            }
                        }
                        
                        // Update previous values
                        if let Some(tempo) = current_tempo {
                            prev_tempo = Some(tempo);
                        }
                        if let Some(time_sig) = current_time_sig {
                            prev_time_sig = Some(time_sig);
                        }
                    }
                }
                tempo_markers.set(markers);
            } else {
                progress_sections.set(Vec::new());
                tempo_markers.set(Vec::new());
            }
        });
    }
    
    // Calculate current progress percentage (reactive - updates when transport positions or song changes)
    let mut current_progress = use_signal(|| 0.0);
    {
        let mut current_progress = current_progress.clone();
        use_effect(move || {
            let new_progress = current_song_data().as_ref()
                .map(|(song, current_pos)| {
                    calculate_song_progress(song, *current_pos)
                })
                .unwrap_or(0.0);
            current_progress.set(new_progress);
        });
    }
    
    // Create detail badges (reactive - includes Musical Position and Time Signature)
    let mut detail_badges = use_signal(|| None::<Vec<DetailBadge>>);
    {
        let mut detail_badges = detail_badges.clone();
        use_effect(move || {
            let badges = current_song_data().as_ref().map(|(song, current_pos)| {
                // Calculate song-relative position for tempo/time sig lookup
                let song_start = if song.effective_start() > 0.0 {
                    song.effective_start()
                } else {
                    song.sections.first()
                        .map(|s| s.start_seconds())
                        .unwrap_or(0.0)
                };
                let song_relative_position = (*current_pos - song_start).max(0.0);
                
                // Get tempo and time signature at current position
                let tempo = get_tempo_at_position(song, song_relative_position);
                let time_sig = get_time_signature_at_position(song, song_relative_position);
                
                // Calculate musical position from time position
                let time_pos = primitives::TimePosition::from_seconds(song_relative_position);
                let musical_pos = time_pos.to_musical_position(tempo, time_sig.clone());
                
                // Format the values
                let musical_str = format_musical_position(&musical_pos);
                let time_str = format_time_position(&time_pos);
                let tempo_str = format!("{:.0}", tempo);
                let time_sig_str = format!("{}/{}", time_sig.numerator, time_sig.denominator);
                
                vec![
                    DetailBadge {
                        label: "Measure".to_string(),
                        value: musical_str,
                    },
                    DetailBadge {
                        label: "Time".to_string(),
                        value: time_str,
                    },
                    DetailBadge {
                        label: "BPM".to_string(),
                        value: tempo_str,
                    },
                    DetailBadge {
                        label: "Time Sig".to_string(),
                        value: time_sig_str,
                    },
                ]
            });
            detail_badges.set(badges);
        });
    }
    
    // Callbacks
    let on_song_click = Callback::new(move |idx: usize| {
        let setlist_ref = setlist.read();
        if let Some(song) = setlist_ref.songs.get(idx) {
            if let Some(section) = song.sections.first() {
                let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                let new_position = section.start_seconds();
                transport_positions.with_mut(|positions| {
                    positions.insert(project_name.clone(), new_position);
                });
                song_positions.with_mut(|positions| {
                    positions.insert(idx.to_string(), new_position);
                });
            }
        }
        current_song_index.set(Some(idx));
        current_section_index.set(Some(0));
    });
    
    let on_section_click = Callback::new(move |(song_idx, section_idx): (usize, usize)| {
        let setlist_ref = setlist.read();
        if let Some(song) = setlist_ref.songs.get(song_idx) {
            if let Some(section) = song.sections.get(section_idx) {
                let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                let new_position = section.start_seconds();
                transport_positions.with_mut(|positions| {
                    positions.insert(project_name.clone(), new_position);
                });
                song_positions.with_mut(|positions| {
                    positions.insert(song_idx.to_string(), new_position);
                });
            }
        }
        current_song_index.set(Some(song_idx));
        current_section_index.set(Some(section_idx));
    });
    
    let on_transport_action = Callback::new(move |action: String| {
        match action.as_str() {
            "back" => {
                // Navigate to previous section
                let setlist_ref = setlist.read();
                let (new_song_idx, new_section_idx) = transport_positions.with_mut(|transport_pos| {
                    song_positions.with_mut(|song_pos| {
                        handle_keyboard_navigation(
                            &setlist_ref,
                            current_song_index(),
                            current_section_index(),
                            false, // is_right = false for back
                            transport_pos,
                            song_pos,
                        )
                    })
                });
                current_song_index.set(new_song_idx);
                current_section_index.set(new_section_idx);
            }
            "play_pause" => {
                is_playing.set(!is_playing());
            }
            "loop_toggle" => {
                is_looping.set(!is_looping());
            }
            "advance" => {
                // Navigate to next section
                let setlist_ref = setlist.read();
                let (new_song_idx, new_section_idx) = transport_positions.with_mut(|transport_pos| {
                    song_positions.with_mut(|song_pos| {
                        handle_keyboard_navigation(
                            &setlist_ref,
                            current_song_index(),
                            current_section_index(),
                            true, // is_right = true for advance
                            transport_pos,
                            song_pos,
                        )
                    })
                });
                current_song_index.set(new_song_idx);
                current_section_index.set(new_section_idx);
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
            onkeydown: move |e| {
                let setlist_ref = setlist.read();
                let song_count = setlist_ref.songs.len();
                
                // Only handle section navigation when not playing
                if !is_playing() {
                    match e.key() {
                        Key::ArrowRight | Key::ArrowLeft => {
                            let is_right = matches!(e.key(), Key::ArrowRight);
                            let setlist_clone = setlist_ref.clone();
                            let (new_song_idx, new_section_idx) = transport_positions.with_mut(|transport_pos| {
                                song_positions.with_mut(|song_pos| {
                                    handle_keyboard_navigation(
                                        &setlist_clone,
                                        current_song_index(),
                                        current_section_index(),
                                        is_right,
                                        transport_pos,
                                        song_pos,
                                    )
                                })
                            });
                            current_song_index.set(new_song_idx);
                            current_section_index.set(new_section_idx);
                        }
                        _ => {}
                    }
                }
                
                // Song navigation (up/down arrows) - always available
                match e.key() {
                    Key::ArrowDown => {
                        if song_count == 0 {
                            return;
                        }
                        let prev_song_idx = current_song_index();
                        
                        let next_index = match current_song_index() {
                            Some(idx) if idx < song_count - 1 => Some(idx + 1),
                            Some(_) => Some(song_count - 1),
                            None => Some(0),
                        };
                        
                        current_song_index.set(next_index);
                        current_section_index.set(Some(0));
                        
                        if let Some(song) = setlist_ref.songs.get(next_index.unwrap_or(0)) {
                            if let Some(section) = song.sections.first() {
                                let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                                let new_position = section.start_seconds();
                                transport_positions.with_mut(|positions| {
                                    positions.insert(project_name, new_position);
                                });
                                song_positions.with_mut(|positions| {
                                    positions.insert(next_index.unwrap_or(0).to_string(), new_position);
                                });
                            }
                        }
                    }
                    Key::ArrowUp => {
                        if song_count == 0 {
                            return;
                        }
                        let prev_index = match current_song_index() {
                            Some(idx) if idx > 0 => Some(idx - 1),
                            Some(_) => Some(0),
                            None => Some(0),
                        };
                        
                        current_song_index.set(prev_index);
                        current_section_index.set(Some(0));
                        
                        if let Some(song) = setlist_ref.songs.get(prev_index.unwrap_or(0)) {
                            if let Some(section) = song.sections.first() {
                                let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                                let new_position = section.start_seconds();
                                transport_positions.with_mut(|positions| {
                                    positions.insert(project_name, new_position);
                                });
                                song_positions.with_mut(|positions| {
                                    positions.insert(prev_index.unwrap_or(0).to_string(), new_position);
                                });
                            }
                        }
                    }
                    Key::Character(c) if c == "c" || c == "C" => {
                        is_looping.set(!is_looping());
                    }
                    Key::Character(c) if c == " " => {
                        e.prevent_default();
                        is_playing.set(!is_playing());
                    }
                    _ => {}
                }
            },
            
            TopBar {
                is_connected: is_connected,
                is_server_mode: is_server_mode(),
                on_toggle_mode: Some(on_toggle_mode),
            }
            
            div {
                class: "flex-1 flex overflow-hidden",
                
                Sidebar {
                    songs: sidebar_song_items,
                    current_song_index: current_song_index,
                    current_section_index: current_section_index,
                    is_playing: is_playing,
                    on_song_click: on_song_click,
                    on_section_click: on_section_click,
                }
                
                MainContent {
                    song_name: song_title,
                    song_position: current_song_index(),
                    song_total: Some(setlist.read().songs.len()),
                    detail_badges: detail_badges(),
                    progress: current_progress,
                    sections: progress_sections(),
                    tempo_markers: tempo_markers(),
                    on_section_click: Some(Callback::new(move |idx: usize| {
                        if let Some(song_idx) = current_song_index() {
                            on_section_click.call((song_idx, idx));
                        }
                    })),
                    is_playing: is_playing,
                    is_looping: is_looping,
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
