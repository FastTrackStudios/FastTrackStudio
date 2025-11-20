use dioxus::prelude::*;
use setlist::Setlist;
use crate::components::transport::ConnectionStatus;
use crate::components::progress::{SongProgressBar, DetailBadges};
use crate::components::sidebar_items::SongItem;
use crate::components::song::SongTitle;
use crate::components::transport::TransportControlBar;
use crate::utils::{get_song_color_bright, get_song_color_muted, get_section_color_bright, get_section_color_muted, get_project_name, calculate_song_progress};
use crate::utils::sidebar_helpers::{get_song_progress_for_sidebar, get_section_progress_for_sidebar, reset_previous_song_position, update_position_on_section_click};
use crate::utils::navigation::update_position_to_section;
use std::collections::HashMap;

/// Top bar component
#[component]
pub fn TopBar(connection_mode: Signal<crate::config::WebSocketMode>, connection_state: Signal<crate::components::transport::ConnectionState>) -> Element {
    rsx! {
        div {
            class: "h-12 flex-shrink-0 border-b border-border bg-card flex items-center justify-between px-4",
            h1 {
                class: "text-lg font-semibold text-card-foreground",
                "FastTrackStudio"
            }
            ConnectionStatus {
                connection_mode: connection_mode,
                connection_state: connection_state,
            }
        }
    }
}

/// Left sidebar component (1/3 width) - Navigator
#[component]
pub fn Sidebar(setlist: Setlist, current_song_index: Signal<Option<usize>>, current_section_index: Signal<Option<usize>>, mut transport_positions: Signal<HashMap<String, f64>>, mut song_positions: Signal<HashMap<String, f64>>, is_playing: Signal<bool>) -> Element {
    // Songs are automatically expanded if they are the active song
    // No need for separate expanded state - it's derived from current_song_index

    rsx! {
        div {
            class: "w-1/3 border-r border-sidebar-border bg-sidebar overflow-y-auto",
            div {
                class: "p-4",
                h2 {
                    class: "text-xl font-bold text-sidebar-foreground mb-4",
                    "Navigator"
                }
                div {
                    class: "space-y-1",
                    for (index, song) in setlist.songs.iter().enumerate() {
                        SongItem {
                            song: song.clone(),
                            index: index,
                            setlist: setlist.clone(),
                            current_song_index: current_song_index,
                            current_section_index: current_section_index,
                            transport_positions: transport_positions,
                            song_positions: song_positions,
                            is_playing: is_playing,
                        }
                    }
                }
            }
        }
    }
}

/// Main content area component (2/3 width)
#[component]
pub fn MainContent(setlist: Setlist, current_song_index: Option<usize>, current_song_index_signal: Signal<Option<usize>>, is_playing: Signal<bool>, is_looping: Signal<bool>, mut transport_positions: Signal<HashMap<String, f64>>, mut song_positions: Signal<HashMap<String, f64>>, mut current_section_index: Signal<Option<usize>>) -> Element {
    // Clone setlist for use in closures
    let setlist_for_transport = setlist.clone();
    // Compute key for progress bar to force remount when song changes
    let song_key = current_song_index.map(|i| i.to_string()).unwrap_or_else(|| "none".to_string());
    
    // Pre-compute detail badges data
    let detail_badges_data = current_song_index.and_then(|song_idx| {
        setlist.songs.get(song_idx).map(|song| {
            let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
            let current_pos = transport_positions.read().get(&project_name).copied().unwrap_or(0.0);
            (song.clone(), current_pos)
        })
    });
    
    rsx! {
        div {
            class: "flex-1 flex flex-col overflow-hidden bg-background",
            // Scrollable content area
            div {
                class: "flex-1 overflow-y-auto",
                div {
                    class: "p-6 relative flex items-center justify-center h-full",
                    // Song Title Component (positioned above progress bar)
                    div {
                        class: "absolute left-0 right-0",
                        style: "bottom: calc(50% + 4rem);",
                        SongTitle {
                            setlist: setlist.clone(),
                            current_song_index,
                        }
                    }
                    // Progress Bar Component (centered independently)
                    // Use key wrapper to force remount when song changes, preventing cross-song animations
                    div {
                        key: "{song_key}",
                        class: "w-full",
                        SongProgressBar {
                            setlist: setlist.clone(),
                            current_song_index,
                            transport_positions: transport_positions.read().clone(),
                            on_section_click: {
                                move |section_idx: usize| {
                                    if !is_playing() {
                                        if let Some(song_idx) = current_song_index {
                                            if let Some(song) = setlist.songs.get(song_idx) {
                                                transport_positions.with_mut(|transport_pos| {
                                                    song_positions.with_mut(|song_pos| {
                                                        update_position_on_section_click(
                                                            song,
                                                            song_idx,
                                                            section_idx,
                                                            transport_pos,
                                                            song_pos,
                                                        );
                                                    });
                                                });
                                                current_section_index.set(Some(section_idx));
                                            }
                                        }
                                    }
                                }
                            },
                        }
                    }
                    // Detail Badges Component (positioned below progress bar)
                    if let Some((song, current_pos)) = detail_badges_data {
                        div {
                            class: "absolute left-0 right-0",
                            style: "top: calc(50% + 4rem);",
                            DetailBadges {
                                song: song,
                                current_position: current_pos,
                            }
                        }
                    }
                    // Example: WebSocket chat can go here
                }
            }
            // Transport control bar at bottom
            TransportControlBar {
                is_playing: is_playing,
                is_looping: is_looping,
                setlist: setlist_for_transport,
                current_song_index: current_song_index_signal,
                current_section_index: current_section_index,
                transport_positions: transport_positions,
                song_positions: song_positions,
            }
        }
    }
}

