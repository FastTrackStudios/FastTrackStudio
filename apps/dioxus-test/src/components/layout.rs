use dioxus::prelude::*;
use setlist::Setlist;
use crate::components::transport::ConnectionStatus;
use crate::components::progress::{SongProgressBar, CompactProgressBar};
use crate::components::song::SongTitle;
use crate::components::transport::TransportControlBar;
use crate::utils::{calculate_song_progress, calculate_section_progress, get_song_color_bright, get_song_color_muted, get_section_color_bright, get_section_color_muted, get_project_name};
use std::collections::HashMap;

/// Top bar component
#[component]
pub fn TopBar() -> Element {
    rsx! {
        div {
            class: "h-12 flex-shrink-0 border-b border-border bg-card flex items-center justify-between px-4",
            h1 {
                class: "text-lg font-semibold text-card-foreground",
                "FastTrackStudio"
            }
            ConnectionStatus {}
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
                        div {
                            key: "{index}",
                            class: "relative",
                            div {
                                class: "flex",
                                // Vertical progress bar container (only shown when expanded)
                                // Active song is automatically expanded
                                if current_song_index() == Some(index) && !song.sections.is_empty() {
                                    div {
                                        class: "relative w-1 flex-shrink-0 mr-2",
                                        style: format!(
                                            "height: calc(3rem + {} * 3rem + {} * 0.25rem);",
                                            song.sections.len(),
                                            song.sections.len()
                                        ),
                                        // Background (unfilled)
                                        div {
                                            class: "absolute inset-0 rounded-full",
                                            style: format!(
                                                "background-color: {}; opacity: 0.3;",
                                                get_song_color_muted(index)
                                            ),
                                        }
                                        // Filled portion (from top to bottom, showing song progress)
                                        // Only show progress if this is the active song
                                        if current_song_index() == Some(index) {
                                            div {
                                                class: "absolute top-0 left-0 right-0 rounded-full transition-all duration-300 ease-in-out",
                                                style: {
                                                    let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                                                    let position = transport_positions.read().get(&project_name).copied().unwrap_or(0.0);
                                                    format!(
                                                        "height: {}%; background-color: {}; opacity: 0.8;",
                                                        calculate_song_progress(song, position),
                                                        get_song_color_bright(index)
                                                    )
                                                },
                                            }
                                        }
                                    }
                                } else {
                                    // Spacer when not expanded
                                    div {
                                        class: "w-0",
                                    }
                                }
                                div {
                                    class: "flex-1 space-y-1",
                                    // Song progress bar - always black background
                                    CompactProgressBar {
                                        label: song.name.clone(),
                                        progress: {
                                            // Only show progress if this is the active song
                                            if current_song_index() == Some(index) {
                                                let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                                                let position = transport_positions.read().get(&project_name).copied().unwrap_or(0.0);
                                                calculate_song_progress(song, position)
                                            } else {
                                                0.0 // No progress for inactive songs
                                            }
                                        },
                                        bright_color: get_song_color_bright(index),
                                        muted_color: get_song_color_muted(index),
                                        is_selected: current_song_index() == Some(index),
                                        is_inactive: current_song_index() != Some(index),
                                        always_black_bg: true,
                                        on_click: {
                                            let setlist_clone = setlist.clone();
                                            move |_| {
                                                // Reset previous song's progress if not playing
                                                if !is_playing() {
                                                    if let Some(prev_idx) = current_song_index() {
                                                        if let Some(prev_song) = setlist_clone.songs.get(prev_idx) {
                                                            let project_name = get_project_name(prev_song).unwrap_or_else(|| "default".to_string());
                                                            // Reset to song start position (relative to project)
                                                            // This ensures calculate_song_progress returns 0%
                                                            let reset_position = if prev_song.effective_start() > 0.0 {
                                                                prev_song.effective_start()
                                                            } else {
                                                                // Fall back to first section start
                                                                prev_song.sections.first()
                                                                    .map(|s| s.start_seconds())
                                                                    .unwrap_or(0.0)
                                                            };
                                                            transport_positions.with_mut(|positions| {
                                                                positions.insert(project_name, reset_position);
                                                            });
                                                        }
                                                    }
                                                }
                                                
                                                // Set as active song (expansion happens automatically)
                                                current_song_index.set(Some(index));
                                                // Reset to first section when selecting a song
                                                current_section_index.set(Some(0));
                                            }
                                        },
                                    }
                                    // Indented sections (only if expanded - active song is auto-expanded)
                                    if current_song_index() == Some(index) {
                                        for (section_idx, section) in song.sections.iter().enumerate() {
                                            div {
                                                key: "{index}-{section_idx}",
                                                class: "pl-4",
                                                CompactProgressBar {
                                                    label: section.display_name(),
                                                    progress: {
                                                        // Only show progress if this song is active
                                                        if current_song_index() == Some(index) {
                                                            let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                                                            let position = transport_positions.read().get(&project_name).copied().unwrap_or(0.0);
                                                            calculate_section_progress(section, position)
                                                        } else {
                                                            0.0 // No progress for inactive songs
                                                        }
                                                    },
                                                    bright_color: get_section_color_bright(index, section_idx),
                                                    muted_color: get_section_color_muted(index, section_idx),
                                                    is_selected: current_section_index() == Some(section_idx),
                                                    is_inactive: current_song_index() != Some(index),
                                                    on_click: {
                                                        let song_clone = song.clone();
                                                        let section_start = section.start_seconds();
                                                        move |_| {
                                                            // Set as active section
                                                            current_section_index.set(Some(section_idx));
                                                            
                                                            // Move transport position to section start if not playing
                                                            if !is_playing() {
                                                                let project_name = get_project_name(&song_clone).unwrap_or_else(|| "default".to_string());
                                                                transport_positions.with_mut(|positions| {
                                                                    positions.insert(project_name.clone(), section_start);
                                                                });
                                                                // Also update the song's position
                                                                song_positions.with_mut(|positions| {
                                                                    positions.insert(index.to_string(), section_start);
                                                                });
                                                            }
                                                        }
                                                    },
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Main content area component (2/3 width)
#[component]
pub fn MainContent(setlist: Setlist, current_song_index: Option<usize>, is_playing: Signal<bool>, is_looping: Signal<bool>, mut transport_positions: Signal<HashMap<String, f64>>, mut song_positions: Signal<HashMap<String, f64>>, mut current_section_index: Signal<Option<usize>>) -> Element {
    // Compute key for progress bar to force remount when song changes
    let song_key = current_song_index.map(|i| i.to_string()).unwrap_or_else(|| "none".to_string());
    
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
                                        // Get the current song to find the section
                                        if let Some(song_idx) = current_song_index {
                                            if let Some(song) = setlist.songs.get(song_idx) {
                                                if let Some(section) = song.sections.get(section_idx) {
                                                    let section_start = section.start_seconds();
                                                    let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                                                    
                                                    // Update transport position
                                                    transport_positions.with_mut(|positions| {
                                                        positions.insert(project_name.clone(), section_start);
                                                    });
                                                    
                                                    // Update song position
                                                    song_positions.with_mut(|positions| {
                                                        positions.insert(song_idx.to_string(), section_start);
                                                    });
                                                    
                                                    // Update current section index
                                                    current_section_index.set(Some(section_idx));
                                                }
                                            }
                                        }
                                    }
                                }
                            },
                        }
                    }
                    // Example: WebSocket chat can go here
                }
            }
            // Transport control bar at bottom
            TransportControlBar {
                is_playing: is_playing,
                is_looping: is_looping,
            }
        }
    }
}

