use dioxus::prelude::*;
use crate::components::transport::ConnectionStatus;
use crate::components::progress::{SongProgressBar, ProgressSection, TempoMarker};
use crate::components::song::SongTitle;
use crate::components::transport::TransportControlBar;
use crate::components::sidebar_items::{SongItem, SongItemData, SectionItem};
use crate::components::mode_toggle::ModeToggle;
use setlist::{SETLIST, Setlist, Song, Section};
use primitives::{TimePosition, MusicalPosition, TimeSignature};
use transport::PlayState;

/// Top bar component
#[component]
pub fn TopBar(
    is_connected: Signal<bool>,
    #[props(default = false)]
    is_server_mode: bool,
    #[props(default)]
    on_toggle_mode: Option<Callback<()>>,
) -> Element {
    rsx! {
        div {
            class: "h-12 flex-shrink-0 border-b border-border bg-card flex items-center justify-between px-4",
            div {
                class: "flex items-center gap-4",
                if let Some(on_toggle) = on_toggle_mode {
                    ModeToggle {
                        is_server_mode: is_server_mode,
                        is_connected: is_connected(),
                        on_toggle: on_toggle,
                    }
                }
                ConnectionStatus { is_connected }
            }
            h1 {
                class: "text-lg font-semibold text-card-foreground",
                "FastTrackStudio"
            }
            div {
                class: "w-16", // Placeholder for right-aligned items
            }
        }
    }
}

/// Left sidebar component (1/3 width) - Navigator
/// 
/// Reads setlist data directly from SETLIST global signal.
#[component]
pub fn Sidebar(
    current_song_index: Signal<Option<usize>>,
    current_section_index: Signal<Option<usize>>,
    is_playing: Signal<bool>,
    on_song_click: Callback<usize>,
    on_section_click: Callback<(usize, usize)>,
) -> Element {
    // Convert setlist to sidebar items using methods on Song and Section
    // Read directly from SETLIST - Dioxus automatically tracks SETLIST.read()
    let sidebar_items = use_memo(move || {
        let setlist_api = SETLIST.read();
        let current_song_idx = current_song_index();
        
        setlist_api.as_ref().map(|api| {
            let setlist = api.get_setlist();
            setlist.songs.iter().enumerate().map(|(idx, song)| {
                // Get position from song's transport_info
                let position = song.transport_info.as_ref()
                    .map(|t| t.playhead_position.time.to_seconds())
                    .unwrap_or(0.0);
                
                // Calculate song progress using song.progress() method
                // Each song shows its own progress based on its transport_info
                let song_progress = song.progress(position);
                
                // Calculate section progress using section.progress() method
                // Each section shows its own progress based on the song's transport_info
                let sections: Vec<_> = song.sections.iter().enumerate().map(|(sec_idx, section)| {
                    let section_progress = section.progress(position);
                    
                    SectionItem {
                        label: section.display_name(),
                        progress: section_progress,
                        bright_color: section.color_bright(),
                        muted_color: section.color_muted(),
                    }
                }).collect();
                
                SongItemData {
                    label: song.name.clone(),
                    progress: song_progress,
                    bright_color: song.color_bright(),
                    muted_color: song.color_muted(),
                    sections,
                }
            }).collect::<Vec<_>>()
        }).unwrap_or_default()
    });
    
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
                    for (index, song_data) in sidebar_items().iter().enumerate() {
                        SongItem {
                            song_data: song_data.clone(),
                            index: index,
                            is_expanded: current_song_index() == Some(index),
                            current_section_index: current_section_index(),
                            on_song_click: {
                                let index = index;
                                let on_click = on_song_click.clone();
                                Callback::new(move |_| {
                                    on_click.call(index);
                                })
                            },
                            on_section_click: {
                                let song_idx = index;
                                let on_click = on_section_click.clone();
                                Callback::new(move |section_idx| {
                                    on_click.call((song_idx, section_idx));
                                })
                            },
                        }
                    }
                }
            }
        }
    }
}

/// Detail badges data
#[derive(Clone, Debug, PartialEq)]
pub struct DetailBadge {
    pub label: String,
    pub value: String,
}

/// Main content area component (2/3 width)
/// 
/// Reads setlist data directly from SETLIST global signal.
#[component]
pub fn MainContent(
    current_song_index: Signal<Option<usize>>,
    current_section_index: Signal<Option<usize>>,
    is_playing: Signal<bool>,
    is_looping: Signal<bool>,
    on_section_click: Option<Callback<usize>>,
    on_play_pause: Callback<()>,
    on_loop_toggle: Callback<()>,
    on_back: Callback<()>,
    on_forward: Callback<()>,
) -> Element {
    // Helper function to get project name from song
    let get_project_name = |song: &Song| -> String {
        song.metadata
            .get("project_name")
            .or_else(|| song.metadata.get("Project"))
            .or_else(|| song.metadata.get("project"))
            .cloned()
            .unwrap_or_else(|| "default".to_string())
    };
    
    // Get current song data with transport info from the song itself
    // Dioxus automatically tracks SETLIST.read() - no memo needed
    let current_song_data = use_memo(move || {
        let setlist_api = SETLIST.read();
        let song_idx = current_song_index();
        song_idx.and_then(|idx| {
            setlist_api.as_ref()?.get_setlist().songs.get(idx).map(|song| {
                // Get position from song's transport_info
                let position = song.transport_info.as_ref()
                    .map(|t| t.playhead_position.time.to_seconds())
                    .unwrap_or(0.0);
                (song.clone(), position)
            })
        })
    });
    
    // Compute song name, position, and total
    let song_info = use_memo(move || {
        let setlist_api = SETLIST.read();
        let song_idx = current_song_index();
        let song_name = song_idx.and_then(|idx| {
            setlist_api.as_ref()?.get_setlist().songs.get(idx).map(|song| song.name.clone())
        }).unwrap_or_else(|| "No song selected".to_string());
        let song_total = setlist_api.as_ref().map(|api| api.get_setlist().songs.len());
        (song_name, song_idx, song_total)
    });
    
    // Compute progress using song.progress() method
    let progress = use_memo(move || {
        current_song_data().as_ref().map(|(song, current_pos)| {
            song.progress(*current_pos)
        }).unwrap_or(0.0)
    });
    
    // Compute progress sections
    let progress_sections = use_memo(move || {
        current_song_data().as_ref().map(|(song, current_pos)| {
            let song_start = if song.effective_start() > 0.0 {
                song.effective_start()
            } else {
                song.sections.first().map(|s| s.start_seconds()).unwrap_or(0.0)
            };
            let song_end = if song.effective_end() > 0.0 {
                song.effective_end()
            } else {
                song.sections.last().map(|s| s.end_seconds()).unwrap_or(0.0)
            };
            let song_duration = song_end - song_start;
            
            song.sections.iter().map(|section| {
                let section_start = section.start_seconds();
                let section_end = section.end_seconds();
                let section_start_percent = if song_duration > 0.0 {
                    ((section_start - song_start) / song_duration * 100.0).max(0.0).min(100.0)
                } else { 0.0 };
                let section_end_percent = if song_duration > 0.0 {
                    ((section_end - song_start) / song_duration * 100.0).max(0.0).min(100.0)
                } else { 0.0 };
                
                ProgressSection {
                    start_percent: section_start_percent,
                    end_percent: section_end_percent,
                    color: section.color_bright(),
                    name: section.display_name(),
                }
            }).collect::<Vec<_>>()
        }).unwrap_or_default()
    });
    
    // Compute tempo markers
    let tempo_markers = use_memo(move || {
        current_song_data().as_ref().map(|(song, _)| {
            let song_start = if song.effective_start() > 0.0 {
                song.effective_start()
            } else {
                song.sections.first().map(|s| s.start_seconds()).unwrap_or(0.0)
            };
            let song_end = if song.effective_end() > 0.0 {
                song.effective_end()
            } else {
                song.sections.last().map(|s| s.end_seconds()).unwrap_or(0.0)
            };
            let song_duration = song_end - song_start;
            
            let mut markers = Vec::new();
            let mut sorted_changes: Vec<_> = song.tempo_time_sig_changes.iter().collect();
            sorted_changes.sort_by(|a, b| a.position.partial_cmp(&b.position).unwrap_or(std::cmp::Ordering::Equal));
            
            let mut prev_tempo: Option<f64> = None;
            let mut prev_time_sig: Option<(i32, i32)> = None;
            let mut prev_tempo_for_label: Option<f64> = None;
            let mut prev_time_sig_for_label: Option<(i32, i32)> = None;
            
            // Handle first point if it's at the start
            if let Some(first_point) = sorted_changes.first() {
                if first_point.position <= song_start {
                    if first_point.tempo > 0.0 {
                        prev_tempo_for_label = Some(first_point.tempo);
                    }
                    if let Some(time_sig) = first_point.time_signature {
                        prev_time_sig_for_label = Some(time_sig);
                    }
                }
            }
            
            for point in sorted_changes.iter() {
                let current_tempo = if point.tempo > 0.0 { Some(point.tempo) } else { None };
                let current_time_sig = point.time_signature;
                
                let tempo_changed = match (prev_tempo, current_tempo) {
                    (Some(prev), Some(curr)) => (prev - curr).abs() > 0.01,
                    _ => false,
                };
                
                let time_sig_changed = match (prev_time_sig, current_time_sig) {
                    (Some(prev), Some(curr)) => prev != curr,
                    _ => false,
                };
                
                if tempo_changed || time_sig_changed {
                    let marker_position_seconds = point.position;
                    let marker_percent = if song_duration > 0.0 {
                        ((marker_position_seconds - song_start) / song_duration * 100.0).max(0.0).min(100.0)
                    } else { 0.0 };
                    
                    let mut label_parts = Vec::new();
                    
                    if let Some((n, d)) = current_time_sig {
                        if let Some(prev_sig) = prev_time_sig_for_label {
                            if prev_sig != (n, d) {
                                label_parts.push(format!("{}/{}", n, d));
                            }
                        }
                    }
                    
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
                    
                    if let Some(tempo) = current_tempo {
                        prev_tempo_for_label = Some(tempo);
                    }
                    if let Some(time_sig) = current_time_sig {
                        prev_time_sig_for_label = Some(time_sig);
                    }
                }
                
                if let Some(tempo) = current_tempo {
                    prev_tempo = Some(tempo);
                }
                if let Some(time_sig) = current_time_sig {
                    prev_time_sig = Some(time_sig);
                }
            }
            
            markers
        }).unwrap_or_default()
    });
    
    // Compute detail badges using transport info from the song itself
    // Only show badges when we have actual transport info (not defaults)
    let detail_badges = use_memo(move || {
        current_song_data().as_ref().and_then(|(song, _)| {
            // Only proceed if we have transport info
            let transport = song.transport_info.as_ref()?;
            
            // Calculate song-relative position
            let song_start = if song.effective_start() > 0.0 {
                song.effective_start()
            } else {
                song.sections.first().map(|s| s.start_seconds()).unwrap_or(0.0)
            };
            let current_pos = transport.playhead_position.time.to_seconds();
            let song_relative_position = (current_pos - song_start).max(0.0);
            
            // Use transport info from the song (bpm, time sig)
            let tempo = transport.tempo.bpm;
            let time_sig = transport.time_signature.clone();
            
            // Calculate musical position from time position using song's tempo and time sig
            let time_pos = TimePosition::from_seconds(song_relative_position);
            let musical_pos = time_pos.to_musical_position(tempo, time_sig.clone());
            
            // Format the values
            let musical_str = format!("{}.{}.{:03}", musical_pos.measure + 1, musical_pos.beat + 1, musical_pos.subdivision);
            let time_str = format!("{}:{:02}.{:03}", time_pos.minutes, time_pos.seconds, time_pos.milliseconds);
            let tempo_str = format!("{:.0}", tempo);
            let time_sig_str = format!("{}/{}", time_sig.numerator, time_sig.denominator);
            
            Some(vec![
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
            ])
        })
    });
    
    // Get transport state for the current song for transport controls
    // Read directly from SETLIST global signal - Dioxus automatically tracks SETLIST.read()
    let setlist_api = SETLIST.read(); // Dioxus tracks this automatically
    let is_playing = current_song_index().and_then(|idx| {
        setlist_api.as_ref()?.get_setlist().songs.get(idx)
            .and_then(|song| song.transport_info.as_ref())
            .map(|transport| matches!(transport.play_state, PlayState::Playing | PlayState::Recording))
    }).unwrap_or(false);
    
    let is_looping = current_song_index().and_then(|idx| {
        setlist_api.as_ref()?.get_setlist().songs.get(idx)
            .and_then(|song| song.transport_info.as_ref())
            .map(|transport| transport.looping)
    }).unwrap_or(false);
    
    let (song_name, song_position, song_total) = song_info();
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
                            song_name: song_name.clone(),
                            position: song_position,
                            total: song_total,
                        }
                    }
                    // Progress Bar Component (centered independently)
                    // Use key wrapper to force remount when song changes, preventing cross-song animations
                    div {
                        key: "{song_position.map(|i| i.to_string()).unwrap_or_else(|| \"none\".to_string())}",
                        class: "w-full",
                        SongProgressBar {
                            progress: Signal::new(progress()),
                            sections: progress_sections(),
                            on_section_click: on_section_click,
                            tempo_markers: tempo_markers(),
                            song_key: song_position.map(|i| i.to_string()),
                        }
                    }
                    // Detail Badges Component (positioned below progress bar)
                    if let Some(badges) = detail_badges() {
                        div {
                            class: "absolute left-0 right-0",
                            style: "top: calc(50% + 4rem);",
                            div {
                                class: "flex flex-wrap gap-2 justify-center",
                                for badge in badges {
                                    div {
                                        class: "px-3 py-1 rounded-full bg-secondary text-secondary-foreground text-sm",
                                        span {
                                            class: "font-medium",
                                            "{badge.label}: "
                                        }
                                        "{badge.value}"
                                    }
                                }
                            }
                        }
                    }
                }
            }
            // Transport control bar at bottom - use transport state from song's project
            TransportControlBar {
                is_playing: is_playing,
                is_looping: is_looping,
                on_play_pause: on_play_pause.clone(),
                on_loop_toggle: on_loop_toggle.clone(),
                on_back: on_back.clone(),
                on_forward: on_forward.clone(),
            }
        }
    }
}

