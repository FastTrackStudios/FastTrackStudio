use dioxus::prelude::*;
use lucide_dioxus::{Clock, Ruler};
use crate::components::transport::ConnectionStatus;
use crate::components::progress::{SongProgressBar, SectionProgressBar, ProgressSection, TempoMarker};
use crate::components::song::{SongTitle, FadedSongTitle};
use crate::components::transport::TransportControlBar;
use crate::components::sidebar_items::{SongItem, SongItemData, SectionItem};
use crate::components::mode_toggle::ModeToggle;
use lumen_blocks::components::button::{Button, ButtonVariant};
use setlist::{SETLIST_STRUCTURE, SONG_TRANSPORT, Setlist, Song, Section};
use daw::primitives::{TimePosition, MusicalPosition, TimeSignature};
use daw::transport::PlayState;

/// Edit view mode for context-specific navigation
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EditViewMode {
    Slides,
    Sync,
}

/// Top bar component
#[component]
pub fn TopBar(
    is_connected: Signal<bool>,
    #[props(default = false)]
    is_server_mode: bool,
    #[props(default)]
    on_toggle_mode: Option<Callback<()>>,
    edit_mode: Signal<bool>,
    on_toggle_edit: Callback<()>,
    #[props(default)]
    edit_view_mode: Option<Signal<Option<EditViewMode>>>,
    #[props(default)]
    on_edit_view_change: Option<Callback<EditViewMode>>,
    #[props(default)]
    current_route_path: Option<String>,
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
                
                // Navigation tabs
                div {
                    class: "flex items-center gap-1 ml-6",
                    Link {
                        to: "/",
                        class: "px-4 py-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-accent font-medium text-sm transition-colors data-[active=true]:bg-primary data-[active=true]:text-primary-foreground",
                        "Performance"
                    }
                    Link {
                        to: "/lyrics",
                        class: "px-4 py-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-accent font-medium text-sm transition-colors data-[active=true]:bg-primary data-[active=true]:text-primary-foreground",
                        "Lyrics"
                    }
                    Link {
                        to: "/arrangement",
                        class: "px-4 py-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-accent font-medium text-sm transition-colors data-[active=true]:bg-primary data-[active=true]:text-primary-foreground",
                        "Arrangement"
                    }
                    Link {
                        to: "/testing",
                        class: "px-4 py-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-accent font-medium text-sm transition-colors data-[active=true]:bg-primary data-[active=true]:text-primary-foreground",
                        "Testing"
                    }
                }
                
                // Context-specific buttons (e.g., Slides/Sync for lyrics edit)
                if edit_mode() {
                    if let Some(path) = &current_route_path {
                        if path == "/lyrics/edit" {
                            if let (Some(edit_mode_signal), Some(on_change)) = (edit_view_mode, on_edit_view_change) {
                                div {
                                    class: "flex items-center gap-1 ml-4",
                                    Button {
                                        variant: if edit_mode_signal() == Some(EditViewMode::Slides) {
                                            ButtonVariant::Primary
                                        } else {
                                            ButtonVariant::Ghost
                                        },
                                        on_click: move |_| {
                                            on_change.call(EditViewMode::Slides);
                                        },
                                        "Slides"
                                    }
                                    Button {
                                        variant: if edit_mode_signal() == Some(EditViewMode::Sync) {
                                            ButtonVariant::Primary
                                        } else {
                                            ButtonVariant::Ghost
                                        },
                                        on_click: move |_| {
                                            on_change.call(EditViewMode::Sync);
                                        },
                                        "Sync"
                                    }
                                }
                            }
                        }
                    }
                }
            }
            h1 {
                class: "text-lg font-semibold text-card-foreground",
                "FastTrackStudio"
            }
            div {
                class: "flex items-center gap-2",
                // Global edit mode toggle
                Button {
                    variant: if edit_mode() { ButtonVariant::Primary } else { ButtonVariant::Ghost },
                    on_click: move |_| on_toggle_edit.call(()),
                    if edit_mode() { "Edit" } else { "View" }
                }
            }
        }
    }
}

/// Left sidebar component (1/3 width) - Navigator
/// 
/// Reads setlist structure from SETLIST_STRUCTURE and transport from SONG_TRANSPORT.
/// Only rerenders when structure or transport for visible songs changes.
#[component]
pub fn Sidebar(
    current_song_index: Signal<Option<usize>>,
    current_section_index: Signal<Option<usize>>,
    is_playing: Signal<bool>,
    on_song_click: Callback<usize>,
    on_section_click: Callback<(usize, usize)>,
) -> Element {
    // Convert setlist to sidebar items using methods on Song and Section
    // Read from SETLIST_STRUCTURE (structure only) and SONG_TRANSPORT (per-song transport)
    // This only rerenders when structure changes or transport for a specific song changes
    let sidebar_items = use_memo(move || {
        let setlist_structure = SETLIST_STRUCTURE.read();
        let song_transport = SONG_TRANSPORT.read();
        let current_song_idx = current_song_index();
        
        setlist_structure.as_ref().map(|setlist| {
            setlist.songs.iter().enumerate().map(|(idx, song)| {
                // Get transport for this song from SONG_TRANSPORT signal
                let transport = song_transport.get(&idx);
                let position = transport
                    .map(|t| t.playhead_position.time.to_seconds())
                    .unwrap_or(0.0);
                
                // Check if this song is actively playing
                let song_is_playing = transport
                    .map(|t| t.is_playing())
                    .unwrap_or(false);
                
                // Calculate song progress using song.progress() method
                // Each song shows its own progress based on its transport
                let song_progress = song.progress(position);
                
                // Calculate section progress using section.progress() method
                // Each section shows its own progress based on the song's transport
                let sections: Vec<_> = song.sections.iter().enumerate().map(|(sec_idx, section)| {
                    let section_progress = section.progress(position);
                    
                    SectionItem {
                        label: section.display_name(),
                        progress: section_progress,
                        bright_color: section.color_bright(),
                        muted_color: section.color_muted(),
                    }
                }).collect();
                
                (SongItemData {
                    label: song.name.clone(),
                    progress: song_progress,
                    bright_color: song.color_bright(),
                    muted_color: song.color_muted(),
                    sections,
                }, song_is_playing)
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
                    for (index, (song_data, song_is_playing)) in sidebar_items().iter().enumerate() {
                        SongItem {
                            song_data: song_data.clone(),
                            index: index,
                            is_expanded: current_song_index() == Some(index),
                            is_playing: *song_is_playing,
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
/// Reads setlist structure from SETLIST_STRUCTURE and transport from SONG_TRANSPORT.
/// Only rerenders when structure or transport for the current song changes.
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
    
    // Get current song data with transport info
    // Read from SETLIST_STRUCTURE (structure) and SONG_TRANSPORT (per-song transport)
    // This only rerenders when structure changes or transport for the current song changes
    let current_song_data = use_memo(move || {
        let setlist_structure = SETLIST_STRUCTURE.read();
        let song_transport = SONG_TRANSPORT.read();
        let song_idx = current_song_index();
        song_idx.and_then(|idx| {
            setlist_structure.as_ref()?.songs.get(idx).map(|song| {
                // Get position from SONG_TRANSPORT signal for this song
                let position = song_transport.get(&idx)
                    .map(|t| t.playhead_position.time.to_seconds())
                    .unwrap_or(0.0);
                (song.clone(), position)
            })
        })
    });
    
    // Compute song name, position, and total
    // Only rerenders when structure changes (song count) or current song changes
    let song_info = use_memo(move || {
        let setlist_structure = SETLIST_STRUCTURE.read();
        let song_idx = current_song_index();
        let song_name = song_idx.and_then(|idx| {
            setlist_structure.as_ref()?.songs.get(idx).map(|song| song.name.clone())
        }).unwrap_or_else(|| "No song selected".to_string());
        let song_total = setlist_structure.as_ref().map(|setlist| setlist.songs.len());
        (song_name, song_idx, song_total)
    });
    
    // Compute progress using full timeline (including count-in and ending)
    let progress = use_memo(move || {
        current_song_data().as_ref().map(|(song, current_pos)| {
            // Get count-in start (if exists)
            let count_in_start = song.count_in_marker.as_ref()
                .map(|m| m.position.time.to_seconds())
                .unwrap_or(0.0);
            
            // Get song start (effective start)
            let song_start = song.effective_start();
            
            // Get ending position (render end or hard cut, whichever is later)
            let ending_pos = song.render_end().max(song.hard_cut());
            
            // Calculate total duration from count-in start to ending
            let total_start = if count_in_start > 0.0 { count_in_start } else { song_start };
            let total_end = if ending_pos > song.effective_end() { ending_pos } else { song.effective_end() };
            let total_duration = total_end - total_start;
            
            if total_duration > 0.0 {
                ((*current_pos - total_start) / total_duration * 100.0).max(0.0).min(100.0)
            } else {
                0.0
            }
        }).unwrap_or(0.0)
    });
    
    // Compute progress sections (including count-in and ending)
    // Also track which indices are count-in/ending so we can map clicks correctly
    let progress_sections_data = use_memo(move || {
        current_song_data().as_ref().map(|(song, current_pos)| {
            // Get count-in start (if exists)
            let count_in_start = song.count_in_marker.as_ref()
                .map(|m| m.position.time.to_seconds())
                .unwrap_or(0.0);
            
            // Get song start (effective start)
            let song_start = song.effective_start();
            
            // Get song end (effective end)
            let song_end = song.effective_end();
            
            // Get ending position (render end or hard cut, whichever is later)
            let ending_pos = song.render_end().max(song.hard_cut());
            
            // Calculate total duration from count-in start to ending
            let total_start = if count_in_start > 0.0 { count_in_start } else { song_start };
            let total_end = if ending_pos > song_end { ending_pos } else { song_end };
            let total_duration = total_end - total_start;
            
            let mut sections = Vec::new();
            let has_count_in_section = count_in_start > 0.0 && count_in_start < song_start;
            let has_ending_section = ending_pos > song_end;
            
            // Add count-in section (pink) if it exists
            if has_count_in_section {
                let count_in_start_percent = if total_duration > 0.0 {
                    ((count_in_start - total_start) / total_duration * 100.0).max(0.0).min(100.0)
                } else { 0.0 };
                let count_in_end_percent = if total_duration > 0.0 {
                    ((song_start - total_start) / total_duration * 100.0).max(0.0).min(100.0)
                } else { 0.0 };
                
                sections.push(ProgressSection {
                    start_percent: count_in_start_percent,
                    end_percent: count_in_end_percent,
                    color: "rgb(255, 192, 203)".to_string(), // Pink
                    name: "Count In".to_string(),
                });
            }
            
            // Add song sections
            for section in song.sections.iter() {
                let section_start = section.start_seconds();
                let section_end = section.end_seconds();
                let section_start_percent = if total_duration > 0.0 {
                    ((section_start - total_start) / total_duration * 100.0).max(0.0).min(100.0)
                } else { 0.0 };
                let section_end_percent = if total_duration > 0.0 {
                    ((section_end - total_start) / total_duration * 100.0).max(0.0).min(100.0)
                } else { 0.0 };
                
                sections.push(ProgressSection {
                    start_percent: section_start_percent,
                    end_percent: section_end_percent,
                    color: section.color_bright(),
                    name: section.display_name(),
                });
            }
            
            // Add ending section (red) if it exists
            if has_ending_section {
                let ending_start_percent = if total_duration > 0.0 {
                    ((song_end - total_start) / total_duration * 100.0).max(0.0).min(100.0)
                } else { 0.0 };
                let ending_end_percent = if total_duration > 0.0 {
                    ((ending_pos - total_start) / total_duration * 100.0).max(0.0).min(100.0)
                } else { 100.0 };
                
                sections.push(ProgressSection {
                    start_percent: ending_start_percent,
                    end_percent: ending_end_percent,
                    color: "rgb(255, 0, 0)".to_string(), // Red
                    name: "Ending".to_string(),
                });
            }
            
            (sections, has_count_in_section, has_ending_section)
        }).unwrap_or((Vec::new(), false, false))
    });
    
    // Extract the tuple from the memo
    let progress_sections = use_memo(move || progress_sections_data().0.clone());
    let has_count_in = use_memo(move || progress_sections_data().1);
    let has_ending = use_memo(move || progress_sections_data().2);
    
    // Compute tempo markers (using same timeline as progress sections)
    let tempo_markers = use_memo(move || {
        current_song_data().as_ref().map(|(song, _)| {
            // Get count-in start (if exists)
            let count_in_start = song.count_in_marker.as_ref()
                .map(|m| m.position.time.to_seconds())
                .unwrap_or(0.0);
            
            // Get song start (effective start) - this is where bar 1 starts
            let song_start = song.effective_start();
            
            // Get ending position (render end or hard cut, whichever is later)
            let ending_pos = song.render_end().max(song.hard_cut());
            
            // Calculate total duration from count-in start to ending (same as progress sections)
            let total_start = if count_in_start > 0.0 { count_in_start } else { song_start };
            let total_end = if ending_pos > song.effective_end() { ending_pos } else { song.effective_end() };
            let song_duration = total_end - total_start;
            
            let mut markers = Vec::new();
            let mut sorted_changes: Vec<_> = song.tempo_time_sig_changes.iter().collect();
            sorted_changes.sort_by(|a, b| a.position.partial_cmp(&b.position).unwrap_or(std::cmp::Ordering::Equal));
            
            let mut prev_tempo: Option<f64> = None;
            let mut prev_time_sig: Option<(i32, i32)> = None;
            let mut prev_tempo_for_label: Option<f64> = None;
            let mut prev_time_sig_for_label: Option<(i32, i32)> = None;
            
            // Always add the first BPM and time signature markers at the start of the progress bar (0%)
            // Use the starting_tempo and starting_time_signature fields from the song
            // These are calculated in the REAPER extension at the count-in position (or song start)
            let first_tempo = song.starting_tempo
                .or_else(|| song.transport().map(|t| t.tempo.bpm));
            
            let first_time_sig = song.starting_time_signature.as_ref()
                .map(|ts| (ts.numerator, ts.denominator))
                .or_else(|| {
                    song.transport()
                        .map(|t| (t.time_signature.numerator, t.time_signature.denominator))
                });
            
            // Always add first BPM marker at 0% (unconditionally)
            if let Some(tempo) = first_tempo {
                markers.push(TempoMarker {
                    position_percent: 0.0,
                    label: format!("{:.0}", tempo),
                    is_tempo: true,
                    is_time_sig: false,
                });
                prev_tempo_for_label = Some(tempo);
            }
            
            // Always add first time signature marker at 0% (unconditionally)
            if let Some((n, d)) = first_time_sig {
                markers.push(TempoMarker {
                    position_percent: 0.0,
                    label: format!("{}/{}", n, d),
                    is_tempo: false,
                    is_time_sig: true,
                });
                prev_time_sig_for_label = Some((n, d));
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
                    // point.position is song-relative (0.0 = song start), convert to absolute time
                    let marker_position_absolute = song_start + point.position;
                    let marker_percent = if song_duration > 0.0 {
                        ((marker_position_absolute - total_start) / song_duration * 100.0).max(0.0).min(100.0)
                    } else { 0.0 };
                    
                    // Skip adding markers at 0% since we already added them at the start
                    if marker_percent > 0.1 {
                        // Create separate markers for tempo and time signature
                        // Time signature marker
                    if let Some((n, d)) = current_time_sig {
                            // Add marker if it's different from previous
                            let should_add = match prev_time_sig_for_label {
                                Some(prev_sig) => prev_sig != (n, d),
                                None => true, // First time signature change, always add it
                            };
                            if should_add {
                                markers.push(TempoMarker {
                                    position_percent: marker_percent,
                                    label: format!("{}/{}", n, d),
                                    is_tempo: false,
                                    is_time_sig: true,
                                });
                            }
                        }
                        
                        // Tempo marker (just the number, no "bpm" text)
                    if let Some(tempo) = current_tempo {
                        if let Some(prev) = prev_tempo_for_label {
                            if (prev - tempo).abs() > 0.01 {
                        markers.push(TempoMarker {
                            position_percent: marker_percent,
                                        label: format!("{:.0}", tempo),
                                        is_tempo: true,
                                        is_time_sig: false,
                        });
                                }
                            }
                        }
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
            let transport = song.transport()?;
            
            // Use musical position directly from REAPER (which uses TimeMap2_timeToBeats internally)
            // This correctly accounts for tempo and time signature changes
            let musical_pos = transport.playhead_position.musical.clone();
            
            // Format the values
            let musical_str = format!("{}.{}.{:03}", musical_pos.measure + 1, musical_pos.beat + 1, musical_pos.subdivision);
            
            // Calculate song-relative time position for time display
            let song_start = if song.effective_start() > 0.0 {
                song.effective_start()
            } else {
                song.sections.first().map(|s| s.start_seconds()).unwrap_or(0.0)
            };
            let current_pos = transport.playhead_position.time.to_seconds();
            let song_relative_position = (current_pos - song_start).max(0.0);
            let time_pos = TimePosition::from_seconds(song_relative_position);
            
            // Use transport info from the song (bpm, time sig) for display
            let tempo = transport.tempo.bpm;
            let time_sig = transport.time_signature.clone();
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
    // Read from SONG_TRANSPORT - only rerenders when transport for current song changes
    let song_transport = SONG_TRANSPORT.read();
    let is_playing = current_song_index().and_then(|idx| {
        song_transport.get(&idx)
            .map(|transport| matches!(transport.play_state, PlayState::Playing | PlayState::Recording))
    }).unwrap_or(false);
    
    let is_looping = current_song_index().and_then(|idx| {
        song_transport.get(&idx)
            .map(|transport| transport.looping)
    }).unwrap_or(false);
    
    let (song_name, song_position, song_total) = song_info();
    
    // Get next song info from SETLIST_STRUCTURE
    // Only rerenders when structure changes or current song changes
    let next_song_info = use_memo(move || {
        let setlist_structure = SETLIST_STRUCTURE.read();
        let current_idx = current_song_index();
        if let (Some(setlist), Some(idx)) = (setlist_structure.as_ref(), current_idx) {
            if let Some(next_song) = setlist.next_song(idx) {
                Some((next_song.name.clone(), next_song.color()))
            } else {
                None
            }
        } else {
            None
        }
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
                            song_name: song_name.clone(),
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
                            on_section_click: {
                                let on_click = on_section_click.clone();
                                let has_count_in_val = has_count_in.clone();
                                let has_ending_val = has_ending.clone();
                                let progress_sections_val = progress_sections.clone();
                                // Map progress section index to actual song section index
                                // Progress sections: [count-in?] + [song sections] + [ending?]
                                // Song sections: [song sections only]
                                Some(Callback::new(move |progress_idx: usize| {
                                    let has_count_in_flag = has_count_in_val();
                                    let has_ending_flag = has_ending_val();
                                    let sections = progress_sections_val();
                                    
                                    // If clicked on count-in (index 0) or ending (last index), ignore
                                    if (has_count_in_flag && progress_idx == 0) || 
                                       (has_ending_flag && progress_idx == sections.len() - 1) {
                                        return;
                                    }
                                    
                                    // Map to actual song section index
                                    let song_section_idx = if has_count_in_flag {
                                        progress_idx - 1
                                    } else {
                                        progress_idx
                                    };
                                    
                                    if let Some(callback) = &on_click {
                                        callback.call(song_section_idx);
                                    }
                                }))
                            },
                            tempo_markers: tempo_markers(),
                            song_key: song_position.map(|i| i.to_string()),
                        }
                    }
                    // Section Progress Bar (positioned below BPM markers)
                    div {
                        class: "absolute left-0 right-0",
                        style: "top: calc(50% + 5.5rem);",
                        div {
                            class: "w-full px-4",
                            SectionProgressBar {
                                progress: Signal::new(progress()),
                                sections: progress_sections(),
                                song_key: song_position.map(|i| i.to_string()),
                            }
                        }
                    }
                    // Detail Badges Component (positioned below section progress bar)
                    if let Some(badges) = detail_badges() {
                        div {
                            class: "absolute left-0 right-0",
                            style: "top: calc(50% + 8rem);",
                            div {
                                class: "flex flex-col items-center gap-4",
                                div {
                                    class: "flex flex-wrap gap-3 justify-center",
                                for badge in badges {
                                    div {
                                            class: "px-4 py-2 rounded-full bg-secondary text-secondary-foreground text-base font-medium flex items-center gap-2",
                                            if badge.label == "Time" {
                                                Clock {
                                                    size: 18,
                                                    color: "currentColor",
                                                }
                                                "{badge.value}"
                                            } else if badge.label == "Measure" {
                                                Ruler {
                                                    size: 18,
                                                    color: "currentColor",
                                        }
                                                "{badge.value}"
                                            } else if badge.label == "BPM" {
                                                "{badge.value} BPM"
                                            } else {
                                        "{badge.value}"
                                            }
                                        }
                                    }
                                }
                                // Next song title (faded, with next song's color)
                                if let Some((next_name, next_color)) = next_song_info() {
                                    FadedSongTitle {
                                        song_name: next_name,
                                        color: next_color,
                                    }
                                }
                            }
                        }
                    } else {
                        // If no badges, still show next song
                        div {
                            class: "absolute left-0 right-0",
                            style: "top: calc(50% + 8rem);",
                            div {
                                class: "flex flex-col items-center gap-4",
                                if let Some((next_name, next_color)) = next_song_info() {
                                    FadedSongTitle {
                                        song_name: next_name,
                                        color: next_color,
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

