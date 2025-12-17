use dioxus::prelude::*;
use lucide_dioxus::{Clock, Ruler};
use crate::components::transport::ConnectionStatus;
use crate::components::progress::{SongProgressBar, ProgressSection, TempoMarker, MeasureIndicator};
use crate::components::section_progress::SectionProgressBar;
use crate::components::song::{SongTitle, FadedSongTitle};
use crate::components::transport::TransportControlBar;
use crate::components::sidebar_items::{SongItem, SongItemData, SectionItem};
use crate::components::mode_toggle::ModeToggle;
use lumen_blocks::components::button::{Button, ButtonVariant};
use fts::setlist::{SETLIST_STRUCTURE, SONG_TRANSPORT, Setlist, Song, Section};
use fts::setlist::infra::dioxus::SETLIST;
use daw::primitives::{TimePosition, MusicalPosition, TimeSignature, Position};
use daw::transport::PlayState;
use tracing::info;

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
                        to: "/chords",
                        class: "px-4 py-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-accent font-medium text-sm transition-colors data-[active=true]:bg-primary data-[active=true]:text-primary-foreground",
                        "Chords"
                    }
                    Link {
                        to: "/chart",
                        class: "px-4 py-2 rounded-md text-muted-foreground hover:text-foreground hover:bg-accent font-medium text-sm transition-colors data-[active=true]:bg-primary data-[active=true]:text-primary-foreground",
                        "Chart"
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
    // For the active song, use reactive signals from SETLIST; for others, calculate from transport
    let sidebar_items = use_memo(move || {
        let setlist_structure = SETLIST_STRUCTURE.read();
        let song_transport = SONG_TRANSPORT.read();
        let current_song_idx = current_song_index();
        
        // Get progress values from SETLIST for the active song
        let active_song_progress = SETLIST.read().as_ref()
            .and_then(|api| {
                if api.active_song_index() == current_song_idx {
                    api.song_progress.map(|p| p * 100.0) // Convert 0.0-1.0 to 0-100
                } else {
                    None
                }
            });
        
        let active_section_progress = SETLIST.read().as_ref()
            .and_then(|api| {
                if api.active_song_index() == current_song_idx {
                    api.section_progress.map(|p| p * 100.0) // Convert 0.0-1.0 to 0-100
                } else {
                    None
                }
            });
        
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
                
                // Use reactive signal progress for active song, calculate for others
                let song_progress = if Some(idx) == current_song_idx {
                    active_song_progress.unwrap_or_else(|| song.progress(position))
                } else {
                    song.progress(position)
                };
                
                // Calculate section progress - use reactive signal for active song's active section
                let sections: Vec<_> = song.sections.iter().enumerate().map(|(sec_idx, section)| {
                    let section_progress = if Some(idx) == current_song_idx {
                        // Check if this is the active section
                        let is_active_section = SETLIST.read().as_ref()
                            .and_then(|api| api.active_section_index())
                            .map(|active_sec_idx| active_sec_idx == sec_idx)
                            .unwrap_or(false);
                        
                        if is_active_section {
                            active_section_progress.unwrap_or_else(|| section.progress(position))
                        } else {
                            section.progress(position)
                        }
                    } else {
                        section.progress(position)
                    };
                    
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
    #[props(default)]
    on_seek_to_time: Option<Callback<(usize, f64)>>,
    #[props(default)]
    on_seek_to_musical_position: Option<Callback<(usize, daw::primitives::MusicalPosition)>>,
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
    
    // Compute progress using song_progress from SetlistApi (computed on server side)
    // Convert from 0.0-1.0 to 0-100 for display
    // Use reactive signal that directly reads from SETLIST
    let mut progress = use_signal(|| {
        SETLIST.read().as_ref()
            .and_then(|api| api.song_progress)
            .map(|p| p * 100.0) // Convert 0.0-1.0 to 0-100
            .unwrap_or(0.0)
    });
    
    // Update progress signal reactively when SETLIST changes
    use_effect(move || {
        // Reading SETLIST here makes this effect reactive to SETLIST changes
        let _ = SETLIST.read();
        let progress_value = SETLIST.read().as_ref()
            .and_then(|api| api.song_progress);
        
        // Log the raw progress value (0.0-1.0) for debugging
        if let Some(p) = progress_value {
            static LAST_SONG_PROGRESS: std::sync::Mutex<Option<f64>> = std::sync::Mutex::new(None);
            let mut last = LAST_SONG_PROGRESS.lock().unwrap();
            if *last != Some(p) {
                info!("song_progress: {:.4} (0.0-1.0)", p);
                *last = Some(p);
            }
        }
        
        let new_progress = progress_value.map(|p| p * 100.0).unwrap_or(0.0);
        progress.set(new_progress);
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
            // Helper function to format section names for display
            let format_section_name = |name: &str, is_last: bool| -> String {
                // Hide "Count In" sections
                if name.eq_ignore_ascii_case("Count In") || name.eq_ignore_ascii_case("Count-In") {
                    return String::new();
                }
                
                // Remove "Ending" from the last section name
                let mut formatted = name.to_string();
                if is_last {
                    // Remove "Ending" word (case-insensitive)
                    formatted = formatted
                        .replace("Ending", "")
                        .replace("ending", "")
                        .replace("ENDING", "")
                        .trim()
                        .to_string();
                }
                
                // Abbreviate long names (truncate if over 15 characters)
                const MAX_LENGTH: usize = 15;
                if formatted.len() > MAX_LENGTH {
                    // Try to abbreviate common words first
                    formatted = formatted
                        .replace("Introduction", "Intro")
                        .replace("Verse", "V")
                        .replace("Chorus", "C")
                        .replace("Bridge", "B")
                        .replace("Outro", "O")
                        .replace("Instrumental", "Inst")
                        .replace("Solo", "S");
                    
                    // If still too long, truncate with ellipsis
                    if formatted.len() > MAX_LENGTH {
                        formatted = format!("{}...", &formatted[..MAX_LENGTH.saturating_sub(3)]);
                    }
                }
                
                formatted
            };
            
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
                    name: format_section_name("Count In", false),
                });
            }
            
            // Calculate ending section start percent first (if it exists) to avoid overlap
            let ending_start_percent = if has_ending_section {
                if total_duration > 0.0 {
                    ((song_end - total_start) / total_duration * 100.0).max(0.0).min(100.0)
                } else {
                    0.0
                }
            } else {
                100.0 // No ending section, so last section can go to 100%
            };
            
            // Add song sections
            for section in song.sections.iter() {
                if let (Some(section_start), Some(section_end)) = (section.start_seconds(), section.end_seconds()) {
                    let section_start_percent = if total_duration > 0.0 {
                        ((section_start - total_start) / total_duration * 100.0).max(0.0).min(100.0)
                    } else { 0.0 };
                    let mut section_end_percent = if total_duration > 0.0 {
                        ((section_end - total_start) / total_duration * 100.0).max(0.0).min(100.0)
                    } else { 0.0 };
                    
                    // If there's an ending section and this section ends at or after song_end,
                    // cap it at the ending section start to prevent overlap
                    if has_ending_section && section_end_percent >= ending_start_percent {
                        // Use a small epsilon (0.01%) to create a clear boundary
                        section_end_percent = (ending_start_percent - 0.01).max(section_start_percent);
                    }
                
                    sections.push(ProgressSection {
                        start_percent: section_start_percent,
                        end_percent: section_end_percent,
                        color: section.color_bright(),
                        name: format_section_name(&section.display_name(), false),
                    });
                }
            }
            
            // Add ending section (red) if it exists - this is always the last section
            if has_ending_section {
                let ending_end_percent = if total_duration > 0.0 {
                    ((ending_pos - total_start) / total_duration * 100.0).max(0.0).min(100.0)
                } else { 100.0 };
                
                sections.push(ProgressSection {
                    start_percent: ending_start_percent,
                    end_percent: ending_end_percent,
                    color: "rgb(255, 0, 0)".to_string(), // Red
                    name: format_section_name("Ending", true), // true = is last section, so remove "Ending" word
                });
            }
            
            // Now update the last section (if no ending section, it's the last song section)
            if !has_ending_section && !sections.is_empty() {
                if let Some(last_section) = sections.last_mut() {
                    // Remove "Ending" from the last section name
                    let original_name = last_section.name.clone();
                    last_section.name = format_section_name(&original_name, true);
                }
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
            sorted_changes.sort_by(|a, b| a.position_seconds().partial_cmp(&b.position_seconds()).unwrap_or(std::cmp::Ordering::Equal));
            
            let mut prev_tempo: Option<f64> = None;
            let mut prev_time_sig: Option<(i32, i32)> = None;
            let mut prev_tempo_for_label: Option<f64> = None;
            let mut prev_time_sig_for_label: Option<(i32, i32)> = None;
            
            // Structure to collect tempo changes for filtering
            struct TempoChange {
                position_seconds: f64,
                position_percent: f64,
                tempo: f64,
                prev_tempo: f64,
                position: Position, // Full position including musical position (measure, beat, subdivision)
                section_index: Option<usize>, // Which section this change occurs in (None if not in any section)
            }
            
            // Structure to collect time signature changes for filtering
            struct TimeSigChange {
                position_seconds: f64,
                position_percent: f64,
                time_sig: (i32, i32),
                prev_time_sig: (i32, i32),
                position: Position, // Full position including musical position (measure, beat, subdivision)
                section_index: Option<usize>, // Which section this change occurs in (None if not in any section)
            }
            
            let mut tempo_changes: Vec<TempoChange> = Vec::new();
            let mut time_sig_changes: Vec<TimeSigChange> = Vec::new();
            
            
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
            // First marker includes " bpm" for context
            if let Some(tempo) = first_tempo {
                markers.push(TempoMarker {
                    position_percent: 0.0,
                    label: format!("{:.0} bpm", tempo),
                    is_tempo: true,
                    is_time_sig: false,
                    show_line_only: false,
                });
                prev_tempo_for_label = Some(tempo);
            }
            
            // Always add first time signature marker at 0% (unconditionally) - always show card
            if let Some((n, d)) = first_time_sig {
                markers.push(TempoMarker {
                    position_percent: 0.0,
                    label: format!("{}/{}", n, d),
                    is_tempo: false,
                    is_time_sig: true,
                    show_line_only: false,
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
                    // point.position is song-relative, convert to absolute time
                    let marker_position_absolute = song_start + point.position_seconds();
                    let marker_percent = if song_duration > 0.0 {
                        ((marker_position_absolute - total_start) / song_duration * 100.0).max(0.0).min(100.0)
                    } else { 0.0 };
                    
                    // Skip adding markers at 0% since we already added them at the start
                    if marker_percent > 0.1 {
                        // Collect time signature changes for filtering (similar to tempo changes)
                        if let Some((n, d)) = current_time_sig {
                            if let Some(prev_sig) = prev_time_sig_for_label {
                                if prev_sig != (n, d) {
                                    // Use the full Position from the point (includes musical position with measure, beat, subdivision)
                                    let position = point.position.clone();
                                    
                                    // Find which section this time sig change belongs to (using song-relative position)
                                    let song_relative_pos = point.position_seconds();
                                    let section_index = song.current_section_index(song_relative_pos);
                                    
                                    time_sig_changes.push(TimeSigChange {
                                        position_seconds: marker_position_absolute,
                                        position_percent: marker_percent,
                                        time_sig: (n, d),
                                        prev_time_sig: prev_sig,
                                        position,
                                        section_index,
                                    });
                                }
                            }
                        }
                        
                        // Collect tempo changes for filtering (don't add to markers yet)
                        if let Some(tempo) = current_tempo {
                            if let Some(prev) = prev_tempo_for_label {
                                if (prev - tempo).abs() > 0.01 {
                                    // Use the full Position from the point (includes musical position with measure, beat, subdivision)
                                    let position = point.position.clone();
                                    
                                    // Find which section this tempo change belongs to (using song-relative position)
                                    let song_relative_pos = point.position_seconds();
                                    let section_index = song.current_section_index(song_relative_pos);
                                    
                                    tempo_changes.push(TempoChange {
                                        position_seconds: marker_position_absolute,
                                        position_percent: marker_percent,
                                        tempo,
                                        prev_tempo: prev,
                                        position,
                                        section_index,
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
            
            // Filter tempo changes based on:
            // 1. Frequency: must be at least 5 measures apart (or 4 seconds as fallback)
            //    EXCEPTION: Very large BPM changes (>= 10) bypass frequency check
            // 2. Section: if in same section, only show if BPM difference >= 5
            // 3. Section: if in different sections, require minimum BPM difference (>= 5)
            const MIN_MEASURES_APART: i32 = 5;
            const MIN_SECONDS_APART: f64 = 4.0;
            const MIN_BPM_DIFFERENCE_SAME_SECTION: f64 = 5.0;
            const MIN_BPM_DIFFERENCE_DIFFERENT_SECTION: f64 = 5.0;
            const LARGE_BPM_CHANGE_THRESHOLD: f64 = 10.0; // Bypass frequency check for changes this large
            
            let mut final_filtered: Vec<usize> = Vec::new();
            
            for (i, change) in tempo_changes.iter().enumerate() {
                let bpm_diff = (change.tempo - change.prev_tempo).abs();
                
                // Check frequency: must be at least 5 measures apart from the previous tempo change
                // UNLESS it's a very large BPM change (>= 10), in which case always show it
                let is_far_enough = if i == 0 {
                    // First change - always allow it
                    true
                } else if bpm_diff >= LARGE_BPM_CHANGE_THRESHOLD {
                    // Very large BPM change - always show it regardless of frequency
                    true
                } else {
                    // Compare with the previous tempo change in the list
                    let prev_change = &tempo_changes[i - 1];
                    
                    // Calculate measure difference using MusicalPosition
                    let measure_diff = (change.position.musical.measure - prev_change.position.musical.measure).abs();
                    
                    // Calculate time difference (seconds)
                    let time_diff = (change.position_seconds - prev_change.position_seconds).abs();
                    
                    // Must be at least 5 measures apart OR at least 4 seconds apart
                    measure_diff >= MIN_MEASURES_APART || time_diff >= MIN_SECONDS_APART
                };
                
                if !is_far_enough {
                    // Too frequent - skip this change
                    continue;
                }
                
                // Check if this change is in a different section from the previous change
                let is_different_section = if i > 0 {
                    let prev_change = &tempo_changes[i - 1];
                    change.section_index != prev_change.section_index
                } else {
                    // First change - always show if it's the first in a section
                    true
                };
                
                let should_include = if is_different_section {
                    // Different section: require minimum BPM difference (>= 5)
                    bpm_diff >= MIN_BPM_DIFFERENCE_DIFFERENT_SECTION
                } else {
                    // Same section: only show if BPM difference >= 5
                    bpm_diff >= MIN_BPM_DIFFERENCE_SAME_SECTION
                };
                
                if !should_include {
                    // BPM difference too small - skip this change
                    continue;
                }
                
                // Check if this is a large tempo change that only lasts less than 1 measure
                // If a large change (>= 10 BPM) is followed by another change within 1 measure (by time), filter it out
                let is_short_large_change = if bpm_diff >= LARGE_BPM_CHANGE_THRESHOLD {
                    // Check if there's a next tempo change within 1 measure (by time, not measure number)
                    if i + 1 < tempo_changes.len() {
                        let next_change = &tempo_changes[i + 1];
                        let time_diff_to_next = (next_change.position_seconds - change.position_seconds).abs();
                        // Estimate 1 measure duration: at current tempo, 1 measure = 4 beats (assuming 4/4)
                        // 1 measure in seconds = (60 / tempo) * 4 beats
                        let one_measure_duration = (60.0 / change.tempo) * 4.0;
                        time_diff_to_next < one_measure_duration
                    } else {
                        false // No next change, so it's not short
                    }
                } else {
                    false // Not a large change, so this check doesn't apply
                };
                
                if is_short_large_change {
                    // Large tempo change that only lasts less than 1 measure - skip it
                    continue;
                }
                
                // Check for bounce-back: if previous marker was a short large change that was filtered,
                // and this marker bounces back to something close to the original tempo, filter it too
                // (e.g., 74 -> 64 -> 77, if 64 is filtered, also filter 77 if it's close to 74)
                let is_bounce_back_from_short_change = if i > 0 {
                    let prev_change = &tempo_changes[i - 1];
                    let prev_bpm_diff = (prev_change.tempo - prev_change.prev_tempo).abs();
                    
                    // Check if previous was a large change that was short (within 1 measure)
                    if prev_bpm_diff >= LARGE_BPM_CHANGE_THRESHOLD {
                        let time_diff_from_prev = (change.position_seconds - prev_change.position_seconds).abs();
                        let one_measure_duration_prev = (60.0 / prev_change.tempo) * 4.0;
                        let prev_was_short = time_diff_from_prev < one_measure_duration_prev;
                        
                        if prev_was_short {
                            // Previous was a short large change - check if current is bouncing back
                            // Compare current tempo to the tempo BEFORE the short change (actual tempo, not filtered)
                            let original_tempo = prev_change.prev_tempo;
                            let bounce_back_diff = (change.tempo - original_tempo).abs();
                            // If bouncing back to within 5 BPM of original, filter it out
                            bounce_back_diff < MIN_BPM_DIFFERENCE_SAME_SECTION
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };
                
                if is_bounce_back_from_short_change {
                    // Bouncing back from a short large change - skip it
                    continue;
                }
                
                // For large tempo changes, check if it's a spike by comparing to average of nearby tempos
                // Look at up to 5 tempo markers within 4 measures in either direction
                let is_tempo_spike = if bpm_diff >= LARGE_BPM_CHANGE_THRESHOLD {
                    let current_measure = change.position.musical.measure;
                    let mut nearby_tempos = Vec::new();
                    
                    // Look backwards up to 4 measures (or up to 5 markers, whichever comes first)
                    for j in (0..i).rev() {
                        let other_change = &tempo_changes[j];
                        let measure_diff = (current_measure - other_change.position.musical.measure).abs();
                        if measure_diff <= 4 {
                            nearby_tempos.push(other_change.tempo);
                            if nearby_tempos.len() >= 5 {
                                break;
                            }
                        } else {
                            break; // Too far away
                        }
                    }
                    
                    // Look forwards up to 4 measures (or up to 5 markers, whichever comes first)
                    for j in (i + 1)..tempo_changes.len() {
                        let other_change = &tempo_changes[j];
                        let measure_diff = (other_change.position.musical.measure - current_measure).abs();
                        if measure_diff <= 4 {
                            nearby_tempos.push(other_change.tempo);
                            if nearby_tempos.len() >= 5 {
                                break;
                            }
                        } else {
                            break; // Too far away
                        }
                    }
                    
                    // Also include the previous tempo (before this change)
                    nearby_tempos.push(change.prev_tempo);
                    
                    if nearby_tempos.len() >= 2 {
                        // Calculate average of nearby tempos
                        let avg_tempo = nearby_tempos.iter().sum::<f64>() / nearby_tempos.len() as f64;
                        // If current tempo is within 5 BPM of the average, it's probably a spike
                        let diff_from_avg = (change.tempo - avg_tempo).abs();
                        diff_from_avg < MIN_BPM_DIFFERENCE_SAME_SECTION
                    } else {
                        false // Not enough nearby tempos to determine
                    }
                } else {
                    false // Not a large change, so this check doesn't apply
                };
                
                if is_tempo_spike {
                    // Large tempo change that's just a spike (close to average of nearby tempos) - skip it
                    continue;
                }
                
                if should_include {
                    final_filtered.push(i);
                }
            }
            
            // Sort by position to maintain order
            final_filtered.sort_by(|&a, &b| {
                tempo_changes[a].position_seconds.partial_cmp(&tempo_changes[b].position_seconds)
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            
            // Add filtered tempo markers
            for &idx in final_filtered.iter() {
                let change = &tempo_changes[idx];
                markers.push(TempoMarker {
                    position_percent: change.position_percent,
                    label: format!("{:.0}", change.tempo),
                    is_tempo: true,
                    is_time_sig: false,
                    show_line_only: false,
                });
            }
            
            // Filter time signature changes based on:
            // 1. Section boundaries: always show (with card)
            // 2. Isolation: show if there are no other time sig changes within 4 measures on either side
            const ISOLATION_DISTANCE_MEASURES: i32 = 4;
            
            let mut time_sig_filtered: Vec<usize> = Vec::new();
            let mut time_sig_line_only: Vec<usize> = Vec::new();
            
            for (i, change) in time_sig_changes.iter().enumerate() {
                // Check if this is at a section boundary (always show with card, even if there are nearby changes)
                let is_section_boundary = {
                    let change_time = change.position_seconds;
                    // Check if this time matches any section start
                    song.sections.iter().any(|section| {
                        if let Some(section_start) = section.start_seconds() {
                            let time_diff = (change_time - section_start).abs();
                            time_diff < 0.1 // Within 0.1 seconds of section start
                        } else {
                            false
                        }
                    })
                };
                
                if is_section_boundary {
                    // Always show section boundary time sig changes with card
                    time_sig_filtered.push(i);
                } else {
                    // Check if this time sig change is "alone" (no other changes within 4 measures on either side)
                    let current_measure = change.position.musical.measure;
                    let has_nearby_changes = time_sig_changes.iter().enumerate().any(|(j, other_change)| {
                        if i == j {
                            false // Don't compare with itself
                        } else {
                            let other_measure = other_change.position.musical.measure;
                            let measure_diff = (current_measure - other_measure).abs();
                            measure_diff <= ISOLATION_DISTANCE_MEASURES
                        }
                    });
                    
                    if !has_nearby_changes {
                        // Isolated change - show with card
                        time_sig_filtered.push(i);
                    } else {
                        // Has nearby changes - show only line (filtered out)
                        time_sig_line_only.push(i);
                    }
                }
            }
            
            // Add filtered time signature markers (with cards)
            for &idx in time_sig_filtered.iter() {
                let change = &time_sig_changes[idx];
                markers.push(TempoMarker {
                    position_percent: change.position_percent,
                    label: format!("{}/{}", change.time_sig.0, change.time_sig.1),
                    is_tempo: false,
                    is_time_sig: true,
                    show_line_only: false,
                });
            }
            
            // Add filtered time signature markers (line only, no card)
            for &idx in time_sig_line_only.iter() {
                let change = &time_sig_changes[idx];
                markers.push(TempoMarker {
                    position_percent: change.position_percent,
                    label: format!("{}/{}", change.time_sig.0, change.time_sig.1),
                    is_tempo: false,
                    is_time_sig: true,
                    show_line_only: true,
                });
            }
            
            markers
        }).unwrap_or_default()
    });
    
    // Compute section-specific tempo markers and measure indicators for the section progress bar
    let section_tempo_markers_and_measures = use_memo(move || {
        let song_idx = current_song_index()?;
        let setlist_structure = SETLIST_STRUCTURE.read();
        let song_transport = SONG_TRANSPORT.read();
        
        let song = setlist_structure.as_ref()?.songs.get(song_idx)?;
        let transport = song_transport.get(&song_idx)?;
        let current_pos = transport.playhead_position.time.to_seconds();
        
        // Find the current section
        let (section_idx, section) = song.section_at_position_with_index(current_pos)?;
        let section_start = section.start_seconds()?;
        let section_end = section.end_seconds()?;
        let section_duration = section_end - section_start;
        
        if section_duration <= 0.0 {
            return None;
        }
        
        // Debug: Log section info
        // Removed verbose section change logging
        
        // Get song timeline info (same as progress sections)
        let count_in_start = song.count_in_marker.as_ref()
            .map(|m| m.position.time.to_seconds())
            .unwrap_or(0.0);
        let song_start = song.effective_start();
        let ending_pos = song.render_end().max(song.hard_cut());
        let total_start = if count_in_start > 0.0 { count_in_start } else { song_start };
        let total_end = if ending_pos > song.effective_end() { ending_pos } else { song.effective_end() };
        let song_duration = total_end - total_start;
        
        // Calculate section position in song timeline (percent)
        let section_start_percent = if song_duration > 0.0 {
            ((section_start - total_start) / song_duration * 100.0).max(0.0).min(100.0)
        } else { 0.0 };
        let section_end_percent = if song_duration > 0.0 {
            ((section_end - total_start) / song_duration * 100.0).max(0.0).min(100.0)
        } else { 100.0 };
        let section_width_percent = section_end_percent - section_start_percent;
        
        // Get tempo markers for filtering
        let all_tempo_markers = tempo_markers();
        
        // Filter tempo markers to only those within this section
        let section_tempo_markers: Vec<TempoMarker> = all_tempo_markers.iter()
            .filter(|marker| {
                marker.position_percent >= section_start_percent && marker.position_percent < section_end_percent
            })
            .map(|marker| {
                // Convert from song-relative percent to section-relative percent (0-100)
                let position_in_section = marker.position_percent - section_start_percent;
                let section_relative_percent = if section_width_percent > 0.0 {
                    (position_in_section / section_width_percent) * 100.0
                } else { 0.0 };
                TempoMarker {
                    position_percent: section_relative_percent,
                    label: marker.label.clone(),
                    is_tempo: marker.is_tempo,
                    is_time_sig: marker.is_time_sig,
                    show_line_only: marker.show_line_only,
                }
            })
            .collect();
        
        // Compute measure indicators for this section using measure_positions from song
        // measure_positions are calculated on the server side with both MusicalPosition and TimePosition
        // We use TimePosition for linear time-based positioning
        let mut measure_indicators = Vec::new();
        
        // Use measure_positions from song if available
        if !song.measure_positions.is_empty() {
            // Filter measures that are within this section
            let section_start_musical = section.start_position.as_ref()?.musical.clone();
            let section_end_musical = section.end_position.as_ref()?.musical.clone();
            let start_measure = section_start_musical.measure;
            let end_measure = section_end_musical.measure;
            
            // Find measures within the section range
            let section_measures: Vec<_> = song.measure_positions.iter()
                .filter(|pos| {
                    pos.musical.measure >= start_measure && pos.musical.measure <= end_measure
                })
                .collect();
            
            if !section_measures.is_empty() {
                // Get time positions for section start and end
                let section_start_time = section_start;
                let section_end_time = section_end;
                let section_duration = section_duration;
                
                // Calculate positions based on TimePosition (linear time-based)
                for measure_pos in section_measures {
                    let measure_time = measure_pos.time.to_seconds();
                    
                    // Calculate position within section (0-100%) based on time
                    let measure_progress_in_section = if section_duration > 0.0 {
                        ((measure_time - section_start_time) / section_duration * 100.0).max(0.0).min(100.0)
                    } else {
                        0.0
                    };
                    
                    // Get time signature for this measure from tempo changes
                    let mut measure_time_sig = song.starting_time_signature
                        .map(|ts| (ts.numerator, ts.denominator))
                        .unwrap_or((4, 4));
                    
                    // Find time signature at this measure's position
                    for change in song.tempo_time_sig_changes.iter() {
                        let change_time = song.effective_start() + change.position_seconds();
                        if change_time <= measure_time {
                            if let Some((num, den)) = change.time_signature {
                                measure_time_sig = (num, den);
                            }
                        } else {
                            break;
                        }
                    }
                    
                    measure_indicators.push(MeasureIndicator {
                        position_percent: measure_progress_in_section,
                        measure_number: measure_pos.musical.measure + 1, // 1-based for display
                        time_signature: Some((measure_time_sig.0 as u8, measure_time_sig.1 as u8)),
                        musical_position: measure_pos.musical.clone(),
                    });
                }
            }
        } else if let Some(chart) = song.chart.as_ref() {
            if let Some(chart_section) = chart.sections.iter()
                .find(|cs| {
                    // Match by section name or type (SectionType implements PartialEq)
                    cs.section.name == section.name || 
                    cs.section.section_type == section.section_type
                }) {
        
                // Calculate measure positions using the same time-based logic as time signature markers
                // Use forward calculation from section start, accounting for tempo changes
                let section_start_musical = section.start_position.as_ref()?.musical.clone();
                let start_measure = section_start_musical.measure;
                
                // Helper function to calculate measure duration in seconds
                let measure_duration = |tempo: f64, time_sig: (i32, i32)| -> f64 {
                    let beats_per_measure = time_sig.0 as f64;
                    let seconds_per_beat = 60.0 / tempo;
                    beats_per_measure * seconds_per_beat
                };
                
                // Get initial tempo and time signature at section start
                let mut current_tempo = song.starting_tempo.unwrap_or(120.0);
                let mut current_time_sig = song.starting_time_signature
                    .map(|ts| (ts.numerator, ts.denominator))
                    .unwrap_or((4, 4));
                
                // Find tempo/time sig at section start
                for change in song.tempo_time_sig_changes.iter() {
                    let change_time = song.effective_start() + change.position_seconds();
                    if change_time <= section_start {
                        current_tempo = change.tempo;
                        if let Some((num, den)) = change.time_signature {
                            current_time_sig = (num, den);
                        }
                    } else {
                        break;
                    }
                }
                
                info!("  Chart section found: {} measures", chart_section.measures.len());
                info!("  Starting at measure {}", start_measure);
                
                // Helper function to calculate quarter note length of a time signature
                // Formula: (numerator / denominator) * 4 = number of quarter notes
                // This normalizes all time signatures to quarter note units
                // Examples:
                //   4/4 = (4/4) * 4 = 4.0 quarter notes (100%)
                //   3/4 = (3/4) * 4 = 3.0 quarter notes (75%)
                //   8/8 = (8/8) * 4 = 4.0 quarter notes (100%)
                //   7/8 = (7/8) * 4 = 3.5 quarter notes (87.5%)
                //   15/16 = (15/16) * 4 = 3.75 quarter notes (93.75%)
                let quarter_note_length = |time_sig: (i32, i32)| -> f64 {
                    let (num, den) = time_sig;
                    (num as f64 / den as f64) * 4.0
                };
                
                // Get initial time signature at section start
                let mut current_time_sig = song.starting_time_signature
                    .map(|ts| (ts.numerator, ts.denominator))
                    .unwrap_or((4, 4));
                
                // Find time signature at section start
                for change in song.tempo_time_sig_changes.iter() {
                    let change_time = song.effective_start() + change.position_seconds();
                    if change_time <= section_start {
                        if let Some((num, den)) = change.time_signature {
                            current_time_sig = (num, den);
                        }
                    } else {
                        break;
                    }
                }
                
                // First pass: calculate quarter note lengths for all measures
                let mut measure_quarter_note_lengths = Vec::new();
                let mut total_quarter_notes = 0.0;
                
                for (measure_idx, measure) in chart_section.measures.iter().enumerate() {
                    let measure_number_absolute = start_measure + measure_idx as i32;
                    
                    // Use measure's time signature if available, otherwise check tempo changes for this measure
                    let measure_time_sig = if measure.time_signature.0 > 0 && measure.time_signature.1 > 0 {
                        (measure.time_signature.0 as i32, measure.time_signature.1 as i32)
                    } else {
                        // Check tempo changes to find time sig for this measure
                        let mut measure_ts = current_time_sig;
                        for change in song.tempo_time_sig_changes.iter() {
                            let change_measure = change.position.musical.measure;
                            if change_measure <= measure_number_absolute {
                                if let Some((num, den)) = change.time_signature {
                                    measure_ts = (num, den);
                                }
                            } else {
                                break;
                            }
                        }
                        measure_ts
                    };
                    
                    let qn_length = quarter_note_length(measure_time_sig);
                    measure_quarter_note_lengths.push((measure_time_sig, qn_length));
                    total_quarter_notes += qn_length;
                }
                
                // Second pass: calculate positions based on cumulative quarter note lengths
                let mut cumulative_quarter_notes = 0.0;
                
                for (measure_idx, (measure_time_sig, qn_length)) in measure_quarter_note_lengths.iter().enumerate() {
                    let measure_number_absolute = start_measure + measure_idx as i32;
                    
                    // Calculate position based on cumulative quarter notes / total quarter notes
                    let measure_progress_in_section = if total_quarter_notes > 0.0 {
                        (cumulative_quarter_notes / total_quarter_notes * 100.0).max(0.0).min(100.0)
                    } else {
                        0.0
                    };
                    
                    info!("  Measure {}: percent={:.2}%, time_sig={}/{}, quarter_notes={:.2}", 
                        measure_number_absolute + 1, measure_progress_in_section, measure_time_sig.0, measure_time_sig.1, qn_length);
                    
                    // Create musical position for this measure (at measure start: beat=0, subdivision=0)
                    let measure_musical = MusicalPosition::new(measure_number_absolute, 0, 0);
                    
                    measure_indicators.push(MeasureIndicator {
                        position_percent: measure_progress_in_section,
                        measure_number: measure_number_absolute + 1, // 1-based for display
                        time_signature: Some((measure_time_sig.0 as u8, measure_time_sig.1 as u8)),
                        musical_position: measure_musical,
                    });
                    
                    // Add this measure's quarter note length to cumulative
                    cumulative_quarter_notes += qn_length;
                }
            } else {
                // Chart section not found - use quarter note-based calculation
                info!("  Chart section NOT found, using quarter note-based calculation");
                let section_start_musical = section.start_position.as_ref()?.musical.clone();
                let section_end_musical = section.end_position.as_ref()?.musical.clone();
                let start_measure = section_start_musical.measure;
                let end_measure = section_end_musical.measure;
                info!("  Measures: {} to {}", start_measure, end_measure);
                
                // Helper function to calculate quarter note length of a time signature
                let quarter_note_length = |time_sig: (i32, i32)| -> f64 {
                    let (num, den) = time_sig;
                    (num as f64 / den as f64) * 4.0
                };
                
                // Get initial time signature at section start
                let mut current_time_sig = song.starting_time_signature
                    .map(|ts| (ts.numerator, ts.denominator))
                    .unwrap_or((4, 4));
                
                // Find time signature at section start
                for change in song.tempo_time_sig_changes.iter() {
                    let change_time = song.effective_start() + change.position_seconds();
                    if change_time <= section_start {
                        if let Some((num, den)) = change.time_signature {
                            current_time_sig = (num, den);
                        }
                    } else {
                        break;
                    }
                }
                
                // Track tempo change index to find time sig for each measure
                let mut tempo_change_idx = 0;
                
                // First pass: calculate quarter note lengths for all measures
                let mut measure_quarter_note_lengths = Vec::new();
                let mut total_quarter_notes = 0.0;
                
                for measure_num in start_measure..=end_measure {
                    // Find time signature for this measure
                    // We need to check tempo changes up to this measure's position
                    // For simplicity, we'll use the time sig at the start of each measure
                    // by checking which tempo changes occur before this measure
                    let mut measure_time_sig = current_time_sig;
                    
                    // Reset and find time sig for this measure
                    let mut check_idx = 0;
                    for change in song.tempo_time_sig_changes.iter() {
                        let change_measure = change.position.musical.measure;
                        if change_measure <= measure_num {
                            if let Some((num, den)) = change.time_signature {
                                measure_time_sig = (num, den);
                            }
                            check_idx += 1;
                        } else {
                            break;
                        }
                    }
                    
                    let qn_length = quarter_note_length(measure_time_sig);
                    measure_quarter_note_lengths.push((measure_time_sig, qn_length));
                    total_quarter_notes += qn_length;
                }
                
                // Second pass: calculate positions based on cumulative quarter note lengths
                let mut cumulative_quarter_notes = 0.0;
                
                for (idx, (measure_time_sig, qn_length)) in measure_quarter_note_lengths.iter().enumerate() {
                    let measure_num = start_measure + idx as i32;
                    
                    // Calculate position based on cumulative quarter notes / total quarter notes
                    let measure_progress_in_section = if total_quarter_notes > 0.0 {
                        (cumulative_quarter_notes / total_quarter_notes * 100.0).max(0.0).min(100.0)
                    } else {
                        0.0
                    };
                    
                    info!("  Measure {}: percent={:.2}%, time_sig={}/{}, quarter_notes={:.2}", 
                        measure_num + 1, measure_progress_in_section, measure_time_sig.0, measure_time_sig.1, qn_length);
                    
                    // Create musical position for this measure (at measure start: beat=0, subdivision=0)
                    let measure_musical = MusicalPosition::new(measure_num, 0, 0);
                    
                    measure_indicators.push(MeasureIndicator {
                        position_percent: measure_progress_in_section,
                        measure_number: measure_num + 1, // 1-based for display
                        time_signature: Some((measure_time_sig.0 as u8, measure_time_sig.1 as u8)),
                        musical_position: measure_musical,
                    });
                    
                    // Add this measure's quarter note length to cumulative
                    cumulative_quarter_notes += qn_length;
                }
            }
        } else {
            // No chart data - use quarter note-based calculation
            info!("  No chart data, using quarter note-based calculation");
            let section_start_musical = section.start_position.as_ref()?.musical.clone();
            let section_end_musical = section.end_position.as_ref()?.musical.clone();
            let start_measure = section_start_musical.measure;
            let end_measure = section_end_musical.measure;
            info!("  Measures: {} to {}", start_measure, end_measure);
            
            // Helper function to calculate quarter note length of a time signature
            let quarter_note_length = |time_sig: (i32, i32)| -> f64 {
                let (num, den) = time_sig;
                (num as f64 / den as f64) * 4.0
            };
            
            // Get initial time signature at section start
            let mut current_time_sig = song.starting_time_signature
                .map(|ts| (ts.numerator, ts.denominator))
                .unwrap_or((4, 4));
            
            // Find time signature at section start
            for change in song.tempo_time_sig_changes.iter() {
                let change_time = song.effective_start() + change.position_seconds();
                if change_time <= section_start {
                    if let Some((num, den)) = change.time_signature {
                        current_time_sig = (num, den);
                    }
                } else {
                    break;
                }
            }
            
            // First pass: calculate quarter note lengths for all measures
            let mut measure_quarter_note_lengths = Vec::new();
            let mut total_quarter_notes = 0.0;
            
            for measure_num in start_measure..=end_measure {
                // Find time signature for this measure by checking tempo changes
                let mut measure_time_sig = current_time_sig;
                
                for change in song.tempo_time_sig_changes.iter() {
                    let change_measure = change.position.musical.measure;
                    if change_measure <= measure_num {
                        if let Some((num, den)) = change.time_signature {
                            measure_time_sig = (num, den);
                        }
                    } else {
                        break;
                    }
                }
                
                let qn_length = quarter_note_length(measure_time_sig);
                measure_quarter_note_lengths.push((measure_time_sig, qn_length));
                total_quarter_notes += qn_length;
            }
            
            // Second pass: calculate positions based on cumulative quarter note lengths
            let mut cumulative_quarter_notes = 0.0;
            
            for (idx, (measure_time_sig, qn_length)) in measure_quarter_note_lengths.iter().enumerate() {
                let measure_num = start_measure + idx as i32;
                
                // Calculate position based on cumulative quarter notes / total quarter notes
                let measure_progress_in_section = if total_quarter_notes > 0.0 {
                    (cumulative_quarter_notes / total_quarter_notes * 100.0).max(0.0).min(100.0)
                } else {
                    0.0
                };
                
                info!("  Measure {}: percent={:.2}%, time_sig={}/{}, quarter_notes={:.2}", 
                    measure_num + 1, measure_progress_in_section, measure_time_sig.0, measure_time_sig.1, qn_length);
                
                // Create musical position for this measure (at measure start: beat=0, subdivision=0)
                let measure_musical = MusicalPosition::new(measure_num, 0, 0);
                
                measure_indicators.push(MeasureIndicator {
                    position_percent: measure_progress_in_section,
                    measure_number: measure_num + 1, // 1-based for display
                    time_signature: Some((measure_time_sig.0 as u8, measure_time_sig.1 as u8)),
                    musical_position: measure_musical,
                });
                
                // Add this measure's quarter note length to cumulative
                cumulative_quarter_notes += qn_length;
            }
        }
        
        Some((section_tempo_markers, measure_indicators))
    });
    
    let section_tempo_markers = use_memo(move || {
        section_tempo_markers_and_measures().as_ref()
            .map(|(markers, _): &(Vec<TempoMarker>, Vec<MeasureIndicator>)| markers.clone())
            .unwrap_or_default()
    });
    
    let section_measure_indicators = use_memo(move || {
        section_tempo_markers_and_measures().as_ref()
            .map(|(_, measures): &(Vec<TempoMarker>, Vec<MeasureIndicator>)| measures.clone())
            .unwrap_or_default()
    });
    
    // Get section progress from SetlistApi (computed on server side)
    // Convert from 0.0-1.0 to 0-100 for display
    // Use reactive signal that directly reads from SETLIST
    let mut section_progress = use_signal(|| {
        SETLIST.read().as_ref()
            .and_then(|api| api.section_progress)
            .map(|p| p * 100.0) // Convert 0.0-1.0 to 0-100
            .unwrap_or(0.0)
    });
    
    // Update section_progress signal reactively when SETLIST changes
    use_effect(move || {
        // Reading SETLIST here makes this effect reactive to SETLIST changes
        let _ = SETLIST.read();
        let progress_value = SETLIST.read().as_ref()
            .and_then(|api| api.section_progress);
        
        // Log the raw progress value (0.0-1.0) for debugging
        if let Some(p) = progress_value {
            static LAST_SECTION_PROGRESS: std::sync::Mutex<Option<f64>> = std::sync::Mutex::new(None);
            let mut last = LAST_SECTION_PROGRESS.lock().unwrap();
            if *last != Some(p) {
                info!("section_progress: {:.4} (0.0-1.0)", p);
                *last = Some(p);
            }
        }
        
        let new_progress = progress_value.map(|p| p * 100.0).unwrap_or(0.0);
        section_progress.set(new_progress);
    });
    
    // Compute detail badges using transport info from the song itself
    // Only show badges when we have actual transport info (not defaults)
    // Reactive signal for detail badges - reads from SONG_TRANSPORT to get real-time updates
    let detail_badges = use_memo(move || {
        let song_idx = current_song_index()?;
        let setlist_structure = SETLIST_STRUCTURE.read();
        let song_transport = SONG_TRANSPORT.read(); // This read makes it reactive to SONG_TRANSPORT changes
        
        let song = setlist_structure.as_ref()?.songs.get(song_idx)?;
        let transport = song_transport.get(&song_idx)?;
        
        // Use musical position directly from REAPER (which uses TimeMap2_timeToBeats internally)
        // This correctly accounts for tempo and time signature changes
        let musical_pos = transport.playhead_position.musical.clone();
        
        // Format the values
        let musical_str = format!("{}.{}.{:03}", musical_pos.measure + 1, musical_pos.beat + 1, musical_pos.subdivision);
        
        // Calculate song-relative time position for time display
        let song_start = if song.effective_start() > 0.0 {
            song.effective_start()
        } else {
            song.sections.first().and_then(|s| s.start_seconds()).unwrap_or(0.0)
        };
        let current_pos = transport.playhead_position.time.to_seconds();
        let song_relative_position = (current_pos - song_start).max(0.0);
        let time_pos = TimePosition::from_seconds(song_relative_position);
        
        // Use transport info from SONG_TRANSPORT (bpm, time sig) for display - reactive!
        // Show BPM rounded to 2 decimal places
        let tempo = transport.tempo.bpm;
        let time_sig = transport.time_signature.clone();
        let time_str = format!("{}:{:02}.{:03}", time_pos.minutes, time_pos.seconds, time_pos.milliseconds);
        let tempo_str = format!("{:.2}", tempo); // Round to 2 decimal places
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
                            progress: progress,
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
                    // Section Progress Bar (positioned below BPM markers, with room for labels)
                    div {
                        class: "absolute left-0 right-0",
                        style: "top: calc(50% + 6.5rem);",
                        div {
                            class: "w-full px-4",
                            SectionProgressBar {
                                progress: section_progress,
                                sections: progress_sections(),
                                tempo_markers: section_tempo_markers(),
                                measure_indicators: section_measure_indicators(),
                                song_key: song_position.map(|i| i.to_string()),
                                on_measure_click: {
                                    let on_seek = on_seek_to_musical_position.clone();
                                    let song_idx_signal = current_song_index.clone();
                                    if let Some(seek_callback) = on_seek {
                                        Some(Callback::new(move |musical_pos: MusicalPosition| {
                                            if let Some(song_index) = song_idx_signal() {
                                                seek_callback.call((song_index, musical_pos));
                                            }
                                        }))
                                    } else {
                                        None
                                    }
                                },
                            }
                        }
                    }
                    // Detail Badges Component (positioned below section progress bar)
                    if let Some(badges) = detail_badges() {
                        div {
                            class: "absolute left-0 right-0",
                            style: "top: calc(50% + 10.5rem);",
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

