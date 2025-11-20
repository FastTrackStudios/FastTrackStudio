use dioxus::prelude::*;
use setlist::Setlist;
use primitives::{Position, MusicalPosition, TimePosition, TimeSignature};
use marker_region::application::TempoTimePoint;
use crate::utils::{calculate_song_progress, calculate_section_progress, get_section_color_bright, get_project_name, get_tempo_at_position, get_time_signature_at_position, format_musical_position, format_time_position};
use std::collections::HashMap;

/// Progress bar section definition
#[derive(Clone, Debug, PartialEq)]
pub struct ProgressSection {
    pub start_percent: f64,  // 0-100, where this section starts
    pub end_percent: f64,    // 0-100, where this section ends
    pub color: String,       // Tailwind color class or CSS color
    pub name: String,        // Name to display inside the section
}

/// Segmented progress bar component with different colored sections
#[component]
pub fn SegmentedProgressBar(progress: Signal<f64>, sections: Vec<ProgressSection>) -> Element {
    let current_progress = progress();
    
    // Pre-calculate section data
    let section_data: Vec<_> = sections.iter().enumerate().map(|(index, section)| {
        let section_start = section.start_percent;
        let section_end = section.end_percent;
        let section_width = section_end - section_start;
        
        // Calculate how much of this section is filled
        let filled_percent = if current_progress <= section_start {
            0.0
        } else if current_progress >= section_end {
            100.0
        } else {
            // Progress is within this section
            let progress_in_section = current_progress - section_start;
            (progress_in_section / section_width) * 100.0
        };
        
        (index, section_start, section_width, section.color.clone(), filled_percent, section.name.clone())
    }).collect();
    
    // Find the active section (the one we're currently in)
    let active_section = section_data.iter().find(|(_, start, end, _, _, _)| {
        current_progress >= *start && current_progress <= *end
    }).or_else(|| {
        // If we're past all sections, use the last one
        section_data.last()
    });
    
    // Calculate section progress (0-100% within the active section)
    let section_progress = if let Some((_, section_start, section_width, _, _, _)) = active_section {
        if current_progress <= *section_start {
            0.0
        } else if current_progress >= *section_start + *section_width {
            100.0
        } else {
            let progress_in_section = current_progress - section_start;
            (progress_in_section / section_width) * 100.0
        }
    } else {
        0.0
    };
    
    let active_color = active_section.map(|(_, _, _, color, _, _)| color.clone()).unwrap_or_else(|| "rgb(100, 100, 100)".to_string());
    
    rsx! {
        div {
            class: "flex flex-col items-center justify-center h-24 w-full px-4",
            // Main segmented progress bar
            div {
                class: "relative w-full h-16 rounded-lg overflow-hidden bg-secondary",
                // Render each section
                for (index, section_start, section_width, section_color, filled_percent, section_name) in section_data.iter() {
                    div {
                        key: "{index}",
                        class: "absolute h-full",
                        style: format!(
                            "left: {}%; width: {}%;",
                            section_start,
                            section_width
                        ),
                        // Background (unfilled portion)
                        div {
                            class: "absolute inset-0 h-full",
                            style: format!(
                                "background-color: {}; opacity: 0.3;",
                                section_color
                            ),
                        }
                        // Filled portion
                        div {
                            class: "absolute left-0 top-0 h-full transition-all duration-300 ease-in-out",
                            style: format!(
                                "width: {}%; background-color: {}; opacity: 0.8;",
                                filled_percent,
                                section_color
                            ),
                        }
                        // Section name text
                        div {
                            class: "absolute inset-0 flex items-center justify-center text-xs font-medium text-white pointer-events-none",
                            style: "text-shadow: 0 1px 2px rgba(0, 0, 0, 0.5);",
                            "{section_name}"
                        }
                    }
                }
            }
            // Section progress bar (shows progress within current section)
            div {
                class: "relative w-full h-2 rounded-lg overflow-hidden bg-secondary mt-2",
                // Background
                div {
                    class: "absolute inset-0 h-full",
                    style: format!(
                        "background-color: {}; opacity: 0.3;",
                        active_color
                    ),
                }
                // Filled portion
                div {
                    class: "absolute left-0 top-0 h-full transition-all duration-300 ease-in-out",
                    style: format!(
                        "width: {}%; background-color: {}; opacity: 0.8;",
                        section_progress,
                        active_color
                    ),
                }
            }
        }
    }
}

/// Compact progress bar component for song/section cards
#[component]
pub fn CompactProgressBar(
    label: String,
    progress: f64,
    bright_color: String,
    muted_color: String,
    is_selected: bool,
    #[props(default = false)]
    is_inactive: bool,
    #[props(default = false)]
    always_black_bg: bool,
    #[props(default)]
    on_click: Option<Callback<MouseEvent>>,
) -> Element {
    rsx! {
        div {
            class: "relative h-12 rounded-lg overflow-hidden cursor-pointer transition-all",
            class: if is_selected { "ring-2 ring-primary" } else { "" },
            onclick: move |e| {
                if let Some(callback) = &on_click {
                    callback.call(e);
                }
            },
            // Background - dark grey if always_black_bg or inactive, colored otherwise
            div {
                class: "absolute inset-0",
                style: if always_black_bg || is_inactive {
                    "background-color: var(--color-card);".to_string()
                } else {
                    format!("background-color: {}; opacity: 0.3;", muted_color)
                },
            }
            // Filled portion - only show if not inactive
            if !is_inactive {
                div {
                    class: "absolute left-0 top-0 h-full transition-all duration-300 ease-in-out",
                    style: format!(
                        "width: {}%; background-color: {}; opacity: 0.8;",
                        progress.max(0.0).min(100.0),
                        bright_color
                    ),
                }
            }
            // Colored band on the right side for songs (when always_black_bg is true)
            if always_black_bg {
                div {
                    class: "absolute right-0 top-0 bottom-0 w-1",
                    style: format!("background-color: {};", bright_color),
                }
            }
            // Label text - white if always_black_bg, colored if inactive, white otherwise
            div {
                class: "absolute inset-0 flex items-center px-3 text-sm font-medium pointer-events-none",
                style: if always_black_bg {
                    "color: white; text-shadow: 0 1px 2px rgba(0, 0, 0, 0.5);".to_string()
                } else if is_inactive {
                    format!("color: {};", bright_color)
                } else {
                    "color: white; text-shadow: 0 1px 2px rgba(0, 0, 0, 0.5);".to_string()
                },
                "{label}"
            }
        }
    }
}

/// Song progress bar component using segmented progress bar
#[component]
pub fn SongProgressBar(setlist: Setlist, current_song_index: Option<usize>, transport_positions: HashMap<String, f64>, on_section_click: Callback<usize>) -> Element {
    // Track the song index to detect changes and disable transitions on song switch
    let mut prev_song_index = use_signal(|| current_song_index);
    let mut should_animate = use_signal(|| false); // Start with no animation
    
    // Check if song changed immediately (synchronous check)
    let song_changed = prev_song_index() != current_song_index;
    
    if song_changed {
        // Song changed - disable animations IMMEDIATELY for instant switch
        should_animate.set(false);
        prev_song_index.set(current_song_index);
        
        // Re-enable animations after a brief delay for smooth updates within the same song
        spawn(async move {
            #[cfg(target_arch = "wasm32")]
            {
                use wasm_bindgen::prelude::*;
                use wasm_bindgen_futures::JsFuture;
                let promise = js_sys::Promise::new(&mut |resolve, _| {
                    web_sys::window()
                        .unwrap()
                        .request_animation_frame(&resolve)
                        .unwrap();
                });
                JsFuture::from(promise).await.ok();
            }
            #[cfg(not(target_arch = "wasm32"))]
            {
                std::thread::sleep(std::time::Duration::from_millis(50));
            }
            should_animate.set(true);
        });
    } else if !should_animate() {
        // Same song - enable animations for smooth updates
        should_animate.set(true);
    }
    
    // Get current song
    let current_song = current_song_index
        .and_then(|idx| setlist.songs.get(idx));
    
    if current_song.is_none() {
        return rsx! {
            div {
                class: "text-center py-4 text-muted-foreground",
                "No song selected"
            }
        };
    }
    
    let song = current_song.unwrap();
    
    // If song has no sections, show empty state
    if song.sections.is_empty() {
        return rsx! {
            div {
                class: "text-center py-4 text-muted-foreground",
                "No sections in song"
            }
        };
    }
    
    // Calculate total song duration
    // If song has no markers, calculate from sections
    let song_start = if song.effective_start() > 0.0 {
        song.effective_start()
    } else {
        // Fall back to first section start
        song.sections.first()
            .map(|s| s.start_seconds())
            .unwrap_or(0.0)
    };
    
    let song_end = if song.effective_end() > 0.0 {
        song.effective_end()
    } else {
        // Fall back to last section end
        song.sections.last()
            .map(|s| s.end_seconds())
            .unwrap_or(0.0)
    };
    
    let song_duration = song_end - song_start;
    
    if song_duration <= 0.0 {
        return rsx! {
            div {
                class: "text-center py-4 text-muted-foreground",
                "Invalid song duration"
            }
        };
    }
    
    // Get transport position for this song's project
    let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
    let current_position = transport_positions.get(&project_name).copied().unwrap_or(0.0);
    
    // Calculate overall song progress (0-100%)
    let song_progress = calculate_song_progress(song, current_position);
    
    // Convert song sections to ProgressSection format
    let sections: Vec<ProgressSection> = song.sections.iter().map(|section| {
        let section_start = section.start_seconds();
        let section_end = section.end_seconds();
        
        // Calculate percentage positions relative to song start
        let start_percent = ((section_start - song_start) / song_duration) * 100.0;
        let end_percent = ((section_end - song_start) / song_duration) * 100.0;
        
        // Get abbreviation for display
        let abbreviation = section.section_type.abbreviation();
        
        ProgressSection {
            start_percent: start_percent.max(0.0),
            end_percent: end_percent.min(100.0),
            color: get_section_color_bright(section),
            name: abbreviation,
        }
    }).collect();
    
    // Determine the active section for the section progress bar
    let active_section_progress = if let Some(active_section) = song.sections.iter().find(|s| s.contains_position(current_position)) {
        calculate_section_progress(active_section, current_position)
    } else {
        0.0
    };

    let active_color = if let Some(active_section) = song.sections.iter().find(|s| s.contains_position(current_position)) {
        get_section_color_bright(active_section)
    } else {
        "rgb(100, 100, 100)".to_string() // Default gray if no active section
    };
    
    // Pre-calculate section rendering data for visual overlays
    let song_idx_str = current_song_index.map(|i| i.to_string()).unwrap_or_else(|| "none".to_string());
    
    // Filter and prepare tempo/time signature markers (remove redundant ones)
    // Similar to TypeScript: track previous values and only show actual changes
    let mut tempo_markers: Vec<&TempoTimePoint> = Vec::new();
    let mut prev_tempo: Option<f64> = None;
    let mut prev_time_sig: Option<(i32, i32)> = None;
    
    // Sort tempo changes by position to ensure correct order
    let mut sorted_changes: Vec<&TempoTimePoint> = song.tempo_time_sig_changes.iter().collect();
    sorted_changes.sort_by(|a, b| a.position.partial_cmp(&b.position).unwrap_or(std::cmp::Ordering::Equal));
    
    // Establish baseline from first marker (for label building)
    let mut baseline_tempo: Option<f64> = None;
    let mut baseline_time_sig: Option<(i32, i32)> = None;
    if let Some(first_point) = sorted_changes.first() {
        if first_point.tempo > 0.0 {
            baseline_tempo = Some(first_point.tempo);
        }
        if let Some(time_sig) = first_point.time_signature {
            baseline_time_sig = Some(time_sig);
        }
    }
    
    for point in sorted_changes.iter() {
        // Check what actually changed
        let current_tempo = if point.tempo > 0.0 { Some(point.tempo) } else { None };
        let current_time_sig = point.time_signature;
        
        let tempo_changed = match (prev_tempo, current_tempo) {
            (Some(prev), Some(curr)) => (prev - curr).abs() > 0.01,
            (None, Some(_)) => false, // First marker establishes baseline, don't show it
            (Some(_), None) => false, // Can't change to None
            (None, None) => false,
        };
        
        let time_sig_changed = match (prev_time_sig, current_time_sig) {
            (Some(prev), Some(curr)) => prev != curr,
            (None, Some(_)) => false, // First marker establishes baseline, don't show it
            (Some(_), None) => false, // Can't change to None
            (None, None) => false,
        };
        
        // Only include if tempo or time signature actually changed from previous
        if tempo_changed || time_sig_changed {
            tempo_markers.push(point);
        }
        
        // Update previous values for next iteration
        if let Some(tempo) = current_tempo {
            prev_tempo = Some(tempo);
        }
        if let Some(time_sig) = current_time_sig {
            prev_time_sig = Some(time_sig);
        }
    }
    
    // Convert tempo markers to percentage positions and format badge text
    // Track previous values starting from baseline
    let mut prev_tempo_for_label = baseline_tempo;
    let mut prev_time_sig_for_label = baseline_time_sig;
    
    let tempo_marker_data: Vec<_> = tempo_markers.iter().enumerate().map(|(idx, point)| {
        let marker_position_seconds = point.position; // Already song-relative
        let marker_percent = ((marker_position_seconds - song_start) / song_duration * 100.0).max(0.0).min(100.0);
        
        // Build label based on what changed from previous marker
        let mut label_parts = Vec::new();
        
        let current_tempo = if point.tempo > 0.0 { Some(point.tempo) } else { None };
        let current_time_sig = point.time_signature;
        
        // Check if time signature changed (add first)
        if let Some((n, d)) = current_time_sig {
            if let Some(prev_sig) = prev_time_sig_for_label {
                if prev_sig != (n, d) {
                    label_parts.push(format!("{}/{}", n, d));
                }
            }
        }
        
        // Check if tempo changed (add second)
        if let Some(tempo) = current_tempo {
            if let Some(prev) = prev_tempo_for_label {
                if (prev - tempo).abs() > 0.01 {
                    label_parts.push(format!("{:.0} bpm", tempo));
                }
            }
        }
        
        // Update previous values for next iteration
        if let Some(tempo) = current_tempo {
            prev_tempo_for_label = Some(tempo);
        }
        if let Some(time_sig) = current_time_sig {
            prev_time_sig_for_label = Some(time_sig);
        }
        
        let badge_text = label_parts.join(" ");
        let marker_key = format!("tempo-marker-{}-{}", song_idx_str, idx);
        (marker_key, marker_percent, badge_text)
    }).filter(|(_, _, badge_text)| !badge_text.is_empty()).collect();
    let section_render_data: Vec<_> = sections.iter().enumerate().map(|(index, section)| {
        let section_start = section.start_percent;
        let section_end = section.end_percent;
        let section_width = section_end - section_start;
        
        // Include song index in key for uniqueness
        let section_key = format!("{}-{}", song_idx_str, index);
        let text_key = format!("text-{}-{}", song_idx_str, index);
        (section_key, text_key, index, section_start, section_width, section.color.clone(), section.name.clone())
    }).collect();
    
    rsx! {
        div {
            class: "flex flex-col items-center justify-center h-24 w-full px-4",
            // Tempo/Time Signature labels (above progress bar, like TypeScript)
            if !tempo_marker_data.is_empty() {
                div {
                    class: "relative mb-6 h-6 w-full",
                    for (marker_key, marker_percent, badge_text) in tempo_marker_data.iter() {
                        div {
                            key: "{marker_key}",
                            class: "absolute",
                            style: format!("left: {}%; transform: translateX(-50%); top: 0;", marker_percent),
                            div {
                                class: "text-xs font-medium text-center whitespace-nowrap px-1 py-0.5 rounded bg-accent text-accent-foreground border border-border",
                                "{badge_text}"
                            }
                        }
                    }
                }
            }
            // Main segmented progress bar
            div {
                class: "relative w-full h-16 rounded-lg overflow-visible bg-secondary",
                // Render sections as background layers (visual only, no individual progress bars)
                for (section_key, _text_key, section_idx, section_start, section_width, section_color, _section_name) in section_render_data.iter() {
                    div {
                        key: "{section_key}",
                        class: "absolute h-full cursor-pointer z-0",
                        style: format!(
                            "left: {}%; width: {}%;",
                            section_start,
                            section_width
                        ),
                        onclick: {
                            let callback = on_section_click.clone();
                            let idx = *section_idx;
                            move |_| {
                                callback.call(idx);
                            }
                        },
                        // Section background (full color, will be brightened by progress overlay)
                        div {
                            class: "absolute inset-0 h-full",
                            style: format!(
                                "background-color: {};",
                                section_color
                            ),
                        }
                    }
                }
                // Dark overlay covering unfilled portion (reveals bright colors as progress increases)
                // pointer-events-none so clicks pass through to sections
                // Disable transition on initial render for instant switch between songs
                div {
                    class: if should_animate() {
                        "absolute left-0 top-0 h-full transition-all duration-300 ease-in-out z-10 pointer-events-none"
                    } else {
                        "absolute left-0 top-0 h-full z-10 pointer-events-none"
                    },
                    style: {
                        let progress = song_progress.max(0.0).min(100.0);
                        // Dark overlay starts from the progress point and covers the rest
                        format!(
                            "left: {}%; width: {}%; background-color: rgba(0, 0, 0, 0.6);",
                            progress,
                            100.0 - progress
                        )
                    },
                }
                // Section name text (rendered separately above everything, never darkened)
                for (_section_key, text_key, _section_idx, section_start, section_width, _section_color, section_name) in section_render_data.iter() {
                    div {
                        key: "{text_key}",
                        class: "absolute h-full pointer-events-none z-30",
                        style: format!(
                            "left: {}%; width: {}%;",
                            section_start,
                            section_width
                        ),
                        div {
                            class: "absolute inset-0 flex items-center justify-center text-xs font-medium text-white",
                            style: "text-shadow: 0 1px 2px rgba(0, 0, 0, 0.5);",
                            "{section_name}"
                        }
                    }
                }
                // Tempo/Time Signature markers (vertical lines extending above and through progress bar)
                for (marker_key, marker_percent, _badge_text) in tempo_marker_data.iter() {
                    div {
                        key: "{marker_key}",
                        class: "absolute pointer-events-none",
                        style: format!(
                            "left: {}%; width: 2px; top: -1rem; bottom: 0; background-color: rgba(255, 255, 255, 0.7); z-index: 40;",
                            marker_percent
                        ),
                    }
                }
            }
            // Section progress bar (shows progress within current section)
            div {
                class: "relative w-full h-2 rounded-lg overflow-hidden bg-secondary mt-2",
                // Background
                div {
                    class: "absolute inset-0 h-full",
                    style: format!(
                        "background-color: {}; opacity: 0.3;",
                        active_color
                    ),
                }
                // Filled portion
                div {
                    class: "absolute left-0 top-0 h-full transition-all duration-300 ease-in-out",
                    style: format!(
                        "width: {}%; background-color: {}; opacity: 0.8;",
                        active_section_progress,
                        active_color
                    ),
                }
            }
        }
    }
}

/// Detail badges component showing musical position, time position, tempo, and time signature
#[component]
pub fn DetailBadges(song: setlist::Song, current_position: f64) -> Element {
    // Calculate song-relative position
    let song_start = if song.effective_start() > 0.0 {
        song.effective_start()
    } else {
        song.sections.first()
            .map(|s| s.start_seconds())
            .unwrap_or(0.0)
    };
    
    // Position relative to song start (for tempo/time sig lookup)
    let song_relative_position = (current_position - song_start).max(0.0);
    
    // Get tempo and time signature at current position (relative to song start)
    let tempo = get_tempo_at_position(&song, song_relative_position);
    let time_sig = get_time_signature_at_position(&song, song_relative_position);
    
    // Calculate musical position from time position
    // Convert the song-relative time position to musical position using tempo and time signature
    let time_pos = TimePosition::from_seconds(song_relative_position);
    let musical_pos = time_pos.to_musical_position(tempo, time_sig.clone());
    
    // Format the values
    let musical_str = format_musical_position(&musical_pos);
    let time_str = format_time_position(&time_pos);
    let tempo_str = format!("{:.0} bpm", tempo);
    let time_sig_str = format!("{}/{}", time_sig.numerator, time_sig.denominator);
    
    rsx! {
        div {
            class: "flex flex-wrap gap-2 justify-center",
            // Musical Position badge (value only)
            div {
                class: "px-3 py-1 rounded-md bg-accent text-accent-foreground text-sm font-mono border border-border",
                "{musical_str}"
            }
            // Time Position badge (value only)
            div {
                class: "px-3 py-1 rounded-md bg-accent text-accent-foreground text-sm font-mono border border-border",
                "{time_str}"
            }
            // Tempo badge (value only)
            div {
                class: "px-3 py-1 rounded-md bg-accent text-accent-foreground text-sm font-mono border border-border",
                "{tempo_str}"
            }
            // Time Signature badge (value only)
            div {
                class: "px-3 py-1 rounded-md bg-accent text-accent-foreground text-sm font-mono border border-border",
                "{time_sig_str}"
            }
        }
    }
}

