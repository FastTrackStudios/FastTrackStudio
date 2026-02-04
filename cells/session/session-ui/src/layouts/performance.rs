//! Performance View Layout
//!
//! Complete performance view layout with sidebar navigation and main progress display.
//! This is a reusable layout component that apps can import and use directly.
//!
//! Based on the FastTrackStudio desktop app performance view.
//! Uses Tailwind CSS for styling.
//!
//! Components call `SetlistService` methods directly via `Session::get().setlist()`.
//! The consuming app must call `Session::init()` during startup.

use crate::components::*;
use crate::signals::*;
use dioxus::prelude::*;
use session_proto::setlist::Setlist;
use session_proto::song::Song;

/// Performance view layout
///
/// Complete performance view with:
/// - Left sidebar: Song/section navigator with progress bars
/// - Right main area: Large progress visualization with transport controls
///
/// This component subscribes to global signals and provides a complete
/// performance view interface that apps can use directly.
///
/// # Example
///
/// ```rust,no_run
/// use session_ui::PerformanceLayout;
/// use dioxus::prelude::*;
///
/// fn App() -> Element {
///     rsx! {
///         PerformanceLayout {}
///     }
/// }
/// ```
#[component]
pub fn PerformanceLayout() -> Element {
    // Read global signals directly - this ensures proper reactivity
    // Each read creates a subscription that will trigger re-render on change
    let setlist = SETLIST_STRUCTURE.read().clone();
    let indices = ACTIVE_INDICES.read().clone();
    let song_transport = SONG_TRANSPORT.read().clone();
    let playback_state = *PLAYBACK_STATE.read();

    // Derive current song from indices and setlist
    let current_song = indices
        .song_index
        .and_then(|idx| setlist.songs.get(idx).cloned());

    // Derive current section from indices and current song
    let current_section = current_song.as_ref().and_then(|song| {
        indices
            .section_index
            .and_then(|idx| song.sections.get(idx).cloned())
    });

    let is_playing = matches!(
        playback_state,
        daw_proto::PlayState::Playing | daw_proto::PlayState::Recording
    );

    let is_looping = indices.looping;

    // Get current transport state
    let current_transport = indices
        .song_index
        .and_then(|idx| song_transport.get(&idx).cloned());

    rsx! {
        div {
            class: "flex h-full w-full bg-background text-foreground",

            // Sidebar (1/3 width)
            PerformanceSidebar {
                setlist: setlist.clone(),
                active_song_index: indices.song_index,
                active_section_index: indices.section_index,
                song_transport: song_transport.clone(),
            }

            // Main content (2/3 width)
            PerformanceMainContent {
                current_song: current_song.clone(),
                current_section: current_section.clone(),
                active_indices: indices,
                transport_state: current_transport,
                is_playing: is_playing,
                is_looping: is_looping,
            }
        }
    }
}

/// Performance sidebar component
///
/// Displays song list with expandable sections and progress bars.
/// Shows progress for ALL songs that have transport state (independent playback).
#[component]
fn PerformanceSidebar(
    setlist: Setlist,
    active_song_index: Option<usize>,
    active_section_index: Option<usize>,
    song_transport: std::collections::HashMap<usize, TransportState>,
) -> Element {
    // Convert setlist to sidebar items, including per-song playing state
    let sidebar_items: Vec<_> = setlist
        .songs
        .iter()
        .enumerate()
        .map(|(song_idx, song)| {
            let transport = song_transport.get(&song_idx);
            let position_seconds = transport
                .and_then(|t| t.position.time.map(|time| time.as_seconds()))
                .unwrap_or(0.0);
            let song_progress = transport
                .map(|_| song.progress(position_seconds))
                .unwrap_or(0.0);
            let is_song_playing = transport.map(|t| t.is_playing).unwrap_or(false);

            let sections = song
                .sections
                .iter()
                .map(|section| {
                    let section_progress = section.progress(position_seconds);

                    SectionItem {
                        label: section.display_name(),
                        progress: section_progress,
                        bright_color: section.bright_color(),
                        muted_color: section.muted_color(),
                    }
                })
                .collect();

            (
                SongItemData {
                    label: song.name.clone(),
                    progress: song_progress,
                    bright_color: song.bright_color(),
                    muted_color: song.muted_color(),
                    sections,
                },
                is_song_playing,
            )
        })
        .collect();

    rsx! {
        div {
            class: "w-1/3 border-r border-border bg-sidebar overflow-y-auto",

            // Header
            div {
                class: "p-4",
                h2 {
                    class: "text-xl font-bold text-sidebar-foreground mb-4",
                    "Navigator"
                }
            }

            // Song list
            div {
                class: "space-y-1 pr-4 pb-4",

                for (song_idx, (song_data, is_song_playing)) in sidebar_items.iter().enumerate() {
                    SongItem {
                        key: "{song_idx}",
                        song_data: song_data.clone(),
                        index: song_idx,
                        is_expanded: active_song_index == Some(song_idx),
                        // Show as playing if THIS song's project is playing (independent transport)
                        is_playing: *is_song_playing,
                        current_section_index: if active_song_index == Some(song_idx) {
                            active_section_index
                        } else {
                            None
                        },
                        on_song_click: Callback::new(move |_| {
                            tracing::info!("on_song_click callback triggered for song_idx={}", song_idx);
                            spawn(async move {
                                tracing::info!("Calling seek_to_song({})", song_idx);
                                match Session::get().setlist().seek_to_song(song_idx).await {
                                    Ok(_) => tracing::info!("seek_to_song({}) completed successfully", song_idx),
                                    Err(e) => tracing::error!("seek_to_song({}) failed: {}", song_idx, e),
                                }
                            });
                        }),
                        on_section_click: Callback::new(move |section_idx| {
                            spawn(async move {
                                let _ = Session::get().setlist().seek_to_section(song_idx, section_idx).await;
                            });
                        }),
                    }
                }
            }
        }
    }
}

/// Main performance content area
///
/// Displays song title, both progress bars (song and section), transport info badges,
/// and transport controls at the bottom.
#[component]
fn PerformanceMainContent(
    current_song: Option<Song>,
    current_section: Option<session_proto::song::Section>,
    active_indices: session_proto::setlist::ActiveIndices,
    transport_state: Option<TransportState>,
    is_playing: bool,
    is_looping: bool,
) -> Element {
    // Build progress sections from song sections
    // Use actual section bounds to ensure sections fill the entire progress bar
    let progress_sections = current_song
        .as_ref()
        .map(|song| {
            // Use actual section bounds instead of song bounds to avoid gaps
            // This ensures the progress bar is fully covered by sections
            let sections_start = song
                .sections
                .first()
                .map(|s| s.start_seconds)
                .unwrap_or(song.start_seconds);
            let sections_end = song
                .sections
                .last()
                .map(|s| s.end_seconds)
                .unwrap_or(song.end_seconds);
            let sections_duration = sections_end - sections_start;

            if sections_duration <= 0.0 {
                return Vec::new();
            }

            song.sections
                .iter()
                .map(|section| {
                    // Calculate percentages relative to actual section bounds
                    let start_percent =
                        ((section.start_seconds - sections_start) / sections_duration) * 100.0;
                    let end_percent =
                        ((section.end_seconds - sections_start) / sections_duration) * 100.0;

                    ProgressSection {
                        name: section.display_name(),
                        short_name: section.short_display(),
                        start_percent: start_percent.max(0.0),
                        end_percent: end_percent.min(100.0),
                        color: section.bright_color(),
                    }
                })
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    // Calculate song progress percentage - use a signal that we update each render
    // We need to use use_signal + set pattern because SongProgressBar expects Signal<f64>
    let mut song_progress = use_signal(|| 0.0);
    let mut section_progress = use_signal(|| 0.0);

    // Calculate progress values from current props (these update on each render)
    // Progress is relative to actual section bounds (same as progress_sections calculation)
    let song_progress_value = current_song
        .as_ref()
        .and_then(|song| {
            transport_state.as_ref().map(|t| {
                let position_seconds = t.position.time.map(|time| time.as_seconds()).unwrap_or(0.0);
                // Use actual section bounds for progress calculation
                let sections_start = song
                    .sections
                    .first()
                    .map(|s| s.start_seconds)
                    .unwrap_or(song.start_seconds);
                let sections_end = song
                    .sections
                    .last()
                    .map(|s| s.end_seconds)
                    .unwrap_or(song.end_seconds);
                let sections_duration = sections_end - sections_start;

                if sections_duration <= 0.0 {
                    return 0.0;
                }

                let relative_pos = position_seconds - sections_start;
                (relative_pos / sections_duration) * 100.0
            })
        })
        .unwrap_or(0.0)
        .clamp(0.0, 100.0);

    let section_progress_value = current_section
        .as_ref()
        .and_then(|section| {
            transport_state.as_ref().map(|t| {
                let position_seconds = t.position.time.map(|time| time.as_seconds()).unwrap_or(0.0);
                let section_duration = section.end_seconds - section.start_seconds;
                if section_duration > 0.0
                    && position_seconds >= section.start_seconds
                    && position_seconds <= section.end_seconds
                {
                    ((position_seconds - section.start_seconds) / section_duration) * 100.0
                } else if position_seconds > section.end_seconds {
                    100.0
                } else {
                    0.0
                }
            })
        })
        .unwrap_or(0.0);

    // Update signals with new values (this will trigger child component updates)
    if (song_progress() - song_progress_value).abs() > 0.001_f64 {
        song_progress.set(song_progress_value);
    }
    if (section_progress() - section_progress_value).abs() > 0.001_f64 {
        section_progress.set(section_progress_value);
    }

    // Get next song info for display
    let next_song_info = {
        let setlist = SETLIST_STRUCTURE.read();
        active_indices.song_index.and_then(|idx| {
            setlist
                .songs
                .get(idx + 1)
                .map(|song| (song.name.clone(), song.bright_color()))
        })
    };

    // Song key for animation detection (changes when song changes)
    let song_key = active_indices.song_index.map(|i| i.to_string());

    // Build tempo markers from song data
    let tempo_markers: Vec<TempoMarkerData> = current_song
        .as_ref()
        .map(|song| {
            let mut markers = Vec::new();

            // Add initial tempo marker at 0%
            if let Some(tempo) = song.tempo {
                markers.push(TempoMarkerData {
                    position_percent: 0.0,
                    label: format!("{:.0} bpm", tempo),
                    is_tempo: true,
                    is_time_sig: false,
                    show_line_only: false,
                });
            }

            // Add initial time signature marker at 0%
            if let Some(ref ts) = song.time_signature {
                markers.push(TempoMarkerData {
                    position_percent: 0.0,
                    label: format!("{}/{}", ts.numerator, ts.denominator),
                    is_tempo: false,
                    is_time_sig: true,
                    show_line_only: false,
                });
            }

            markers
        })
        .unwrap_or_default();

    // Build measure indicators for the current section
    // Measure 1 starts at SONGSTART marker, so count-in measures are 0, -1, -2, etc.
    // We need to know the song's content start (SONGSTART) to calculate correct measure numbers
    let measure_indicators: Vec<MeasureIndicator> = current_section
        .as_ref()
        .map(|section| {
            let section_duration = section.end_seconds - section.start_seconds;
            let section_start = section.start_seconds;
            if section_duration <= 0.0 {
                return Vec::new();
            }

            // Get tempo from transport or song default
            let bpm = transport_state.as_ref().map(|t| t.bpm).unwrap_or(120.0);
            let time_sig_num = transport_state
                .as_ref()
                .map(|t| t.time_sig_num)
                .unwrap_or(4);
            let time_sig_denom = transport_state
                .as_ref()
                .map(|t| t.time_sig_denom)
                .unwrap_or(4);

            let seconds_per_beat = 60.0 / bpm;
            let seconds_per_measure = seconds_per_beat * time_sig_num as f64;
            let num_measures = (section_duration / seconds_per_measure).ceil() as i32;

            // Determine the content start (SONGSTART marker position)
            // If song has count_in_seconds, the content starts at song.start_seconds + count_in_seconds
            // Otherwise, content starts at song.start_seconds
            let content_start = current_song
                .as_ref()
                .map(|song| song.start_seconds + song.count_in_seconds.unwrap_or(0.0))
                .unwrap_or(0.0);

            // Calculate measure number relative to content start (SONGSTART = measure 1)
            // Measures before SONGSTART are 0, -1, -2, etc.
            let section_start_measure =
                ((section_start - content_start) / seconds_per_measure).floor() as i32;

            (0..num_measures)
                .map(|i| {
                    let position_percent =
                        (i as f64 * seconds_per_measure / section_duration) * 100.0;
                    // Calculate measure number relative to content start
                    // section_start_measure is negative for count-in sections
                    let measure_from_content_start = section_start_measure + i;

                    // Display number: measure 1 is the first measure at/after SONGSTART
                    // Count-in shows as 0, -1, -2 (or could show as "Count 1", "Count 2")
                    let display_number = if measure_from_content_start >= 0 {
                        measure_from_content_start + 1 // 1-indexed for regular measures
                    } else {
                        measure_from_content_start // 0, -1, -2 for count-in
                    };

                    MeasureIndicator {
                        position_percent: position_percent.min(100.0),
                        measure_number: display_number,
                        time_signature: Some((time_sig_num as u8, time_sig_denom as u8)),
                        // Store the measure number for goto_measure (0-indexed from SONGSTART)
                        musical_position: daw_proto::MusicalPosition::new(
                            measure_from_content_start,
                            0,
                            0,
                        ),
                    }
                })
                .collect()
        })
        .unwrap_or_default();

    // Capture song_index for closures
    let song_index_for_section_click = active_indices.song_index;

    rsx! {
        div {
            class: "flex-1 flex flex-col overflow-hidden bg-background",
            // Scrollable content area
            div {
                class: "flex-1 overflow-y-auto",
                div {
                    class: "p-6 relative flex items-center justify-center h-full",

                    // Song Title (positioned above progress bar)
                    div {
                        class: "absolute left-0 right-0",
                        style: "bottom: calc(50% + 4rem);",
                        if let Some(ref song) = current_song {
                            SongTitle {
                                song_name: song.name.clone(),
                            }
                        }
                    }

                    // Main Song Progress Bar (centered)
                    div {
                        key: "{song_key.clone().unwrap_or_else(|| \"none\".to_string())}",
                        class: "w-full px-4",
                        if !progress_sections.is_empty() {
                            SongProgressBar {
                                progress: song_progress,
                                sections: progress_sections.clone(),
                                on_section_click: Some(Callback::new(move |section_idx: usize| {
                                    if let Some(song_idx) = song_index_for_section_click {
                                        spawn(async move {
                                            let _ = Session::get().setlist().seek_to_section(song_idx, section_idx).await;
                                        });
                                    }
                                })),
                                tempo_markers: tempo_markers.clone(),
                                song_key: song_key.clone(),
                            }
                        }
                    }

                    // Section Progress Bar (positioned below song progress bar)
                    div {
                        class: "absolute left-0 right-0",
                        style: "top: calc(50% + 6.5rem);",
                        div {
                            class: "w-full px-4",
                            SectionProgressBar {
                                progress: section_progress,
                                sections: progress_sections.clone(),
                                measure_indicators: measure_indicators.clone(),
                                song_key: song_key.clone(),
                                on_measure_click: Some(Callback::new({
                                    let song_index = active_indices.song_index;
                                    move |musical_position: daw_proto::MusicalPosition| {
                                        if let Some(song_idx) = song_index {
                                            // The measure field contains the absolute measure number
                                            let measure = musical_position.measure;
                                            spawn(async move {
                                                tracing::info!(
                                                    "Going to measure {} in song {}",
                                                    measure,
                                                    song_idx
                                                );
                                                let _ = Session::get()
                                                    .setlist()
                                                    .goto_measure(song_idx, measure)
                                                    .await;
                                            });
                                        }
                                    }
                                })),
                            }
                        }
                    }

                    // Detail badges and next song (positioned below section progress bar)
                    div {
                        class: "absolute left-0 right-0",
                        style: "top: calc(50% + 10.5rem);",
                        div {
                            class: "flex flex-col items-center gap-4",

                            // Transport info badges
                            if let Some(ref transport) = transport_state {
                                {
                                    // Get time position from transport Position struct
                                    let position_seconds = transport.position.time.map(|t| t.as_seconds()).unwrap_or(0.0);
                                    let minutes = (position_seconds / 60.0).floor() as i32;
                                    let seconds = (position_seconds % 60.0).floor() as i32;
                                    let millis = ((position_seconds % 1.0) * 1000.0).floor() as i32;
                                    let time_str = format!("{}:{:02}.{:03}", minutes, seconds, millis);

                                    // Get musical position from transport Position struct
                                    // This comes from REAPER's TimeMap2_timeToBeats and properly handles tempo changes
                                    let musical_str = if let Some(ref musical) = transport.position.musical {
                                        format!("{}.{}.{:03}", musical.measure, musical.beat, musical.subdivision)
                                    } else {
                                        "1.1.000".to_string()
                                    };

                                    rsx! {
                                        div {
                                            class: "flex flex-wrap gap-3 justify-center",

                                            // Musical position badge (Measure.Beat.Subdivision)
                                            div {
                                                class: "px-4 py-2 rounded-full bg-secondary text-secondary-foreground text-base font-medium flex items-center gap-2",
                                                // Ruler icon
                                                svg {
                                                    width: "18",
                                                    height: "18",
                                                    view_box: "0 0 24 24",
                                                    fill: "none",
                                                    stroke: "currentColor",
                                                    stroke_width: "2",
                                                    stroke_linecap: "round",
                                                    stroke_linejoin: "round",
                                                    // Simple ruler icon
                                                    line { x1: "3", y1: "12", x2: "21", y2: "12" }
                                                    line { x1: "3", y1: "8", x2: "3", y2: "16" }
                                                    line { x1: "7", y1: "10", x2: "7", y2: "14" }
                                                    line { x1: "11", y1: "8", x2: "11", y2: "16" }
                                                    line { x1: "15", y1: "10", x2: "15", y2: "14" }
                                                    line { x1: "19", y1: "8", x2: "19", y2: "16" }
                                                    line { x1: "21", y1: "8", x2: "21", y2: "16" }
                                                }
                                                span {
                                                    class: "font-mono tabular-nums",
                                                    "{musical_str}"
                                                }
                                            }

                                            // Time position badge (MM:SS.mmm)
                                            div {
                                                class: "px-4 py-2 rounded-full bg-secondary text-secondary-foreground text-base font-medium flex items-center gap-2",
                                                // Clock icon
                                                svg {
                                                    width: "18",
                                                    height: "18",
                                                    view_box: "0 0 24 24",
                                                    fill: "none",
                                                    stroke: "currentColor",
                                                    stroke_width: "2",
                                                    stroke_linecap: "round",
                                                    stroke_linejoin: "round",
                                                    circle { cx: "12", cy: "12", r: "10" }
                                                    polyline { points: "12 6 12 12 16 14" }
                                                }
                                                span {
                                                    class: "font-mono tabular-nums",
                                                    "{time_str}"
                                                }
                                            }

                                            // BPM badge
                                            div {
                                                class: "px-4 py-2 rounded-full bg-secondary text-secondary-foreground text-base font-medium",
                                                "{transport.bpm:.0} BPM"
                                            }

                                            // Time signature badge
                                            div {
                                                class: "px-4 py-2 rounded-full bg-secondary text-secondary-foreground text-base font-medium",
                                                "{transport.time_sig_num}/{transport.time_sig_denom}"
                                            }

                                            // Loop indicator badge
                                            if transport.is_looping {
                                                div {
                                                    class: "px-4 py-2 rounded-full bg-yellow-500/20 text-yellow-500 text-base font-medium",
                                                    "Loop ON"
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            // Next song (faded)
                            if let Some((next_name, next_color)) = next_song_info {
                                FadedSongTitle {
                                    song_name: next_name,
                                    color: next_color,
                                }
                            }
                        }
                    }
                }
            }

            // Transport control bar at bottom
            TransportControlBar {
                is_playing: is_playing,
                is_looping: is_looping,
                on_play_pause: Callback::new(move |_| {
                    // Start latency tracking before making the call
                    LATENCY_TRACKER.write().start_play_toggle();
                    spawn(async move {
                        let _ = Session::get().setlist().toggle_playback().await;
                    });
                }),
                on_loop_toggle: Callback::new(move |_| {
                    spawn(async move {
                        let _ = Session::get().setlist().toggle_song_loop().await;
                    });
                }),
                on_back: Callback::new(move |_| {
                    spawn(async move {
                        let _ = Session::get().setlist().previous_section().await;
                    });
                }),
                on_forward: Callback::new(move |_| {
                    spawn(async move {
                        let _ = Session::get().setlist().next_section().await;
                    });
                }),
            }
        }
    }
}
