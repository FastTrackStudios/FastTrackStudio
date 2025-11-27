use dioxus::prelude::*;
use setlist::{SETLIST_STRUCTURE, ACTIVE_INDICES, Song};
use daw::primitives::{MusicalPosition, TimePosition};
use crate::components::{Slider, Ruler, MeasureInfo, TrackControlPanel};
use lyrics::{Lyrics, output::{Slides, SlideBreakConfig}};
use crate::reactive_state::{use_transport_for_active_song, use_tracks_for_active_song};

/// Arrangement view component - DAW timeline/arrange view
#[component]
pub fn ArrangementView(
    /// Callback for seeking to a time position: (song_index, time_seconds)
    on_seek_to_time: Option<Callback<(usize, f64)>>,
    /// Callback when track mute is toggled: (project_name, track_index, new_muted_state)
    on_track_mute: Option<Callback<(String, usize, bool)>>,
    /// Callback when track solo is toggled: (project_name, track_index, new_solo_mode)
    on_track_solo: Option<Callback<(String, usize, daw::tracks::api::solo::SoloMode)>>,
) -> Element {
    // Get active song from SETLIST_STRUCTURE and ACTIVE_INDICES
    // Only rerenders when structure changes or active song changes
    let active_song = use_memo(move || {
        let setlist_structure = SETLIST_STRUCTURE.read();
        let active_indices = ACTIVE_INDICES.read();
        active_indices.0.and_then(|idx| {
            setlist_structure.as_ref()?.songs.get(idx).cloned()
        })
    });
    
    // Calculate measure grid from tempo/time signature changes
    let measures = use_memo(move || {
        active_song().as_ref().map(|song| {
            calculate_measures(song)
        }).unwrap_or_default()
    });
    
    // Get transport state for active song using reactive state hook
    let transport = use_transport_for_active_song();
    
    // Get playhead position (play cursor) from transport
    // Only rerenders when transport for active song changes
    let playhead_position = use_memo(move || {
        transport().as_ref().map(|t| t.playhead_position.time.to_seconds())
    });
    
    // Get playhead musical position for measure calculation
    let playhead_measure = use_memo(move || {
        transport().as_ref().map(|t| t.playhead_position.musical.measure)
    });
    
    // Get edit cursor position (separate from playhead)
    let edit_cursor_position = use_memo(move || {
        transport().as_ref().map(|t| t.edit_position.time.to_seconds())
    });
    
    // Track container width for zoom calculation
    // Use onresize to get actual container dimensions
    let container_width = use_signal(|| 1200.0); // Default fallback
    
    // Create copies of the signal for both the memo and handler (signals are Copy)
    // Note: We need separate copies because both the memo and handler will move them
    let container_width_for_memo = container_width;
    let mut container_width_for_handler = container_width;
    
    // Calculate initial zoom to fit entire song in viewport
    let initial_zoom = use_memo(move || {
        // Read container_width without moving it (signals are Copy)
        let container_width_val = container_width_for_memo();
        if let Some(song) = active_song() {
            let song_start = song.effective_start();
            let song_end = song.effective_end();
            
            // Calculate timeline start (include count-in if present)
            let timeline_start = song.count_in_marker.as_ref()
                .and_then(|m| Some(m.position.time.to_seconds()))
                .unwrap_or(song_start);
            
            let song_duration = song_end - timeline_start;
            let viewport_width = container_width_val;
            
            if song_duration > 0.0 && viewport_width > 0.0 {
                // Account for TCP width (192px = w-48) and header/padding
                let tcp_width = 192.0; // w-48 = 12rem = 192px
                let header_height = 48.0; // Header bar
                let padding = 80.0; // Extra padding for scrollbars and margins
                let available_width = viewport_width - tcp_width - padding;
                let target_song_width = available_width * 0.98; // Use 98% of available width
                let required_pps = target_song_width / song_duration;
                
                // Convert to zoom level (base_pixels_per_second = 10.0)
                let base_pps = 10.0;
                let calculated_zoom = required_pps / base_pps;
                let clamped_zoom = calculated_zoom.max(0.1).min(10.0);
                
                clamped_zoom
            } else {
                1.0
            }
        } else {
            1.0
        }
    });
    
    // Zoom state - controls pixels per second (resolution)
    // Use a signal that can be manually adjusted, but initialize from memo
    let zoom_level = use_signal(|| {
        // Calculate initial zoom on first load
        let initial = initial_zoom();
        if initial > 0.0 {
            initial
        } else {
            1.0
        }
    });
    
    // Handler for resize events to update container width
    let handle_resize = move |evt: Event<ResizeData>| {
        let data = evt.data();
        // Get content box size - returns Result<Size2D<f64, Pixels>, ResizeError>
        // Size2D from euclid has .width and .height as fields
        if let Ok(size) = data.get_content_box_size() {
            // width is the horizontal dimension (inline_size in horizontal layout)
            tracing::info!("[Arrangement] Container resize: width={:.2}px, height={:.2}px", size.width, size.height);
            container_width_for_handler.set(size.width);
        } else if let Ok(size) = data.get_border_box_size() {
            // Fallback to border box size
            tracing::info!("[Arrangement] Container resize (border box): width={:.2}px, height={:.2}px", size.width, size.height);
            container_width_for_handler.set(size.width);
        }
    };
    
    // Track if zoom has been manually adjusted by user
    let zoom_manually_adjusted = use_signal(|| false);
    
    // Update zoom_level when initial_zoom changes, but only if user hasn't manually adjusted it
    // Signals are Copy, so create copies for the effect
    let zoom_manually_adjusted_effect = zoom_manually_adjusted;
    let mut zoom_level_effect = zoom_level;
    use_effect(move || {
        let new_initial = initial_zoom();
        if !zoom_manually_adjusted_effect() && new_initial > 0.0 {
            let current_zoom = zoom_level_effect();
            // Only update and log if the value actually changes
            if (current_zoom - new_initial).abs() > 0.01 {
                tracing::info!("[Arrangement] Auto-updating zoom_level from {:.2} to {:.2}", current_zoom, new_initial);
                zoom_level_effect.set(new_initial);
            }
        }
    });
    
    // Vertical zoom state - controls track heights (not ruler)
    let vertical_zoom_level = use_signal(|| 1.0);
    let vertical_zoom_manually_adjusted = use_signal(|| false);
    
    // Calculate pixel positions based on zoom (resolution-based, not visual scale)
    let base_pixels_per_second = 10.0; // Base scaling factor
    let pixels_per_second = use_memo(move || base_pixels_per_second * zoom_level());
    
    // Get active song index for seeking from ACTIVE_INDICES
    // Only rerenders when active song changes
    let active_song_idx = use_memo(move || {
        ACTIVE_INDICES.read().0
    });
    
    // Calculate cursor positions (reactive to zoom)
    // Both cursors need to account for song_start offset like measures and sections do
    let play_cursor_left = use_memo(move || {
        let pps = pixels_per_second();
        if let (Some(pos), Some(song)) = (playhead_position(), active_song()) {
            let song_start = song.effective_start();
            // playhead_position is absolute time, subtract song_start to get relative position
            let relative_pos = pos - song_start;
            Some(relative_pos * pps)
        } else {
            None
        }
    });
    
    let edit_cursor_left = use_memo(move || {
        let pps = pixels_per_second();
        if let Some(song) = active_song() {
            let song_start = song.effective_start();
            if let Some(edit_pos) = edit_cursor_position() {
                // edit_cursor_position is absolute time, subtract song_start to get relative position
                let relative_pos = edit_pos - song_start;
                Some(relative_pos * pps)
            } else {
                None
            }
        } else {
            None
        }
    });
    
    let show_edit_cursor = use_memo(move || {
        if let (Some(play_pos), Some(edit_pos)) = (playhead_position(), edit_cursor_position()) {
            (edit_pos - play_pos).abs() > 0.01
        } else {
            true
        }
    });
    
    // Calculate section positions and widths for rendering (reactive to zoom)
    // Positions are relative to timeline_start (count-in start if present, otherwise song_start)
    let sections_data = use_memo(move || {
        let pps = pixels_per_second();
        active_song().as_ref().map(|song| {
            let song_start = song.effective_start();
            // Calculate timeline start (count-in start if present, otherwise song_start)
            let timeline_start = song.count_in_marker.as_ref()
                .and_then(|m| Some(m.position.time.to_seconds()))
                .unwrap_or(song_start);
            
            let mut sections: Vec<_> = Vec::new();
            
            // Add count-in section if it exists
            if let Some(count_in_seconds) = song.count_in_marker.as_ref()
                .and_then(|m| Some(m.position.time.to_seconds())) {
                let count_in_start = count_in_seconds;
                let count_in_end = song_start;
                if count_in_start < count_in_end {
                    // Position relative to timeline_start (which is count_in_start in this case)
                    let start_rel = count_in_start - timeline_start; // This will be 0
                    let end_rel = count_in_end - timeline_start; // This will be positive
                    let width = (end_rel - start_rel) * pps;
                    let left = start_rel * pps;
                    // Use a muted gray color for count-in
                    let color = "rgb(100, 100, 100)".to_string();
                    sections.push(("Count In".to_string(), left, width, color));
                }
            }
            
            // Add regular sections (positioned relative to timeline_start)
            let regular_sections: Vec<_> = song.sections.iter().map(|section| {
                let start_rel = section.start_seconds() - timeline_start;
                let end_rel = section.end_seconds() - timeline_start;
                let width = (end_rel - start_rel) * pps;
                let left = start_rel * pps;
                let color = section.color_bright();
                (section.name.clone(), left, width, color)
            }).collect();
            sections.extend(regular_sections);
            
            sections
        }).unwrap_or_default()
    });
    
    // Generate slides from lyrics and calculate their positions
    let slides_data = use_memo(move || {
        let pps = pixels_per_second();
        active_song().as_ref().and_then(|song| {
            song.lyrics.as_ref().map(|lyrics| {
                let config = SlideBreakConfig {
                    max_chars: 120,
                    max_words: 19,
                    min_chars_to_bundle: 32,
                    min_words_to_bundle: 7,
                };
                let generated_slides = Slides::generate_with_config(lyrics, config);
                let song_start = song.effective_start();
                
                // Map slides to their time positions based on sections
                let mut slides_with_positions = Vec::new();
                let mut slide_idx = 0;
                
                for section in song.sections.iter() {
                    let section_start = section.start_seconds() - song_start;
                    let section_end = section.end_seconds() - song_start;
                    let section_duration = section_end - section_start;
                    
                    // Find slides for this section (collect indices first to avoid lifetime issues)
                    let mut section_slide_indices = Vec::new();
                    for (idx, slide) in generated_slides.iter().enumerate().skip(slide_idx) {
                        if slide.section_name.trim().eq_ignore_ascii_case(section.name.trim()) {
                            section_slide_indices.push(idx);
                        } else {
                            break;
                        }
                    }
                    
                    if !section_slide_indices.is_empty() {
                        // Distribute slides evenly across the section
                        let slide_duration = section_duration / section_slide_indices.len() as f64;
                        
                        for (i, &slide_idx_in_vec) in section_slide_indices.iter().enumerate() {
                            let slide = &generated_slides[slide_idx_in_vec];
                            let slide_start = section_start + (i as f64 * slide_duration);
                            let slide_end = slide_start + slide_duration;
                            let width = (slide_end - slide_start) * pps;
                            let left = slide_start * pps;
                            
                            // Get preview text (first line or truncated)
                            let preview_text = slide.lines.first()
                                .map(|line| {
                                    use lyrics::core::LinePart;
                                    let mut text = String::new();
                                    for part in line.parts.iter() {
                                        match part {
                                            LinePart::Regular(t) => {
                                                text.push_str(t);
                                                text.push(' ');
                                            }
                                            LinePart::Parenthetical(t) => {
                                                text.push_str(&format!("({}) ", t));
                                            }
                                        }
                                    }
                                    text.trim().to_string()
                                })
                                .unwrap_or_else(|| "".to_string());
                            
                            slides_with_positions.push((
                                slide_idx_in_vec, // Store original slide index for stable key
                                slide.clone(),
                                left,
                                width,
                                preview_text,
                            ));
                        }
                        
                        slide_idx += section_slide_indices.len();
                    }
                }
                
                slides_with_positions
            })
        }).unwrap_or_default()
    });
    
    // Tracks from active song using reactive state hook
    // Only rerenders when tracks for active song change
    let tracks = use_tracks_for_active_song();
    
    // Compute initial collapsed state from REAPER's folder collapse state
    let initial_collapsed_state = use_memo(move || {
        tracks().as_ref().map(|tracks_vec| {
            tracks_vec.iter().map(|track| {
            // Check bus_compact.arrange first (arrange view collapse state)
            if let Some(bus_compact) = &track.bus_compact {
                match bus_compact.arrange {
                    daw::tracks::api::collapse::ArrangeCollapseState::NotCollapsed => false,
                    daw::tracks::api::collapse::ArrangeCollapseState::CollapsedMedium => true,
                    daw::tracks::api::collapse::ArrangeCollapseState::CollapsedSmall => true,
                    daw::tracks::api::collapse::ArrangeCollapseState::Unknown(_) => false,
                }
            } else if let Some(folder_state) = track.folder_state_tcp {
                // Fall back to folder_state_tcp
                match folder_state {
                    daw::tracks::api::folder::TcpFolderState::Normal => false,
                    daw::tracks::api::folder::TcpFolderState::Small => false,
                    daw::tracks::api::folder::TcpFolderState::Collapsed => true,
                }
            } else {
                // Default to expanded if no collapse state is set
                false
            }
        }).collect::<Vec<bool>>()
        }).unwrap_or_default()
    });
    
    // Collapsed folder state - initialize from REAPER, but allow user to toggle
    let mut collapsed_folders = use_signal(|| initial_collapsed_state().clone());
    
    // Update collapsed state when tracks change (preserve user changes if possible)
    let tracks_for_effect = tracks;
    let initial_state_for_effect = initial_collapsed_state;
    let mut collapsed_folders_for_effect = collapsed_folders;
    use_effect(move || {
        let tracks_list = tracks_for_effect().as_ref().map(|t| t.clone()).unwrap_or_default();
        let track_count = tracks_list.len();
        let current_state = collapsed_folders_for_effect();
        let initial_state = initial_state_for_effect();
        
        if current_state.len() != track_count {
            // Rebuild state: use initial REAPER state for new tracks, preserve user changes for existing
            let mut new_state: Vec<bool> = (0..track_count).map(|idx| {
                if idx < current_state.len() {
                    // Preserve existing user state
                    current_state[idx]
                } else if idx < initial_state.len() {
                    // Use initial REAPER state
                    initial_state[idx]
                } else {
                    // New track not in initial state - default to expanded
                    false
                }
            }).collect();
            
            *collapsed_folders_for_effect.write() = new_state;
        }
    });
    
    // Track heights (resizable) - base heights before vertical zoom
    // Compute base heights from track count (default 64px per track)
    let base_track_heights = use_memo(move || {
        let track_count = tracks().as_ref().map(|t| t.len()).unwrap_or(0).max(1);
        vec![64.0; track_count]
    });
    
    // Apply vertical zoom to track heights (ruler not affected)
    let track_heights = use_memo(move || {
        let vertical_zoom = vertical_zoom_level();
        base_track_heights().iter().map(|h| h * vertical_zoom).collect::<Vec<f64>>()
    });
    
    // Calculate total height for tracks area (ruler + all tracks) - must match TCP
    // Note: ruler height is NOT affected by vertical zoom
    let tracks_total_height = use_memo(move || {
        use crate::components::ruler::RULER_HEIGHT;
        let tracks_height: f64 = track_heights().iter().sum();
        RULER_HEIGHT + tracks_height
    });
    
    // Calculate total timeline width based on song duration and zoom
    // Includes count-in if present
    let timeline_width = use_memo(move || {
        let pps = pixels_per_second();
        active_song().as_ref().map(|song| {
            let song_start = song.effective_start();
            let song_end = song.effective_end();
            
            // Calculate timeline start (include count-in if present)
            let timeline_start = song.count_in_marker.as_ref()
                .and_then(|m| Some(m.position.time.to_seconds()))
                .unwrap_or(song_start);
            
            let duration = song_end - timeline_start;
            // Add some padding
            duration * pps + 200.0
        }).unwrap_or(2000.0)
    });
    
    // Handle click on timeline (accounts for zoom resolution and scroll)
    let handle_timeline_click = move |_evt: Event<MouseData>| {
        if let Some(song_idx) = active_song_idx() {
            if let Some(callback) = on_seek_to_time {
                #[cfg(target_arch = "wasm32")]
                {
                    use web_sys::MouseEvent;
                    if let Ok(mouse_evt) = _evt.web_event::<MouseEvent>() {
                        let pps = pixels_per_second();
                        let song_start = active_song().as_ref().map(|s| s.effective_start()).unwrap_or(0.0);
                        if let Some(target) = mouse_evt.current_target() {
                            if let Some(element) = target.dyn_ref::<web_sys::HtmlElement>() {
                                let rect = element.get_bounding_client_rect();
                                let click_x = mouse_evt.client_x() as f64 - rect.left();
                                // Calculate time relative to song start
                                let time_seconds = click_x / pps;
                                callback.call((song_idx, time_seconds));
                            }
                        }
                    }
                }
            }
        }
    };
    
    rsx! {
        div {
            class: "w-full h-full flex flex-col overflow-hidden bg-background",
            // Timeline header with song name and zoom control
            if let Some(song) = active_song() {
                div {
                    class: "h-12 flex-shrink-0 border-b border-border bg-card flex items-center justify-between px-4",
                    div {
                        class: "font-semibold text-lg",
                        "{song.name}"
                    }
                    // Zoom sliders
                    div {
                        class: "flex items-center gap-4",
                        div {
                            class: "flex items-center gap-2",
                            Slider {
                                value: zoom_level,
                                min: 0.1,
                                max: 10.0,
                                step: 0.1,
                                label: Some("Zoom".to_string()),
                                on_change: {
                                    let mut zoom_manually_adjusted_slider = zoom_manually_adjusted;
                                    Some(Callback::new(move |_| {
                                        zoom_manually_adjusted_slider.set(true);
                                    }))
                                },
                            }
                            // Reset button - always rendered to maintain layout, hidden when not needed
                            button {
                                class: if zoom_manually_adjusted() {
                                    "px-3 py-1.5 text-xs font-medium text-muted-foreground hover:text-foreground hover:bg-accent rounded-md border border-border transition-colors"
                                } else {
                                    "px-3 py-1.5 text-xs font-medium text-transparent rounded-md border border-transparent pointer-events-none"
                                },
                                onclick: {
                                    let mut zoom_manually_adjusted_reset = zoom_manually_adjusted;
                                    let mut zoom_level_reset = zoom_level;
                                    move |_| {
                                        let new_initial = initial_zoom();
                                        if new_initial > 0.0 {
                                            zoom_level_reset.set(new_initial);
                                        }
                                        zoom_manually_adjusted_reset.set(false);
                                    }
                                },
                                "Reset"
                            }
                        }
                        div {
                            class: "flex items-center gap-2",
                            Slider {
                                value: vertical_zoom_level,
                                min: 0.1,
                                max: 5.0,
                                step: 0.1,
                                label: Some("V-Zoom".to_string()),
                                on_change: {
                                    let mut vertical_zoom_manually_adjusted_slider = vertical_zoom_manually_adjusted;
                                    Some(Callback::new(move |_| {
                                        vertical_zoom_manually_adjusted_slider.set(true);
                                    }))
                                },
                            }
                            // Reset button - always rendered to maintain layout, hidden when not needed
                            button {
                                class: if vertical_zoom_manually_adjusted() {
                                    "px-3 py-1.5 text-xs font-medium text-muted-foreground hover:text-foreground hover:bg-accent rounded-md border border-border transition-colors"
                                } else {
                                    "px-3 py-1.5 text-xs font-medium text-transparent rounded-md border border-transparent pointer-events-none"
                                },
                                onclick: {
                                    let mut vertical_zoom_level_reset = vertical_zoom_level;
                                    let mut vertical_zoom_manually_adjusted_reset = vertical_zoom_manually_adjusted;
                                    move |_| {
                                        vertical_zoom_level_reset.set(1.0);
                                        vertical_zoom_manually_adjusted_reset.set(false);
                                    }
                                },
                                "Reset"
                            }
                        }
                    }
                }
            }
            
            // Main content area with TCP and tracks - shared scroll container
            div {
                class: "flex-1 flex overflow-hidden min-h-0",
                // Shared scrollable container for both TCP and tracks
                div {
                    class: "flex-1 flex overflow-x-auto overflow-y-auto relative min-h-0",
                    onresize: handle_resize,
                    onwheel: move |evt| {
                        // Handle zoom with Ctrl/Cmd + wheel
                        #[cfg(target_arch = "wasm32")]
                        {
                            use web_sys::WheelEvent;
                            if let Ok(wheel_evt) = evt.web_event::<WheelEvent>() {
                                if wheel_evt.ctrl_key() || wheel_evt.meta_key() {
                                    wheel_evt.prevent_default();
                                    let delta = wheel_evt.delta_y();
                                    let zoom_delta = if delta < 0.0 { 1.1 } else { 0.9 };
                                    let new_zoom = (zoom_level() * zoom_delta).max(0.1).min(10.0);
                                    zoom_level.set(new_zoom);
                                }
                            }
                        }
                    },
                    // Content container with fixed width for horizontal scrolling
                    // Height is auto to allow empty space at bottom
                    {
                        let timeline_w = timeline_width();
                        let container_w = container_width();
                        // Log comparison when rendered
                        if let Some(song) = active_song() {
                            let song_end = song.effective_end();
                            let song_start = song.effective_start();
                            let timeline_start = song.count_in_marker.as_ref()
                                .and_then(|m| Some(m.position.time.to_seconds()))
                                .unwrap_or(song_start);
                            let pps = pixels_per_second();
                            let song_end_px = (song_end - timeline_start) * pps;
                            tracing::info!(
                                "[Arrangement] Render comparison: timeline_width={:.2}px, container_width={:.2}px, timeline_start={:.2}s, song_end_px={:.2}px, ratio={:.2}%, song_end_px/container={:.2}%",
                                timeline_w, container_w, timeline_start, song_end_px, (timeline_w / container_w.max(1.0)) * 100.0, (song_end_px / container_w.max(1.0)) * 100.0
                            );
                        }
                        rsx! {
                            div {
                                class: "flex relative",
                                style: format!("min-width: {}px; min-height: 100%;", timeline_w as i32),
                        
                        // TCP (Track Control Panel) on the left - sticky, extends full height
                        div {
                            class: "sticky left-0 z-30 bg-card",
                            style: "height: 100%;",
                            TrackControlPanel {
                                tracks: tracks().as_ref().map(|t| t.clone()).unwrap_or_default(),
                                track_heights: track_heights,
                                collapsed_folders: collapsed_folders,
                                on_track_height_change: None,
                                on_folder_toggle: Some({
                                    let mut collapsed_folders_for_callback = collapsed_folders;
                                    Callback::new(move |idx: usize| {
                                        let state = collapsed_folders_for_callback();
                                        let mut new_state = state.clone();
                                        if let Some(collapsed) = new_state.get_mut(idx) {
                                            *collapsed = !*collapsed;
                                            *collapsed_folders_for_callback.write() = new_state;
                                        }
                                    })
                                }),
                                project_name: active_song().as_ref()
                                    .and_then(|song| {
                                        song.metadata
                                            .get("project_name")
                                            .or_else(|| song.metadata.get("Project"))
                                            .or_else(|| song.metadata.get("project"))
                                            .cloned()
                                    })
                                    .unwrap_or_else(|| "default".to_string()),
                                on_track_mute: on_track_mute.clone(),
                                on_track_solo: on_track_solo.clone(),
                            }
                        }
                        
                        // Tracks area on the right
                        div {
                            class: "flex-1 flex flex-col relative",
                            style: format!("min-height: {}px;", tracks_total_height()),
                            // Gridlines layer - extends from measure positions all the way down (including through ruler)
                            // NOTE: Use ALL measures for gridlines, not just visible ones (measure numbers are filtered)
                            {
                                let timeline_start = active_song().as_ref().map(|s| {
                                    s.count_in_marker.as_ref()
                                        .and_then(|m| Some(m.position.time.to_seconds()))
                                        .unwrap_or(s.effective_start())
                                }).unwrap_or(0.0);
                                let pps = pixels_per_second();
                                // Use all measures for gridlines (not filtered visible_measures)
                                let all_measures = measures();
                                rsx! {
                                    div {
                                        class: "absolute pointer-events-none z-0",
                                        style: "top: 0px; bottom: 0px; left: 0px; right: 0px;", // Extend all the way down
                                        for measure in all_measures.iter() {
                                            div {
                                                key: "gridline-{measure.measure_number}",
                                                class: "absolute w-px bg-border/30",
                                                style: format!(
                                                    "left: {}px; top: 0px; bottom: 0px; height: 100%;",
                                                    (measure.time_position - timeline_start) * pps
                                                ),
                                            }
                                        }
                                    }
                                }
                            }
                            
                            // Section boundary lines - extend from section start and end positions
                            // Use section colors, and prioritize start lines when start/end coincide
                            {
                                let timeline_start = active_song().as_ref().map(|s| {
                                    s.count_in_marker.as_ref()
                                        .and_then(|m| Some(m.position.time.to_seconds()))
                                        .unwrap_or(s.effective_start())
                                }).unwrap_or(0.0);
                                let pps = pixels_per_second();
                                let sections_list = sections_data();
                                
                                // Collect all boundary positions with their colors and types (start/end)
                                // This allows us to prioritize start lines when positions coincide
                                let mut boundaries: Vec<(f64, String, bool)> = Vec::new(); // (position, color, is_start)
                                
                                for (_section_name, left, width, color) in sections_list.iter() {
                                    let start_pos = *left;
                                    let end_pos = left + width;
                                    boundaries.push((start_pos, color.clone(), true)); // is_start = true
                                    boundaries.push((end_pos, color.clone(), false)); // is_start = false
                                }
                                
                                // Sort by position, then by is_start (start lines come first at same position)
                                boundaries.sort_by(|a, b| {
                                    a.0.partial_cmp(&b.0)
                                        .unwrap_or(std::cmp::Ordering::Equal)
                                        .then_with(|| b.2.cmp(&a.2)) // Reverse: true (start) comes before false (end)
                                });
                                
                                // Filter out duplicate positions, keeping only the first (which will be start if it exists)
                                // Use a tolerance for floating point comparison
                                const TOLERANCE: f64 = 0.5; // 0.5px tolerance
                                let mut unique_boundaries = Vec::new();
                                
                                for (pos, color, is_start) in boundaries {
                                    // Check if this position is close to any already rendered position
                                    let is_duplicate = unique_boundaries.iter().any(|(existing_pos, _, _)| {
                                        (pos - existing_pos).abs() < TOLERANCE
                                    });
                                    
                                    if !is_duplicate {
                                        unique_boundaries.push((pos, color, is_start));
                                    }
                                }
                                
                                rsx! {
                                    div {
                                        class: "absolute pointer-events-none",
                                        style: "top: 0px; bottom: 0px; left: 0px; right: 0px; z-index: 15;", // Extend all the way down, above ruler (z-10) but below cursors (z-20)
                                        // Render section boundary lines with section colors
                                        for (idx, (pos, color, _is_start)) in unique_boundaries.iter().enumerate() {
                                            div {
                                                key: "section-boundary-{idx}",
                                                class: "absolute w-0.5",
                                                style: format!("left: {}px; top: 0px; bottom: 0px; height: 100%; background-color: {};", pos, color),
                                            }
                                        }
                                    }
                                }
                            }
                            
                            // Ruler component (sticky at top)
                            div {
                                class: "sticky top-0 z-10 bg-background flex-shrink-0",
                                Ruler {
                                    song: active_song(),
                                    measures: measures(),
                                    sections: sections_data(),
                                    pixels_per_second: pixels_per_second(),
                                }
                            }
                            
                            // Cursors layer (spans ruler and tracks)
                            // Calculate positions inline using same logic as measures/gridlines
                            {
                                let timeline_start_val = active_song().as_ref().map(|s| {
                                    s.count_in_marker.as_ref()
                                        .and_then(|m| Some(m.position.time.to_seconds()))
                                        .unwrap_or(s.effective_start())
                                }).unwrap_or(0.0);
                                let pps_val = pixels_per_second();
                                let play_pos_val = playhead_position();
                                let edit_pos_val = edit_cursor_position();
                                let show_edit_val = show_edit_cursor();
                                
                                // Calculate play cursor position
                                let play_cursor_px = play_pos_val.map(|pos| {
                                    let relative_pos = pos - timeline_start_val;
                                    relative_pos * pps_val
                                });
                                
                                // Calculate edit cursor position
                                let edit_cursor_px = if show_edit_val {
                                    edit_pos_val.map(|pos| {
                                        let relative_pos = pos - timeline_start_val;
                                        relative_pos * pps_val
                                    })
                                } else {
                                    None
                                };
                                
                                // Measure numbers row starts at 80px (48px sections + 32px tempo/time sig)
                                let measure_row_top = 80.0;
                                rsx! {
                                    div {
                                        class: "absolute inset-0 pointer-events-none z-20",
                                        style: format!("top: {}px;", measure_row_top), // Start from measure numbers row
                                        // Play cursor (green) - line extends down from measure row
                                        if let Some(left_px) = play_cursor_px {
                                            // Upside-down triangle at top
                                            div {
                                                class: "absolute",
                                                style: format!("left: {}px; top: 0px; width: 0; height: 0; border-left: 6px solid transparent; border-right: 6px solid transparent; border-top: 8px solid rgb(34, 197, 94);", left_px - 6.0),
                                            }
                                            // Vertical line
                                            div {
                                                class: "absolute top-0 bottom-0 w-0.5 bg-green-500",
                                                style: format!("left: {}px; top: 8px;", left_px), // Start below triangle
                                            }
                                        }
                                        // Edit cursor (blue) - line extends down from measure row
                                        if let Some(left_px) = edit_cursor_px {
                                            // Upside-down triangle at top
                                            div {
                                                class: "absolute",
                                                style: format!("left: {}px; top: 0px; width: 0; height: 0; border-left: 6px solid transparent; border-right: 6px solid transparent; border-top: 8px solid rgb(59, 130, 246);", left_px - 6.0),
                                            }
                                            // Vertical line
                                            div {
                                                class: "absolute top-0 bottom-0 w-0.5 bg-blue-500",
                                                style: format!("left: {}px; top: 8px;", left_px), // Start below triangle
                                            }
                                        }
                                    }
                                }
                            }
                            
                            // Lyric slides track
                            div {
                                class: "relative cursor-pointer flex-shrink-0",
                                style: format!("height: {}px;", track_heights().get(0).copied().unwrap_or(64.0)),
                                onclick: handle_timeline_click,
                                for (slide_idx, slide, left, width, preview_text) in slides_data().iter() {
                                    div {
                                        key: "slide-{slide_idx}",
                                        class: "absolute top-0 h-full rounded border border-border/50 bg-card/50 hover:bg-card flex items-center px-2 py-1 text-xs text-foreground overflow-hidden",
                                        style: format!("left: {}px; width: {}px;", 
                                            left, width.max(40.0)),
                                        div {
                                            class: "w-full break-words overflow-wrap-anywhere",
                                            style: "word-wrap: break-word; overflow-wrap: break-word; hyphens: auto;",
                                            "{preview_text}"
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

/// Calculate measure information from song's tempo/time signature changes
/// Includes negative measures for count-in section
fn calculate_measures(song: &Song) -> Vec<MeasureInfo> {
    let mut measures = Vec::new();
    
    // Get song start time
    let song_start = song.effective_start();
    let song_end = song.effective_end();
    
    if song_end <= song_start {
        return measures;
    }
    
    // Get timeline start (count-in start if present, otherwise song_start)
    let timeline_start = song.count_in_marker.as_ref()
        .and_then(|m| Some(m.position.time.to_seconds()))
        .unwrap_or(song_start);
    
    // Sort tempo/time signature changes by position
    let mut tempo_changes: Vec<_> = song.tempo_time_sig_changes.iter().collect();
    tempo_changes.sort_by(|a, b| a.position.partial_cmp(&b.position).unwrap_or(std::cmp::Ordering::Equal));
    
    // Get starting tempo and time signature
    let starting_tempo = song.starting_tempo.unwrap_or(120.0);
    let starting_time_sig = song.starting_time_signature.as_ref()
        .map(|ts| (ts.numerator, ts.denominator))
        .unwrap_or((4, 4));
    
    // Calculate measure duration in seconds
    let measure_duration = |bpm: f64, time_sig: (i32, i32)| -> f64 {
        let beats_per_measure = time_sig.0 as f64;
        let seconds_per_beat = 60.0 / bpm;
        beats_per_measure * seconds_per_beat
    };
    
    // Calculate negative measures (count-in) - go back 4 measures from song_start
    // But start from timeline_start if count-in exists
    let count_in_measures = 4;
    let mut current_time = song_start;
    let mut current_measure = -(count_in_measures as i32);
    let mut current_tempo = starting_tempo;
    let mut current_time_sig = starting_time_sig;
    
    // Work backwards from song_start for count-in
    for _ in 0..count_in_measures {
        let duration = measure_duration(current_tempo, current_time_sig);
        current_time -= duration;
        
        // Only add measure if it's at or after timeline_start
        if current_time >= timeline_start {
            measures.insert(0, MeasureInfo {
                measure_number: current_measure,
                bpm: current_tempo,
                time_signature: current_time_sig,
                time_position: current_time,
            });
        }
        
        current_measure += 1;
    }
    
    // Reset to song_start for positive measures
    current_time = song_start;
    current_measure = 1;
    let mut tempo_change_idx = 0;
    
    // Calculate positive measures
    while current_time < song_end {
        // Check if we've hit a tempo/time signature change
        while tempo_change_idx < tempo_changes.len() {
            let change = &tempo_changes[tempo_change_idx];
            let change_time = song_start + change.position;
            
            if change_time > current_time {
                break;
            }
            
            if change.tempo > 0.0 {
                current_tempo = change.tempo;
            }
            if let Some(ts) = change.time_signature {
                current_time_sig = ts;
            }
            
            tempo_change_idx += 1;
        }
        
        // Add measure info
        measures.push(MeasureInfo {
            measure_number: current_measure,
            bpm: current_tempo,
            time_signature: current_time_sig,
            time_position: current_time,
        });
        
        // Move to next measure
        let duration = measure_duration(current_tempo, current_time_sig);
        current_time += duration;
        current_measure += 1;
        
        // Safety limit
        if current_measure > 1000 {
            break;
        }
    }
    
    measures
}

