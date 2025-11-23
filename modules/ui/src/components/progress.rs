use dioxus::prelude::*;

/// Progress bar section definition
#[derive(Clone, Debug, PartialEq)]
pub struct ProgressSection {
    pub start_percent: f64,  // 0-100, where this section starts
    pub end_percent: f64,    // 0-100, where this section ends
    pub color: String,       // Tailwind color class or CSS color
    pub name: String,        // Name to display inside the section
}

/// Tempo/Time signature marker definition
#[derive(Clone, Debug, PartialEq)]
pub struct TempoMarker {
    pub position_percent: f64,  // 0-100, where this marker is positioned
    pub label: String,          // Label text (e.g., "4/4", "120 bpm", "4/4 120 bpm")
}

/// Segmented progress bar component with different colored sections
#[component]
pub fn SegmentedProgressBar(
    progress: Signal<f64>,
    sections: Vec<ProgressSection>,
    #[props(default)]
    tempo_markers: Vec<TempoMarker>,
    #[props(default)]
    song_key: Option<String>,
    #[props(default)]
    on_section_click: Option<Callback<usize>>,
) -> Element {
    let current_progress = progress();
    
    // Track progress changes to detect jumps and disable animations
    // Store previous values in signals
    let mut prev_progress = use_signal(|| None::<f64>);
    let mut prev_song_key = use_signal(|| None::<String>);
    
    // Compute animation state using use_memo - reads signals but doesn't modify them
    // Clone song_key to avoid move issues
    let song_key_for_memo = song_key.clone();
    let should_animate = use_memo(move || {
        let prev = prev_progress();
        let prev_key = prev_song_key();
        
        // Check if song changed
        let song_changed = prev_key != song_key_for_memo;
        
        // Check if there's a large position jump (more than 5% change)
        let large_jump = if let Some(prev_val) = prev {
            (current_progress - prev_val).abs() > 5.0
        } else {
            false
        };
        
        // Disable animations for song changes or large jumps (instant updates)
        // Enable animations for small incremental changes (smooth updates)
        !song_changed && !large_jump && prev.is_some()
    });
    
    // Update tracking state in effect - clone values to avoid move issues
    let current_progress_for_effect = current_progress;
    let song_key_for_effect = song_key.clone();
    use_effect(move || {
        prev_progress.set(Some(current_progress_for_effect));
        if prev_song_key() != song_key_for_effect {
            prev_song_key.set(song_key_for_effect.clone());
        }
    });
    
    // Pre-calculate section data
    let section_data: Vec<_> = sections.iter().enumerate().map(|(index, section)| {
        let section_start = section.start_percent;
        let section_end = section.end_percent;
        let section_width = section.end_percent - section.start_percent;
        
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
        
        (index, section_start, section_end, section_width, section.color.clone(), filled_percent, section.name.clone())
    }).collect();
    
    // Find the active section (the one we're currently in)
    let active_section = section_data.iter().find(|(_, start, end, _, _, _, _)| {
        current_progress >= *start && current_progress < *end
    }).or_else(|| {
        // If we're past all sections, check if we're at the very end of the last section
        if let Some((_, start, end, _, _, _, _)) = section_data.last() {
            if current_progress >= *start && current_progress <= *end {
                section_data.last()
            } else {
                None
            }
        } else {
            None
        }
    });
    
    // Calculate section progress (0-100% within the active section)
    let section_progress = if let Some((_, section_start, section_end, section_width, _, _, _)) = active_section {
        if current_progress <= *section_start {
            0.0
        } else if current_progress >= *section_end {
            100.0
        } else {
            let progress_in_section = current_progress - section_start;
            (progress_in_section / section_width) * 100.0
        }
    } else {
        0.0
    };
    
    let active_color = active_section.map(|(_, _, _, _, color, _, _)| color.clone()).unwrap_or_else(|| "rgb(100, 100, 100)".to_string());
    
    rsx! {
        div {
            class: "flex flex-col items-center justify-center h-24 w-full px-4",
            // Tempo/Time Signature labels (above progress bar)
            if !tempo_markers.is_empty() {
                div {
                    class: "relative mb-6 h-6 w-full",
                    for (index, marker) in tempo_markers.iter().enumerate() {
                        div {
                            key: "tempo-label-{index}",
                            class: "absolute",
                            style: format!("left: {}%; transform: translateX(-50%); top: 0;", marker.position_percent),
                            div {
                                class: "text-xs font-medium text-center whitespace-nowrap px-1 py-0.5 rounded bg-accent text-accent-foreground border border-border",
                                "{marker.label}"
                            }
                        }
                    }
                }
            }
            // Main segmented progress bar
            div {
                class: "relative w-full h-16 rounded-lg overflow-hidden bg-secondary",
                // Render sections as background layers (visual only, no individual progress bars)
                for (index, section_start, _section_end, section_width, section_color, _filled_percent, _section_name) in section_data.iter() {
                    div {
                        key: "{index}",
                        class: if on_section_click.is_some() { 
                            "absolute h-full z-0 cursor-pointer transition-all duration-200 hover:brightness-110 hover:ring-2 hover:ring-white hover:ring-opacity-50" 
                        } else { 
                            "absolute h-full z-0" 
                        },
                        style: format!(
                            "left: {}%; width: {}%;",
                            section_start,
                            section_width
                        ),
                        onclick: {
                            let callback_opt = on_section_click.clone();
                            let idx = *index;
                            move |_| {
                                if let Some(callback) = &callback_opt {
                                    callback.call(idx);
                                }
                            }
                        },
                        // Section background (full color, will be brightened by progress overlay)
                        div {
                            class: "absolute inset-0 h-full transition-all duration-200",
                            style: format!(
                                "background-color: {};",
                                section_color
                            ),
                        }
                    }
                }
                // Dark overlay covering unfilled portion (reveals bright colors as progress increases)
                // pointer-events-none so clicks pass through to sections
                // Disable transition on jumps for instant position updates
                div {
                    class: if should_animate() {
                        "absolute left-0 top-0 h-full transition-all duration-100 ease-linear z-10 pointer-events-none"
                    } else {
                        "absolute left-0 top-0 h-full z-10 pointer-events-none"
                    },
                    style: {
                        let progress = current_progress.max(0.0).min(100.0);
                        // Dark overlay starts from the progress point and covers the rest
                        // Add border-radius on the right side for rounded corners
                        format!(
                            "left: {}%; width: {}%; background-color: rgba(0, 0, 0, 0.6); border-top-right-radius: 0.5rem; border-bottom-right-radius: 0.5rem;",
                            progress,
                            100.0 - progress
                        )
                    },
                }
                // Tempo/Time Signature markers (vertical lines extending above and through progress bar)
                for (index, marker) in tempo_markers.iter().enumerate() {
                    div {
                        key: "tempo-marker-{index}",
                        class: "absolute pointer-events-none",
                        style: format!(
                            "left: {}%; width: 2px; top: -1rem; bottom: 0; background-color: rgba(255, 255, 255, 0.7); z-index: 40;",
                            marker.position_percent
                        ),
                    }
                }
                // Section name text (rendered separately above everything, never darkened)
                for (index, section_start, _section_end, section_width, _section_color, _filled_percent, section_name) in section_data.iter() {
                    div {
                        key: "text-{index}",
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
                // Filled portion - disable transitions for instant updates
                div {
                    class: if should_animate() {
                        "absolute left-0 top-0 h-full transition-all duration-100 ease-linear"
                    } else {
                        "absolute left-0 top-0 h-full"
                    },
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
            // Use shorter transition for smoother updates without lag
            if !is_inactive {
                div {
                    class: "absolute left-0 top-0 h-full transition-all duration-100 ease-linear",
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
/// 
/// This is a wrapper around SegmentedProgressBar that handles song-specific logic.
/// It accepts sections as props to keep it domain-agnostic.
#[component]
pub fn SongProgressBar(
    progress: Signal<f64>,
    sections: Vec<ProgressSection>,
    on_section_click: Option<Callback<usize>>,
    #[props(default)]
    tempo_markers: Vec<TempoMarker>,
    #[props(default)]
    song_key: Option<String>,
) -> Element {
    rsx! {
        div {
            class: "w-full relative",
            SegmentedProgressBar {
                progress: progress,
                sections: sections.clone(),
                tempo_markers: tempo_markers,
                song_key: song_key,
                on_section_click: on_section_click,
            }
        }
    }
}
