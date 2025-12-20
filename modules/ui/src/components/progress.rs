use dioxus::prelude::*;

/// Time signature card component
/// Displays a time signature in fraction format (e.g., 4/4)
#[component]
pub fn TimeSignatureCard(
    position_percent: f64,
    label: String,
    top_offset: String,
    card_key: String,
    #[props(default = true)]
    vertical: bool,
    #[props(default = "sm".to_string())]
    size: String,
) -> Element {
    // Parse time signature for fraction display
    let (numerator, denominator) = if label.contains('/') {
        if let Some(slash_pos) = label.find('/') {
            (label[..slash_pos].trim().to_string(), label[slash_pos + 1..].trim().to_string())
        } else {
            (label.clone(), String::new())
        }
    } else {
        (String::new(), String::new())
    };
    
    // Size presets: text size, padding, and spacing
    let (text_size, padding, spacing) = match size.as_str() {
        "xs" => ("text-xs", "px-0.5 py-0.5", "my-0.5"),
        "sm" => ("text-xs", "px-1 py-0.5", "my-0.5"),
        "md" => ("text-sm", "px-1 py-1", "my-0.5"),
        "lg" => ("text-base", "px-2 py-1", "my-1"),
        _ => ("text-xs", "px-1 py-0.5", "my-0.5"), // Default to sm
    };
    
    let card_class = format!("{} font-medium text-center whitespace-nowrap {} rounded bg-accent text-accent-foreground border border-border", text_size, padding);
    let divider_class = format!("border-t border-current w-full {}", spacing);
    
    rsx! {
        div {
            key: "{card_key}",
            class: "absolute pointer-events-none",
            style: format!("left: {}%; transform: translateX(-50%); top: {};", position_percent, top_offset),
            div {
                class: "{card_class}",
                if vertical && !denominator.is_empty() {
                    // Vertical fraction display (numerator over denominator with line)
                    div {
                        class: "flex flex-col items-center leading-tight",
                        span { "{numerator}" }
                        span {
                            class: "{divider_class}"
                        }
                        span { "{denominator}" }
                    }
                } else if !denominator.is_empty() {
                    // Horizontal display (numerator/denominator as text)
                    "{numerator}/{denominator}"
                } else {
                    "{label}"
                }
            }
        }
    }
}

/// Tempo/BPM card component
/// Displays a tempo or BPM value
#[component]
pub fn TempoCard(
    position_percent: f64,
    label: String,
    bottom_offset: String,
    card_key: String,
    #[props(default = "text-xs".to_string())]
    text_size: String,
    #[props(default = "px-2 py-0.5 rounded text-xs font-medium text-white bg-black/70 whitespace-nowrap".to_string())]
    card_class: String,
    #[props(default = false)]
    position_above: bool,
    #[props(default = false)]
    left_align: bool,
) -> Element {
    let transform = if left_align { "" } else { "transform: translateX(-50%);" };
    let style = if position_above {
        format!("left: {}%; {} top: {};", position_percent, transform, bottom_offset)
    } else {
        format!("left: {}%; {} top: calc(100% + {});", position_percent, transform, bottom_offset)
    };
    
    // Adjust card class for left alignment - replace text-center with text-left
    let adjusted_card_class = if left_align && card_class.contains("text-center") {
        card_class.replace("text-center", "text-left")
    } else {
        card_class
    };
    
    rsx! {
        div {
            key: "{card_key}",
            class: "absolute pointer-events-none",
            style: style,
            div {
                class: "{adjusted_card_class}",
                style: if adjusted_card_class.contains("bg-black/70") {
                    "text-shadow: 0 1px 2px rgba(0, 0, 0, 0.5);"
                } else {
                    ""
                },
                "{label}"
            }
        }
    }
}

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
    pub is_tempo: bool,         // true if this is a tempo marker, false if time signature
    pub is_time_sig: bool,       // true if this is a time signature marker
    #[allow(dead_code)]
    pub show_line_only: bool,   // true if this marker should only show a line (no card) - for filtered markers
}

/// Measure indicator definition for section progress bar
#[derive(Clone, Debug, PartialEq)]
pub struct MeasureIndicator {
    pub position_percent: f64,  // 0-100, where this measure indicator is positioned (within section)
    pub measure_number: i32,    // Measure number (1-based for display)
    pub time_signature: Option<(u8, u8)>, // Time signature for this measure (numerator, denominator)
    pub musical_position: daw::primitives::MusicalPosition, // Musical position for seeking
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
    
    // Helper function to check if two markers overlap (assuming ~4rem label width with larger text)
    // Labels are centered, so they overlap if distance < ~2rem (4rem / 2)
    // At 100% width, 2rem ≈ 2% of typical container width
    let check_overlap = |pos1: f64, pos2: f64| -> bool {
        (pos1 - pos2).abs() < 2.0
    };
    
    // Calculate line and card positions - lines end at a fixed position, cards positioned to start there
    // Lines should go FROM bottom of container UP TO a fixed end point
    // Then cards are positioned so their bottom aligns with that end point
    let card_size = "sm"; // Song progress bar uses small cards
    let card_height_rem = match card_size {
        "xs" => 2.5,  // vertical fraction, smaller
        "sm" => 3.5,  // vertical fraction with text-xs + spacing + padding
        "md" => 4.0,  // vertical fraction, larger
        "lg" => 5.0,  // vertical fraction, largest
        _ => 3.5,
    };
    
    // Fixed line end position (where cards should start) - adjust this to control line length
    // Making it slightly lower (-0.75rem) so cards sit better on the lines
    let line_end_rem = -0.75; // Line ends at -0.75rem from container top
    // Container is h-20 (5rem), so line height = 5rem - (-0.75rem) = 5.75rem
    let line_height = format!("calc(5rem + {}rem)", -line_end_rem); // 5rem + 0.75rem = 5.75rem
    
    // Card top position = line_end - card_height (so card bottom aligns with line end)
    // line_end is -0.75rem, card_height is 3.5rem, so card top = -0.75 - 3.5 = -4.25rem
    let card_top_offset_rem = line_end_rem - card_height_rem; // -0.75 - 3.5 = -4.25
    let card_top_offset_str = format!("{}rem", card_top_offset_rem); // "-4.25rem"
    
    // Pre-calculate positions for time signature markers (all in same row, no staggering)
    // Only include markers that should show cards (filter out show_line_only markers)
    let time_sig_with_positions: Vec<_> = {
        tempo_markers.iter()
            .enumerate()
            .filter(|(_, m)| m.is_time_sig && !m.show_line_only)
            .map(|(orig_idx, marker)| {
                (orig_idx, marker, card_top_offset_str.clone())
            })
            .collect()
    };
    
    // Collect filtered time signature markers (show_line_only: true) for small lines at top of progress bar
    let filtered_time_sig_markers: Vec<_> = tempo_markers.iter()
        .enumerate()
        .filter(|(_, m)| m.is_time_sig && m.show_line_only)
        .collect();
    
    let tempo_with_positions: Vec<_> = {
        let tempo_markers_list: Vec<_> = tempo_markers.iter()
            .enumerate()
            .filter(|(_, m)| m.is_tempo)
            .collect();
        
        let mut result = Vec::new();
        for (idx, (orig_idx, marker)) in tempo_markers_list.iter().enumerate() {
            // Check if this marker overlaps with previous ones
            let needs_stagger = if idx > 0 {
                tempo_markers_list[..idx].iter().any(|(_, prev_marker)| {
                    check_overlap(marker.position_percent, prev_marker.position_percent)
                })
            } else {
                false
            };
            
            let bottom_offset = if needs_stagger && idx % 2 == 1 {
                "0.75rem"   // Lower lane for odd indices when overlapping
            } else {
                "0.5rem"    // Default higher lane (closer to progress bar)
            };
            result.push((*orig_idx, *marker, bottom_offset));
        }
        result
    };
    
    rsx! {
        div {
            class: "relative flex flex-col items-center justify-center h-20 w-full",
            // Tempo/Time Signature labels (overlaid above and below progress bar)
            // Time signature markers on top (staggered), BPM markers on bottom (staggered)
            if !tempo_markers.is_empty() {
                // Time signature markers (staggered - alternate heights)
                for (orig_idx, marker, top_offset) in time_sig_with_positions.iter() {
                    TimeSignatureCard {
                        position_percent: marker.position_percent,
                        label: marker.label.clone(),
                        top_offset: top_offset.to_string(),
                        card_key: format!("timesig-label-{orig_idx}"),
                        size: "sm".to_string(),
                    }
                }
                // Tempo/BPM markers (staggered - alternate heights)
                // First marker (at 0%) is left-aligned, others are centered
                for (orig_idx, marker, bottom_offset) in tempo_with_positions.iter() {
                    TempoCard {
                        position_percent: marker.position_percent,
                        label: marker.label.clone(),
                        bottom_offset: bottom_offset.to_string(),
                        card_key: format!("tempo-label-{orig_idx}"),
                        card_class: "text-xs font-medium text-center whitespace-nowrap px-1 py-1 rounded bg-accent text-accent-foreground border border-border".to_string(),
                        left_align: marker.position_percent < 0.5, // Left-align first marker at 0%
                    }
                }
                // Small lines at top of progress bar for filtered time signature markers (show_line_only)
                // Skip lines at the very start (0%) as they look ugly
                for (index, marker) in filtered_time_sig_markers.iter() {
                    if marker.position_percent > 0.5 {
                        div {
                            key: "filtered-timesig-line-{index}",
                            class: "absolute pointer-events-none z-50",
                            style: format!(
                                "left: {}%; width: 1px; top: 0; height: 0.5rem; background-color: rgba(255, 255, 255, 0.5); transform: translateX(-50%);",
                                marker.position_percent
                            ),
                        }
                    }
                }
                // Tempo/Time Signature marker lines (positioned relative to outer container)
                // Lines extend FROM bottom of progress bar UP TO bottom of time signature cards
                // OR DOWN TO bottom of BPM cards (if no time sig card at this position)
                // Skip lines at the very start (0%) as they look ugly
                for (index, marker) in tempo_markers.iter().enumerate() {
                    // Only render lines for markers not at the start (skip 0% or very close)
                    if marker.position_percent >= 0.5 {
                        if marker.is_time_sig && !marker.show_line_only {
                            // Time signature card exists - line goes from bottom of container UP through progress bar to card bottom
                            // Container is h-20 (5rem), card is at -4.25rem (top), card height ~3.5rem, so card bottom at -0.75rem
                            // Line should extend from bottom of container (0) up to card bottom (-0.75rem) = 5.75rem total
                            // This makes it visible on top of the progress bar as well
                            div {
                                key: "tempo-marker-{index}",
                                class: "absolute pointer-events-none z-50",
                                style: format!(
                                    "left: {}%; width: 1px; bottom: 0; height: {}; background-color: rgba(255, 255, 255, 0.7); transform: translateX(-50%);",
                                    marker.position_percent,
                                    line_height
                                ),
                            }
                        } else if marker.is_tempo {
                            // BPM card exists (no time sig) - line goes DOWN from progress bar bottom to card bottom
                            // Progress bar bottom is at 5rem (h-20 container), BPM card is at calc(100% + 0.5rem) = 5.5rem from container top
                            // BPM card height ~1.5rem, so bottom at 7rem from container top
                            // Line should go from progress bar bottom (5rem) to BPM card bottom (7rem) = 2rem height
                            div {
                                key: "tempo-marker-{index}",
                                class: "absolute pointer-events-none z-50",
                                style: format!(
                                    "left: {}%; width: 1px; top: 5rem; height: 2rem; background-color: rgba(255, 255, 255, 0.7); transform: translateX(-50%);",
                                    marker.position_percent
                                ),
                            }
                        }
                    }
                }
            }
            // Main segmented progress bar
            div {
                class: "relative w-full h-20 rounded-lg overflow-hidden bg-secondary px-4",
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
                // Section boundary lines (very subtle vertical lines at section boundaries)
                for (index, section_start, _section_end, _section_width, _section_color, _filled_percent, _section_name) in section_data.iter() {
                    // Add a line at the start of each section (except the first one at 0%)
                    if *section_start > 0.0 {
                        div {
                            key: "section-boundary-{index}",
                            class: "absolute pointer-events-none z-20",
                            style: format!(
                                "left: {}%; width: 1px; top: 0; bottom: 0; background-color: rgba(255, 255, 255, 0.15);",
                                section_start
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
                        let progress = current_progress.clamp(0.0, 100.0);
                        // Dark overlay starts from the progress point and covers the rest
                        // Add border-radius on the right side for rounded corners
                        format!(
                            "left: {}%; width: {}%; background-color: rgba(0, 0, 0, 0.6); border-top-right-radius: 0.5rem; border-bottom-right-radius: 0.5rem;",
                            progress,
                            100.0 - progress
                        )
                    },
                }
                // Section name text (rendered separately above everything, never darkened)
                // Skip rendering if section name is empty (e.g., Count-In sections)
                for (index, section_start, _section_end, section_width, _section_color, _filled_percent, section_name) in section_data.iter() {
                    if !section_name.is_empty() {
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
                        progress.clamp(0.0, 100.0),
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
