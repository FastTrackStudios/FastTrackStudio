use dioxus::prelude::*;
use fts::setlist::infra::dioxus::SETLIST;

use crate::components::progress::{TimeSignatureCard, TempoCard, ProgressSection, TempoMarker, MeasureIndicator};

/// Section progress bar component
/// 
/// Shows progress within the current section as a small bar below the main progress bar.
/// Can display tempo markers, time signature markers, and measure indicators for a zoomed-in view.
#[component]
pub fn SectionProgressBar(
    progress: Signal<f64>,
    sections: Vec<ProgressSection>,
    #[props(default)]
    tempo_markers: Vec<TempoMarker>,
    #[props(default)]
    measure_indicators: Vec<MeasureIndicator>,
    #[props(default)]
    song_key: Option<String>,
    #[props(default)]
    on_measure_click: Option<Callback<daw::primitives::MusicalPosition>>,
) -> Element {
    let current_progress = progress();
    
    // Track progress changes to detect jumps and disable animations
    let mut prev_progress = use_signal(|| None::<f64>);
    let mut prev_song_key = use_signal(|| None::<String>);
    
    let song_key_for_memo = song_key.clone();
    let should_animate = use_memo(move || {
        let prev = prev_progress();
        let prev_key = prev_song_key();
        let song_changed = prev_key != song_key_for_memo;
        let large_jump = if let Some(prev_val) = prev {
            (current_progress - prev_val).abs() > 5.0
        } else {
            false
        };
        !song_changed && !large_jump && prev.is_some()
    });
    
    let current_progress_for_effect = current_progress;
    let song_key_for_effect = song_key.clone();
    use_effect(move || {
        prev_progress.set(Some(current_progress_for_effect));
        if prev_song_key() != song_key_for_effect {
            prev_song_key.set(song_key_for_effect.clone());
        }
    });
    
    // Get the active section color from the SETLIST API (same approach as SongProgressBar)
    // This ensures we use the actual current section, not calculated from progress
    let base_color = SETLIST.read().as_ref()
        .and_then(|api| api.active_section())
        .map(|section| section.color_bright())
        .unwrap_or_else(|| {
            // Fallback to first section color if no active section
            sections.first()
                .map(|s| s.color.clone())
                .unwrap_or_else(|| "rgb(100, 100, 100)".to_string())
        });
    
    // Convert measure indicators into measure sections for clickable/highlightable measures
    let measure_sections: Vec<_> = measure_indicators.iter().enumerate().map(|(idx, measure)| {
        let measure_start = measure.position_percent;
        // End position is the next measure's start, or 100% if this is the last measure
        let measure_end = if idx + 1 < measure_indicators.len() {
            measure_indicators[idx + 1].position_percent
        } else {
            100.0
        };
        let measure_width = measure_end - measure_start;
        
        // Calculate how much of this measure is filled
        let filled_percent = if current_progress <= measure_start {
            0.0
        } else if current_progress >= measure_end {
            100.0
        } else {
            let progress_in_measure = current_progress - measure_start;
            (progress_in_measure / measure_width) * 100.0
        };
        
        (idx, measure_start, measure_end, measure_width, measure.clone(), filled_percent)
    }).collect();
    
    // Find the active measure
    let active_measure = measure_sections.iter().find(|(_, start, end, _, _, _)| {
        current_progress >= *start && current_progress < *end
    }).or_else(|| {
        if let Some((_, start, end, _, _, _)) = measure_sections.last() {
            if current_progress >= *start && current_progress <= *end {
                measure_sections.last()
            } else {
                None
            }
        } else {
            None
        }
    });
    
    // Helper function to check if two markers overlap (same as SongProgressBar)
    let check_overlap = |pos1: f64, pos2: f64| -> bool {
        (pos1 - pos2).abs() < 2.0
    };
    
    // Pre-calculate positions for time signature markers (all in same row, no staggering)
    // For section progress bar, use -2rem offset (cards closer to bar for compact display)
    let time_sig_with_positions: Vec<_> = {
        tempo_markers.iter()
            .enumerate()
            .filter(|(_, m)| m.is_time_sig)
            .map(|(orig_idx, marker)| {
                (orig_idx, marker, "-2rem")
            })
            .collect()
    };
    
    // Calculate line positions dynamically based on card size
    // Lines should go FROM progress bar UP TO bottom of cards
    // Card top is at -2rem (card_top_offset)
    // For "sm" size cards with horizontal text "4/4": 
    //   - text-xs line-height: ~0.75rem
    //   - py-0.5 padding: 0.5rem top + 0.5rem bottom = 1rem total
    //   - border: ~0.125rem (1px top + 1px bottom)
    //   - Total card height: ~1.75rem
    // Card bottom = -2rem + 1.75rem = -0.25rem
    // Line should extend from progress bar (0) to card bottom (-0.25rem)
    // So line height = 0 - (-0.25rem) = 0.25rem
    let card_size = "sm"; // Section progress bar uses small cards
    let card_height_rem = match card_size {
        "xs" => 1.25,  // text-xs + py-0.5 + border
        "sm" => 1.75,  // text-xs + py-0.5 + border (horizontal text, more accurate)
        "md" => 2.125, // text-sm + py-1 + border
        "lg" => 2.625, // text-base + py-1 + border
        _ => 1.75,
    };
    let card_top_offset_rem = 2.0; // Card position in rem (negative, so -2rem)
    // Card bottom position: card_top + card_height = -2rem + 1.75rem = -0.25rem
    // Line height is distance from progress bar (0) to card bottom (-0.25rem) = 0.25rem
    let card_bottom_rem = -card_top_offset_rem + card_height_rem; // -2 + 1.75 = -0.25
    let line_height_rem = -card_bottom_rem; // 0.25 (distance from 0 to -0.25rem)
    let line_height = format!("{}rem", line_height_rem);
    
    // Pre-calculate positions for tempo markers with staggering
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
            class: "relative flex flex-col items-center justify-center w-full",
            // Time signature and tempo markers (positioned above and below progress bar)
            // Cards are positioned relative to the outer container
            if !tempo_markers.is_empty() {
                // Time signature markers (above progress bar) - horizontal display for section progress bar
                for (orig_idx, marker, top_offset) in time_sig_with_positions.iter() {
                    TimeSignatureCard {
                        position_percent: marker.position_percent,
                        label: marker.label.clone(),
                        top_offset: top_offset.to_string(),
                        card_key: format!("section-timesig-label-{orig_idx}"),
                        vertical: false,
                        size: "sm".to_string(),
                    }
                }
                // Tempo/BPM markers (below progress bar, with staggering)
                // First marker (at 0%) is left-aligned, others are centered
                for (orig_idx, marker, bottom_offset) in tempo_with_positions.iter() {
                    TempoCard {
                        position_percent: marker.position_percent,
                        label: marker.label.clone(),
                        bottom_offset: bottom_offset.to_string(),
                        card_key: format!("section-tempo-label-{orig_idx}"),
                        card_class: "text-xs font-medium text-center whitespace-nowrap px-1 py-1 rounded bg-accent text-accent-foreground border border-border".to_string(),
                        left_align: marker.position_percent < 0.5, // Left-align first marker at 0%
                    }
                }
                // Tempo/Time signature marker lines (positioned relative to outer container, responsive to card size)
                // Lines extend FROM progress bar UP TO bottom of cards - thinner lines
                // Using calc to ensure lines meet exactly at card bottom: card is at -2rem, card height ~1.75rem, so bottom at -0.25rem
                // Line height = 2rem - 1.75rem = 0.25rem (distance from progress bar to card bottom)
                for (index, marker) in tempo_markers.iter().enumerate() {
                    div {
                        key: "section-tempo-marker-{index}",
                        class: "absolute pointer-events-none z-40",
                        style: format!(
                            "left: {}%; width: 1px; bottom: 0; height: calc(2rem - {}rem); background-color: rgba(255, 255, 255, 0.7); transform: translateX(-50%);",
                            marker.position_percent,
                            card_height_rem
                        ),
                    }
                }
            }
            // Progress bar content (with padding to align with cards)
            div {
                class: "relative w-full h-4 rounded-lg overflow-visible bg-secondary px-4",
                // Measure sections (clickable and highlightable)
                for (measure_idx, measure_start, _measure_end, measure_width, measure, filled_percent) in measure_sections.iter() {
                    div {
                        key: "measure-section-{measure_idx}",
                        class: if on_measure_click.is_some() { 
                            "absolute h-full z-10 cursor-pointer transition-all duration-200 hover:brightness-110 hover:ring-2 hover:ring-white hover:ring-opacity-50" 
                        } else { 
                            "absolute h-full z-0" 
                        },
                        style: format!(
                            "left: {}%; width: {}%;",
                            measure_start,
                            measure_width
                        ),
                        onclick: {
                            let callback_opt = on_measure_click.clone();
                            let musical_pos = measure.musical_position.clone();
                            move |_| {
                                if let Some(callback) = &callback_opt {
                                    callback.call(musical_pos.clone());
                                }
                            }
                        },
                        // Measure background (full color, will be brightened by progress overlay)
                        div {
                            class: "absolute inset-0 h-full transition-all duration-200",
                            style: format!(
                                "background-color: {}; opacity: 0.3;",
                                base_color
                            ),
                        }
                        // Filled portion within this measure
                        div {
                            class: if should_animate() {
                                "absolute left-0 top-0 h-full transition-all duration-100 ease-linear"
                            } else {
                                "absolute left-0 top-0 h-full"
                            },
                            style: format!(
                                "width: {}%; background-color: {}; opacity: 0.8;",
                                filled_percent,
                                base_color
                            ),
                        }
                    }
                }
                // Measure boundary lines (subtle vertical lines at measure boundaries)
                for (index, measure) in measure_indicators.iter().enumerate() {
                    if measure.position_percent > 0.0 {
                        div {
                            key: "measure-boundary-{index}",
                            class: "absolute pointer-events-none z-30",
                            style: format!(
                                "left: {}%; width: 1px; top: 0; bottom: 0; background-color: rgba(255, 255, 255, 0.3); transform: translateX(-50%);",
                                measure.position_percent
                            ),
                        }
                    }
                }
            }
        }
    }
}
