use dioxus::prelude::*;
use fts::setlist::Song;

/// Measure information for the grid
#[derive(Clone, Debug, PartialEq)]
pub struct MeasureInfo {
    pub measure_number: i32, // 1-based measure number
    pub bpm: f64,
    pub time_signature: (i32, i32), // (numerator, denominator)
    pub time_position: f64, // Time in seconds for this measure start
}

/// Ruler height constant (48px sections + 32px tempo/time sig + 32px measures = 112px)
pub const RULER_HEIGHT: f64 = 112.0;

/// Ruler component - displays measures, BPM, time signatures, and sections
#[component]
pub fn Ruler(
    /// Active song
    song: Option<Song>,
    /// Measures to display
    measures: Vec<MeasureInfo>,
    /// Sections data: (name, left, width, color)
    sections: Vec<(String, f64, f64, String)>,
    /// Pixels per second (for positioning)
    pixels_per_second: f64,
) -> Element {
    // Use timeline_start (count-in start if present, otherwise song_start)
    let timeline_start = song.as_ref().map(|s| {
        s.count_in_marker.as_ref()
            .and_then(|m| Some(m.position.time.to_seconds()))
            .unwrap_or(s.effective_start())
    }).unwrap_or(0.0);
    
    // Get section start times (for always showing measure numbers at section boundaries)
    let section_start_times: Vec<f64> = song.as_ref().map(|s| {
        let mut starts = Vec::new();
        // Add count-in start if present
        if let Some(count_in_seconds) = s.count_in_marker.as_ref()
            .and_then(|m| Some(m.position.time.to_seconds())) {
            starts.push(count_in_seconds);
        }
        // Add regular section starts
        for section in &s.sections {
            if let Some(start) = section.start_seconds() {
                starts.push(start);
            }
        }
        starts
    }).unwrap_or_default();
    
    // Calculate which measures to show based on zoom level
    // Progressive rendering: every 1, 2, 4, 8, etc. based on measure width in pixels
    let measures_clone = measures.clone();
    let song_clone = song.clone();
    let section_start_times_clone = section_start_times.clone();
    let pixels_per_second_clone = pixels_per_second;
    let visible_measures = use_memo(move || {
        let measures = &measures_clone;
        let pixels_per_second = pixels_per_second_clone;
        
        if measures.is_empty() {
            return Vec::new();
        }
        
        // Calculate average measure width in pixels
        let avg_measure_width_px = if measures.len() > 1 {
            let first_measure = &measures[0];
            let second_measure = &measures[1];
            let time_diff = second_measure.time_position - first_measure.time_position;
            time_diff * pixels_per_second
        } else {
            // Fallback: estimate based on first measure's BPM and time signature
            let first_measure = &measures[0];
            let beats_per_measure = first_measure.time_signature.0 as f64;
            let seconds_per_beat = 60.0 / first_measure.bpm;
            let measure_duration = beats_per_measure * seconds_per_beat;
            measure_duration * pixels_per_second
        };
        
        // Determine interval based on measure width
        // If measures are very narrow (< 30px), show every 8th
        // If measures are narrow (< 60px), show every 4th
        // If measures are medium (< 120px), show every 2nd
        // If measures are wide (>= 120px), show every measure
        let interval = if avg_measure_width_px < 30.0 {
            8
        } else if avg_measure_width_px < 60.0 {
            4
        } else if avg_measure_width_px < 120.0 {
            2
        } else {
            1
        };
        
        // Filter measures to show only those at the interval
        // But always include measures at section boundaries
        // Interval restarts at each section
        let mut visible = Vec::new();
        
        // Get all section boundaries (start and end times)
        let section_boundaries: Vec<f64> = song_clone.as_ref().map(|s| {
            let mut boundaries = Vec::new();
            // Add count-in start if present
            if let Some(count_in_seconds) = s.count_in_marker.as_ref()
                .and_then(|m| Some(m.position.time.to_seconds())) {
                boundaries.push(count_in_seconds);
            }
            // Add all section starts
            for section in &s.sections {
                if let Some(start) = section.start_seconds() {
                    boundaries.push(start);
                }
            }
            boundaries.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
            boundaries
        }).unwrap_or_default();
        
        // Group measures by section
        // For each section, we'll restart the interval counter
        let mut current_section_start: Option<f64> = None;
        let mut measure_count_in_section = 0;
        
        for measure in measures.iter() {
            // Check if this measure is at a section boundary (always show)
            let is_section_boundary = section_start_times_clone.iter().any(|&start_time| {
                (measure.time_position - start_time).abs() < 0.01
            });
            
            // Determine which section this measure belongs to
            // Find the section start that this measure is after
            let measure_section_start = if section_boundaries.is_empty() {
                // If no sections, use None (all measures in one "section")
                None
            } else {
                section_boundaries.iter()
                    .rev()
                    .find(|&&boundary| measure.time_position >= boundary)
                    .copied()
            };
            
            // If we've moved to a new section, reset the counter
            if measure_section_start != current_section_start {
                current_section_start = measure_section_start;
                measure_count_in_section = 0;
            }
            
            // Always show section boundaries, or show if at interval within section
            // Also always show the first measure in each section (count_in_section == 0)
            if is_section_boundary || measure_count_in_section % interval == 0 {
                visible.push(measure.clone());
            }
            
            measure_count_in_section += 1;
        }
        
        // Fallback: if no measures were selected (shouldn't happen, but safety check)
        // show at least the first and last measures, or all measures if very few
        if visible.is_empty() {
            if measures.len() <= 10 {
                // If there are 10 or fewer measures, just show all of them
                return measures.clone();
            } else {
                // Otherwise show first and last
                let mut fallback = Vec::new();
                fallback.push(measures[0].clone());
                fallback.push(measures[measures.len() - 1].clone());
                return fallback;
            }
        }
        
        visible
    });
    
    rsx! {
        div {
            class: "flex flex-col",
            // Sections track (top)
            div {
                class: "sticky top-0 z-10 bg-background border-b border-border relative",
                style: "height: 48px;",
                for (section_name, left, width, color) in sections.iter() {
                    div {
                        key: "{section_name}",
                        class: "absolute top-0 h-full rounded border border-border/50 flex items-center justify-center text-xs font-medium text-white",
                        style: format!("left: {}px; width: {}px; background-color: {};", 
                            left, width.max(1.0), color),
                        "{section_name}"
                    }
                }
            }
            
            // Time sig and tempo changes (combined in same lane)
            {
                // Filter measures to only show when tempo or time sig changes
                let mut prev_bpm: Option<f64> = None;
                let mut prev_time_sig: Option<(i32, i32)> = None;
                let changes: Vec<_> = measures.iter()
                    .filter(|m| {
                        let bpm_changed = prev_bpm.map(|prev| (prev - m.bpm).abs() > 0.1).unwrap_or(true);
                        let ts_changed = prev_time_sig.map(|prev| prev != m.time_signature).unwrap_or(true);
                        let show = bpm_changed || ts_changed;
                        prev_bpm = Some(m.bpm);
                        prev_time_sig = Some(m.time_signature);
                        show
                    })
                    .collect();
                
                rsx! {
                    div {
                        class: "sticky top-12 z-10 bg-background border-b border-border relative",
                        style: "height: 32px;",
                        for measure in changes.iter() {
                            div {
                                key: "change-{measure.measure_number}",
                                class: "absolute border-r border-border px-2 py-1 text-xs text-muted-foreground",
                                style: format!("left: {}px; min-width: {}px;", 
                                    (measure.time_position - timeline_start) * pixels_per_second,
                                    (60.0 / measure.bpm * measure.time_signature.0 as f64) * pixels_per_second.max(40.0)
                                ),
                                "{measure.bpm:.0} BPM {measure.time_signature.0}/{measure.time_signature.1}"
                            }
                        }
                    }
                }
            }
            
            // Measure numbers row - positioned absolutely based on time
            // Only show measures based on zoom level (progressive rendering)
            div {
                class: "sticky top-20 z-10 bg-background border-b border-border relative",
                style: "height: 32px;",
                for measure in visible_measures().iter() {
                    div {
                        key: "{measure.measure_number}",
                        class: "absolute border-r border-border px-2 py-1 text-xs text-muted-foreground",
                        style: format!("left: {}px; min-width: {}px;", 
                            (measure.time_position - timeline_start) * pixels_per_second,
                            (60.0 / measure.bpm * measure.time_signature.0 as f64) * pixels_per_second.max(40.0)
                        ),
                        "{measure.measure_number}"
                    }
                }
            }
        }
    }
}

