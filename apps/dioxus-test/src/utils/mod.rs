use setlist::{Song, Section, SectionType};
use std::collections::HashMap;

/// Helper function to get project name from song metadata
pub fn get_project_name(song: &Song) -> Option<String> {
    song.metadata
        .get("project_name")
        .or_else(|| song.metadata.get("Project"))
        .or_else(|| song.metadata.get("project"))
        .cloned()
}

/// Calculate song progress percentage (0-100) based on transport position
pub fn calculate_song_progress(song: &Song, transport_position: f64) -> f64 {
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
        return 0.0;
    }
    
    let relative_position = (transport_position - song_start).max(0.0);
    let progress = (relative_position / song_duration).min(1.0);
    progress * 100.0
}

/// Calculate section progress percentage (0-100) based on transport position
pub fn calculate_section_progress(section: &Section, transport_position: f64) -> f64 {
    let start = section.start_position.time.to_seconds();
    let end = section.end_position.time.to_seconds();
    
    if end <= start {
        return 0.0;
    }
    
    if transport_position >= end {
        return 100.0;
    }
    if transport_position < start {
        return 0.0;
    }
    
    let progress = ((transport_position - start) / (end - start)).clamp(0.0, 1.0);
    progress * 100.0
}

// Color palette: [brightest, bright, medium, dark, darkest]
const COLOR_PALETTE: &[&[&str]] = &[
    &["rgb(239, 68, 68)", "rgb(220, 38, 38)", "rgb(185, 28, 28)", "rgb(153, 27, 27)", "rgb(127, 29, 29)"], // Red
    &["rgb(34, 197, 94)", "rgb(22, 163, 74)", "rgb(21, 128, 61)", "rgb(20, 83, 45)", "rgb(22, 101, 52)"], // Green
    &["rgb(59, 130, 246)", "rgb(37, 99, 235)", "rgb(29, 78, 216)", "rgb(30, 64, 175)", "rgb(30, 58, 138)"], // Blue
    &["rgb(168, 85, 247)", "rgb(147, 51, 234)", "rgb(126, 34, 206)", "rgb(107, 33, 168)", "rgb(88, 28, 135)"], // Purple
    &["rgb(251, 191, 36)", "rgb(245, 158, 11)", "rgb(217, 119, 6)", "rgb(180, 83, 9)", "rgb(146, 64, 14)"], // Yellow
    &["rgb(236, 72, 153)", "rgb(219, 39, 119)", "rgb(190, 24, 93)", "rgb(157, 23, 77)", "rgb(131, 24, 67)"], // Pink
    &["rgb(20, 184, 166)", "rgb(15, 118, 110)", "rgb(17, 94, 89)", "rgb(19, 78, 74)", "rgb(19, 78, 74)"], // Teal
];

/// Get song color (bright) for progress bars
pub fn get_song_color_bright(index: usize) -> String {
    let palette_idx = index % COLOR_PALETTE.len();
    COLOR_PALETTE[palette_idx][1].to_string() // Bright shade
}

/// Get song color (muted) for background
pub fn get_song_color_muted(index: usize) -> String {
    let palette_idx = index % COLOR_PALETTE.len();
    COLOR_PALETTE[palette_idx][4].to_string() // Darkest shade
}

/// Get section color (bright) for progress bars
pub fn get_section_color_bright(song_idx: usize, section_idx: usize) -> String {
    let palette_idx = (song_idx * 10 + section_idx) % COLOR_PALETTE.len();
    COLOR_PALETTE[palette_idx][1].to_string() // Bright shade
}

/// Get section color (muted) for background
pub fn get_section_color_muted(song_idx: usize, section_idx: usize) -> String {
    let palette_idx = (song_idx * 10 + section_idx) % COLOR_PALETTE.len();
    COLOR_PALETTE[palette_idx][4].to_string() // Darkest shade
}

/// Helper function to get color for section type (for main progress bar)
pub fn get_section_type_color(section_type: &SectionType) -> String {
    match section_type {
        SectionType::Intro => "rgb(59, 130, 246)".to_string(),      // blue
        SectionType::Verse => "rgb(34, 197, 94)".to_string(),        // green
        SectionType::Chorus => "rgb(168, 85, 247)".to_string(),      // purple
        SectionType::Bridge => "rgb(239, 68, 68)".to_string(),       // red
        SectionType::Outro => "rgb(59, 130, 246)".to_string(),        // blue
        SectionType::Instrumental => "rgb(251, 191, 36)".to_string(), // yellow
        SectionType::Custom => "rgb(100, 100, 100)".to_string(),      // gray
        SectionType::Pre(_) => "rgb(34, 197, 94)".to_string(),        // green
        SectionType::Post(_) => "rgb(34, 197, 94)".to_string(),      // green
    }
}

