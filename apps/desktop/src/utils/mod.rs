use setlist::{Song, Section, SectionType};
use primitives::{Position, MusicalPosition, TimePosition, TimeSignature};
use std::collections::HashMap;

pub mod navigation;
pub mod sidebar_helpers;

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

/// Convert u32 color (RGB format: (r << 16) | (g << 8) | b) to RGB string
fn color_u32_to_rgb(color: u32) -> (u8, u8, u8) {
    let r = ((color >> 16) & 0xFF) as u8;
    let g = ((color >> 8) & 0xFF) as u8;
    let b = (color & 0xFF) as u8;
    (r, g, b)
}

/// Convert RGB tuple to CSS rgb() string
fn rgb_to_css_string(r: u8, g: u8, b: u8) -> String {
    format!("rgb({}, {}, {})", r, g, b)
}

/// Create a brighter variant of an RGB color
fn brighten_color(r: u8, g: u8, b: u8) -> (u8, u8, u8) {
    // Increase brightness by ~20%
    let r = (r as f32 * 1.2).min(255.0) as u8;
    let g = (g as f32 * 1.2).min(255.0) as u8;
    let b = (b as f32 * 1.2).min(255.0) as u8;
    (r, g, b)
}

/// Create a muted/darker variant of an RGB color
fn mute_color(r: u8, g: u8, b: u8) -> (u8, u8, u8) {
    // Reduce brightness by ~40%
    let r = (r as f32 * 0.6) as u8;
    let g = (g as f32 * 0.6) as u8;
    let b = (b as f32 * 0.6) as u8;
    (r, g, b)
}

/// Get song color from the song struct (from markers)
/// Returns bright variant for progress bars
pub fn get_song_color_bright(song: &Song) -> String {
    // Get color from song_region_start_marker first (preserves song region color)
    let color = song.song_region_start_marker.as_ref()
        .and_then(|m| m.color)
        .filter(|&c| c != 0)
        .or_else(|| {
            // Fallback to start_marker if available
            song.start_marker.as_ref()
                .and_then(|m| m.color)
                .filter(|&c| c != 0)
        });
    
    if let Some(color_u32) = color {
        let (r, g, b) = color_u32_to_rgb(color_u32);
        let (r, g, b) = brighten_color(r, g, b);
        rgb_to_css_string(r, g, b)
    } else {
        // Default gray if no color found
        "rgb(128, 128, 128)".to_string()
    }
}

/// Get song color from the song struct (from markers)
/// Returns muted variant for backgrounds
pub fn get_song_color_muted(song: &Song) -> String {
    // Get color from song_region_start_marker first (preserves song region color)
    let color = song.song_region_start_marker.as_ref()
        .and_then(|m| m.color)
        .filter(|&c| c != 0)
        .or_else(|| {
            // Fallback to start_marker if available
            song.start_marker.as_ref()
                .and_then(|m| m.color)
                .filter(|&c| c != 0)
        });
    
    if let Some(color_u32) = color {
        let (r, g, b) = color_u32_to_rgb(color_u32);
        let (r, g, b) = mute_color(r, g, b);
        rgb_to_css_string(r, g, b)
    } else {
        // Default dark gray if no color found
        "rgb(80, 80, 80)".to_string()
    }
}

/// Get section color from the section struct
/// Returns bright variant for progress bars
pub fn get_section_color_bright(section: &Section) -> String {
    if let Some(color_u32) = section.color.filter(|&c| c != 0) {
        let (r, g, b) = color_u32_to_rgb(color_u32);
        let (r, g, b) = brighten_color(r, g, b);
        rgb_to_css_string(r, g, b)
    } else {
        // Default gray if no color found
        "rgb(128, 128, 128)".to_string()
    }
}

/// Get section color from the section struct
/// Returns muted variant for backgrounds
pub fn get_section_color_muted(section: &Section) -> String {
    if let Some(color_u32) = section.color.filter(|&c| c != 0) {
        let (r, g, b) = color_u32_to_rgb(color_u32);
        let (r, g, b) = mute_color(r, g, b);
        rgb_to_css_string(r, g, b)
    } else {
        // Default dark gray if no color found
        "rgb(80, 80, 80)".to_string()
    }
}

/// Get tempo at a specific position from song's tempo_time_sig_changes
/// Returns default tempo (120 BPM) if no tempo changes found
pub fn get_tempo_at_position(song: &Song, position_seconds: f64) -> f64 {
    // Find the last tempo change before or at this position
    let mut current_tempo = 120.0; // Default tempo
    
    for change in &song.tempo_time_sig_changes {
        if change.position <= position_seconds {
            current_tempo = change.tempo;
        } else {
            break;
        }
    }
    
    current_tempo
}

/// Get time signature at a specific position from song's tempo_time_sig_changes
/// Returns default time signature (4/4) if no time signature changes found
pub fn get_time_signature_at_position(song: &Song, position_seconds: f64) -> TimeSignature {
    // Find the last time signature change before or at this position
    let mut current_time_sig = TimeSignature::new(4, 4); // Default 4/4
    
    for change in &song.tempo_time_sig_changes {
        if change.position <= position_seconds {
            if let Some((num, den)) = change.time_signature {
                current_time_sig = TimeSignature::new(num, den);
            }
        } else {
            break;
        }
    }
    
    current_time_sig
}

/// Format musical position as string (e.g., "1.1.000")
pub fn format_musical_position(musical: &MusicalPosition) -> String {
    format!("{}.{}.{:03}", musical.measure + 1, musical.beat + 1, musical.subdivision)
}

/// Format time position as string (e.g., "1:23.456")
pub fn format_time_position(time: &TimePosition) -> String {
    format!("{}:{:02}.{:03}", time.minutes, time.seconds, time.milliseconds)
}

