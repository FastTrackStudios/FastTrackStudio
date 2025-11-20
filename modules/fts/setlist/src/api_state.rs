//! Setlist API State - Computed/derived fields for API responses
//!
//! This module contains structures that compute derived fields from the core Setlist
//! for use in WebSocket/OSC API responses. These fields are computed on-demand and
//! not stored in the core domain model.

use serde::{Deserialize, Serialize};
use specta::Type;
use std::collections::HashMap;

use super::core::{Setlist, Song, Section};

/// Helper to convert color u32 to hex string
fn color_to_hex(color: Option<u32>) -> String {
    color.map(|c| {
        let r = (c >> 16) & 0xFF;
        let g = (c >> 8) & 0xFF;
        let b = c & 0xFF;
        format!("#{:02x}{:02x}{:02x}", r, g, b)
    }).unwrap_or_else(|| "#808080".to_string())
}

/// Helper to convert color u32 to color name (simplified - could be enhanced)
fn color_to_name(color: Option<u32>) -> String {
    // For now, just return "Custom" - could map to color names
    color.map(|_| "Custom".to_string()).unwrap_or_else(|| "Default".to_string())
}

/// Song information for API responses
#[derive(Debug, Clone, Serialize, Deserialize, Type)]
pub struct SongApiInfo {
    pub index: usize,
    pub name: String,
    pub color: Option<u32>,
    pub color_hex: String,
    pub color_name: String,
    pub duration_seconds: f64,
    pub start_position_beats: f64,
    pub end_position_beats: f64,
    pub length_measures: Option<f64>,
}

/// Section information for API responses
#[derive(Debug, Clone, Serialize, Deserialize, Type)]
pub struct SectionApiInfo {
    pub index: usize,
    pub name: String,
    pub color: Option<u32>,
    pub color_hex: String,
    pub color_name: String,
    pub duration_seconds: f64,
    pub start_position_beats: f64,
    pub end_position_beats: f64,
    pub length_measures: Option<f64>,
}

/// Complete setlist API state with all computed fields
#[derive(Debug, Clone, Serialize, Deserialize, Type)]
pub struct SetlistApiState {
    // Basic setlist info
    pub name: String,
    pub songs: Vec<String>, // Just song names
    pub song_colors: Vec<String>, // Hex colors
    pub song_durations: Vec<f64>, // Durations in seconds
    
    // Current song info
    pub active_song_name: String,
    pub active_song_index: Option<usize>,
    pub active_song_start: f64, // beats
    pub active_song_end: f64, // beats
    pub active_song_duration: f64, // seconds
    pub active_song_color: (String, String), // (name, hex)
    pub active_song_progress: f64, // 0-1
    
    // Current song sections
    pub sections: Vec<String>, // Section names of current song
    pub section_colors: Vec<String>, // Hex colors of sections
    
    // Current section info
    pub active_section_name: String,
    pub active_section_index: Option<usize>,
    pub active_section_start: f64, // beats
    pub active_section_end: f64, // beats
    pub active_section_color: (String, String), // (name, hex)
    pub active_section_duration: f64, // seconds
    pub active_section_progress: f64, // 0-1
    
    // Next section info
    pub next_section_name: String,
    pub next_section_index: Option<usize>,
    pub next_section_start: f64, // beats
    pub next_section_end: f64, // beats
    pub next_section_color: (String, String), // (name, hex)
    pub next_section_duration: f64, // seconds
    
    // Next song info
    pub next_song_name: String,
    pub next_song_index: Option<usize>,
    pub next_song_duration: f64, // seconds
    pub next_song_color: (String, String), // (name, hex)
    
    // Queued info
    pub queued_name: (String, String), // (song_name, section_name)
    pub queued_index: (Option<usize>, Option<usize>), // (song_index, section_index)
    
    // Loop info
    pub loop_enabled: bool,
    pub loop_start: f64, // beats
    pub loop_end: f64, // beats
    
    // Count-in info
    pub is_counting_in: bool,
    
    // Remaining time
    pub remaining_time_in_song: f64, // seconds
    pub remaining_time_in_set: f64, // seconds
}

impl SetlistApiState {
    /// Build API state from Setlist and current transport position
    /// 
    /// This computes all derived fields from the base Setlist struct.
    /// The `current_position_beats` and `current_position_seconds` are used
    /// to calculate progress and determine active song/section.
    pub fn from_setlist(
        setlist: &Setlist,
        active_song_index: Option<usize>,
        current_position_beats: f64,
        current_position_seconds: f64,
        tempo: f64, // BPM
        time_signature_numerator: i32, // e.g., 4 for 4/4
    ) -> Self {
        let songs: Vec<String> = setlist.songs.iter().map(|s| s.name.clone()).collect();
        
        // Calculate song colors and durations
        let song_colors: Vec<String> = setlist.songs.iter().map(|song| {
            let color = song.song_region_start_marker.as_ref()
                .and_then(|m| m.color)
                .or_else(|| song.start_marker.as_ref().and_then(|m| m.color));
            color_to_hex(color)
        }).collect();
        
        let song_durations: Vec<f64> = setlist.songs.iter().map(|song| {
            song.duration()
        }).collect();
        
        // Determine active song
        let active_song = active_song_index
            .and_then(|idx| setlist.songs.get(idx));
        
        let active_song_name = active_song.map(|s| s.name.clone()).unwrap_or_default();
        let active_song_color = active_song.map(|song| {
            let color = song.song_region_start_marker.as_ref()
                .and_then(|m| m.color)
                .or_else(|| song.start_marker.as_ref().and_then(|m| m.color));
            (color_to_name(color), color_to_hex(color))
        }).unwrap_or_else(|| ("Default".to_string(), "#808080".to_string()));
        
        // Calculate active song positions and duration
        let (active_song_start, active_song_end, active_song_duration) = active_song.map(|song| {
            let start_beats = song.effective_start() * (tempo / 60.0);
            let end_beats = song.effective_end() * (tempo / 60.0);
            let duration = song.duration();
            (start_beats, end_beats, duration)
        }).unwrap_or((0.0, 0.0, 0.0));
        
        // Calculate active song progress
        let active_song_progress = if active_song_duration > 0.0 {
            let relative_pos = current_position_seconds - active_song_start / (tempo / 60.0);
            (relative_pos / active_song_duration).max(0.0).min(1.0)
        } else {
            0.0
        };
        
        // Get sections of active song
        let sections: Vec<String> = active_song
            .map(|song| song.sections.iter().map(|s| s.name.clone()).collect())
            .unwrap_or_default();
        
        let section_colors: Vec<String> = active_song
            .map(|song| song.sections.iter().map(|section| {
                color_to_hex(section.color)
            }).collect())
            .unwrap_or_default();
        
        // Find active section
        let active_section = active_song.and_then(|song| {
            song.sections.iter()
                .enumerate()
                .find(|(_, section)| {
                    let start = section.start_position.time.to_seconds();
                    let end = section.end_position.time.to_seconds();
                    current_position_seconds >= start && current_position_seconds <= end
                })
                .map(|(idx, section)| (idx, section))
        });
        
        let (active_section_index, active_section) = active_section
            .map(|(idx, section)| (Some(idx), Some(section)))
            .unwrap_or((None, None));
        
        let active_section_name = active_section.map(|s| s.name.clone()).unwrap_or_default();
        let active_section_color = active_section.map(|section| {
            let color = section.color;
            (color_to_name(color), color_to_hex(color))
        }).unwrap_or_else(|| ("Default".to_string(), "#808080".to_string()));
        
        let (active_section_start, active_section_end, active_section_duration) = active_section.map(|section| {
            let start_beats = section.start_position.time.to_seconds() * (tempo / 60.0);
            let end_beats = section.end_position.time.to_seconds() * (tempo / 60.0);
            let duration = section.duration();
            (start_beats, end_beats, duration)
        }).unwrap_or((0.0, 0.0, 0.0));
        
        // Calculate active section progress
        let active_section_progress = if active_section_duration > 0.0 {
            let relative_pos = current_position_seconds - active_section_start / (tempo / 60.0);
            (relative_pos / active_section_duration).max(0.0).min(1.0)
        } else {
            0.0
        };
        
        // Find next section
        let next_section = active_song.and_then(|song| {
            if let Some(active_idx) = active_section_index {
                song.sections.get(active_idx + 1)
                    .map(|section| (active_idx + 1, section))
            } else {
                song.sections.first().map(|section| (0, section))
            }
        });
        
        let (next_section_index, next_section) = next_section
            .map(|(idx, section)| (Some(idx), Some(section)))
            .unwrap_or((None, None));
        
        let next_section_name = next_section.map(|s| s.name.clone()).unwrap_or_default();
        let next_section_color = next_section.map(|section| {
            let color = section.color;
            (color_to_name(color), color_to_hex(color))
        }).unwrap_or_else(|| ("Default".to_string(), "#808080".to_string()));
        
        let (next_section_start, next_section_end, next_section_duration) = next_section.map(|section| {
            let start_beats = section.start_position.time.to_seconds() * (tempo / 60.0);
            let end_beats = section.end_position.time.to_seconds() * (tempo / 60.0);
            let duration = section.duration();
            (start_beats, end_beats, duration)
        }).unwrap_or((0.0, 0.0, 0.0));
        
        // Find next song
        let next_song = active_song_index
            .and_then(|idx| setlist.songs.get(idx + 1));
        
        let next_song_name = next_song.map(|s| s.name.clone()).unwrap_or_default();
        let next_song_index = active_song_index.map(|idx| idx + 1).filter(|&idx| idx < setlist.songs.len());
        let next_song_duration = next_song.map(|song| song.duration()).unwrap_or(0.0);
        let next_song_color = next_song.map(|song| {
            let color = song.song_region_start_marker.as_ref()
                .and_then(|m| m.color)
                .or_else(|| song.start_marker.as_ref().and_then(|m| m.color));
            (color_to_name(color), color_to_hex(color))
        }).unwrap_or_else(|| ("Default".to_string(), "#808080".to_string()));
        
        // Calculate remaining time
        let remaining_time_in_song = if active_song_duration > 0.0 {
            (active_song_duration - (current_position_seconds - active_song_start / (tempo / 60.0))).max(0.0)
        } else {
            0.0
        };
        
        let remaining_time_in_set = setlist.songs.iter()
            .skip(active_song_index.unwrap_or(0))
            .map(|song| song.duration())
            .sum::<f64>() - remaining_time_in_song;
        
        Self {
            name: setlist.name.clone(),
            songs,
            song_colors,
            song_durations,
            active_song_name,
            active_song_index,
            active_song_start,
            active_song_end,
            active_song_duration,
            active_song_color,
            active_song_progress,
            sections,
            section_colors,
            active_section_name,
            active_section_index,
            active_section_start,
            active_section_end,
            active_section_color,
            active_section_duration,
            active_section_progress,
            next_section_name,
            next_section_index,
            next_section_start,
            next_section_end,
            next_section_color,
            next_section_duration,
            next_song_name,
            next_song_index,
            next_song_duration,
            next_song_color,
            queued_name: (String::new(), String::new()),
            queued_index: (None, None),
            loop_enabled: false,
            loop_start: 0.0,
            loop_end: 0.0,
            is_counting_in: false,
            remaining_time_in_song,
            remaining_time_in_set,
        }
    }
}

