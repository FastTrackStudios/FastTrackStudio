use fts::fts::setlist::{Setlist, Song};
use std::collections::HashMap;
use crate::utils::{get_project_name, calculate_song_progress, calculate_section_progress};

/// Calculate song progress for sidebar display
pub fn get_song_progress_for_sidebar(
    song: &Song,
    song_idx: usize,
    current_song_idx: Option<usize>,
    transport_positions: &HashMap<String, f64>,
) -> f64 {
    if current_song_idx == Some(song_idx) {
        let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
        let position = transport_positions.get(&project_name).copied().unwrap_or(0.0);
        calculate_song_progress(song, position)
    } else {
        0.0 // No progress for inactive songs
    }
}

/// Calculate section progress for sidebar display
pub fn get_section_progress_for_sidebar(
    section: &fts::setlist::Section,
    song: &Song,
    song_idx: usize,
    current_song_idx: Option<usize>,
    transport_positions: &HashMap<String, f64>,
) -> f64 {
    if current_song_idx == Some(song_idx) {
        let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
        let position = transport_positions.get(&project_name).copied().unwrap_or(0.0);
        calculate_section_progress(section, position)
    } else {
        0.0 // No progress for inactive songs
    }
}

/// Reset previous song's position when selecting a new song
pub fn reset_previous_song_position(
    setlist: &Setlist,
    prev_idx: Option<usize>,
    transport_positions: &mut HashMap<String, f64>,
) {
    if let Some(prev_idx) = prev_idx {
        if let Some(prev_song) = setlist.songs.get(prev_idx) {
            let project_name = get_project_name(prev_song).unwrap_or_else(|| "default".to_string());
            let reset_position = if prev_song.effective_start() > 0.0 {
                prev_song.effective_start()
            } else {
                prev_song.sections.first()
                    .map(|s| s.start_seconds())
                    .unwrap_or(0.0)
            };
            transport_positions.insert(project_name, reset_position);
        }
    }
}

/// Update transport position when clicking a section
pub fn update_position_on_section_click(
    song: &Song,
    song_idx: usize,
    section_idx: usize,
    transport_positions: &mut HashMap<String, f64>,
    song_positions: &mut HashMap<String, f64>,
) -> Option<f64> {
    if let Some(section) = song.sections.get(section_idx) {
        let section_start = section.start_seconds();
        let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
        transport_positions.insert(project_name.clone(), section_start);
        song_positions.insert(song_idx.to_string(), section_start);
        Some(section_start)
    } else {
        None
    }
}

