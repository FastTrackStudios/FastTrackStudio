use setlist::{Setlist, Song};
use std::collections::HashMap;
use crate::utils::get_project_name;

/// Update transport and song positions to a specific section
pub fn update_position_to_section(
    song: &Song,
    song_idx: usize,
    section_idx: usize,
    transport_positions: &mut HashMap<String, f64>,
    song_positions: &mut HashMap<String, f64>,
) {
    if let Some(section) = song.sections.get(section_idx) {
        let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
        let new_position = section.start_seconds();
        transport_positions.insert(project_name.clone(), new_position);
        song_positions.insert(song_idx.to_string(), new_position);
    }
}

/// Reset a song's position to its start
pub fn reset_song_position(
    song: &Song,
    song_idx: usize,
    song_positions: &mut HashMap<String, f64>,
) {
    let reset_position = if song.effective_start() > 0.0 {
        song.effective_start()
    } else {
        song.sections.first()
            .map(|s| s.start_seconds())
            .unwrap_or(0.0)
    };
    song_positions.insert(song_idx.to_string(), reset_position);
}

/// Navigate to the next section, wrapping to next song if needed
pub fn navigate_to_next_section(
    setlist: &Setlist,
    current_song_idx: Option<usize>,
    current_section_idx: Option<usize>,
    transport_positions: &mut HashMap<String, f64>,
    song_positions: &mut HashMap<String, f64>,
) -> (Option<usize>, Option<usize>) {
    let song_count = setlist.songs.len();
    
    if let Some(song_idx) = current_song_idx {
        if let Some(song) = setlist.songs.get(song_idx) {
            let section_count = song.sections.len();
            if section_count == 0 {
                return (current_song_idx, current_section_idx);
            }
            
            let current_sec = current_section_idx.unwrap_or(0);
            
            if current_sec < section_count - 1 {
                // Move to next section in current song
                let next_sec = current_sec + 1;
                update_position_to_section(song, song_idx, next_sec, transport_positions, song_positions);
                return (Some(song_idx), Some(next_sec));
            } else {
                // Last section of current song, move to first section of next song
                if song_idx < song_count - 1 {
                    reset_song_position(song, song_idx, song_positions);
                    
                    if let Some(next_song) = setlist.songs.get(song_idx + 1) {
                        if let Some(section) = next_song.sections.first() {
                            update_position_to_section(next_song, song_idx + 1, 0, transport_positions, song_positions);
                            return (Some(song_idx + 1), Some(0));
                        }
                    }
                }
                // If already at last song, stay at last section
                return (Some(song_idx), current_section_idx);
            }
        }
    }
    
    // No song selected, go to first section of first song
    if song_count > 0 {
        if let Some(song) = setlist.songs.first() {
            if let Some(_section) = song.sections.first() {
                update_position_to_section(song, 0, 0, transport_positions, song_positions);
                return (Some(0), Some(0));
            }
        }
    }
    
    (current_song_idx, current_section_idx)
}

/// Navigate to the previous section, wrapping to previous song if needed
pub fn navigate_to_previous_section(
    setlist: &Setlist,
    current_song_idx: Option<usize>,
    current_section_idx: Option<usize>,
    transport_positions: &mut HashMap<String, f64>,
    song_positions: &mut HashMap<String, f64>,
) -> (Option<usize>, Option<usize>) {
    let song_count = setlist.songs.len();
    
    if let Some(song_idx) = current_song_idx {
        if let Some(song) = setlist.songs.get(song_idx) {
            let section_count = song.sections.len();
            if section_count == 0 {
                return (current_song_idx, current_section_idx);
            }
            
            let current_sec = current_section_idx.unwrap_or(0);
            
            if current_sec > 0 {
                // Move to previous section in current song
                let prev_sec = current_sec - 1;
                update_position_to_section(song, song_idx, prev_sec, transport_positions, song_positions);
                return (Some(song_idx), Some(prev_sec));
            } else {
                // First section of current song, move to last section of previous song
                if song_idx > 0 {
                    reset_song_position(song, song_idx, song_positions);
                    
                    if let Some(prev_song) = setlist.songs.get(song_idx - 1) {
                        let prev_section_count = prev_song.sections.len();
                        if prev_section_count > 0 {
                            let last_section_idx = prev_section_count - 1;
                            update_position_to_section(prev_song, song_idx - 1, last_section_idx, transport_positions, song_positions);
                            return (Some(song_idx - 1), Some(last_section_idx));
                        }
                    }
                }
                // If already at first song, stay at first section
                return (Some(song_idx), Some(0));
            }
        }
    }
    
    // No song selected, go to last section of last song
    if song_count > 0 {
        if let Some(last_song) = setlist.songs.last() {
            let last_section_count = last_song.sections.len();
            if last_section_count > 0 {
                let last_section_idx = last_section_count - 1;
                update_position_to_section(last_song, song_count - 1, last_section_idx, transport_positions, song_positions);
                return (Some(song_count - 1), Some(last_section_idx));
            }
        }
    }
    
    (current_song_idx, current_section_idx)
}


/// Handle keyboard navigation for arrow keys
pub fn handle_keyboard_navigation(
    setlist: &Setlist,
    current_song_idx: Option<usize>,
    current_section_idx: Option<usize>,
    is_right: bool,
    transport_positions: &mut HashMap<String, f64>,
    song_positions: &mut HashMap<String, f64>,
) -> (Option<usize>, Option<usize>) {
    if is_right {
        navigate_to_next_section(setlist, current_song_idx, current_section_idx, transport_positions, song_positions)
    } else {
        navigate_to_previous_section(setlist, current_song_idx, current_section_idx, transport_positions, song_positions)
    }
}

