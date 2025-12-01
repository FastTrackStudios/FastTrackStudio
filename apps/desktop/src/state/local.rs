//! Local state manager operations
//!
//! These functions provide local state management operations.
//! They directly update signals, which is appropriate for local mode.

use dioxus::prelude::*;
use fts::fts::setlist::Setlist;
use std::collections::HashMap;
use crate::utils::{get_project_name, navigation::handle_keyboard_navigation};
use crate::utils::sidebar_helpers::update_position_on_section_click;

/// Navigate to next section (local mode)
pub fn navigate_to_next_section_local(
    setlist: Signal<Setlist>,
    mut transport_positions: Signal<HashMap<String, f64>>,
    mut song_positions: Signal<HashMap<String, f64>>,
    mut current_song_index: Signal<Option<usize>>,
    mut current_section_index: Signal<Option<usize>>,
) {
    let setlist_ref = setlist.read();
    let (new_song_idx, new_section_idx) = transport_positions.with_mut(|transport_pos| {
        song_positions.with_mut(|song_pos| {
            handle_keyboard_navigation(
                &setlist_ref,
                current_song_index(),
                current_section_index(),
                true, // is_right
                transport_pos,
                song_pos,
            )
        })
    });
    current_song_index.set(new_song_idx);
    current_section_index.set(new_section_idx);
}

/// Navigate to previous section (local mode)
pub fn navigate_to_previous_section_local(
    setlist: Signal<Setlist>,
    mut transport_positions: Signal<HashMap<String, f64>>,
    mut song_positions: Signal<HashMap<String, f64>>,
    mut current_song_index: Signal<Option<usize>>,
    mut current_section_index: Signal<Option<usize>>,
) {
    let setlist_ref = setlist.read();
    let (new_song_idx, new_section_idx) = transport_positions.with_mut(|transport_pos| {
        song_positions.with_mut(|song_pos| {
            handle_keyboard_navigation(
                &setlist_ref,
                current_song_index(),
                current_section_index(),
                false, // is_right
                transport_pos,
                song_pos,
            )
        })
    });
    current_song_index.set(new_song_idx);
    current_section_index.set(new_section_idx);
}

/// Navigate to next song (local mode)
pub fn navigate_to_next_song_local(
    setlist: Signal<Setlist>,
    mut transport_positions: Signal<HashMap<String, f64>>,
    mut song_positions: Signal<HashMap<String, f64>>,
    mut current_song_index: Signal<Option<usize>>,
    mut current_section_index: Signal<Option<usize>>,
) {
    let setlist_ref = setlist.read();
    let song_count = setlist_ref.songs.len();
    
    if song_count == 0 {
        return;
    }
    
    let next_index = match current_song_index() {
        Some(idx) if idx < song_count - 1 => Some(idx + 1),
        Some(_) => Some(song_count - 1),
        None => Some(0),
    };
    
    current_song_index.set(next_index);
    current_section_index.set(Some(0));
    
    // Update transport position for the new song's project
    if let Some(song) = setlist_ref.songs.get(next_index.unwrap_or(0)) {
        if let Some(section) = song.sections.first() {
            let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
            let new_position = section.start_seconds();
            transport_positions.with_mut(|positions| {
                positions.insert(project_name.clone(), new_position);
            });
            song_positions.with_mut(|positions| {
                positions.insert(next_index.unwrap_or(0).to_string(), new_position);
            });
        }
    }
}

/// Navigate to previous song (local mode)
pub fn navigate_to_previous_song_local(
    setlist: Signal<Setlist>,
    mut transport_positions: Signal<HashMap<String, f64>>,
    mut song_positions: Signal<HashMap<String, f64>>,
    mut current_song_index: Signal<Option<usize>>,
    mut current_section_index: Signal<Option<usize>>,
) {
    let setlist_ref = setlist.read();
    let song_count = setlist_ref.songs.len();
    
    if song_count == 0 {
        return;
    }
    
    let prev_index = match current_song_index() {
        Some(idx) if idx > 0 => Some(idx - 1),
        Some(_) => Some(0),
        None => Some(0),
    };
    
    current_song_index.set(prev_index);
    current_section_index.set(Some(0));
    
    // Update transport position for the new song's project
    if let Some(song) = setlist_ref.songs.get(prev_index.unwrap_or(0)) {
        if let Some(section) = song.sections.first() {
            let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
            let new_position = section.start_seconds();
            transport_positions.with_mut(|positions| {
                positions.insert(project_name.clone(), new_position);
            });
            song_positions.with_mut(|positions| {
                positions.insert(prev_index.unwrap_or(0).to_string(), new_position);
            });
        }
    }
}

/// Seek to a specific section (local mode)
pub fn seek_to_section_local(
    setlist: Signal<Setlist>,
    mut transport_positions: Signal<HashMap<String, f64>>,
    mut song_positions: Signal<HashMap<String, f64>>,
    mut current_song_index: Signal<Option<usize>>,
    mut current_section_index: Signal<Option<usize>>,
    song_idx: usize,
    section_idx: usize,
) {
    let setlist_ref = setlist.read();
    if let Some(song) = setlist_ref.songs.get(song_idx) {
        transport_positions.with_mut(|transport_pos| {
            song_positions.with_mut(|song_pos| {
                update_position_on_section_click(
                    song,
                    song_idx,
                    section_idx,
                    transport_pos,
                    song_pos,
                );
            });
        });
        current_song_index.set(Some(song_idx));
        current_section_index.set(Some(section_idx));
    }
}

