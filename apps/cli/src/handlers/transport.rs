use crate::app::CliSetlistApp;
use transport::TransportActions;

pub fn handle_play_pause(app: &mut CliSetlistApp) {
    let section_idx = app.selected_section_index;
    let song_idx = app.selected_song_index;
    
    if let Some(song) = app.selected_song_mut() {
        let was_playing = TransportActions::is_playing(song).unwrap_or(false);
        
        if !was_playing {
            if section_idx.is_some() {
                drop(song);
                app.jump_to_section_start();
                
                if let Some(song) = app.selected_song_mut() {
                    match TransportActions::play(song) {
                        Ok(msg) => app.status_message = msg,
                        Err(e) => app.status_message = format!("Error: {}", e),
                    }
                }
            } else {
                match TransportActions::play(song) {
                    Ok(msg) => app.status_message = msg,
                    Err(e) => app.status_message = format!("Error: {}", e),
                }
            }
        } else {
            match TransportActions::pause(song) {
                Ok(msg) => app.status_message = msg,
                Err(e) => app.status_message = format!("Error: {}", e),
            }
        }
    }
}

pub fn handle_back(app: &mut CliSetlistApp) {
    if let Some(song) = app.selected_song_mut() {
        match TransportActions::set_position(song, 0.0) {
            Ok(_) => app.status_message = format!("Jumped to start of {}", song.name),
            Err(e) => app.status_message = format!("Error: {}", e),
        }
    }
}

pub fn handle_loop(app: &mut CliSetlistApp) {
    if let Some(song) = app.selected_song_mut() {
        if let Some(project) = &mut song.project {
            let transport = project.transport_mut();
            transport.looping = !transport.looping;
            app.status_message = if transport.looping {
                "Looping enabled".to_string()
            } else {
                "Looping disabled".to_string()
            };
        }
    }
}

pub fn handle_advance(app: &mut CliSetlistApp) {
    if let Some(song) = app.selected_song() {
        let num_sections = song.sections.len();
        if num_sections > 0 {
            let current_section = app.selected_section_index.unwrap_or(0);
            
            if current_section < num_sections - 1 {
                let next_section = current_section + 1;
                app.selected_section_index = Some(next_section);
                app.expanded_songs.insert(app.selected_song_index);
                
                if !app.is_song_playing(app.selected_song_index) {
                    app.jump_to_section_start();
                }
            } else if app.selected_song_index < app.setlist.songs.len().saturating_sub(1) {
                let prev_song_idx = app.selected_song_index;
                app.selected_song_index += 1;
                app.selected_section_index = Some(0);
                app.expanded_songs.insert(app.selected_song_index);
                
                if app.is_song_stopped_or_paused(prev_song_idx) {
                    app.jump_to_song_start(prev_song_idx);
                }
                
                if app.is_song_stopped_or_paused(app.selected_song_index) {
                    app.jump_to_song_start(app.selected_song_index);
                } else if !app.is_song_playing(app.selected_song_index) {
                    app.jump_to_section_start();
                }
            }
        } else if app.selected_song_index < app.setlist.songs.len().saturating_sub(1) {
            let prev_song_idx = app.selected_song_index;
            app.selected_song_index += 1;
            app.selected_section_index = None;
            app.expanded_songs.insert(app.selected_song_index);
            
            if app.is_song_stopped_or_paused(prev_song_idx) {
                app.jump_to_song_start(prev_song_idx);
            }
            
            if app.is_song_stopped_or_paused(app.selected_song_index) {
                app.jump_to_song_start(app.selected_song_index);
            }
        }
    }
}

