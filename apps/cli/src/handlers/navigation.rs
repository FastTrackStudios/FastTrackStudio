use crate::app::CliSetlistApp;

pub fn handle_up(app: &mut CliSetlistApp) {
    if app.selected_song_index > 0 {
        let prev_song_idx = app.selected_song_index;
        app.selected_song_index -= 1;
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

pub fn handle_down(app: &mut CliSetlistApp) {
    if app.selected_song_index < app.setlist.songs.len().saturating_sub(1) {
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

pub fn handle_right(app: &mut CliSetlistApp) {
    if let Some(song) = app.selected_song() {
        let num_sections = song.sections.len();
        if num_sections > 0 {
            let was_playing = app.is_song_playing(app.selected_song_index);
            let current_section = app.selected_section_index.unwrap_or(0);
            
            if current_section < num_sections - 1 {
                let next_section = current_section + 1;
                app.selected_section_index = Some(next_section);
                app.expanded_songs.insert(app.selected_song_index);
                
                if !was_playing {
                    if let Some(song) = app.selected_song() {
                        if let Some(section) = song.sections.get(next_section) {
                            if let Some(project) = &song.project {
                                let transport = project.transport();
                                let current_pos = transport.playhead_position.time.to_seconds();
                                let section_start = section.start_position.time.to_seconds();
                                
                                if (current_pos - section_start).abs() < 0.1 {
                                    app.jump_to_section_end();
                                } else {
                                    app.jump_to_section_start();
                                }
                            } else {
                                app.jump_to_section_start();
                            }
                        } else {
                            app.jump_to_section_start();
                        }
                    } else {
                        app.jump_to_section_start();
                    }
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
                } else if !was_playing {
                    app.jump_to_section_start();
                }
            }
        }
    }
}

pub fn handle_left(app: &mut CliSetlistApp) {
    if let Some(song) = app.selected_song() {
        let num_sections = song.sections.len();
        if num_sections > 0 {
            let was_playing = app.is_song_playing(app.selected_song_index);
            
            if let Some(current_section) = app.selected_section_index {
                if current_section > 0 {
                    let prev_section = current_section - 1;
                    app.selected_section_index = Some(prev_section);
                    app.expanded_songs.insert(app.selected_song_index);
                    
                    if !was_playing {
                        if let Some(song) = app.selected_song() {
                            if let Some(section) = song.sections.get(prev_section) {
                                if let Some(project) = &song.project {
                                    let transport = project.transport();
                                    let current_pos = transport.playhead_position.time.to_seconds();
                                    let section_start = section.start_position.time.to_seconds();
                                    
                                    if (current_pos - section_start).abs() < 0.1 {
                                        app.jump_to_section_end();
                                    } else {
                                        app.jump_to_section_start();
                                    }
                                } else {
                                    app.jump_to_section_start();
                                }
                            } else {
                                app.jump_to_section_start();
                            }
                        } else {
                            app.jump_to_section_start();
                        }
                    }
                } else {
                    app.selected_section_index = None;
                }
            } else if app.selected_song_index > 0 {
                let prev_song_idx = app.selected_song_index;
                app.selected_song_index -= 1;
                if let Some(prev_song) = app.selected_song() {
                    let prev_num_sections = prev_song.sections.len();
                    if prev_num_sections > 0 {
                        app.selected_section_index = Some(prev_num_sections - 1);
                        app.expanded_songs.insert(app.selected_song_index);
                        
                        if app.is_song_stopped_or_paused(prev_song_idx) {
                            app.jump_to_song_start(prev_song_idx);
                        }
                        
                        if app.is_song_stopped_or_paused(app.selected_song_index) {
                            app.jump_to_song_start(app.selected_song_index);
                        } else if !was_playing {
                            app.jump_to_section_start();
                        }
                    } else {
                        app.selected_section_index = None;
                        
                        if app.is_song_stopped_or_paused(prev_song_idx) {
                            app.jump_to_song_start(prev_song_idx);
                        }
                        
                        if app.is_song_stopped_or_paused(app.selected_song_index) {
                            app.jump_to_song_start(app.selected_song_index);
                        }
                    }
                }
            }
        }
    }
}

