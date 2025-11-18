use crossterm::event::{MouseButton, MouseEvent, MouseEventKind};
use crate::app::CliSetlistApp;
use crate::handlers::transport as transport_handlers;
use transport::TransportActions;

pub fn handle_mouse(app: &mut CliSetlistApp, mouse: MouseEvent) {
    // Handle hover and clicks on song list
    if let Some(list_area) = app.song_list_area {
        if mouse.row >= list_area.y + 1
            && mouse.row < list_area.y + list_area.height - 1
            && mouse.column >= list_area.x + 1
            && mouse.column < list_area.x + list_area.width - 1
        {
            let relative_row = mouse.row - list_area.y - 1;
            let entry_index = (relative_row / 3) as usize;
            
            match mouse.kind {
                MouseEventKind::Moved => {
                    handle_song_list_hover(app, entry_index);
                }
                MouseEventKind::Down(MouseButton::Left) => {
                    handle_song_list_click(app, entry_index);
                    return;
                }
                _ => {}
            }
        } else {
            app.hovered_song_list_item = None;
        }
    }

    // Handle hover and clicks on progress bar
    if let Some(progress_area) = app.progress_bar_area {
        if mouse.row >= progress_area.y
            && mouse.row < progress_area.y + progress_area.height
            && mouse.column >= progress_area.x
            && mouse.column < progress_area.x + progress_area.width
        {
            match mouse.kind {
                MouseEventKind::Moved => {
                    handle_progress_bar_hover(app, mouse.column - progress_area.x, progress_area.width);
                }
                MouseEventKind::Down(MouseButton::Left) => {
                    handle_progress_bar_click(app, mouse.column - progress_area.x, progress_area.width);
                    return;
                }
                _ => {}
            }
        } else {
            app.hovered_progress_section = None;
        }
    }

    // Handle bottom controls
    if let Some(controls_area) = app.bottom_controls_area {
        if mouse.row >= controls_area.y 
            && mouse.row < controls_area.y + controls_area.height
            && mouse.column >= controls_area.x
            && mouse.column < controls_area.x + controls_area.width {
            
            let button_width = controls_area.width / 4;
            let relative_x = mouse.column - controls_area.x;
            let button_index = (relative_x / button_width).min(3);
            
            match mouse.kind {
                MouseEventKind::Moved => {
                    app.hovered_button = Some(button_index as usize);
                }
                MouseEventKind::Down(MouseButton::Left) => {
                    match button_index {
                        0 => transport_handlers::handle_back(app),
                        1 => transport_handlers::handle_play_pause(app),
                        2 => transport_handlers::handle_loop(app),
                        3 => transport_handlers::handle_advance(app),
                        _ => {}
                    }
                }
                _ => {}
            }
        } else {
            app.hovered_button = None;
        }
    }
}

fn handle_song_list_hover(app: &mut CliSetlistApp, entry_index: usize) {
    let mut display_items = Vec::new();
    
    for (song_idx, song) in app.setlist.songs.iter().enumerate() {
        display_items.push((song_idx, None));
        
        if app.expanded_songs.contains(&song_idx) {
            for section_idx in 0..song.sections.len() {
                display_items.push((song_idx, Some(section_idx)));
            }
        }
    }
    
    if let Some(&(song_idx, section_idx_opt)) = display_items.get(entry_index) {
        app.hovered_song_list_item = Some((song_idx, section_idx_opt));
    } else {
        app.hovered_song_list_item = None;
    }
}

fn handle_song_list_click(app: &mut CliSetlistApp, entry_index: usize) {
    let mut display_items = Vec::new();
    
    for (song_idx, song) in app.setlist.songs.iter().enumerate() {
        display_items.push((song_idx, None));
        
        if app.expanded_songs.contains(&song_idx) {
            for section_idx in 0..song.sections.len() {
                display_items.push((song_idx, Some(section_idx)));
            }
        }
    }
    
    if let Some(&(song_idx, section_idx_opt)) = display_items.get(entry_index) {
        app.selected_song_index = song_idx;
        app.selected_section_index = section_idx_opt;
        app.expanded_songs.insert(song_idx);
        
        if let Some(section_idx) = section_idx_opt {
            if !app.is_song_playing(song_idx) {
                app.jump_to_section_start();
            }
        } else {
            if app.is_song_stopped_or_paused(song_idx) {
                app.jump_to_song_start(song_idx);
            }
        }
    }
}

fn handle_progress_bar_hover(app: &mut CliSetlistApp, relative_x: u16, bar_width: u16) {
    if let Some(song) = app.selected_song() {
        if song.sections.is_empty() {
            app.hovered_progress_section = None;
            return;
        }
        
        let song_start = song.effective_start();
        let song_length = song.duration();
        let hover_ratio = (relative_x as f64) / (bar_width as f64);
        let hover_time = song_start + (hover_ratio * song_length);
        
        for (section_idx, section) in song.sections.iter().enumerate() {
            let section_start = section.start_position.time.to_seconds();
            let section_end = section.end_position.time.to_seconds();
            
            if hover_time >= section_start && hover_time <= section_end {
                app.hovered_progress_section = Some(section_idx);
                return;
            }
        }
    }
    app.hovered_progress_section = None;
}

fn handle_progress_bar_click(app: &mut CliSetlistApp, relative_x: u16, bar_width: u16) {
    if let Some(song) = app.selected_song() {
        if song.sections.is_empty() {
            return;
        }
        
        let song_start = song.effective_start();
        let song_length = song.duration();
        let click_ratio = (relative_x as f64) / (bar_width as f64);
        let click_time = song_start + (click_ratio * song_length);
        
        for (section_idx, section) in song.sections.iter().enumerate() {
            let section_start = section.start_position.time.to_seconds();
            let section_end = section.end_position.time.to_seconds();
            
            if click_time >= section_start && click_time <= section_end {
                app.selected_section_index = Some(section_idx);
                app.expanded_songs.insert(app.selected_song_index);
                
                if !app.is_song_playing(app.selected_song_index) {
                    app.jump_to_section_start();
                }
                break;
            }
        }
    }
}

