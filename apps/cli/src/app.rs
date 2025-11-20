use std::{
    collections::HashSet,
    time::Instant,
};

use crossterm::event::{KeyCode, KeyEvent, MouseEvent};
use ratatui::layout::Rect;
use project::TransportProject;
use ratatui::Frame;
use setlist::{Setlist, Song};
use transport::{PlayState, Transport, TransportActions};

use crate::handlers::{mouse, navigation, transport as transport_handlers};

pub struct CliSetlistApp {
    pub setlist: Setlist,
    pub selected_song_index: usize,
    pub selected_section_index: Option<usize>,
    pub should_exit: bool,
    pub last_tick: Instant,
    pub status_message: String,
    pub expanded_songs: HashSet<usize>,
    pub hovered_button: Option<usize>, // 0=Back, 1=Play/Pause, 2=Loop, 3=Advance
    pub hovered_song_list_item: Option<(usize, Option<usize>)>, // (song_idx, section_idx_opt)
    pub hovered_progress_section: Option<usize>, // section_idx
    pub bottom_controls_area: Option<Rect>, // Store the area for mouse hit testing
    pub song_list_area: Option<Rect>, // Store the area for song list mouse hit testing
    pub progress_bar_area: Option<Rect>, // Store the area for progress bar mouse hit testing
}

impl CliSetlistApp {
    pub fn new() -> Self {
        // Create a sample setlist with projects attached
        let mut setlist = Setlist::sample_concert_setlist()
            .unwrap_or_else(|_| Setlist::default_app_setlist().unwrap());

        // Attach projects to songs
        for song in &mut setlist.songs {
            if song.project.is_none() {
                let transport = Transport::new();
                let project = TransportProject::new(song.name.clone(), transport);
                song.project = Some(project);
            }
        }

        let mut expanded_songs = HashSet::new();
        expanded_songs.insert(0); // Expand first song by default

        Self {
            setlist,
            selected_song_index: 0,
            selected_section_index: None,
            should_exit: false,
            last_tick: Instant::now(),
            status_message: "Setlist ready".to_string(),
            expanded_songs,
            hovered_button: None,
            hovered_song_list_item: None,
            hovered_progress_section: None,
            bottom_controls_area: None,
            song_list_area: None,
            progress_bar_area: None,
        }
    }

    pub fn selected_song(&self) -> Option<&Song> {
        self.setlist.songs.get(self.selected_song_index)
    }

    pub fn selected_song_mut(&mut self) -> Option<&mut Song> {
        self.setlist.songs.get_mut(self.selected_song_index)
    }

    pub fn is_song_playing(&self, song_idx: usize) -> bool {
        self.setlist
            .songs
            .get(song_idx)
            .and_then(|song| song.project.as_ref())
            .map(|project| project.transport().is_playing())
            .unwrap_or(false)
    }

    pub fn is_song_stopped_or_paused(&self, song_idx: usize) -> bool {
        self.setlist
            .songs
            .get(song_idx)
            .and_then(|song| song.project.as_ref())
            .map(|project| {
                let transport = project.transport();
                matches!(transport.play_state, PlayState::Stopped | PlayState::Paused)
            })
            .unwrap_or(true) // If no project, consider it stopped
    }

    pub fn jump_to_song_start(&mut self, song_idx: usize) {
        if let Some(song) = self.setlist.songs.get_mut(song_idx) {
            match TransportActions::set_position(song, 0.0) {
                Ok(_) => {
                    self.status_message = format!(
                        "Jumped to start of {}",
                        song.name
                    );
                }
                Err(e) => {
                    self.status_message = format!("Error: {}", e);
                }
            }
        }
    }

    pub fn jump_to_section_start(&mut self) {
        let section_idx = self.selected_section_index;
        if let Some(section_idx) = section_idx {
            // Get immutable data first
            let target_pos_opt = if let Some(song) = self.selected_song() {
                if let Some(section) = song.sections.get(section_idx) {
                    // Use musical position from start_position to calculate target time position
                    let musical_pos = &section.start_position.musical;
                    if let Some(project) = &song.project {
                        let transport = project.transport();
                        let bpm = transport.tempo.bpm;
                        let time_sig = transport.time_signature;
                        
                        // Convert musical position to time position
                        let target_time = musical_pos.clone().to_time_position(bpm, time_sig);
                        let target_pos = target_time.to_seconds();
                        Some((target_pos, section.display_name()))
                    } else {
                        // Fallback to time position if project not available
                        let target_pos = section.start_position.time.to_seconds();
                        Some((target_pos, section.display_name()))
                    }
                } else {
                    None
                }
            } else {
                None
            };
            
            // Now do the mutable operation
            if let Some((target_pos, section_name)) = target_pos_opt {
                if let Some(song) = self.selected_song_mut() {
                    match TransportActions::set_position(song, target_pos) {
                        Ok(_) => {
                            self.status_message = format!("Jumped to {}", section_name);
                        }
                        Err(e) => {
                            self.status_message = format!("Error: {}", e);
                        }
                    }
                }
            }
        }
    }

    pub fn jump_to_section_end(&mut self) {
        let section_idx = self.selected_section_index;
        if let Some(section_idx) = section_idx {
            // Get immutable data first
            let target_pos_opt = if let Some(song) = self.selected_song() {
                if let Some(section) = song.sections.get(section_idx) {
                    // Use end position
                    let target_pos = section.end_position.time.to_seconds();
                    Some((target_pos, section.display_name()))
                } else {
                    None
                }
            } else {
                None
            };
            
            // Now do the mutable operation
            if let Some((target_pos, section_name)) = target_pos_opt {
                if let Some(song) = self.selected_song_mut() {
                    match TransportActions::set_position(song, target_pos) {
                        Ok(_) => {
                            self.status_message = format!("Jumped to end of {}", section_name);
                        }
                        Err(e) => {
                            self.status_message = format!("Error: {}", e);
                        }
                    }
                }
            }
        }
    }

    pub fn draw(&mut self, frame: &mut Frame) {
        crate::ui::draw_ui(frame, self);
    }

    pub fn handle_key(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Char('q') | KeyCode::Esc => {
                self.should_exit = true;
            }
            KeyCode::Up | KeyCode::Char('k') => {
                navigation::handle_up(self);
            }
            KeyCode::Down | KeyCode::Char('j') => {
                navigation::handle_down(self);
            }
            KeyCode::Right | KeyCode::Char('l') => {
                navigation::handle_right(self);
            }
            KeyCode::Left | KeyCode::Char('h') => {
                navigation::handle_left(self);
            }
            KeyCode::Char(' ') => {
                transport_handlers::handle_play_pause(self);
            }
            KeyCode::Char('b') | KeyCode::Home => {
                transport_handlers::handle_back(self);
            }
            KeyCode::Char('L') | KeyCode::Char('r') => {
                transport_handlers::handle_loop(self);
            }
            KeyCode::Char('n') => {
                transport_handlers::handle_advance(self);
            }
            _ => {}
        }
    }

    pub fn on_tick(&mut self) {
        use std::time::Duration;
        
        // Calculate elapsed time before any mutable borrows
        let elapsed = self.last_tick.elapsed();
        
        // Update expanded songs to keep only selected and playing songs expanded
        let mut new_expanded = HashSet::new();
        new_expanded.insert(self.selected_song_index);
        
        for (idx, song) in self.setlist.songs.iter().enumerate() {
            if self.is_song_playing(idx) {
                new_expanded.insert(idx);
            }
        }
        
        self.expanded_songs = new_expanded;
        
        // Advance playhead for playing songs
        if let Some(song) = self.selected_song_mut() {
            match TransportActions::is_playing(song) {
                Ok(true) => {
                    if let Some(project) = &song.project {
                        let transport = project.transport();
                        let current = transport.playhead_position.time.to_seconds();
                        let length = song.duration();
                        let playrate = transport.playrate;
                        let next = (current + elapsed.as_secs_f64() * playrate).min(length);

                        if (next - length).abs() < f64::EPSILON {
                            let _ = TransportActions::stop(song);
                            self.status_message = "Reached end of song".to_string();
                        } else {
                            let _ = TransportActions::set_position(song, next);
                        }
                    }
                }
                _ => {}
            }
        }
        
        self.last_tick = Instant::now();
    }

    pub fn handle_mouse(&mut self, mouse: MouseEvent) {
        mouse::handle_mouse(self, mouse);
    }
}

