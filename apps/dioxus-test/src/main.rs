use dioxus::prelude::*;
use setlist::{Setlist, Song, Section, SectionType};
use crate::components::{TopBar, Sidebar, MainContent};
use crate::utils::get_project_name;
use std::collections::HashMap;

mod components;
mod utils;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    // Transport positions per project - each project has its own transport state
    // Key: project name (or "default" if no project), Value: transport position in seconds
    let mut transport_positions = use_signal(|| HashMap::<String, f64>::new());
    
    // Per-song positions - tracks the last known position for each song
    // This allows us to reset a song's progress bar when navigating away, even if songs share a project
    // Key: song index (as string), Value: position in seconds
    let mut song_positions = use_signal(|| HashMap::<String, f64>::new());
    
    // Setlist state - using a sample setlist with sections
    let setlist = use_signal(|| {
        let mut setlist = Setlist::new("Sample Setlist".to_string()).unwrap();
        let measure_duration = 2.0; // 2 seconds per measure at 120 BPM
        
        // Song 1: "Midnight Dreams" - Complex structure with bridge
        let mut song1 = Song::new("Midnight Dreams".to_string()).unwrap();
        song1.metadata.insert("project_name".to_string(), "Project A".to_string());
        let mut current_time = 0.0;
        song1.sections.push(Section::from_seconds(SectionType::Intro, current_time, current_time + 4.0 * measure_duration, "Intro".to_string(), None).unwrap());
        current_time += 4.0 * measure_duration;
        song1.sections.push(Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 1".to_string(), Some(1)).unwrap());
        current_time += 8.0 * measure_duration;
        song1.sections.push(Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 1".to_string(), Some(1)).unwrap());
        current_time += 8.0 * measure_duration;
        song1.sections.push(Section::from_seconds(SectionType::Verse, current_time, current_time + 4.0 * measure_duration, "Verse 2".to_string(), Some(2)).unwrap());
        current_time += 4.0 * measure_duration;
        song1.sections.push(Section::from_seconds(SectionType::Chorus, current_time, current_time + 6.0 * measure_duration, "Chorus 2".to_string(), Some(2)).unwrap());
        current_time += 6.0 * measure_duration;
        song1.sections.push(Section::from_seconds(SectionType::Bridge, current_time, current_time + 8.0 * measure_duration, "Bridge".to_string(), None).unwrap());
        current_time += 8.0 * measure_duration;
        song1.sections.push(Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 3".to_string(), Some(3)).unwrap());
        current_time += 8.0 * measure_duration;
        song1.sections.push(Section::from_seconds(SectionType::Outro, current_time, current_time + 8.0 * measure_duration, "Outro".to_string(), None).unwrap());
        setlist.add_song(song1).unwrap();
        
        // Song 2: "Electric Nights" - Simple verse-chorus structure (same project as Song 1)
        let mut song2 = Song::new("Electric Nights".to_string()).unwrap();
        song2.metadata.insert("project_name".to_string(), "Project A".to_string());
        current_time = 100.0; // Start at measure 50 (100 seconds) in the same project
        song2.sections.push(Section::from_seconds(SectionType::Intro, current_time, current_time + 2.0 * measure_duration, "Intro".to_string(), None).unwrap());
        current_time += 2.0 * measure_duration;
        song2.sections.push(Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 1".to_string(), Some(1)).unwrap());
        current_time += 8.0 * measure_duration;
        song2.sections.push(Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 1".to_string(), Some(1)).unwrap());
        current_time += 8.0 * measure_duration;
        song2.sections.push(Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 2".to_string(), Some(2)).unwrap());
        current_time += 8.0 * measure_duration;
        song2.sections.push(Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 2".to_string(), Some(2)).unwrap());
        current_time += 8.0 * measure_duration;
        song2.sections.push(Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 3".to_string(), Some(3)).unwrap());
        current_time += 8.0 * measure_duration;
        song2.sections.push(Section::from_seconds(SectionType::Outro, current_time, current_time + 4.0 * measure_duration, "Outro".to_string(), None).unwrap());
        setlist.add_song(song2).unwrap();
        
        // Song 3: "City Lights" - Verse-heavy with instrumental break (different project)
        let mut song3 = Song::new("City Lights".to_string()).unwrap();
        song3.metadata.insert("project_name".to_string(), "Project B".to_string());
        current_time = 0.0; // Different project, starts at 0
        song3.sections.push(Section::from_seconds(SectionType::Intro, current_time, current_time + 6.0 * measure_duration, "Intro".to_string(), None).unwrap());
        current_time += 6.0 * measure_duration;
        song3.sections.push(Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 1".to_string(), Some(1)).unwrap());
        current_time += 8.0 * measure_duration;
        song3.sections.push(Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 1".to_string(), Some(1)).unwrap());
        current_time += 8.0 * measure_duration;
        song3.sections.push(Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 2".to_string(), Some(2)).unwrap());
        current_time += 8.0 * measure_duration;
        song3.sections.push(Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 2".to_string(), Some(2)).unwrap());
        current_time += 8.0 * measure_duration;
        song3.sections.push(Section::from_seconds(SectionType::Instrumental, current_time, current_time + 12.0 * measure_duration, "Instrumental".to_string(), None).unwrap());
        current_time += 12.0 * measure_duration;
        song3.sections.push(Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 3".to_string(), Some(3)).unwrap());
        current_time += 8.0 * measure_duration;
        song3.sections.push(Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 3".to_string(), Some(3)).unwrap());
        current_time += 8.0 * measure_duration;
        song3.sections.push(Section::from_seconds(SectionType::Outro, current_time, current_time + 6.0 * measure_duration, "Outro".to_string(), None).unwrap());
        setlist.add_song(song3).unwrap();
        
        setlist
    });
    
    // Current active song index
    let mut current_song_index = use_signal(|| Some(0));
    // Current active section index (within the current song)
    let mut current_section_index = use_signal(|| Some(0));
    
    // Initialize transport positions for each project to the first section of their first song
    // Also initialize per-song positions
    use_effect(move || {
        let setlist_ref = setlist.read();
        let mut project_positions = HashMap::new();
        let mut song_pos = HashMap::new();
        
        for (idx, song) in setlist_ref.songs.iter().enumerate() {
            let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
            if !project_positions.contains_key(&project_name) {
                if let Some(section) = song.sections.first() {
                    project_positions.insert(project_name.clone(), section.start_seconds());
                } else {
                    project_positions.insert(project_name.clone(), 0.0);
                }
            }
            
            // Initialize song position to its start
            let song_start = if song.effective_start() > 0.0 {
                song.effective_start()
            } else {
                song.sections.first()
                    .map(|s| s.start_seconds())
                    .unwrap_or(0.0)
            };
            song_pos.insert(idx.to_string(), song_start);
        }
        
        transport_positions.set(project_positions);
        song_positions.set(song_pos);
    });
    
    
    // Transport control state
    let mut is_playing = use_signal(|| false);
    let mut is_looping = use_signal(|| false);
    
    rsx! {
        document::Link { rel: "icon", href: FAVICON }
        document::Link { rel: "stylesheet", href: MAIN_CSS } 
        document::Link { rel: "stylesheet", href: TAILWIND_CSS }
        
        div {
            class: "h-screen w-screen flex flex-col overflow-hidden bg-background",
            tabindex: "0",
            onkeydown: move |e| {
                let setlist_ref = setlist.read();
                let song_count = setlist_ref.songs.len();
                
                // Only handle section navigation when not playing
                if !is_playing() {
                    match e.key() {
                        Key::ArrowRight => {
                            // Navigate to next section, wrapping to next song if needed
                            if let Some(song_idx) = current_song_index() {
                                if let Some(song) = setlist_ref.songs.get(song_idx) {
                                    let section_count = song.sections.len();
                                    if section_count == 0 {
                                        return;
                                    }
                                    
                                    let current_sec = current_section_index().unwrap_or(0);
                                    
                                    if current_sec < section_count - 1 {
                                        // Move to next section in current song
                                        let next_sec = current_sec + 1;
                                        current_section_index.set(Some(next_sec));
                                        // Update transport position for this song's project
                                        if let Some(section) = song.sections.get(next_sec) {
                                            let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                                            let new_position = section.start_seconds();
                                            transport_positions.with_mut(|positions| {
                                                positions.insert(project_name.clone(), new_position);
                                            });
                                            // Also sync the song's position
                                            song_positions.with_mut(|positions| {
                                                positions.insert(song_idx.to_string(), new_position);
                                            });
                                        }
                                    } else {
                                        // Last section of current song, move to first section of next song
                                        if song_idx < song_count - 1 {
                                            current_song_index.set(Some(song_idx + 1));
                                            current_section_index.set(Some(0));
                                            // Update transport position for the next song's project
                                            if let Some(next_song) = setlist_ref.songs.get(song_idx + 1) {
                                                if let Some(section) = next_song.sections.first() {
                                                    let project_name = get_project_name(next_song).unwrap_or_else(|| "default".to_string());
                                                    let new_position = section.start_seconds();
                                                    transport_positions.with_mut(|positions| {
                                                        positions.insert(project_name.clone(), new_position);
                                                    });
                                                    // Also sync the song's position
                                                    song_positions.with_mut(|positions| {
                                                        positions.insert((song_idx + 1).to_string(), new_position);
                                                    });
                                                }
                                            }
                                        }
                                        // If already at last song, stay at last section
                                    }
                                }
                            } else {
                                // No song selected, go to first section of first song
                                if song_count > 0 {
                                    current_song_index.set(Some(0));
                                    current_section_index.set(Some(0));
                                    if let Some(song) = setlist_ref.songs.first() {
                                        if let Some(section) = song.sections.first() {
                                            let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                                            transport_positions.with_mut(|positions| {
                                                positions.insert(project_name, section.start_seconds());
                                            });
                                        }
                                    }
                                }
                            }
                        }
                        Key::ArrowLeft => {
                            // Navigate to previous section, wrapping to previous song if needed
                            if let Some(song_idx) = current_song_index() {
                                if let Some(song) = setlist_ref.songs.get(song_idx) {
                                    let section_count = song.sections.len();
                                    if section_count == 0 {
                                        return;
                                    }
                                    
                                    let current_sec = current_section_index().unwrap_or(0);
                                    
                                    if current_sec > 0 {
                                        // Move to previous section in current song
                                        let prev_sec = current_sec - 1;
                                        current_section_index.set(Some(prev_sec));
                                        // Update transport position for this song's project
                                        if let Some(section) = song.sections.get(prev_sec) {
                                            let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                                            let new_position = section.start_seconds();
                                            transport_positions.with_mut(|positions| {
                                                positions.insert(project_name.clone(), new_position);
                                            });
                                            // Also sync the song's position
                                            song_positions.with_mut(|positions| {
                                                positions.insert(song_idx.to_string(), new_position);
                                            });
                                        }
                                    } else {
                                        // First section of current song, move to last section of previous song
                                        if song_idx > 0 {
                                            // Reset current song's progress if not playing
                                            if !is_playing() {
                                                let reset_position = if song.effective_start() > 0.0 {
                                                    song.effective_start()
                                                } else {
                                                    song.sections.first()
                                                        .map(|s| s.start_seconds())
                                                        .unwrap_or(0.0)
                                                };
                                                song_positions.with_mut(|positions| {
                                                    positions.insert(song_idx.to_string(), reset_position);
                                                });
                                            }
                                            
                                            if let Some(prev_song) = setlist_ref.songs.get(song_idx - 1) {
                                                let prev_section_count = prev_song.sections.len();
                                                if prev_section_count > 0 {
                                                    current_song_index.set(Some(song_idx - 1));
                                                    current_section_index.set(Some(prev_section_count - 1));
                                                    // Update transport position for the previous song's project
                                                    if let Some(section) = prev_song.sections.get(prev_section_count - 1) {
                                                        let project_name = get_project_name(prev_song).unwrap_or_else(|| "default".to_string());
                                                        let new_position = section.start_seconds();
                                                        transport_positions.with_mut(|positions| {
                                                            positions.insert(project_name.clone(), new_position);
                                                        });
                                                        // Also sync the song's position
                                                        song_positions.with_mut(|positions| {
                                                            positions.insert((song_idx - 1).to_string(), new_position);
                                                        });
                                                    }
                                                }
                                            }
                                        }
                                        // If already at first song, stay at first section
                                    }
                                }
                            } else {
                                // No song selected, go to last section of last song
                                if song_count > 0 {
                                    if let Some(last_song) = setlist_ref.songs.last() {
                                        let last_section_count = last_song.sections.len();
                                        if last_section_count > 0 {
                                            current_song_index.set(Some(song_count - 1));
                                            current_section_index.set(Some(last_section_count - 1));
                                            // Update transport position for the last song's project
                                            if let Some(section) = last_song.sections.get(last_section_count - 1) {
                                                let project_name = get_project_name(last_song).unwrap_or_else(|| "default".to_string());
                                                let new_position = section.start_seconds();
                                                transport_positions.with_mut(|positions| {
                                                    positions.insert(project_name.clone(), new_position);
                                                });
                                                // Also sync the song's position
                                                song_positions.with_mut(|positions| {
                                                    positions.insert((song_count - 1).to_string(), new_position);
                                                });
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        _ => {}
    }
}

                // Song navigation (up/down arrows) - always available
                match e.key() {
                    Key::ArrowDown => {
                        if song_count == 0 {
                            return;
                        }
                        // Get previous song index before changing
                        let prev_song_idx = current_song_index();
                        
                        // Next song
                        let next_index = match current_song_index() {
                            Some(idx) if idx < song_count - 1 => Some(idx + 1),
                            Some(_) => Some(song_count - 1), // Stay at last song
                            None => Some(0),
                        };
                        
                        // Reset previous song's progress if not playing
                        // We need to store the reset position before setting the new song's position
                        // because if they're in the same project, the new position will overwrite
                        let prev_reset_position = if !is_playing() {
                            if let Some(prev_idx) = prev_song_idx {
                                if let Some(prev_song) = setlist_ref.songs.get(prev_idx) {
                                    // Calculate reset position (same logic as calculate_song_progress)
                                    Some(if prev_song.effective_start() > 0.0 {
                                        prev_song.effective_start()
                                    } else {
                                        prev_song.sections.first()
                                            .map(|s| s.start_seconds())
                                            .unwrap_or(0.0)
                                    })
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                        
                        current_song_index.set(next_index);
                        // Reset to first section when changing songs
                        current_section_index.set(Some(0));
                        // Update transport position for the new song's project
                        if let Some(song) = setlist_ref.songs.get(next_index.unwrap_or(0)) {
                            if let Some(section) = song.sections.first() {
                                let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                                let new_position = section.start_seconds();
                                transport_positions.with_mut(|positions| {
                                    positions.insert(project_name, new_position);
                                });
                                // Also update the song's position to keep it in sync
                                song_positions.with_mut(|positions| {
                                    positions.insert(next_index.unwrap_or(0).to_string(), new_position);
                                });
                            }
                        }
                    }
                    Key::ArrowUp => {
                        if song_count == 0 {
                            return;
                        }
                        // Get previous song index before changing
                        let prev_song_idx = current_song_index();
                        
                        // Previous song
                        let prev_index = match current_song_index() {
                            Some(idx) if idx > 0 => Some(idx - 1),
                            Some(_) => Some(0), // Stay at first song
                            None => Some(0),
                        };
                        
                        // Reset previous song's position if not playing
                        if !is_playing() {
                            if let Some(prev_idx) = prev_song_idx {
                                if let Some(prev_song) = setlist_ref.songs.get(prev_idx) {
                                    // Reset song position to its start
                                    let reset_position = if prev_song.effective_start() > 0.0 {
                                        prev_song.effective_start()
                                    } else {
                                        prev_song.sections.first()
                                            .map(|s| s.start_seconds())
                                            .unwrap_or(0.0)
                                    };
                                    song_positions.with_mut(|positions| {
                                        positions.insert(prev_idx.to_string(), reset_position);
                                    });
                                }
                            }
                        }
                        
                        current_song_index.set(prev_index);
                        // Reset to first section when changing songs
                        current_section_index.set(Some(0));
                        // Update transport position for the new song's project
                        if let Some(song) = setlist_ref.songs.get(prev_index.unwrap_or(0)) {
                            if let Some(section) = song.sections.first() {
                                let project_name = get_project_name(song).unwrap_or_else(|| "default".to_string());
                                let new_position = section.start_seconds();
                                transport_positions.with_mut(|positions| {
                                    positions.insert(project_name, new_position);
                                });
                                // Also update the song's position to keep it in sync
                                song_positions.with_mut(|positions| {
                                    positions.insert(prev_index.unwrap_or(0).to_string(), new_position);
                                });
                            }
                        }
                    }
                    Key::Character(c) if c == "c" || c == "C" => {
                        // Toggle loop state
                        is_looping.set(!is_looping());
                    }
                    Key::Character(c) if c == " " => {
                        // Toggle play/pause state (spacebar)
                        e.prevent_default();
                        is_playing.set(!is_playing());
                    }
                    _ => {}
                }
            },
            
            // Top Bar
            TopBar {}
            
            // Main Content Area (Sidebar + Content)
            div {
                class: "flex-1 flex overflow-hidden",
                
                // Left Sidebar (1/3 width)
                Sidebar {
                    setlist: setlist.read().clone(),
                    current_song_index,
                    current_section_index,
                    transport_positions,
                    song_positions,
                    is_playing,
                }
                
                // Main Content Area (2/3 width)
                MainContent {
                    setlist: setlist.read().clone(),
                    current_song_index: current_song_index(),
                    is_playing: is_playing,
                    is_looping: is_looping,
                    transport_positions,
                    song_positions,
                    current_section_index,
                }
            }
        }
    }
}
