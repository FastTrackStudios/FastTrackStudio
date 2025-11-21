use dioxus::prelude::*;
use setlist::{Setlist, Song, Section, SectionType};
use marker_region::{Marker, application::TempoTimePoint};
use primitives::Position;
use crate::components::{TopBar, Sidebar, MainContent};
use crate::utils::{get_project_name, navigation::handle_keyboard_navigation};
use std::collections::HashMap;

mod components;
mod utils;

const FAVICON: Asset = asset!("/assets/favicon.ico");
const MAIN_CSS: Asset = asset!("/assets/main.css");
const TAILWIND_CSS: Asset = asset!("/assets/tailwind.css");

fn main() {
    // Initialize tracing subscriber for logging (only for desktop builds)
    #[cfg(feature = "desktop")]
    {
        use tracing_subscriber::prelude::*;
        tracing_subscriber::registry()
            .with(
                tracing_subscriber::EnvFilter::try_from_default_env()
                    .unwrap_or_else(|_| "info".into())
            )
            .with(tracing_subscriber::fmt::layer())
            .init();
    }
    
;
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
        
        // Set song color (blue: rgb(59, 130, 246))
        let song1_color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
        let song1_start_marker = Marker::new_full(
            None,
            Position::from_seconds(0.0),
            "SONGSTART".to_string(),
            song1_color,
            None,
            None,
            None,
        );
        song1.set_start_marker(song1_start_marker);
        let song1_region_marker = Marker::new_full(
            None,
            Position::from_seconds(0.0),
            "Midnight Dreams_REGION_START".to_string(),
            song1_color,
            None,
            None,
            None,
        );
        song1.set_song_region_start_marker(song1_region_marker);
        
        let mut current_time = 0.0;
        // Intro - blue
        let mut intro = Section::from_seconds(SectionType::Intro, current_time, current_time + 4.0 * measure_duration, "Intro".to_string(), None).unwrap();
        intro.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
        song1.sections.push(intro);
        current_time += 4.0 * measure_duration;
        // Verse 1 - green
        let mut verse1 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 1".to_string(), Some(1)).unwrap();
        verse1.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
        song1.sections.push(verse1);
        current_time += 8.0 * measure_duration;
        // Chorus 1 - purple
        let mut chorus1 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 1".to_string(), Some(1)).unwrap();
        chorus1.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
        song1.sections.push(chorus1);
        current_time += 8.0 * measure_duration;
        // Verse 2 - green
        let mut verse2 = Section::from_seconds(SectionType::Verse, current_time, current_time + 4.0 * measure_duration, "Verse 2".to_string(), Some(2)).unwrap();
        verse2.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
        song1.sections.push(verse2);
        current_time += 4.0 * measure_duration;
        // Chorus 2 - purple
        let mut chorus2 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 6.0 * measure_duration, "Chorus 2".to_string(), Some(2)).unwrap();
        chorus2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
        song1.sections.push(chorus2);
        current_time += 6.0 * measure_duration;
        // Bridge - red
        let mut bridge = Section::from_seconds(SectionType::Bridge, current_time, current_time + 8.0 * measure_duration, "Bridge".to_string(), None).unwrap();
        bridge.color = Some((239u32 << 16) | (68u32 << 8) | 68u32);
        song1.sections.push(bridge);
        current_time += 8.0 * measure_duration;
        // Chorus 3 - purple
        let mut chorus3 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 3".to_string(), Some(3)).unwrap();
        chorus3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
        song1.sections.push(chorus3);
        current_time += 8.0 * measure_duration;
        // Outro - blue
        let mut outro = Section::from_seconds(SectionType::Outro, current_time, current_time + 8.0 * measure_duration, "Outro".to_string(), None).unwrap();
        outro.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
        song1.sections.push(outro);
        
        // Add tempo and time signature changes for Song 1
        // Start at 120 BPM, 4/4
        song1.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            0.0, // Start of song
            120.0,
            None,
            Some((4, 4)),
            None,
            None,
            None,
        ));
        // Change to 6/8 time signature for verse 2 (at measure 18, which is 36 seconds)
        song1.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            36.0, // Start of verse 2
            120.0,
            None,
            Some((6, 8)), // Change to 6/8
            None,
            None,
            None,
        ));
        // Slow down to 100 BPM during bridge (at measure 30, which is 60 seconds)
        song1.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            60.0, // Start of bridge
            100.0,
            None,
            Some((4, 4)), // Back to 4/4
            None,
            None,
            None,
        ));
        // Speed up to 140 BPM for final chorus (at measure 38, which is 76 seconds)
        song1.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            76.0, // Start of chorus 3
            140.0,
            None,
            None, // Keep 4/4
            None,
            None,
            None,
        ));
        
        setlist.add_song(song1).unwrap();
        
        // Song 2: "Electric Nights" - Simple verse-chorus structure (same project as Song 1)
        let mut song2 = Song::new("Electric Nights".to_string()).unwrap();
        song2.metadata.insert("project_name".to_string(), "Project A".to_string());
        
        // Set song color (green: rgb(34, 197, 94))
        let song2_color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
        let song2_start_marker = Marker::new_full(
            None,
            Position::from_seconds(100.0),
            "SONGSTART".to_string(),
            song2_color,
            None,
            None,
            None,
        );
        song2.set_start_marker(song2_start_marker);
        let song2_region_marker = Marker::new_full(
            None,
            Position::from_seconds(100.0),
            "Electric Nights_REGION_START".to_string(),
            song2_color,
            None,
            None,
            None,
        );
        song2.set_song_region_start_marker(song2_region_marker);
        
        current_time = 100.0; // Start at measure 50 (100 seconds) in the same project
        let mut intro2 = Section::from_seconds(SectionType::Intro, current_time, current_time + 2.0 * measure_duration, "Intro".to_string(), None).unwrap();
        intro2.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
        song2.sections.push(intro2);
        current_time += 2.0 * measure_duration;
        let mut verse1_2 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 1".to_string(), Some(1)).unwrap();
        verse1_2.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
        song2.sections.push(verse1_2);
        current_time += 8.0 * measure_duration;
        let mut chorus1_2 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 1".to_string(), Some(1)).unwrap();
        chorus1_2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
        song2.sections.push(chorus1_2);
        current_time += 8.0 * measure_duration;
        let mut verse2_2 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 2".to_string(), Some(2)).unwrap();
        verse2_2.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
        song2.sections.push(verse2_2);
        current_time += 8.0 * measure_duration;
        let mut chorus2_2 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 2".to_string(), Some(2)).unwrap();
        chorus2_2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
        song2.sections.push(chorus2_2);
        current_time += 8.0 * measure_duration;
        let mut chorus3_2 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 3".to_string(), Some(3)).unwrap();
        chorus3_2.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
        song2.sections.push(chorus3_2);
        current_time += 8.0 * measure_duration;
        let mut outro2 = Section::from_seconds(SectionType::Outro, current_time, current_time + 4.0 * measure_duration, "Outro".to_string(), None).unwrap();
        outro2.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
        song2.sections.push(outro2);
        
        // Add tempo and time signature changes for Song 2
        // Start at 120 BPM, 4/4 (song-relative position: 0.0)
        song2.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            0.0, // Start of song (relative to song start at 100.0)
            120.0,
            None,
            Some((4, 4)),
            None,
            None,
            None,
        ));
        // Speed up to 130 BPM for first chorus (at measure 4, which is 8 seconds song-relative)
        song2.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            8.0, // Start of chorus 1
            130.0,
            None,
            None, // Keep 4/4
            None,
            None,
            None,
        ));
        // Change to 3/4 time signature for verse 2 (at measure 12, which is 24 seconds song-relative)
        song2.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            24.0, // Start of verse 2
            130.0,
            None,
            Some((3, 4)), // Change to 3/4
            None,
            None,
            None,
        ));
        // Change to 5/4 time signature for chorus 2 (at measure 20, which is 40 seconds song-relative)
        song2.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            40.0, // Start of chorus 2
            130.0,
            None,
            Some((5, 4)), // Change to 5/4
            None,
            None,
            None,
        ));
        // Back to 4/4 for final chorus (at measure 28, which is 56 seconds song-relative)
        song2.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            56.0, // Start of chorus 3
            130.0,
            None,
            Some((4, 4)), // Back to 4/4
            None,
            None,
            None,
        ));
        
        setlist.add_song(song2).unwrap();
        
        // Song 3: "City Lights" - Verse-heavy with instrumental break (different project)
        let mut song3 = Song::new("City Lights".to_string()).unwrap();
        song3.metadata.insert("project_name".to_string(), "Project B".to_string());
        
        // Set song color (purple: rgb(168, 85, 247))
        let song3_color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
        let song3_start_marker = Marker::new_full(
            None,
            Position::from_seconds(0.0),
            "SONGSTART".to_string(),
            song3_color,
            None,
            None,
            None,
        );
        song3.set_start_marker(song3_start_marker);
        let song3_region_marker = Marker::new_full(
            None,
            Position::from_seconds(0.0),
            "City Lights_REGION_START".to_string(),
            song3_color,
            None,
            None,
            None,
        );
        song3.set_song_region_start_marker(song3_region_marker);
        
        current_time = 0.0; // Different project, starts at 0
        let mut intro3 = Section::from_seconds(SectionType::Intro, current_time, current_time + 6.0 * measure_duration, "Intro".to_string(), None).unwrap();
        intro3.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
        song3.sections.push(intro3);
        current_time += 6.0 * measure_duration;
        let mut verse1_3 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 1".to_string(), Some(1)).unwrap();
        verse1_3.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
        song3.sections.push(verse1_3);
        current_time += 8.0 * measure_duration;
        let mut chorus1_3 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 1".to_string(), Some(1)).unwrap();
        chorus1_3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
        song3.sections.push(chorus1_3);
        current_time += 8.0 * measure_duration;
        let mut verse2_3 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 2".to_string(), Some(2)).unwrap();
        verse2_3.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
        song3.sections.push(verse2_3);
        current_time += 8.0 * measure_duration;
        let mut chorus2_3 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 2".to_string(), Some(2)).unwrap();
        chorus2_3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
        song3.sections.push(chorus2_3);
        current_time += 8.0 * measure_duration;
        let mut instrumental = Section::from_seconds(SectionType::Instrumental, current_time, current_time + 12.0 * measure_duration, "Instrumental".to_string(), None).unwrap();
        instrumental.color = Some((251u32 << 16) | (191u32 << 8) | 36u32);
        song3.sections.push(instrumental);
        current_time += 12.0 * measure_duration;
        let mut verse3_3 = Section::from_seconds(SectionType::Verse, current_time, current_time + 8.0 * measure_duration, "Verse 3".to_string(), Some(3)).unwrap();
        verse3_3.color = Some((34u32 << 16) | (197u32 << 8) | 94u32);
        song3.sections.push(verse3_3);
        current_time += 8.0 * measure_duration;
        let mut chorus3_3 = Section::from_seconds(SectionType::Chorus, current_time, current_time + 8.0 * measure_duration, "Chorus 3".to_string(), Some(3)).unwrap();
        chorus3_3.color = Some((168u32 << 16) | (85u32 << 8) | 247u32);
        song3.sections.push(chorus3_3);
        current_time += 8.0 * measure_duration;
        let mut outro3 = Section::from_seconds(SectionType::Outro, current_time, current_time + 6.0 * measure_duration, "Outro".to_string(), None).unwrap();
        outro3.color = Some((59u32 << 16) | (130u32 << 8) | 246u32);
        song3.sections.push(outro3);
        
        // Add tempo and time signature changes for Song 3
        // Start at 120 BPM, 4/4
        song3.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            0.0, // Start of song
            120.0,
            None,
            Some((4, 4)),
            None,
            None,
            None,
        ));
        // Change to 3/4 time signature for verse 2 (at measure 14, which is 28 seconds)
        song3.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            28.0, // Start of verse 2
            120.0,
            None,
            Some((3, 4)), // Change to 3/4
            None,
            None,
            None,
        ));
        // Change to 7/8 time signature during instrumental (at measure 22, which is 44 seconds)
        song3.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            44.0, // Start of instrumental
            120.0,
            None,
            Some((7, 8)), // Change to 7/8
            None,
            None,
            None,
        ));
        // Speed up to 140 BPM and back to 4/4 for verse 3 (at measure 34, which is 68 seconds)
        song3.tempo_time_sig_changes.push(TempoTimePoint::new_full(
            68.0, // Start of verse 3
            140.0,
            None,
            Some((4, 4)), // Back to 4/4
            None,
            None,
            None,
        ));
        
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
                        Key::ArrowRight | Key::ArrowLeft => {
                            let is_right = matches!(e.key(), Key::ArrowRight);
                            let setlist_clone = setlist_ref.clone();
                            let (new_song_idx, new_section_idx) = transport_positions.with_mut(|transport_pos| {
                                song_positions.with_mut(|song_pos| {
                                    handle_keyboard_navigation(
                                        &setlist_clone,
                                        current_song_index(),
                                        current_section_index(),
                                        is_right,
                                        transport_pos,
                                        song_pos,
                                    )
                                })
                            });
                            current_song_index.set(new_song_idx);
                            current_section_index.set(new_section_idx);
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
                    current_song_index_signal: current_song_index,
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
