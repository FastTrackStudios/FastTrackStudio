//! Functions for reading lyrics data from REAPER

use reaper_high::{Reaper, Project, Track};
use reaper_medium::MediaItemTake;
use tracing::warn;

/// Find a track by name in the project
pub fn find_track_by_name(project: Project, name: &str) -> Option<Track> {
    for track in project.tracks() {
        if let Some(track_name) = track.name() {
            if track_name.to_str().eq_ignore_ascii_case(name) {
                return Some(track);
            }
        }
    }
    None
}

/// Find a folder track by name and return all tracks within that folder
pub fn find_folder_tracks(project: Project, folder_name: &str) -> Option<Vec<Track>> {
    let folder_track = find_track_by_name(project, folder_name)?;
    
    // Check if it's a folder track (folder_depth_change == 1)
    let folder_depth_change = folder_track.folder_depth_change();
    if folder_depth_change != 1 {
        warn!(folder_name = folder_name, "Track found but is not a folder track");
        return None;
    }
    
    // Get the folder track's raw pointer and index before moving
    let folder_track_raw = folder_track.raw();
    let folder_index = {
        let mut idx = 0u32;
        for (i, track) in project.tracks().enumerate() {
            if track.raw() == folder_track_raw {
                idx = i as u32;
                break;
            }
        }
        idx
    };
    
    // Collect all tracks within the folder
    let mut folder_tracks = vec![folder_track];
    let mut folder_depth = 1i32;
    
    // Iterate through tracks after the folder track
    for i in (folder_index + 1)..project.track_count() {
        if let Some(track) = project.track_by_index(i) {
            folder_depth += track.folder_depth_change();
            
            // If we've exited the folder (depth <= 0), stop
            if folder_depth <= 0 {
                break;
            }
            
            // Add track to folder list
            folder_tracks.push(track);
        }
    }
    
    Some(folder_tracks)
}

/// Read lyrics from a specific REAPER project
/// 
/// Looks for:
/// - A folder track named "LYRICS"
/// - A track named "SLIDES" within that folder (contains text items with slide text)
/// - Other tracks in the LYRICS folder with MIDI items (contains syllable timing and MIDI notes)
pub fn read_lyrics_from_project(project: Project) -> Result<LyricsData, LyricsReadError> {
    let reaper = Reaper::get();
    
    // Find LYRICS folder
    let lyrics_folder_tracks = find_folder_tracks(project.clone(), "LYRICS")
        .ok_or(LyricsReadError::LyricsFolderNotFound)?;
    
    // Find SLIDES track
    let slides_track = lyrics_folder_tracks
        .iter()
        .find(|track| {
            track.name()
                .map(|n| n.to_str().eq_ignore_ascii_case("SLIDES"))
                .unwrap_or(false)
        })
        .ok_or(LyricsReadError::SlidesTrackNotFound)?;
    
    // Read text items from SLIDES track
    let mut slides = Vec::new();
    for item in slides_track.items() {
        // Get item notes (contains slide text)
        let notes = unsafe {
            let medium_reaper = reaper.medium_reaper();
            let notes_key = std::ffi::CString::new("P_NOTES").expect("CString::new failed");
            let mut buffer = vec![0u8; 4096];
            let buffer_ptr = buffer.as_mut_ptr() as *mut std::os::raw::c_char;
            
            let item_raw = item.raw();
            let success = medium_reaper.low().GetSetMediaItemInfo_String(
                item_raw.as_ptr(),
                notes_key.as_ptr(),
                buffer_ptr,
                false, // setNewValue = false (get value)
            );
            
            if success {
                let c_str = std::ffi::CStr::from_ptr(buffer_ptr);
                c_str.to_string_lossy().to_string()
            } else {
                String::new()
            }
        };
        
        if !notes.trim().is_empty() {
            let position = item.position().get();
            let length = item.length().get();
            
            slides.push(SlideData {
                text: notes,
                position,
                length,
            });
        }
    }
    
    // Find other tracks in LYRICS folder with MIDI items
    let mut midi_tracks = Vec::new();
    for track in lyrics_folder_tracks {
        // Skip the SLIDES track
        if track.name()
            .map(|n| n.to_str().eq_ignore_ascii_case("SLIDES"))
            .unwrap_or(false)
        {
            continue;
        }
        
        // Check if track has MIDI items
        let mut has_midi = false;
        let mut midi_items = Vec::new();
        
        for item in track.items() {
            // Get active take
            let item_raw = item.raw();
            let reaper = Reaper::get();
            let take = unsafe {
                let medium_reaper = reaper.medium_reaper();
                match medium_reaper.get_active_take(item_raw) {
                    Some(take) => take,
                    None => continue,
                }
            };
            
            // Check if take is MIDI
            let is_midi = unsafe {
                let medium_reaper = reaper.medium_reaper();
                medium_reaper.low().TakeIsMIDI(take.as_ptr())
            };
            
            if is_midi {
                has_midi = true;
                
                // Get MIDI events using MIDI_GetAllEvts
                let midi_events = get_midi_events_from_take(take)?;
                
                midi_items.push(MidiItemData {
                    position: item.position().get(),
                    length: item.length().get(),
                    events: midi_events,
                });
            }
        }
        
        if has_midi {
            let track_name = track.name()
                .map(|n| n.to_str().to_string())
                .unwrap_or_else(|| "Unknown".to_string());
            
            midi_tracks.push(MidiTrackData {
                name: track_name.clone(),
                items: midi_items,
            });
        }
    }
    
    Ok(LyricsData {
        slides,
        midi_tracks,
    })
}

/// Read lyrics from REAPER project (uses current project)
/// 
/// Looks for:
/// - A folder track named "LYRICS"
/// - A track named "SLIDES" within that folder (contains text items with slide text)
/// - Other tracks in the LYRICS folder with MIDI items (contains syllable timing and MIDI notes)
pub fn read_lyrics_from_reaper() -> Result<LyricsData, LyricsReadError> {
    let reaper = Reaper::get();
    let project = reaper.current_project();
    read_lyrics_from_project(project)
}

/// Convert LyricsData from REAPER into a lyrics::Lyrics struct
/// Each slide is already a line, and we assign it to the appropriate song section based on position
pub fn convert_lyrics_data_to_lyrics(
    lyrics_data: LyricsData,
    song_name: String,
    song: &setlist::core::Song,
) -> Result<lyrics::Lyrics, String> {
    use fts::lyrics::core::{Lyrics, LyricSection, LyricLine};
    
    let mut lyrics = Lyrics::new(song_name);
    
    // Sort slides by position
    let mut slides = lyrics_data.slides.clone();
    slides.sort_by(|a, b| a.position.partial_cmp(&b.position).unwrap_or(std::cmp::Ordering::Equal));
    
    // Create a map of section name -> LyricSection for easy lookup
    let mut section_map: std::collections::HashMap<String, LyricSection> = std::collections::HashMap::new();
    
    // Process each slide
    for slide in &slides {
        // Find which song section this slide belongs to based on position
        let section_name = find_section_for_position(slide.position, song);
        
        // Get or create the lyric section
        let section = section_map.entry(section_name.clone())
            .or_insert_with(|| {
                let mut new_section = LyricSection::new(section_name.clone());
                // Set color from the matching song section
                if let Some(song_section) = find_song_section_by_name(&section_name, song) {
                    if let Some(color) = song_section.color {
                        new_section.set_metadata("color", color.to_string());
                    }
                }
                new_section
            });
        
        // Create a lyric line from the slide text
        let mut line = LyricLine::new(slide.text.clone());
        
        // Set timing information from slide position and length
        // Position is absolute project time, but we need song-relative time
        // For now, use absolute position (will be adjusted if needed)
        line.start_time = Some(slide.position);
        line.end_time = Some(slide.position + slide.length);
        
        // Add the line to the section
        section.add_line(line);
    }
    
    // Ensure all song sections are represented, even if they have no slides
    // Add empty lines for sections that don't have any lyrics (like instrumentals)
    for song_section in &song.sections {
        let section_name = song_section.name.clone();
        
        // If this section doesn't have any lyrics yet, add an empty line
        if !section_map.contains_key(&section_name) {
            let mut empty_section = LyricSection::new(section_name.clone());
            // Set color from the song section
            if let Some(color) = song_section.color {
                empty_section.set_metadata("color", color.to_string());
            }
            // Add an empty line to represent this section
            empty_section.add_line(LyricLine::new(String::new()));
            section_map.insert(section_name, empty_section);
        }
    }
    
    // Convert section map to vector, sorted by song section order
    // We'll sort by finding the section's position in the song
    let mut sections: Vec<LyricSection> = section_map.into_values().collect();
    
    // Sort sections by their position in the song (using section start time)
    sections.sort_by(|a, b| {
        let a_pos = find_section_start_position(a, song);
        let b_pos = find_section_start_position(b, song);
        a_pos.partial_cmp(&b_pos).unwrap_or(std::cmp::Ordering::Equal)
    });
    
    // Add all sections to lyrics
    for section in sections {
        lyrics.add_section(section);
    }
    
    Ok(lyrics)
}

/// Find which song section a position (in seconds) belongs to
fn find_section_for_position(position_seconds: f64, song: &setlist::core::Song) -> String {
    // Find the section that contains this position
    for section in &song.sections {
        let section_start = section.start_seconds();
        let section_end = section.end_seconds();
        
        if position_seconds >= section_start && position_seconds <= section_end {
            return section.name.clone();
        }
    }
    
    // If no section found, use "Default"
    "Default".to_string()
}

/// Find a song section by name
fn find_song_section_by_name<'a>(name: &str, song: &'a setlist::core::Song) -> Option<&'a setlist::core::Section> {
    song.sections.iter().find(|s| s.name == name)
}

/// Find the start position of a section in the song
fn find_section_start_position(
    section: &fts::lyrics::core::LyricSection,
    song: &setlist::core::Song,
) -> f64 {
    // Find the song section that matches this lyric section by name
    for song_section in &song.sections {
        if song_section.name == section.name {
            return song_section.start_seconds();
        }
    }
    
    // If not found, return 0.0 (shouldn't happen, but handle gracefully)
    0.0
}

/// Get MIDI events from a take using MIDI_GetAllEvts
fn get_midi_events_from_take(take: MediaItemTake) -> Result<Vec<MidiEventData>, LyricsReadError> {
    unsafe {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        
        // First call: get required buffer size
        let mut buf_size: i32 = 0;
        let success = medium_reaper.low().MIDI_GetAllEvts(
            take.as_ptr(),
            std::ptr::null_mut(),
            &mut buf_size,
        );
        
        if !success || buf_size <= 0 {
            return Ok(Vec::new()); // No MIDI events
        }
        
        // Second call: get actual MIDI data
        let mut buffer = vec![0u8; buf_size as usize];
        let buffer_ptr = buffer.as_mut_ptr() as *mut std::os::raw::c_char;
        let success = medium_reaper.low().MIDI_GetAllEvts(
            take.as_ptr(),
            buffer_ptr,
            &mut buf_size,
        );
        
        if !success {
            return Err(LyricsReadError::MidiReadFailed);
        }
        
        // Parse MIDI buffer
        // Format: { int offset, char flag, int msglen, unsigned char msg[] }
        let mut events = Vec::new();
        let mut pos = 0usize;
        
        while pos < buffer.len() {
            // Read offset (i32, 4 bytes)
            if pos + 4 > buffer.len() {
                break;
            }
            let offset = i32::from_le_bytes([
                buffer[pos],
                buffer[pos + 1],
                buffer[pos + 2],
                buffer[pos + 3],
            ]);
            pos += 4;
            
            // Read flag (u8, 1 byte)
            if pos >= buffer.len() {
                break;
            }
            let flag = buffer[pos];
            pos += 1;
            
            // Read msglen (i32, 4 bytes)
            if pos + 4 > buffer.len() {
                break;
            }
            let msglen = i32::from_le_bytes([
                buffer[pos],
                buffer[pos + 1],
                buffer[pos + 2],
                buffer[pos + 3],
            ]);
            pos += 4;
            
            // Read msg (msglen bytes)
            if pos + msglen as usize > buffer.len() {
                break;
            }
            let msg = buffer[pos..pos + msglen as usize].to_vec();
            pos += msglen as usize;
            
            events.push(MidiEventData {
                offset_ticks: offset,
                flag,
                message: msg,
            });
        }
        
        Ok(events)
    }
}

/// Data structure for lyrics read from REAPER
#[derive(Debug, Clone)]
pub struct LyricsData {
    pub slides: Vec<SlideData>,
    pub midi_tracks: Vec<MidiTrackData>,
}

#[derive(Debug, Clone)]
pub struct SlideData {
    pub text: String,
    pub position: f64,
    pub length: f64,
}

#[derive(Debug, Clone)]
pub struct MidiTrackData {
    pub name: String,
    pub items: Vec<MidiItemData>,
}

#[derive(Debug, Clone)]
pub struct MidiItemData {
    pub position: f64,
    pub length: f64,
    pub events: Vec<MidiEventData>,
}

#[derive(Debug, Clone)]
pub struct MidiEventData {
    pub offset_ticks: i32,
    pub flag: u8,
    pub message: Vec<u8>,
}

#[derive(Debug, thiserror::Error)]
pub enum LyricsReadError {
    #[error("LYRICS folder not found")]
    LyricsFolderNotFound,
    #[error("SLIDES track not found in LYRICS folder")]
    SlidesTrackNotFound,
    #[error("Failed to read MIDI events")]
    MidiReadFailed,
}

