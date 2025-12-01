//! REAPER Lyrics Command Handler Implementation
//!
//! Implements lyrics command handlers for REAPER extension.
//! These are used by the setlist stream protocol.

use fts::setlist::infra::stream::LyricsState;
use reaper_high::Reaper;
use reaper_medium::{MediaItemTake, PositionInSeconds};
use std::ffi::CString;
use fts::lyrics::Syllable;
use fts::lyrics::core::split_line_into_words;

/// REAPER implementation of lyrics command handlers
pub struct ReaperLyricsCommandHandler;

impl ReaperLyricsCommandHandler {
    pub fn new() -> Self {
        Self
    }
    
    /// Get the current line from the LYRIC track at edit cursor position
    fn get_current_line_at_cursor(&self) -> Result<Option<(usize, String)>, String> {
        let reaper = Reaper::get();
        let project = reaper.current_project();
        
        // Find LYRICS folder
        let lyrics_folder_tracks = crate::lyrics::read::find_folder_tracks(project, "LYRICS")
            .ok_or_else(|| "LYRICS folder not found".to_string())?;
        
        // Find SLIDES track
        let slides_track = lyrics_folder_tracks
            .iter()
            .find(|track| {
                track.name()
                    .map(|n| n.to_str().eq_ignore_ascii_case("SLIDES"))
                    .unwrap_or(false)
            })
            .ok_or_else(|| "SLIDES track not found in LYRICS folder".to_string())?;
        
        // Get edit cursor position
        let cursor_pos = project
            .edit_cursor_position()
            .unwrap_or(PositionInSeconds::ZERO)
            .get();
        
        // Find the text item that contains the cursor position
        let mut current_line_idx = 0;
        let mut found_line: Option<String> = None;
        
        for (idx, item) in slides_track.items().enumerate() {
            let item_start = item.position().get();
            let item_end = item_start + item.length().get();
            
            if cursor_pos >= item_start && cursor_pos < item_end {
                // Get item notes (contains slide text)
                let notes = unsafe {
                    let medium_reaper = reaper.medium_reaper();
                    let notes_key = CString::new("P_NOTES").expect("CString::new failed");
                    let mut buffer = vec![0u8; 4096];
                    let buffer_ptr = buffer.as_mut_ptr() as *mut std::os::raw::c_char;
                    
                    let success = medium_reaper.low().GetSetMediaItemInfo_String(
                        item.raw().as_ptr(),
                        notes_key.as_ptr(),
                        buffer_ptr,
                        false, // setNewValue = false (get value)
                    );
                    
                    if success {
                        use std::ffi::CStr;
                        let c_str = CStr::from_ptr(buffer_ptr);
                        c_str.to_string_lossy().to_string()
                    } else {
                        String::new()
                    }
                };
                
                if !notes.trim().is_empty() {
                    current_line_idx = idx;
                    found_line = Some(notes);
                    break;
                }
            }
        }
        
        Ok(found_line.map(|line| (current_line_idx, line)))
    }
    
    /// Get the next MIDI note at or after edit cursor position
    fn get_next_midi_note_at_cursor(&self) -> Result<Option<(MediaItemTake, i32, f64)>, String> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let project = reaper.current_project();
        
        // Get edit cursor position
        let cursor_pos = project
            .edit_cursor_position()
            .unwrap_or(PositionInSeconds::ZERO)
            .get();
        
        // Find LYRICS folder
        let lyrics_folder_tracks = crate::lyrics::read::find_folder_tracks(project, "LYRICS")
            .ok_or_else(|| "LYRICS folder not found".to_string())?;
        
        // Find all MIDI items in LYRICS folder (excluding SLIDES track)
        let mut next_note: Option<(MediaItemTake, i32, f64)> = None;
        let mut next_note_ppq = f64::MAX;
        
        for track in lyrics_folder_tracks {
            let track_name = track.name()
                .map(|n| n.to_str().to_string())
                .unwrap_or_default();
            
            if track_name.eq_ignore_ascii_case("SLIDES") {
                continue; // Skip SLIDES track
            }
            
            for item in track.items() {
                let item_start = item.position().get();
                let item_end = item_start + item.length().get();
                
                // Only look at items that start at or after cursor
                if item_start < cursor_pos {
                    continue;
                }
                
                // Get active take
                let take = match item.active_take() {
                    Some(take) => take,
                    None => continue,
                };
                
                // Check if take is MIDI
                let is_midi = unsafe {
                    medium_reaper.low().TakeIsMIDI(take.raw().as_ptr())
                };
                
                if !is_midi {
                    continue;
                }
                
                // Count MIDI notes
                let mut note_count: i32 = 0;
                let mut _cc_count: i32 = 0;
                let mut _text_sysex_count: i32 = 0;
                
                unsafe {
                    medium_reaper.low().MIDI_CountEvts(
                        take.raw().as_ptr(),
                        &mut note_count,
                        &mut _cc_count,
                        &mut _text_sysex_count,
                    );
                }
                
                // Find the first note at or after cursor position
                for note_idx in 0..(note_count as usize) {
                    let mut _selected: bool = false;
                    let mut _muted: bool = false;
                    let mut start_ppq: f64 = 0.0;
                    let mut _end_ppq: f64 = 0.0;
                    let mut _channel: i32 = 0;
                    let mut _pitch: i32 = 0;
                    let mut _velocity: i32 = 0;
                    
                    let success = unsafe {
                        medium_reaper.low().MIDI_GetNote(
                            take.raw().as_ptr(),
                            note_idx as i32,
                            &mut _selected,
                            &mut _muted,
                            &mut start_ppq,
                            &mut _end_ppq,
                            &mut _channel,
                            &mut _pitch,
                            &mut _velocity,
                        )
                    };
                    
                    if success {
                        // Convert PPQ to project time
                        let note_time = unsafe {
                            medium_reaper.low().MIDI_GetProjTimeFromPPQPos(take.raw().as_ptr(), start_ppq)
                        };
                        
                        // Only consider notes at or after cursor
                        if note_time >= cursor_pos && start_ppq < next_note_ppq {
                            next_note = Some((take.raw(), note_idx as i32, start_ppq));
                            next_note_ppq = start_ppq;
                            break; // Found a note in this item, move to next item
                        }
                    }
                }
            }
        }
        
        Ok(next_note)
    }
    
    /// Assign syllable to a MIDI note
    fn assign_syllable_to_midi_note(&self, take: MediaItemTake, note_idx: i32, start_ppq: f64, syllable_text: &str) -> Result<(), String> {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        
        let c_syllable_text = CString::new(syllable_text).map_err(|e| format!("Failed to create CString: {}", e))?;
        
        // MIDI text event type 1 is for lyrics
        let success = unsafe {
            medium_reaper.low().MIDI_InsertTextSysexEvt(
                take.as_ptr(),
                false, // selected
                false, // muted
                start_ppq,
                1, // type: 1 for lyrics
                c_syllable_text.as_ptr(),
                c_syllable_text.as_bytes().len() as i32,
            )
        };
        
        if success {
            // Sort MIDI events after inserting
            unsafe {
                medium_reaper.low().MIDI_Sort(take.as_ptr());
            }
            Ok(())
        } else {
            Err("Failed to insert MIDI text event".to_string())
        }
    }
}

impl ReaperLyricsCommandHandler {
    pub async fn advance_syllable(&self) -> Result<LyricsState, String> {
        // Get current line at cursor
        let (line_idx, line_text) = match self.get_current_line_at_cursor()? {
            Some((idx, text)) => (idx, text),
            None => {
                return Ok(LyricsState {
                    current_line_index: None,
                    current_syllable_index: None,
                    total_syllables: 0,
                    line_text: None,
                    has_lyrics: false,
                });
            }
        };
        
        // Split line into words and syllables
        let words = split_line_into_words(&line_text);
        let syllables: Vec<(usize, usize, &Syllable)> = words
            .iter()
            .enumerate()
            .flat_map(|(word_idx, word)| {
                word.syllables.iter().enumerate().map(move |(syl_idx, syl)| (word_idx, syl_idx, syl))
            })
            .collect();
        
        if syllables.is_empty() {
            return Ok(LyricsState {
                current_line_index: Some(line_idx),
                current_syllable_index: None,
                total_syllables: 0,
                line_text: Some(line_text),
                has_lyrics: true,
            });
        }
        
        // Find the next unassigned syllable (one without a MIDI note)
        // For now, we'll just get the next syllable in sequence
        // TODO: Track which syllables have been assigned
        
        // Get the next MIDI note at cursor
        let (take, note_idx, start_ppq) = match self.get_next_midi_note_at_cursor()? {
            Some(note) => note,
            None => {
                return Err("No MIDI note found at or after edit cursor position".to_string());
            }
        };
        
        // Get the first syllable (we'll improve this to track progress later)
        let (word_idx, syl_idx, syllable) = syllables[0];
        let syllable_text = syllable.text.clone();
        
        // Assign syllable to MIDI note
        self.assign_syllable_to_midi_note(take, note_idx, start_ppq, &syllable_text)?;
        
        Ok(LyricsState {
            current_line_index: Some(line_idx),
            current_syllable_index: Some(syl_idx),
            total_syllables: syllables.len(),
            line_text: Some(line_text),
            has_lyrics: true,
        })
    }
    
    pub async fn get_lyrics_state(&self) -> Result<LyricsState, String> {
        match self.get_current_line_at_cursor()? {
            Some((line_idx, line_text)) => {
                let words = split_line_into_words(&line_text);
                let total_syllables: usize = words.iter().map(|w| w.syllables.len()).sum();
                
                Ok(LyricsState {
                    current_line_index: Some(line_idx),
                    current_syllable_index: None, // TODO: Track current syllable
                    total_syllables,
                    line_text: Some(line_text),
                    has_lyrics: true,
                })
            }
            None => {
                Ok(LyricsState {
                    current_line_index: None,
                    current_syllable_index: None,
                    total_syllables: 0,
                    line_text: None,
                    has_lyrics: false,
                })
            }
        }
    }
    
    pub async fn assign_syllable_to_note(&self, syllable_text: String) -> Result<(), String> {
        let (take, note_idx, start_ppq) = match self.get_next_midi_note_at_cursor()? {
            Some(note) => note,
            None => {
                return Err("No MIDI note found at or after edit cursor position".to_string());
            }
        };
        
        self.assign_syllable_to_midi_note(take, note_idx, start_ppq, &syllable_text)
    }
}

