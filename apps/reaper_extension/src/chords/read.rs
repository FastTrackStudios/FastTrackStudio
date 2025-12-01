//! Functions for reading MIDI data from REAPER for chord detection

use reaper_high::{Reaper, Project};
use reaper_medium::MediaItemTake;
use tracing::debug;
use crate::chords::types::MidiNoteEvent;
use crate::lyrics::read::find_track_by_name;

/// Read MIDI notes from a specific track for chord detection
pub fn read_midi_notes_from_track(
    project: Project,
    track_name: &str,
) -> Result<Vec<MidiNoteEvent>, ChordReadError> {
    let track = find_track_by_name(project, track_name)
        .ok_or(ChordReadError::TrackNotFound {
            track_name: track_name.to_string(),
        })?;

    let mut all_notes = Vec::new();

    // Iterate through all items on the track
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

        if !is_midi {
            continue;
        }

        // Get item position for PPQ conversion
        let item_position = item.position().get();

        // Read notes directly using MIDI_GetNote
        let notes = read_notes_from_take(take, item_position)?;

        all_notes.extend(notes);
    }

    debug!(
        track_name = track_name,
        note_count = all_notes.len(),
        "Read {} MIDI notes from track",
        all_notes.len()
    );

    Ok(all_notes)
}

/// Read MIDI notes directly from a take using MIDI_GetNote
fn read_notes_from_take(
    take: MediaItemTake,
    item_position: f64,
) -> Result<Vec<MidiNoteEvent>, ChordReadError> {
    unsafe {
        let reaper = Reaper::get();
        let medium_reaper = reaper.medium_reaper();
        let low_reaper = medium_reaper.low();

        // Get note count
        let mut note_count: i32 = 0;
        let mut cc_count: i32 = 0;
        let mut text_sysex_count: i32 = 0;
        low_reaper.MIDI_CountEvts(
            take.as_ptr(),
            &mut note_count,
            &mut cc_count,
            &mut text_sysex_count,
        );

        let mut notes = Vec::new();

        // Read each note
        for i in 0..note_count {
            let mut selected = false;
            let mut muted = false;
            let mut start_ppq: f64 = 0.0;
            let mut end_ppq: f64 = 0.0;
            let mut channel: i32 = 0;
            let mut pitch: i32 = 0;
            let mut velocity: i32 = 0;

            let success = low_reaper.MIDI_GetNote(
                take.as_ptr(),
                i as i32,
                &mut selected,
                &mut muted,
                &mut start_ppq,
                &mut end_ppq,
                &mut channel,
                &mut pitch,
                &mut velocity,
            );

            if !success {
                continue;
            }

            // Filter out muted notes
            if muted {
                continue;
            }

            // Convert PPQ positions to i32 (REAPER uses f64 but we'll use i32 for precision)
            let start_ppq_i32 = start_ppq as i32;
            let end_ppq_i32 = end_ppq as i32;

            notes.push(MidiNoteEvent {
                pitch: pitch as u8,
                start_ppq: start_ppq_i32,
                end_ppq: end_ppq_i32,
                selected,
                muted: false, // Already filtered
            });
        }

        Ok(notes)
    }
}

/// Read MIDI notes from REAPER project (uses current project)
pub fn read_midi_notes_from_reaper(track_name: &str) -> Result<Vec<MidiNoteEvent>, ChordReadError> {
    let reaper = Reaper::get();
    let project = reaper.current_project();
    read_midi_notes_from_track(project, track_name)
}

#[derive(Debug, thiserror::Error)]
pub enum ChordReadError {
    #[error("Track '{track_name}' not found")]
    TrackNotFound { track_name: String },
    #[error("Failed to read MIDI events")]
    MidiReadFailed,
    #[error("Invalid MIDI data: {message}")]
    InvalidMidiData { message: String },
}

