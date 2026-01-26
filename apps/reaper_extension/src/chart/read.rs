//! Functions for reading MIDI data from REAPER for chord detection

use reaper_high::{Project, Reaper, Track};
use reaper_medium::MediaItemTake;
use tracing::warn;

/// Find a track by name in the project
pub fn find_track_by_name(project: Project, name: &str) -> Option<Track> {
    let mut all_track_names = Vec::new();
    for track in project.tracks() {
        if let Some(track_name) = track.name() {
            let track_name_str = track_name.to_str();
            all_track_names.push(track_name_str.to_string());
            // Debug: log all track names to help diagnose
            tracing::debug!("Checking track: '{}' against '{}'", track_name_str, name);
            if track_name_str.eq_ignore_ascii_case(name) {
                tracing::info!("Found matching track: '{}'", track_name_str);
                return Some(track);
            }
        } else {
            all_track_names.push("<unnamed>".to_string());
        }
    }
    tracing::warn!(
        "Track '{}' not found in project. Available tracks: {:?}",
        name,
        all_track_names
    );
    None
}

/// MIDI note event
#[derive(Debug, Clone)]
pub struct MidiNote {
    pub pitch: u8,
    pub start_ppq: i64,
    pub end_ppq: i64,
    pub channel: u8,
    pub velocity: u8,
}

/// Read all MIDI notes from a track
pub fn read_midi_notes_from_track(project: Project, track: Track) -> Vec<MidiNote> {
    let mut notes = Vec::new();
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    let track_name = track
        .name()
        .map(|n| n.to_str().to_string())
        .unwrap_or_else(|| "Unknown".to_string());
    tracing::info!("Reading MIDI notes from track: '{}'", track_name);

    let item_count = track.items().count();
    tracing::info!("Found {} items on track '{}'", item_count, track_name);

    // Reset iterator since we consumed it with count()
    for item in track.items() {
        let item_raw = item.raw();

        // Get active take
        let take = match unsafe { medium_reaper.get_active_take(item_raw) } {
            Some(take) => take,
            None => {
                tracing::debug!("Item has no active take, skipping");
                continue;
            }
        };

        // Check if take is MIDI using unsafe low-level API
        let is_midi = unsafe { medium_reaper.low().TakeIsMIDI(take.as_ptr()) };

        if !is_midi {
            tracing::debug!("Take is not MIDI, skipping");
            continue;
        }

        tracing::debug!("Found MIDI take, reading notes...");

        // Read MIDI notes using MIDI_CountEvts and MIDI_GetNote
        unsafe {
            let low_reaper = medium_reaper.low();
            let mut note_count: i32 = 0;
            let mut cc_count: i32 = 0;
            let mut text_sysex_count: i32 = 0;

            // Get note count
            let _ = low_reaper.MIDI_CountEvts(
                take.as_ptr(),
                &mut note_count,
                &mut cc_count,
                &mut text_sysex_count,
            );

            tracing::debug!(
                "MIDI take has {} notes, {} CC events, {} text/sysex events",
                note_count,
                cc_count,
                text_sysex_count
            );

            // Read each note
            for i in 0..note_count {
                let mut selected: bool = false;
                let mut muted: bool = false;
                let mut start_ppq: f64 = 0.0;
                let mut end_ppq: f64 = 0.0;
                let mut channel: i32 = 0;
                let mut pitch: i32 = 0;
                let mut velocity: i32 = 0;

                let success = low_reaper.MIDI_GetNote(
                    take.as_ptr(),
                    i,
                    &mut selected,
                    &mut muted,
                    &mut start_ppq,
                    &mut end_ppq,
                    &mut channel,
                    &mut pitch,
                    &mut velocity,
                );

                if success && !muted {
                    notes.push(MidiNote {
                        pitch: pitch as u8,
                        start_ppq: start_ppq as i64,
                        end_ppq: end_ppq as i64,
                        channel: channel as u8,
                        velocity: velocity as u8,
                    });
                }
            }
        }
    }

    tracing::info!(
        "Read {} MIDI notes from track '{}'",
        notes.len(),
        track_name
    );
    notes
}

/// Read key signature from KEY track
///
/// The KEY track should have items where each item's notes (P_NOTES) contain the key name.
/// For example: "C", "Cm", "F#", "Bbm", "Eb major", etc.
///
/// Falls back to reading the first MIDI note pitch if no item notes are found.
pub fn read_key_from_track(project: Project) -> Option<keyflow::Key> {
    let key_track = find_track_by_name(project, "KEY")?;

    // Get the first item on the KEY track
    let first_item = key_track.items().next()?;
    let item_raw = first_item.raw();

    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();

    // First, try to read key from item notes (P_NOTES)
    let key_from_notes = unsafe {
        let notes_key = std::ffi::CString::new("P_NOTES").expect("CString::new failed");
        let mut buffer = vec![0u8; 256];
        let buffer_ptr = buffer.as_mut_ptr() as *mut std::os::raw::c_char;

        let success = medium_reaper.low().GetSetMediaItemInfo_String(
            item_raw.as_ptr(),
            notes_key.as_ptr(),
            buffer_ptr,
            false, // setNewValue = false (get value)
        );

        if success {
            let c_str = std::ffi::CStr::from_ptr(buffer_ptr);
            let notes = c_str.to_string_lossy().to_string();
            let trimmed = notes.trim();
            if !trimmed.is_empty() {
                tracing::debug!("KEY track item notes: '{}'", trimmed);
                parse_key_from_string(trimmed)
            } else {
                None
            }
        } else {
            None
        }
    };

    if key_from_notes.is_some() {
        return key_from_notes;
    }

    // Fallback: read key from first MIDI note pitch
    let take = unsafe { medium_reaper.get_active_take(item_raw) }?;

    // Check if take is MIDI using unsafe low-level API
    let is_midi = unsafe { medium_reaper.low().TakeIsMIDI(take.as_ptr()) };

    if !is_midi {
        return None;
    }

    // Get the first note (should represent the key root)
    unsafe {
        let low_reaper = medium_reaper.low();
        let mut note_count: i32 = 0;
        let mut cc_count: i32 = 0;
        let mut text_sysex_count: i32 = 0;

        // Get note count
        let _ = low_reaper.MIDI_CountEvts(
            take.as_ptr(),
            &mut note_count,
            &mut cc_count,
            &mut text_sysex_count,
        );

        if note_count == 0 {
            return None;
        }

        // Read first note to get key root
        let mut selected: bool = false;
        let mut muted: bool = false;
        let mut start_ppq: f64 = 0.0;
        let mut end_ppq: f64 = 0.0;
        let mut channel: i32 = 0;
        let mut pitch: i32 = 0;
        let mut velocity: i32 = 0;

        let success = low_reaper.MIDI_GetNote(
            take.as_ptr(),
            0,
            &mut selected,
            &mut muted,
            &mut start_ppq,
            &mut end_ppq,
            &mut channel,
            &mut pitch,
            &mut velocity,
        );

        if !success {
            return None;
        }

        // Convert MIDI note number to MusicalNote
        // MIDI note 60 = C4, so we need to get the note name (C, C#, D, etc.)
        let note_names = [
            "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B",
        ];
        let note_index = ((pitch as u8) % 12) as usize;
        let note_name = note_names[note_index];

        // Try to parse as MusicalNote
        if let Some(musical_note) = keyflow::primitives::MusicalNote::from_string(note_name) {
            // For now, assume major key (we can enhance this later to detect minor)
            // We could check if there are multiple notes to determine major/minor
            return Some(keyflow::Key::major(musical_note));
        }
    }

    None
}

/// Parse a key from a string like "C", "Cm", "F#m", "Bb major", "Eb minor"
fn parse_key_from_string(s: &str) -> Option<keyflow::Key> {
    let s = s.trim();
    if s.is_empty() {
        return None;
    }

    // Check for minor indicators
    let is_minor = s.ends_with('m')
        || s.to_lowercase().ends_with("minor")
        || s.to_lowercase().ends_with("min");

    // Extract the root note
    let root_str = if is_minor {
        if s.to_lowercase().ends_with("minor") {
            s[..s.len() - 5].trim()
        } else if s.to_lowercase().ends_with("min") {
            s[..s.len() - 3].trim()
        } else {
            // Ends with 'm'
            &s[..s.len() - 1]
        }
    } else if s.to_lowercase().ends_with("major") {
        s[..s.len() - 5].trim()
    } else if s.to_lowercase().ends_with("maj") {
        s[..s.len() - 3].trim()
    } else {
        s
    };

    // Try to parse as MusicalNote
    let musical_note = keyflow::primitives::MusicalNote::from_string(root_str)?;

    if is_minor {
        Some(keyflow::Key::minor(musical_note))
    } else {
        Some(keyflow::Key::major(musical_note))
    }
}
