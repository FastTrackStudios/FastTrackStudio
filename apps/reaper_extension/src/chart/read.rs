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
                tracing::debug!("Found matching track: '{}'", track_name_str);
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

/// A project marker (single time point)
#[derive(Debug, Clone)]
pub struct ProjectMarker {
    pub name: String,
    pub position_seconds: f64,
}

/// A project region (time range)
#[derive(Debug, Clone)]
pub struct ProjectRegion {
    pub name: String,
    pub start_seconds: f64,
    pub end_seconds: f64,
}

/// Song boundaries derived from markers
#[derive(Debug, Clone)]
pub struct SongBoundaries {
    /// SONGSTART marker position in seconds
    pub song_start: f64,
    /// SONGEND marker position in seconds (or end of last region/item)
    pub song_end: f64,
    /// Count-In marker position in seconds (if present, before SONGSTART)
    pub count_in_start: Option<f64>,
}

/// Read all markers from the current project
pub fn read_markers_from_project(project: Project) -> Vec<ProjectMarker> {
    let mut markers = Vec::new();
    let bookmark_count = project.bookmark_count();

    for i in 0..bookmark_count.total_count {
        if let Some(bookmark) = project.find_bookmark_by_index(i) {
            let info = bookmark.basic_info();

            if info.bookmark_type() == reaper_high::BookmarkType::Marker {
                let position_seconds: f64 = info.position.into();
                let name = bookmark.name().to_string().trim().to_string();

                tracing::debug!("Found marker: '{}' at {:.3}s", name, position_seconds);
                markers.push(ProjectMarker {
                    name,
                    position_seconds,
                });
            }
        }
    }

    markers.sort_by(|a, b| {
        a.position_seconds
            .partial_cmp(&b.position_seconds)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    markers
}

/// Read all regions from the current project
pub fn read_regions_from_project(project: Project) -> Vec<ProjectRegion> {
    let mut regions = Vec::new();
    let bookmark_count = project.bookmark_count();

    for i in 0..bookmark_count.total_count {
        if let Some(bookmark) = project.find_bookmark_by_index(i) {
            let info = bookmark.basic_info();

            if info.bookmark_type() == reaper_high::BookmarkType::Region {
                if let Some(end_position) = info.region_end_position {
                    let start_seconds: f64 = info.position.into();
                    let end_seconds: f64 = end_position.into();
                    let name = bookmark.name().to_string().trim().to_string();

                    tracing::debug!(
                        "Found region: '{}' at {:.3}s - {:.3}s",
                        name,
                        start_seconds,
                        end_seconds
                    );
                    regions.push(ProjectRegion {
                        name,
                        start_seconds,
                        end_seconds,
                    });
                }
            }
        }
    }

    regions.sort_by(|a, b| {
        a.start_seconds
            .partial_cmp(&b.start_seconds)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    regions
}

/// Find song boundaries from markers
///
/// Looks for:
/// - SONGSTART marker (required) - defines the song start
/// - SONGEND marker (optional) - defines the song end
/// - Count-In marker (optional) - defines count-in start before SONGSTART
///
/// Returns None if no SONGSTART marker is found.
pub fn find_song_boundaries(
    markers: &[ProjectMarker],
    regions: &[ProjectRegion],
) -> Option<SongBoundaries> {
    // Find SONGSTART marker (case-insensitive)
    let song_start_marker = markers
        .iter()
        .find(|m| m.name.eq_ignore_ascii_case("SONGSTART"));

    let song_start = match song_start_marker {
        Some(m) => m.position_seconds,
        None => {
            tracing::warn!("No SONGSTART marker found in project");
            return None;
        }
    };

    // Find SONGEND marker (case-insensitive)
    let song_end = markers
        .iter()
        .find(|m| m.name.eq_ignore_ascii_case("SONGEND"))
        .map(|m| m.position_seconds)
        .unwrap_or_else(|| {
            // Fallback: use the end of the last region that starts after SONGSTART,
            // or a large value if no regions
            regions
                .iter()
                .filter(|r| r.start_seconds >= song_start)
                .map(|r| r.end_seconds)
                .fold(f64::NEG_INFINITY, f64::max)
                .max(song_start + 1.0) // At least 1 second after song start
        });

    // Find Count-In marker (case-insensitive)
    let count_in_start = markers
        .iter()
        .find(|m| m.name.eq_ignore_ascii_case("Count-In"))
        .map(|m| m.position_seconds)
        .filter(|&pos| pos < song_start); // Only valid if before SONGSTART

    tracing::info!(
        "Song boundaries: start={:.3}s, end={:.3}s, count_in={:?}",
        song_start,
        song_end,
        count_in_start
    );

    Some(SongBoundaries {
        song_start,
        song_end,
        count_in_start,
    })
}

/// Get sections from regions that fall between SONGSTART and SONGEND
///
/// Returns regions as sections, filtered to only those within the song boundaries.
/// Also adds a Count-In section if a Count-In marker exists before SONGSTART.
pub fn get_sections_from_regions(
    regions: &[ProjectRegion],
    boundaries: &SongBoundaries,
) -> Vec<ProjectRegion> {
    let mut sections: Vec<ProjectRegion> = regions
        .iter()
        .filter(|r| {
            // Region must be within song boundaries
            // Allow small tolerance (0.01s) for regions that start exactly at SONGSTART
            let within = r.start_seconds >= (boundaries.song_start - 0.01)
                && r.end_seconds <= (boundaries.song_end + 0.01);

            // Exclude the outermost region that spans the entire song
            // (this is the "song region" wrapper, not a section)
            let is_song_wrapper = (r.start_seconds - boundaries.song_start).abs() < 0.01
                && (r.end_seconds - boundaries.song_end).abs() < 0.01;

            let include = within && !is_song_wrapper;
            if include {
                tracing::debug!(
                    "  Section region: '{}' ({:.3}s - {:.3}s)",
                    r.name,
                    r.start_seconds,
                    r.end_seconds
                );
            }
            include
        })
        .cloned()
        .collect();

    // Add Count-In section if present
    if let Some(count_in_start) = boundaries.count_in_start {
        sections.push(ProjectRegion {
            name: "Count-In".to_string(),
            start_seconds: count_in_start,
            end_seconds: boundaries.song_start,
        });
    }

    // Sort by start time
    sections.sort_by(|a, b| {
        a.start_seconds
            .partial_cmp(&b.start_seconds)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    tracing::info!("Found {} sections within song boundaries", sections.len());
    for s in &sections {
        tracing::info!(
            "  Section: '{}' ({:.3}s - {:.3}s)",
            s.name,
            s.start_seconds,
            s.end_seconds
        );
    }

    sections
}

/// A key entry from the KEY track with its position in the project
#[derive(Debug, Clone)]
pub struct KeyEntry {
    /// The parsed key
    pub key: keyflow::Key,
    /// Position in seconds (start of the item on the KEY track)
    pub position_seconds: f64,
}

/// Read all key signatures from the KEY track, sorted by position.
///
/// Each item on the KEY track represents a key signature. The item's notes (P_NOTES)
/// contain the key name (e.g., "Eb", "Cm", "F#m"). Items later in the timeline
/// represent key changes.
///
/// Returns an empty Vec if no KEY track exists or no valid keys are found.
pub fn read_all_keys_from_track(project: Project) -> Vec<KeyEntry> {
    let key_track = match find_track_by_name(project, "KEY") {
        Some(t) => t,
        None => return Vec::new(),
    };

    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let mut entries = Vec::new();

    for item in key_track.items() {
        let item_raw = item.raw();

        // Get item start position in project time
        let item_start_time = unsafe {
            medium_reaper
                .low()
                .GetMediaItemInfo_Value(item_raw.as_ptr(), c"D_POSITION".as_ptr())
        };

        // Try to read key from item notes (P_NOTES)
        let key = unsafe {
            let notes_key = std::ffi::CString::new("P_NOTES").expect("CString::new failed");
            let mut buffer = vec![0u8; 256];
            let buffer_ptr = buffer.as_mut_ptr() as *mut std::os::raw::c_char;

            let success = medium_reaper.low().GetSetMediaItemInfo_String(
                item_raw.as_ptr(),
                notes_key.as_ptr(),
                buffer_ptr,
                false,
            );

            if success {
                let c_str = std::ffi::CStr::from_ptr(buffer_ptr);
                let notes = c_str.to_string_lossy().to_string();
                let trimmed = notes.trim();
                if !trimmed.is_empty() {
                    tracing::debug!(
                        "KEY track item at {:.3}s: '{}'",
                        item_start_time,
                        trimmed
                    );
                    parse_key_from_string(trimmed)
                } else {
                    None
                }
            } else {
                None
            }
        };

        if let Some(key) = key {
            entries.push(KeyEntry {
                key,
                position_seconds: item_start_time,
            });
        }
    }

    // Sort by position
    entries.sort_by(|a, b| {
        a.position_seconds
            .partial_cmp(&b.position_seconds)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    if !entries.is_empty() {
        tracing::debug!(
            "Read {} key entries from KEY track: {:?}",
            entries.len(),
            entries
                .iter()
                .map(|e| format!("{} at {:.3}s", e.key, e.position_seconds))
                .collect::<Vec<_>>()
        );
    }

    entries
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
