//! MIDI service for reading MIDI data from REAPER tracks
//!
//! Provides shared utilities for reading MIDI notes from REAPER tracks,
//! used by chord_overlay, chart_window, and other features.

use keyflow::chord::{
    DetectedChord, MidiNote as KeyflowMidiNote, TimingAnalysisConfig,
    analyze_chord_timing, detect_chords_from_midi_notes, ChordTimingAnalysis,
};
use reaper_high::{Project, Reaper, Track};
use reaper_medium::MediaItemTake;
use tracing::warn;

/// Minimum chord duration in PPQ to filter out arpeggiated fragments
pub const MIN_CHORD_DURATION_PPQ: i64 = 180;

/// Find a track by name in the project (case-insensitive)
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

/// MIDI take info returned from get_first_midi_take
pub struct MidiTakeInfo {
    /// The MIDI take
    pub take: MediaItemTake,
    /// Item start position in project time (seconds)
    pub item_start_time: f64,
    /// Item start measure (0-indexed, internal)
    pub item_start_measure: i32,
}

/// Get the first MIDI take from a track with item position info
pub fn get_first_midi_take(track: &Track) -> Option<MidiTakeInfo> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let low = medium_reaper.low();

    let first_item = track.items().next()?;
    let item_raw = first_item.raw();
    let take = unsafe { medium_reaper.get_active_take(item_raw) }?;

    // Check if take is MIDI
    let is_midi = unsafe { low.TakeIsMIDI(take.as_ptr()) };
    if !is_midi {
        return None;
    }

    // Get item start position in project time
    let item_start_time = unsafe {
        low.GetMediaItemInfo_Value(item_raw.as_ptr(), c"D_POSITION".as_ptr())
    };

    // Convert item start time to measure number
    let mut measures: i32 = 0;
    let mut cml: i32 = 0;
    let mut fullbeats: f64 = 0.0;
    let mut cdenom: i32 = 0;

    unsafe {
        low.TimeMap2_timeToBeats(
            std::ptr::null_mut(),
            item_start_time,
            &mut measures as *mut _,
            &mut cml as *mut _,
            &mut fullbeats as *mut _,
            &mut cdenom as *mut _,
        );
    }

    Some(MidiTakeInfo {
        take,
        item_start_time,
        item_start_measure: measures,
    })
}

/// Read MIDI notes from a take and convert to keyflow MidiNote format
pub fn read_midi_notes_from_take(take: MediaItemTake) -> Vec<KeyflowMidiNote> {
    let reaper = Reaper::get();
    let medium_reaper = reaper.medium_reaper();
    let low_reaper = medium_reaper.low();

    let mut notes = Vec::new();

    unsafe {
        let mut note_count: i32 = 0;
        let mut cc_count: i32 = 0;
        let mut text_sysex_count: i32 = 0;

        let _ = low_reaper.MIDI_CountEvts(
            take.as_ptr(),
            &mut note_count,
            &mut cc_count,
            &mut text_sysex_count,
        );

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
                notes.push(KeyflowMidiNote::new(
                    pitch as u8,
                    start_ppq as i64,
                    end_ppq as i64,
                    channel as u8,
                    velocity as u8,
                ));
            }
        }
    }

    notes
}

/// Chord with timing analysis result
#[derive(Debug, Clone)]
pub struct AnalyzedChord {
    /// The detected chord
    pub chord: DetectedChord,
    /// Timing analysis result
    pub timing: ChordTimingAnalysis,
    /// Start time in project seconds
    pub start_time: f64,
    /// End time in project seconds
    pub end_time: f64,
}

/// Detect chords from the "MIDI CHORDS" track with timing analysis
///
/// Returns analyzed chords with push/pull timing information.
/// The measure_index in timing analysis is offset by the MIDI item's start position
/// so it aligns with the project's measure numbers.
pub fn detect_chords_from_midi_track(
    project: Project,
    time_sig_numerator: u8,
    time_sig_denominator: u8,
) -> Option<Vec<AnalyzedChord>> {
    // Find "MIDI CHORDS" track
    let track = find_track_by_name(project, "MIDI CHORDS")?;

    // Get MIDI take with item position info
    let midi_info = get_first_midi_take(&track)?;
    let take = midi_info.take;
    let item_start_measure = midi_info.item_start_measure;

    // Read MIDI notes
    let midi_notes = read_midi_notes_from_take(take);
    if midi_notes.is_empty() {
        warn!("No MIDI notes found on MIDI CHORDS track");
        return None;
    }

    // Detect chords
    let detected_chords = detect_chords_from_midi_notes(&midi_notes, MIN_CHORD_DURATION_PPQ);
    if detected_chords.is_empty() {
        warn!("No chords detected from MIDI notes");
        return None;
    }

    // Analyze timing (measure_index is relative to take start)
    let config = TimingAnalysisConfig::default();
    let timing_analyses = analyze_chord_timing(
        &detected_chords,
        time_sig_numerator,
        time_sig_denominator,
        &config,
    );

    let reaper = Reaper::get();
    let low_reaper = reaper.medium_reaper().low();

    // Convert to AnalyzedChord with project times
    // Offset measure_index by item_start_measure to align with project measures
    let analyzed: Vec<AnalyzedChord> = detected_chords
        .iter()
        .zip(timing_analyses.iter())
        .map(|(chord, timing)| {
            let start_time = unsafe {
                low_reaper.MIDI_GetProjTimeFromPPQPos(take.as_ptr(), chord.start_ppq as f64)
            };
            let end_time = unsafe {
                low_reaper.MIDI_GetProjTimeFromPPQPos(take.as_ptr(), chord.end_ppq as f64)
            };

            // Offset measure_index by item start position
            let mut adjusted_timing = timing.clone();
            adjusted_timing.measure_index = (timing.measure_index as i32 + item_start_measure) as usize;

            AnalyzedChord {
                chord: chord.clone(),
                timing: adjusted_timing,
                start_time,
                end_time,
            }
        })
        .collect();

    Some(analyzed)
}

/// Get the project time signature at a specific position
pub fn get_time_signature_at_position(project: Project, position: f64) -> (u8, u8) {
    let reaper = Reaper::get();
    let low_reaper = reaper.medium_reaper().low();

    let mut timesig_num: i32 = 4;
    let mut timesig_denom: i32 = 4;
    let mut _tempo: f64 = 0.0;

    unsafe {
        low_reaper.TimeMap_GetTimeSigAtTime(
            project.context().to_raw(),
            position,
            &mut timesig_num,
            &mut timesig_denom,
            &mut _tempo,
        );
    }

    (timesig_num as u8, timesig_denom as u8)
}

/// Get the project's initial time signature
pub fn get_project_time_signature(project: Project) -> (u8, u8) {
    get_time_signature_at_position(project, 0.0)
}
