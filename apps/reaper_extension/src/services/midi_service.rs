//! MIDI service for reading MIDI data from REAPER tracks
//!
//! Provides shared utilities for reading MIDI notes from REAPER tracks,
//! used by chord_overlay, chart_window, and other features.

use keyflow::chord::{
    ChordTimingAnalysis, DetectedChord, MidiNote as KeyflowMidiNote, TimingAnalysisConfig,
    analyze_chord_timing, detect_chords_from_midi_notes,
};
use keyflow::engraver::import::{
    MarkerEvent, MarkerType, MidiChartConfig, MidiFile, MidiNote as ImportMidiNote, MidiTrack,
    TempoEvent, TimeSignatureEvent, generate_chart_text,
};
use reaper_high::{Project, Reaper, Track};
use reaper_medium::MediaItemTake;
use tracing::{debug, info, warn};

/// Minimum chord duration in PPQ to filter out arpeggiated fragments
/// Set low enough to capture staccato chords (160 ticks = short stab)
pub const MIN_CHORD_DURATION_PPQ: i64 = 120;

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
    /// Item start measure (internal 0-indexed, NOT offset-adjusted)
    ///
    /// This is the raw internal measure from `TimeMap2_timeToBeats`.
    /// Use `get_measure_at_time()` for display/offset-adjusted measures.
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
    let item_start_time =
        unsafe { low.GetMediaItemInfo_Value(item_raw.as_ptr(), c"D_POSITION".as_ptr()) };

    // Convert item start time to internal measure number (0-indexed, no offset)
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

/// Detect chords from the "CHORDS" track with timing analysis
///
/// Returns analyzed chords with push/pull timing information.
/// The measure_index in timing analysis is offset by the MIDI item's start position
/// to give absolute REAPER measure numbers.
pub fn detect_chords_from_midi_track(
    project: Project,
    time_sig_numerator: u8,
    time_sig_denominator: u8,
) -> Option<Vec<AnalyzedChord>> {
    // Find "CHORDS" track
    let track = find_track_by_name(project, "CHORDS")?;

    // Get MIDI take with item position info
    let midi_info = get_first_midi_take(&track)?;
    let take = midi_info.take;

    // Read MIDI notes
    let midi_notes = read_midi_notes_from_take(take);
    if midi_notes.is_empty() {
        warn!("No MIDI notes found on CHORDS track");
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
    // measure_index is offset by item_start_measure to give absolute REAPER measure
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

            // Offset measure_index by item start position to get absolute REAPER measure
            let mut adjusted_timing = timing.clone();
            adjusted_timing.measure_index =
                (timing.measure_index as i32 + midi_info.item_start_measure) as usize;

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

/// Get the total number of quarter-note beats from project start to a given time.
///
/// Uses REAPER's `TimeMap2_timeToBeats` for tempo-accurate calculation.
/// This accounts for tempo changes throughout the project.
/// Returns the `fullbeats` value which counts quarter notes from time 0.
pub fn get_beats_at_time(project: Project, time_seconds: f64) -> f64 {
    let reaper = Reaper::get();
    let low_reaper = reaper.medium_reaper().low();

    let mut measures: i32 = 0;
    let mut cml: i32 = 0;
    let mut fullbeats: f64 = 0.0;
    let mut cdenom: i32 = 0;

    unsafe {
        low_reaper.TimeMap2_timeToBeats(
            project.context().to_raw(),
            time_seconds,
            &mut measures as *mut _,
            &mut cml as *mut _,
            &mut fullbeats as *mut _,
            &mut cdenom as *mut _,
        );
    }

    fullbeats
}

/// Get the raw internal measure number at a given project time (0-indexed, no offset).
///
/// This returns the raw value from `TimeMap2_timeToBeats`, which is what MIDI chord
/// `measure_index` values use. Use this for mapping between MIDI data and chart measures.
pub fn get_internal_measure_at_time(project: Project, time_seconds: f64) -> i32 {
    let reaper = Reaper::get();
    let low_reaper = reaper.medium_reaper().low();

    let mut measures: i32 = 0;
    let mut cml: i32 = 0;
    let mut fullbeats: f64 = 0.0;
    let mut cdenom: i32 = 0;

    unsafe {
        low_reaper.TimeMap2_timeToBeats(
            project.context().to_raw(),
            time_seconds,
            &mut measures as *mut _,
            &mut cml as *mut _,
            &mut fullbeats as *mut _,
            &mut cdenom as *mut _,
        );
    }

    debug!(
        "get_internal_measure_at_time({:.3}s): internal={}",
        time_seconds, measures
    );

    measures
}

/// Get the display measure number at a given project time, automatically including the project's measure offset.
///
/// REAPER's `TimeMap2_timeToBeats` returns 0-indexed internal measures. This function
/// adds the project's `projmeasoffs` to give the actual display measure number.
///
/// For example, if a project has `measure_offset = -3` and a chord is at internal measure 5,
/// this returns `5 + (-3) = 2` (display measure 2).
pub fn get_measure_at_time(project: Project, time_seconds: f64) -> i32 {
    let internal = get_internal_measure_at_time(project, time_seconds);
    let measure_offset = project.measure_offset();
    let display_measure = internal + measure_offset;

    debug!(
        "get_measure_at_time({:.3}s): internal={}, offset={}, display={}",
        time_seconds, internal, measure_offset, display_measure
    );

    display_measure
}

/// REAPER's PPQ (pulses per quarter note) — always 960.
const REAPER_PPQ: u32 = 960;

/// Convert a project time (seconds) to an absolute tick position using REAPER's beat API.
///
/// Uses `TimeMap2_timeToBeats` to get the exact quarter-note beat count from project start,
/// then multiplies by PPQ. This is tempo-accurate and handles tempo changes.
fn time_to_tick(project: Project, time_seconds: f64) -> u32 {
    let beats = get_beats_at_time(project, time_seconds);
    let tick = (beats * f64::from(REAPER_PPQ)).round() as i64;
    tick.max(0) as u32
}

/// Build a keyflow `MidiFile` from REAPER project data.
///
/// This constructs the same `MidiFile` structure that `generate_chart_text()` expects,
/// but sourced from a live REAPER project instead of a `.mid` file on disk.
/// The result can be passed directly to `generate_chart_text()` for identical output
/// to the test-22 pipeline.
///
/// Data sources:
/// - MIDI notes: from the "CHORDS" track's first MIDI take
/// - Markers: REAPER project markers (SONGSTART, section names, =END)
/// - Tempo/time sig: from REAPER project at time 0
pub fn build_midi_file_from_reaper(project: Project) -> Option<MidiFile> {
    // --- 1. Read MIDI notes from CHORDS track ---
    let track = find_track_by_name(project, "CHORDS")?;
    let midi_info = get_first_midi_take(&track)?;
    let take = midi_info.take;

    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    let midi_notes = read_midi_notes_from_take(take);
    if midi_notes.is_empty() {
        warn!("build_midi_file_from_reaper: No MIDI notes found on CHORDS track");
        return None;
    }

    // Convert KeyflowMidiNote (start_ppq/end_ppq i64) → import MidiNote (start_tick/duration_ticks u32)
    // REAPER MIDI PPQ positions are relative to the take (item) start.
    // We need to offset them by the item's start position in project ticks.
    let item_start_tick = time_to_tick(project, midi_info.item_start_time);

    let import_notes: Vec<ImportMidiNote> = midi_notes
        .iter()
        .map(|n| {
            let abs_start = item_start_tick + (n.start_ppq.max(0) as u32);
            let abs_end = item_start_tick + (n.end_ppq.max(0) as u32);
            ImportMidiNote {
                pitch: n.pitch,
                velocity: n.velocity,
                start_tick: abs_start,
                duration_ticks: abs_end.saturating_sub(abs_start),
                channel: n.channel,
            }
        })
        .collect();

    let import_track = MidiTrack {
        index: 0,
        name: Some("CHORDS".to_string()),
        notes: import_notes,
        channel: None,
    };

    // --- 2. Read REAPER markers (both markers and regions provide section names) ---
    let mut markers = Vec::new();
    let mut idx = 0;
    loop {
        let mut is_region = false;
        let mut pos: f64 = 0.0;
        let mut _end: f64 = 0.0;
        let mut name_ptr: *const std::os::raw::c_char = std::ptr::null();
        let mut _marker_idx: i32 = 0;

        let result = unsafe {
            low.EnumProjectMarkers(
                idx,
                &mut is_region,
                &mut pos,
                &mut _end,
                &mut name_ptr,
                &mut _marker_idx,
            )
        };

        if result == 0 {
            break;
        }
        idx += 1;

        // Only collect non-region markers (point markers = section markers, SONGSTART, =END)
        if is_region {
            continue;
        }

        if !name_ptr.is_null() {
            let name = unsafe { std::ffi::CStr::from_ptr(name_ptr) }
                .to_string_lossy()
                .to_string();

            if !name.is_empty() {
                let tick = time_to_tick(project, pos);
                markers.push(MarkerEvent {
                    tick,
                    text: name,
                    marker_type: MarkerType::Marker,
                });
            }
        }
    }

    // Also collect region names as markers — REAPER sections are often regions
    idx = 0;
    loop {
        let mut is_region = false;
        let mut pos: f64 = 0.0;
        let mut _end: f64 = 0.0;
        let mut name_ptr: *const std::os::raw::c_char = std::ptr::null();
        let mut _marker_idx: i32 = 0;

        let result = unsafe {
            low.EnumProjectMarkers(
                idx,
                &mut is_region,
                &mut pos,
                &mut _end,
                &mut name_ptr,
                &mut _marker_idx,
            )
        };

        if result == 0 {
            break;
        }
        idx += 1;

        if !is_region {
            continue;
        }

        if !name_ptr.is_null() {
            let name = unsafe { std::ffi::CStr::from_ptr(name_ptr) }
                .to_string_lossy()
                .to_string();

            if !name.is_empty() {
                let tick = time_to_tick(project, pos);
                // Only add region as marker if no point marker exists at same tick with same name
                let already_exists = markers
                    .iter()
                    .any(|m| m.tick == tick && m.text == name);
                if !already_exists {
                    markers.push(MarkerEvent {
                        tick,
                        text: name,
                        marker_type: MarkerType::Marker,
                    });
                }
            }
        }
    }

    markers.sort_by_key(|m| m.tick);

    debug!(
        "build_midi_file_from_reaper: {} notes, {} markers, item_start_tick={}",
        midi_notes.len(),
        markers.len(),
        item_start_tick,
    );

    // --- 3. Tempo ---
    let mut tempo_bpm: f64 = 120.0;
    let mut _timesig_num: i32 = 4;
    let mut _timesig_denom: i32 = 4;
    unsafe {
        low.TimeMap_GetTimeSigAtTime(
            project.context().to_raw(),
            0.0,
            &mut _timesig_num,
            &mut _timesig_denom,
            &mut tempo_bpm,
        );
    }
    let microseconds_per_quarter = (60_000_000.0 / tempo_bpm).round() as u32;

    let tempo_map = vec![TempoEvent {
        tick: 0,
        microseconds_per_quarter,
    }];

    // --- 4. Time signature ---
    let (ts_num, ts_denom) = get_project_time_signature(project);
    let time_signatures = vec![TimeSignatureEvent {
        tick: 0,
        numerator: ts_num,
        denominator: ts_denom,
    }];

    // --- 5. Build MidiFile ---
    Some(MidiFile::from_parts(
        REAPER_PPQ,
        vec![import_track],
        tempo_map,
        time_signatures,
        markers,
        vec![Some("CHORDS".to_string())],
    ))
}

/// Generate chart text from the current REAPER project using the same pipeline as test 22.
///
/// This builds a `MidiFile` from REAPER data and calls `generate_chart_text()`,
/// producing identical output to parsing a `.mid` file directly.
pub fn generate_chart_text_from_reaper(
    project: Project,
    key_root: Option<&str>,
    title: Option<&str>,
) -> Option<String> {
    let midi_file = build_midi_file_from_reaper(project)?;

    let config = MidiChartConfig {
        key_root: key_root.map(String::from),
        title: title.map(String::from),
    };

    let text = generate_chart_text(&midi_file, &config);
    info!(
        "generate_chart_text_from_reaper: produced {} chars of chart text",
        text.len()
    );
    debug!("Chart text:\n{}", text);

    Some(text)
}
