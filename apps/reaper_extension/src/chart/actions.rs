//! Actions for chart/chord detection

use crate::chart::{build, detection, read};
use crate::infrastructure::action_registry::{ActionDef, ActionSection};
use keyflow::chord::midi::{MidiNoteName, midi_pitch_to_note_name};
use keyflow::engraver::export::{PdfExportConfig, PdfSerializer, SvgExportConfig, SvgSerializer};
use keyflow::engraver::layout::chart::{ChartLayoutConfig, ChartLayoutEngine, LayoutMode};
use keyflow::engraver::style::MStyle;
use keyflow::time::{MusicalDuration, MusicalPosition, PPQDuration, PPQPosition, TimeSignature};
use reaper_high::{Project, Reaper};
use reaper_medium::ReaperStringArg;
use std::sync::Arc;
use tracing::{info, warn};

/// Log chords detected from MIDI data in the current project.
///
/// Uses the unified `generate_chart_text_from_reaper()` pipeline — the same
/// code path as the chart window and test 022 — so chord spelling respects
/// the key signature and the output uses proper keyflow notation.
fn log_chords_handler() {
    use crate::services::generate_chart_text_from_reaper;

    let reaper = Reaper::get();
    let project = reaper.current_project();

    info!("\n=== FastTrackStudio: Chart - Detected Chords ===\n");

    // Get key signature from KEY track (optional, used for chord respelling)
    let key = read::read_key_from_track(project.clone());
    let key_root = key.as_ref().map(|k| {
        let s = k.to_string();
        // Key::to_string produces e.g. "Eb Ionian" — extract root only
        s.split_whitespace().next().unwrap_or(&s).to_string()
    });
    if let Some(ref kr) = key_root {
        info!(key = %kr, "Detected key from KEY track");
    } else {
        warn!("Key: Not detected (KEY track not found or empty)");
    }

    // Use the unified pipeline (same as chart window and test 022)
    let chart_text = match generate_chart_text_from_reaper(
        project,
        key_root.as_deref(),
        None, // title comes from the pipeline
    ) {
        Some(text) if !text.trim().is_empty() => text,
        _ => {
            warn!("No chart text generated — is there a CHORDS track with MIDI notes?");
            return;
        }
    };

    // Log to tracing and REAPER console
    info!("\n{}", chart_text);
    reaper.show_console_msg(ReaperStringArg::from(chart_text.as_str()));
}

/// Convert a Chart to chart syntax format
/// Format: Section name on one line, chords with rhythm notation on next lines
/// Example:
///   Intro
///   D /// G/ //// x4
fn format_chart_to_syntax(chart: &keyflow::chart::Chart) -> String {
    use keyflow::chart::types::{ChordInstance, Measure};
    use keyflow::time::DurationTrait;

    let mut output = String::new();

    // Add title if present
    if let Some(ref title) = chart.metadata.title {
        output.push_str(title);
        output.push('\n');
    }

    // Process each section
    for section in &chart.sections {
        // Add newline before section header for spacing
        output.push('\n');
        // Section name
        output.push_str(&section.section.display_name());
        output.push('\n');

        if section.measures().is_empty() {
            output.push('\n');
            continue;
        }

        // Format all measures without repeat detection - just write them all out
        // Group measures into lines of roughly 4 measures each
        const MEASURES_PER_LINE: usize = 4;

        // Format all measures first
        let mut all_measures_formatted = Vec::new();
        for measure in section.measures() {
            all_measures_formatted.push(format_measure_to_syntax(measure));
        }

        // Group into lines of MEASURES_PER_LINE
        for line_start in (0..all_measures_formatted.len()).step_by(MEASURES_PER_LINE) {
            let line_end = (line_start + MEASURES_PER_LINE).min(all_measures_formatted.len());

            // Build this line
            let mut line = String::new();
            for measure_idx in line_start..line_end {
                if measure_idx > line_start {
                    line.push(' ');
                }
                line.push_str(&all_measures_formatted[measure_idx]);
            }

            output.push_str(&line);
            output.push('\n');
        }

        output.push('\n');
    }

    output
}

/// Group of measures that repeat together
struct MeasureGroup {
    measures: Vec<keyflow::chart::types::Measure>,
    repeat_count: usize,
}

/// Group measures and detect repeating patterns
fn group_measures_with_repeats(measures: &[keyflow::chart::types::Measure]) -> Vec<MeasureGroup> {
    if measures.is_empty() {
        return vec![];
    }

    let mut groups = Vec::new();
    let mut i = 0;

    while i < measures.len() {
        // Look for repeating patterns
        let mut best_pattern: Option<(usize, usize)> = None; // (pattern_length, repeat_count)

        // Try different pattern lengths (1 to 8 measures)
        for pattern_len in 1..=8.min(measures.len() - i) {
            let pattern = &measures[i..i + pattern_len];

            // Count how many times this pattern repeats
            let mut repeat_count = 1;
            let mut j = i + pattern_len;

            while j + pattern_len <= measures.len() {
                let next_pattern = &measures[j..j + pattern_len];
                if measures_equal(pattern, next_pattern) {
                    repeat_count += 1;
                    j += pattern_len;
                } else {
                    break;
                }
            }

            // Prefer longer patterns with more repeats
            if repeat_count > 1 {
                if let Some((best_len, best_repeats)) = best_pattern {
                    if repeat_count > best_repeats
                        || (repeat_count == best_repeats && pattern_len > best_len)
                    {
                        best_pattern = Some((pattern_len, repeat_count));
                    }
                } else {
                    best_pattern = Some((pattern_len, repeat_count));
                }
            }
        }

        if let Some((pattern_len, repeat_count)) = best_pattern {
            // Found a repeating pattern
            let pattern = measures[i..i + pattern_len].to_vec();
            groups.push(MeasureGroup {
                measures: pattern,
                repeat_count,
            });
            i += pattern_len * repeat_count;
        } else {
            // No repeat found, add single measure
            groups.push(MeasureGroup {
                measures: vec![measures[i].clone()],
                repeat_count: 1,
            });
            i += 1;
        }
    }

    groups
}

/// Check if two measure slices are equal (same chords)
fn measures_equal(
    a: &[keyflow::chart::types::Measure],
    b: &[keyflow::chart::types::Measure],
) -> bool {
    if a.len() != b.len() {
        return false;
    }

    for (m1, m2) in a.iter().zip(b.iter()) {
        if m1.chords.len() != m2.chords.len() {
            return false;
        }

        for (c1, c2) in m1.chords.iter().zip(m2.chords.iter()) {
            if c1.full_symbol != c2.full_symbol {
                return false;
            }
            // Could also check duration, but for repeat detection, symbol is enough
        }
    }

    true
}

/// Format a single measure to chart syntax
/// Format: Each chord followed by slashes representing its duration in beats
/// In 4/4: / = 1 beat, // = 2 beats, /// = 3 beats, //// = 4 beats
/// Example: "D //// G ////" = D for 4 beats, G for 4 beats (2 measures)
/// Example: "D /// G /" = D for 3 beats, G for 1 beat (1 measure, 4 beats total)
fn format_measure_to_syntax(measure: &keyflow::chart::types::Measure) -> String {
    use keyflow::time::DurationTrait;

    if measure.chords.is_empty() {
        return String::new();
    }

    let time_sig = keyflow::time::TimeSignature::new(
        measure.time_signature.0 as i32,
        measure.time_signature.1 as i32,
    );
    let beats_per_measure = time_sig.numerator as f64;

    // Log time signature for debugging
    tracing::debug!(
        "Formatting measure with time signature: {}/{} ({} beats per measure)",
        measure.time_signature.0,
        measure.time_signature.1,
        beats_per_measure
    );

    let mut result = String::new();
    let mut current_beat = 0.0;

    // Calculate positions for all chords first
    let mut chord_positions: Vec<(f64, f64)> = Vec::new(); // (start_beat, end_beat)
    for chord in &measure.chords {
        let duration_beats = chord.duration.to_beats(time_sig);
        let start_beat = current_beat;
        let end_beat = current_beat + duration_beats;
        chord_positions.push((start_beat, end_beat));
        current_beat = end_beat;
    }

    // Format chords with rhythm notation
    for (chord_idx, chord) in measure.chords.iter().enumerate() {
        if chord_idx > 0 {
            result.push(' ');
        }

        // Push/pull notation
        if let Some((is_push, _)) = chord.push_pull {
            if is_push {
                result.push('\'');
            }
        }

        // Chord symbol
        result.push_str(&chord.full_symbol);

        // Note: The full_symbol already includes the bass note from chord.to_string()
        // So we don't need to add it again here. The Display implementation for Chord
        // already handles slash chords (e.g., "A/C#"), so adding it again would create "A/C#/C#"
        //
        // However, we should check if the bass note is the same as the root (not a real inversion)
        // and if so, we might want to suppress it, but that's handled in the chord detection logic.

        // Pull notation
        if let Some((is_push, _)) = chord.push_pull {
            if !is_push {
                result.push('\'');
            }
        }

        // Add space before slashes
        result.push(' ');

        // Use the actual chord duration, not calculated from positions
        let duration_beats = chord.duration.to_beats(time_sig);

        // Always add slashes to represent the duration in beats
        // In 4/4: / = 1 beat (quarter note), // = 2 beats (half note), etc.
        // Use floor to avoid rounding up (e.g., 2.9 beats should show as 2, not 3)
        // But also check if we're very close to the next beat (within 0.1)
        let beats_rounded = if duration_beats - duration_beats.floor() > 0.9 {
            duration_beats.ceil() as usize
        } else {
            duration_beats.floor() as usize
        };
        // Don't cap at beats_per_measure - show the actual duration (chords can span multiple measures)
        let beats_to_show = beats_rounded.max(1); // At least 1 beat

        // Get the starting beat within the measure
        // MusicalPosition.beat appears to be 1-indexed based on display format (e.g., "1.1.000")
        // But we need to verify: if beat=1 means "at the start of beat 1", then:
        // - At beat 1, we can use beats 1, 2, 3, 4 (4 beats available)
        // - At beat 4, we can use beat 4 only (1 beat available)
        // However, if beat is 0-indexed internally, beat 0 = first beat, beat 3 = fourth beat
        // Let's check: if beat is 0-indexed, beat 3 means we're at the start of the 4th beat
        // So beats available = 4 - 3 = 1 beat (the 4th beat itself)
        // If beat is 1-indexed, beat 4 means we're at the start of the 4th beat
        // So beats available = 4 - (4 - 1) = 1 beat

        // Based on the debug output showing "Pos: 1.1.000", it seems beat is 1-indexed
        // But let's be safe and handle both cases
        let start_beat_raw = chord.position.total_duration.beat.max(0) as usize;
        let beats_per_measure_int = beats_per_measure as usize;

        // Try treating beat as 0-indexed first (more common in programming)
        // If beat is 0-indexed: beat 0 = first beat, so at beat 0 we have 4 beats (0,1,2,3)
        // At beat 3 (4th beat), we have 1 beat (beat 3)
        let beats_available_in_measure = if start_beat_raw < beats_per_measure_int {
            beats_per_measure_int - start_beat_raw
        } else {
            // If beat is >= beats_per_measure, we're in the next measure
            // This shouldn't happen, but handle it gracefully
            beats_per_measure_int
        };

        // Calculate how many beats of the chord are in the current measure
        let beats_in_current_measure = beats_available_in_measure.min(beats_to_show);
        let remaining_beats = beats_to_show.saturating_sub(beats_in_current_measure);

        // Format slashes, respecting measure boundaries
        // Strategy: Group by beats_per_measure (e.g., 3 for 3/4, 4 for 4/4), but insert a space at measure boundaries
        // We track slashes in the current group, and add a space when we hit beats_per_measure or at a measure boundary

        let beats_per_measure_int = beats_per_measure as usize;
        let mut slashes_in_current_group = 0;

        // First, add slashes for the current measure
        for _ in 0..beats_in_current_measure {
            // If we've completed a group of beats_per_measure, add a space before the next slash
            if slashes_in_current_group == beats_per_measure_int {
                result.push(' '); // Space between groups
                slashes_in_current_group = 0;
            }

            result.push('/');
            slashes_in_current_group += 1;
        }

        // If the chord spans into the next measure(s), add a space at measure boundary and continue
        if remaining_beats > 0 {
            // Always add a space at measure boundary (even if we just completed a group)
            result.push(' '); // Space at measure boundary
            slashes_in_current_group = 0; // Reset group counter at measure boundary

            for _ in 0..remaining_beats {
                // If we've completed a group of beats_per_measure, add a space before the next slash
                if slashes_in_current_group == beats_per_measure_int {
                    result.push(' '); // Space between groups
                    slashes_in_current_group = 0;
                }

                result.push('/');
                slashes_in_current_group += 1;
            }
        }
    }

    result
}

/// Debug handler: Log MIDI notes and section information
fn debug_midi_and_sections_handler() {
    let reaper = Reaper::get();
    let current_project = reaper.current_project();

    info!("\n=== FastTrackStudio: Chart Debug - MIDI Notes & Sections ===\n");

    // Find CHORDS track
    let chords_track = match read::find_track_by_name(current_project.clone(), "CHORDS") {
        Some(track) => track,
        None => {
            let msg = "CHORDS track not found.\n";
            reaper.show_console_msg(msg);
            warn!("CHORDS track not found");
            return;
        }
    };

    // Read MIDI notes
    let midi_notes = read::read_midi_notes_from_track(current_project.clone(), chords_track);

    // Read markers and regions for section info
    use reaper_high::Reaper;
    let markers = read::read_markers_from_project(current_project.clone());
    let regions = read::read_regions_from_project(current_project.clone());
    let boundaries = read::find_song_boundaries(&markers, &regions);
    let section_regions = boundaries
        .as_ref()
        .map(|b| read::get_sections_from_regions(&regions, b));

    // Get tempo for PPQ to seconds conversion
    let ppq_resolution = 480.0;
    let bpm = 120.0; // TODO: Get actual tempo from project
    let time_sig = keyflow::time::TimeSignature {
        numerator: 4,
        denominator: 4,
    };
    let beats_per_measure = time_sig.numerator as f64;
    let ppq_to_seconds = |ppq: i64| -> f64 { (ppq as f64 / ppq_resolution) * (60.0 / bpm) };

    // Detect chords first so we can show them with MIDI groups
    let min_chord_duration_ppq = 180;
    let detected_chords =
        detection::detect_chords_from_reaper_midi_notes(&midi_notes, min_chord_duration_ppq);

    // Create a map of start_ppq -> detected chord for quick lookup
    use std::collections::BTreeMap;
    let mut chord_by_start: BTreeMap<i64, &detection::DetectedChord> = BTreeMap::new();
    for chord in &detected_chords {
        chord_by_start.insert(chord.start_ppq, chord);
    }

    let mut debug_output = String::new();
    debug_output.push_str("=== MIDI Notes & Chord Detection ===\n");
    debug_output.push_str(&format!(
        "Total MIDI notes: {} | Detected chords: {}\n\n",
        midi_notes.len(),
        detected_chords.len()
    ));

    // Group notes by start_ppq
    let mut notes_by_time: BTreeMap<i64, Vec<&read::MidiNote>> = BTreeMap::new();
    for note in &midi_notes {
        notes_by_time
            .entry(note.start_ppq)
            .or_insert_with(Vec::new)
            .push(note);
    }

    // Log grouped notes with corresponding chords (first 30 groups)
    let mut group_count = 0;
    for (start_ppq, notes) in notes_by_time.iter().take(30) {
        group_count += 1;
        let note_names: Vec<String> = notes
            .iter()
            .map(|n| midi_pitch_to_note_name(n.pitch))
            .collect();
        let duration_ppq = notes[0].end_ppq - notes[0].start_ppq;
        let time_sec = ppq_to_seconds(*start_ppq);
        let duration_sec = ppq_to_seconds(duration_ppq);

        // Sort notes by MIDI pitch (ascending) for consistent display
        let mut notes_with_pitch: Vec<(u8, String)> = notes
            .iter()
            .map(|n| (n.pitch, midi_pitch_to_note_name(n.pitch)))
            .collect();
        notes_with_pitch.sort_by_key(|(pitch, _)| *pitch);
        let sorted_notes: Vec<String> = notes_with_pitch
            .iter()
            .map(|(_, name)| name.clone())
            .collect();

        // Convert PPQ to MusicalPosition and PPQDuration for display
        let ppq_pos = PPQPosition::new(*start_ppq);
        let ppq_dur = PPQDuration::new(duration_ppq);

        // Convert to musical position/duration (using manual calculation for display)
        // TODO: Use REAPER's conversion functions here too
        let ppq_resolution = 480.0;
        let bpm = 120.0;
        let time_sig = keyflow::time::TimeSignature {
            numerator: 4,
            denominator: 4,
        };
        let beats_per_measure = time_sig.numerator as f64;
        let total_beats = *start_ppq as f64 / ppq_resolution;
        let measures = (total_beats / beats_per_measure).floor() as i32;
        let beats_in_measure = (total_beats % beats_per_measure).floor() as i32;
        let subdivision = ((total_beats % 1.0) * 1000.0).round() as i32;
        let musical_pos =
            MusicalPosition::try_new(measures, beats_in_measure, subdivision.clamp(0, 999))
                .unwrap_or_else(|_| MusicalPosition::start());

        let dur_beats = duration_ppq as f64 / ppq_resolution;
        let dur_measures = (dur_beats / beats_per_measure).floor() as i32;
        let dur_beats_in_measure = (dur_beats % beats_per_measure).floor() as i32;
        let dur_subdivision = ((dur_beats % 1.0) * 1000.0).round() as i32;
        let musical_dur = MusicalDuration::try_new(
            dur_measures,
            dur_beats_in_measure,
            dur_subdivision.clamp(0, 999),
        )
        .unwrap_or_else(|_| MusicalDuration::zero());

        debug_output.push_str(&format!(
            "[{:6.2}s] {:3} notes: [{}] | PPQ: {} | Pos: {} | Dur: {}\n",
            time_sec,
            notes.len(),
            sorted_notes.join(", "),
            ppq_pos,
            musical_pos,
            musical_dur
        ));

        // Show detected chord if one exists at this start time
        if let Some(chord) = chord_by_start.get(start_ppq) {
            let chord_end_sec = ppq_to_seconds(chord.end_ppq);
            let chord_duration_sec = chord_end_sec - time_sec;

            // Convert chord PPQ to musical position/duration
            let chord_ppq_pos = PPQPosition::new(chord.start_ppq);
            let chord_ppq_dur = PPQDuration::new(chord.end_ppq - chord.start_ppq);
            let chord_total_beats = chord.start_ppq as f64 / ppq_resolution;
            let chord_measures = (chord_total_beats / beats_per_measure).floor() as i32;
            let chord_beats_in_measure = (chord_total_beats % beats_per_measure).floor() as i32;
            let chord_subdivision = ((chord_total_beats % 1.0) * 1000.0).round() as i32;
            let chord_musical_pos = MusicalPosition::try_new(
                chord_measures,
                chord_beats_in_measure,
                chord_subdivision.clamp(0, 999),
            )
            .unwrap_or_else(|_| MusicalPosition::start());

            let chord_dur_beats = (chord.end_ppq - chord.start_ppq) as f64 / ppq_resolution;
            let chord_dur_measures = (chord_dur_beats / beats_per_measure).floor() as i32;
            let chord_dur_beats_in_measure = (chord_dur_beats % beats_per_measure).floor() as i32;
            let chord_dur_subdivision = ((chord_dur_beats % 1.0) * 1000.0).round() as i32;
            let chord_musical_dur = MusicalDuration::try_new(
                chord_dur_measures,
                chord_dur_beats_in_measure,
                chord_dur_subdivision.clamp(0, 999),
            )
            .unwrap_or_else(|_| MusicalDuration::zero());

            debug_output.push_str(&format!(
                "         → Chord: {} (root: {}) | PPQ: {} - {} (dur: {}) | Pos: {} | Dur: {}\n",
                chord.chord.to_string(),
                midi_pitch_to_note_name(chord.root_pitch),
                chord_ppq_pos,
                PPQPosition::new(chord.end_ppq),
                chord_ppq_dur,
                chord_musical_pos,
                chord_musical_dur
            ));
        } else {
            debug_output.push_str("         → No chord detected\n");
        }
        debug_output.push('\n');
    }

    if notes_by_time.len() > 30 {
        debug_output.push_str(&format!(
            "... and {} more note groups\n\n",
            notes_by_time.len() - 30
        ));
    }

    debug_output.push_str("=== Markers & Regions ===\n");

    // Show markers
    debug_output.push_str(&format!("Markers: {}\n", markers.len()));
    for m in &markers {
        debug_output.push_str(&format!("  '{}' at {:.3}s\n", m.name, m.position_seconds));
    }

    // Show boundaries
    if let Some(ref bounds) = boundaries {
        debug_output.push_str(&format!(
            "\nSong boundaries: start={:.3}s, end={:.3}s",
            bounds.song_start, bounds.song_end
        ));
        if let Some(ci) = bounds.count_in_start {
            debug_output.push_str(&format!(", count_in={:.3}s", ci));
        }
        debug_output.push('\n');
    } else {
        debug_output.push_str("\nNo SONGSTART marker found - no song boundaries\n");
    }

    debug_output.push_str("\n=== Sections ===\n");

    if let Some(ref sections) = section_regions {
        debug_output.push_str(&format!("{} sections found\n\n", sections.len()));

        for (idx, section) in sections.iter().enumerate() {
            let start_sec = section.start_seconds;
            let end_sec = section.end_seconds;
            let duration = end_sec - start_sec;

            debug_output.push_str(&format!(
                "Section {}: '{}' [{:.3}s - {:.3}s, dur: {:.3}s]\n",
                idx + 1,
                section.name,
                start_sec,
                end_sec,
                duration
            ));

            // Find notes and chords in this section
            let notes_in_section: Vec<_> = midi_notes
                .iter()
                .filter(|note| {
                    let note_time = ppq_to_seconds(note.start_ppq);
                    let note_end_time = ppq_to_seconds(note.end_ppq);
                    note_time < end_sec && note_end_time > start_sec
                })
                .collect();

            let chords_in_section: Vec<_> = detected_chords
                .iter()
                .filter(|chord| {
                    let chord_start = ppq_to_seconds(chord.start_ppq);
                    chord_start >= start_sec && chord_start < end_sec
                })
                .collect();

            debug_output.push_str(&format!(
                "  Notes: {} | Chords: {}\n",
                notes_in_section.len(),
                chords_in_section.len()
            ));

            // Group notes by start_ppq within this section
            let mut section_notes_by_time: BTreeMap<i64, Vec<&read::MidiNote>> =
                BTreeMap::new();
            for note in &notes_in_section {
                section_notes_by_time
                    .entry(note.start_ppq)
                    .or_insert_with(Vec::new)
                    .push(note);
            }

            // Show note groups with chords (first 8 groups per section)
            for (start_ppq, notes) in section_notes_by_time.iter().take(8) {
                let note_time = ppq_to_seconds(*start_ppq);
                let mut notes_with_pitch: Vec<(u8, String)> = notes
                    .iter()
                    .map(|n| (n.pitch, midi_pitch_to_note_name(n.pitch)))
                    .collect();
                notes_with_pitch.sort_by_key(|(pitch, _)| *pitch);
                let sorted_note_names: Vec<String> = notes_with_pitch
                    .iter()
                    .map(|(_, name)| name.clone())
                    .collect();

                let note_ppq_pos = PPQPosition::new(*start_ppq);

                debug_output.push_str(&format!(
                    "  [{:6.2}s] [{}] | PPQ: {}",
                    note_time,
                    sorted_note_names.join(", "),
                    note_ppq_pos
                ));

                if let Some(chord) = chord_by_start.get(start_ppq) {
                    debug_output.push_str(&format!(" → {}\n", chord.chord.to_string()));
                } else {
                    debug_output.push_str(" → (no chord)\n");
                }
            }

            if section_notes_by_time.len() > 8 {
                debug_output.push_str(&format!(
                    "  ... and {} more note groups\n",
                    section_notes_by_time.len() - 8
                ));
            }

            debug_output.push('\n');
        }
    } else {
        debug_output.push_str("No sections found (no SONGSTART marker or no regions)\n");
    }

    debug_output.push_str("\n=====================================\n");

    // Log to tracing
    info!("{}", debug_output);

    // Also log to REAPER console
    reaper.show_console_msg(ReaperStringArg::from(debug_output.as_str()));
}

// Embedded fonts from musescore reference library
static BRAVURA_FONT: &[u8] = include_bytes!(
    "../../../../libs/reference/sheet-music/musescore/fonts/bravura/Bravura.otf"
);
static TEXT_FONT: &[u8] =
    include_bytes!("../../../../libs/reference/sheet-music/musescore/fonts/FreeSans.ttf");

/// Export chart to PDF file
fn export_chart_pdf_handler() {
    let reaper = Reaper::get();
    let current_project = reaper.current_project();

    info!("\n=== FastTrackStudio: Export Chart to PDF ===\n");

    // Find CHORDS track (default name, can be made configurable later)
    let chords_track = match read::find_track_by_name(current_project.clone(), "CHORDS") {
        Some(track) => track,
        None => {
            warn!("CHORDS track not found. Please create a track named 'CHORDS' with MIDI notes.");
            reaper.show_console_msg("CHORDS track not found.\n");
            return;
        }
    };

    // Read MIDI notes from CHORDS track
    let midi_notes = read::read_midi_notes_from_track(current_project.clone(), chords_track);

    if midi_notes.is_empty() {
        info!("No MIDI notes found in CHORDS track");
        reaper.show_console_msg("No MIDI notes found in CHORDS track.\n");
        return;
    }

    // Get key signature from KEY track (optional)
    let key = read::read_key_from_track(current_project.clone());
    if let Some(ref key_info) = key {
        info!(key = %key_info, "Detected key from KEY track");
    }

    // Detect chords from MIDI notes
    let min_chord_duration_ppq = 180;
    let detected_chords =
        detection::detect_chords_from_reaper_midi_notes(&midi_notes, min_chord_duration_ppq);

    if detected_chords.is_empty() {
        reaper.show_console_msg("No chords detected from MIDI notes.\n");
        info!("No chords detected");
        return;
    }

    info!(
        chord_count = detected_chords.len(),
        "Detected chords from MIDI notes"
    );

    // Build Chart from detected chords
    let tempo_bpm = Some(120.0);
    let chart = build::build_chart_from_chords(detected_chords, current_project.clone(), key, tempo_bpm);

    // Get project file path for default save location
    let default_path = if let Some(project_path) = current_project.file() {
        let mut pdf_path = project_path.clone();
        pdf_path.set_extension("pdf");
        pdf_path
    } else {
        camino::Utf8PathBuf::from("chart.pdf")
    };

    // Create layout engine
    let style: &'static MStyle = Box::leak(Box::new(MStyle::default()));
    let config = ChartLayoutConfig {
        use_stems: false,
        use_page_offsets: false, // For PDF export
        ..ChartLayoutConfig::default()
    };
    let mode = LayoutMode::Paginated {
        page_width: 612.0,  // US Letter
        page_height: 792.0,
    };

    let engine = ChartLayoutEngine::with_config(
        config,
        style,
        Arc::new(TEXT_FONT.to_vec()),
        Arc::new(BRAVURA_FONT.to_vec()),
    );

    // Layout the chart
    let layout_result = engine.layout_chart(&chart, &mode);

    // Get the scene for rendering
    let scene = &layout_result.scene;

    // Create PDF config with embedded fonts
    let title = chart.metadata.title.clone().unwrap_or_else(|| "Chart".to_string());
    let pdf_config = PdfExportConfig::letter()
        .with_title(&title)
        .with_author("FastTrackStudio")
        .with_font(Arc::new(TEXT_FONT.to_vec()))
        .with_smufl_font(Arc::new(BRAVURA_FONT.to_vec()));

    // Serialize to PDF
    let mut serializer = PdfSerializer::new(pdf_config);

    match serializer.serialize(&scene) {
        Ok(pdf_bytes) => {
            // Save to file
            let save_path = default_path.as_str();
            match std::fs::write(&save_path, &pdf_bytes) {
                Ok(_) => {
                    let msg = format!("Chart exported to: {}\n", save_path);
                    reaper.show_console_msg(ReaperStringArg::from(msg.as_str()));
                    info!("PDF exported to: {}", save_path);
                }
                Err(e) => {
                    let msg = format!("Failed to save PDF: {}\n", e);
                    reaper.show_console_msg(ReaperStringArg::from(msg.as_str()));
                    warn!("Failed to save PDF: {}", e);
                }
            }
        }
        Err(e) => {
            let msg = format!("Failed to generate PDF: {}\n", e);
            reaper.show_console_msg(ReaperStringArg::from(msg.as_str()));
            warn!("Failed to generate PDF: {}", e);
        }
    }
}

/// Register chart/keyflow actions
pub fn register_chart_actions() {
    // Register keyflow actions (chord detection, chart building, PDF export)
    let keyflow_actions = vec![
        ActionDef {
            command_id: "FTS_KEYFLOW_LOG_CHORDS",
            display_name: "Log Chords".to_string(),
            handler: log_chords_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
        ActionDef {
            command_id: "FTS_KEYFLOW_EXPORT_PDF",
            display_name: "Export Chart as PDF".to_string(),
            handler: export_chart_pdf_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        },
    ];

    crate::infrastructure::action_registry::register_actions(&keyflow_actions, "Keyflow");

    // Register dev/debug actions (only if dev feature is enabled)
    #[cfg(feature = "dev")]
    {
        let dev_actions = vec![ActionDef {
            command_id: "FTS_DEV_DEBUG_MIDI_SECTIONS",
            display_name: "Debug MIDI & Sections".to_string(),
            handler: debug_midi_and_sections_handler,
            appears_in_menu: true,
            section: ActionSection::Main,
            ..Default::default()
        }];

        crate::infrastructure::action_registry::register_actions(&dev_actions, "Dev");
    }
}
