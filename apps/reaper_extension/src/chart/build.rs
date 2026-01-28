//! Build Chart from detected chords

use super::detection::DetectedChord;
use super::read;
use keyflow::chart::{Chart, ChartSection, ChordInstance, Measure};
use keyflow::chord::ChordRhythm;
use keyflow::key::Key;
use keyflow::sections::{Section, SectionType};
use keyflow::time::{
    AbsolutePosition, MusicalDuration, MusicalPosition, PPQDuration, PPQPosition, Tempo,
    TimeSignature,
};
use reaper_high::{Project, Reaper};
use reaper_medium::MediaItemTake;
use tracing::warn;

/// Build a Chart from detected chords
///
/// This converts PPQ-based chord detections into a structured Chart with sections and measures.
/// It uses the setlist to get actual sections (Verse, Chorus, etc.) and organizes chords by section.
pub fn build_chart_from_chords(
    detected_chords: Vec<DetectedChord>,
    project: Project,
    key: Option<Key>,
    tempo_bpm: Option<f64>,
) -> Chart {
    let mut chart = Chart::new();

    // Set metadata
    let project_name = if let Some(file_path) = project.file() {
        if let Some(file_name) = file_path.file_stem() {
            let name = file_name.to_string();
            chart.metadata.title = Some(name.clone());
            Some(name)
        } else {
            None
        }
    } else {
        None
    };

    // Set key
    if let Some(k) = key {
        chart.initial_key = Some(k.clone());
        chart.current_key = Some(k);
    }

    // Set tempo
    let bpm = tempo_bpm.unwrap_or(120.0);
    chart.tempo = Some(Tempo::new(bpm));

    // Default time signature (TODO: read from project tempo/time sig markers)
    let time_sig = TimeSignature::new(4, 4);

    tracing::debug!(
        "Chart time signature: {}/{}",
        time_sig.numerator,
        time_sig.denominator
    );

    chart.initial_time_signature = Some(time_sig);
    chart.time_signature = Some(time_sig);

    if detected_chords.is_empty() {
        return chart;
    }

    // Read markers and regions directly from the project
    let markers = read::read_markers_from_project(project.clone());
    let regions = read::read_regions_from_project(project.clone());

    // Find song boundaries from SONGSTART/SONGEND/Count-In markers
    let boundaries = read::find_song_boundaries(&markers, &regions);

    // Get sections from regions within the song boundaries
    let section_regions = boundaries.as_ref().map(|b| read::get_sections_from_regions(&regions, b));

    // Get a MIDI take from the project to use for PPQ conversions
    // We'll use the CHORDS track if available, otherwise any MIDI take
    let chords_track = read::find_track_by_name(project.clone(), "CHORDS");
    let midi_take: Option<MediaItemTake> = chords_track.and_then(|track| {
        track.items().next().and_then(|item| {
            let reaper = Reaper::get();
            let medium_reaper = reaper.medium_reaper();
            unsafe { medium_reaper.get_active_take(item.raw()) }
        })
    });

    // Time signature (assume 4/4 for now - TODO: get from project)
    let time_sig = TimeSignature {
        numerator: 4,
        denominator: 4,
    };
    let beats_per_measure = time_sig.numerator as f64;

    // Convert PPQ to MusicalPosition using REAPER's functions
    // First convert PPQ to quarter notes, then to musical position
    let ppq_to_musical_position = |ppq: i64| -> MusicalPosition {
        if let Some(take) = midi_take {
            let reaper = Reaper::get();
            let low_reaper = reaper.medium_reaper().low();
            // Convert PPQ to quarter notes using REAPER
            let quarter_notes =
                unsafe { low_reaper.MIDI_GetProjQNFromPPQPos(take.as_ptr(), ppq as f64) };
            // Convert quarter notes to musical position
            let measures = (quarter_notes / beats_per_measure).floor() as i32;
            let beats_in_measure = (quarter_notes % beats_per_measure).floor() as i32;
            let subdivision = ((quarter_notes % 1.0) * 1000.0).round() as i32;
            MusicalPosition::try_new(measures, beats_in_measure, subdivision.clamp(0, 999))
                .unwrap_or_else(|_| MusicalPosition::start())
        } else {
            // Fallback: manual calculation if no take available
            let ppq_resolution = 480.0;
            let total_beats = ppq as f64 / ppq_resolution;
            let measures = (total_beats / beats_per_measure).floor() as i32;
            let beats_in_measure = (total_beats % beats_per_measure).floor() as i32;
            let subdivision = ((total_beats % 1.0) * 1000.0).round() as i32;
            MusicalPosition::try_new(measures, beats_in_measure, subdivision.clamp(0, 999))
                .unwrap_or_else(|_| MusicalPosition::start())
        }
    };

    // Convert PPQ duration to MusicalDuration using REAPER's functions
    // Note: We need to convert start and end positions separately, then subtract
    let ppq_to_musical_duration = |start_ppq: i64, end_ppq: i64| -> MusicalDuration {
        if let Some(take) = midi_take {
            let reaper = Reaper::get();
            let low_reaper = reaper.medium_reaper().low();
            // Convert PPQ positions to quarter notes using REAPER
            // MIDI_GetProjQNFromPPQPos returns quarter notes (in 4/4, 1 quarter note = 1 beat)
            let start_qn =
                unsafe { low_reaper.MIDI_GetProjQNFromPPQPos(take.as_ptr(), start_ppq as f64) };
            let end_qn =
                unsafe { low_reaper.MIDI_GetProjQNFromPPQPos(take.as_ptr(), end_ppq as f64) };
            // Duration in quarter notes is the difference
            let quarter_notes = end_qn - start_qn;
            // Convert quarter notes to musical duration
            // In 4/4: 1 quarter note = 1 beat, so quarter_notes directly gives us beats
            let total_beats = quarter_notes;
            let measures = (total_beats / beats_per_measure).floor() as i32;
            let beats_in_measure = (total_beats % beats_per_measure).floor() as i32;
            let subdivision = ((total_beats % 1.0) * 1000.0).round() as i32;
            MusicalDuration::try_new(measures, beats_in_measure, subdivision.clamp(0, 999))
                .unwrap_or_else(|_| MusicalDuration::zero())
        } else {
            // Fallback: manual calculation if no take available
            let ppq_resolution = 480.0;
            let total_beats = (end_ppq - start_ppq) as f64 / ppq_resolution;
            let measures = (total_beats / beats_per_measure).floor() as i32;
            let beats_in_measure = (total_beats % beats_per_measure).floor() as i32;
            let subdivision = ((total_beats % 1.0) * 1000.0).round() as i32;
            MusicalDuration::try_new(measures, beats_in_measure, subdivision.clamp(0, 999))
                .unwrap_or_else(|_| MusicalDuration::zero())
        }
    };

    // Convert PPQ to seconds using REAPER's function
    let ppq_to_seconds = |ppq: i64| -> f64 {
        if let Some(take) = midi_take {
            let reaper = Reaper::get();
            let low_reaper = reaper.medium_reaper().low();
            unsafe { low_reaper.MIDI_GetProjTimeFromPPQPos(take.as_ptr(), ppq as f64) }
        } else {
            // Fallback: manual calculation
            let ppq_resolution = 480.0;
            (ppq as f64 / ppq_resolution) * (60.0 / bpm)
        }
    };

    // For measure calculations, we still need ticks_per_measure
    // Get PPQ resolution from REAPER if possible, otherwise use default
    let ppq_resolution = 480.0; // TODO: Get actual PPQ resolution from project
    let ticks_per_measure = (ppq_resolution * beats_per_measure) as i64;

    // Convert all detected chords to chord instances with time positions
    struct ChordWithTime {
        instance: ChordInstance,
        position: MusicalPosition,
        duration: MusicalDuration,
        ppq_position: PPQPosition,
        ppq_duration: PPQDuration,
        time_seconds: f64, // For section matching (sections use seconds)
        end_time_seconds: f64,
    }

    let mut chords_with_time: Vec<ChordWithTime> = Vec::new();

    for detected_chord in detected_chords {
        // Convert PPQ to musical position and duration using REAPER's conversion functions
        let position = ppq_to_musical_position(detected_chord.start_ppq);
        let duration = ppq_to_musical_duration(detected_chord.start_ppq, detected_chord.end_ppq);

        // Create PPQ position and duration
        let ppq_position = PPQPosition::new(detected_chord.start_ppq);
        let ppq_duration = PPQDuration::new(detected_chord.end_ppq - detected_chord.start_ppq);

        // Convert PPQ to seconds for section matching
        let chord_time_seconds = ppq_to_seconds(detected_chord.start_ppq);
        let chord_end_time_seconds = ppq_to_seconds(detected_chord.end_ppq);

        let absolute_position = AbsolutePosition::new(position.clone(), 0);

        // Get root notation from chord
        let chord_string = detected_chord.chord.to_string();
        let root = detected_chord.chord.root.clone();
        let chord = detected_chord.chord.clone();

        // Clone duration before moving it into ChordInstance
        let duration_for_instance = duration.clone();

        // Create chord instance
        let chord_instance = ChordInstance::new(
            root,
            chord_string.clone(),
            chord,
            ChordRhythm::Default,
            chord_string,
            duration_for_instance,
            absolute_position,
        );

        chords_with_time.push(ChordWithTime {
            instance: chord_instance,
            position: position.clone(),
            duration,
            ppq_position,
            ppq_duration,
            time_seconds: chord_time_seconds,
            end_time_seconds: chord_end_time_seconds,
        });
    }

    // If we have section regions, organize chords by section
    if let Some(ref regions) = section_regions {
        if !regions.is_empty() {
            // Group chords by section based on time position
            for region in regions {
                let section_start = region.start_seconds;
                let section_end = region.end_seconds;

                // Log section boundaries for debugging
                tracing::debug!(
                    "Section '{}': start={:.3}s, end={:.3}s",
                    region.name,
                    section_start,
                    section_end
                );

                // Find all chords that are within this section's time range
                let section_chords: Vec<_> = chords_with_time
                    .iter()
                    .filter(|cwt| {
                        let in_section =
                            cwt.time_seconds >= section_start && cwt.time_seconds < section_end;
                        if in_section {
                            tracing::debug!(
                                "  Chord {} ({:.3}s - {:.3}s) in section '{}' ({:.3}s - {:.3}s)",
                                cwt.instance.full_symbol,
                                cwt.time_seconds,
                                cwt.end_time_seconds,
                                region.name,
                                section_start,
                                section_end
                            );
                        }
                        in_section
                    })
                    .collect();

                if section_chords.is_empty() {
                    continue;
                }

                // Convert section start to PPQ and musical position to calculate offset
                let section_start_ppq = if let Some(take) = midi_take {
                    let reaper = Reaper::get();
                    let low_reaper = reaper.medium_reaper().low();
                    unsafe {
                        low_reaper.MIDI_GetPPQPosFromProjTime(take.as_ptr(), section_start) as i64
                    }
                } else {
                    (section_start / (60.0 / bpm) * ppq_resolution) as i64
                };

                // Get the musical position of the section start to use as offset
                let section_start_position = ppq_to_musical_position(section_start_ppq);
                let section_start_measure = section_start_position.measure;

                // Organize chords into measures for this section
                let measure_offset = section_start_measure - 1;
                let mut measures: Vec<Measure> = Vec::new();
                let mut current_measure_chords: Vec<ChordInstance> = Vec::new();
                let mut current_measure_index: Option<i32> = None;

                let time_sig_tuple = chart
                    .time_signature
                    .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                    .unwrap_or((4, 4));

                tracing::debug!(
                    "Building measures for section '{}' with time signature: {}/{}",
                    region.name,
                    time_sig_tuple.0,
                    time_sig_tuple.1
                );

                for chord_with_time in section_chords {
                    let chord_measure_absolute = chord_with_time.position.measure;
                    let mut chord_measure_relative = chord_measure_absolute - measure_offset;
                    chord_measure_relative = chord_measure_relative.max(1);

                    if let Some(measure_idx) = current_measure_index {
                        if chord_measure_relative > measure_idx {
                            if !current_measure_chords.is_empty() {
                                let measure = Measure::new()
                                    .with_chords(current_measure_chords.clone())
                                    .with_time_signature(time_sig_tuple);
                                measures.push(measure);
                            }

                            let mut gap_measure = measure_idx + 1;
                            while gap_measure < chord_measure_relative {
                                measures.push(Measure::new().with_time_signature(time_sig_tuple));
                                gap_measure += 1;
                            }

                            current_measure_chords = Vec::new();
                            current_measure_index = Some(chord_measure_relative);
                        }
                    } else {
                        if chord_measure_relative > 1 {
                            for _ in 1..chord_measure_relative {
                                measures.push(Measure::new().with_time_signature(time_sig_tuple));
                            }
                        }
                        current_measure_index = Some(chord_measure_relative);
                    }

                    current_measure_chords.push(chord_with_time.instance.clone());
                }

                // Add final measure
                if !current_measure_chords.is_empty() {
                    let measure = Measure::new()
                        .with_chords(current_measure_chords)
                        .with_time_signature(time_sig_tuple);
                    measures.push(measure);
                }

                // Parse section type from region name
                let section = parse_section_from_region_name(&region.name);
                let chart_section = ChartSection::new(section).with_measures(measures);
                chart.sections.push(chart_section);
            }

            return chart;
        }
    }

    // Fallback: no sections found - use all chords between SONGSTART and SONGEND
    // (or all chords if no boundaries found)
    let time_sig_tuple = chart
        .time_signature
        .map(|ts| (ts.numerator as u8, ts.denominator as u8))
        .unwrap_or((4, 4));

    // Filter chords to song boundaries if available
    let filtered_chords: Vec<&ChordWithTime> = if let Some(ref bounds) = boundaries {
        chords_with_time
            .iter()
            .filter(|cwt| {
                cwt.time_seconds >= bounds.song_start && cwt.time_seconds < bounds.song_end
            })
            .collect()
    } else {
        chords_with_time.iter().collect()
    };

    tracing::debug!(
        "Building fallback section with {} chords, time signature: {}/{}",
        filtered_chords.len(),
        time_sig_tuple.0,
        time_sig_tuple.1
    );

    // Organize chords into measures
    let mut measures: Vec<Measure> = Vec::new();
    let mut current_measure_chords: Vec<ChordInstance> = Vec::new();
    let mut current_measure_index: Option<i32> = None;

    // Calculate measure offset from song start
    let song_start_measure_offset = if let Some(ref bounds) = boundaries {
        let song_start_ppq = if let Some(take) = midi_take {
            let reaper = Reaper::get();
            let low_reaper = reaper.medium_reaper().low();
            unsafe {
                low_reaper.MIDI_GetPPQPosFromProjTime(take.as_ptr(), bounds.song_start) as i64
            }
        } else {
            (bounds.song_start / (60.0 / bpm) * ppq_resolution) as i64
        };
        ppq_to_musical_position(song_start_ppq).measure - 1
    } else {
        0
    };

    for chord_with_time in &filtered_chords {
        let chord_measure_absolute = chord_with_time.position.measure;
        let mut chord_measure_relative = chord_measure_absolute - song_start_measure_offset;
        chord_measure_relative = chord_measure_relative.max(1);

        if let Some(measure_idx) = current_measure_index {
            if chord_measure_relative > measure_idx {
                if !current_measure_chords.is_empty() {
                    let measure = Measure::new()
                        .with_chords(current_measure_chords.clone())
                        .with_time_signature(time_sig_tuple);
                    measures.push(measure);
                }

                let mut gap_measure = measure_idx + 1;
                while gap_measure < chord_measure_relative {
                    measures.push(Measure::new().with_time_signature(time_sig_tuple));
                    gap_measure += 1;
                }

                current_measure_chords = Vec::new();
                current_measure_index = Some(chord_measure_relative);
            }
        } else {
            if chord_measure_relative > 1 {
                for _ in 1..chord_measure_relative {
                    measures.push(Measure::new().with_time_signature(time_sig_tuple));
                }
            }
            current_measure_index = Some(chord_measure_relative);
        }

        current_measure_chords.push(chord_with_time.instance.clone());
    }

    if !current_measure_chords.is_empty() {
        let measure = Measure::new()
            .with_chords(current_measure_chords)
            .with_time_signature(time_sig_tuple);
        measures.push(measure);
    }

    let section_name = if boundaries.is_some() {
        "Song"
    } else {
        "Detected Chords"
    };
    let section = Section::new(SectionType::Custom(section_name.to_string()));
    let chart_section = ChartSection::new(section).with_measures(measures);
    chart.sections.push(chart_section);

    chart
}

/// Parse a section type from a REAPER region name
///
/// Attempts to match common section names (Verse, Chorus, Bridge, etc.)
/// Falls back to Custom section type for unrecognized names.
fn parse_section_from_region_name(name: &str) -> Section {
    let name_trimmed = name.trim();

    // Try to parse using keyflow's section type parser
    if let Some(parsed) = SectionType::parse_with_measure_count(name_trimmed) {
        let mut section = Section::new(parsed.section_type);
        section.name = Some(name_trimmed.to_string());
        section.comment = parsed.comment;
        return section;
    }

    // Fallback: use the raw name as a Custom section
    let mut section = Section::new(SectionType::Custom(name_trimmed.to_string()));
    section.name = Some(name_trimmed.to_string());
    section
}
