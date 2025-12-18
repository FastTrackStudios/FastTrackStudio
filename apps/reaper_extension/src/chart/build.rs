//! Build Chart from detected chords

use super::detection::DetectedChord;
use super::read;
use keyflow::chart::{Chart, ChartSection, ChordInstance, Measure};
use keyflow::chord::ChordRhythm;
use keyflow::key::Key;
use keyflow::primitives::RootNotation;
use keyflow::sections::{Section, SectionType};
use keyflow::time::{AbsolutePosition, MusicalDuration, MusicalPosition, PPQDuration, PPQPosition, Tempo, TimeSignature};
use reaper_high::{Project, Reaper};
use reaper_medium::{MediaItemTake, ProjectRef};
use fts::setlist::infra::traits::SetlistBuilder;
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
    
    // Get time signature from REAPER project using the same logic as setlist service
    // Use the setlist building function which already handles tempo/time signature reading
    // We'll get the time signature from the song's starting_time_signature if available
    let time_sig = {
        // Try to get time signature from setlist (which reads from tempo/time sig markers)
        // Use the trait method directly on the Reaper instance (operates on all open projects)
        let reaper = Reaper::get();
        if let Ok(setlist) = reaper.build_setlist_from_open_projects(None) {
            // Find the current song by project name (reuse project_name from above)
            if let Some(ref name) = project_name {
                if let Some(song) = setlist.songs.iter().find(|s| s.name == *name) {
                    if let Some(ts) = song.starting_time_signature {
                        tracing::debug!(
                            "Using time signature from setlist song '{}': {}/{}",
                            name,
                            ts.numerator,
                            ts.denominator
                        );
                        ts
                    } else {
                        TimeSignature::new(4, 4) // Fallback to 4/4
                    }
                } else {
                    TimeSignature::new(4, 4) // Fallback to 4/4
                }
            } else {
                TimeSignature::new(4, 4) // Fallback to 4/4
            }
        } else {
            TimeSignature::new(4, 4) // Fallback to 4/4
        }
    };
    
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
    
    // Get setlist and find current song
    // Use the trait method directly on the Reaper instance (operates on all open projects)
    let reaper = Reaper::get();
    let song_sections = if let Ok(setlist) = reaper.build_setlist_from_open_projects(None) {
        project_name.and_then(|name| {
            setlist.songs.iter().find(|song| {
                song.name == name || song.name.contains(&name) || name.contains(&song.name)
            }).map(|song| song.sections.clone())
        })
    } else {
        None
    };
    
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
    let time_sig = TimeSignature { numerator: 4, denominator: 4 };
    let beats_per_measure = time_sig.numerator as f64;
    
    // Convert PPQ to MusicalPosition using REAPER's functions
    // First convert PPQ to quarter notes, then to musical position
    let ppq_to_musical_position = |ppq: i64| -> MusicalPosition {
        if let Some(take) = midi_take {
            let reaper = Reaper::get();
            let low_reaper = reaper.medium_reaper().low();
            // Convert PPQ to quarter notes using REAPER
            let quarter_notes = unsafe { low_reaper.MIDI_GetProjQNFromPPQPos(take.as_ptr(), ppq as f64) };
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
            let start_qn = unsafe { low_reaper.MIDI_GetProjQNFromPPQPos(take.as_ptr(), start_ppq as f64) };
            let end_qn = unsafe { low_reaper.MIDI_GetProjQNFromPPQPos(take.as_ptr(), end_ppq as f64) };
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
    
    // If we have sections from the setlist, organize chords by section
    if let Some(sections) = song_sections {
        // Group chords by section based on time position
        for setlist_section in sections {
            let section_start = setlist_section.start_seconds().unwrap_or(0.0);
            let section_end = setlist_section.end_seconds().unwrap_or(f64::MAX);
            
            // Log section boundaries for debugging
            tracing::debug!(
                "Section '{}': start={:.3}s, end={:.3}s",
                setlist_section.display_name(),
                section_start,
                section_end
            );
            
            // Find all chords that are within this section's time range
            // A chord is in the section if:
            // - It starts at or after the section starts AND
            // - It starts before the section ends
            // This excludes chords that start before the section (even if they extend into it)
            let section_chords: Vec<_> = chords_with_time.iter()
                .filter(|cwt| {
                    let in_section = cwt.time_seconds >= section_start && cwt.time_seconds < section_end;
                    if in_section {
                        tracing::debug!(
                            "  Chord {} ({} - {:.3}s) is in section '{}' ({:.3}s - {:.3}s)",
                            cwt.instance.full_symbol,
                            cwt.time_seconds,
                            cwt.end_time_seconds,
                            setlist_section.display_name(),
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
                // Convert section start time to PPQ
                unsafe { low_reaper.MIDI_GetPPQPosFromProjTime(take.as_ptr(), section_start) as i64 }
            } else {
                // Fallback: manual calculation
                (section_start / (60.0 / bpm) * ppq_resolution) as i64
            };
            
            // Get the musical position of the section start to use as offset
            let section_start_position = ppq_to_musical_position(section_start_ppq);
            let section_start_measure = section_start_position.measure;
            
            // Organize chords into measures for this section
            // Measure 1.0 should be at the section start, so we offset by (section_start_measure - 1)
            // This means if section starts at measure 5, offset = 4, so measure 5 becomes measure 1
            let measure_offset = section_start_measure - 1;
            let mut measures: Vec<Measure> = Vec::new();
            let mut current_measure_chords: Vec<ChordInstance> = Vec::new();
            let mut current_measure_index: Option<i32> = None;
            
            // Get the time signature for this section (use chart's time signature or default to 4/4)
            let time_sig_tuple = chart.time_signature
                .map(|ts| (ts.numerator as u8, ts.denominator as u8))
                .unwrap_or((4, 4));
            
            tracing::debug!(
                "Building measures for section '{}' with time signature: {}/{}",
                setlist_section.display_name(),
                time_sig_tuple.0,
                time_sig_tuple.1
            );
            
            for chord_with_time in section_chords {
                // Calculate measure relative to section start
                // Measure 1.0 should be at the section start
                let chord_measure_absolute = chord_with_time.position.measure;
                let mut chord_measure_relative = chord_measure_absolute - measure_offset;
                
                // Ensure relative measure is at least 1 (measure 1.0 is section start)
                chord_measure_relative = chord_measure_relative.max(1);
                
                // If we've moved to a new measure, finalize the current one
                if let Some(measure_idx) = current_measure_index {
                    if chord_measure_relative > measure_idx {
                        // Add any remaining chords to current measure
                        if !current_measure_chords.is_empty() {
                            let measure = Measure::new()
                                .with_chords(current_measure_chords.clone())
                                .with_time_signature(time_sig_tuple);
                            measures.push(measure);
                        }
                        
                        // Start new measure(s) if there's a gap
                        let mut gap_measure = measure_idx + 1;
                        while gap_measure < chord_measure_relative {
                            measures.push(Measure::new().with_time_signature(time_sig_tuple));
                            gap_measure += 1;
                        }
                        
                        current_measure_chords = Vec::new();
                        current_measure_index = Some(chord_measure_relative);
                    }
                } else {
                    // First chord in section
                    // Measure 1.0 should be at section start
                    // If the first chord is not at measure 1, add empty measures before it
                    if chord_measure_relative > 1 {
                        // Add empty measures from 1 to chord_measure_relative - 1
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
    
            // Create chart section from setlist section
            let chart_section = ChartSection::new(setlist_section.clone()).with_measures(measures);
            chart.sections.push(chart_section);
        }
    } else {
        // Fallback: if no sections found, create a single "Detected Chords" section
        // Get the time signature for the fallback section
        let time_sig_tuple = chart.time_signature
            .map(|ts| (ts.numerator as u8, ts.denominator as u8))
            .unwrap_or((4, 4));
        
        tracing::debug!(
            "Building fallback section with time signature: {}/{}",
            time_sig_tuple.0,
            time_sig_tuple.1
        );
        
        // Organize all chords into measures
        let mut measures: Vec<Measure> = Vec::new();
        let mut current_measure_chords: Vec<ChordInstance> = Vec::new();
        let mut current_measure_index: Option<usize> = None;
        
        for chord_with_time in &chords_with_time {
            let chord_measure = (chord_with_time.ppq_position.ppq / ticks_per_measure) as usize;
            
            if let Some(measure_idx) = current_measure_index {
                if chord_measure > measure_idx {
                    if !current_measure_chords.is_empty() {
                        let measure = Measure::new()
                            .with_chords(current_measure_chords.clone())
                            .with_time_signature(time_sig_tuple);
                        measures.push(measure);
                    }
                    
                    let mut gap_measure = measure_idx + 1;
                    while gap_measure < chord_measure {
                        measures.push(Measure::new().with_time_signature(time_sig_tuple));
                        gap_measure += 1;
                    }
                    
                    current_measure_chords = Vec::new();
                    current_measure_index = Some(chord_measure);
                }
            } else {
                current_measure_index = Some(chord_measure);
            }
            
            current_measure_chords.push(chord_with_time.instance.clone());
        }
        
        if !current_measure_chords.is_empty() {
            let measure = Measure::new()
                .with_chords(current_measure_chords)
                .with_time_signature(time_sig_tuple);
            measures.push(measure);
        }
        
    let section = Section::new(SectionType::Custom("Detected Chords".to_string()));
    let chart_section = ChartSection::new(section).with_measures(measures);
    chart.sections.push(chart_section);
    }
    
    chart
}

