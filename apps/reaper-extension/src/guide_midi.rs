//! Live guide MIDI generation using direct REAPER API.
//!
//! Runs synchronously on the main thread using reaper_high/medium/low APIs
//! (same pattern as the original guide_track.rs, but cleaner).

use daw::reaper::midi::{add_notes_to_take_on_main_thread, create_midi_item_on_main_thread};
use daw::reaper::region::get_regions_on_main_thread;
use daw::reaper::tempo_map::get_tempo_and_time_sig_at_on_main_thread;
use daw::service::MidiNoteCreate;
use reaper_high::Reaper;
use tracing::info;

/// Fill missing guide MIDI on Click, Count, Guide tracks.
/// Runs synchronously on REAPER's main thread.
pub fn fill_guide_midi_sync() -> Result<(u32, u32), String> {
    let reaper = Reaper::get();
    let project = reaper.current_project();

    // Get regions
    let regions = get_regions_on_main_thread();
    let section_regions: Vec<_> = regions
        .iter()
        .filter(|r| {
            let upper = r.name.to_uppercase();
            !r.name.is_empty()
                && !matches!(
                    upper.as_str(),
                    "SONGSTART" | "SONGEND" | "=START" | "=END"
                        | "PREROLL" | "POSTROLL" | "COUNT-IN" | "COUNTIN"
                )
        })
        .collect();

    if section_regions.is_empty() {
        return Err("No section regions found".to_string());
    }

    info!("Guide gen: {} section regions", section_regions.len());

    // Find guide tracks
    let click_track = project.tracks().find(|t| {
        t.name().map(|n| n.to_str() == "Click").unwrap_or(false)
    });
    let count_track = project.tracks().find(|t| {
        t.name().map(|n| n.to_str() == "Count").unwrap_or(false)
    });
    let guide_track = project.tracks().find(|t| {
        t.name().map(|n| n.to_str() == "Guide").unwrap_or(false)
    });

    if click_track.is_none() && count_track.is_none() && guide_track.is_none() {
        return Err("No Click, Count, or Guide tracks found".to_string());
    }

    // Get item coverage
    let click_coverage = get_track_item_coverage(&click_track);
    let count_coverage = get_track_item_coverage(&count_track);
    let guide_coverage = get_track_item_coverage(&guide_track);

    let mut generated = 0u32;
    let mut skipped = 0u32;
    let mut prev_end = 0.0f64;

    for region in &section_regions {
        let start = region.start_seconds();
        let end = region.end_seconds();

        let has_click = click_track.is_none() || is_covered(start, end, &click_coverage);
        let has_count = count_track.is_none() || is_covered(start, end, &count_coverage);
        let has_guide = guide_track.is_none() || is_covered(start, end, &guide_coverage);

        if has_click && has_count && has_guide {
            prev_end = end;
            skipped += 1;
            continue;
        }

        let (bpm, num, _denom) = get_tempo_and_time_sig_at_on_main_thread(start);
        let beats_per_measure = num as u32;
        let beat_seconds = 60.0 / bpm;
        let measure_seconds = beats_per_measure as f64 * beat_seconds;

        let count_in_start = (start - measure_seconds).max(prev_end).max(0.0);

        // Click
        if !has_click {
            if let Some(ref track) = click_track {
                if let Ok(media_track) = track.raw() {
                    let mut notes = Vec::new();
                    let mut pos = count_in_start;
                    let start_qn = daw::reaper::tempo_map::time_to_qn_on_main_thread(count_in_start);
                    while pos < end - 0.001 {
                        let qn = daw::reaper::tempo_map::time_to_qn_on_main_thread(pos);
                        let beat_in_measure = ((pos - count_in_start) / beat_seconds).round() as u32
                            % beats_per_measure;
                        let (pitch, vel) = if beat_in_measure == 0 { (76u8, 120u8) } else { (77, 100) };
                        notes.push((qn, pitch, vel));
                        pos += beat_seconds;
                    }
                    if !notes.is_empty() {
                        create_and_add_notes(media_track, count_in_start, end, &notes);
                    }
                }
            }
        }

        // Count
        if !has_count && count_in_start < start - 0.001 {
            if let Some(ref track) = count_track {
                if let Ok(media_track) = track.raw() {
                    let mut notes = Vec::new();
                    let mut pos = count_in_start;
                    let mut beat_num = 1u32;
                    while pos < start - 0.001 && beat_num <= beats_per_measure {
                        let qn = daw::reaper::tempo_map::time_to_qn_on_main_thread(pos);
                        notes.push((qn, 60 + beat_num as u8, 100u8));
                        pos += beat_seconds;
                        beat_num += 1;
                    }
                    if !notes.is_empty() {
                        create_and_add_notes(media_track, count_in_start, start, &notes);
                    }
                }
            }
        }

        // Guide
        if !has_guide {
            if let Some(ref track) = guide_track {
                if let Ok(media_track) = track.raw() {
                    if let Some(pitch) = section_name_to_midi(&region.name) {
                        let qn = daw::reaper::tempo_map::time_to_qn_on_main_thread(count_in_start);
                        create_and_add_notes(
                            media_track,
                            count_in_start,
                            count_in_start + measure_seconds,
                            &[(qn, pitch, 127)],
                        );
                    }
                }
            }
        }

        prev_end = end;
        generated += 1;
    }

    Ok((generated, skipped))
}

fn create_and_add_notes(
    track: reaper_medium::MediaTrack,
    start: f64,
    end: f64,
    notes: &[(f64, u8, u8)], // (qn_position, pitch, velocity)
) {
    let Some(take) = create_midi_item_on_main_thread(track, start, end) else {
        return;
    };
    let midi_notes: Vec<MidiNoteCreate> = notes
        .iter()
        .map(|&(qn, pitch, vel)| MidiNoteCreate {
            channel: 0,
            pitch,
            velocity: vel,
            start_ppq: qn,
            length_ppq: 120.0,
        })
        .collect();
    add_notes_to_take_on_main_thread(take, &midi_notes);
}

fn get_track_item_coverage(track: &Option<reaper_high::Track>) -> Vec<(f64, f64)> {
    let Some(track) = track else { return Vec::new() };
    let low = Reaper::get().medium_reaper().low();
    let Some(index) = track.index() else { return Vec::new() };
    let raw = unsafe { low.GetTrack(std::ptr::null_mut(), index as i32) };
    if raw.is_null() { return Vec::new(); }

    let mut coverage = Vec::new();
    unsafe {
        let count = low.CountTrackMediaItems(raw);
        for i in 0..count {
            let item = low.GetTrackMediaItem(raw, i);
            if item.is_null() { continue; }
            let pos_p = std::ffi::CString::new("D_POSITION").unwrap();
            let len_p = std::ffi::CString::new("D_LENGTH").unwrap();
            let pos = low.GetMediaItemInfo_Value(item, pos_p.as_ptr());
            let len = low.GetMediaItemInfo_Value(item, len_p.as_ptr());
            coverage.push((pos, pos + len));
        }
    }
    coverage
}

fn is_covered(start: f64, end: f64, coverage: &[(f64, f64)]) -> bool {
    let len = end - start;
    if len <= 0.0 { return false; }
    coverage.iter().any(|&(is, ie)| {
        let overlap = (end.min(ie) - start.max(is)).max(0.0);
        overlap > len * 0.5
    })
}

fn section_name_to_midi(name: &str) -> Option<u8> {
    let w = name.split_whitespace().next().unwrap_or("").to_uppercase();
    match w.as_str() {
        "INTRO" => Some(36), "VS" | "VERSE" => Some(38),
        "PRE" | "PRECHORUS" | "PRE-CH" => Some(40),
        "CH" | "CHORUS" => Some(41), "BR" | "BRIDGE" => Some(43),
        "SOLO" => Some(45), "OUTRO" => Some(47),
        "BREAK" | "BREAKDOWN" => Some(48), "INTERLUDE" => Some(50),
        "INSTRUMENTAL" | "INST" => Some(52), "HITS" => Some(55),
        _ => Some(60),
    }
}
