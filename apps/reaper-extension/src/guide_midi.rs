//! Live guide MIDI generation using direct REAPER services.
//!
//! Uses `daw::reaper::*` services directly (like signal_save) to avoid
//! the RPC deadlock that occurs when calling `Daw::get()` from REAPER's
//! main thread. The Daw RPC facade routes through LocalCaller → roam →
//! main thread dispatch, which deadlocks when already on the main thread.

use daw::service::{
    ItemService, MidiNoteCreate, MidiService, ProjectContext, RegionService,
    TempoMapService, TrackRef, TrackService,
};
use tracing::{debug, info};

/// Fill missing guide MIDI using direct REAPER service calls.
pub async fn fill_guide_midi_direct(
    region_svc: &impl RegionService,
    track_svc: &impl TrackService,
    item_svc: &impl ItemService,
    midi_svc: &impl MidiService,
    tempo_svc: &impl TempoMapService,
) -> Result<(u32, u32), String> {
    let ctx = ProjectContext::Current;

    // Get all regions
    let regions = region_svc.get_regions(ctx.clone()).await;
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

    // Find guide tracks by name
    let all_tracks = track_svc.get_tracks(ctx.clone()).await;
    let click_guid = all_tracks.iter().find(|t| t.name == "Click").map(|t| t.guid.clone());
    let count_guid = all_tracks.iter().find(|t| t.name == "Count").map(|t| t.guid.clone());
    let guide_guid = all_tracks.iter().find(|t| t.name == "Guide").map(|t| t.guid.clone());

    if click_guid.is_none() && count_guid.is_none() && guide_guid.is_none() {
        return Err("No Click, Count, or Guide tracks found".to_string());
    }

    // Get item coverage on each track
    let click_coverage = get_coverage(item_svc, &ctx, click_guid.as_deref()).await;
    let count_coverage = get_coverage(item_svc, &ctx, count_guid.as_deref()).await;
    let guide_coverage = get_coverage(item_svc, &ctx, guide_guid.as_deref()).await;

    let mut generated = 0u32;
    let mut skipped = 0u32;
    let mut prev_end = 0.0f64;

    for region in &section_regions {
        let start = region.time_range.start_seconds();
        let end = region.time_range.end_seconds();

        let has_click = click_guid.is_none() || is_covered(start, end, &click_coverage);
        let has_count = count_guid.is_none() || is_covered(start, end, &count_coverage);
        let has_guide = guide_guid.is_none() || is_covered(start, end, &guide_coverage);

        if has_click && has_count && has_guide {
            debug!("SKIP: {} ({:.1}..{:.1}s)", region.name, start, end);
            prev_end = end;
            skipped += 1;
            continue;
        }

        info!("FILL: {} ({:.1}..{:.1}s)", region.name, start, end);

        // Get tempo
        let bpm = tempo_svc.get_tempo_at(ctx.clone(), start).await;
        let beats_per_measure = 4u32; // TODO: get time sig
        let beat_seconds = 60.0 / bpm;
        let measure_seconds = beats_per_measure as f64 * beat_seconds;

        let count_in_start = (start - measure_seconds).max(prev_end).max(0.0);

        // Click
        if !has_click {
            if let Some(ref guid) = click_guid {
                let mut notes = Vec::new();
                let mut pos = count_in_start;
                while pos < end - 0.001 {
                    let beat_in_measure = ((pos - count_in_start) / beat_seconds).round() as u32
                        % beats_per_measure;
                    let (pitch, vel) = if beat_in_measure == 0 { (76u8, 120u8) } else { (77, 100) };
                    notes.push(MidiNoteCreate::new(pitch, vel, pos * 960.0, 120.0));
                    pos += beat_seconds;
                }
                if !notes.is_empty() {
                    create_midi_with_notes(
                        midi_svc, &ctx, guid, count_in_start, end, notes,
                    ).await;
                }
            }
        }

        // Count
        if !has_count && count_in_start < start - 0.001 {
            if let Some(ref guid) = count_guid {
                let mut notes = Vec::new();
                let mut pos = count_in_start;
                let mut beat_num = 1u32;
                while pos < start - 0.001 && beat_num <= beats_per_measure {
                    notes.push(MidiNoteCreate::new(60 + beat_num as u8, 100, pos * 960.0, 120.0));
                    pos += beat_seconds;
                    beat_num += 1;
                }
                if !notes.is_empty() {
                    create_midi_with_notes(
                        midi_svc, &ctx, guid, count_in_start, start, notes,
                    ).await;
                }
            }
        }

        // Guide
        if !has_guide {
            if let Some(ref guid) = guide_guid {
                if let Some(pitch) = section_name_to_midi(&region.name) {
                    let notes = vec![MidiNoteCreate::new(pitch, 127, count_in_start * 960.0, 120.0)];
                    create_midi_with_notes(
                        midi_svc, &ctx, guid, count_in_start, count_in_start + measure_seconds, notes,
                    ).await;
                }
            }
        }

        prev_end = end;
        generated += 1;
    }

    Ok((generated, skipped))
}

async fn get_coverage(
    item_svc: &impl ItemService,
    ctx: &ProjectContext,
    guid: Option<&str>,
) -> Vec<(f64, f64)> {
    let Some(guid) = guid else { return Vec::new() };
    let items = item_svc
        .get_items(ctx.clone(), TrackRef::Guid(guid.to_string()))
        .await;
    items
        .iter()
        .map(|item| {
            let start = item.position.as_seconds();
            let end = start + item.length.as_seconds();
            (start, end)
        })
        .collect()
}

fn is_covered(start: f64, end: f64, coverage: &[(f64, f64)]) -> bool {
    let len = end - start;
    if len <= 0.0 { return false; }
    coverage.iter().any(|&(is, ie)| {
        let overlap = (end.min(ie) - start.max(is)).max(0.0);
        overlap > len * 0.5
    })
}

async fn create_midi_with_notes(
    midi_svc: &impl MidiService,
    ctx: &ProjectContext,
    track_guid: &str,
    start: f64,
    end: f64,
    notes: Vec<MidiNoteCreate>,
) {
    let location = midi_svc
        .create_midi_item(ctx.clone(), TrackRef::Guid(track_guid.to_string()), start, end)
        .await;
    if let Some(loc) = location {
        midi_svc.add_notes(loc, notes).await;
    }
}

fn section_name_to_midi(name: &str) -> Option<u8> {
    let w = name.split_whitespace().next().unwrap_or("").to_uppercase();
    match w.as_str() {
        "INTRO" => Some(36),
        "VS" | "VERSE" => Some(38),
        "PRE" | "PRECHORUS" | "PRE-CHORUS" | "PRE-CH" => Some(40),
        "CH" | "CHORUS" => Some(41),
        "BR" | "BRIDGE" => Some(43),
        "SOLO" => Some(45),
        "OUTRO" => Some(47),
        "BREAK" | "BREAKDOWN" => Some(48),
        "INTERLUDE" => Some(50),
        "INSTRUMENTAL" => Some(52),
        "TAG" | "VAMP" => Some(53),
        "HITS" => Some(55),
        _ => Some(60),
    }
}
