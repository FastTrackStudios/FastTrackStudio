//! Guide track generation — generates Click, Count, and Guide MIDI items
//! from project regions using the Daw facade.
//!
//! Works with both single-song projects and combined setlists:
//! - Finds existing Click, Count, Guide tracks (does not create new ones)
//! - Scans existing items to detect which regions already have coverage
//! - Only generates MIDI for uncovered regions (safe to run multiple times)

// r[impl organize.guide.click]
// r[impl organize.guide.count]
// r[impl organize.guide.section-cue]

use daw::Daw;
use session_proto::SessionServiceError;
use tracing::{debug, info};

/// Generate guide MIDI for all uncovered regions in the current project.
///
/// Returns the number of regions generated and skipped.
pub async fn fill_guide_midi() -> Result<(u32, u32), SessionServiceError> {
    let daw = Daw::get();
    let project = daw.current_project().await.map_err(map_daw)?;

    // Get all regions (section markers)
    let regions = project.regions().all().await.map_err(map_daw)?;
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
        return Err(SessionServiceError::Internal(
            "No section regions found in project".to_string(),
        ));
    }

    info!("Guide gen: {} section regions found", section_regions.len());

    // Find guide tracks (must already exist in the project)
    let click_track = find_track(&project, "Click").await;
    let count_track = find_track(&project, "Count").await;
    let guide_track = find_track(&project, "Guide").await;

    if click_track.is_none() && count_track.is_none() && guide_track.is_none() {
        return Err(SessionServiceError::Internal(
            "No Click, Count, or Guide tracks found. Run Organize Tracks first.".to_string(),
        ));
    }

    // Get existing item coverage on each track
    let click_coverage = match &click_track {
        Some(t) => get_item_coverage(t).await,
        None => Vec::new(),
    };
    let count_coverage = match &count_track {
        Some(t) => get_item_coverage(t).await,
        None => Vec::new(),
    };
    let guide_coverage = match &guide_track {
        Some(t) => get_item_coverage(t).await,
        None => Vec::new(),
    };

    let mut generated = 0u32;
    let mut skipped = 0u32;
    let mut prev_region_end = 0.0f64;

    for region in &section_regions {
        let region_start = region.time_range.start_seconds();
        let region_end = region.time_range.end_seconds();

        // Check coverage
        let has_click = click_track.is_none() || is_covered(region_start, region_end, &click_coverage);
        let has_count = count_track.is_none() || is_covered(region_start, region_end, &count_coverage);
        let has_guide = guide_track.is_none() || is_covered(region_start, region_end, &guide_coverage);

        if has_click && has_count && has_guide {
            debug!("SKIP: {} ({:.1}..{:.1}s)", region.name, region_start, region_end);
            prev_region_end = region_end;
            skipped += 1;
            continue;
        }

        info!(
            "FILL: {} ({:.1}..{:.1}s) click={} count={} guide={}",
            region.name, region_start, region_end, !has_click, !has_count, !has_guide
        );

        // Get tempo at region start
        let bpm = project
            .tempo_map()
            .tempo_at(region_start)
            .await
            .unwrap_or(120.0);
        let beats_per_measure = 4u32; // TODO: get time signature from tempo map

        let beat_seconds = 60.0 / bpm;
        let measure_seconds = beats_per_measure as f64 * beat_seconds;

        // Count-in: one measure before section, clamped
        let count_in_start = (region_start - measure_seconds).max(prev_region_end).max(0.0);

        // Click: full section from count-in to end
        if !has_click {
            if let Some(ref track) = click_track {
                let mut notes = Vec::new();
                let mut pos = count_in_start;
                while pos < region_end - 0.001 {
                    let beat_in_measure = ((pos - count_in_start) / beat_seconds).round() as u32
                        % beats_per_measure;
                    let (pitch, vel) = if beat_in_measure == 0 {
                        (76u8, 120u8)
                    } else {
                        (77u8, 100u8)
                    };
                    notes.push(daw::service::MidiNoteCreate::new(pitch, vel, pos * 960.0, 120.0));
                    pos += beat_seconds;
                }
                if !notes.is_empty() {
                    let _ = track
                        .items()
                        .create_midi_item_with_notes(count_in_start, region_end, notes)
                        .await;
                }
            }
        }

        // Count: one measure of count-in beats
        if !has_count && count_in_start < region_start - 0.001 {
            if let Some(ref track) = count_track {
                let mut notes = Vec::new();
                let mut pos = count_in_start;
                let mut beat_num = 1u32;
                while pos < region_start - 0.001 && beat_num <= beats_per_measure {
                    let pitch = 60 + beat_num as u8;
                    notes.push(daw::service::MidiNoteCreate::new(pitch, 100, pos * 960.0, 120.0));
                    pos += beat_seconds;
                    beat_num += 1;
                }
                if !notes.is_empty() {
                    let _ = track
                        .items()
                        .create_midi_item_with_notes(count_in_start, region_start, notes)
                        .await;
                }
            }
        }

        // Guide: section cue note
        if !has_guide {
            if let Some(ref track) = guide_track {
                if let Some(pitch) = section_name_to_midi(&region.name) {
                    let notes = vec![daw::service::MidiNoteCreate::new(
                        pitch,
                        127,
                        count_in_start * 960.0,
                        120.0,
                    )];
                    let _ = track
                        .items()
                        .create_midi_item_with_notes(
                            count_in_start,
                            count_in_start + measure_seconds,
                            notes,
                        )
                        .await;
                }
            }
        }

        prev_region_end = region_end;
        generated += 1;
    }

    info!(
        "Guide MIDI: {} regions generated, {} skipped (already covered)",
        generated, skipped
    );

    Ok((generated, skipped))
}

// ── Helpers ──────────────────────────────────────────────────────────────────

fn map_daw(e: daw::Error) -> SessionServiceError {
    SessionServiceError::DawError(e.to_string())
}

/// Find a track by name (returns None if not found).
async fn find_track(
    project: &daw::Project,
    name: &str,
) -> Option<daw::TrackHandle> {
    project.get_track_by_name(name).await.ok().flatten()
}

/// Get time ranges covered by items on a track.
async fn get_item_coverage(track: &daw::TrackHandle) -> Vec<(f64, f64)> {
    let items = match track.items().all().await {
        Ok(items) => items,
        Err(_) => return Vec::new(),
    };

    items
        .iter()
        .map(|item| {
            let start = item.position.as_seconds();
            let end = start + item.length.as_seconds();
            (start, end)
        })
        .collect()
}

/// Check if a time range is covered by existing items (50%+ overlap).
fn is_covered(start: f64, end: f64, coverage: &[(f64, f64)]) -> bool {
    let range_len = end - start;
    if range_len <= 0.0 {
        return false;
    }
    for &(item_start, item_end) in coverage {
        let overlap_start = start.max(item_start);
        let overlap_end = end.min(item_end);
        let overlap = (overlap_end - overlap_start).max(0.0);
        if overlap > range_len * 0.5 {
            return true;
        }
    }
    false
}

/// Map section name to MIDI note for guide cue.
fn section_name_to_midi(name: &str) -> Option<u8> {
    let first_word = name.split_whitespace().next().unwrap_or("").to_uppercase();
    match first_word.as_str() {
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
