//! Guide track generation — reads project regions and creates MIDI click/count/guide tracks.
//!
//! Bridges the keyflow guide algorithms with REAPER's project model by:
//! 1. Reading project regions (section markers)
//! 2. Parsing region names into `SectionType`
//! 3. Generating guide events via `GuideGenerator`
//! 4. Writing events as MIDI notes to 3 dedicated tracks in a folder
//!
//! All REAPER interaction goes through `daw_reaper` sync helpers (which run on
//! the main thread, matching this action handler's execution context).

use actions_proto::ActionResult;
use daw::service::MidiNoteCreate;
use daw::reaper::midi::{add_notes_to_take_on_main_thread, create_midi_item_on_main_thread};
use daw::reaper::region::get_regions_on_main_thread;
use daw::reaper::tempo_map::{
    get_tempo_and_time_sig_at_on_main_thread, qn_to_time_on_main_thread, time_to_qn_on_main_thread,
};
use daw::reaper::track::{add_track_on_main_thread, set_folder_depth_on_main_thread};
use keyflow::midi::guide::GuideGenerator;
use keyflow::{ClickConfig, CountInConfig, GuideConfig, GuideEvent, SectionType, TimeSignature};
use reaper_high::Reaper;
use tracing::info;

// ─── Section name parsing ────────────────────────────────────────────────────

/// Parse a region name like "Verse 1", "Chorus", "Bridge 2" into a SectionType + optional number.
fn parse_section_name(name: &str) -> (SectionType, Option<u32>) {
    let trimmed = name.trim();

    // Split into words, check if the last word is a number
    let parts: Vec<&str> = trimmed.split_whitespace().collect();
    if parts.is_empty() {
        return (SectionType::Custom(trimmed.to_string()), None);
    }

    // Try to extract a trailing number: "Verse 1" → ("Verse", Some(1))
    let (type_str, number) = if parts.len() >= 2 {
        if let Ok(n) = parts.last().unwrap().parse::<u32>() {
            let type_part = parts[..parts.len() - 1].join(" ");
            (type_part, Some(n))
        } else {
            (trimmed.to_string(), None)
        }
    } else {
        (trimmed.to_string(), None)
    };

    match SectionType::parse(&type_str) {
        Ok(section_type) => (section_type, number),
        Err(_) => (SectionType::Custom(type_str), number),
    }
}

// ─── Track creation ──────────────────────────────────────────────────────────

/// GUID-based references to the guide tracks.
struct GuideTracks {
    click: String,
    count: String,
    guide: String,
}

fn create_guide_tracks() -> Option<GuideTracks> {
    // Create 5 tracks: folder + Click + Loop + Count + Guide
    let folder_guid = add_track_on_main_thread("Click + Guide", None)?;
    let click_guid = add_track_on_main_thread("Click", None)?;
    let _loop_guid = add_track_on_main_thread("Loop", None)?;
    let count_guid = add_track_on_main_thread("Count", None)?;
    let guide_guid = add_track_on_main_thread("Guide", None)?;

    // Set folder structure: parent = +1, last child = -1
    set_folder_depth_on_main_thread(&folder_guid, 1).ok()?;
    set_folder_depth_on_main_thread(&guide_guid, -1).ok()?;

    Some(GuideTracks {
        click: click_guid,
        count: count_guid,
        guide: guide_guid,
    })
}

// ─── MIDI item + note writing ────────────────────────────────────────────────

fn create_midi_item_with_notes(
    track_guid: &str,
    start_seconds: f64,
    end_seconds: f64,
    notes: &[(f64, u8, u8)], // (position_qn, midi_note, velocity)
) {
    // Resolve the track GUID to a raw pointer via reaper_high
    let reaper = Reaper::get();
    let proj = reaper.current_project();
    let track = proj
        .tracks()
        .find(|t| t.guid().to_string_without_braces() == track_guid);
    let Some(track) = track else { return };
    let Ok(media_track) = track.raw() else {
        return;
    };

    let Some(take) = create_midi_item_on_main_thread(media_track, start_seconds, end_seconds) else {
        return;
    };

    // Convert (qn, note, vel) tuples to MidiNoteCreate
    // start_ppq field is used as project quarter-note position (converted to PPQ
    // inside add_notes_to_take_on_main_thread via MIDI_GetPPQPosFromProjQN).
    let midi_notes: Vec<MidiNoteCreate> = notes
        .iter()
        .map(|&(pos_qn, note, vel)| MidiNoteCreate {
            channel: 0,
            pitch: note,
            velocity: vel,
            start_ppq: pos_qn, // project QN position (converted to PPQ by helper)
            length_ppq: 120.0, // short trigger note (~eighth note at 960 PPQ)
        })
        .collect();

    add_notes_to_take_on_main_thread(take, &midi_notes);
}

// ─── Main orchestrator ───────────────────────────────────────────────────────

pub fn generate_guide_tracks() -> ActionResult {
    let regions = get_regions_on_main_thread();
    if regions.is_empty() {
        return ActionResult::failure("No regions found in project");
    }

    // Wrap everything in an undo block
    let reaper = Reaper::get();
    let project = reaper.current_project();
    let result: Option<String> = project.undoable("Generate Guide Tracks", || {
        let tracks = create_guide_tracks()?;

        let click_config = ClickConfig::default();
        let count_in_config = CountInConfig::default();
        let guide_config = GuideConfig::default();

        let mut prev_region_end_seconds: Option<f64> = None;

        for region in &regions {
            let (section_type, section_number) = parse_section_name(&region.name);
            let (tempo, num, denom) =
                get_tempo_and_time_sig_at_on_main_thread(region.start_seconds());
            let time_sig = TimeSignature::new(num as u32, denom as u32);

            let section_start_qn = time_to_qn_on_main_thread(region.start_seconds());
            let section_end_qn = time_to_qn_on_main_thread(region.end_seconds());

            // Compute count-in: one measure before the section start,
            // clamped so it doesn't overlap the previous region
            let beat_unit = 4.0 / time_sig.denominator as f64;
            let measure_length_qn = beat_unit * time_sig.numerator as f64;
            let mut count_in_start_qn = section_start_qn - measure_length_qn;

            if let Some(prev_end) = prev_region_end_seconds {
                let prev_end_qn = time_to_qn_on_main_thread(prev_end);
                if count_in_start_qn < prev_end_qn {
                    count_in_start_qn = prev_end_qn;
                }
            }

            let count_in_start_seconds = qn_to_time_on_main_thread(count_in_start_qn);

            let events = GuideGenerator::generate_section(
                section_start_qn,
                section_end_qn,
                count_in_start_qn,
                &time_sig,
                tempo,
                &section_type,
                section_number,
                &click_config,
                &count_in_config,
                &guide_config,
            );

            // Partition events by type
            let mut click_notes: Vec<(f64, u8, u8)> = Vec::new();
            let mut count_notes: Vec<(f64, u8, u8)> = Vec::new();
            let mut guide_notes: Vec<(f64, u8, u8)> = Vec::new();

            for event in &events {
                match event {
                    GuideEvent::Click(e) => {
                        click_notes.push((e.position_quarters, e.midi_note, e.velocity));
                    }
                    GuideEvent::Count(e) => {
                        count_notes.push((e.position_quarters, e.midi_note, 100));
                    }
                    GuideEvent::SectionCue(e) => {
                        guide_notes.push((e.position_quarters, e.midi_note, 127));
                    }
                }
            }

            // MIDI item spans from count-in start to region end (clamped to project start)
            let item_start = count_in_start_seconds.max(0.0);
            let item_end = region.end_seconds();

            // Only create items if there are notes and the item has positive length
            if item_end > item_start {
                if !click_notes.is_empty() {
                    create_midi_item_with_notes(&tracks.click, item_start, item_end, &click_notes);
                }
                if !count_notes.is_empty() {
                    create_midi_item_with_notes(&tracks.count, item_start, item_end, &count_notes);
                }
                if !guide_notes.is_empty() {
                    create_midi_item_with_notes(&tracks.guide, item_start, item_end, &guide_notes);
                }
            }

            prev_region_end_seconds = Some(region.end_seconds());
        }

        Some(format!(
            "Generated guide tracks for {} regions",
            regions.len()
        ))
    });

    match result {
        Some(msg) => {
            info!("{}", msg);
            ActionResult::success_with_message(msg)
        }
        None => ActionResult::failure("Failed to create guide tracks"),
    }
}
