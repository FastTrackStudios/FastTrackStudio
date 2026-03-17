//! Guide track generation — reads project regions and creates MIDI click/count/guide tracks.
//!
//! Bridges the keyflow guide algorithms with REAPER's project model by:
//! 1. Reading project regions (section markers)
//! 2. Parsing region names into `SectionType`
//! 3. Generating guide events via `GuideGenerator`
//! 4. Writing events as short MIDI items (1 measure each) to dedicated tracks in a folder
//!
//! Track structure:
//!   Click + Guide/        (folder)
//!     Click/              (subfolder)
//!       Click Native      (REAPER click source — auto-syncs with tempo/metronome)
//!     Loop                (empty, reserved)
//!     Count               (short MIDI items — 1 measure per count-in measure)
//!     Guide               (short MIDI items — 1 measure section cue)
//!
//! All REAPER interaction goes through `daw_reaper` sync helpers (which run on
//! the main thread, matching this action handler's execution context).

use actions_proto::ActionResult;
use daw::reaper::midi::{add_notes_to_take_on_main_thread, create_midi_item_on_main_thread};
use daw::reaper::region::get_regions_on_main_thread;
use daw::reaper::tempo_map::{
    get_tempo_and_time_sig_at_on_main_thread, qn_to_time_on_main_thread, time_to_qn_on_main_thread,
};
use daw::reaper::track::{add_track_on_main_thread, set_folder_depth_on_main_thread};
use daw::service::MidiNoteCreate;
use keyflow::midi::guide::GuideGenerator;
use keyflow::{ClickConfig, CountInConfig, GuideConfig, GuideEvent, SectionType, TimeSignature};
use reaper_high::Reaper;
use reaper_medium::{CommandId, ProjectContext};
use std::ffi::CString;
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
    click: String, // Click folder track — holds MIDI click items
    count: String,
    guide: String,
}

fn create_guide_tracks() -> Option<GuideTracks> {
    // Create track structure:
    //   Click + Guide/      (folder, depth +1)
    //     Click/            (subfolder, depth +1)
    //       Click Native    (depth -1, closes Click subfolder)
    //     Loop              (empty)
    //     Count             (MIDI items)
    //     Guide             (MIDI items, depth -1, closes main folder)
    let folder_guid = add_track_on_main_thread("Click + Guide", None)?;
    let click_folder_guid = add_track_on_main_thread("Click", None)?;
    let click_native_guid = add_track_on_main_thread("Click Native", None)?;
    let _loop_guid = add_track_on_main_thread("Loop", None)?;
    let count_guid = add_track_on_main_thread("Count", None)?;
    let guide_guid = add_track_on_main_thread("Guide", None)?;

    // Set folder structure
    set_folder_depth_on_main_thread(&folder_guid, 1).ok()?;
    set_folder_depth_on_main_thread(&click_folder_guid, 1).ok()?;
    set_folder_depth_on_main_thread(&click_native_guid, -1).ok()?; // closes Click subfolder
    set_folder_depth_on_main_thread(&guide_guid, -1).ok()?; // closes main folder

    // Insert REAPER's native click source on Click Native track
    insert_click_source(&click_native_guid);

    Some(GuideTracks {
        click: click_folder_guid,
        count: count_guid,
        guide: guide_guid,
    })
}

/// Insert REAPER's native click source on a track via actions.
///
/// Steps: unselect all tracks → select target → set as last touched →
/// insert click source → resize the item to cover the full project.
fn insert_click_source(track_guid: &str) {
    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let low = medium.low();

    // Resolve GUID to raw track pointer via reaper_high → index → low API
    let proj = reaper.current_project();
    let track = proj
        .tracks()
        .find(|t| t.guid().to_string_without_braces() == track_guid);
    let Some(track) = track else { return };
    let Some(index) = track.index() else { return };
    let raw_track = unsafe { low.GetTrack(std::ptr::null_mut(), index as i32) };
    if raw_track.is_null() {
        return;
    }

    unsafe {
        // Unselect all tracks (action 40297)
        medium.main_on_command_ex(CommandId::new(40297), 0, ProjectContext::CurrentProject);

        // Select our track
        let param = CString::new("I_SELECTED").unwrap();
        low.SetMediaTrackInfo_Value(raw_track, param.as_ptr(), 1.0);

        // Set as last touched track (action 40914)
        medium.main_on_command_ex(CommandId::new(40914), 0, ProjectContext::CurrentProject);

        // Insert click source (action 40013)
        medium.main_on_command_ex(CommandId::new(40013), 0, ProjectContext::CurrentProject);

        // Find the newly created item on this track and resize it
        let item_count = low.CountTrackMediaItems(raw_track);
        if item_count > 0 {
            let item = low.GetTrackMediaItem(raw_track, item_count - 1);
            if !item.is_null() {
                let pos_param = CString::new("D_POSITION").unwrap();
                let len_param = CString::new("D_LENGTH").unwrap();
                low.SetMediaItemInfo_Value(item, pos_param.as_ptr(), 0.0);
                low.SetMediaItemInfo_Value(item, len_param.as_ptr(), 600.0);
            }
        }

        // Unselect all tracks again to clean up
        medium.main_on_command_ex(CommandId::new(40297), 0, ProjectContext::CurrentProject);
    }
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

    let Some(take) = create_midi_item_on_main_thread(media_track, start_seconds, end_seconds)
    else {
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

            let count_in_start_seconds = qn_to_time_on_main_thread(count_in_start_qn);

            // Click MIDI: full-section item on the Click folder track
            let click_item_start = count_in_start_seconds.max(0.0);
            let click_item_end = region.end_seconds();
            if click_item_end > click_item_start && !click_notes.is_empty() {
                create_midi_item_with_notes(
                    &tracks.click,
                    click_item_start,
                    click_item_end,
                    &click_notes,
                );
            }

            // Count/Guide: short MIDI items (1 measure each)
            create_short_count_items(
                &tracks.count,
                count_in_start_qn,
                section_start_qn,
                measure_length_qn,
                &count_notes,
            );

            create_short_guide_items(
                &tracks.guide,
                count_in_start_qn,
                measure_length_qn,
                &guide_notes,
            );

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

/// Create short (1-measure) MIDI items on the Count track for each count-in measure.
fn create_short_count_items(
    track_guid: &str,
    count_in_start_qn: f64,
    section_start_qn: f64,
    measure_length_qn: f64,
    count_notes: &[(f64, u8, u8)],
) {
    if count_notes.is_empty() {
        return;
    }

    // Walk measure boundaries from count-in start to section start
    let mut measure_start_qn = count_in_start_qn;
    while measure_start_qn < section_start_qn {
        let measure_end_qn = (measure_start_qn + measure_length_qn).min(section_start_qn);

        // Collect notes that fall within this measure
        let notes_in_measure: Vec<(f64, u8, u8)> = count_notes
            .iter()
            .filter(|(pos_qn, _, _)| *pos_qn >= measure_start_qn && *pos_qn < measure_end_qn)
            .copied()
            .collect();

        if !notes_in_measure.is_empty() {
            let start_sec = qn_to_time_on_main_thread(measure_start_qn);
            let end_sec = qn_to_time_on_main_thread(measure_end_qn);
            if end_sec > start_sec {
                create_midi_item_with_notes(track_guid, start_sec, end_sec, &notes_in_measure);
            }
        }

        measure_start_qn += measure_length_qn;
    }
}

/// Create a short (1-measure) MIDI item on the Guide track for the section cue.
fn create_short_guide_items(
    track_guid: &str,
    count_in_start_qn: f64,
    measure_length_qn: f64,
    guide_notes: &[(f64, u8, u8)],
) {
    if guide_notes.is_empty() {
        return;
    }

    // Guide item: 1 measure starting at the count-in start
    let item_start_qn = count_in_start_qn;
    let item_end_qn = count_in_start_qn + measure_length_qn;

    let start_sec = qn_to_time_on_main_thread(item_start_qn).max(0.0);
    let end_sec = qn_to_time_on_main_thread(item_end_qn);

    if end_sec > start_sec {
        create_midi_item_with_notes(track_guid, start_sec, end_sec, guide_notes);
    }
}
