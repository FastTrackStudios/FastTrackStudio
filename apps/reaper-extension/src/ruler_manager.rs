//! Ruler Manager — scaffolds and manages FTS ruler lanes in REAPER projects.
//!
//! Provides actions for:
//! - Generating the standard FTS ruler lane layout
//! - Inserting sections, structural markers, bound markers, key/mode/chord markers
//! - Inserting per-instrument notes into dedicated lanes

use reaper_high::Reaper;
use reaper_medium::{CommandId, ProjectContext::CurrentProject};
use session_proto::ruler_lanes::{CoreLane, FtsLane, InstrumentLane};
use std::ffi::CString;
use tracing::info;

// ── Ruler Lane Management ────────────────────────────────────────────────────

/// Set a ruler lane's name using `GetSetProjectInfo_String`.
fn set_lane_name(lane_index: u32, name: &str) {
    let reaper = Reaper::get().medium_reaper();
    let low = reaper.low();
    let key = CString::new(format!("RULER_LANE_NAME:{}", lane_index)).unwrap();
    let value = CString::new(name).unwrap();
    unsafe {
        low.GetSetProjectInfo_String(
            std::ptr::null_mut(),
            key.as_ptr(),
            value.as_ptr() as *mut _,
            true,
        );
    }
}

/// Insert a new ruler lane at the end and return its position index.
fn insert_lane() -> u32 {
    let reaper = Reaper::get().medium_reaper();
    let low = reaper.low();
    // RULER_LANE_ORDER:-1 inserts a new lane
    let key = CString::new("RULER_LANE_ORDER:-1").unwrap();
    let value = CString::new("0").unwrap();
    unsafe {
        low.GetSetProjectInfo_String(
            std::ptr::null_mut(),
            key.as_ptr(),
            value.as_ptr() as *mut _,
            true,
        );
    }
    // Return the index of the newly created lane
    // Lanes are 0-based in the ORDER API but 1-based in NAME API
    get_lane_count()
}

/// Get the number of ruler lanes by probing names until empty.
fn get_lane_count() -> u32 {
    let reaper = Reaper::get().medium_reaper();
    let low = reaper.low();
    for i in 1..=64 {
        let key = CString::new(format!("RULER_LANE_NAME:{}", i)).unwrap();
        let mut buf = [0u8; 256];
        let ok = unsafe {
            low.GetSetProjectInfo_String(
                std::ptr::null_mut(),
                key.as_ptr(),
                buf.as_mut_ptr() as *mut _,
                false,
            )
        };
        if !ok {
            return i - 1;
        }
        let name = unsafe { std::ffi::CStr::from_ptr(buf.as_ptr() as *const _) }
            .to_string_lossy();
        if name.is_empty() {
            return i - 1;
        }
    }
    64
}

/// Get the name of a ruler lane, or None if it doesn't exist.
fn get_lane_name(lane_index: u32) -> Option<String> {
    let reaper = Reaper::get().medium_reaper();
    let low = reaper.low();
    let key = CString::new(format!("RULER_LANE_NAME:{}", lane_index)).unwrap();
    let mut buf = [0u8; 256];
    let ok = unsafe {
        low.GetSetProjectInfo_String(
            std::ptr::null_mut(),
            key.as_ptr(),
            buf.as_mut_ptr() as *mut _,
            false,
        )
    };
    if !ok {
        return None;
    }
    let name = unsafe { std::ffi::CStr::from_ptr(buf.as_ptr() as *const _) }
        .to_string_lossy()
        .into_owned();
    if name.is_empty() { None } else { Some(name) }
}

/// Check if the FTS ruler lanes are already scaffolded.
pub fn is_scaffolded() -> bool {
    // Check if lane 1 is named "SECTIONS"
    get_lane_name(1)
        .map(|n| n == "SECTIONS")
        .unwrap_or(false)
}

/// Generate the standard FTS ruler lane layout.
///
/// Creates all 8 core lanes with correct names and flags.
/// If lanes already exist, updates their names.
pub fn scaffold_core_lanes() {
    info!("Scaffolding FTS ruler lanes...");

    for lane in CoreLane::all() {
        let idx = lane.lane_index();

        // Ensure the lane exists by checking if we can read it
        if get_lane_name(idx).is_none() {
            insert_lane();
        }

        set_lane_name(idx, lane.display_name());
    }

    // Force REAPER to refresh the ruler display
    let reaper = Reaper::get().medium_reaper();
    unsafe { reaper.low().UpdateTimeline() };

    info!("FTS ruler lanes scaffolded: {} core lanes", CoreLane::count());
}

/// Ensure an instrument note lane exists, creating it if needed.
pub fn ensure_instrument_lane(instrument: InstrumentLane) {
    let idx = instrument.lane_index();

    if get_lane_name(idx).is_some() {
        // Already exists — make sure name is correct
        set_lane_name(idx, instrument.display_name());
        return;
    }

    // Need to create lanes up to this index
    let current_count = get_lane_count();
    for _ in current_count..idx {
        insert_lane();
    }
    set_lane_name(idx, instrument.display_name());

    let reaper = Reaper::get().medium_reaper();
    unsafe { reaper.low().UpdateTimeline() };
}

// ── Marker/Region Insertion ──────────────────────────────────────────────────

/// Insert a marker at the edit cursor position in the specified lane.
fn insert_marker_at_cursor(name: &str, lane: FtsLane) {
    let reaper = Reaper::get().medium_reaper();
    let low = reaper.low();

    let cursor_pos = unsafe { low.GetCursorPosition() };
    let c_name = CString::new(name).unwrap_or_default();

    // Add marker
    let marker_idx = unsafe {
        low.AddProjectMarker2(
            std::ptr::null_mut(),
            false, // is_region = false
            cursor_pos,
            0.0, // end (unused for markers)
            c_name.as_ptr(),
            -1, // auto-number
            0,  // color (default)
        )
    };

    // Set the lane
    if marker_idx >= 0 {
        // Find the enumeration index of the marker we just created
        // The marker_idx from AddProjectMarker2 is the marker *number*, not enum index
        set_last_marker_lane(lane.lane_index());
    }

    info!("Inserted marker '{}' at {:.2}s in lane {} ({})",
        name, cursor_pos, lane.lane_index(), lane.display_name());
}

/// Insert a region from the time selection in the specified lane.
fn insert_region_from_selection(name: &str, lane: FtsLane) {
    let reaper = Reaper::get().medium_reaper();
    let low = reaper.low();

    let mut start: f64 = 0.0;
    let mut end: f64 = 0.0;
    unsafe {
        low.GetSet_LoopTimeRange(false, false, &mut start, &mut end, false);
    }

    if (end - start).abs() < 0.001 {
        // No time selection — use a default 4-beat region from cursor
        let cursor = unsafe { low.GetCursorPosition() };
        let tempo = unsafe { low.Master_GetTempo() };
        start = cursor;
        end = cursor + (4.0 * 60.0 / tempo); // 4 beats
    }

    let c_name = CString::new(name).unwrap_or_default();

    let region_idx = unsafe {
        low.AddProjectMarker2(
            std::ptr::null_mut(),
            true, // is_region = true
            start,
            end,
            c_name.as_ptr(),
            -1,
            0,
        )
    };

    if region_idx >= 0 {
        set_last_marker_lane(lane.lane_index());
    }

    info!("Inserted region '{}' ({:.2}s → {:.2}s) in lane {} ({})",
        name, start, end, lane.lane_index(), lane.display_name());
}

/// Set the lane of the most recently added marker/region.
///
/// After `AddProjectMarker2`, the new marker is the last in the enumeration.
/// We find it and set its lane via the ruler lane API.
fn set_last_marker_lane(lane: u32) {
    let reaper = Reaper::get().medium_reaper();
    let low = reaper.low();

    let count = unsafe {
        low.CountProjectMarkers(
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        )
    };

    if count > 0 {
        daw::reaper::safe_wrappers::ruler_lanes::set_marker_lane(
            low,
            reaper_medium::ProjectContext::CurrentProject,
            (count - 1) as u32,
            lane,
        );
    }
}

// ── Action Handlers ──────────────────────────────────────────────────────────

use actions_proto::ActionResult;

// Generate FTS Ruler Lanes
pub fn handle_generate_ruler_lanes() -> ActionResult {
    scaffold_core_lanes();
    ActionResult::success_with_message("FTS ruler lanes generated")
}

// ── Section Actions (SECTIONS lane — regions from time selection) ─────────

pub fn handle_insert_section_verse() -> ActionResult {
    insert_region_from_selection("VS", FtsLane::Core(CoreLane::Sections));
    ActionResult::success_with_message("Inserted Verse section")
}

pub fn handle_insert_section_chorus() -> ActionResult {
    insert_region_from_selection("CH", FtsLane::Core(CoreLane::Sections));
    ActionResult::success_with_message("Inserted Chorus section")
}

pub fn handle_insert_section_bridge() -> ActionResult {
    insert_region_from_selection("BR", FtsLane::Core(CoreLane::Sections));
    ActionResult::success_with_message("Inserted Bridge section")
}

pub fn handle_insert_section_intro() -> ActionResult {
    insert_region_from_selection("Intro", FtsLane::Core(CoreLane::Sections));
    ActionResult::success_with_message("Inserted Intro section")
}

pub fn handle_insert_section_outro() -> ActionResult {
    insert_region_from_selection("Outro", FtsLane::Core(CoreLane::Sections));
    ActionResult::success_with_message("Inserted Outro section")
}

pub fn handle_insert_section_prechorus() -> ActionResult {
    insert_region_from_selection("Pre-CH", FtsLane::Core(CoreLane::Sections));
    ActionResult::success_with_message("Inserted Pre-Chorus section")
}

pub fn handle_insert_section_solo() -> ActionResult {
    insert_region_from_selection("Solo", FtsLane::Core(CoreLane::Sections));
    ActionResult::success_with_message("Inserted Solo section")
}

pub fn handle_insert_section_breakdown() -> ActionResult {
    insert_region_from_selection("Breakdown", FtsLane::Core(CoreLane::Sections));
    ActionResult::success_with_message("Inserted Breakdown section")
}

pub fn handle_insert_section_interlude() -> ActionResult {
    insert_region_from_selection("Interlude", FtsLane::Core(CoreLane::Sections));
    ActionResult::success_with_message("Inserted Interlude section")
}

pub fn handle_insert_section_tag() -> ActionResult {
    insert_region_from_selection("Tag", FtsLane::Core(CoreLane::Sections));
    ActionResult::success_with_message("Inserted Tag section")
}

// ── Structural Markers (MARKS lane) ──────────────────────────────────────

pub fn handle_insert_songstart() -> ActionResult {
    insert_marker_at_cursor("SONGSTART", FtsLane::Core(CoreLane::Marks));
    ActionResult::success_with_message("Inserted SONGSTART marker")
}

pub fn handle_insert_songend() -> ActionResult {
    insert_marker_at_cursor("SONGEND", FtsLane::Core(CoreLane::Marks));
    ActionResult::success_with_message("Inserted SONGEND marker")
}

pub fn handle_insert_countin() -> ActionResult {
    insert_marker_at_cursor("Count-In", FtsLane::Core(CoreLane::Marks));
    ActionResult::success_with_message("Inserted Count-In marker")
}

// ── Bound Markers (START/END lane) ───────────────────────────────────────

pub fn handle_insert_start() -> ActionResult {
    insert_marker_at_cursor("=START", FtsLane::Core(CoreLane::StartEnd));
    ActionResult::success_with_message("Inserted =START marker")
}

pub fn handle_insert_end() -> ActionResult {
    insert_marker_at_cursor("=END", FtsLane::Core(CoreLane::StartEnd));
    ActionResult::success_with_message("Inserted =END marker")
}

pub fn handle_insert_preroll() -> ActionResult {
    insert_marker_at_cursor("PREROLL", FtsLane::Core(CoreLane::StartEnd));
    ActionResult::success_with_message("Inserted PREROLL marker")
}

// ── Musical Markers ──────────────────────────────────────────────────────

pub fn handle_insert_key() -> ActionResult {
    // TODO: prompt for key name or use a default
    insert_marker_at_cursor("C", FtsLane::Core(CoreLane::Key));
    ActionResult::success_with_message("Inserted Key marker")
}

pub fn handle_insert_mode() -> ActionResult {
    insert_marker_at_cursor("Ionian", FtsLane::Core(CoreLane::Mode));
    ActionResult::success_with_message("Inserted Mode marker")
}

pub fn handle_insert_chord() -> ActionResult {
    insert_marker_at_cursor("C", FtsLane::Core(CoreLane::Chords));
    ActionResult::success_with_message("Inserted Chord marker")
}

// ── Instrument Note Actions ──────────────────────────────────────────────

fn insert_note_for(instrument: Option<InstrumentLane>) -> ActionResult {
    let lane = match instrument {
        Some(inst) => {
            ensure_instrument_lane(inst);
            FtsLane::Instrument(inst)
        }
        None => FtsLane::Core(CoreLane::Notes),
    };
    let name = instrument.map(|i| i.display_name()).unwrap_or("All");
    insert_marker_at_cursor("", lane);
    ActionResult::success_with_message(&format!("Inserted note for {}", name))
}

pub fn handle_insert_note_all() -> ActionResult { insert_note_for(None) }
pub fn handle_insert_note_drums() -> ActionResult { insert_note_for(Some(InstrumentLane::Drums)) }
pub fn handle_insert_note_bass() -> ActionResult { insert_note_for(Some(InstrumentLane::Bass)) }
pub fn handle_insert_note_guitar() -> ActionResult { insert_note_for(Some(InstrumentLane::Guitar)) }
pub fn handle_insert_note_guitar2() -> ActionResult { insert_note_for(Some(InstrumentLane::Guitar2)) }
pub fn handle_insert_note_keys() -> ActionResult { insert_note_for(Some(InstrumentLane::Keys)) }
pub fn handle_insert_note_keys2() -> ActionResult { insert_note_for(Some(InstrumentLane::Keys2)) }
pub fn handle_insert_note_lead() -> ActionResult { insert_note_for(Some(InstrumentLane::Lead)) }
pub fn handle_insert_note_bgvs() -> ActionResult { insert_note_for(Some(InstrumentLane::BGVs)) }
