//! Local actions owned by reaper-extension itself (not provided by cells).

use actions_proto::{ActionId, ActionResult};
use daw::service::markers_regions::fts_markers_regions_actions;
use daw::service::transport::fts_transport_actions;
use dynamic_template_proto::actions::dynamic_template_actions;
use dynamic_template_proto::auto_color::actions::auto_color_actions;
use dynamic_template_proto::visibility_manager::actions::visibility_manager_actions;
use input_reaper::InputProfile;
use reaper_high::Reaper;
use reaper_low::{raw, Swell};
use reaper_medium::{
    CommandId, PositionInSeconds, ProjectContext::CurrentProject, SetEditCurPosOptions,
};
use std::collections::{HashMap, HashSet};
use std::ffi::CString;
use std::sync::{Arc, Mutex, OnceLock};
use tracing::{debug, info};

actions_proto::define_actions! {
    pub reaper_extension_actions {
        prefix: "fts.reaper_extension",
        LOG_RUNTIME = "log_runtime" {
            name: "Log Extension Runtime",
            description: "Log extension runtime details (pid/cwd)",
            category: Dev,
            group: "Dev",
            implementation: supported(super::handle_log_runtime),
        }
        CONSOLE_MSG = "console_msg" {
            name: "Console Msg (REAPER)",
            description: "Write a debug line to the REAPER console",
            category: Dev,
            group: "Dev",
            implementation: supported(super::handle_reaper_console_msg),
        }
        MAIN_ON_COMMAND_40044 = "main_on_command_40044" {
            name: "Main_OnCommandEx 40044",
            description: "Execute REAPER native command 40044 via main_on_command_ex",
            category: Dev,
            group: "Dev",
            implementation: supported(super::handle_main_on_command_40044),
        }
        LOG_INPUT = "log_input" {
            name: "Log Input Runtime State",
            description: "Log enabled/debug/profile state to REAPER console",
            category: Dev,
            group: "Input",
            implementation: supported(super::handle_log_input),
        }
        TOGGLE_INPUT = "toggle_input" {
            name: "Toggle Input Runtime",
            description: "Enable/disable input interception runtime",
            category: Dev,
            group: "Input",
            implementation: supported(super::handle_toggle_input),
        }
        TOGGLE_INPUT_DEBUG = "toggle_input_debug" {
            name: "Toggle Input Debug Logging",
            description: "Enable/disable verbose input runtime logging",
            category: Dev,
            group: "Input",
            implementation: supported(super::handle_toggle_input_debug),
        }
        TOGGLE_INPUT_INTERCEPT = "toggle_input_intercept" {
            name: "Toggle Input Intercept",
            description: "When enabled, handled input is intercepted (except text fields)",
            category: Dev,
            group: "Input",
            implementation: supported(super::handle_toggle_input_intercept),
        }
        INPUT_MENU = "input_menu" {
            name: "Input Menu",
            description: "Open input runtime menu (toggle + profile)",
            category: Dev,
            group: "Input",
            implementation: supported(super::handle_input_menu),
        }
        PROFILE_FASTTRACKSTUDIO = "profile_fasttrackstudio" {
            name: "Input Profile: FastTrackStudio",
            description: "Switch input runtime profile to FastTrackStudio",
            category: Dev,
            group: "Input Profiles",
            implementation: supported(super::handle_profile_fasttrackstudio),
        }
        PROFILE_LOGIC = "profile_logic" {
            name: "Input Profile: Logic",
            description: "Switch input runtime profile to Logic",
            category: Dev,
            group: "Input Profiles",
            implementation: supported(super::handle_profile_logic),
        }
        PROFILE_PROTOOLS = "profile_protools" {
            name: "Input Profile: Pro Tools",
            description: "Switch input runtime profile to Pro Tools",
            category: Dev,
            group: "Input Profiles",
            implementation: supported(super::handle_profile_protools),
        }
        RESET_MOUSE_MODIFIERS = "reset_mouse_modifiers" {
            name: "Reset Mouse Modifiers",
            description: "Re-apply mouse modifiers for the current active input profile",
            category: Dev,
            group: "Input",
            implementation: supported(super::handle_reset_mouse_modifiers),
        }
        WORKFLOW_TEMPO_MAPPING = "workflow_tempo_mapping" {
            name: "Workflow: Tempo Mapping",
            description: "Toggle Tempo Mapping workflow overlays",
            category: Dev,
            group: "Input Workflows",
            implementation: supported(super::handle_workflow_tempo_mapping),
        }
        WORKFLOW_FAST_SLIP_EDIT = "workflow_fast_slip_edit" {
            name: "Workflow: Fast Slip Edit",
            description: "Toggle Fast Slip Edit workflow overlays",
            category: Dev,
            group: "Input Workflows",
            implementation: supported(super::handle_workflow_fast_slip_edit),
        }
        WORKFLOW_DEACTIVATE = "workflow_deactivate" {
            name: "Workflow: Deactivate",
            description: "Deactivate the current active input workflow",
            category: Dev,
            group: "Input Workflows",
            implementation: supported(super::handle_workflow_deactivate),
        }
        GENERATE_GUIDE_TRACK = "generate_guide_track" {
            name: "Generate Guide Track",
            description: "Generate click, count, and section guide MIDI tracks from project regions",
            category: Dev,
            group: "Guide",
            implementation: supported(super::handle_generate_guide_track),
        }
        GENERATE_KEYFLOW_TRACKS = "generate_keyflow_tracks" {
            name: "Generate Keyflow Tracks",
            description: "Create/configure the Keyflow folder and child MIDI sends",
            category: Dev,
            group: "Keyflow",
            implementation: supported(super::handle_generate_keyflow_tracks),
        }
        EXPORT_KEYFLOW_CHART_TO_MIDI = "export_keyflow_chart_to_midi" {
            name: "Export Keyflow Chart to MIDI",
            description: "Export the Keyflow chart tracks, markers, and regions to MIDI",
            category: Dev,
            group: "Keyflow",
            implementation: supported(super::handle_export_keyflow_chart_to_midi),
        }
        // ── Session / Setlist ─────────────────────────────────────────
        GENERATE_COMBINED_SETLIST = "generate_combined_setlist" {
            name: "Generate Combined Setlist from Open Projects",
            description: "Combines all open song projects into a single setlist project (2 measure gap)",
            category: Dev,
            group: "Session",
            implementation: supported(super::handle_generate_combined_setlist),
        }
        GENERATE_RULER_LANES = "generate_ruler_lanes" {
            name: "Generate Ruler Lanes",
            description: "Create the standard FTS ruler lane layout (SECTIONS, MARKS, SONG, START/END, KEY, MODE, CHORDS, NOTES)",
            category: Dev,
            group: "Ruler Manager",
            implementation: supported(super::handle_generate_ruler_lanes),
        }
        ORGANIZE_RULER_LANES = "organize_ruler_lanes" {
            name: "Organize Regions/Markers into Ruler Lanes",
            description: "Sort existing markers and regions into the correct FTS ruler lanes based on their names",
            category: Dev,
            group: "Ruler Manager",
            implementation: supported(super::handle_organize_ruler_lanes),
        }
        // ── Routing Project ────────────────────────────────────────────
        OPEN_ROUTING_PROJECT = "open_routing_project" {
            name: "Open/Create FTS Routing Project",
            description: "Open existing or create new FTS routing project with loopback stem channels",
            category: Dev,
            group: "Session",
            implementation: supported(super::handle_open_routing_project),
        }
        // ── Organize ──────────────────────────────────────────────────
        ORGANIZE_TRACKS = "organize_tracks" {
            name: "Organize Tracks into FTS Hierarchy",
            description: "Sort tracks into canonical FTS hierarchy (Click+Guide, TRACKS, Reference/Stem Split) with ruler lanes and fixed lanes",
            category: Dev,
            group: "Dynamic Template",
            implementation: supported(super::handle_organize_tracks),
        }
        // ── Sync / Ableton Link ──────────────────────────────────────
        SYNC_TOGGLE_LINK = "sync_toggle_link" {
            name: "Toggle Ableton Link",
            description: "Enable/disable Ableton Link sync (toggles between Puppet and Off)",
            category: Dev,
            group: "Sync",
            implementation: supported(super::handle_sync_toggle_link),
        }
        SYNC_LINK_PUPPET = "sync_link_puppet" {
            name: "Link Puppet Mode",
            description: "Enable Ableton Link in Puppet mode (follow Link session)",
            category: Dev,
            group: "Sync",
            implementation: supported(super::handle_sync_link_puppet),
        }
        SYNC_LINK_MASTER = "sync_link_master" {
            name: "Link Master Mode",
            description: "Enable Ableton Link in Master mode (drive Link session)",
            category: Dev,
            group: "Sync",
            implementation: supported(super::handle_sync_link_master),
        }
        SYNC_LINK_OFF = "sync_link_off" {
            name: "Link Off",
            description: "Disable Ableton Link sync",
            category: Dev,
            group: "Sync",
            implementation: supported(super::handle_sync_link_off),
        }
        SYNC_SETLIST_TOGGLE = "sync_setlist_toggle" {
            name: "Toggle Setlist Sync",
            description: "Enable/disable live sync between song tabs and the combined setlist",
            category: Dev,
            group: "Sync",
            implementation: supported(super::handle_sync_setlist_toggle),
        }
        // ── Signal Save ─────────────────────────────────────────────
        SAVE_BLOCK = "save_block" {
            name: "Signal - Save Block",
            description: "Save selected FX as a Block preset (.RfxChain)",
            category: Dev,
            group: "Signal",
            implementation: supported(super::handle_save_block),
        }
        SAVE_MODULE = "save_module" {
            name: "Signal - Save Module",
            description: "Save selected track's FX chain as a Module preset (.RfxChain)",
            category: Dev,
            group: "Signal",
            implementation: supported(super::handle_save_module),
        }
        SAVE_LAYER = "save_layer" {
            name: "Signal - Save Layer",
            description: "Save selected track as a Layer preset (.RTrackTemplate)",
            category: Dev,
            group: "Signal",
            implementation: supported(super::handle_save_layer),
        }
        SAVE_RIG = "save_rig" {
            name: "Signal - Save Rig",
            description: "Save selected folder + children as a Rig preset (.RTrackTemplate)",
            category: Dev,
            group: "Signal",
            implementation: supported(super::handle_save_rig),
        }
        LOAD_PROFILE = "load_profile" {
            name: "Signal - Load Profile",
            description: "Load a profile (all variations as tracks from .RTrackTemplate files)",
            category: Dev,
            group: "Signal",
            implementation: supported(super::handle_load_profile),
        }
        // ── Setlist Navigation ──────────────────────────────────────
        SETLIST_INIT = "setlist_init" {
            name: "Setlist - Init Navigation",
            description: "Scan [S] song folders and activate the first song",
            category: Dev,
            group: "Setlist",
            implementation: supported(super::handle_setlist_init),
        }
        SETLIST_NEXT_SONG = "setlist_next_song" {
            name: "Setlist - Next Song",
            description: "Switch to the next song (mute current, unmute next)",
            category: Dev,
            group: "Setlist",
            implementation: supported(super::handle_setlist_next_song),
        }
        SETLIST_PREV_SONG = "setlist_prev_song" {
            name: "Setlist - Previous Song",
            description: "Switch to the previous song",
            category: Dev,
            group: "Setlist",
            implementation: supported(super::handle_setlist_prev_song),
        }
        SETLIST_NEXT_SECTION = "setlist_next_section" {
            name: "Setlist - Next Section",
            description: "Advance to the next section (follows song section order, wraps to next song)",
            category: Dev,
            group: "Setlist",
            implementation: supported(super::handle_setlist_next_section),
        }
        SETLIST_PREV_SECTION = "setlist_prev_section" {
            name: "Setlist - Previous Section",
            description: "Go back to the previous section (wraps to previous song)",
            category: Dev,
            group: "Setlist",
            implementation: supported(super::handle_setlist_prev_section),
        }
        SETLIST_NEXT_VARIATION = "setlist_next_variation" {
            name: "Setlist - Next Variation",
            description: "Cycle to the next variation track (ignores section order)",
            category: Dev,
            group: "Setlist",
            implementation: supported(super::handle_setlist_next_variation),
        }
        SETLIST_PREV_VARIATION = "setlist_prev_variation" {
            name: "Setlist - Previous Variation",
            description: "Cycle to the previous variation track",
            category: Dev,
            group: "Setlist",
            implementation: supported(super::handle_setlist_prev_variation),
        }
    }
}

fn handle_log_runtime() -> ActionResult {
    info!(
        pid = std::process::id(),
        cwd = ?std::env::current_dir().ok(),
        "Reaper-extension local runtime info"
    );
    ActionResult::success_with_message("Reaper-extension runtime info logged")
}

fn handle_reaper_console_msg() -> ActionResult {
    let msg = format!(
        "FTS local action hit: pid={} cwd={:?}\n",
        std::process::id(),
        std::env::current_dir().ok()
    );
    Reaper::get().show_console_msg(msg);
    ActionResult::success_with_message("Wrote message to REAPER console")
}

fn handle_main_on_command_40044() -> ActionResult {
    Reaper::get()
        .medium_reaper()
        .main_on_command_ex(CommandId::new(40044), 0, CurrentProject);
    ActionResult::success_with_message("Executed REAPER Main_OnCommandEx 40044")
}

static LAST_PLAY_START_SECONDS: OnceLock<Mutex<Option<f64>>> = OnceLock::new();

fn last_play_start_storage() -> &'static Mutex<Option<f64>> {
    LAST_PLAY_START_SECONDS.get_or_init(|| Mutex::new(None))
}

fn update_last_play_start_from_current_cursor() {
    let reaper = Reaper::get();
    let start_seconds = reaper
        .current_project()
        .play_or_edit_cursor_position()
        .map(|p| p.get())
        .unwrap_or(0.0);
    if let Ok(mut guard) = last_play_start_storage().lock() {
        *guard = Some(start_seconds);
    }
    debug!(
        start_seconds,
        "FTS transport remembered play start position"
    );
}

fn remember_start_if_not_playing() {
    let play_state = Reaper::get()
        .medium_reaper()
        .get_play_state_ex(CurrentProject);
    if !play_state.is_playing && !play_state.is_recording {
        update_last_play_start_from_current_cursor();
    }
}

fn run_transport_command(command_id: u32, should_capture_start: bool) {
    if should_capture_start {
        remember_start_if_not_playing();
    }
    Reaper::get()
        .medium_reaper()
        .main_on_command_ex(CommandId::new(command_id), 0, CurrentProject);
}

fn handle_fts_transport_play_stop() -> ActionResult {
    run_transport_command(40044, true);
    ActionResult::success_with_message("Executed FTS transport play/stop")
}

fn handle_fts_transport_play() -> ActionResult {
    run_transport_command(1007, true);
    ActionResult::success_with_message("Executed FTS transport play")
}

fn handle_fts_transport_play_pause() -> ActionResult {
    run_transport_command(40073, true);
    ActionResult::success_with_message("Executed FTS transport play/pause")
}

fn handle_fts_transport_play_skip_time_selection() -> ActionResult {
    run_transport_command(40317, true);
    ActionResult::success_with_message("Executed FTS transport play (skip time selection)")
}

fn handle_fts_transport_play_from_last_start_position() -> ActionResult {
    let last_start = last_play_start_storage()
        .lock()
        .ok()
        .and_then(|guard| *guard)
        .unwrap_or_else(|| {
            Reaper::get()
                .current_project()
                .play_or_edit_cursor_position()
                .map(|p| p.get())
                .unwrap_or(0.0)
        });

    let Ok(position) = PositionInSeconds::new(last_start) else {
        return ActionResult::failure("Invalid last play start position");
    };

    Reaper::get().current_project().set_edit_cursor_position(
        position,
        SetEditCurPosOptions {
            move_view: false,
            seek_play: false,
        },
    );

    run_transport_command(1007, false);

    ActionResult::success_with_message(format!(
        "Executed FTS transport play from last start ({last_start:.3}s)"
    ))
}

fn handle_fts_transport_toggle_recording() -> ActionResult {
    run_transport_command(1013, false);
    ActionResult::success_with_message("Executed FTS transport toggle recording")
}

struct ReaperExtensionTransportActionBinder;

impl fts_transport_actions::LocalActionBinder for ReaperExtensionTransportActionBinder {
    fn PLAY(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_fts_transport_play))
    }

    fn PLAY_STOP(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(
            handle_fts_transport_play_stop,
        ))
    }

    fn PLAY_PAUSE(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(
            handle_fts_transport_play_pause,
        ))
    }

    fn PLAY_SKIP_TIME_SELECTION(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(
            handle_fts_transport_play_skip_time_selection,
        ))
    }

    fn PLAY_FROM_LAST_START_POSITION(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(
            handle_fts_transport_play_from_last_start_position,
        ))
    }

    fn TOGGLE_RECORDING(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(
            handle_fts_transport_toggle_recording,
        ))
    }
}

fn handle_fts_insert_region_and_edit() -> ActionResult {
    Reaper::get()
        .medium_reaper()
        .main_on_command_ex(CommandId::new(40306), 0, CurrentProject);
    ActionResult::success_with_message("Executed FTS insert region and edit")
}

fn handle_fts_insert_marker_and_edit() -> ActionResult {
    let medium = Reaper::get().medium_reaper();
    // Force marker creation first so repeated invocations at same position create new markers.
    medium.main_on_command_ex(CommandId::new(40157), 0, CurrentProject);
    // Then open the marker editor for the current position marker.
    medium.main_on_command_ex(CommandId::new(40171), 0, CurrentProject);
    ActionResult::success_with_message("Executed FTS insert marker and edit")
}

struct ReaperExtensionMarkersRegionsActionBinder;

impl fts_markers_regions_actions::LocalActionBinder for ReaperExtensionMarkersRegionsActionBinder {
    fn INSERT_REGION_AND_EDIT(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(
            handle_fts_insert_region_and_edit,
        ))
    }

    fn INSERT_MARKER_AND_EDIT(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(
            handle_fts_insert_marker_and_edit,
        ))
    }
}

fn handle_log_input() -> ActionResult {
    let now_enabled = input_reaper::toggle_debug_logging();
    refresh_toolbar("FTS_INPUT_LOG_INPUT_RUNTIME_STATE");
    ActionResult::success_with_message(format!("Input debug logging enabled={}", now_enabled))
}

fn handle_toggle_input() -> ActionResult {
    let now_enabled = input_reaper::toggle_enabled();
    Reaper::get().show_console_msg(format!("FTS Input: enabled={}\n", now_enabled));
    refresh_toolbar("FTS_INPUT_TOGGLE_INPUT_RUNTIME");
    ActionResult::success_with_message(format!("Input runtime enabled={}", now_enabled))
}

fn handle_toggle_input_debug() -> ActionResult {
    let now_enabled = input_reaper::toggle_debug_logging();
    Reaper::get().show_console_msg(format!("FTS Input Debug: enabled={}\n", now_enabled));
    refresh_toolbar("FTS_INPUT_TOGGLE_INPUT_DEBUG_LOGGING");
    ActionResult::success_with_message(format!("Input debug logging enabled={}", now_enabled))
}

fn handle_toggle_input_intercept() -> ActionResult {
    let now_enabled = input_reaper::toggle_intercepting();
    Reaper::get().show_console_msg(format!(
        "FTS Input Intercept: enabled={} (text fields still passthrough)\n",
        now_enabled
    ));
    refresh_toolbar("FTS_INPUT_TOGGLE_INPUT_INTERCEPT");
    ActionResult::success_with_message(format!("Input intercept enabled={}", now_enabled))
}

fn handle_input_menu() -> ActionResult {
    let swell = Swell::get();
    let medium = Reaper::get().medium_reaper();
    let main_hwnd = medium.get_main_hwnd();
    let mut cursor_pos = raw::POINT { x: 0, y: 0 };
    unsafe {
        swell.GetCursorPos(&mut cursor_pos);
    }

    let menu = swell.CreatePopupMenu();
    if menu.is_null() {
        return ActionResult::failure("Failed to create popup menu");
    }

    const TOGGLE_ENABLE: u32 = 1001;
    const TOGGLE_INTERCEPT: u32 = 1002;
    const TOGGLE_DEBUG: u32 = 1003;
    const PROFILE_FTS: u32 = 2001;
    const PROFILE_LOGIC: u32 = 2002;
    const PROFILE_PROTOOLS: u32 = 2003;
    const RESET_MOUSE_MODIFIERS: u32 = 2100;
    const WORKFLOW_TEMPO_MAPPING: u32 = 3001;
    const WORKFLOW_FAST_SLIP_EDIT: u32 = 3002;
    const WORKFLOW_DEACTIVATE: u32 = 3003;

    let current_profile = input_reaper::current_profile();
    unsafe {
        add_menu_item(
            menu,
            TOGGLE_ENABLE,
            "Enable Input",
            input_reaper::is_enabled(),
        );
        add_menu_item(
            menu,
            TOGGLE_INTERCEPT,
            "Intercept",
            input_reaper::is_intercepting(),
        );
        add_menu_item(
            menu,
            TOGGLE_DEBUG,
            "Debug Logging",
            input_reaper::is_debug_logging(),
        );
        add_separator(menu);
        add_menu_item(
            menu,
            PROFILE_FTS,
            "Profile: FastTrackStudio",
            current_profile == InputProfile::FastTrackStudio,
        );
        add_menu_item(
            menu,
            PROFILE_LOGIC,
            "Profile: Logic",
            current_profile == InputProfile::Logic,
        );
        add_menu_item(
            menu,
            PROFILE_PROTOOLS,
            "Profile: Pro Tools",
            current_profile == InputProfile::ProTools,
        );
        add_menu_item(menu, RESET_MOUSE_MODIFIERS, "Reset Mouse Modifiers", false);
        add_separator(menu);
        add_menu_item(
            menu,
            WORKFLOW_TEMPO_MAPPING,
            "Workflow: Tempo Mapping",
            input_reaper::input::workflows::is_active("tempo_mapping"),
        );
        add_menu_item(
            menu,
            WORKFLOW_FAST_SLIP_EDIT,
            "Workflow: Fast Slip Edit",
            input_reaper::input::workflows::is_active("fast_slip_edit"),
        );
        add_menu_item(menu, WORKFLOW_DEACTIVATE, "Workflow: Deactivate", false);
    }

    let result = unsafe {
        swell.TrackPopupMenu(
            menu,
            raw::TPM_RETURNCMD as i32,
            cursor_pos.x,
            cursor_pos.y,
            0,
            main_hwnd.as_ptr(),
            std::ptr::null(),
        )
    };
    unsafe {
        swell.DestroyMenu(menu);
    }

    match result as u32 {
        TOGGLE_ENABLE => handle_toggle_input(),
        TOGGLE_INTERCEPT => handle_toggle_input_intercept(),
        TOGGLE_DEBUG => handle_toggle_input_debug(),
        PROFILE_FTS => handle_profile_fasttrackstudio(),
        PROFILE_LOGIC => handle_profile_logic(),
        PROFILE_PROTOOLS => handle_profile_protools(),
        RESET_MOUSE_MODIFIERS => handle_reset_mouse_modifiers(),
        WORKFLOW_TEMPO_MAPPING => handle_workflow_tempo_mapping(),
        WORKFLOW_FAST_SLIP_EDIT => handle_workflow_fast_slip_edit(),
        WORKFLOW_DEACTIVATE => handle_workflow_deactivate(),
        _ => ActionResult::success_with_message("Input menu dismissed"),
    }
}

fn set_profile(profile: InputProfile) -> ActionResult {
    match input_reaper::set_profile(profile) {
        Ok(()) => {
            input_reaper::log_state_to_console();
            refresh_toolbar("FTS_INPUT_PROFILES_INPUT_PROFILE_FAST_TRACK_STUDIO");
            refresh_toolbar("FTS_INPUT_PROFILES_INPUT_PROFILE_LOGIC");
            refresh_toolbar("FTS_INPUT_PROFILES_INPUT_PROFILE_PRO_TOOLS");
            ActionResult::success_with_message(format!("Input profile set to {}", profile.as_str()))
        }
        Err(error) => ActionResult::failure(format!("Failed to set profile: {error}")),
    }
}

fn handle_profile_fasttrackstudio() -> ActionResult {
    set_profile(InputProfile::FastTrackStudio)
}

fn handle_profile_logic() -> ActionResult {
    set_profile(InputProfile::Logic)
}

fn handle_profile_protools() -> ActionResult {
    set_profile(InputProfile::ProTools)
}

fn handle_reset_mouse_modifiers() -> ActionResult {
    let current = input_reaper::current_profile();
    match input_reaper::set_profile(current) {
        Ok(()) => {
            Reaper::get().show_console_msg(format!(
                "FTS Input: reset mouse modifiers for profile {}\n",
                current.as_str()
            ));
            ActionResult::success_with_message(format!(
                "Reset mouse modifiers for {}",
                current.as_str()
            ))
        }
        Err(error) => ActionResult::failure(format!("Failed to reset mouse modifiers: {error}")),
    }
}

fn handle_workflow_tempo_mapping() -> ActionResult {
    match input_reaper::input::workflows::toggle("tempo_mapping") {
        Ok(active) => {
            refresh_toolbar("FTS_INPUT_WORKFLOWS_WORKFLOW_TEMPO_MAPPING");
            ActionResult::success_with_message(format!(
                "Tempo Mapping workflow {}",
                if active { "activated" } else { "deactivated" }
            ))
        }
        Err(error) => ActionResult::failure(format!("Failed to toggle workflow: {error}")),
    }
}

fn handle_workflow_fast_slip_edit() -> ActionResult {
    match input_reaper::input::workflows::toggle("fast_slip_edit") {
        Ok(active) => {
            refresh_toolbar("FTS_INPUT_WORKFLOWS_WORKFLOW_FAST_SLIP_EDIT");
            ActionResult::success_with_message(format!(
                "Fast Slip Edit workflow {}",
                if active { "activated" } else { "deactivated" }
            ))
        }
        Err(error) => ActionResult::failure(format!("Failed to toggle workflow: {error}")),
    }
}

fn handle_workflow_deactivate() -> ActionResult {
    let deactivated = input_reaper::input::workflows::deactivate();
    refresh_toolbar("FTS_INPUT_WORKFLOWS_WORKFLOW_TEMPO_MAPPING");
    refresh_toolbar("FTS_INPUT_WORKFLOWS_WORKFLOW_FAST_SLIP_EDIT");
    ActionResult::success_with_message(if deactivated {
        "Workflow deactivated"
    } else {
        "No active workflow"
    })
}

// ============================================================================
// Guide Track Generation
// ============================================================================

fn handle_generate_guide_track() -> ActionResult {
    crate::guide_track::generate_guide_tracks()
}

fn handle_generate_keyflow_tracks() -> ActionResult {
    crate::keyflow_actions::generate_keyflow_tracks()
}

fn handle_export_keyflow_chart_to_midi() -> ActionResult {
    crate::keyflow_actions::export_keyflow_chart_to_midi()
}

// ============================================================================
// Dynamic Template: Sorting Actions
// ============================================================================

/// Item info for REAPER media items (pointer, name, original track)
struct DtItemInfo {
    media_item: *mut raw::MediaItem,
    name: String,
    original_track: *mut raw::MediaTrack,
}

/// Get info for all selected items in the current project
fn get_selected_item_info() -> Vec<DtItemInfo> {
    let low = Reaper::get().medium_reaper().low();
    let mut items = Vec::new();
    unsafe {
        let count = low.CountSelectedMediaItems(std::ptr::null_mut());
        for i in 0..count {
            let item = low.GetSelectedMediaItem(std::ptr::null_mut(), i);
            if item.is_null() {
                continue;
            }
            let original_track = low.GetMediaItem_Track(item);
            let take = low.GetActiveTake(item);
            let name = if take.is_null() {
                format!("Item {}", i)
            } else {
                let ptr = low.GetTakeName(take);
                if ptr.is_null() {
                    format!("Item {}", i)
                } else {
                    std::ffi::CStr::from_ptr(ptr).to_string_lossy().into_owned()
                }
            };
            items.push(DtItemInfo {
                media_item: item,
                name,
                original_track,
            });
        }
    }
    items
}

/// Get info for ALL items in the current project
fn get_all_item_info() -> Vec<DtItemInfo> {
    let low = Reaper::get().medium_reaper().low();
    let mut items = Vec::new();
    unsafe {
        let count = low.CountMediaItems(std::ptr::null_mut());
        for i in 0..count {
            let item = low.GetMediaItem(std::ptr::null_mut(), i);
            if item.is_null() {
                continue;
            }
            let original_track = low.GetMediaItem_Track(item);
            let take = low.GetActiveTake(item);
            let name = if take.is_null() {
                format!("Item {}", i)
            } else {
                let ptr = low.GetTakeName(take);
                if ptr.is_null() {
                    format!("Item {}", i)
                } else {
                    std::ffi::CStr::from_ptr(ptr).to_string_lossy().into_owned()
                }
            };
            items.push(DtItemInfo {
                media_item: item,
                name,
                original_track,
            });
        }
    }
    items
}

/// Delete empty tracks from a set of track pointers
fn delete_empty_tracks(tracks: &HashSet<*mut raw::MediaTrack>) {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();
    let mut deleted = 0;
    unsafe {
        for &track in tracks {
            if track.is_null() {
                continue;
            }
            if low.CountTrackMediaItems(track) == 0 {
                low.DeleteTrack(track);
                deleted += 1;
            }
        }
    }
    if deleted > 0 {
        reaper.show_console_msg(format!("Deleted {} empty original track(s)\n", deleted));
    }
}

/// Core sorting logic: takes items, organizes via dynamic-template, creates REAPER tracks
fn sort_items_core(items: Vec<DtItemInfo>, undo_label: &str) -> ActionResult {
    use dynamic_template::{default_config, OrganizeIntoTracks};

    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    if items.is_empty() {
        reaper.show_console_msg("No items found to sort.\n");
        return ActionResult::failure("No items to sort");
    }

    reaper.show_console_msg(format!("Found {} item(s) to sort\n", items.len()));

    // Collect original tracks so we can delete empties after moving items
    let original_tracks: HashSet<*mut raw::MediaTrack> = items
        .iter()
        .map(|i| i.original_track)
        .filter(|t| !t.is_null())
        .collect();

    let item_names: Vec<String> = items.iter().map(|i| i.name.clone()).collect();

    let config = default_config();
    let hierarchy = match item_names.organize_into_tracks(&config, None) {
        Ok(h) => h,
        Err(e) => {
            let msg = format!("Error organizing items: {}\n", e);
            reaper.show_console_msg(msg.as_str());
            return ActionResult::failure(msg);
        }
    };

    // Build name→MediaItem lookup (multiple items can share a name)
    let mut item_map: HashMap<&str, Vec<*mut raw::MediaItem>> = HashMap::new();
    for info in &items {
        item_map
            .entry(info.name.as_str())
            .or_default()
            .push(info.media_item);
    }

    unsafe {
        let undo_cstr = CString::new(undo_label).unwrap_or_default();
        low.Undo_BeginBlock();

        let mut track_index = low.CountTracks(std::ptr::null_mut());

        for track_node in &hierarchy.tracks {
            low.InsertTrackAtIndex(track_index, true);
            let reaper_track = low.GetTrack(std::ptr::null_mut(), track_index);
            if reaper_track.is_null() {
                track_index += 1;
                continue;
            }

            // Set track name
            let name_cstr = CString::new(track_node.name.as_str()).unwrap_or_default();
            let param_name = CString::new("P_NAME").unwrap();
            low.GetSetMediaTrackInfo_String(
                reaper_track,
                param_name.as_ptr(),
                name_cstr.as_ptr() as *mut _,
                true,
            );

            // Set folder depth
            let param_folder = CString::new("I_FOLDERDEPTH").unwrap();
            let folder_value = track_node.folder_depth_change.to_raw_value() as f64;
            low.SetMediaTrackInfo_Value(reaper_track, param_folder.as_ptr(), folder_value);

            // Move matching items to this track
            for item_name in &track_node.items {
                if let Some(media_items) = item_map.get(item_name.as_str()) {
                    for &media_item in media_items {
                        low.MoveMediaItemToTrack(media_item, reaper_track);
                    }
                }
            }

            track_index += 1;
        }

        delete_empty_tracks(&original_tracks);
        low.Undo_EndBlock(undo_cstr.as_ptr(), 0);
    }

    let msg = format!(
        "Sorted {} items into {} tracks",
        items.len(),
        hierarchy.tracks.len()
    );
    reaper.show_console_msg(format!("{}\n", msg));
    ActionResult::success_with_message(msg)
}

fn handle_dt_sort_selected() -> ActionResult {
    let items = get_selected_item_info();
    if items.is_empty() {
        Reaper::get().show_console_msg("No items selected.\n");
        return ActionResult::failure("No items selected");
    }
    sort_items_core(items, "Sort selected items into template")
}

fn handle_dt_sort_all() -> ActionResult {
    let items = get_all_item_info();
    if items.is_empty() {
        Reaper::get().show_console_msg("No items in project.\n");
        return ActionResult::failure("No items in project");
    }
    sort_items_core(items, "Sort all items into template")
}

fn handle_dt_import_and_sort() -> ActionResult {
    let reaper = Reaper::get();
    let low = reaper.medium_reaper().low();

    unsafe {
        let mut file_buf = vec![0u8; 4096];
        let title = CString::new("Select audio files to import and sort").unwrap();

        let result = low.GetUserFileNameForRead(
            file_buf.as_mut_ptr() as *mut i8,
            title.as_ptr(),
            std::ptr::null(),
        );

        if !result {
            return ActionResult::success_with_message("Import cancelled");
        }

        let file_path = std::ffi::CStr::from_ptr(file_buf.as_ptr() as *const i8)
            .to_string_lossy()
            .into_owned();

        if file_path.is_empty() {
            return ActionResult::failure("No file selected");
        }

        reaper.show_console_msg(format!("Importing: {}\n", file_path));

        let undo_label = CString::new("Import and sort files into template").unwrap();
        low.Undo_BeginBlock();

        let file_cstr = CString::new(file_path).unwrap_or_default();
        low.InsertMedia(file_cstr.as_ptr(), 1);

        let items = get_selected_item_info();
        if items.is_empty() {
            low.Undo_EndBlock(undo_label.as_ptr(), 0);
            return ActionResult::failure("No items were imported");
        }

        reaper.show_console_msg(format!("Imported {} item(s), sorting...\n", items.len()));

        // Collect original tracks before sorting
        let original_tracks: HashSet<*mut raw::MediaTrack> = items
            .iter()
            .map(|i| i.original_track)
            .filter(|t| !t.is_null())
            .collect();

        let item_names: Vec<String> = items.iter().map(|i| i.name.clone()).collect();

        let config = dynamic_template::default_config();
        match dynamic_template::OrganizeIntoTracks::organize_into_tracks(item_names, &config, None)
        {
            Ok(hierarchy) => {
                let mut item_map: HashMap<&str, Vec<*mut raw::MediaItem>> = HashMap::new();
                for info in &items {
                    item_map
                        .entry(info.name.as_str())
                        .or_default()
                        .push(info.media_item);
                }

                let mut track_index = low.CountTracks(std::ptr::null_mut());
                for track_node in &hierarchy.tracks {
                    low.InsertTrackAtIndex(track_index, true);
                    let reaper_track = low.GetTrack(std::ptr::null_mut(), track_index);
                    if reaper_track.is_null() {
                        track_index += 1;
                        continue;
                    }

                    let name_cstr = CString::new(track_node.name.as_str()).unwrap_or_default();
                    let param_name = CString::new("P_NAME").unwrap();
                    low.GetSetMediaTrackInfo_String(
                        reaper_track,
                        param_name.as_ptr(),
                        name_cstr.as_ptr() as *mut _,
                        true,
                    );

                    let param_folder = CString::new("I_FOLDERDEPTH").unwrap();
                    let folder_value = track_node.folder_depth_change.to_raw_value() as f64;
                    low.SetMediaTrackInfo_Value(reaper_track, param_folder.as_ptr(), folder_value);

                    for item_name in &track_node.items {
                        if let Some(media_items) = item_map.get(item_name.as_str()) {
                            for &media_item in media_items {
                                low.MoveMediaItemToTrack(media_item, reaper_track);
                            }
                        }
                    }

                    track_index += 1;
                }

                delete_empty_tracks(&original_tracks);
            }
            Err(e) => {
                reaper.show_console_msg(format!("Error organizing: {}\n", e));
                low.Undo_EndBlock(undo_label.as_ptr(), 0);
                return ActionResult::failure(format!("Error organizing: {}", e));
            }
        }

        low.Undo_EndBlock(undo_label.as_ptr(), 0);
    }

    ActionResult::success_with_message("Import and sort completed")
}

fn handle_auto_color_all() -> ActionResult {
    crate::auto_color::apply_colors_to_all_tracks();
    ActionResult::success_with_message("Auto-color applied to all tracks")
}

fn handle_auto_color_selected() -> ActionResult {
    crate::auto_color::apply_colors_to_selected();
    ActionResult::success_with_message("Auto-color applied to selected tracks")
}

fn handle_auto_color_toggle() -> ActionResult {
    let enabled = crate::auto_color::toggle_auto_color();
    refresh_toolbar("FTS_AUTO_COLOR_TOGGLE");
    ActionResult::success_with_message(format!(
        "Auto-color {}",
        if enabled { "enabled" } else { "disabled" }
    ))
}

fn handle_auto_color_clear_all() -> ActionResult {
    crate::auto_color::clear_all_track_colors();
    ActionResult::success_with_message("All track colors cleared")
}

fn handle_auto_color_clear_selected() -> ActionResult {
    crate::auto_color::clear_selected_track_colors();
    ActionResult::success_with_message("Selected track colors cleared")
}

struct DynamicTemplateActionBinder;

impl dynamic_template_actions::LocalActionBinder for DynamicTemplateActionBinder {
    fn SORT_SELECTED(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_dt_sort_selected))
    }

    fn SORT_ALL(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_dt_sort_all))
    }

    fn IMPORT_AND_SORT(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_dt_import_and_sort))
    }

    fn ORGANIZE_DEMO(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Unsupported("Demo-only action")
    }

    fn LOG_STATUS(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Unsupported("Dev-only action")
    }

    fn LOG_GROUPS(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Unsupported("Dev-only action")
    }
}

// ============================================================================
// Auto Color Actions
// ============================================================================

struct AutoColorActionBinder;

impl auto_color_actions::LocalActionBinder for AutoColorActionBinder {
    fn COLOR_ALL(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_auto_color_all))
    }

    fn COLOR_SELECTED(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_auto_color_selected))
    }

    fn TOGGLE(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_auto_color_toggle))
    }

    fn CLEAR_ALL(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_auto_color_clear_all))
    }

    fn CLEAR_SELECTED(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(
            handle_auto_color_clear_selected,
        ))
    }
}

// ============================================================================
// Visibility Manager: Group Toggle Actions
// ============================================================================

fn handle_vis_toggle(group: &str) -> ActionResult {
    let visible = crate::visibility::toggle_group(group);
    ActionResult::success_with_message(format!(
        "{} {}",
        group,
        if visible { "shown" } else { "hidden" }
    ))
}

fn handle_vis_show_all() -> ActionResult {
    crate::visibility::show_all();
    ActionResult::success_with_message("All tracks shown")
}

fn handle_vis_hide_all() -> ActionResult {
    crate::visibility::hide_all();
    ActionResult::success_with_message("All group tracks hidden")
}

fn handle_vis_rebuild_cache() -> ActionResult {
    crate::visibility::rebuild_cache();
    ActionResult::success_with_message("Visibility cache rebuilt")
}

struct VisibilityManagerActionBinder;

impl visibility_manager_actions::LocalActionBinder for VisibilityManagerActionBinder {
    fn TOGGLE_DRUMS(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| handle_vis_toggle("Drums")))
    }
    fn TOGGLE_PERCUSSION(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| {
            handle_vis_toggle("Percussion")
        }))
    }
    fn TOGGLE_BASS(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| handle_vis_toggle("Bass")))
    }
    fn TOGGLE_GUITARS(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| {
            handle_vis_toggle("Guitars")
        }))
    }
    fn TOGGLE_KEYS(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| handle_vis_toggle("Keys")))
    }
    fn TOGGLE_SYNTHS(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| {
            handle_vis_toggle("Synths")
        }))
    }
    fn TOGGLE_HORNS(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| handle_vis_toggle("Horns")))
    }
    fn TOGGLE_HARMONICA(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| {
            handle_vis_toggle("Harmonica")
        }))
    }
    fn TOGGLE_VOCALS(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| {
            handle_vis_toggle("Vocals")
        }))
    }
    fn TOGGLE_CHOIR(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| handle_vis_toggle("Choir")))
    }
    fn TOGGLE_ORCHESTRA(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| {
            handle_vis_toggle("Orchestra")
        }))
    }
    fn TOGGLE_SFX(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| handle_vis_toggle("SFX")))
    }
    fn TOGGLE_GUIDE(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| handle_vis_toggle("Guide")))
    }
    fn TOGGLE_REFERENCE(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(|| {
            handle_vis_toggle("Reference")
        }))
    }
    fn SHOW_ALL(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_vis_show_all))
    }
    fn HIDE_ALL(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_vis_hide_all))
    }
    fn REBUILD_CACHE(&self) -> actions_proto::LocalActionImplementation {
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_vis_rebuild_cache))
    }
}

/// Built-in local actions owned by reaper-extension.
pub fn builtin_local_actions() -> Vec<actions_proto::LocalActionRegistration> {
    let mut actions = reaper_extension_actions::definitions_with_handlers();
    actions.extend(fts_transport_actions::definitions_with_binder(
        &ReaperExtensionTransportActionBinder,
    ));
    actions.extend(fts_markers_regions_actions::definitions_with_binder(
        &ReaperExtensionMarkersRegionsActionBinder,
    ));
    actions.extend(dynamic_template_actions::definitions_with_binder(
        &DynamicTemplateActionBinder,
    ));
    actions.extend(auto_color_actions::definitions_with_binder(
        &AutoColorActionBinder,
    ));
    actions.extend(visibility_manager_actions::definitions_with_binder(
        &VisibilityManagerActionBinder,
    ));
    // Session actions (setlist navigation, build, playback control)
    actions.extend(crate::session::session_action_registrations());
    // Signal actions (variation switching 1-24, next/previous section)
    actions.extend(crate::signal_actions::signal_action_registrations());
    actions
        .into_iter()
        .map(|mut entry| {
            let generated = actions_proto::generated_action_id(
                entry.definition.menu_path.as_deref(),
                &entry.definition.name,
            );
            entry.definition.id = ActionId::new(generated);
            entry
        })
        .collect()
}

pub fn register_toggle_states() {
    crate::action_registry::set_local_toggle_getter(
        "FTS_INPUT_TOGGLE_INPUT_RUNTIME",
        Arc::new(input_reaper::is_enabled),
    );
    crate::action_registry::set_local_toggle_getter(
        "FTS_INPUT_TOGGLE_INPUT_INTERCEPT",
        Arc::new(input_reaper::is_intercepting),
    );
    crate::action_registry::set_local_toggle_getter(
        "FTS_INPUT_TOGGLE_INPUT_DEBUG_LOGGING",
        Arc::new(input_reaper::is_debug_logging),
    );
    crate::action_registry::set_local_toggle_getter(
        "FTS_INPUT_LOG_INPUT_RUNTIME_STATE",
        Arc::new(input_reaper::is_debug_logging),
    );
    crate::action_registry::set_local_toggle_getter(
        "FTS_INPUT_PROFILES_INPUT_PROFILE_FAST_TRACK_STUDIO",
        Arc::new(|| input_reaper::current_profile() == InputProfile::FastTrackStudio),
    );
    crate::action_registry::set_local_toggle_getter(
        "FTS_INPUT_PROFILES_INPUT_PROFILE_LOGIC",
        Arc::new(|| input_reaper::current_profile() == InputProfile::Logic),
    );
    crate::action_registry::set_local_toggle_getter(
        "FTS_INPUT_PROFILES_INPUT_PROFILE_PRO_TOOLS",
        Arc::new(|| input_reaper::current_profile() == InputProfile::ProTools),
    );
    crate::action_registry::set_local_toggle_getter(
        "FTS_INPUT_WORKFLOWS_WORKFLOW_TEMPO_MAPPING",
        Arc::new(|| input_reaper::input::workflows::is_active("tempo_mapping")),
    );
    crate::action_registry::set_local_toggle_getter(
        "FTS_INPUT_WORKFLOWS_WORKFLOW_FAST_SLIP_EDIT",
        Arc::new(|| input_reaper::input::workflows::is_active("fast_slip_edit")),
    );

    // Auto-color toggle state
    crate::action_registry::set_local_toggle_getter(
        "FTS_AUTO_COLOR_TOGGLE",
        Arc::new(crate::auto_color::is_auto_color_enabled),
    );

    // Visibility Manager toggle states
    for (cmd_id, group) in [
        ("FTS_VISIBILITY_MANAGER_TOGGLE_DRUMS", "Drums"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_PERCUSSION", "Percussion"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_BASS", "Bass"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_GUITARS", "Guitars"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_KEYS", "Keys"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_SYNTHS", "Synths"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_HORNS", "Horns"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_HARMONICA", "Harmonica"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_VOCALS", "Vocals"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_CHOIR", "Choir"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_ORCHESTRA", "Orchestra"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_SFX", "SFX"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_GUIDE", "Guide"),
        ("FTS_VISIBILITY_MANAGER_TOGGLE_REFERENCE", "Reference"),
    ] {
        let group = group.to_string();
        crate::action_registry::set_local_toggle_getter(
            cmd_id,
            Arc::new(move || crate::visibility::is_group_visible(&group)),
        );
    }
}

fn refresh_toolbar(command_id: &str) {
    let medium = Reaper::get().medium_reaper();
    if let Some(cmd_id) = medium
        .named_command_lookup(command_id)
        .or_else(|| medium.named_command_lookup(format!("_{}", command_id)))
    {
        unsafe {
            medium.low().RefreshToolbar2(0, cmd_id.get() as i32);
        }
    }
}

unsafe fn add_menu_item(menu: raw::HMENU, id: u32, text: &str, checked: bool) {
    let swell = Swell::get();
    let text_cstring = CString::new(text).unwrap_or_default();
    let mut mi = raw::MENUITEMINFO {
        fMask: raw::MIIM_TYPE | raw::MIIM_DATA | raw::MIIM_ID | raw::MIIM_STATE,
        fType: raw::MF_STRING,
        wID: id,
        fState: if checked { raw::MF_CHECKED } else { 0 },
        dwTypeData: text_cstring.as_ptr() as *mut i8,
        ..Default::default()
    };
    unsafe {
        swell.InsertMenuItem(menu, -1, 1, &mut mi);
    }
}

unsafe fn add_separator(menu: raw::HMENU) {
    let swell = Swell::get();
    let mut sep_info = raw::MENUITEMINFO {
        fMask: raw::MIIM_TYPE,
        fType: raw::MF_SEPARATOR,
        ..Default::default()
    };
    unsafe {
        swell.InsertMenuItem(menu, -1, 1, &mut sep_info);
    }
}


// ── Sync / Ableton Link Action Handlers ──────────────────────────────────────

fn handle_sync_toggle_link() -> ActionResult {
    use sync::link::Mode;
    if crate::sync_bridge::is_link_enabled() {
        crate::sync_bridge::disable_link();
        ActionResult::success_with_message("Ableton Link disabled")
    } else {
        crate::sync_bridge::enable_link(Mode::Puppet);
        ActionResult::success_with_message("Ableton Link enabled (Puppet mode)")
    }
}

fn handle_sync_link_puppet() -> ActionResult {
    crate::sync_bridge::enable_link(sync::link::Mode::Puppet);
    ActionResult::success_with_message("Ableton Link: Puppet mode")
}

fn handle_sync_link_master() -> ActionResult {
    crate::sync_bridge::enable_link(sync::link::Mode::Master);
    ActionResult::success_with_message("Ableton Link: Master mode")
}

fn handle_sync_link_off() -> ActionResult {
    crate::sync_bridge::disable_link();
    ActionResult::success_with_message("Ableton Link: Off")
}

fn handle_sync_setlist_toggle() -> ActionResult {
    let low = reaper_high::Reaper::get().medium_reaper().low();
    let section = std::ffi::CString::new("FTS_SYNC").unwrap();
    let key = std::ffi::CString::new("setlist_sync_enabled").unwrap();

    // Read current state
    let currently_enabled = unsafe {
        let ptr = low.GetExtState(section.as_ptr(), key.as_ptr());
        if ptr.is_null() {
            true // Default: enabled
        } else {
            std::ffi::CStr::from_ptr(ptr).to_string_lossy() != "0"
        }
    };

    let new_state = !currently_enabled;
    let value = std::ffi::CString::new(if new_state { "1" } else { "0" }).unwrap();
    unsafe {
        low.SetExtState(section.as_ptr(), key.as_ptr(), value.as_ptr(), false);
    }

    if new_state {
        ActionResult::success_with_message("Setlist sync enabled")
    } else {
        ActionResult::success_with_message("Setlist sync disabled")
    }
}

// ============================================================================
// Signal Save Actions
// ============================================================================

fn handle_save_block() -> ActionResult {
    tokio::task::spawn_local(crate::signal_save::save_block());
    ActionResult::success()
}

fn handle_save_module() -> ActionResult {
    tokio::task::spawn_local(crate::signal_save::save_module());
    ActionResult::success()
}

fn handle_save_layer() -> ActionResult {
    tokio::task::spawn_local(crate::signal_save::save_layer());
    ActionResult::success()
}

fn handle_save_rig() -> ActionResult {
    tokio::task::spawn_local(crate::signal_save::save_rig());
    ActionResult::success()
}

fn handle_load_profile() -> ActionResult {
    tokio::task::spawn_local(crate::signal_save::load_profile());
    ActionResult::success()
}

// ============================================================================
// Setlist Navigation Actions
// ============================================================================

fn handle_setlist_init() -> ActionResult {
    tokio::task::spawn_local(crate::setlist_nav::init_setlist());
    ActionResult::success()
}

fn handle_setlist_next_song() -> ActionResult {
    tokio::task::spawn_local(crate::setlist_nav::next_song());
    ActionResult::success()
}

fn handle_setlist_prev_song() -> ActionResult {
    tokio::task::spawn_local(crate::setlist_nav::prev_song());
    ActionResult::success()
}

fn handle_setlist_next_section() -> ActionResult {
    tokio::task::spawn_local(crate::setlist_nav::next_section());
    ActionResult::success()
}

fn handle_setlist_prev_section() -> ActionResult {
    tokio::task::spawn_local(crate::setlist_nav::prev_section());
    ActionResult::success()
}

fn handle_setlist_next_variation() -> ActionResult {
    tokio::task::spawn_local(crate::setlist_nav::next_variation());
    ActionResult::success()
}

fn handle_setlist_prev_variation() -> ActionResult {
    tokio::task::spawn_local(crate::setlist_nav::prev_variation());
    ActionResult::success()
}

// ── Session / Setlist Action Handlers ────────────────────────────────────────

fn handle_generate_combined_setlist() -> ActionResult {
    use session::SetlistService;

    moire::task::spawn(async {
        if let Some(session) = crate::session::SessionManager::try_get() {
            match session.setlist_service().generate_combined_setlist(2).await {
                Ok(result) => {
                    info!("Combined setlist generated: {}", result);
                }
                Err(e) => {
                    tracing::warn!("Failed to generate combined setlist: {}", e);
                }
            }
        } else {
            tracing::warn!("Session manager not initialized");
        }
    })
    .named("generate_combined_setlist");
    ActionResult::success_with_message("Generating combined setlist...")
}

// ── Ruler Manager Action Handlers ────────────────────────────────────────────

fn handle_generate_ruler_lanes() -> ActionResult {
    use session::ruler_lanes::CoreLane;

    moire::task::spawn(async {
        let daw = daw::Daw::get();
        match daw.current_project().await {
            Ok(project) => {
                for lane in CoreLane::all() {
                    if let Err(e) = project
                        .set_ruler_lane_name(lane.lane_index(), lane.display_name())
                        .await
                    {
                        tracing::warn!("Failed to set ruler lane {}: {e}", lane.display_name());
                    }
                }
                info!("Generated {} FTS ruler lanes", CoreLane::all().len());
            }
            Err(e) => tracing::warn!("Failed to get current project: {e}"),
        }
    })
    .named("generate_ruler_lanes");
    ActionResult::success_with_message("Generating FTS ruler lanes...")
}

fn handle_organize_ruler_lanes() -> ActionResult {
    use session::ruler_lanes::{
        classify_marker_lane, classify_region_lane, CoreLane,
    };

    moire::task::spawn(async {
        let daw = daw::Daw::get();
        let project = match daw.current_project().await {
            Ok(p) => p,
            Err(e) => {
                tracing::warn!("Failed to get current project: {e}");
                return;
            }
        };

        // Step 1: Ensure ruler lanes exist
        for lane in CoreLane::all() {
            let _ = project
                .set_ruler_lane_name(lane.lane_index(), lane.display_name())
                .await;
        }

        // Step 2: Classify and move all markers
        let markers = match project.markers().all().await {
            Ok(m) => m,
            Err(e) => {
                tracing::warn!("Failed to get markers: {e}");
                return;
            }
        };

        let mut marker_count = 0;
        for marker in &markers {
            let lane = classify_marker_lane(&marker.name);
            if let Some(id) = marker.id {
                let _ = project.markers().set_lane(id, Some(lane.lane_index())).await;
                marker_count += 1;
            }
        }

        // Step 3: Classify and move all regions
        let regions = match project.regions().all().await {
            Ok(r) => r,
            Err(e) => {
                tracing::warn!("Failed to get regions: {e}");
                return;
            }
        };

        let mut region_count = 0;
        for region in &regions {
            let lane = classify_region_lane(&region.name);
            if let Some(id) = region.id {
                let _ = project.regions().set_lane(id, Some(lane.lane_index())).await;
                region_count += 1;
            }
        }

        info!(
            "Organized {} markers and {} regions into ruler lanes",
            marker_count, region_count
        );
    })
    .named("organize_ruler_lanes");
    ActionResult::success_with_message("Organizing markers/regions into ruler lanes...")
}

fn handle_open_routing_project() -> ActionResult {
    use session::routing_project::ensure_routing_project;
    use session::LoopbackConfig;

    moire::task::spawn(async {
        let config = LoopbackConfig::default();
        match ensure_routing_project(&config).await {
            Ok(guid) => {
                info!("Routing project ready: {guid}");
            }
            Err(e) => {
                tracing::warn!("Failed to open/create routing project: {e}");
            }
        }
    })
    .named("open_routing_project");
    ActionResult::success_with_message("Opening/creating FTS routing project...")
}

fn handle_organize_tracks() -> ActionResult {
    // Save the current project, run the offline FTS organizer on the RPP file,
    // then reopen. This uses the same organize_into_fts_hierarchy function as
    // the session CLI, ensuring consistent behavior.
    moire::task::spawn(async {
        let daw = daw::Daw::get();
        let project = match daw.current_project().await {
            Ok(p) => p,
            Err(e) => {
                tracing::warn!("Failed to get current project: {e}");
                return;
            }
        };

        // Save first so RPP file is current
        if let Err(e) = project.save().await {
            tracing::warn!("Failed to save project: {e}");
            return;
        }

        // Get the project file path
        let info = match project.info().await {
            Ok(i) => i,
            Err(e) => {
                tracing::warn!("Failed to get project info: {e}");
                return;
            }
        };

        if info.path.is_empty() {
            tracing::warn!("Project has no file path — save it first");
            return;
        }

        let path = std::path::PathBuf::from(&info.path);

        // Run offline organizer on the RPP file
        let content = match std::fs::read_to_string(&path) {
            Ok(c) => c,
            Err(e) => {
                tracing::warn!("Failed to read RPP file: {e}");
                return;
            }
        };

        let mut rpp_project = match daw::file::parse_project_text(&content) {
            Ok(p) => p,
            Err(e) => {
                tracing::warn!("Failed to parse RPP: {e}");
                return;
            }
        };

        let original_count = rpp_project.tracks.len();

        // Reorganize tracks into FTS hierarchy
        rpp_project.tracks =
            daw::file::types::organize_into_fts_hierarchy(rpp_project.tracks);

        // Organize ruler lanes
        daw::file::setlist_rpp::organize_ruler_lanes(&mut rpp_project);

        // Write back
        let organized = daw::file::setlist_rpp::project_to_rpp_text(&rpp_project);
        if let Err(e) = std::fs::write(&path, &organized) {
            tracing::warn!("Failed to write organized RPP: {e}");
            return;
        }

        // Reopen the project to pick up changes
        // Close current and reopen from file
        let guid = project.guid().to_string();
        let _ = daw.close_project(&guid).await;
        match daw.open_project(path.to_string_lossy().as_ref()).await {
            Ok(_) => info!(
                "Organized {} tracks into FTS hierarchy",
                original_count
            ),
            Err(e) => tracing::warn!("Failed to reopen organized project: {e}"),
        }
    })
    .named("organize_tracks_fts");

    ActionResult::success_with_message("Organizing tracks into FTS hierarchy...")
}
