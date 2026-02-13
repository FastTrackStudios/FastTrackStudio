//! Local actions owned by reaper-extension itself (not provided by cells).

use actions_proto::{ActionId, ActionResult};
use daw_proto::transport::fts_transport_actions;
use input_reaper::InputProfile;
use reaper_high::Reaper;
use reaper_low::{raw, Swell};
use reaper_medium::{CommandId, PositionInSeconds, ProjectContext::CurrentProject, SetEditCurPosOptions};
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
    debug!(start_seconds, "FTS transport remembered play start position");
}

fn remember_start_if_not_playing() {
    let play_state = Reaper::get().medium_reaper().get_play_state_ex(CurrentProject);
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
        actions_proto::LocalActionImplementation::Supported(Arc::new(handle_fts_transport_play_stop))
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

fn handle_log_input() -> ActionResult {
    input_reaper::log_state_to_console();
    ActionResult::success_with_message("Logged input runtime state")
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

/// Built-in local actions owned by reaper-extension.
pub fn builtin_local_actions() -> Vec<actions_proto::LocalActionRegistration> {
    let mut actions = reaper_extension_actions::definitions_with_handlers();
    actions.extend(fts_transport_actions::definitions_with_binder(
        &ReaperExtensionTransportActionBinder,
    ));
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
