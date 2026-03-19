//! Local actions owned by reaper-extension itself (not provided by cells).

use actions_proto::{ActionId, ActionResult};
use daw::service::markers_regions::fts_markers_regions_actions;
use daw::service::transport::fts_transport_actions;
use reaper_high::Reaper;
use reaper_medium::{
    CommandId, PositionInSeconds, ProjectContext::CurrentProject, SetEditCurPosOptions,
};
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

/// Built-in local actions owned by reaper-extension.
pub fn builtin_local_actions() -> Vec<actions_proto::LocalActionRegistration> {
    let mut actions = reaper_extension_actions::definitions_with_handlers();
    actions.extend(fts_transport_actions::definitions_with_binder(
        &ReaperExtensionTransportActionBinder,
    ));
    actions.extend(fts_markers_regions_actions::definitions_with_binder(
        &ReaperExtensionMarkersRegionsActionBinder,
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
    // REMOVED: Input, auto-color, and visibility toggle states moved to their respective SHM extensions
}
