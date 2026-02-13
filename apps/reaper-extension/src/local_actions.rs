//! Local actions owned by reaper-extension itself (not provided by cells).

use actions_proto::ActionResult;
use reaper_high::Reaper;
use reaper_medium::{CommandId, ProjectContext::CurrentProject};
use tracing::info;

actions_proto::define_actions! {
    pub reaper_extension_actions {
        prefix: "fts.reaper_extension",
        title: "Reaper Extension",
        LOG_RUNTIME = "log_runtime" {
            name: "Log Extension Runtime",
            description: "Log extension runtime details (pid/cwd)",
            category: Dev,
            group: "Dev",
            implementation: supported(handle_log_runtime),
        }
        CONSOLE_MSG = "console_msg" {
            name: "Console Msg (REAPER)",
            description: "Write a debug line to the REAPER console",
            category: Dev,
            group: "Dev",
            implementation: supported(handle_reaper_console_msg),
        }
        MAIN_ON_COMMAND_40044 = "main_on_command_40044" {
            name: "Main_OnCommandEx 40044",
            description: "Execute REAPER native command 40044 via main_on_command_ex",
            category: Dev,
            group: "Dev",
            implementation: supported(handle_main_on_command_40044),
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

/// Built-in local actions owned by reaper-extension.
pub fn builtin_local_actions() -> Vec<actions_proto::LocalActionRegistration> {
    reaper_extension_actions::definitions_with_handlers()
}
