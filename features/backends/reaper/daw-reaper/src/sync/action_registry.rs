//! Sync `ActionRegistry` impl.
//!
//! Action registration is fundamentally synchronous on REAPER's main thread:
//! `reaper_high::Reaper::register_action` is itself a sync API. The async
//! `ActionRegistryService` just wraps the same logic in `main_thread::query`.
//!
//! Both impls now call the shared `register_action_main_thread` /
//! `unregister_action_main_thread` helpers in `crate::action_registry`. The
//! sync path calls them directly because `ReaperMainThread` already requires
//! running on the main thread.
//!
//! Trigger consumers (including `daw_module` actions) subscribe to the global
//! action broadcast via the async `ActionRegistryService::subscribe` channel —
//! sync registration plugs into the same broadcast.

use daw_proto::{DawError, DawResult, sync::ActionRegistry};

use crate::action_registry::{register_action_main_thread, unregister_action_main_thread};

use super::ReaperMainThread;

pub struct ReaperActionRegistry<'a> {
    _mt: &'a ReaperMainThread,
}

impl<'a> ReaperActionRegistry<'a> {
    pub(crate) fn new(mt: &'a ReaperMainThread) -> Self {
        Self { _mt: mt }
    }
}

fn check(cmd_id: u32, name: &str) -> DawResult<u32> {
    if cmd_id > 0 {
        Ok(cmd_id)
    } else {
        Err(DawError::operation_failed(format!(
            "register_action returned 0 for '{name}'"
        )))
    }
}

impl<'a> ActionRegistry for ReaperActionRegistry<'a> {
    fn register(&self, cmd_name: &str, description: &str) -> DawResult<u32> {
        check(
            register_action_main_thread(cmd_name, description, false, false),
            cmd_name,
        )
    }

    fn register_in_menu(&self, cmd_name: &str, description: &str) -> DawResult<u32> {
        check(
            register_action_main_thread(cmd_name, description, true, false),
            cmd_name,
        )
    }

    fn register_toggle(&self, cmd_name: &str, description: &str) -> DawResult<u32> {
        check(
            register_action_main_thread(cmd_name, description, false, true),
            cmd_name,
        )
    }

    fn register_toggle_in_menu(&self, cmd_name: &str, description: &str) -> DawResult<u32> {
        check(
            register_action_main_thread(cmd_name, description, true, true),
            cmd_name,
        )
    }

    fn unregister(&self, cmd_name: &str) -> DawResult<()> {
        if unregister_action_main_thread(cmd_name) {
            Ok(())
        } else {
            Err(DawError::not_found("Action", cmd_name))
        }
    }
}
