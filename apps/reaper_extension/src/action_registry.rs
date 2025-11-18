//! Action Registration Infrastructure for FastTrackStudio REAPER Extension
//!
//! Provides a unified way to register actions with REAPER.

use reaper_high::{ActionKind, Reaper, RegisteredAction};
use tracing::{debug, info};
use std::sync::{OnceLock, Mutex};

/// Global storage for registered actions (keeps them alive)
static REGISTERED_ACTIONS: OnceLock<Mutex<Vec<RegisteredAction>>> = OnceLock::new();

/// Global storage for action definitions (for menu building)
static ACTION_DEFS: OnceLock<Mutex<Vec<ActionDef>>> = OnceLock::new();

/// Get the storage for registered actions
pub fn get_registered_actions_storage() -> &'static Mutex<Vec<RegisteredAction>> {
    REGISTERED_ACTIONS.get_or_init(|| Mutex::new(Vec::new()))
}

fn get_action_defs_storage() -> &'static Mutex<Vec<ActionDef>> {
    ACTION_DEFS.get_or_init(|| Mutex::new(Vec::new()))
}

/// Get all registered action definitions (for menu building)
pub fn get_all_registered_actions() -> Vec<ActionDef> {
    get_action_defs_storage()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .clone()
}

/// Simple action definition for FastTrackStudio
#[derive(Clone)]
pub struct ActionDef {
    /// Command ID (must be unique)
    pub command_id: &'static str,
    
    /// Display name shown in REAPER
    pub display_name: String,
    
    /// Function to execute when action is invoked
    pub handler: fn(),
    
    /// Whether this action should appear in the menu
    pub appears_in_menu: bool,
}

impl ActionDef {
    /// Register this action with REAPER
    pub fn register(&self) {
        let handler = self.handler;
        let command_id = self.command_id;
        
        info!(
            command_id = %self.command_id,
            display_name = %self.display_name,
            "Registering action with REAPER"
        );

        // Build full action name
        let action_name = format!("FTS: {}", self.display_name);

        // Register the action and store the RegisteredAction to keep it alive
        let registered_action = Reaper::get().register_action(
            self.command_id,
            action_name.clone(),
            None, // No default key binding
            move || {
                debug!(command_id = %command_id, "Executing action");
                handler();
            },
            ActionKind::NotToggleable,
        );

        // Store the RegisteredAction to keep it alive
        if let Ok(mut storage) = get_registered_actions_storage().lock() {
            storage.push(registered_action);
        }

        info!(
            command_id = %self.command_id,
            action_name = %action_name,
            "✅ Action successfully registered with REAPER"
        );
    }
}

/// Register a collection of actions with REAPER
pub fn register_actions(actions: &[ActionDef], module_name: &str) {
    // Store action definitions for menu building
    if let Ok(mut defs_storage) = get_action_defs_storage().lock() {
        defs_storage.extend(actions.iter().cloned());
    }
    
    info!(
        module = %module_name,
        action_count = actions.len(),
        "Starting action registration for module"
    );
    
    if actions.is_empty() {
        tracing::warn!(module = %module_name, "No actions to register for module");
        return;
    }
    
    let mut success_count = 0;
    
    for (index, action) in actions.iter().enumerate() {
        info!(
            module = %module_name,
            index = index + 1,
            total = actions.len(),
            command_id = %action.command_id,
            "Processing action {}/{}",
            index + 1,
            actions.len()
        );
        
        match std::panic::catch_unwind(|| {
            action.register();
            true
        }) {
            Ok(true) => {
                success_count += 1;
                debug!(
                    module = %module_name,
                    command_id = %action.command_id,
                    "Action registration succeeded"
                );
            }
            Err(e) => {
                tracing::warn!(
                    module = %module_name,
                    command_id = %action.command_id,
                    error = ?e,
                    "Failed to register action (panic caught)"
                );
            }
            _ => {
                tracing::warn!(
                    module = %module_name,
                    command_id = %action.command_id,
                    "Unknown result when registering action"
                );
            }
        }
    }

    info!(
        module = %module_name,
        success_count,
        total = actions.len(),
        "Completed action registration: {}/{} actions successfully registered",
        success_count,
        actions.len()
    );
    
    // Wake up REAPER so that registered actions appear in the action list
    if let Err(e) = Reaper::get().wake_up() {
        tracing::warn!(
            module = %module_name,
            error = %e,
            "Failed to wake up REAPER after action registration"
        );
    } else {
        debug!(module = %module_name, "REAPER woken up after action registration");
    }
}

