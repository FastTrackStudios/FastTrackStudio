//! Actions Standalone - Local actions for web and desktop apps
//!
//! This module provides a standalone actions implementation for apps that
//! don't have a host process (like the REAPER extension). It allows:
//!
//! 1. Registering local actions (app-specific functionality)
//! 2. Implementing `ActionsService` for command palette integration
//! 3. Combining with remote hosts for multi-source action palettes
//!
//! ## Usage
//!
//! ```ignore
//! use actions_standalone::StandaloneActions;
//!
//! // Create standalone actions
//! let standalone = StandaloneActions::new();
//!
//! // Register local actions
//! standalone.register_action(
//!     ActionDefinition::new(
//!         "fts.standalone.open_settings",
//!         "Open Settings",
//!         "Opens the application settings",
//!     )
//!     .with_category(ActionCategory::Settings)
//!     .with_shortcut("Cmd+,"),
//!     |_action_id| {
//!         // Open settings logic here
//!         ActionResult::success()
//!     },
//! );
//!
//! // Get all actions (for command palette)
//! let actions = standalone.get_all_actions();
//!
//! // Execute an action
//! let result = standalone.execute("fts.standalone.open_settings").await;
//! ```

use actions_proto::{
    ActionDefinition, ActionId, ActionResult, ActionsService, ActionsServiceDispatcher,
};
use async_lock::RwLock;
use roam::session::Context;
use std::collections::HashMap;
use std::sync::Arc;
use tracing::{info, warn};

/// Type alias for action handler functions.
///
/// Handlers receive the action ID and return an ActionResult.
pub type ActionHandler = Arc<dyn Fn(&ActionId) -> ActionResult + Send + Sync>;

/// A registered local action with its definition and handler.
struct RegisteredAction {
    definition: ActionDefinition,
    handler: ActionHandler,
}

/// Standalone actions registry for local app actions.
///
/// This provides actions that are local to the app (not from remote cells).
/// Use this for web/desktop app functionality like "Open Settings",
/// "Toggle Dark Mode", etc.
pub struct StandaloneActions {
    /// Map of action_id -> registered action
    actions: RwLock<HashMap<String, RegisteredAction>>,
}

impl StandaloneActions {
    /// Create a new empty standalone actions registry.
    pub fn new() -> Arc<Self> {
        Arc::new(Self {
            actions: RwLock::new(HashMap::new()),
        })
    }

    /// Register a local action with a handler.
    ///
    /// The handler will be called when the action is executed.
    pub async fn register<F>(&self, definition: ActionDefinition, handler: F)
    where
        F: Fn(&ActionId) -> ActionResult + Send + Sync + 'static,
    {
        let action_id = definition.id.as_str().to_string();
        let action_name = definition.name.clone();

        {
            let mut actions = self.actions.write().await;
            actions.insert(
                action_id.clone(),
                RegisteredAction {
                    definition: definition.clone(),
                    handler: Arc::new(handler),
                },
            );
        }

        info!(action_id = %action_id, name = %action_name, "Registered standalone action");
    }

    /// Unregister a local action.
    pub async fn unregister(&self, action_id: impl Into<ActionId>) {
        let action_id = action_id.into();
        let id_str = action_id.as_str().to_string();

        let removed = {
            let mut actions = self.actions.write().await;
            actions.remove(&id_str)
        };

        if removed.is_some() {
            info!(action_id = %action_id, "Unregistered standalone action");
        }
    }

    /// Get all registered actions.
    pub async fn get_all_actions(&self) -> Vec<ActionDefinition> {
        let actions = self.actions.read().await;
        actions.values().map(|a| a.definition.clone()).collect()
    }

    /// Execute an action by ID.
    pub async fn execute(&self, action_id: impl Into<ActionId>) -> ActionResult {
        let action_id = action_id.into();
        let id_str = action_id.as_str();

        let handler = {
            let actions = self.actions.read().await;
            actions.get(id_str).map(|a| a.handler.clone())
        };

        match handler {
            Some(handler) => {
                info!(action_id = %action_id, "Executing standalone action");
                handler(&action_id)
            }
            None => {
                warn!(action_id = %action_id, "Standalone action not found");
                ActionResult::failure(format!("Action not found: {}", action_id))
            }
        }
    }

    /// Check if an action is registered.
    pub async fn has_action(&self, action_id: &ActionId) -> bool {
        let actions = self.actions.read().await;
        actions.contains_key(action_id.as_str())
    }

    /// Create a dispatcher for exposing these actions as an ActionsService.
    pub fn dispatcher(self: &Arc<Self>) -> ActionsServiceDispatcher<StandaloneActionsServiceImpl> {
        ActionsServiceDispatcher::new(StandaloneActionsServiceImpl {
            standalone: self.clone(),
        })
    }
}

impl Default for StandaloneActions {
    fn default() -> Self {
        Self {
            actions: RwLock::new(HashMap::new()),
        }
    }
}

/// Implementation of ActionsService for standalone actions.
#[derive(Clone)]
pub struct StandaloneActionsServiceImpl {
    standalone: Arc<StandaloneActions>,
}

impl ActionsService for StandaloneActionsServiceImpl {
    async fn get_all_actions(&self, _cx: &Context) -> Vec<ActionDefinition> {
        self.standalone.get_all_actions().await
    }

    async fn execute(&self, _cx: &Context, action_id: ActionId) -> ActionResult {
        self.standalone.execute(action_id).await
    }
}

// ============================================================================
// Common standalone actions that apps might want to use
// ============================================================================

/// Register common standalone actions for a desktop/web app.
///
/// This registers actions like:
/// - `fts.standalone.open_settings`
/// - `fts.standalone.toggle_dark_mode`
/// - `fts.standalone.show_about`
///
/// You should provide handlers for these actions.
pub async fn register_common_actions<F>(standalone: &StandaloneActions, handler: F)
where
    F: Fn(&str) -> ActionResult + Send + Sync + Clone + 'static,
{
    use actions_proto::ActionCategory;

    let actions = vec![
        ActionDefinition::new(
            "fts.standalone.open_settings",
            "Open Settings",
            "Opens the application settings",
        )
        .with_category(ActionCategory::Settings)
        .with_shortcut("Cmd+,"),
        ActionDefinition::new(
            "fts.standalone.toggle_dark_mode",
            "Toggle Dark Mode",
            "Switches between light and dark themes",
        )
        .with_category(ActionCategory::View)
        .with_shortcut("Cmd+Shift+D"),
        ActionDefinition::new(
            "fts.standalone.show_about",
            "About FastTrackStudio",
            "Shows information about the application",
        )
        .with_category(ActionCategory::General),
        ActionDefinition::new(
            "fts.standalone.command_palette",
            "Command Palette",
            "Opens the command palette",
        )
        .with_category(ActionCategory::General)
        .with_shortcut("Cmd+Shift+P"),
    ];

    for action in actions {
        let action_id_str = action.id.as_str().to_string();
        let handler_clone = handler.clone();
        standalone
            .register(action, move |_id| handler_clone(&action_id_str))
            .await;
    }
}
