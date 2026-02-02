//! Action Registration for REAPER
//!
//! Bridges between the ActionsRegistry library and REAPER's native action system.
//! This module:
//! 1. Uses ActionsRegistry to query cells for actions
//! 2. Registers actions with REAPER's action list
//! 3. Routes action execution back through ActionsRegistry

use actions_proto::ActionDefinition;
use actions_registry::ActionsRegistry;
use reaper_high::{ActionKind, Reaper, RegisteredAction};
use roam::session::ConnectionHandle;
use std::sync::{Arc, Mutex, OnceLock};
use tracing::{debug, info, warn};

/// Global storage for registered actions (keeps them alive for REAPER)
static REGISTERED_ACTIONS: OnceLock<Mutex<Vec<RegisteredAction>>> = OnceLock::new();

/// Global storage for action definitions (for menu building)
static ACTION_DEFS: OnceLock<Mutex<Vec<MenuActionDef>>> = OnceLock::new();

/// Global registry instance
static REGISTRY: OnceLock<Arc<ActionsRegistry>> = OnceLock::new();

/// Global tokio runtime for async operations from REAPER callbacks
static TOKIO_RUNTIME: OnceLock<tokio::runtime::Runtime> = OnceLock::new();

fn get_registered_actions_storage() -> &'static Mutex<Vec<RegisteredAction>> {
    REGISTERED_ACTIONS.get_or_init(|| Mutex::new(Vec::new()))
}

fn get_action_defs_storage() -> &'static Mutex<Vec<MenuActionDef>> {
    ACTION_DEFS.get_or_init(|| Mutex::new(Vec::new()))
}

/// Get the global ActionsRegistry
pub fn get_registry() -> Option<Arc<ActionsRegistry>> {
    REGISTRY.get().cloned()
}

/// Get the tokio runtime for async operations
fn get_runtime() -> &'static tokio::runtime::Runtime {
    TOKIO_RUNTIME.get_or_init(|| {
        tokio::runtime::Builder::new_multi_thread()
            .worker_threads(1)
            .enable_all()
            .build()
            .expect("Failed to create tokio runtime")
    })
}

/// Initialize the actions registry.
/// Call this once at extension startup.
pub fn init_registry() -> Arc<ActionsRegistry> {
    let registry = ActionsRegistry::new();
    let _ = REGISTRY.set(registry.clone());
    registry
}

/// Get all registered action definitions (for menu building)
pub fn get_all_registered_actions() -> Vec<MenuActionDef> {
    get_action_defs_storage()
        .lock()
        .unwrap_or_else(|e| e.into_inner())
        .clone()
}

/// Simple action definition for menu display
#[derive(Clone)]
pub struct MenuActionDef {
    /// Command ID (REAPER format, e.g., "FTS_SESSION_LOG_HELLO")
    pub command_id: String,
    /// Display name shown in REAPER
    pub display_name: String,
    /// Menu path (e.g., "FTS/Session")
    pub menu_path: Option<String>,
}

/// Register a cell with the actions registry and register its actions with REAPER.
///
/// This queries the cell for actions via `DefinesActions::get_actions()` and
/// registers each action with REAPER's action system.
pub async fn register_cell(cell_name: &str, handle: ConnectionHandle) {
    let registry = match get_registry() {
        Some(r) => r,
        None => {
            warn!("ActionsRegistry not initialized");
            return;
        }
    };

    // Register the cell with the registry (queries for actions via RPC)
    registry.register_cell(cell_name, handle).await;

    // Get the actions we just registered
    let actions = registry.get_cell_actions(cell_name).await;

    // Register each action with REAPER
    for action in actions {
        if let Err(e) = register_action_with_reaper(&action) {
            warn!(action = %action.id, error = %e, "Failed to register action with REAPER");
        }
    }

    // Wake up REAPER so actions appear in the action list
    if let Err(e) = Reaper::get().wake_up() {
        warn!(error = %e, "Failed to wake up REAPER after action registration");
    }
}

/// Register a single action with REAPER's action system
pub fn register_action_with_reaper(action: &ActionDefinition) -> Result<(), String> {
    let action_id = action.id.clone();
    let command_id = action.command_id();
    let display_name = action.display_name();
    let menu_path = action.menu_path.clone();

    debug!(
        command_id = %command_id,
        display_name = %display_name,
        "Registering action with REAPER"
    );

    // Leak the strings to get 'static lifetime (REAPER requires this)
    let command_id_static: &'static str = Box::leak(command_id.clone().into_boxed_str());
    let display_name_static: &'static str = Box::leak(display_name.clone().into_boxed_str());

    // Create the action handler closure
    let handler = move || {
        let action_id = action_id.clone();

        let registry = match get_registry() {
            Some(r) => r,
            None => {
                warn!(action_id = %action_id, "No registry available");
                return;
            }
        };

        debug!(action_id = %action_id, "Executing action");

        // Execute async code from sync REAPER callback context
        let rt = get_runtime();
        rt.block_on(async move {
            let result = registry.execute(action_id.clone()).await;
            if result.success {
                info!(
                    action_id = %action_id,
                    message = ?result.message,
                    "Action executed successfully"
                );
            } else {
                warn!(
                    action_id = %action_id,
                    message = ?result.message,
                    "Action execution failed"
                );
            }
        });
    };

    // Register with REAPER
    let registered_action = Reaper::get().register_action(
        command_id_static,
        display_name_static,
        None, // No default key binding
        handler,
        ActionKind::NotToggleable,
    );

    // Store the RegisteredAction to keep it alive
    if let Ok(mut storage) = get_registered_actions_storage().lock() {
        storage.push(registered_action);
    }

    // Store action def for menu building
    if let Ok(mut defs_storage) = get_action_defs_storage().lock() {
        defs_storage.push(MenuActionDef {
            command_id,
            display_name: action.name.clone(),
            menu_path,
        });
    }

    info!(action_id = %action.id, "Action registered with REAPER");

    Ok(())
}

/// Unregister a cell's actions from REAPER.
/// Note: REAPER doesn't support unregistering actions at runtime,
/// so this just removes from our internal tracking.
pub async fn unregister_cell(cell_name: &str) {
    if let Some(registry) = get_registry() {
        registry.unregister_cell(cell_name).await;
        // Note: We can't actually unregister from REAPER, actions persist until restart
    }
}
