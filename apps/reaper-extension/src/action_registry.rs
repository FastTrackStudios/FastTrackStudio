//! Action Registration for REAPER
//!
//! Bridges between the ActionsRegistry library and REAPER's native action system.
//! This module:
//! 1. Uses ActionsRegistry to query cells for actions
//! 2. Registers actions with REAPER's action list
//! 3. Routes action execution back through ActionsRegistry

use actions_proto::{ActionDefinition, LocalActionImplementation, LocalActionRegistration};
use actions_registry::ActionsRegistry;
use reaper_high::{ActionKind, Reaper, RegisteredAction};
use roam::ErasedCaller;
use std::collections::HashMap;
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
type LocalActionHandler = Arc<dyn Fn() -> actions_proto::ActionResult + Send + Sync>;
static LOCAL_ACTION_HANDLERS: OnceLock<Mutex<HashMap<String, LocalActionHandler>>> =
    OnceLock::new();
type LocalToggleGetter = Arc<dyn Fn() -> bool + Send + Sync>;
static LOCAL_TOGGLE_GETTERS: OnceLock<Mutex<HashMap<String, LocalToggleGetter>>> = OnceLock::new();

fn get_registered_actions_storage() -> &'static Mutex<Vec<RegisteredAction>> {
    REGISTERED_ACTIONS.get_or_init(|| Mutex::new(Vec::new()))
}

fn get_action_defs_storage() -> &'static Mutex<Vec<MenuActionDef>> {
    ACTION_DEFS.get_or_init(|| Mutex::new(Vec::new()))
}

fn get_local_action_handlers_storage() -> &'static Mutex<HashMap<String, LocalActionHandler>> {
    LOCAL_ACTION_HANDLERS.get_or_init(|| Mutex::new(HashMap::new()))
}

fn get_local_toggle_getters_storage() -> &'static Mutex<HashMap<String, LocalToggleGetter>> {
    LOCAL_TOGGLE_GETTERS.get_or_init(|| Mutex::new(HashMap::new()))
}

pub fn set_local_toggle_getter(action_id: &str, getter: LocalToggleGetter) {
    if let Ok(mut map) = get_local_toggle_getters_storage().lock() {
        map.insert(action_id.to_string(), getter);
    }
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
    /// Canonical action ID (also used as REAPER command name)
    pub action_id: String,
    /// Display name shown in REAPER
    pub display_name: String,
    /// Menu path (e.g., "FTS/Session")
    pub menu_path: Option<String>,
}

pub enum ActionRegistrationSource {
    Local(Vec<LocalActionRegistration>),
    Cell {
        cell_name: String,
        handle: ErasedCaller,
    },
}

/// Unified action registration entrypoint.
///
/// Handles both local in-process actions and cell-provided actions in one call.
pub async fn register_actions(sources: Vec<ActionRegistrationSource>) -> Result<usize, String> {
    let mut total_registered = 0usize;

    for source in sources {
        match source {
            ActionRegistrationSource::Local(actions) => {
                total_registered += register_local_actions_internal(actions)?;
            }
            ActionRegistrationSource::Cell { cell_name, handle } => {
                total_registered += register_cell_actions_internal(&cell_name, handle).await?;
            }
        }
    }

    // Force REAPER action list refresh after mixed registration.
    if let Err(error) = Reaper::get().wake_up() {
        warn!(%error, "Failed to wake up REAPER after action registration");
    } else {
        info!(total_registered, "Registered actions with REAPER");
    }

    Ok(total_registered)
}

/// Register a cell with the actions registry and register its actions with REAPER.
///
/// This queries the cell for actions via `DefinesActions::get_actions()` and
/// registers each action with REAPER's action system.
pub async fn register_cell(cell_name: &str, handle: ErasedCaller) {
    let _ = register_actions(vec![ActionRegistrationSource::Cell {
        cell_name: cell_name.to_string(),
        handle,
    }])
    .await;
}

async fn register_cell_actions_internal(
    cell_name: &str,
    handle: ErasedCaller,
) -> Result<usize, String> {
    let registry = match get_registry() {
        Some(r) => r,
        None => {
            return Err("ActionsRegistry not initialized".to_string());
        }
    };

    // Register the cell with the registry (queries for actions via RPC)
    registry.register_cell(cell_name, handle).await;

    // Get the actions we just registered
    let actions = registry.get_cell_actions(cell_name).await;

    let mut count = 0usize;

    // Register each action with REAPER
    for action in actions {
        if let Err(e) = register_action_with_reaper(&action) {
            warn!(action = %action.id, error = %e, "Failed to register action with REAPER");
        } else {
            count += 1;
        }
    }
    Ok(count)
}

/// Register local in-process actions owned by the REAPER extension.
pub fn register_local_actions(actions: Vec<LocalActionRegistration>) -> Result<(), String> {
    let _ = register_local_actions_internal(actions)?;
    if let Err(error) = Reaper::get().wake_up() {
        warn!(%error, "Failed to wake up REAPER after local action registration");
    }
    Ok(())
}

fn register_local_actions_internal(actions: Vec<LocalActionRegistration>) -> Result<usize, String> {
    let mut registered_count = 0usize;
    for entry in actions {
        match entry.implementation {
            LocalActionImplementation::Supported(handler) => {
                if let Ok(mut handlers) = get_local_action_handlers_storage().lock() {
                    handlers.insert(entry.definition.id.as_str().to_string(), handler);
                }
                register_action_with_reaper(&entry.definition)?;
                registered_count += 1;
            }
            LocalActionImplementation::Unsupported(reason) => {
                info!(
                    action_id = %entry.definition.id,
                    reason = reason,
                    "Skipping unsupported local action"
                );
            }
        }
    }
    Ok(registered_count)
}

/// Register a single action with REAPER's action system
pub fn register_action_with_reaper(action: &ActionDefinition) -> Result<(), String> {
    let action_key = action.id.clone();
    let action_id = action.id.as_str().to_string();
    let display_name = display_name_for_reaper(action);
    let menu_path = action.menu_path.clone();

    debug!(
        action_id = %action_id,
        display_name = %display_name,
        "Registering action with REAPER"
    );

    // Leak the strings to get 'static lifetime (REAPER requires this)
    let action_id_static: &'static str = Box::leak(action_id.clone().into_boxed_str());
    let display_name_static: &'static str = Box::leak(display_name.clone().into_boxed_str());

    // Create the action handler closure
    let action_id_for_handler = action_key.clone();
    let handler = move || {
        let action_id = action_id_for_handler.clone();

        if let Some(local) = get_local_action_handlers_storage()
            .lock()
            .ok()
            .and_then(|handlers| handlers.get(action_id.as_str()).cloned())
        {
            let result = local();
            if result.success {
                info!(action_id = %action_id, message = ?result.message, "Local action executed");
            } else {
                warn!(action_id = %action_id, message = ?result.message, "Local action failed");
            }
            return;
        }

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

    let action_kind = get_local_toggle_getters_storage()
        .lock()
        .ok()
        .and_then(|map| map.get(action_id.as_str()).cloned())
        .map(|getter| {
            let toggle_fn = move || getter();
            ActionKind::Toggleable(Box::new(toggle_fn))
        })
        .unwrap_or(ActionKind::NotToggleable);

    // Register with REAPER
    let registered_action = Reaper::get().register_action(
        action_id_static,
        display_name_static,
        None, // No default key binding
        handler,
        action_kind,
    );

    // Store the RegisteredAction to keep it alive
    if let Ok(mut storage) = get_registered_actions_storage().lock() {
        storage.push(registered_action);
    }

    // Mirror command ID mapping into input-reaper workflow infrastructure.
    if let Ok(cmd_id) = Reaper::get()
        .action_by_command_name(action_id_static)
        .command_id()
    {
        input_reaper::infrastructure::action_registry::remember_command_id(
            action_id.clone(),
            cmd_id,
        );
    }

    // Store action def for menu building
    if let Ok(mut defs_storage) = get_action_defs_storage().lock() {
        defs_storage.push(MenuActionDef {
            action_id: action_id.clone(),
            display_name: action.name.clone(),
            menu_path,
        });
    }

    info!(
        action_id = %action.id,
        menu_path = ?action.menu_path,
        "Action registered with REAPER"
    );

    Ok(())
}

fn display_name_for_reaper(action: &ActionDefinition) -> String {
    if let Some(menu_path) = action.menu_path.as_deref() {
        let mut parts = menu_path.split('/').filter(|p| !p.is_empty());
        let first = parts.next();
        let hierarchy = if matches!(first, Some("FTS")) {
            parts.collect::<Vec<_>>()
        } else {
            let mut all = Vec::new();
            if let Some(f) = first {
                all.push(f);
            }
            all.extend(parts);
            all
        };

        if !hierarchy.is_empty() {
            return format!("FTS: {} - {}", hierarchy.join(" / "), action.name);
        }
    }

    action.display_name()
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
