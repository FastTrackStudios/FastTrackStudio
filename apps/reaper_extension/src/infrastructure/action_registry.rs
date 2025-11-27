//! Action Registration Infrastructure for FastTrackStudio REAPER Extension
//!
//! Provides a unified way to register actions with REAPER.

use reaper_high::{ActionKind, Reaper, RegisteredAction};
use reaper_medium::{CommandId, HookCommand2, SectionContext, ActionValueChange, WindowContext};
use tracing::{debug, warn};
use std::sync::{OnceLock, Mutex};
use std::collections::HashMap;
use std::ffi::CString;

/// Global storage for registered actions (keeps them alive)
static REGISTERED_ACTIONS: OnceLock<Mutex<Vec<RegisteredAction>>> = OnceLock::new();

/// Global storage for action definitions (for menu building)
static ACTION_DEFS: OnceLock<Mutex<Vec<ActionDef>>> = OnceLock::new();

/// Global storage for command IDs (looked up on main thread, safe to use from any thread)
pub(crate) static COMMAND_IDS: OnceLock<Mutex<HashMap<&'static str, CommandId>>> = OnceLock::new();

/// Global storage for MIDI editor action handlers (command_id -> handler function)
static MIDI_EDITOR_HANDLERS: OnceLock<Mutex<HashMap<CommandId, fn()>>> = OnceLock::new();

/// Hook command handler for MIDI editor actions
pub struct MidiEditorActionHook;

impl HookCommand2 for MidiEditorActionHook {
    fn call(
        section: SectionContext,
        command_id: CommandId,
        _value_change: ActionValueChange,
        _window: WindowContext,
    ) -> bool {
        // Only handle MIDI editor section (32060)
        if let SectionContext::Sec(section_info) = section {
            if section_info.unique_id().get() == 32060 {
                // Check if this is one of our registered MIDI editor actions
                if let Ok(handlers) = MIDI_EDITOR_HANDLERS.get_or_init(|| Mutex::new(HashMap::new())).lock() {
                    if let Some(handler) = handlers.get(&command_id) {
                        debug!(command_id = command_id.get(), "Executing MIDI editor action");
                        handler();
                        return true; // We handled it
                    }
                }
            }
        }
        false // Not our command, let REAPER handle it
    }
}

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

/// Get command ID for an action by its command_id string
/// Returns None if not found or not yet registered
/// Safe to call from any thread (reads from static storage)
pub fn get_command_id(command_id_str: &str) -> Option<CommandId> {
    COMMAND_IDS
        .get()
        .and_then(|map| map.lock().ok())
        .and_then(|map| map.get(command_id_str).copied())
}

/// Section where an action should be registered
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ActionSection {
    /// Main section (default)
    Main,
    /// MIDI editor section (32060)
    MidiEditor,
}

impl ActionSection {
    fn section_id(&self) -> i32 {
        match self {
            ActionSection::Main => 0,
            ActionSection::MidiEditor => 32060,
        }
    }
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
    
    /// Section where this action should be registered (default: Main)
    pub section: ActionSection,
}

impl Default for ActionDef {
    fn default() -> Self {
        ActionDef {
            command_id: "",
            display_name: String::new(),
            handler: || {},
            appears_in_menu: false,
            section: ActionSection::Main,
        }
    }
}

/// Helper function to create an ActionDef with default section (Main)
pub fn action_def(
    command_id: &'static str,
    display_name: String,
    handler: fn(),
    appears_in_menu: bool,
) -> ActionDef {
    ActionDef {
        command_id,
        display_name,
        handler,
        appears_in_menu,
        section: ActionSection::Main,
    }
}

impl ActionDef {
    /// Register this action with REAPER
    pub fn register(&self) {
        let handler = self.handler;
        let command_id = self.command_id;
        let section_id = self.section.section_id();
        
        debug!(
            command_id = %self.command_id,
            display_name = %self.display_name,
            section_id = section_id,
            "Registering action with REAPER"
        );

        // Build full action name with category prefix
        let action_name = if self.command_id.starts_with("FTS_LYRICS_") {
            format!("FTS / Lyrics: {}", self.display_name)
        } else if self.command_id.starts_with("FTS_LIVE_") {
            format!("FTS / Live: {}", self.display_name)
        } else if self.command_id.starts_with("FTS_VM_") {
            format!("FTS / Visibility Manager: {}", self.display_name)
        } else if self.command_id.starts_with("FTS_DEV_") {
            format!("FTS / Dev: {}", self.display_name)
        } else {
            format!("FTS: {}", self.display_name)
        };

        // Register to main section using high-level API
        if self.section == ActionSection::Main {
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

            debug!(
                command_id = %self.command_id,
                action_name = %action_name,
                "Action successfully registered with REAPER (main section)"
            );
        } else {
            // Register to MIDI editor or other sections using low-level custom_action API
            self.register_to_section(section_id, &action_name, handler, command_id);
        }
    }
    
    /// Register action to a specific section using custom_action registration
    fn register_to_section(
        &self,
        section_id: i32,
        action_name: &str,
        handler: fn(),
        command_id: &'static str,
    ) {
        unsafe {
            let reaper = Reaper::get();
            let medium_reaper = reaper.medium_reaper();
            let low_reaper = medium_reaper.low();
            
            // Create custom_action_register_t structure
            let id_str_cstring = CString::new(command_id).expect("CString::new failed");
            let name_cstring = CString::new(action_name).expect("CString::new failed");
            
            #[repr(C)]
            struct CustomActionRegister {
                unique_section_id: i32,
                id_str: *const std::os::raw::c_char,
                name: *const std::os::raw::c_char,
                extra: *mut std::ffi::c_void,
            }
            
            let mut custom_action_reg = CustomActionRegister {
                unique_section_id: section_id,
                id_str: id_str_cstring.as_ptr(),
                name: name_cstring.as_ptr(),
                extra: std::ptr::null_mut(),
            };
            
            // Register the custom action using plugin_register
            let custom_action_str = CString::new("custom_action").expect("CString::new failed");
            let cmd_id = low_reaper.plugin_register(
                custom_action_str.as_ptr(),
                &mut custom_action_reg as *mut _ as *mut std::ffi::c_void,
            );
            std::mem::forget(custom_action_str); // Keep alive
            
            if cmd_id == 0 {
                warn!(
                    command_id = %command_id,
                    section_id = section_id,
                    "Failed to register action to section (custom_action returned 0)"
                );
                return;
            }
            
            let command_id_value = CommandId::new(cmd_id as u32);
            
            // Store command ID for lookup
            if let Ok(mut map) = COMMAND_IDS.get_or_init(|| Mutex::new(HashMap::new())).lock() {
                map.insert(command_id, command_id_value);
            }
            
            // Store handler for this command ID
            if let Ok(mut handlers) = MIDI_EDITOR_HANDLERS.get_or_init(|| Mutex::new(HashMap::new())).lock() {
                handlers.insert(command_id_value, handler);
            }
            
            // Keep the CStrings alive (they need to persist)
            std::mem::forget(id_str_cstring);
            std::mem::forget(name_cstring);
            
            debug!(
                command_id = %command_id,
                action_name = %action_name,
                section_id = section_id,
                registered_cmd_id = cmd_id,
                "Action successfully registered to MIDI editor section"
            );
        }
    }
}

/// Register a collection of actions with REAPER
pub fn register_actions(actions: &[ActionDef], module_name: &str) {
    // Store action definitions for menu building
    if let Ok(mut defs_storage) = get_action_defs_storage().lock() {
        defs_storage.extend(actions.iter().cloned());
    }
    
    debug!(
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
        debug!(
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

    debug!(
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
    
    // Look up and store command IDs for all registered actions (must be done on main thread)
    // This allows background threads to trigger actions without calling REAPER APIs
    let medium_reaper = Reaper::get().medium_reaper();
    let command_ids = COMMAND_IDS.get_or_init(|| Mutex::new(HashMap::new()));
    if let Ok(mut map) = command_ids.lock() {
        for action in actions {
            // Try both with and without underscore prefix
            let lookup_names = [action.command_id, &format!("_{}", action.command_id)];
            for lookup_name in &lookup_names {
                if let Some(cmd_id) = medium_reaper.named_command_lookup(*lookup_name) {
                    map.insert(action.command_id, cmd_id);
                    debug!(
                        command_id = %action.command_id,
                        lookup_name = %lookup_name,
                        "Stored command ID for action"
                    );
                    break;
                }
            }
        }
        debug!(
            module = %module_name,
            stored_count = map.len(),
            "Stored {} command IDs for background thread access",
            map.len()
        );
    }
}

/// Action registry service
#[derive(Debug)]
pub struct ActionRegistry;

impl ActionRegistry {
    /// Create a new action registry
    pub fn new() -> Result<Self, Box<dyn std::error::Error>> {
        Ok(Self)
    }
    
    /// Register all actions
    pub fn register_all(&self) -> Result<(), Box<dyn std::error::Error>> {
        // Register actions from modules
        crate::actions::register_all_actions();
        
        // Wake up REAPER so actions are available
        if let Err(e) = Reaper::get().wake_up() {
            warn!("Failed to wake up REAPER: {}", e);
        }
        
        Ok(())
    }
}
