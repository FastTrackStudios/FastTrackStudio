//! Workflow System
//!
//! Workflows are high-level context switches that coordinate:
//! - Keybind overlays
//! - Mouse modifier overlays
//! - REAPER settings (toggles, preferences)
//! - Armed click actions (custom click interception)
//!
//! Examples: Tempo Mapping, Fast Slip Edit, Recording, Mixing
//!
//! ## Profile-Based Implementations
//!
//! Workflows are **profile-agnostic** - they represent a concept (e.g., "Tempo Mapping")
//! rather than a specific implementation. Each profile (FastTrackStudio, Logic, etc.)
//! can register its own implementation of a workflow, or use the default.
//!
//! When a workflow is activated:
//! 1. Check if the current profile has a registered implementation
//! 2. If not, use the default implementation
//! 3. If no implementation exists, return an error

pub mod armed;
pub mod context_detection;

use crate::infrastructure::toolbar::{self, ToolbarButton, ToolbarTarget};
use crate::input::keybinds;
use crate::input::mouse_modifiers::manager as mouse_manager;
pub use armed::{
    ArmedClickAction, ArmedContext,
    detect_mouse_modifier_context, is_debug_mouse_context_enabled, toggle_debug_mouse_context,
    // Comprehensive MM_CTX detection
    MouseModifierContext, MouseContextResult, ItemHitInfo, detect_context_at_point,
};
use reaper_high::Reaper;
use std::collections::HashMap;
use std::sync::{OnceLock, RwLock};
use tracing::{debug, info, warn};

// region: --- Workflow Definition (Profile-Agnostic)

/// A workflow definition - the profile-agnostic concept
///
/// This represents "what" a workflow is (e.g., "Tempo Mapping"),
/// not "how" it's implemented for a specific profile.
#[derive(Debug, Clone)]
pub struct WorkflowDefinition {
    /// Unique workflow ID (e.g., "tempo_mapping")
    pub id: &'static str,
    /// Human-readable name (e.g., "Tempo Mapping")
    pub name: &'static str,
    /// Short name for toolbar display (e.g., "Tempo")
    pub short_name: Option<&'static str>,
    /// Description of what this workflow does
    pub description: &'static str,
}

impl WorkflowDefinition {
    pub fn new(id: &'static str, name: &'static str, description: &'static str) -> Self {
        Self { id, name, short_name: None, description }
    }

    pub fn with_short_name(id: &'static str, name: &'static str, short_name: &'static str, description: &'static str) -> Self {
        Self { id, name, short_name: Some(short_name), description }
    }

    /// Get the display name for toolbar buttons
    ///
    /// Uses short_name if:
    /// - The name is longer than 12 characters AND has no spaces
    /// - A short_name is defined
    ///
    /// Otherwise uses the full name.
    pub fn display_name(&self) -> &str {
        if let Some(short) = self.short_name {
            // Use short name if full name is > 12 chars with no spaces
            if self.name.len() > 12 && !self.name.contains(' ') {
                return short;
            }
        }
        self.name
    }
}

// endregion: --- Workflow Definition

// region: --- Workflow Implementation (Profile-Specific)

/// A workflow implementation - the profile-specific settings
///
/// This represents "how" a workflow is implemented for a specific profile,
/// including keybinds, mouse modifiers, REAPER settings, etc.
#[derive(Debug, Clone, Default)]
pub struct WorkflowImplementation {
    /// Keybind overlays to enable
    pub keybind_overlays: Vec<&'static str>,
    /// Mouse modifier overlays to enable
    pub mouse_overlays: Vec<&'static str>,
    /// REAPER settings to apply
    pub reaper_settings: Vec<ReaperSetting>,
    /// Action to arm when workflow is active (uses REAPER's native arm)
    pub armed_action: Option<ArmedAction>,
    /// Custom armed click action (intercepts left-clicks)
    pub armed_click: Option<ArmedClickAction>,
    /// Toolbar buttons to add when workflow is active
    pub toolbar_buttons: Vec<ToolbarButton>,
}

impl WorkflowImplementation {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a keybind overlay to enable
    pub fn with_keybind_overlay(mut self, overlay: &'static str) -> Self {
        self.keybind_overlays.push(overlay);
        self
    }

    /// Add multiple keybind overlays
    pub fn with_keybind_overlays(mut self, overlays: impl IntoIterator<Item = &'static str>) -> Self {
        self.keybind_overlays.extend(overlays);
        self
    }

    /// Add a mouse modifier overlay to enable
    pub fn with_mouse_overlay(mut self, overlay: &'static str) -> Self {
        self.mouse_overlays.push(overlay);
        self
    }

    /// Add multiple mouse modifier overlays
    pub fn with_mouse_overlays(mut self, overlays: impl IntoIterator<Item = &'static str>) -> Self {
        self.mouse_overlays.extend(overlays);
        self
    }

    /// Add a REAPER setting
    pub fn with_setting(mut self, setting: ReaperSetting) -> Self {
        self.reaper_settings.push(setting);
        self
    }

    /// Add multiple REAPER settings
    pub fn with_settings(mut self, settings: impl IntoIterator<Item = ReaperSetting>) -> Self {
        self.reaper_settings.extend(settings);
        self
    }

    /// Set an action to arm when workflow is active
    pub fn with_armed_action(mut self, action: ArmedAction) -> Self {
        self.armed_action = Some(action);
        self
    }

    /// Set a custom armed click action
    pub fn with_armed_click(mut self, action: ArmedClickAction) -> Self {
        self.armed_click = Some(action);
        self
    }

    /// Add a toolbar button to a floating toolbar
    pub fn with_floating_toolbar_button(mut self, toolbar_num: u8, command_name: &str, label: &str) -> Self {
        self.toolbar_buttons.push(ToolbarButton {
            command_name: command_name.to_string(),
            label: label.to_string(),
            icon: None,
            toolbar: ToolbarTarget::Floating(toolbar_num),
            toolbar_flags: toolbar::flags::NORMAL,
        });
        self
    }

    /// Add a toolbar button to the main toolbar
    pub fn with_main_toolbar_button(mut self, command_name: &str, label: &str) -> Self {
        self.toolbar_buttons.push(ToolbarButton {
            command_name: command_name.to_string(),
            label: label.to_string(),
            icon: None,
            toolbar: ToolbarTarget::Main,
            toolbar_flags: toolbar::flags::NORMAL,
        });
        self
    }
}

// endregion: --- Workflow Implementation

// region: --- REAPER Settings

/// A REAPER setting that can be toggled
#[derive(Debug, Clone)]
pub struct ReaperSetting {
    /// Human-readable name
    pub name: &'static str,
    /// Command ID to toggle (or action name)
    pub command: &'static str,
    /// Desired state when workflow is active
    pub enabled: bool,
}

/// An action to arm when the workflow is active
#[derive(Debug, Clone)]
pub struct ArmedAction {
    /// Human-readable name
    pub name: &'static str,
    /// Command ID or action name to arm
    pub command: &'static str,
    /// Section name (empty string for main section)
    pub section: &'static str,
    /// If true, intercept left-clicks to trigger this action
    /// If false, only REAPER's native arm mechanism is used (may not work for all actions)
    pub intercept_clicks: bool,
}

impl ArmedAction {
    pub fn new(name: &'static str, command: &'static str) -> Self {
        Self {
            name,
            command,
            section: "", // Main section by default
            intercept_clicks: false, // Default to not intercepting clicks
        }
    }

    pub fn with_section(mut self, section: &'static str) -> Self {
        self.section = section;
        self
    }

    /// Enable click interception - left-clicks in arrange will trigger this action
    pub fn with_click_intercept(mut self) -> Self {
        self.intercept_clicks = true;
        self
    }
}

impl ReaperSetting {
    pub fn new(name: &'static str, command: &'static str, enabled: bool) -> Self {
        Self { name, command, enabled }
    }

    /// Create a setting that should be ON when workflow is active
    pub fn on(name: &'static str, command: &'static str) -> Self {
        Self::new(name, command, true)
    }

    /// Create a setting that should be OFF when workflow is active
    pub fn off(name: &'static str, command: &'static str) -> Self {
        Self::new(name, command, false)
    }
}


/// Stored state for restoration when workflow is deactivated
#[derive(Debug, Clone, Default)]
struct StoredState {
    /// Keybind overlays that were active before workflow
    keybind_overlays: Vec<String>,
    /// Mouse overlays that were active before workflow
    mouse_overlays: Vec<String>,
    /// Original toggle states for REAPER settings (command -> was_on)
    reaper_settings: HashMap<String, bool>,
    /// Previously armed command (cmd_id, section) - 0 if nothing was armed
    previous_armed: Option<(i32, String)>,
}

// endregion: --- Stored State

// region: --- Workflow Manager

/// Manages workflows and their activation state
///
/// Workflows are stored as:
/// - **Definitions**: Profile-agnostic workflow concepts (id, name, description)
/// - **Default implementations**: The fallback implementation for each workflow
/// - **Profile implementations**: Profile-specific overrides for workflows
pub struct WorkflowManager {
    /// Workflow definitions (id -> definition)
    definitions: HashMap<String, WorkflowDefinition>,
    /// Default implementations (workflow_id -> implementation)
    default_implementations: HashMap<String, WorkflowImplementation>,
    /// Profile-specific implementations ((workflow_id, profile_id) -> implementation)
    profile_implementations: HashMap<(String, String), WorkflowImplementation>,
    /// Currently active workflow ID (only one at a time)
    active_workflow: Option<String>,
    /// The resolved implementation currently in use
    active_implementation: Option<WorkflowImplementation>,
    /// Stored state for restoration
    stored_state: StoredState,
}

impl WorkflowManager {
    pub fn new() -> Self {
        Self {
            definitions: HashMap::new(),
            default_implementations: HashMap::new(),
            profile_implementations: HashMap::new(),
            active_workflow: None,
            active_implementation: None,
            stored_state: StoredState::default(),
        }
    }

    // region: --- New API (Profile-Based)

    /// Define a workflow (profile-agnostic)
    ///
    /// This creates the workflow concept without any implementation.
    /// Use `implement()` or `implement_for_profile()` to add implementations.
    pub fn define(&mut self, id: &'static str, name: &'static str, description: &'static str) {
        debug!(id = %id, name = %name, "Defining workflow");
        self.definitions.insert(id.to_string(), WorkflowDefinition::new(id, name, description));
    }

    /// Define a workflow with a short name for toolbar display
    ///
    /// The short name is used when the full name is > 12 chars with no spaces.
    pub fn define_with_short_name(&mut self, id: &'static str, name: &'static str, short_name: &'static str, description: &'static str) {
        debug!(id = %id, name = %name, short_name = %short_name, "Defining workflow with short name");
        self.definitions.insert(id.to_string(), WorkflowDefinition::with_short_name(id, name, short_name, description));
    }

    /// Register the default implementation for a workflow
    ///
    /// This is used when no profile-specific implementation is available.
    pub fn implement(&mut self, workflow_id: &str, implementation: WorkflowImplementation) {
        debug!(workflow_id = %workflow_id, "Registering default implementation");
        self.default_implementations.insert(workflow_id.to_string(), implementation);
    }

    /// Register a profile-specific implementation for a workflow
    ///
    /// This overrides the default implementation for the specified profile.
    pub fn implement_for_profile(&mut self, workflow_id: &str, profile: &str, implementation: WorkflowImplementation) {
        debug!(workflow_id = %workflow_id, profile = %profile, "Registering profile implementation");
        self.profile_implementations.insert((workflow_id.to_string(), profile.to_string()), implementation);
    }

    /// Get the implementation for a workflow based on the current profile
    ///
    /// Returns the profile-specific implementation if available, otherwise the default.
    pub fn get_implementation(&self, workflow_id: &str, profile: &str) -> Option<&WorkflowImplementation> {
        // Try profile-specific first
        let key = (workflow_id.to_string(), profile.to_string());
        if let Some(impl_) = self.profile_implementations.get(&key) {
            return Some(impl_);
        }

        // Fall back to default
        self.default_implementations.get(workflow_id)
    }

    /// Get all workflow definitions (profile-agnostic list)
    pub fn list_definitions(&self) -> Vec<(&str, &str, &str)> {
        self.definitions
            .values()
            .map(|d| (d.id, d.name, d.description))
            .collect()
    }

    /// Get a workflow definition by ID
    pub fn get_definition(&self, id: &str) -> Option<&WorkflowDefinition> {
        self.definitions.get(id)
    }

    /// Get the active workflow definition (if any)
    pub fn active_definition(&self) -> Option<&WorkflowDefinition> {
        self.active_workflow.as_ref().and_then(|id| self.definitions.get(id))
    }

    /// List all registered workflows as (id, name, description) tuples
    pub fn list(&self) -> Vec<(&str, &str, &str)> {
        self.list_definitions()
    }

    /// Check if a workflow is active
    pub fn is_active(&self, id: &str) -> bool {
        self.active_workflow.as_ref().map(|s| s.as_str()) == Some(id)
    }

    // endregion: --- New API

    /// Activate a workflow
    ///
    /// This will:
    /// 1. Deactivate any currently active workflow
    /// 2. Look up the implementation for the current profile
    /// 3. Store current state for restoration
    /// 4. Enable all overlays for the new workflow
    /// 5. Apply REAPER settings
    pub fn activate(&mut self, id: &str) -> Result<(), String> {
        // Get workflow definition
        let definition = self.definitions.get(id)
            .ok_or_else(|| format!("Unknown workflow: {}", id))?;
        let name = definition.name;

        // Get current profile
        let current_profile = keybinds::active_preset_name();

        // Look up implementation for current profile
        let implementation = self.get_implementation(id, &current_profile)
            .ok_or_else(|| format!("No implementation for workflow '{}' with profile '{}'", id, current_profile))?
            .clone();

        // Deactivate current workflow first (if any)
        if self.active_workflow.is_some() {
            self.deactivate();
        }

        info!(
            id = %id,
            name = %name,
            profile = %current_profile,
            "Activating workflow"
        );

        // Store current state
        self.store_current_state(&implementation);

        // Enable keybind overlays
        for overlay in &implementation.keybind_overlays {
            if !keybinds::is_override_active(overlay) {
                keybinds::enable_override(overlay);
                debug!(overlay = %overlay, "Enabled keybind overlay");
            }
        }

        // Enable mouse modifier overlays
        for overlay in &implementation.mouse_overlays {
            if !mouse_manager::is_override_active(overlay) {
                mouse_manager::enable_override(overlay);
                debug!(overlay = %overlay, "Enabled mouse modifier overlay");
            }
        }

        // Apply REAPER settings
        self.apply_reaper_settings(&implementation.reaper_settings);

        // Arm the action if specified
        if let Some(ref armed) = implementation.armed_action {
            self.arm_action(armed);
        }

        // Add toolbar buttons
        info!(
            toolbar_api_available = toolbar::is_available(),
            button_count = implementation.toolbar_buttons.len(),
            "Adding toolbar buttons"
        );
        for button in &implementation.toolbar_buttons {
            match toolbar::add_button(button, id) {
                Ok(cmd_id) => {
                    info!(
                        button = %button.command_name,
                        label = %button.label,
                        toolbar = ?button.toolbar,
                        command_id = ?cmd_id,
                        "Added toolbar button"
                    );
                }
                Err(e) => {
                    warn!(
                        button = %button.command_name,
                        error = %e,
                        "Failed to add toolbar button"
                    );
                }
            }
        }

        // Store the active implementation for deactivation
        self.active_implementation = Some(implementation);
        self.active_workflow = Some(id.to_string());

        // Show notification
        let reaper = Reaper::get();
        reaper.show_console_msg(format!("Workflow activated: {}\n", name));

        info!(id = %id, "Workflow activated successfully");
        Ok(())
    }

    /// Deactivate the current workflow and restore previous state
    pub fn deactivate(&mut self) -> bool {
        let Some(id) = self.active_workflow.take() else {
            return false;
        };

        // Use the stored implementation (preferred) or fall back to definition
        let implementation = match self.active_implementation.take() {
            Some(impl_) => impl_,
            None => {
                warn!(id = %id, "No stored implementation, cannot fully deactivate");
                return false;
            }
        };

        // Get the workflow name from definition
        let name = self.definitions.get(&id)
            .map(|d| d.name)
            .unwrap_or("Unknown");

        info!(id = %id, name = %name, "Deactivating workflow");

        // Remove toolbar buttons added by this workflow
        if let Err(e) = toolbar::remove_workflow_buttons(&id) {
            warn!(
                workflow = %id,
                error = %e,
                "Failed to remove toolbar buttons"
            );
        }

        // Disable keybind overlays that the workflow enabled
        for overlay in &implementation.keybind_overlays {
            // Only disable if it wasn't active before
            if !self.stored_state.keybind_overlays.contains(&overlay.to_string()) {
                keybinds::disable_override(overlay);
                debug!(overlay = %overlay, "Disabled keybind overlay");
            }
        }

        // Disable mouse modifier overlays that the workflow enabled
        for overlay in &implementation.mouse_overlays {
            if !self.stored_state.mouse_overlays.contains(&overlay.to_string()) {
                mouse_manager::disable_override(overlay);
                debug!(overlay = %overlay, "Disabled mouse modifier overlay");
            }
        }

        // Restore REAPER settings
        self.restore_reaper_settings(&implementation.reaper_settings);

        // Disarm and restore previous armed command
        if implementation.armed_action.is_some() {
            self.disarm_and_restore();
        }

        // Clear stored state
        self.stored_state = StoredState::default();

        // Show notification
        let reaper = Reaper::get();
        reaper.show_console_msg(format!("Workflow deactivated: {}\n", name));

        info!(id = %id, "Workflow deactivated successfully");
        true
    }

    /// Toggle a workflow on/off
    pub fn toggle(&mut self, id: &str) -> Result<bool, String> {
        let currently_active = self.is_active(id);
        debug!(
            id = %id,
            currently_active = currently_active,
            active_workflow = ?self.active_workflow,
            "Toggle workflow called"
        );

        if currently_active {
            debug!(id = %id, "Workflow is active, deactivating...");
            self.deactivate();
            Ok(false)
        } else {
            debug!(id = %id, "Workflow is not active, activating...");
            self.activate(id)?;
            Ok(true)
        }
    }

    /// Store current state before activating a workflow
    fn store_current_state(&mut self, implementation: &WorkflowImplementation) {
        // Note: We would need to query current overlay states
        // For now, we'll just track what the workflow enables
        // A more complete implementation would query keybinds::get_active_overrides() etc.

        self.stored_state = StoredState::default();

        // Store REAPER toggle states
        for setting in &implementation.reaper_settings {
            if let Some(current) = self.get_reaper_toggle_state(setting.command) {
                self.stored_state.reaper_settings.insert(setting.command.to_string(), current);
            }
        }
    }

    /// Apply REAPER settings for a workflow
    fn apply_reaper_settings(&self, settings: &[ReaperSetting]) {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();

        for setting in settings {
            // Get current state
            let current = self.get_reaper_toggle_state(setting.command);

            // Only toggle if state doesn't match desired
            if current != Some(setting.enabled) {
                // Try to run the command
                if let Some(cmd_id) = self.resolve_command_id(setting.command) {
                    debug!(
                        setting = %setting.name,
                        command = %setting.command,
                        desired = setting.enabled,
                        "Toggling REAPER setting"
                    );
                    unsafe {
                        medium.low().Main_OnCommand(cmd_id, 0);
                    }
                } else {
                    warn!(command = %setting.command, "Could not resolve command ID");
                }
            }
        }
    }

    /// Restore REAPER settings to their previous state
    fn restore_reaper_settings(&self, settings: &[ReaperSetting]) {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();

        for setting in settings {
            if let Some(&original) = self.stored_state.reaper_settings.get(setting.command) {
                let current = self.get_reaper_toggle_state(setting.command);

                // Only toggle if state doesn't match original
                if current != Some(original) {
                    if let Some(cmd_id) = self.resolve_command_id(setting.command) {
                        debug!(
                            setting = %setting.name,
                            command = %setting.command,
                            restoring_to = original,
                            "Restoring REAPER setting"
                        );
                        unsafe {
                            medium.low().Main_OnCommand(cmd_id, 0);
                        }
                    }
                }
            }
        }
    }

    /// Get the current toggle state of a REAPER command
    fn get_reaper_toggle_state(&self, command: &str) -> Option<bool> {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();

        let cmd_id = self.resolve_command_id(command)?;

        unsafe {
            let state = medium.low().GetToggleCommandState(cmd_id);
            // -1 = not a toggle, 0 = off, 1 = on
            if state >= 0 {
                Some(state == 1)
            } else {
                None
            }
        }
    }

    /// Resolve a command string to a command ID
    fn resolve_command_id(&self, command: &str) -> Option<i32> {
        // Try parsing as numeric first
        if let Ok(id) = command.parse::<i32>() {
            return Some(id);
        }

        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();

        // Try using the action registry's stored command IDs first
        if let Some(cmd_id) = crate::infrastructure::action_registry::get_command_id(command) {
            return Some(cmd_id.get() as i32);
        }

        // Try as named command with NamedCommandLookup
        if let Some(cmd_id) = medium.named_command_lookup(command) {
            return Some(cmd_id.get() as i32);
        }

        // Try with underscore prefix (REAPER sometimes uses this format)
        let underscore_name = format!("_{}", command);
        if let Some(cmd_id) = medium.named_command_lookup(underscore_name.as_str()) {
            return Some(cmd_id.get() as i32);
        }

        None
    }

    /// Arm an action (it will run on next click/mouse action)
    ///
    /// Note: Not all actions support REAPER's arm mechanism. Only actions designed
    /// for mouse-position operations (like "Insert media item at mouse position")
    /// are typically armable. For actions that don't support native arming, use
    /// `.with_click_intercept()` on the ArmedAction to intercept clicks instead.
    fn arm_action(&mut self, action: &ArmedAction) {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();

        // First, store the currently armed command (if any)
        self.stored_state.previous_armed = self.get_armed_command();

        // Resolve the command ID
        if let Some(cmd_id) = self.resolve_command_id(action.command) {
            use std::ffi::CString;
            let section = CString::new(action.section).unwrap_or_default();

            debug!(
                action = %action.name,
                command = %action.command,
                cmd_id = cmd_id,
                section = %action.section,
                "About to arm action"
            );

            // Try direct ArmCommand API
            unsafe {
                medium.low().ArmCommand(cmd_id, section.as_ptr());
            }

            // Verify arming worked
            if let Some((armed_id, armed_section)) = self.get_armed_command() {
                info!(
                    action = %action.name,
                    command = %action.command,
                    cmd_id = cmd_id,
                    armed_id = armed_id,
                    armed_section = %armed_section,
                    "Armed action for workflow (verified)"
                );
            } else {
                // Arming didn't work - this is expected for most custom actions
                if action.intercept_clicks {
                    info!(
                        action = %action.name,
                        cmd_id = cmd_id,
                        "Action not armable via REAPER API, using click interception instead"
                    );
                } else {
                    warn!(
                        action = %action.name,
                        cmd_id = cmd_id,
                        "Action not armable - use keybinds or add .with_click_intercept()"
                    );
                }
            }
        } else {
            warn!(command = %action.command, "Could not resolve command ID for arming");
        }
    }

    /// Disarm and restore previously armed command
    fn disarm_and_restore(&mut self) {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();

        // Disarm current
        unsafe {
            let empty = std::ffi::CString::new("").unwrap();
            medium.low().ArmCommand(0, empty.as_ptr());
        }
        debug!("Disarmed action");

        // Restore previous if there was one
        if let Some((prev_cmd, prev_section)) = self.stored_state.previous_armed.take() {
            if prev_cmd != 0 {
                use std::ffi::CString;
                let section = CString::new(prev_section).unwrap_or_default();

                unsafe {
                    medium.low().ArmCommand(prev_cmd, section.as_ptr());
                }
                debug!(cmd_id = prev_cmd, "Restored previously armed command");
            }
        }
    }

    /// Get the currently armed command (cmd_id, section)
    fn get_armed_command(&self) -> Option<(i32, String)> {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();

        let mut section_buf = vec![0i8; 256];

        unsafe {
            let cmd_id = medium.low().GetArmedCommand(
                section_buf.as_mut_ptr(),
                section_buf.len() as i32,
            );

            if cmd_id == 0 {
                return None;
            }

            // Convert section buffer to string
            let null_pos = section_buf
                .iter()
                .position(|&b| b == 0)
                .unwrap_or(section_buf.len());
            let section_bytes: Vec<u8> = section_buf[..null_pos]
                .iter()
                .map(|&b| b as u8)
                .collect();
            let section = String::from_utf8(section_bytes).unwrap_or_default();

            Some((cmd_id, section))
        }
    }
}

impl Default for WorkflowManager {
    fn default() -> Self {
        Self::new()
    }
}

// === Global Manager Instance ===

static MANAGER: OnceLock<RwLock<WorkflowManager>> = OnceLock::new();

/// Get the global workflow manager (lazily initialized with defaults)
pub fn get_manager() -> &'static RwLock<WorkflowManager> {
    MANAGER.get_or_init(|| {
        let mut manager = WorkflowManager::new();

        // Register built-in workflows
        register_default_workflows(&mut manager);

        info!(
            definitions = manager.definitions.len(),
            default_impls = manager.default_implementations.len(),
            profile_impls = manager.profile_implementations.len(),
            "Workflow manager initialized"
        );

        RwLock::new(manager)
    })
}

/// Initialize the workflow manager
pub fn init() {
    let _ = get_manager();
}

/// Activate a workflow by ID
pub fn activate(id: &str) -> Result<(), String> {
    let mut manager = get_manager().write().unwrap();
    manager.activate(id)
}

/// Deactivate the current workflow
pub fn deactivate() -> bool {
    let mut manager = get_manager().write().unwrap();
    manager.deactivate()
}

/// Toggle a workflow on/off
pub fn toggle(id: &str) -> Result<bool, String> {
    let mut manager = get_manager().write().unwrap();
    manager.toggle(id)
}

/// Check if a workflow is active
pub fn is_active(id: &str) -> bool {
    let manager = get_manager().read().unwrap();
    manager.is_active(id)
}

/// Get the active workflow name (if any)
pub fn active_workflow_name() -> Option<String> {
    let manager = get_manager().read().unwrap();
    manager.active_definition().map(|d| d.name.to_string())
}

/// Get the active workflow name (alias for active_workflow_name)
pub fn get_active_workflow_name() -> Option<String> {
    active_workflow_name()
}

/// Get the active workflow display name (short name if applicable)
///
/// Uses the short name if the full name is > 12 chars with no spaces.
pub fn get_active_workflow_display_name() -> Option<String> {
    let manager = get_manager().read().unwrap();
    manager.active_definition().map(|d| d.display_name().to_string())
}

/// Get the active workflow ID (if any)
pub fn get_active_workflow_id() -> Option<String> {
    let manager = get_manager().read().unwrap();
    manager.active_workflow.clone()
}

/// List all registered workflows
/// Returns a list of (id, name, description) tuples
pub fn list_workflows() -> Vec<(String, String, String)> {
    let manager = get_manager().read().unwrap();
    manager.list()
        .iter()
        .map(|(id, name, desc)| (id.to_string(), name.to_string(), desc.to_string()))
        .collect()
}

/// Get the click action for the active workflow (if any)
/// Only returns an action if click interception is enabled for the armed action
/// This is used by the mouse hook to trigger actions on left-click
/// DEPRECATED: Use get_armed_click_action() instead
pub fn get_click_action() -> Option<String> {
    let manager = get_manager().read().unwrap();
    manager.active_implementation
        .as_ref()
        .and_then(|impl_| impl_.armed_action.as_ref())
        .filter(|a| a.intercept_clicks)
        .map(|a| a.command.to_string())
}

/// Get the armed click action for the active workflow (if any)
/// This is the preferred method for click interception - it returns the
/// ArmedClickAction which includes context matching logic
pub fn get_armed_click_action() -> Option<ArmedClickAction> {
    let manager = get_manager().read().unwrap();
    manager.active_implementation
        .as_ref()
        .and_then(|impl_| impl_.armed_click.clone())
}

// === Built-in Workflows ===

fn register_default_workflows(manager: &mut WorkflowManager) {
    // ============================================
    // Tempo Mapping Workflow
    // ============================================

    // Define the workflow (profile-agnostic)
    manager.define(
        "tempo_mapping",
        "Tempo Mapping",
        "Workflow for tempo mapping audio to the grid"
    );

    // Default implementation (used by any profile without a specific override)
    manager.implement(
        "tempo_mapping",
        WorkflowImplementation::new()
            .with_keybind_overlay("tempo_map")
            .with_mouse_overlay("tempo_map")
    );

    // Logic profile: uses different overlays
    manager.implement_for_profile(
        "tempo_mapping",
        "logic",
        WorkflowImplementation::new()
            .with_keybind_overlay("tempo_map_logic")
            .with_mouse_overlay("tempo_map")
    );

    // FastTrackStudio profile: uses FTS-specific overlays
    manager.implement_for_profile(
        "tempo_mapping",
        "fastrackstudio",
        WorkflowImplementation::new()
            .with_keybind_overlay("tempo_map_fts")
            .with_mouse_overlay("tempo_map")
    );

    // ============================================
    // Fast Slip Edit Workflow
    // ============================================

    // Define the workflow
    manager.define(
        "fast_slip_edit",
        "Fast Slip Edit",
        "Quick slip editing workflow for tight timing adjustments"
    );

    // Default implementation
    manager.implement(
        "fast_slip_edit",
        WorkflowImplementation::new()
            .with_keybind_overlay("fast_slip_edit")
            .with_mouse_overlay("quick_edit")
            // Disable snapping for precise slip editing
            .with_setting(ReaperSetting::off("Snapping", "1157"))
            // Enable auto-crossfade on split for seamless edits
            .with_setting(ReaperSetting::on("Auto-crossfade on split", "40912"))
            // Custom click interception - clicking on items triggers split with crossfade
            .with_armed_click(ArmedClickAction::on_item("FTS_SPLIT_ITEMS_CROSSFADE_LEFT"))
    );

    // Note: Profiles can add their own implementations here:
    // manager.implement_for_profile("fast_slip_edit", "logic", ...);
}
