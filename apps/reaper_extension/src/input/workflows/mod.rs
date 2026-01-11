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
//! Workflows can have variants per base profile (e.g., "Logic Tempo Mapping").

pub mod armed;

use crate::input::keybinds;
use crate::input::mouse_modifiers::manager as mouse_manager;
pub use armed::{
    ArmedClickAction, ArmedContext,
    detect_mouse_modifier_context, is_debug_mouse_context_enabled, toggle_debug_mouse_context,
};
use reaper_high::Reaper;
use std::collections::HashMap;
use std::sync::{OnceLock, RwLock};
use tracing::{debug, info, warn};

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

/// A complete workflow definition
#[derive(Debug, Clone)]
pub struct Workflow {
    /// Unique workflow ID (e.g., "tempo_mapping", "fast_slip_edit")
    pub id: &'static str,
    /// Human-readable name
    pub name: &'static str,
    /// Description
    pub description: &'static str,
    /// Base profile this workflow is designed for (None = works with any)
    pub base_profile: Option<&'static str>,
    /// Keybind overlays to enable
    pub keybind_overlays: Vec<&'static str>,
    /// Mouse modifier overlays to enable
    pub mouse_overlays: Vec<&'static str>,
    /// REAPER settings to apply
    pub reaper_settings: Vec<ReaperSetting>,
    /// Action to arm when workflow is active (uses REAPER's native arm - often doesn't work)
    pub armed_action: Option<ArmedAction>,
    /// Custom armed click action (intercepts left-clicks - preferred method)
    pub armed_click: Option<ArmedClickAction>,
}

impl Workflow {
    pub fn new(id: &'static str, name: &'static str, description: &'static str) -> Self {
        Self {
            id,
            name,
            description,
            base_profile: None,
            keybind_overlays: Vec::new(),
            mouse_overlays: Vec::new(),
            reaper_settings: Vec::new(),
            armed_action: None,
            armed_click: None,
        }
    }

    /// Set the base profile this workflow is for
    pub fn for_profile(mut self, profile: &'static str) -> Self {
        self.base_profile = Some(profile);
        self
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

    /// Set an action to arm when workflow is active (uses REAPER's native arm - often doesn't work)
    pub fn with_armed_action(mut self, action: ArmedAction) -> Self {
        self.armed_action = Some(action);
        self
    }

    /// Set a custom armed click action (intercepts left-clicks in arrange view)
    /// This is the preferred method as REAPER's native arming doesn't work for most actions
    pub fn with_armed_click(mut self, action: ArmedClickAction) -> Self {
        self.armed_click = Some(action);
        self
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

/// Manages workflows and their activation state
pub struct WorkflowManager {
    /// All registered workflows (id -> workflow)
    workflows: HashMap<String, Workflow>,
    /// Currently active workflow ID (only one at a time)
    active_workflow: Option<String>,
    /// Stored state for restoration
    stored_state: StoredState,
}

impl WorkflowManager {
    pub fn new() -> Self {
        Self {
            workflows: HashMap::new(),
            active_workflow: None,
            stored_state: StoredState::default(),
        }
    }

    /// Register a workflow
    pub fn register(&mut self, workflow: Workflow) {
        debug!(id = %workflow.id, name = %workflow.name, "Registering workflow");
        self.workflows.insert(workflow.id.to_string(), workflow);
    }

    /// Get all registered workflows
    pub fn all_workflows(&self) -> impl Iterator<Item = &Workflow> {
        self.workflows.values()
    }

    /// Get a workflow by ID
    pub fn get(&self, id: &str) -> Option<&Workflow> {
        self.workflows.get(id)
    }

    /// Get the currently active workflow
    pub fn active(&self) -> Option<&Workflow> {
        self.active_workflow.as_ref().and_then(|id| self.workflows.get(id))
    }

    /// Check if a workflow is active
    pub fn is_active(&self, id: &str) -> bool {
        self.active_workflow.as_ref().map(|s| s.as_str()) == Some(id)
    }

    /// Get the best workflow variant for the current base profile
    ///
    /// For example, if base profile is "logic" and we want "tempo_mapping":
    /// 1. First try "tempo_mapping_logic" (profile-specific variant)
    /// 2. Fall back to "tempo_mapping" (generic)
    pub fn find_workflow_for_profile(&self, workflow_base_id: &str, profile: &str) -> Option<&Workflow> {
        // Try profile-specific variant first
        let profile_specific = format!("{}_{}", workflow_base_id, profile);
        if let Some(w) = self.workflows.get(&profile_specific) {
            return Some(w);
        }

        // Try generic workflow (no base_profile or matching base_profile)
        if let Some(w) = self.workflows.get(workflow_base_id) {
            if w.base_profile.is_none() || w.base_profile == Some(profile) {
                return Some(w);
            }
        }

        None
    }

    /// Activate a workflow
    ///
    /// This will:
    /// 1. Deactivate any currently active workflow
    /// 2. Store current state for restoration
    /// 3. Enable all overlays for the new workflow
    /// 4. Apply REAPER settings
    pub fn activate(&mut self, id: &str) -> Result<(), String> {
        let workflow = self.workflows.get(id)
            .ok_or_else(|| format!("Unknown workflow: {}", id))?
            .clone();

        // Deactivate current workflow first (if any)
        if self.active_workflow.is_some() {
            self.deactivate();
        }

        info!(id = %id, name = %workflow.name, "Activating workflow");

        // Store current state
        self.store_current_state(&workflow);

        // Enable keybind overlays
        for overlay in &workflow.keybind_overlays {
            if !keybinds::is_override_active(overlay) {
                keybinds::enable_override(overlay);
                debug!(overlay = %overlay, "Enabled keybind overlay");
            }
        }

        // Enable mouse modifier overlays
        for overlay in &workflow.mouse_overlays {
            if !mouse_manager::is_override_active(overlay) {
                mouse_manager::enable_override(overlay);
                debug!(overlay = %overlay, "Enabled mouse modifier overlay");
            }
        }

        // Apply REAPER settings
        self.apply_reaper_settings(&workflow.reaper_settings);

        // Arm the action if specified
        if let Some(ref armed) = workflow.armed_action {
            self.arm_action(armed);
        }

        self.active_workflow = Some(id.to_string());

        // Show notification
        let reaper = Reaper::get();
        reaper.show_console_msg(format!("Workflow activated: {}\n", workflow.name));

        info!(id = %id, "Workflow activated successfully");
        Ok(())
    }

    /// Deactivate the current workflow and restore previous state
    pub fn deactivate(&mut self) -> bool {
        let Some(id) = self.active_workflow.take() else {
            return false;
        };

        let workflow = match self.workflows.get(&id) {
            Some(w) => w.clone(),
            None => {
                warn!(id = %id, "Cannot deactivate unknown workflow");
                return false;
            }
        };

        info!(id = %id, name = %workflow.name, "Deactivating workflow");

        // Disable keybind overlays that the workflow enabled
        for overlay in &workflow.keybind_overlays {
            // Only disable if it wasn't active before
            if !self.stored_state.keybind_overlays.contains(&overlay.to_string()) {
                keybinds::disable_override(overlay);
                debug!(overlay = %overlay, "Disabled keybind overlay");
            }
        }

        // Disable mouse modifier overlays that the workflow enabled
        for overlay in &workflow.mouse_overlays {
            if !self.stored_state.mouse_overlays.contains(&overlay.to_string()) {
                mouse_manager::disable_override(overlay);
                debug!(overlay = %overlay, "Disabled mouse modifier overlay");
            }
        }

        // Restore REAPER settings
        self.restore_reaper_settings(&workflow.reaper_settings);

        // Disarm and restore previous armed command
        if workflow.armed_action.is_some() {
            self.disarm_and_restore();
        }

        // Clear stored state
        self.stored_state = StoredState::default();

        // Show notification
        let reaper = Reaper::get();
        reaper.show_console_msg(format!("Workflow deactivated: {}\n", workflow.name));

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
    fn store_current_state(&mut self, workflow: &Workflow) {
        // Note: We would need to query current overlay states
        // For now, we'll just track what the workflow enables
        // A more complete implementation would query keybinds::get_active_overrides() etc.

        self.stored_state = StoredState::default();

        // Store REAPER toggle states
        for setting in &workflow.reaper_settings {
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

        info!("Workflow manager initialized with {} workflows", manager.workflows.len());

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
    manager.active().map(|w| w.name.to_string())
}

/// Get the click action for the active workflow (if any)
/// Only returns an action if click interception is enabled for the armed action
/// This is used by the mouse hook to trigger actions on left-click
/// DEPRECATED: Use get_armed_click_action() instead
pub fn get_click_action() -> Option<String> {
    let manager = get_manager().read().unwrap();
    manager
        .active()
        .and_then(|w| w.armed_action.as_ref())
        .filter(|a| a.intercept_clicks)
        .map(|a| a.command.to_string())
}

/// Get the armed click action for the active workflow (if any)
/// This is the preferred method for click interception - it returns the
/// ArmedClickAction which includes context matching logic
pub fn get_armed_click_action() -> Option<ArmedClickAction> {
    let manager = get_manager().read().unwrap();
    manager
        .active()
        .and_then(|w| w.armed_click.clone())
}

// === Built-in Workflows ===

fn register_default_workflows(manager: &mut WorkflowManager) {
    // Tempo Mapping workflow (generic - works with any base profile)
    manager.register(
        Workflow::new(
            "tempo_mapping",
            "Tempo Mapping",
            "Workflow for tempo mapping audio to the grid"
        )
        .with_keybind_overlay("tempo_map")
        .with_mouse_overlay("tempo_map")
        // Example REAPER settings that might be useful for tempo mapping:
        // .with_setting(ReaperSetting::on("Snap to grid", "40754"))
    );

    // Fast Slip Edit workflow (generic)
    manager.register(
        Workflow::new(
            "fast_slip_edit",
            "Fast Slip Edit",
            "Quick slip editing workflow for tight timing adjustments"
        )
        .with_keybind_overlay("fast_slip_edit") // s/x = split with crossfade
        .with_mouse_overlay("quick_edit")       // Item lower = move contents, crossfade = move fades
        // Disable snapping for precise slip editing
        .with_setting(ReaperSetting::off("Snapping", "1157"))
        // Enable auto-crossfade on split for seamless edits
        .with_setting(ReaperSetting::on("Auto-crossfade on split", "40912"))
        // Custom click interception - clicking on items triggers split with crossfade
        .with_armed_click(
            ArmedClickAction::on_item("FTS_SPLIT_ITEMS_CROSSFADE_LEFT")
        )
    );

    // Tempo Mapping for Logic users
    manager.register(
        Workflow::new(
            "tempo_mapping_logic",
            "Tempo Mapping (Logic)",
            "Tempo mapping with Logic-style behaviors"
        )
        .for_profile("logic")
        .with_keybind_overlay("tempo_map")
        .with_mouse_overlay("tempo_map")
    );

    // Tempo Mapping for FTS users
    manager.register(
        Workflow::new(
            "tempo_mapping_fastrackstudio",
            "Tempo Mapping (FTS)",
            "Tempo mapping with FastTrackStudio behaviors"
        )
        .for_profile("fastrackstudio")
        .with_keybind_overlay("tempo_map")
        .with_mouse_overlay("tempo_map")
    );
}
