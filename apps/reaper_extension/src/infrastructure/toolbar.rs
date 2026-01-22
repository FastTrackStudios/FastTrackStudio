//! Dynamic Toolbar Manager
//!
//! Manages toolbar buttons that can be added/removed at runtime.
//! Integrates with the workflow system for workflow-specific toolbars.
//!
//! ## Requirements
//!
//! Requires REAPER >= 711+dev0305 for the dynamic toolbar API.
//! On older versions, toolbar functions will gracefully degrade (no-op).
//!
//! ## Default Toolbar
//!
//! FTS uses Floating toolbar 32 as the default workflow toolbar.
//!
//! ## Usage
//!
//! ```rust
//! use crate::infrastructure::toolbar::{ToolbarButton, ToolbarTarget, add_button, remove_button};
//!
//! // Add a button to the FTS toolbar (Floating toolbar 32)
//! let button = ToolbarButton::new("FTS_MY_ACTION", "My Action");
//! add_button(&button, "my_workflow")?;
//!
//! // Add a button with icon to main toolbar
//! let button = ToolbarButton::main("FTS_OTHER", "Other")
//!     .with_icon("toolbar_other.png");
//! add_button(&button, "my_workflow")?;
//!
//! // Remove when done
//! remove_button(&ToolbarTarget::Floating(32), "FTS_MY_ACTION")?;
//! ```

use crate::input::error::{lock_mut, lock_read};
use reaper_high::Reaper;
use reaper_medium::{CommandId, MenuOrToolbarItem, PositionDescriptor, UiRefreshBehavior};
use std::collections::{HashMap, VecDeque};
use std::sync::{Mutex, OnceLock};
use tracing::{debug, info, warn};

// region: --- Deferred Operations

/// Deferred toolbar operation to be executed on next timer tick
#[derive(Debug, Clone)]
enum DeferredOp {
    Add {
        button: ToolbarButton,
        workflow_id: String,
    },
    /// Add at a specific position (for updates that preserve position)
    AddAtPosition {
        button: ToolbarButton,
        workflow_id: String,
        position: u32,
    },
    Remove {
        toolbar: ToolbarTarget,
        command_name: String,
    },
    RemoveWorkflow {
        workflow_id: String,
    },
    /// Update a button's label in-place (remove and re-add at same position)
    Update {
        button: ToolbarButton,
        workflow_id: String,
    },
}

static DEFERRED_OPS: OnceLock<Mutex<VecDeque<DeferredOp>>> = OnceLock::new();

fn get_deferred_ops() -> &'static Mutex<VecDeque<DeferredOp>> {
    DEFERRED_OPS.get_or_init(|| Mutex::new(VecDeque::new()))
}

/// Queue a deferred operation
fn queue_op(op: DeferredOp) {
    if let Ok(mut ops) = get_deferred_ops().lock() {
        ops.push_back(op);
    }
}

/// Process all pending deferred operations
/// Call this from the timer callback
pub fn process_deferred_ops() {
    let ops: Vec<DeferredOp> = {
        if let Ok(mut queue) = get_deferred_ops().lock() {
            queue.drain(..).collect()
        } else {
            return;
        }
    };

    for op in ops {
        match op {
            DeferredOp::Add {
                button,
                workflow_id,
            } => {
                if let Err(e) = add_button_immediate(&button, &workflow_id) {
                    warn!(error = %e, "Deferred toolbar add failed");
                }
            }
            DeferredOp::AddAtPosition {
                button,
                workflow_id,
                position,
            } => {
                if let Err(e) = add_button_at_position_immediate(&button, &workflow_id, position) {
                    warn!(error = %e, "Deferred toolbar add at position failed");
                }
            }
            DeferredOp::Remove {
                toolbar,
                command_name,
            } => {
                if let Err(e) = remove_button_immediate(&toolbar, &command_name) {
                    warn!(error = %e, "Deferred toolbar remove failed");
                }
            }
            DeferredOp::RemoveWorkflow { workflow_id } => {
                if let Err(e) = remove_workflow_buttons_immediate(&workflow_id) {
                    warn!(error = %e, "Deferred workflow toolbar remove failed");
                }
            }
            DeferredOp::Update {
                button,
                workflow_id,
            } => {
                if let Err(e) = update_button_immediate(&button, &workflow_id) {
                    warn!(error = %e, "Deferred toolbar update failed");
                }
            }
        }
    }
}

// endregion: --- Deferred Operations

// region: --- Types

/// Toolbar button flags for special display modes
/// Note: These values are experimental - REAPER may use icon filename for text modes
pub mod flags {
    /// No special flags (default button)
    pub const NORMAL: u32 = 0;
}

/// Special icon values for text-based toolbar buttons
/// In REAPER, "text" and "text_wide" are special icon names, not actual files
pub mod icons {
    /// Text icon (no image, just text label)
    pub const TEXT: &str = "text";
    /// Double-wide text icon (wider button for longer labels)
    pub const TEXT_WIDE: &str = "text_wide";
}

/// Toolbar button definition
#[derive(Debug, Clone)]
pub struct ToolbarButton {
    /// Command name (registered action ID, e.g., "FTS_MY_ACTION")
    pub command_name: String,
    /// Display label shown on the button
    pub label: String,
    /// Optional icon filename (e.g., "toolbar_my_action.png")
    /// Icons should be placed in REAPER's Data/toolbar_icons/ directory
    pub icon: Option<String>,
    /// Target toolbar
    pub toolbar: ToolbarTarget,
    /// Toolbar flags (see `flags` module for constants)
    pub toolbar_flags: u32,
}

impl ToolbarButton {
    /// Create a new toolbar button for the Main toolbar
    /// Note: Floating toolbars are not reliably supported by REAPER's dynamic toolbar API
    pub fn new(command_name: impl Into<String>, label: impl Into<String>) -> Self {
        Self {
            command_name: command_name.into(),
            label: label.into(),
            icon: None,
            toolbar: ToolbarTarget::Main,
            toolbar_flags: flags::NORMAL,
        }
    }

    /// Create a new toolbar button for the main toolbar
    pub fn main(command_name: impl Into<String>, label: impl Into<String>) -> Self {
        Self {
            command_name: command_name.into(),
            label: label.into(),
            icon: None,
            toolbar: ToolbarTarget::Main,
            toolbar_flags: flags::NORMAL,
        }
    }

    /// Create a new toolbar button with an icon
    pub fn with_icon(mut self, icon: impl Into<String>) -> Self {
        self.icon = Some(icon.into());
        self
    }

    /// Set the target toolbar
    pub fn on_toolbar(mut self, toolbar: ToolbarTarget) -> Self {
        self.toolbar = toolbar;
        self
    }

    /// Make this a text icon (no image, just text label)
    /// Uses special icon name "text" recognized by REAPER
    pub fn text_icon(mut self) -> Self {
        self.icon = Some(icons::TEXT.to_string());
        self
    }

    /// Make this a double-wide text button (for longer labels)
    /// Uses special icon name "text_wide" recognized by REAPER
    pub fn double_wide(mut self) -> Self {
        self.icon = Some(icons::TEXT_WIDE.to_string());
        self
    }

    /// Set custom toolbar flags (experimental)
    pub fn with_flags(mut self, flags: u32) -> Self {
        self.toolbar_flags = flags;
        self
    }
}

/// Which toolbar to target
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum ToolbarTarget {
    /// Main toolbar
    #[default]
    Main,
    /// Floating toolbar (1-32)
    Floating(u8),
}

impl ToolbarTarget {
    /// Get the REAPER toolbar name string
    pub fn as_str(&self) -> String {
        match self {
            Self::Main => "Main toolbar".to_string(),
            Self::Floating(n) => format!("Floating toolbar {}", (*n).max(1).min(32)),
        }
    }

    /// Parse from a toolbar name string
    pub fn from_str(s: &str) -> Option<Self> {
        if s == "Main toolbar" {
            Some(Self::Main)
        } else if s.starts_with("Floating toolbar ") {
            let n: u8 = s[17..].parse().ok()?;
            if (1..=32).contains(&n) {
                Some(Self::Floating(n))
            } else {
                None
            }
        } else {
            None
        }
    }
}

/// Internal state tracking for added buttons
struct ToolbarState {
    /// Map: (toolbar_name, command_name) -> workflow_id that added it
    added_buttons: HashMap<(String, String), String>,
}

impl Default for ToolbarState {
    fn default() -> Self {
        Self {
            added_buttons: HashMap::new(),
        }
    }
}

// endregion: --- Types

// region: --- Global State

static TOOLBAR_STATE: OnceLock<Mutex<ToolbarState>> = OnceLock::new();

fn get_state() -> &'static Mutex<ToolbarState> {
    TOOLBAR_STATE.get_or_init(|| Mutex::new(ToolbarState::default()))
}

// endregion: --- Global State

// region: --- Public API

/// Check if the dynamic toolbar API is available (REAPER >= 711+dev0305)
pub fn is_available() -> bool {
    Reaper::get()
        .medium_reaper()
        .low()
        .pointers()
        .GetCustomMenuOrToolbarItem
        .is_some()
}

/// Add a button to a toolbar (deferred - executes on next timer tick)
///
/// This queues the operation to avoid deadlocks when called from REAPER callbacks.
/// The actual button will appear after the next timer tick.
///
/// # Arguments
/// * `button` - The toolbar button definition
/// * `workflow_id` - ID of the workflow adding this button (for cleanup tracking)
pub fn add_button(button: &ToolbarButton, workflow_id: &str) -> Result<CommandId, String> {
    if !is_available() {
        return Err("Dynamic toolbar API not available (requires REAPER >= 711+dev0305)".into());
    }

    // Resolve command ID now to return it (and validate the command exists)
    let reaper = Reaper::get();
    let action = reaper.action_by_command_name(button.command_name.as_str());
    let command_id = action
        .command_id()
        .map_err(|e| format!("Command not found: {} - {}", button.command_name, e))?;

    info!(
        command = %button.command_name,
        toolbar = ?button.toolbar,
        "Queuing toolbar button add (deferred)"
    );

    queue_op(DeferredOp::Add {
        button: button.clone(),
        workflow_id: workflow_id.to_string(),
    });

    Ok(command_id)
}

/// Add a button to a toolbar (immediate - use with caution)
///
/// This executes immediately and may cause deadlocks if called from certain REAPER callbacks.
/// Prefer `add_button` which defers the operation.
fn add_button_immediate(button: &ToolbarButton, workflow_id: &str) -> Result<CommandId, String> {
    let toolbar_name = button.toolbar.as_str();
    info!(
        command = %button.command_name,
        toolbar_name = %toolbar_name,
        toolbar_target = ?button.toolbar,
        "add_button_immediate: start"
    );

    if !is_available() {
        return Err("Dynamic toolbar API not available (requires REAPER >= 711+dev0305)".into());
    }

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();

    // Resolve command ID from command name
    let action = reaper.action_by_command_name(button.command_name.as_str());
    let command_id = action
        .command_id()
        .map_err(|e| format!("Command not found: {} - {}", button.command_name, e))?;

    info!(
        command = %button.command_name,
        toolbar = %toolbar_name,
        command_id = ?command_id,
        "add_button: scanning toolbar for existing button"
    );

    // Check if already on toolbar
    if let Some(_pos) = scan_toolbar_for_command(&toolbar_name, command_id) {
        debug!(
            command = %button.command_name,
            toolbar = %toolbar_name,
            "Button already exists on toolbar, tracking only"
        );
        // Already exists - just track it
        lock_mut(get_state(), |state| {
            state.added_buttons.insert(
                (toolbar_name.clone(), button.command_name.clone()),
                workflow_id.to_string(),
            );
        });
        return Ok(command_id);
    }

    // Build icon path if provided
    let icon_path = button.icon.as_ref().map(|i| camino::Utf8Path::new(i));

    info!(
        command = %button.command_name,
        toolbar = %toolbar_name,
        label = %button.label,
        "add_button: scan complete, calling REAPER API to add button"
    );

    // Add to toolbar
    // Safe to use Refresh since we're in the timer callback (deferred context)
    let result = medium.add_custom_menu_or_toolbar_item_command(
        toolbar_name.as_str(),
        PositionDescriptor::Append,
        command_id,
        button.toolbar_flags,
        button.label.as_str(),
        icon_path,
        UiRefreshBehavior::Refresh,
    );

    match &result {
        Ok(()) => info!(
            command = %button.command_name,
            toolbar = %toolbar_name,
            "add_button: REAPER API call succeeded"
        ),
        Err(e) => warn!(
            command = %button.command_name,
            toolbar = %toolbar_name,
            error = %e,
            "add_button: REAPER API call FAILED"
        ),
    }

    result.map_err(|e| format!("Failed to add toolbar button: {}", e))?;

    debug!(
        command = %button.command_name,
        toolbar = %toolbar_name,
        label = %button.label,
        "Added toolbar button"
    );

    // Track it
    lock_mut(get_state(), |state| {
        state.added_buttons.insert(
            (toolbar_name, button.command_name.clone()),
            workflow_id.to_string(),
        );
    });

    Ok(command_id)
}

/// Add a button at a specific position (immediate)
fn add_button_at_position_immediate(
    button: &ToolbarButton,
    workflow_id: &str,
    position: u32,
) -> Result<CommandId, String> {
    let toolbar_name = button.toolbar.as_str();

    if !is_available() {
        return Err("Dynamic toolbar API not available".into());
    }

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();

    // Resolve command ID
    let action = reaper.action_by_command_name(button.command_name.as_str());
    let command_id = action
        .command_id()
        .map_err(|e| format!("Command not found: {} - {}", button.command_name, e))?;

    // Build icon path if provided
    let icon_path = button.icon.as_ref().map(|i| camino::Utf8Path::new(i));

    debug!(
        command = %button.command_name,
        toolbar = %toolbar_name,
        position = position,
        "Adding toolbar button at position"
    );

    // Add at specific position
    medium
        .add_custom_menu_or_toolbar_item_command(
            toolbar_name.as_str(),
            PositionDescriptor::AtPos(position),
            command_id,
            button.toolbar_flags,
            button.label.as_str(),
            icon_path,
            UiRefreshBehavior::Refresh,
        )
        .map_err(|e| format!("Failed to add toolbar button: {}", e))?;

    // Track it
    lock_mut(get_state(), |state| {
        state.added_buttons.insert(
            (toolbar_name, button.command_name.clone()),
            workflow_id.to_string(),
        );
    });

    Ok(command_id)
}

/// Update a button's label in-place (immediate)
/// Removes and re-adds at the same position to preserve ordering
fn update_button_immediate(button: &ToolbarButton, workflow_id: &str) -> Result<CommandId, String> {
    let toolbar_name = button.toolbar.as_str();

    if !is_available() {
        return Err("Dynamic toolbar API not available".into());
    }

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();

    // Resolve command ID
    let action = reaper.action_by_command_name(button.command_name.as_str());
    let command_id = action
        .command_id()
        .map_err(|e| format!("Command not found: {} - {}", button.command_name, e))?;

    // Find current position
    let position = scan_toolbar_for_command(&toolbar_name, command_id);

    if let Some(pos) = position {
        // Remove from current position (no refresh yet)
        medium
            .delete_custom_menu_or_toolbar_item(
                toolbar_name.as_str(),
                pos,
                UiRefreshBehavior::NoRefresh,
            )
            .map_err(|e| format!("Failed to remove toolbar button: {}", e))?;

        // Re-add at same position with new label
        let icon_path = button.icon.as_ref().map(|i| camino::Utf8Path::new(i));

        medium
            .add_custom_menu_or_toolbar_item_command(
                toolbar_name.as_str(),
                PositionDescriptor::AtPos(pos),
                command_id,
                button.toolbar_flags,
                button.label.as_str(),
                icon_path,
                UiRefreshBehavior::Refresh,
            )
            .map_err(|e| format!("Failed to add toolbar button: {}", e))?;

        debug!(
            command = %button.command_name,
            toolbar = %toolbar_name,
            position = pos,
            label = %button.label,
            "Updated toolbar button in-place"
        );
    } else {
        // Button doesn't exist yet, just add it
        return add_button_immediate(button, workflow_id);
    }

    // Update tracking
    lock_mut(get_state(), |state| {
        state.added_buttons.insert(
            (toolbar_name, button.command_name.clone()),
            workflow_id.to_string(),
        );
    });

    Ok(command_id)
}

/// Update a button's label in-place (deferred - preserves position)
///
/// Use this instead of remove + add when you want to update a button's label
/// without changing its position on the toolbar.
pub fn update_button(button: &ToolbarButton, workflow_id: &str) -> Result<(), String> {
    if !is_available() {
        return Err("Dynamic toolbar API not available".into());
    }

    debug!(
        command = %button.command_name,
        label = %button.label,
        "Queuing toolbar button update (deferred)"
    );

    queue_op(DeferredOp::Update {
        button: button.clone(),
        workflow_id: workflow_id.to_string(),
    });

    Ok(())
}

/// Remove a button from a toolbar (deferred - executes on next timer tick)
///
/// # Arguments
/// * `toolbar` - The target toolbar
/// * `command_name` - The command name of the button to remove
pub fn remove_button(toolbar: &ToolbarTarget, command_name: &str) -> Result<(), String> {
    if !is_available() {
        return Err("Dynamic toolbar API not available".into());
    }

    info!(
        command = %command_name,
        toolbar = ?toolbar,
        "Queuing toolbar button remove (deferred)"
    );

    queue_op(DeferredOp::Remove {
        toolbar: toolbar.clone(),
        command_name: command_name.to_string(),
    });

    Ok(())
}

/// Remove a button from a toolbar (immediate - use with caution)
fn remove_button_immediate(toolbar: &ToolbarTarget, command_name: &str) -> Result<(), String> {
    if !is_available() {
        return Err("Dynamic toolbar API not available".into());
    }

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();

    // Resolve command ID
    let action = reaper.action_by_command_name(command_name);
    let command_id = action
        .command_id()
        .map_err(|e| format!("Command not found: {} - {}", command_name, e))?;

    let toolbar_name = toolbar.as_str();

    // Find and remove the button
    if let Some(pos) = scan_toolbar_for_command(&toolbar_name, command_id) {
        medium
            .delete_custom_menu_or_toolbar_item(
                toolbar_name.as_str(),
                pos,
                UiRefreshBehavior::Refresh,
            )
            .map_err(|e| format!("Failed to remove toolbar button: {}", e))?;

        debug!(
            command = %command_name,
            toolbar = %toolbar_name,
            position = pos,
            "Removed toolbar button"
        );
    }

    // Remove from tracking
    lock_mut(get_state(), |state| {
        state
            .added_buttons
            .remove(&(toolbar_name, command_name.to_string()));
    });

    Ok(())
}

/// Remove all buttons added by a specific workflow (deferred)
///
/// This is called when a workflow is deactivated to clean up its toolbar buttons.
pub fn remove_workflow_buttons(workflow_id: &str) -> Result<(), String> {
    if !is_available() {
        return Ok(()); // No-op if API not available
    }

    info!(
        workflow = %workflow_id,
        "Queuing workflow toolbar buttons remove (deferred)"
    );

    queue_op(DeferredOp::RemoveWorkflow {
        workflow_id: workflow_id.to_string(),
    });

    Ok(())
}

/// Remove all buttons added by a specific workflow (immediate)
fn remove_workflow_buttons_immediate(workflow_id: &str) -> Result<(), String> {
    if !is_available() {
        return Ok(()); // No-op if API not available
    }

    // Collect buttons to remove (can't modify while iterating)
    let buttons_to_remove: Vec<(String, String)> = lock_read(get_state(), |state| {
        state
            .added_buttons
            .iter()
            .filter(|(_, wf)| *wf == workflow_id)
            .map(|((toolbar, cmd), _)| (toolbar.clone(), cmd.clone()))
            .collect()
    })
    .unwrap_or_default();

    if buttons_to_remove.is_empty() {
        return Ok(());
    }

    debug!(
        workflow = %workflow_id,
        count = buttons_to_remove.len(),
        "Removing workflow toolbar buttons"
    );

    // Remove each button
    for (toolbar_name, command_name) in buttons_to_remove {
        let target = ToolbarTarget::from_str(&toolbar_name).unwrap_or(ToolbarTarget::Main);

        if let Err(e) = remove_button_immediate(&target, &command_name) {
            warn!(
                command = %command_name,
                toolbar = %toolbar_name,
                error = %e,
                "Failed to remove toolbar button"
            );
        }
    }

    Ok(())
}

/// Check if a button is currently on a toolbar
pub fn button_exists(toolbar: &ToolbarTarget, command_name: &str) -> bool {
    if !is_available() {
        return false;
    }

    let reaper = Reaper::get();
    let action = reaper.action_by_command_name(command_name);
    let Ok(command_id) = action.command_id() else {
        return false;
    };

    scan_toolbar_for_command(&toolbar.as_str(), command_id).is_some()
}

/// Get all buttons currently tracked as added by workflows
pub fn get_tracked_buttons() -> Vec<(String, String, String)> {
    lock_read(get_state(), |state| {
        state
            .added_buttons
            .iter()
            .map(|((toolbar, cmd), wf)| (toolbar.clone(), cmd.clone(), wf.clone()))
            .collect()
    })
    .unwrap_or_default()
}

// endregion: --- Public API

// region: --- Support

/// Scan a toolbar for a specific command ID
/// Returns the position if found, None otherwise
fn scan_toolbar_for_command(toolbar_name: &str, command_id: CommandId) -> Option<u32> {
    let reaper = Reaper::get().medium_reaper();
    let mut pos = 0;

    loop {
        let result =
            reaper.get_custom_menu_or_toolbar_item(toolbar_name, pos, |item| match item? {
                MenuOrToolbarItem::Command(cmd) if cmd.command_id == command_id => Some(Some(pos)),
                _ => Some(None),
            });

        match result {
            None => return None,             // No more items
            Some(None) => pos += 1,          // Not this one, continue
            Some(Some(p)) => return Some(p), // Found it
        }
    }
}

// endregion: --- Support

// region: --- Tests

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_toolbar_target_as_str() {
        assert_eq!(ToolbarTarget::Main.as_str(), "Main toolbar");
        assert_eq!(ToolbarTarget::Floating(1).as_str(), "Floating toolbar 1");
        assert_eq!(ToolbarTarget::Floating(32).as_str(), "Floating toolbar 32");
        // Clamping
        assert_eq!(ToolbarTarget::Floating(0).as_str(), "Floating toolbar 1");
        assert_eq!(ToolbarTarget::Floating(99).as_str(), "Floating toolbar 32");
    }

    #[test]
    fn test_toolbar_target_from_str() {
        assert_eq!(
            ToolbarTarget::from_str("Main toolbar"),
            Some(ToolbarTarget::Main)
        );
        assert_eq!(
            ToolbarTarget::from_str("Floating toolbar 1"),
            Some(ToolbarTarget::Floating(1))
        );
        assert_eq!(
            ToolbarTarget::from_str("Floating toolbar 32"),
            Some(ToolbarTarget::Floating(32))
        );
        assert_eq!(ToolbarTarget::from_str("Floating toolbar 0"), None);
        assert_eq!(ToolbarTarget::from_str("Floating toolbar 33"), None);
        assert_eq!(ToolbarTarget::from_str("Unknown"), None);
    }

    #[test]
    fn test_toolbar_button_builder() {
        let button = ToolbarButton::main("FTS_TEST", "Test Button")
            .with_icon("toolbar_test.png")
            .on_toolbar(ToolbarTarget::Floating(5));

        assert_eq!(button.command_name, "FTS_TEST");
        assert_eq!(button.label, "Test Button");
        assert_eq!(button.icon, Some("toolbar_test.png".to_string()));
        assert_eq!(button.toolbar, ToolbarTarget::Floating(5));
        assert_eq!(button.toolbar_flags, flags::NORMAL);
    }

    #[test]
    fn test_toolbar_button_double_wide() {
        let button = ToolbarButton::new("FTS_TEST", "Test").double_wide();

        assert_eq!(button.toolbar_flags, flags::DOUBLE_WIDE);
    }

    #[test]
    fn test_toolbar_button_text_icon_double_wide() {
        let button = ToolbarButton::new("FTS_TEST", "Test")
            .text_icon()
            .double_wide();

        assert_eq!(button.toolbar_flags, flags::TEXT_ICON | flags::DOUBLE_WIDE);
    }
}

// endregion: --- Tests
