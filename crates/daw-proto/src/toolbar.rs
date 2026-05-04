//! Toolbar Service — Dynamic toolbar management for DAW extensions.
//!
//! Extensions can add, update, and remove toolbar buttons in the host DAW.
//! Operations are deferred and applied on the main thread to avoid re-entrancy
//! issues inside DAW callbacks.

use facet::Facet;
use vox::service;

/// Target toolbar for button placement.
#[repr(u8)]
#[derive(Debug, Clone, Default, PartialEq, Eq, Facet)]
pub enum ToolbarTarget {
    /// Main toolbar.
    #[default]
    Main,
    /// Floating toolbar (1–32).
    Floating(u8),
    /// Floating MIDI toolbar (1–8).
    Midi(u8),
}

/// Where a toolbar item should be inserted.
#[repr(u8)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Facet)]
#[facet(rename_all = "snake_case")]
pub enum ToolbarPlacement {
    /// Append to the end of the toolbar.
    #[default]
    Append,
    /// Insert at a zero-based toolbar position.
    Position(u32),
}

/// How REAPER should resolve a toolbar icon value.
#[repr(u8)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Facet)]
#[facet(rename_all = "snake_case")]
pub enum ToolbarIconKind {
    /// A REAPER toolbar icon file name, for example `toolbar_custom.png`.
    #[default]
    FileName,
    /// A filesystem path to an icon file.
    Path,
}

/// Icon assigned to a toolbar button.
#[derive(Debug, Clone, Default, PartialEq, Eq, Facet)]
pub struct ToolbarIcon {
    /// Icon lookup strategy.
    pub kind: ToolbarIconKind,
    /// Icon file name or path.
    pub value: String,
}

/// A toolbar button to add or update.
#[derive(Debug, Clone, Default, Facet)]
pub struct ToolbarButton {
    /// REAPER command name (e.g., `_FTS_SIGNAL_OPEN_BROWSER`).
    pub command_name: String,
    /// Display label shown on the button.
    pub label: String,
    /// Optional icon.
    pub icon: Option<ToolbarIcon>,
    /// Which toolbar to place the button on.
    pub target: ToolbarTarget,
    /// Desired placement when adding or moving the button.
    pub placement: ToolbarPlacement,
    /// Toolbar button flags (bitmask).
    pub flags: u32,
}

/// Result of a toolbar operation.
#[derive(Debug, Clone, Default, Facet)]
pub struct ToolbarResult {
    /// Whether the operation was accepted by the host service.
    pub ok: bool,
    /// Resolved REAPER command ID, when applicable.
    pub command_id: Option<u32>,
    /// Error message when `ok` is false.
    pub error: Option<String>,
}

impl ToolbarResult {
    pub fn ok(command_id: u32) -> Self {
        Self {
            ok: true,
            command_id: Some(command_id),
            error: None,
        }
    }

    pub fn error(message: impl Into<String>) -> Self {
        Self {
            ok: false,
            command_id: None,
            error: Some(message.into()),
        }
    }
}

/// Source used to build a toolbar snapshot.
#[repr(u8)]
#[derive(Debug, Clone, Default, PartialEq, Eq, Facet)]
#[facet(rename_all = "snake_case")]
pub enum ToolbarSnapshotSource {
    /// Live REAPER API state.
    #[default]
    Live,
    /// Parsed `reaper-menu.ini` state.
    Config,
}

/// A toolbar snapshot.
#[derive(Debug, Clone, Default, Facet)]
pub struct ToolbarSnapshot {
    /// Toolbar name (for example, `Main toolbar`, `Floating toolbar 1`, or `Floating MIDI toolbar 1`).
    pub toolbar_name: String,
    /// Source used to build this snapshot.
    pub source: ToolbarSnapshotSource,
    /// Items in toolbar order.
    pub items: Vec<ToolbarItemInfo>,
}

/// A single toolbar item.
#[derive(Debug, Clone, Default, Facet)]
pub struct ToolbarItemInfo {
    /// Zero-based item position.
    pub position: u32,
    /// Item kind: `command`, `separator`, `submenu-start`, `submenu-end`, or `unknown`.
    pub kind: String,
    /// Numeric REAPER command ID for command items.
    pub command_id: Option<u32>,
    /// Named command ID where available (for example, `_SWS_ABOUT` or `_FTS_*`).
    pub command_name: Option<String>,
    /// Display label from the toolbar/config entry.
    pub label: String,
    /// Toolbar flags.
    pub flags: u32,
    /// Optional toolbar icon file name.
    pub icon: Option<String>,
    /// Raw config line value when parsed from `reaper-menu.ini`.
    pub raw: Option<String>,
}

/// Service for managing toolbar buttons in the host DAW.
///
/// Operations are queued and applied from the host's timer callback
/// to avoid re-entrancy issues.
#[service]
pub trait ToolbarService {
    /// Add a toolbar button. Returns the resolved command ID.
    ///
    /// If the button already exists, this is a no-op (returns existing ID).
    /// The `workflow_id` groups buttons for batch removal.
    async fn add_button(&self, button: ToolbarButton, workflow_id: String) -> ToolbarResult;

    /// Update an existing toolbar button (or add if not present).
    async fn update_button(&self, button: ToolbarButton, workflow_id: String) -> ToolbarResult;

    /// Remove a single toolbar button by command name and target.
    async fn remove_button(&self, target: ToolbarTarget, command_name: String) -> ToolbarResult;

    /// Move a toolbar button to a zero-based position.
    async fn move_button(
        &self,
        target: ToolbarTarget,
        command_name: String,
        position: u32,
    ) -> ToolbarResult;

    /// Set or clear a toolbar button icon while preserving label and flags.
    async fn set_button_icon(
        &self,
        target: ToolbarTarget,
        command_name: String,
        icon: Option<ToolbarIcon>,
    ) -> ToolbarResult;

    /// Remove all toolbar buttons belonging to a workflow.
    async fn remove_workflow_buttons(&self, workflow_id: String) -> ToolbarResult;

    /// Check if the dynamic toolbar API is available in the host.
    async fn is_available(&self) -> bool;

    /// List all tracked buttons: (toolbar_name, command_name, workflow_id).
    async fn get_tracked_buttons(&self) -> Vec<TrackedButton>;
}

/// A tracked toolbar button entry.
#[derive(Debug, Clone, Facet)]
pub struct TrackedButton {
    /// Toolbar name (e.g., "Main toolbar", "Floating toolbar 1", "Floating MIDI toolbar 1").
    pub toolbar_name: String,
    /// REAPER command name.
    pub command_name: String,
    /// Workflow that owns this button.
    pub workflow_id: String,
}
