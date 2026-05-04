//! FTS Screenset Service.
//!
//! Screensets are named, host-managed workspace snapshots. They are separate
//! from REAPER's built-in numbered screenset slots so FTS can support named
//! laptop/multi-monitor/workflow layouts and more than ten presets.

use facet::Facet;
use vox::service;

/// Where a screenset should be stored.
#[repr(u8)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Facet)]
#[facet(rename_all = "snake_case")]
pub enum ScreensetScope {
    /// Persist in the DAW profile/global extension state.
    #[default]
    Global,
    /// Persist with the current project when the backend supports it.
    Project,
}

/// Capture/apply options for a screenset operation.
#[derive(Debug, Clone, Default, Facet)]
pub struct ScreensetOptions {
    /// Storage scope.
    pub scope: ScreensetScope,
    /// Persist across host restarts when using global storage.
    pub persist: bool,
}

/// A known display in a screenset.
#[derive(Debug, Clone, Default, Facet)]
pub struct ScreensetMonitor {
    pub id: String,
    pub name: String,
    pub x: i32,
    pub y: i32,
    pub width: u32,
    pub height: u32,
    pub scale: f32,
    pub primary: bool,
}

/// Rectangle in screen coordinates.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Facet)]
pub struct ScreensetRect {
    pub x: i32,
    pub y: i32,
    pub width: u32,
    pub height: u32,
}

/// State for one managed panel/window.
#[derive(Debug, Clone, Default, Facet)]
pub struct ScreensetWindow {
    /// Stable window/panel id.
    pub id: String,
    /// Human-readable title.
    pub title: String,
    /// Monitor id this window belongs to, if known.
    pub monitor_id: Option<String>,
    /// Last known bounds.
    pub bounds: Option<ScreensetRect>,
    /// Whether the window should be visible after applying the screenset.
    pub visible: bool,
    /// REAPER docker id when the window is docked.
    pub dock_id: Option<i32>,
    /// True if the window is floating instead of docked.
    pub floating: bool,
}

/// Full FTS screenset snapshot.
#[derive(Debug, Clone, Default, Facet)]
pub struct Screenset {
    /// Stable id used by CLI/services.
    pub id: String,
    /// Display name.
    pub name: String,
    /// Free-form description.
    pub description: String,
    /// Version of the FTS screenset schema.
    pub schema_version: u32,
    /// Unix timestamp in seconds when this was captured/saved.
    pub updated_at_unix: u64,
    /// Workflow tags such as `laptop`, `mixing`, or `tracking`.
    pub tags: Vec<String>,
    /// Captured monitor topology.
    pub monitors: Vec<ScreensetMonitor>,
    /// Captured managed window/panel state.
    pub windows: Vec<ScreensetWindow>,
    /// Opaque dock-host layout blob for host-specific round trips.
    pub dock_layout_blob: Vec<u8>,
    /// Optional REAPER/extension actions to run after applying the layout.
    pub actions_on_apply: Vec<String>,
}

/// Compact row returned by list operations.
#[derive(Debug, Clone, Default, Facet)]
pub struct ScreensetSummary {
    pub id: String,
    pub name: String,
    pub description: String,
    pub updated_at_unix: u64,
    pub tags: Vec<String>,
    pub window_count: u32,
    pub monitor_count: u32,
    pub action_count: u32,
}

/// Result for mutation operations.
#[derive(Debug, Clone, Default, Facet)]
pub struct ScreensetResult {
    pub ok: bool,
    pub id: Option<String>,
    pub error: Option<String>,
}

impl ScreensetResult {
    pub fn ok(id: impl Into<String>) -> Self {
        Self {
            ok: true,
            id: Some(id.into()),
            error: None,
        }
    }

    pub fn error(message: impl Into<String>) -> Self {
        Self {
            ok: false,
            id: None,
            error: Some(message.into()),
        }
    }
}

/// Request for capturing a screenset.
#[derive(Debug, Clone, Default, Facet)]
pub struct CaptureScreensetRequest {
    pub id: String,
    pub name: String,
    pub description: String,
    pub tags: Vec<String>,
    pub actions_on_apply: Vec<String>,
    pub options: ScreensetOptions,
}

/// Service for named FTS screensets.
#[service]
pub trait ScreensetService {
    /// Capture the current managed screenset state into a named screenset.
    async fn capture_screenset(&self, request: CaptureScreensetRequest) -> ScreensetResult;

    /// Save an explicit screenset snapshot.
    async fn save_screenset(
        &self,
        screenset: Screenset,
        options: ScreensetOptions,
    ) -> ScreensetResult;

    /// List known screensets.
    async fn list_screensets(&self, options: ScreensetOptions) -> Vec<ScreensetSummary>;

    /// Load one screenset by id.
    async fn get_screenset(&self, id: String, options: ScreensetOptions) -> Option<Screenset>;

    /// Apply a screenset by id.
    async fn apply_screenset(&self, id: String, options: ScreensetOptions) -> ScreensetResult;

    /// Delete a screenset by id.
    async fn delete_screenset(&self, id: String, options: ScreensetOptions) -> ScreensetResult;
}
