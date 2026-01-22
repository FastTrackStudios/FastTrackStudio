//! ProjectService - Roam service for project operations
//!
//! This service provides RPC access to project state and metadata.
//! Commands are executed via the service implementation (Mock or REAPER).

use facet::Facet;
use roam::Tx;

// region:    --- RPC Types

/// Simplified project information for RPC communication.
#[derive(Debug, Clone, Facet)]
pub struct ProjectInfo {
    /// Project name (used as identifier)
    pub name: String,
    /// Project description
    pub description: Option<String>,
    /// Path to the project file (if saved)
    pub path: Option<String>,
    /// Whether the project has unsaved changes
    pub is_dirty: bool,
    /// Number of tracks in the project
    pub track_count: usize,
    /// Number of markers in the project
    pub marker_count: usize,
    /// Number of regions in the project
    pub region_count: usize,
    /// Total project length in seconds
    pub length: f64,
    /// Sample rate (Hz)
    pub sample_rate: u32,
}

/// Commands that can be executed on projects
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum ProjectCommand {
    /// Create a new project
    New { name: String },
    /// Open a project from a file path
    Open { path: String },
    /// Save the current project
    Save,
    /// Save the current project to a new path
    SaveAs { path: String },
    /// Close the current project
    Close,
    /// Set the project name
    SetName { name: String },
    /// Set the project description
    SetDescription { description: String },
    /// Undo the last action
    Undo,
    /// Redo the last undone action
    Redo,
}

// endregion: --- RPC Types

// region:    --- Events

/// Events emitted by the project service
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum ProjectEvent {
    /// A project was opened/created
    ProjectOpened(ProjectInfo),
    /// The current project was closed
    ProjectClosed,
    /// Project properties changed
    ProjectChanged(ProjectInfo),
    /// Project was saved
    ProjectSaved { path: String },
    /// Project dirty state changed
    DirtyChanged { is_dirty: bool },
    /// Active project changed (for multi-project DAWs)
    ActiveProjectChanged(ProjectInfo),
}

// endregion: --- Events

// region:    --- Service Trait

/// ProjectService provides RPC access to project operations.
///
/// This trait defines the contract for project management services.
/// Implementations can be for REAPER, mock testing, or other DAWs.
#[roam::service]
pub trait ProjectService {
    /// Get information about the active project
    async fn get_active_project(&self) -> Option<ProjectInfo>;

    /// Get a list of all open projects (for multi-project DAWs)
    async fn get_open_projects(&self) -> Vec<ProjectInfo>;

    /// Get the project name
    async fn get_name(&self) -> Option<String>;

    /// Get the project path (if saved)
    async fn get_path(&self) -> Option<String>;

    /// Check if the project has unsaved changes
    async fn is_dirty(&self) -> bool;

    /// Get the total project length in seconds
    async fn get_length(&self) -> f64;

    /// Get the sample rate
    async fn get_sample_rate(&self) -> u32;

    /// Execute a project command
    async fn execute(&self, cmd: ProjectCommand);

    /// Subscribe to project events
    async fn subscribe(&self, events: Tx<ProjectEvent>);
}

// endregion: --- Service Trait
