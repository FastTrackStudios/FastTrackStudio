//! Application State Types for TypeScript Generation
//!
//! This module defines the complete application state structure that will be
//! exported to TypeScript, providing a single source of truth for all frontend state.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use ts_rs::TS;

use primitives::TimeSignature;
use project::ProjectError;
use transport::Transport;

/// Complete project state including transport and metadata
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct ProjectState {
    /// Project name
    pub name: String,
    /// Transport state
    pub transport: Transport,
    /// Project metadata
    pub metadata: ProjectMetadata,
    /// Whether this project is currently active
    pub is_active: bool,
}

/// Project metadata and settings
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct ProjectMetadata {
    /// When the project was created
    pub created_at: String, // ISO 8601 timestamp
    /// When the project was last modified
    pub modified_at: String, // ISO 8601 timestamp
    /// Project description
    pub description: Option<String>,
    /// Project tags
    pub tags: Vec<String>,
    /// Project sample rate
    pub sample_rate: u32,
    /// Project bit depth
    pub bit_depth: u16,
}

impl Default for ProjectMetadata {
    fn default() -> Self {
        let now = chrono::Utc::now().to_rfc3339();
        Self {
            created_at: now.clone(),
            modified_at: now,
            description: None,
            tags: Vec::new(),
            sample_rate: 44100,
            bit_depth: 24,
        }
    }
}

/// Application preferences and settings
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct AppPreferences {
    /// Theme preference
    pub theme: Theme,
    /// Auto-save interval in minutes (0 = disabled)
    pub auto_save_interval: u32,
    /// Default project settings
    pub default_project: ProjectDefaults,
    /// Audio settings
    pub audio: AudioSettings,
    /// Keyboard shortcuts
    pub shortcuts: KeyboardShortcuts,
}

impl Default for AppPreferences {
    fn default() -> Self {
        Self {
            theme: Theme::System,
            auto_save_interval: 5,
            default_project: ProjectDefaults::default(),
            audio: AudioSettings::default(),
            shortcuts: KeyboardShortcuts::default(),
        }
    }
}

/// Theme options
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum Theme {
    Light,
    Dark,
    System,
}

/// Default project settings
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct ProjectDefaults {
    /// Default tempo
    pub tempo: f64,
    /// Default time signature
    pub time_signature: TimeSignature,
    /// Default sample rate
    pub sample_rate: u32,
    /// Default bit depth
    pub bit_depth: u16,
}

impl Default for ProjectDefaults {
    fn default() -> Self {
        Self {
            tempo: 120.0,
            time_signature: TimeSignature::new(4, 4),
            sample_rate: 44100,
            bit_depth: 24,
        }
    }
}

/// Audio system settings
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct AudioSettings {
    /// Audio device name
    pub device_name: Option<String>,
    /// Buffer size
    pub buffer_size: u32,
    /// Sample rate
    pub sample_rate: u32,
    /// Input monitoring enabled
    pub input_monitoring: bool,
}

impl Default for AudioSettings {
    fn default() -> Self {
        Self {
            device_name: None,
            buffer_size: 512,
            sample_rate: 44100,
            input_monitoring: true,
        }
    }
}

/// Keyboard shortcuts configuration
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct KeyboardShortcuts {
    /// Play/Pause shortcut
    pub play_pause: String,
    /// Stop shortcut
    pub stop: String,
    /// Record shortcut
    pub record: String,
    /// Save project shortcut
    pub save: String,
    /// New project shortcut
    pub new_project: String,
    /// Open project shortcut
    pub open_project: String,
}

impl Default for KeyboardShortcuts {
    fn default() -> Self {
        Self {
            play_pause: "Space".to_string(),
            stop: "Escape".to_string(),
            record: "R".to_string(),
            save: "Cmd+S".to_string(),
            new_project: "Cmd+N".to_string(),
            open_project: "Cmd+O".to_string(),
        }
    }
}

/// UI state for the application
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct UIState {
    /// Whether the transport panel is visible
    pub transport_panel_visible: bool,
    /// Whether the project browser is visible
    pub project_browser_visible: bool,
    /// Whether the mixer is visible
    pub mixer_visible: bool,
    /// Current selected tool
    pub selected_tool: Tool,
    /// Zoom level for timeline
    pub timeline_zoom: f64,
    /// Timeline scroll position
    pub timeline_scroll_x: f64,
    /// Window dimensions
    pub window_size: WindowSize,
}

impl Default for UIState {
    fn default() -> Self {
        Self {
            transport_panel_visible: true,
            project_browser_visible: true,
            mixer_visible: false,
            selected_tool: Tool::Select,
            timeline_zoom: 1.0,
            timeline_scroll_x: 0.0,
            window_size: WindowSize::default(),
        }
    }
}

/// Available tools in the application
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub enum Tool {
    Select,
    Cut,
    Draw,
    Erase,
    Zoom,
}

/// Window size information
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct WindowSize {
    pub width: u32,
    pub height: u32,
}

impl Default for WindowSize {
    fn default() -> Self {
        Self {
            width: 1200,
            height: 800,
        }
    }
}

/// Recent projects list
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct RecentProjects {
    /// List of recent project paths with metadata
    pub projects: Vec<RecentProject>,
    /// Maximum number of recent projects to keep
    pub max_items: usize,
}

impl Default for RecentProjects {
    fn default() -> Self {
        Self {
            projects: Vec::new(),
            max_items: 10,
        }
    }
}

/// Information about a recent project
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct RecentProject {
    /// Project name
    pub name: String,
    /// Project file path
    pub path: String,
    /// Last opened timestamp
    pub last_opened: String, // ISO 8601 timestamp
}

/// Complete application state
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct AppState {
    /// All projects in the workspace
    pub projects: HashMap<String, ProjectState>,
    /// Currently active project name
    pub active_project: Option<String>,
    /// Application preferences
    pub preferences: AppPreferences,
    /// UI state
    pub ui: UIState,
    /// Recent projects
    pub recent_projects: RecentProjects,
    /// Application version
    pub app_version: String,
    /// Whether the application is in debug mode
    pub debug_mode: bool,
}

impl Default for AppState {
    fn default() -> Self {
        Self {
            projects: HashMap::new(),
            active_project: None,
            preferences: AppPreferences::default(),
            ui: UIState::default(),
            recent_projects: RecentProjects::default(),
            app_version: env!("CARGO_PKG_VERSION").to_string(),
            debug_mode: cfg!(debug_assertions),
        }
    }
}

impl AppState {
    /// Create a new application state with a default project
    pub fn new() -> Self {
        let mut state = Self::default();

        // Create a default project
        let default_project = ProjectState {
            name: "Default Project".to_string(),
            transport: Transport::new(),
            metadata: ProjectMetadata::default(),
            is_active: true,
        };

        state.projects.insert("Default Project".to_string(), default_project);
        state.active_project = Some("Default Project".to_string());

        state
    }

    /// Get the active project
    pub fn get_active_project(&self) -> Option<&ProjectState> {
        self.active_project.as_ref().and_then(|name| self.projects.get(name))
    }

    /// Get the active project mutably
    pub fn get_active_project_mut(&mut self) -> Option<&mut ProjectState> {
        let name = self.active_project.clone()?;
        self.projects.get_mut(&name)
    }

    /// Set the active project
    pub fn set_active_project(&mut self, name: &str) -> Result<(), ProjectError> {
        if self.projects.contains_key(name) {
            // Mark previous project as inactive
            if let Some(current_active) = &self.active_project {
                if let Some(project) = self.projects.get_mut(current_active) {
                    project.is_active = false;
                }
            }

            // Mark new project as active
            if let Some(project) = self.projects.get_mut(name) {
                project.is_active = true;
            }

            self.active_project = Some(name.to_string());
            Ok(())
        } else {
            Err(ProjectError::NotFound(name.to_string()))
        }
    }

    /// Create a new project
    pub fn create_project(&mut self, name: String) -> Result<&mut ProjectState, ProjectError> {
        if name.trim().is_empty() {
            return Err(ProjectError::InvalidName(name));
        }

        if self.projects.contains_key(&name) {
            return Err(ProjectError::AlreadyExists(name));
        }

        let project = ProjectState {
            name: name.clone(),
            transport: Transport::new(),
            metadata: ProjectMetadata::default(),
            is_active: false,
        };

        self.projects.insert(name.clone(), project);
        Ok(self.projects.get_mut(&name).unwrap())
    }

    /// Get a project by name
    pub fn get_project(&self, name: &str) -> Option<&ProjectState> {
        self.projects.get(name)
    }

    /// Get a project mutably by name
    pub fn get_project_mut(&mut self, name: &str) -> Option<&mut ProjectState> {
        self.projects.get_mut(name)
    }

    /// List all project names
    pub fn list_projects(&self) -> Vec<String> {
        self.projects.keys().cloned().collect()
    }

    /// Update recent projects when a project is opened
    pub fn add_recent_project(&mut self, name: String, path: String) {
        let recent = RecentProject {
            name,
            path,
            last_opened: chrono::Utc::now().to_rfc3339(),
        };

        // Remove if already exists
        self.recent_projects.projects.retain(|p| p.path != recent.path);

        // Add to front
        self.recent_projects.projects.insert(0, recent);

        // Trim to max items
        self.recent_projects.projects.truncate(self.recent_projects.max_items);
    }
}

/// Serializable version of the app state for sending to frontend
/// This excludes any non-serializable fields and provides a clean interface
#[derive(Debug, Clone, Serialize, Deserialize, TS)]
#[ts(export)]
pub struct AppStateSnapshot {
    /// All projects with their current state
    pub projects: HashMap<String, ProjectState>,
    /// Currently active project name
    pub active_project: Option<String>,
    /// Application preferences
    pub preferences: AppPreferences,
    /// UI state
    pub ui: UIState,
    /// Recent projects
    pub recent_projects: RecentProjects,
    /// Application version
    pub app_version: String,
    /// Whether the application is in debug mode
    pub debug_mode: bool,
    /// Timestamp when this snapshot was taken
    pub snapshot_timestamp: String,
}

impl From<&AppState> for AppStateSnapshot {
    fn from(state: &AppState) -> Self {
        Self {
            projects: state.projects.clone(),
            active_project: state.active_project.clone(),
            preferences: state.preferences.clone(),
            ui: state.ui.clone(),
            recent_projects: state.recent_projects.clone(),
            app_version: state.app_version.clone(),
            debug_mode: state.debug_mode,
            snapshot_timestamp: chrono::Utc::now().to_rfc3339(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_app_state_creation() {
        let state = AppState::new();
        assert!(state.active_project.is_some());
        assert_eq!(state.projects.len(), 1);
        assert!(state.projects.contains_key("Default Project"));
    }

    #[test]
    fn test_create_project() {
        let mut state = AppState::new();
        let result = state.create_project("Test Project".to_string());
        assert!(result.is_ok());
        assert_eq!(state.projects.len(), 2);
    }

    #[test]
    fn test_set_active_project() {
        let mut state = AppState::new();
        state.create_project("Test Project".to_string()).unwrap();

        let result = state.set_active_project("Test Project");
        assert!(result.is_ok());
        assert_eq!(state.active_project, Some("Test Project".to_string()));

        // Check that the project is marked as active
        let project = state.get_project("Test Project").unwrap();
        assert!(project.is_active);
    }

    #[test]
    fn test_recent_projects() {
        let mut state = AppState::new();
        state.add_recent_project("Test".to_string(), "/path/test".to_string());

        assert_eq!(state.recent_projects.projects.len(), 1);
        assert_eq!(state.recent_projects.projects[0].name, "Test");
    }
}
