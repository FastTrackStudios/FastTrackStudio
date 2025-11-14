use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

use primitives::TimeSignature;
use project::{ProjectActions, ProjectError, ProjectProvider};
use transport::{RecordMode, Tempo, Transport, TransportActions, TransportError};

pub mod state;
use state::{AppState as AppStateStruct, AppStateSnapshot};

/// A concrete DAW project that implements both project and transport actions
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DawProject {
    name: String,
    #[serde(skip)]
    transport: Transport,
}

impl DawProject {
    fn new(name: String) -> Self {
        Self {
            name,
            transport: Transport::new(),
        }
    }
}

// Implement project actions
impl ProjectActions for DawProject {
    fn get_name(&self) -> &str {
        &self.name
    }

    fn set_name(&mut self, name: String) {
        self.name = name;
    }
}

// Implement transport actions by delegating to the transport component
impl TransportActions for DawProject {
    fn play(&mut self) -> Result<String, TransportError> {
        self.transport.play()
    }

    fn pause(&mut self) -> Result<String, TransportError> {
        self.transport.pause()
    }

    fn stop(&mut self) -> Result<String, TransportError> {
        self.transport.stop()
    }

    fn play_pause(&mut self) -> Result<String, TransportError> {
        self.transport.play_pause()
    }

    fn play_stop(&mut self) -> Result<String, TransportError> {
        self.transport.play_stop()
    }

    fn start_recording(&mut self) -> Result<String, TransportError> {
        self.transport.start_recording()
    }

    fn stop_recording(&mut self) -> Result<String, TransportError> {
        self.transport.stop_recording()
    }

    fn toggle_recording(&mut self) -> Result<String, TransportError> {
        self.transport.toggle_recording()
    }

    fn set_tempo(&mut self, tempo: Tempo) -> Result<String, TransportError> {
        match self.transport.set_tempo(tempo) {
            Ok(()) => Ok(format!("Tempo set to {} BPM", tempo.bpm)),
            Err(e) => Err(TransportError::InvalidTempo(e)),
        }
    }

    fn set_time_signature(
        &mut self,
        time_signature: TimeSignature,
    ) -> Result<String, TransportError> {
        self.transport.time_signature = time_signature;
        Ok(format!(
            "Time signature set to {}/{}",
            time_signature.numerator, time_signature.denominator
        ))
    }

    fn set_record_mode(&mut self, record_mode: RecordMode) -> Result<String, TransportError> {
        self.transport.record_mode = record_mode;
        Ok(format!("Record mode set to {:?}", record_mode))
    }

    fn set_position(&mut self, seconds: f64) -> Result<String, TransportError> {
        use primitives::Position;
        self.transport.playhead_position = Position::from_seconds(seconds);
        Ok(format!("Position set to {} seconds", seconds))
    }

    fn get_tempo(&self) -> Result<Tempo, TransportError> {
        Ok(self.transport.tempo)
    }

    fn get_time_signature(&self) -> Result<TimeSignature, TransportError> {
        Ok(self.transport.time_signature)
    }

    fn get_record_mode(&self) -> Result<RecordMode, TransportError> {
        Ok(self.transport.record_mode)
    }

    fn get_position(&self) -> Result<f64, TransportError> {
        Ok(self.transport.playhead_position.time.to_seconds())
    }

    fn is_playing(&self) -> Result<bool, TransportError> {
        Ok(self.transport.is_playing())
    }

    fn is_recording(&self) -> Result<bool, TransportError> {
        Ok(self.transport.is_recording())
    }

    fn get_transport(&self) -> Result<Transport, TransportError> {
        Ok(self.transport.clone())
    }

    fn is_ready(&self) -> Result<bool, TransportError> {
        Ok(true)
    }
}

/// Desktop app's project manager that holds multiple DAW projects
#[derive(Default)]
pub struct DesktopProjectManager {
    projects: HashMap<String, DawProject>,
    active_project: Option<String>,
}

impl DesktopProjectManager {
    pub fn new() -> Self {
        Self {
            projects: HashMap::new(),
            active_project: None,
        }
    }

    pub fn set_active_project(&mut self, name: &str) -> Result<(), ProjectError> {
        if self.projects.contains_key(name) {
            self.active_project = Some(name.to_string());
            Ok(())
        } else {
            Err(ProjectError::NotFound(name.to_string()))
        }
    }

    pub fn get_active_project(&self) -> Option<&str> {
        self.active_project.as_deref()
    }

    pub fn get_active_project_mut(&mut self) -> Option<&mut DawProject> {
        let active_name = self.active_project.clone()?;
        self.projects.get_mut(&active_name)
    }
}

impl ProjectProvider for DesktopProjectManager {
    type Project = DawProject;
    type Error = ProjectError;

    fn get_project(&self, name: &str) -> Result<&Self::Project, Self::Error> {
        self.projects
            .get(name)
            .ok_or_else(|| ProjectError::NotFound(name.to_string()))
    }

    fn get_project_mut(&mut self, name: &str) -> Result<&mut Self::Project, Self::Error> {
        self.projects
            .get_mut(name)
            .ok_or_else(|| ProjectError::NotFound(name.to_string()))
    }

    fn list_projects(&self) -> Vec<String> {
        self.projects.keys().cloned().collect()
    }

    fn create_project(&mut self, name: String) -> Result<&mut Self::Project, Self::Error> {
        if name.trim().is_empty() {
            return Err(ProjectError::InvalidName(name));
        }

        if self.projects.contains_key(&name) {
            return Err(ProjectError::AlreadyExists(name));
        }

        let project = DawProject::new(name.clone());
        self.projects.insert(name.clone(), project);

        // Set as active if it's the first project
        if self.active_project.is_none() {
            self.active_project = Some(name.clone());
        }

        Ok(self.projects.get_mut(&name).unwrap())
    }
}

/// Global state for the desktop application
pub struct AppState {
    pub project_manager: Arc<RwLock<DesktopProjectManager>>,
    pub app_state: Arc<RwLock<AppStateStruct>>,
}

impl AppState {
    pub fn new() -> Self {
        let mut manager = DesktopProjectManager::new();

        // Create a default project and set it as active
        let _ = manager.create_project("Default Project".to_string());
        let _ = manager.set_active_project("Default Project");

        // Create comprehensive app state
        let app_state = AppStateStruct::new();

        Self {
            project_manager: Arc::new(RwLock::new(manager)),
            app_state: Arc::new(RwLock::new(app_state)),
        }
    }
}

// Tauri command implementations
#[tauri::command]
async fn list_projects(state: tauri::State<'_, AppState>) -> Result<Vec<String>, String> {
    let manager = state.project_manager.read().await;
    Ok(manager.list_projects())
}

#[tauri::command]
async fn get_active_project(state: tauri::State<'_, AppState>) -> Result<Option<String>, String> {
    let manager = state.project_manager.read().await;
    Ok(manager.get_active_project().map(|s| s.to_string()))
}

#[tauri::command]
async fn create_project(state: tauri::State<'_, AppState>, name: String) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    match manager.create_project(name.clone()) {
        Ok(_) => Ok(format!("Created project: {}", name)),
        Err(e) => Err(e.to_string()),
    }
}

#[tauri::command]
async fn set_active_project(
    state: tauri::State<'_, AppState>,
    name: String,
) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    match manager.set_active_project(&name) {
        Ok(()) => Ok(format!("Set active project: {}", name)),
        Err(e) => Err(e.to_string()),
    }
}

#[tauri::command]
async fn get_project_info(
    state: tauri::State<'_, AppState>,
    name: String,
) -> Result<serde_json::Value, String> {
    let manager = state.project_manager.read().await;
    let project = manager.get_project(&name).map_err(|e| e.to_string())?;

    Ok(serde_json::json!({
        "name": project.get_name(),
        "is_playing": project.is_playing().unwrap_or(false),
        "is_recording": project.is_recording().unwrap_or(false),
        "tempo": project.get_tempo().map(|t| t.bpm).unwrap_or(120.0),
        "position": project.get_position().unwrap_or(0.0),
    }))
}

// Transport commands for active project
#[tauri::command]
async fn transport_play(state: tauri::State<'_, AppState>) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_active_project_mut()
        .ok_or_else(|| "No active project".to_string())?;
    project.play().map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_pause(state: tauri::State<'_, AppState>) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_active_project_mut()
        .ok_or_else(|| "No active project".to_string())?;
    project.pause().map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_stop(state: tauri::State<'_, AppState>) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_active_project_mut()
        .ok_or_else(|| "No active project".to_string())?;
    project.stop().map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_play_pause(state: tauri::State<'_, AppState>) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_active_project_mut()
        .ok_or_else(|| "No active project".to_string())?;
    project.play_pause().map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_start_recording(state: tauri::State<'_, AppState>) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_active_project_mut()
        .ok_or_else(|| "No active project".to_string())?;
    project.start_recording().map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_stop_recording(state: tauri::State<'_, AppState>) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_active_project_mut()
        .ok_or_else(|| "No active project".to_string())?;
    project.stop_recording().map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_set_tempo(
    state: tauri::State<'_, AppState>,
    bpm: f64,
) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_active_project_mut()
        .ok_or_else(|| "No active project".to_string())?;
    let tempo = Tempo::new(bpm);
    project.set_tempo(tempo).map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_set_position(
    state: tauri::State<'_, AppState>,
    seconds: f64,
) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_active_project_mut()
        .ok_or_else(|| "No active project".to_string())?;
    project.set_position(seconds).map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_set_time_signature(
    state: tauri::State<'_, AppState>,
    numerator: u32,
    denominator: u32,
) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_active_project_mut()
        .ok_or_else(|| "No active project".to_string())?;
    let time_sig = TimeSignature::new(numerator as i32, denominator as i32);
    project
        .set_time_signature(time_sig)
        .map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_get_state(
    state: tauri::State<'_, AppState>,
) -> Result<serde_json::Value, String> {
    let manager = state.project_manager.read().await;
    let active_name = manager
        .get_active_project()
        .ok_or_else(|| "No active project".to_string())?;
    let project = manager
        .get_project(active_name)
        .map_err(|e| e.to_string())?;

    Ok(serde_json::json!({
        "project_name": project.get_name(),
        "is_playing": project.is_playing().unwrap_or(false),
        "is_recording": project.is_recording().unwrap_or(false),
        "tempo": project.get_tempo().map(|t| t.bpm).unwrap_or(120.0),
        "position": project.get_position().unwrap_or(0.0),
        "time_signature": {
            "numerator": project.get_time_signature().map(|ts| ts.numerator).unwrap_or(4),
            "denominator": project.get_time_signature().map(|ts| ts.denominator).unwrap_or(4),
        }
    }))
}

// Transport commands for specific project
#[tauri::command]
async fn project_transport_play(
    state: tauri::State<'_, AppState>,
    project_name: String,
) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_project_mut(&project_name)
        .map_err(|e| e.to_string())?;
    project.play().map_err(|e| e.to_string())
}

#[tauri::command]
async fn project_transport_pause(
    state: tauri::State<'_, AppState>,
    project_name: String,
) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_project_mut(&project_name)
        .map_err(|e| e.to_string())?;
    project.pause().map_err(|e| e.to_string())
}

#[tauri::command]
async fn project_transport_stop(
    state: tauri::State<'_, AppState>,
    project_name: String,
) -> Result<String, String> {
    let mut manager = state.project_manager.write().await;
    let project = manager
        .get_project_mut(&project_name)
        .map_err(|e| e.to_string())?;
    project.stop().map_err(|e| e.to_string())
}

// Comprehensive app state commands
#[tauri::command]
async fn get_app_state(state: tauri::State<'_, AppState>) -> Result<AppStateSnapshot, String> {
    let app_state = state.app_state.read().await;
    Ok(AppStateSnapshot::from(&*app_state))
}

#[tauri::command]
async fn update_app_preferences(
    state: tauri::State<'_, AppState>,
    preferences: state::AppPreferences,
) -> Result<String, String> {
    let mut app_state = state.app_state.write().await;
    app_state.preferences = preferences;
    Ok("Preferences updated".to_string())
}

#[tauri::command]
async fn update_ui_state(
    state: tauri::State<'_, AppState>,
    ui_state: state::UIState,
) -> Result<String, String> {
    let mut app_state = state.app_state.write().await;
    app_state.ui = ui_state;
    Ok("UI state updated".to_string())
}

#[tauri::command]
fn greet(name: &str) -> String {
    format!("Hello, {}! You've been greeted from Rust!", name)
}

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    let app_state = AppState::new();

    tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .manage(app_state)
        .invoke_handler(tauri::generate_handler![
            greet,
            // Project management
            list_projects,
            get_active_project,
            create_project,
            set_active_project,
            get_project_info,
            // Transport for active project
            transport_play,
            transport_pause,
            transport_stop,
            transport_play_pause,
            transport_start_recording,
            transport_stop_recording,
            transport_set_tempo,
            transport_set_position,
            transport_set_time_signature,
            transport_get_state,
            // Transport for specific project
            project_transport_play,
            project_transport_pause,
            project_transport_stop,
            // Comprehensive app state
            get_app_state,
            update_app_preferences,
            update_ui_state,
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
