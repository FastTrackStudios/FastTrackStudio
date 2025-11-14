use std::sync::Arc;
use tokio::sync::RwLock;
use tauri::Manager;

use primitives::TimeSignature;
use project::{ProjectActions, ProjectProvider};
use project::app::DesktopProjectManager;
use transport::{Tempo, Transport};

pub mod state;
use state::{AppState as AppStateStruct, AppStateSnapshot};

#[derive(Clone)]
pub struct AppState {
    pub project_manager: Arc<RwLock<DesktopProjectManager>>,
    pub app_state: Arc<RwLock<AppStateStruct>>,
}

impl AppState {
    pub fn new() -> Self {
        let manager = DesktopProjectManager::new();
        let app_state = AppStateStruct::new();

        Self {
            project_manager: Arc::new(RwLock::new(manager)),
            app_state: Arc::new(RwLock::new(app_state)),
        }
    }

    /// Initialize the app with a default project and start its transport
    pub async fn initialize(&self) -> Result<(), String> {
        println!("Initializing app state with default project...");

        let mut manager = self.project_manager.write().await;

        // Check if we already have projects
        let existing_projects = manager.list_projects();
        if !existing_projects.is_empty() {
            println!("Found existing projects: {:?}", existing_projects);

            // If no active project is set, set the first one as active
            if manager.get_active_project().is_none() {
                if let Some(first_project) = existing_projects.first() {
                    manager.set_active_project(first_project)
                        .map_err(|e| e.to_string())?;
                    println!("Set first existing project as active: {}", first_project);
                }
            }

            return Ok(());
        }

        // Create and initialize the default project
        match manager.create_default_project().await {
            Ok(()) => {
                println!("Default project created and initialized successfully");
                Ok(())
            }
            Err(e) => {
                eprintln!("Failed to create default project: {}", e);
                // Try to create a minimal fallback project
                match manager.create_project("Fallback Project".to_string()) {
                    Ok(_) => {
                        manager.set_active_project("Fallback Project")
                            .map_err(|e| e.to_string())?;
                        println!("Created fallback project as backup");
                        Ok(())
                    }
                    Err(fallback_err) => {
                        Err(format!("Failed to create both default and fallback projects: {} / {}", e, fallback_err))
                    }
                }
            }
        }
    }
}

// Tauri command handlers
#[tauri::command]
async fn list_projects(app_state: tauri::State<'_, AppState>) -> Result<Vec<String>, String> {
    let manager = app_state.project_manager.read().await;
    Ok(manager.list_projects())
}

#[tauri::command]
async fn get_active_project(app_state: tauri::State<'_, AppState>) -> Result<Option<String>, String> {
    let manager = app_state.project_manager.read().await;
    let active = manager.get_active_project();

    println!("get_active_project called: {}", manager.get_active_project_status());

    if let Some(name) = active {
        Ok(Some(name.to_string()))
    } else {
        println!("No active project found");
        Ok(None)
    }
}

#[tauri::command]
async fn create_project(
    app_state: tauri::State<'_, AppState>,
    name: String,
) -> Result<String, String> {
    let mut manager = app_state.project_manager.write().await;
    manager.create_project(name.clone()).map_err(|e| e.to_string())?;
    manager.initialize_project(&name).await.map_err(|e| e.to_string())?;
    Ok(format!("Project '{}' created successfully", name))
}

#[tauri::command]
async fn set_active_project(
    app_state: tauri::State<'_, AppState>,
    name: String,
) -> Result<String, String> {
    let mut manager = app_state.project_manager.write().await;
    manager.set_active_project(&name).map_err(|e| e.to_string())?;
    Ok(format!("Active project set to '{}'", name))
}

#[tauri::command]
async fn get_project_info(
    app_state: tauri::State<'_, AppState>,
    name: String,
) -> Result<serde_json::Value, String> {
    let manager = app_state.project_manager.read().await;
    let project = manager.get_project(&name).map_err(|e| e.to_string())?;

    Ok(serde_json::json!({
        "name": project.get_name(),
        "transport_ready": project.transport_app().is_some(),
    }))
}

#[tauri::command]
async fn get_transport_stats(
    app_state: tauri::State<'_, AppState>,
    name: String,
) -> Result<serde_json::Value, String> {
    let manager = app_state.project_manager.read().await;
    manager.get_transport_stats(&name).await.map_err(|e| e.to_string())
}

// Transport command handlers
#[tauri::command]
async fn transport_play(app_state: tauri::State<'_, AppState>) -> Result<String, String> {
    println!("🎵 [TAURI] transport_play command called from frontend");
    let mut manager = app_state.project_manager.write().await;
    let active_name = manager.get_active_project().ok_or("No active project")?.to_string();
    println!("🎵 [TAURI] Transport PLAY called on active project: {}", active_name);
    let project = manager.get_active_project_mut().ok_or("No active project")?;
    let result = project.async_play().await.map_err(|e| e.to_string());
    println!("🎵 [TAURI] transport_play result: {:?}", result);
    result
}

#[tauri::command]
async fn transport_pause(app_state: tauri::State<'_, AppState>) -> Result<String, String> {
    println!("⏸️ [TAURI] transport_pause command called from frontend");
    let mut manager = app_state.project_manager.write().await;
    let active_name = manager.get_active_project().ok_or("No active project")?.to_string();
    println!("⏸️ [TAURI] Transport PAUSE called on active project: {}", active_name);
    let project = manager.get_active_project_mut().ok_or("No active project")?;
    let result = project.async_pause().await.map_err(|e| e.to_string());
    println!("⏸️ [TAURI] transport_pause result: {:?}", result);
    result
}

#[tauri::command]
async fn transport_stop(app_state: tauri::State<'_, AppState>) -> Result<String, String> {
    println!("⏹️ [TAURI] transport_stop command called from frontend");
    let mut manager = app_state.project_manager.write().await;
    let active_name = manager.get_active_project().ok_or("No active project")?.to_string();
    println!("⏹️ [TAURI] Transport STOP called on active project: {}", active_name);
    let project = manager.get_active_project_mut().ok_or("No active project")?;
    let result = project.async_stop().await.map_err(|e| e.to_string());
    println!("⏹️ [TAURI] transport_stop result: {:?}", result);
    result
}

#[tauri::command]
async fn transport_play_pause(app_state: tauri::State<'_, AppState>) -> Result<String, String> {
    let mut manager = app_state.project_manager.write().await;
    let active_name = manager.get_active_project().ok_or("No active project")?.to_string();
    println!("Transport PLAY_PAUSE called on active project: {}", active_name);
    let project = manager.get_active_project_mut().ok_or("No active project")?;
    project.async_play_pause().await.map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_start_recording(app_state: tauri::State<'_, AppState>) -> Result<String, String> {
    let mut manager = app_state.project_manager.write().await;
    let active_name = manager.get_active_project().ok_or("No active project")?.to_string();
    println!("Transport START_RECORDING called on active project: {}", active_name);
    let project = manager.get_active_project_mut().ok_or("No active project")?;
    project.async_start_recording().await.map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_stop_recording(app_state: tauri::State<'_, AppState>) -> Result<String, String> {
    let mut manager = app_state.project_manager.write().await;
    let active_name = manager.get_active_project().ok_or("No active project")?.to_string();
    println!("Transport STOP_RECORDING called on active project: {}", active_name);
    let project = manager.get_active_project_mut().ok_or("No active project")?;
    project.async_stop_recording().await.map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_set_tempo(
    app_state: tauri::State<'_, AppState>,
    bpm: f64,
) -> Result<String, String> {
    let tempo = Tempo::new(bpm);
    let mut manager = app_state.project_manager.write().await;
    let active_name = manager.get_active_project().ok_or("No active project")?.to_string();
    println!("Transport SET_TEMPO({}) called on active project: {}", bpm, active_name);
    let project = manager.get_active_project_mut().ok_or("No active project")?;
    project.async_set_tempo(tempo).await.map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_set_position(
    app_state: tauri::State<'_, AppState>,
    seconds: f64,
) -> Result<String, String> {
    let mut manager = app_state.project_manager.write().await;
    let active_name = manager.get_active_project().ok_or("No active project")?.to_string();
    println!("Transport SET_POSITION({}) called on active project: {}", seconds, active_name);
    let project = manager.get_active_project_mut().ok_or("No active project")?;
    project.async_set_position(seconds).await.map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_set_time_signature(
    app_state: tauri::State<'_, AppState>,
    numerator: u8,
    denominator: u8,
) -> Result<String, String> {
    let time_signature = TimeSignature { numerator: numerator.into(), denominator: denominator.into() };
    let mut manager = app_state.project_manager.write().await;
    let active_name = manager.get_active_project().ok_or("No active project")?.to_string();
    println!("Transport SET_TIME_SIGNATURE({}/{}) called on active project: {}", numerator, denominator, active_name);
    let project = manager.get_active_project_mut().ok_or("No active project")?;
    project.async_set_time_signature(time_signature).await.map_err(|e| e.to_string())
}

#[tauri::command]
async fn transport_get_state(app_state: tauri::State<'_, AppState>) -> Result<Transport, String> {
    println!("📊 [TAURI] transport_get_state command called from frontend");
    let manager = app_state.project_manager.read().await;
    let active_name = manager.get_active_project().ok_or("No active project")?;
    println!("📊 [TAURI] Transport GET_STATE called on active project: {}", active_name);
    let project = manager.get_project(active_name).map_err(|e| e.to_string())?;
    let result = project.async_get_transport().await.map_err(|e| e.to_string());
    println!("📊 [TAURI] transport_get_state result: {:?}", result);
    result
}

// Project-specific transport commands
#[tauri::command]
async fn project_transport_play(
    app_state: tauri::State<'_, AppState>,
    project_name: String,
) -> Result<String, String> {
    let mut manager = app_state.project_manager.write().await;
    let project = manager.get_project_mut(&project_name).map_err(|e| e.to_string())?;
    project.async_play().await.map_err(|e| e.to_string())
}

#[tauri::command]
async fn project_transport_pause(
    app_state: tauri::State<'_, AppState>,
    project_name: String,
) -> Result<String, String> {
    let mut manager = app_state.project_manager.write().await;
    let project = manager.get_project_mut(&project_name).map_err(|e| e.to_string())?;
    project.async_pause().await.map_err(|e| e.to_string())
}

#[tauri::command]
async fn project_transport_stop(
    app_state: tauri::State<'_, AppState>,
    project_name: String,
) -> Result<String, String> {
    let mut manager = app_state.project_manager.write().await;
    let project = manager.get_project_mut(&project_name).map_err(|e| e.to_string())?;
    project.async_stop().await.map_err(|e| e.to_string())
}

// App state commands
#[tauri::command]
async fn get_app_state(app_state: tauri::State<'_, AppState>) -> Result<AppStateSnapshot, String> {
    let state = app_state.app_state.read().await;
    Ok(state.snapshot())
}

#[tauri::command]
async fn update_app_preferences(
    app_state: tauri::State<'_, AppState>,
    preferences: serde_json::Value,
) -> Result<String, String> {
    let mut state = app_state.app_state.write().await;
    state.update_preferences(preferences);
    Ok("Preferences updated".to_string())
}

#[tauri::command]
async fn update_ui_state(
    app_state: tauri::State<'_, AppState>,
    ui_state: serde_json::Value,
) -> Result<String, String> {
    let mut state = app_state.app_state.write().await;
    state.update_ui_state(ui_state);
    Ok("UI state updated".to_string())
}

#[tauri::command]
fn greet(name: &str) -> String {
    format!("Hello, {}! You've been greeted from Rust!", name)
}

#[tauri::command]
async fn test_transport_object() -> Result<Transport, String> {
    println!("🧪 test_transport_object called - returning hardcoded Transport");

    // Create a hardcoded transport with all default values to test serialization
    let transport = Transport::default();

    println!("🧪 Hardcoded transport created: {:?}", transport);
    Ok(transport)
}

pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .setup(|app| {
            let app_state = AppState::new();
            println!("🚀 Starting FastTrack Studio Desktop...");

            // Initialize the app state in a separate task with better error handling
            let app_state_clone = app_state.clone();
            tauri::async_runtime::spawn(async move {
                println!("📦 Initializing default project and transport engine...");
                match app_state_clone.initialize().await {
                    Ok(()) => {
                        println!("✅ App state initialized successfully");
                        println!("🎵 Transport engine ready for use");
                    }
                    Err(e) => {
                        eprintln!("❌ Failed to initialize app state: {}", e);
                        eprintln!("⚠️  Application may not function correctly");
                    }
                }
            });

            app.manage(app_state);
            println!("📋 App state managed, setup complete");
            Ok(())
        })
        .invoke_handler(tauri::generate_handler![
            list_projects,
            get_active_project,
            create_project,
            set_active_project,
            get_project_info,
            get_transport_stats,
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
            project_transport_play,
            project_transport_pause,
            project_transport_stop,
            get_app_state,
            update_app_preferences,
            update_ui_state,
            greet,
            test_transport_object
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
