use std::sync::Arc;
use std::time::Duration;
use tokio::sync::{Mutex, RwLock};
use tauri::AppHandle;
use tauri::ipc::Channel;
use taurpc::Router;

// Import the separate API crates
use user_api::{UserApiState, UserData, create_user_api_handler, UserApi};
use counter_api::{CounterState, CounterData, create_counter_api_handler, CounterApi};
use world_api::{WorldApiState, WorldData, create_world_api_handler, WorldApi, WorldApiIntegration, world_connection_monitor};

// Main app state that coordinates between different APIs
type AppState = Arc<RwLock<AppData>>;

#[derive(Debug, Clone)]
struct AppData {
    is_processing: bool,
    last_global_message: String,
    world_integration_active: bool,
}

impl Default for AppData {
    fn default() -> Self {
        Self {
            is_processing: false,
            last_global_message: "Application started".to_string(),
            world_integration_active: false,
        }
    }
}

// Global app status that combines data from different APIs
#[taurpc::ipc_type]
struct AppStatus {
    counter_value: i32,
    user_count: u32,
    is_processing: bool,
    last_message: String,
    world_greeting_count: u32,
    world_connected: bool,
}

#[taurpc::ipc_type]
struct TaskProgress {
    task_id: String,
    progress: u8,
    message: String,
}

#[taurpc::ipc_type]
struct ProcessingResult {
    success: bool,
    message: String,
    duration_ms: u64,
}

// Custom error type for main app
#[derive(Debug)]
#[taurpc::ipc_type]
struct ApiError {
    code: u16,
    message: String,
}

impl std::fmt::Display for ApiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "API Error {}: {}", self.code, self.message)
    }
}

impl std::error::Error for ApiError {}

// Main API - handles core functionality and coordination
#[taurpc::procedures(event_trigger = ApiEventTrigger)]
trait Api {
    async fn get_status() -> Result<AppStatus, ApiError>;
    async fn reset_app() -> Result<String, ApiError>;

    // Events that can be triggered from backend
    #[taurpc(event)]
    async fn status_changed(status: AppStatus);

    #[taurpc(event)]
    async fn error_occurred(error: ApiError);
}

#[derive(Clone)]
struct ApiImpl {
    app_state: AppState,
    counter_state: CounterState,
    user_state: UserApiState,
    world_state: WorldApiState,
}

#[taurpc::resolvers]
impl Api for ApiImpl {
    async fn get_status(self) -> Result<AppStatus, ApiError> {
        let app_data = self.app_state.read().await;
        let counter_data = self.counter_state.lock().await;
        let user_data = self.user_state.read().await;
        let world_data = self.world_state.read().await;

        Ok(AppStatus {
            counter_value: counter_data.value,
            user_count: user_data.user_count,
            is_processing: app_data.is_processing,
            last_message: app_data.last_global_message.clone(),
            world_greeting_count: world_data.greeting_count,
            world_connected: world_data.connected_to_server,
        })
    }

    async fn reset_app(self) -> Result<String, ApiError> {
        let mut app_data = self.app_state.write().await;
        let mut counter_data = self.counter_state.lock().await;
        let mut user_data = self.user_state.write().await;
        let mut world_data = self.world_state.write().await;

        // Reset all states
        counter_data.value = 0;
        counter_data.operations_count = 0;
        counter_data.last_operation = "Reset".to_string();

        user_data.user_count = 0;
        user_data.last_message = "Reset".to_string();

        *world_data = WorldData::default();

        app_data.is_processing = false;
        app_data.last_global_message = "Application reset successfully".to_string();

        Ok("All application state has been reset successfully".to_string())
    }
}

// Tasks API - demonstrates channels for progress updates
#[taurpc::procedures(path = "tasks")]
trait TaskApi {
    async fn run_long_task(
        task_name: String,
        duration_seconds: u64,
        on_progress: Channel<TaskProgress>,
    ) -> Result<ProcessingResult, ApiError>;

    async fn simulate_batch_processing(
        batch_size: u32,
        on_progress: Channel<TaskProgress>,
    ) -> Result<ProcessingResult, ApiError>;
}

#[derive(Clone)]
struct TaskApiImpl {
    app_state: AppState,
}

#[taurpc::resolvers]
impl TaskApi for TaskApiImpl {
    async fn run_long_task(
        self,
        task_name: String,
        duration_seconds: u64,
        on_progress: Channel<TaskProgress>,
    ) -> Result<ProcessingResult, ApiError> {
        if duration_seconds > 60 {
            return Err(ApiError {
                code: 400,
                message: "Task duration cannot exceed 60 seconds".to_string(),
            });
        }

        // Mark as processing
        {
            let mut app_data = self.app_state.write().await;
            app_data.is_processing = true;
            app_data.last_global_message = format!("Started task: {}", task_name);
        }

        let start_time = std::time::Instant::now();
        let steps = 10;
        let step_duration = Duration::from_millis((duration_seconds * 1000) / steps);

        for i in 0..=steps {
            let progress = ((i as f64 / steps as f64) * 100.0) as u8;

            let _ = on_progress.send(TaskProgress {
                task_id: task_name.clone(),
                progress,
                message: format!("Processing step {} of {}", i, steps),
            });

            if i < steps {
                tokio::time::sleep(step_duration).await;
            }
        }

        // Mark as not processing
        {
            let mut app_data = self.app_state.write().await;
            app_data.is_processing = false;
            app_data.last_global_message = format!("Completed task: {}", task_name);
        }

        Ok(ProcessingResult {
            success: true,
            message: format!("Task '{}' completed successfully", task_name),
            duration_ms: start_time.elapsed().as_millis() as u64,
        })
    }

    async fn simulate_batch_processing(
        self,
        batch_size: u32,
        on_progress: Channel<TaskProgress>,
    ) -> Result<ProcessingResult, ApiError> {
        if batch_size > 1000 {
            return Err(ApiError {
                code: 400,
                message: "Batch size cannot exceed 1000 items".to_string(),
            });
        }

        let start_time = std::time::Instant::now();

        for i in 0..batch_size {
            let progress = ((i as f64 / batch_size as f64) * 100.0) as u8;

            let _ = on_progress.send(TaskProgress {
                task_id: "batch_processing".to_string(),
                progress,
                message: format!("Processing item {} of {}", i + 1, batch_size),
            });

            // Simulate processing time
            tokio::time::sleep(Duration::from_millis(50)).await;
        }

        // Send completion
        let _ = on_progress.send(TaskProgress {
            task_id: "batch_processing".to_string(),
            progress: 100,
            message: "Batch processing completed".to_string(),
        });

        Ok(ProcessingResult {
            success: true,
            message: format!("Successfully processed {} items", batch_size),
            duration_ms: start_time.elapsed().as_millis() as u64,
        })
    }
}

// Background task to periodically trigger events
async fn background_event_trigger(
    app_handle: AppHandle,
    app_state: AppState,
    counter_state: CounterState,
    user_state: UserApiState,
    world_state: WorldApiState,
) {
    let trigger = ApiEventTrigger::new(app_handle);
    let mut interval = tokio::time::interval(Duration::from_secs(30));

    loop {
        interval.tick().await;

        // Send periodic status updates by combining data from different APIs
        let app_data = app_state.read().await;
        let counter_data = counter_state.lock().await;
        let user_data = user_state.read().await;
        let world_data = world_state.read().await;

        let status = AppStatus {
            counter_value: counter_data.value,
            user_count: user_data.user_count,
            is_processing: app_data.is_processing,
            last_message: format!("Periodic update at {}", chrono::Utc::now().format("%H:%M:%S")),
            world_greeting_count: world_data.greeting_count,
            world_connected: world_data.connected_to_server,
        };

        // Send to all windows
        if let Err(e) = trigger.status_changed(status) {
            eprintln!("Failed to send status update: {}", e);
        }
    }
}

#[tokio::main]
async fn main() {
    // Initialize shared state for different APIs
    let app_state: AppState = Arc::new(RwLock::new(AppData::default()));
    let counter_state: CounterState = Arc::new(Mutex::new(CounterData::default()));
    let user_state: UserApiState = Arc::new(RwLock::new(UserData::default()));

    // Initialize World API integration
    let world_integration = WorldApiIntegration::new();
    let world_state = world_integration.state.clone();

    // Create main API implementation
    let api_impl = ApiImpl {
        app_state: app_state.clone(),
        counter_state: counter_state.clone(),
        user_state: user_state.clone(),
        world_state: world_state.clone(),
    };

    // Create task API implementation
    let task_impl = TaskApiImpl {
        app_state: app_state.clone(),
    };

    // Create router with all API handlers from different crates
    let router = Router::new()
        .export_config(
            specta_typescript::Typescript::default()
                .bigint(specta_typescript::BigIntExportBehavior::Number)
        )
        .merge(api_impl.into_handler())
        .merge(task_impl.into_handler())
        .merge(create_counter_api_handler(counter_state.clone()).into_handler())
        .merge(create_user_api_handler(user_state.clone()).into_handler())
        .merge(world_integration.into_router_handler());

    tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .setup(move |app| {
            println!("🚀 Starting FastTrack Studio Desktop with Modular TauRPC APIs...");
            println!("📊 Features enabled:");
            println!("  - Modular API architecture with separate crates");
            println!("  - Cross-crate shared state management");
            println!("  - Multiple API sections with routing");
            println!("  - Real-time events coordination");
            println!("  - Progress channels");
            println!("  - Custom error handling per domain");
            println!("  - World API integration (tarpc + taurpc bridge)");

            // Start background event trigger
            let app_handle = app.handle().clone();
            let app_state_bg = app_state.clone();
            let counter_state_bg = counter_state.clone();
            let user_state_bg = user_state.clone();
            let world_state_bg = world_state.clone();

            tokio::spawn(background_event_trigger(
                app_handle.clone(),
                app_state_bg,
                counter_state_bg,
                user_state_bg,
                world_state_bg.clone(),
            ));

            // Start World API connection monitor
            let world_event_trigger = world_api::WorldEventTrigger::new(app_handle.clone());
            tokio::spawn(world_connection_monitor(
                world_state_bg,
                world_event_trigger,
                30, // Check every 30 seconds
            ));

            Ok(())
        })
        .invoke_handler(router.into_handler())
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
