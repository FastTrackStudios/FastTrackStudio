//! Session Cell - Control Surface for DAW
//!
//! This cell provides a control surface that connects to a DAW implementation
//! through daw-control and presents transport controls.
//!
//! This is the cell binary entry point. For library usage, import `session` crate directly.

use actions_proto::{
    ActionDefinition, ActionId, ActionResult, DefinesActions, DefinesActionsDispatcher,
};
use cell_runtime::{HostServiceClient, WaitPolicy, run_cell};
use daw_control::Daw;
use roam::session::{ConnectionHandle, Context};
use roam_telemetry::{ExporterConfig, OtlpExporter, TelemetryMiddleware};
use session::{SetlistServiceImpl, SongServiceImpl};
use session_proto::{
    SessionService, SessionServiceDispatcher, SetlistService, SetlistServiceDispatcher,
    SongServiceDispatcher,
};
use std::sync::{Arc, OnceLock};
use std::time::Duration;
use tracing::{info, warn};

/// Create telemetry middleware (OTLP only, no console logging)
fn create_telemetry() -> TelemetryMiddleware<OtlpExporter> {
    let otlp_endpoint = std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT")
        .unwrap_or_else(|_| "http://localhost:4318/v1/traces".to_string());

    let otlp_exporter = OtlpExporter::with_config(ExporterConfig {
        endpoint: otlp_endpoint,
        service_name: "session".to_string(),
        resource_attributes: vec![],
        max_batch_size: 10,
        max_batch_delay: Duration::from_secs(2),
        timeout: Duration::from_secs(10),
    });

    TelemetryMiddleware::new(otlp_exporter)
}

// Action handlers — definitions come from session::session_actions (lib.rs).
// This macro generates only `SessionServiceImpl::dispatch_action()`.
actions_proto::impl_action_dispatch! {
    for SessionServiceImpl, use session::session_actions {
        TOGGLE_PLAYBACK => handler(self_, cx) {
            self_.setlist_service.toggle_playback(cx).await;
            ActionResult::success()
        }
        TOGGLE_SONG_LOOP => handler(self_, cx) {
            self_.setlist_service.toggle_song_loop(cx).await;
            ActionResult::success()
        }
        SMART_NEXT => handler(self_, cx) {
            self_.setlist_service.next_section(cx).await;
            ActionResult::success()
        }
        SMART_PREVIOUS => handler(self_, cx) {
            self_.setlist_service.previous_section(cx).await;
            ActionResult::success()
        }
        NEXT_SONG => handler(self_, cx) {
            self_.setlist_service.next_song(cx).await;
            ActionResult::success()
        }
        PREVIOUS_SONG => handler(self_, cx) {
            self_.setlist_service.previous_song(cx).await;
            ActionResult::success()
        }
        NEXT_SECTION => handler(self_, cx) {
            self_.setlist_service.next_section(cx).await;
            ActionResult::success()
        }
        PREVIOUS_SECTION => handler(self_, cx) {
            self_.setlist_service.previous_section(cx).await;
            ActionResult::success()
        }
        LOG_HELLO => handler(self_, _cx) {
            info!("Hello from the session cell!");
            ActionResult::success_with_message("Logged hello from session")
        }
        LOG_STATUS => handler(self_, _cx) {
            info!("Session status: healthy");
            ActionResult::success_with_message("Logged session status")
        }
    }
}

/// Implementation of SessionService and DefinesActions
#[derive(Clone)]
pub struct SessionServiceImpl {
    handle_cell: Arc<OnceLock<ConnectionHandle>>,
    setlist_service: SetlistServiceImpl,
}

impl SessionServiceImpl {
    fn new(
        handle_cell: Arc<OnceLock<ConnectionHandle>>,
        setlist_service: SetlistServiceImpl,
    ) -> Self {
        Self {
            handle_cell,
            setlist_service,
        }
    }

    fn handle(&self) -> &ConnectionHandle {
        self.handle_cell.get().expect("handle not initialized")
    }

    #[allow(dead_code)]
    fn host_client(&self) -> HostServiceClient {
        HostServiceClient::new(self.handle().clone())
    }
}

impl SessionService for SessionServiceImpl {
    async fn get_status(&self, _cx: &Context) -> String {
        "session: healthy".to_string()
    }
}

impl DefinesActions for SessionServiceImpl {
    async fn get_actions(&self, _cx: &Context) -> Vec<ActionDefinition> {
        session::session_actions::definitions()
    }

    async fn execute_action(&self, cx: &Context, action_id: ActionId) -> ActionResult {
        self.dispatch_action(cx, &action_id)
            .await
            .unwrap_or_else(|| ActionResult::failure(format!("Unknown action: {}", action_id)))
    }
}

/// Try to poll readiness for a specific DAW cell name
async fn try_poll_daw_ready(host: &HostServiceClient, cell_name: &str) -> bool {
    // Use a short timeout since we're checking multiple cells
    let policy = WaitPolicy {
        max_attempts: 5,
        initial_backoff_ms: 50,
        max_backoff_ms: 200,
        backoff_multiplier: 1.5,
    };

    match host.poll_ready(cell_name.to_string(), policy).await {
        Ok(resp) if resp.ready => {
            info!("DAW cell '{}' is ready", cell_name);
            true
        }
        Ok(_) => {
            tracing::debug!("DAW cell '{}' not ready", cell_name);
            false
        }
        Err(e) => {
            tracing::debug!("Failed to poll '{}' readiness: {}", cell_name, e);
            false
        }
    }
}

/// Initialize DAW control after the cell is ready
async fn init_daw_control(handle: ConnectionHandle) {
    // Initialize daw-control with the connection handle
    if let Err(e) = Daw::init(handle.clone()) {
        warn!("Failed to initialize daw-control: {}", e);
        return;
    }
    info!("daw-control initialized");

    // Wait for DAW to be ready using host service
    // Note: DAW can be either a separate "daw-standalone" cell or in-process "daw-reaper"
    // In reaper-extension, DAW services are provided in-process as "daw-reaper"
    info!("Waiting for DAW to be ready...");
    let host = HostServiceClient::new(handle);

    // Try daw-reaper first (in-process REAPER), then daw-standalone (separate cell)
    let daw_ready = try_poll_daw_ready(&host, "daw-reaper").await
        || try_poll_daw_ready(&host, "daw-standalone").await;

    if !daw_ready {
        warn!("No DAW cell became ready within timeout");
        info!("Session cell running without transport control.");
        return;
    }
    info!("DAW is ready!");

    // Try to get current project with retry logic
    match wait_for_project_with_retry(3, Duration::from_millis(100)).await {
        Ok(project) => {
            info!("Got current project: {}", project.guid());
        }
        Err(e) => {
            warn!("Could not get current project after retries: {}", e);
            warn!("DAW cell may not be available");
        }
    }

    info!("Session cell fully initialized");
}

/// Retry getting the current project with exponential backoff
async fn wait_for_project_with_retry(
    max_retries: u32,
    delay: Duration,
) -> eyre::Result<daw_control::Project> {
    let daw = Daw::get();
    for attempt in 1..=max_retries {
        match daw.current_project().await {
            Ok(project) => return Ok(project),
            Err(e) => {
                if attempt < max_retries {
                    warn!(
                        "Attempt {}/{}: Failed to get project ({}), retrying in {:?}...",
                        attempt, max_retries, e, delay
                    );
                    tokio::time::sleep(delay).await;
                } else {
                    return Err(e.into());
                }
            }
        }
    }
    Err(eyre::eyre!("Exhausted all retries"))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run_cell!("session", |handle| {
        let telemetry = create_telemetry();

        // Create service implementations
        let song_service = SongServiceImpl::new();
        let setlist_service = SetlistServiceImpl::new();
        let session_service = SessionServiceImpl::new(handle.clone(), setlist_service.clone());

        // Create dispatchers for all services
        let session_dispatcher = SessionServiceDispatcher::new(session_service.clone())
            .with_middleware(telemetry.clone());
        let actions_dispatcher =
            DefinesActionsDispatcher::new(session_service).with_middleware(telemetry.clone());
        let song_dispatcher =
            SongServiceDispatcher::new(song_service).with_middleware(telemetry.clone());
        let setlist_dispatcher =
            SetlistServiceDispatcher::new(setlist_service).with_middleware(telemetry);

        // Spawn DAW initialization in background (after ready is signaled)
        let handle_for_init = handle.clone();
        tokio::spawn(async move {
            // Small delay to ensure ready() has been sent
            tokio::time::sleep(Duration::from_millis(100)).await;

            if let Some(h) = handle_for_init.get() {
                init_daw_control(h.clone()).await;
            }
        });

        // Combine all dispatchers
        // RoutedDispatcher only takes 2, so we need to nest them
        let session_and_actions = RoutedDispatcher::new(session_dispatcher, actions_dispatcher);
        let song_and_setlist = RoutedDispatcher::new(song_dispatcher, setlist_dispatcher);
        RoutedDispatcher::new(session_and_actions, song_and_setlist)
    })
}
