//! Session Cell - Control Surface for DAW
//!
//! This cell provides a control surface that connects to a DAW implementation
//! through daw-control and presents transport controls.

use actions_proto::{
    ActionCategory, ActionDefinition, ActionId, ActionResult, DefinesActions,
    DefinesActionsDispatcher,
};
use cell_runtime::{HostServiceClient, WaitPolicy, run_cell};
use daw_control::Daw;
use roam::session::{ConnectionHandle, Context};
use roam_telemetry::{
    ExporterConfig, LoggingExporter, OtlpExporter, SpanExporter, TelemetryMiddleware,
};
use session_proto::{SessionService, SessionServiceDispatcher};
use std::sync::{Arc, OnceLock};
use std::time::Duration;
use tracing::{info, warn};

/// Composite exporter that sends to both OTLP and console
#[derive(Clone)]
struct CompositeExporter {
    otlp: OtlpExporter,
    logging: LoggingExporter,
}

impl SpanExporter for CompositeExporter {
    fn send(&self, span: roam_telemetry::Span) {
        self.logging.send(span.clone());
        self.otlp.send(span);
    }

    fn service_name(&self) -> &str {
        "session"
    }
}

fn create_telemetry() -> TelemetryMiddleware<CompositeExporter> {
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

    let logging_exporter = LoggingExporter::new("session");

    TelemetryMiddleware::new(CompositeExporter {
        otlp: otlp_exporter,
        logging: logging_exporter,
    })
}

/// Session actions defined by this cell
fn session_actions() -> Vec<ActionDefinition> {
    vec![
        ActionDefinition::new(
            "fts.session.log_hello",
            "Log Hello",
            "Logs 'Hello from session!' to demonstrate the action system",
        )
        .with_category(ActionCategory::Session)
        .with_menu_path("FTS/Session"),
        ActionDefinition::new(
            "fts.session.log_status",
            "Log Status",
            "Logs current session status",
        )
        .with_category(ActionCategory::Session)
        .with_menu_path("FTS/Session"),
    ]
}

/// Implementation of SessionService and DefinesActions
#[derive(Clone)]
pub struct SessionServiceImpl {
    handle_cell: Arc<OnceLock<ConnectionHandle>>,
}

impl SessionServiceImpl {
    fn new(handle_cell: Arc<OnceLock<ConnectionHandle>>) -> Self {
        Self { handle_cell }
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
        session_actions()
    }

    async fn execute_action(&self, _cx: &Context, action_id: ActionId) -> ActionResult {
        match action_id.as_str() {
            "fts.session.log_hello" => {
                info!("Hello from session!");
                ActionResult::success_with_message("Logged hello from session")
            }
            "fts.session.log_status" => {
                info!("Session status: healthy");
                ActionResult::success_with_message("Logged session status")
            }
            _ => ActionResult::failure(format!("Unknown action: {}", action_id)),
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

    // Wait for DAW cell to be ready using host service
    info!("Waiting for DAW cell to be ready...");
    let host = HostServiceClient::new(handle);
    let poll_response = match host
        .poll_ready("daw-standalone".to_string(), WaitPolicy::default())
        .await
    {
        Ok(resp) => resp,
        Err(e) => {
            warn!("Failed to poll DAW readiness: {}", e);
            return;
        }
    };

    if !poll_response.ready {
        warn!("DAW cell did not become ready within timeout");
        info!("Session cell running without transport control.");
        return;
    }
    info!("DAW cell is ready!");

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

        // Create service implementation with access to handle
        let service_impl = SessionServiceImpl::new(handle.clone());

        // Create dispatchers for both services
        let session_dispatcher =
            SessionServiceDispatcher::new(service_impl.clone()).with_middleware(telemetry.clone());
        let actions_dispatcher =
            DefinesActionsDispatcher::new(service_impl).with_middleware(telemetry);

        // Spawn DAW initialization in background (after ready is signaled)
        let handle_for_init = handle.clone();
        tokio::spawn(async move {
            // Small delay to ensure ready() has been sent
            tokio::time::sleep(Duration::from_millis(100)).await;

            if let Some(h) = handle_for_init.get() {
                init_daw_control(h.clone()).await;
            }
        });

        // Return combined dispatcher
        RoutedDispatcher::new(session_dispatcher, actions_dispatcher)
    })
}
