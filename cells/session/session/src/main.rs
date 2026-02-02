//! Session Cell - Control Surface for DAW
//!
//! This cell provides a control surface that connects to a DAW implementation
//! through daw-control and presents transport controls.
//!
//! Note: This cell uses explicit setup instead of `run_cell!` macro because
//! it needs to perform application logic (DAW control) after driver startup.

use actions_proto::{
    ActionCategory, ActionDefinition, ActionId, ActionResult, DefinesActions,
    DefinesActionsDispatcher,
};
use cell_runtime::{
    CellTracingDispatcher, DiagnosticState, HostServiceClient, ReadyMsg, RoutedDispatcher,
    ShmGuestTransport, SpawnArgs, WaitPolicy, dump_all_diagnostics,
    establish_guest_with_diagnostics, init_cell_tracing, install_sigusr1_handler,
    register_diagnostic, register_diagnostic_state, tracing_subscriber, ur_taking_me_with_you,
};
use daw_control::Daw;
use eyre::Result;
use roam::session::Context;
use roam_telemetry::{
    ExporterConfig, LoggingExporter, OtlpExporter, SpanExporter, TelemetryMiddleware,
};
use session_proto::{SessionService, SessionServiceDispatcher};
use std::sync::Arc;
use std::time::Duration;
use tracing::{info, warn};
use tracing_subscriber::prelude::*;

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
pub struct SessionServiceImpl;

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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Install SIGUSR1 handler for diagnostics
    install_sigusr1_handler("cell-session");

    // Register diagnostic callback
    register_diagnostic(|| {
        let diagnostics = dump_all_diagnostics();
        if !diagnostics.is_empty() {
            eprint!("{}", diagnostics);
        }
    });

    // Ensure this process dies when the parent dies
    ur_taking_me_with_you::die_with_parent();

    tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?
        .block_on(async_main())
}

async fn async_main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize cell-side tracing
    let (tracing_layer, tracing_guard) = init_cell_tracing(1024);
    tracing_subscriber::registry().with(tracing_layer).init();

    info!("Session Cell starting...");

    // Parse spawn args and create transport
    let args = SpawnArgs::from_env()?;
    let peer_id = args.peer_id;
    let transport = ShmGuestTransport::from_spawn_args(args)?;
    info!("Connecting to host SHM segment...");

    // Create telemetry middleware
    let telemetry = create_telemetry();

    // Create service implementation
    let service_impl = SessionServiceImpl;

    // Create dispatchers for both services
    let session_dispatcher =
        SessionServiceDispatcher::new(service_impl.clone()).with_middleware(telemetry.clone());
    let actions_dispatcher = DefinesActionsDispatcher::new(service_impl).with_middleware(telemetry);

    // Combine session + actions dispatchers
    let services_dispatcher = RoutedDispatcher::new(session_dispatcher, actions_dispatcher);

    // Combine with tracing dispatcher
    let tracing_dispatcher = CellTracingDispatcher::new(tracing_guard.service());
    let dispatcher = RoutedDispatcher::new(tracing_dispatcher, services_dispatcher);

    // Create diagnostic state
    let diagnostic_state = Arc::new(DiagnosticState::new("cell-session".to_string()));
    register_diagnostic_state(&diagnostic_state);

    // Establish guest connection
    let (connection_handle, _incoming, driver) =
        establish_guest_with_diagnostics(transport, dispatcher, Some(diagnostic_state));

    info!("Connected to host!");

    // Spawn driver FIRST - it needs to be running for RPC calls to work
    let driver_handle = tokio::spawn(async move {
        if let Err(e) = driver.run().await {
            eprintln!("Driver error: {:?}", e);
            std::process::exit(1);
        }
    });

    // Start tracing after driver is spawned (needs driver for RPC)
    tracing_guard.start(connection_handle.clone()).await;

    // Signal readiness to host
    let host = HostServiceClient::new(connection_handle.clone());
    host.ready(ReadyMsg {
        peer_id: peer_id.get() as u16,
        cell_name: "session".to_string(),
        pid: Some(std::process::id()),
    })
    .await?;

    // Initialize daw-control with the connection handle
    Daw::init(connection_handle.clone())?;
    info!("daw-control initialized");

    // Wait for DAW cell to be ready using host service (tower::Service pattern)
    info!("Waiting for DAW cell to be ready...");
    let poll_response = host
        .poll_ready("daw-standalone".to_string(), WaitPolicy::default())
        .await?;

    if !poll_response.ready {
        warn!("DAW cell did not become ready within timeout");
        info!("Session cell running without transport control.");
        let _ = driver_handle.await;
        return Ok(());
    }
    info!("DAW cell is ready!");

    // Try to get current project with retry logic (short retries since we know DAW is ready)
    let _project = match wait_for_project_with_retry(3, Duration::from_millis(100)).await {
        Ok(project) => {
            info!("Got current project: {}", project.guid());
            project
        }
        Err(e) => {
            warn!("Could not get current project after retries: {}", e);
            warn!("DAW cell may not be available");

            info!("Session cell running without transport control.");
            let _ = driver_handle.await;
            return Ok(());
        }
    };

    // Session cell is ready - just wait for driver to stop
    info!("Session cell ready, waiting for shutdown signal...");
    let _ = driver_handle.await;
    info!("Session cell shutting down...");
    Ok(())
}

/// Retry getting the current project with exponential backoff
async fn wait_for_project_with_retry(
    max_retries: u32,
    delay: Duration,
) -> Result<daw_control::Project> {
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
