//! Thin shell — env-driven config, then hands off to `task_server::router`.

use std::net::SocketAddr;
use std::sync::Arc;

use agent_hermes::MockIntegration;
use agent_proto::integration::IntegrationRegistry;
use eyre::WrapErr;
use task_db::{WORKSPACE_DOC_ID, default_database_url, open_and_migrate, seed};
use task_server::{AppState, Sealing, router};
use tracing::info;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "task_server=info,task_db=info,tower_http=info".into()),
        )
        .init();

    let database_url = default_database_url();
    let bind: SocketAddr = std::env::var("TASK_SERVER_BIND")
        .unwrap_or_else(|_| "0.0.0.0:9090".into())
        .parse()
        .wrap_err("invalid TASK_SERVER_BIND")?;
    let seed_on_start = env_truthy("TASK_SERVER_SEED");

    info!(%database_url, "connecting");
    let persistence = open_and_migrate(&database_url).await?;

    if seed_on_start {
        info!("TASK_SERVER_SEED=1 — seeding workspace doc before listening");
        seed::run(persistence.clone(), WORKSPACE_DOC_ID).await?;
    }

    let mut state = AppState::new(persistence).await?;

    // Sealing for at-rest webhook secrets. Reads
    // TASK_SERVER_SEALING_KEY when set; falls back to a
    // machine-stable dev key with a warning. Always present in v1 —
    // GitHub-webhook verification needs it.
    state.sealing = Some(Sealing::from_env());

    // Hermes inbound webhook secret. Optional; when unset the
    // `/webhooks/hermes/event` route returns 401 for every request.
    state.hermes_webhook_secret = std::env::var("HERMES_WEBHOOK_SECRET").ok();

    // Build the EventSink that wraps the in-state repos. Shared by
    // every plugin's `run_event_loop`.
    let sink = state.build_event_sink();
    state.event_sink = Some(sink.clone());

    // Plugin registry. Mock is always present; Hermes only when
    // `HERMES_BASE_URL` is configured.
    let mut registry = IntegrationRegistry::new();
    registry.register(Arc::new(MockIntegration::new()));
    if let Ok(hermes_base_url) = std::env::var("HERMES_BASE_URL") {
        // Phase B2 ships only the stub HermesIntegration shell —
        // wire it so the registry has the plugin slot occupied;
        // dispatch returns IntegrationError::Disabled until the
        // parallel HTTP/WS work lands.
        match build_hermes(&hermes_base_url) {
            Ok(p) => registry.register(p),
            Err(e) => tracing::warn!(?e, "skipping hermes plugin"),
        }
    }
    state.registry = Arc::new(registry);

    // Spawn the per-plugin event loops. They keep running until
    // `state.shutdown` flips.
    let shutdown = state.shutdown.clone();
    for plugin in state.registry.all() {
        let sink = sink.clone();
        let s = shutdown.clone();
        let name = plugin.name();
        tokio::spawn(async move {
            if let Err(e) = plugin.run_event_loop(sink, s).await {
                tracing::warn!(plugin = name, ?e, "event loop exited");
            }
        });
    }

    let app = router(state);

    info!(%bind, "listening");
    let listener = tokio::net::TcpListener::bind(bind).await?;
    axum::serve(listener, app).await?;

    shutdown.set();
    Ok(())
}

fn build_hermes(
    base_url: &str,
) -> eyre::Result<Arc<dyn agent_proto::integration::AgentIntegration>> {
    use agent_hermes::{HermesConfig, HermesIntegration};
    let config = HermesConfig {
        base_url: url::Url::parse(base_url).wrap_err("invalid HERMES_BASE_URL")?,
        session_token: std::env::var("HERMES_SESSION_TOKEN").unwrap_or_default(),
        default_board: std::env::var("HERMES_DEFAULT_BOARD").ok(),
        default_profile: std::env::var("HERMES_DEFAULT_PROFILE")
            .unwrap_or_else(|_| "engineer".to_string()),
        webhook_secret: std::env::var("HERMES_WEBHOOK_SECRET").ok(),
    };
    Ok(Arc::new(HermesIntegration::new(config)))
}

fn env_truthy(key: &str) -> bool {
    matches!(
        std::env::var(key).ok().as_deref(),
        Some("1") | Some("true") | Some("TRUE") | Some("yes")
    )
}
