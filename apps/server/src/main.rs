//! Thin shell — env-driven config, then hands off to `task_server::router`.

use std::net::SocketAddr;

use eyre::WrapErr;
use task_db::{WORKSPACE_DOC_ID, default_database_url, open_and_migrate, seed};
use task_server::{AppState, router};
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

    let state = AppState::new(persistence).await?;

    if seed_on_start {
        seed_knowledge_org_vault(&state).await?;
    }

    let app = router(state);

    info!(%bind, "listening");
    let listener = tokio::net::TcpListener::bind(bind).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

/// Ensure the org vault doc has an "Org" vault row. Idempotent.
/// Phase 5c demo seeding so the Knowledge route renders something
/// on first paint without needing the CLI.
async fn seed_knowledge_org_vault(state: &AppState) -> eyre::Result<()> {
    use knowledge_proto::{VaultCreate, VaultRepo};
    let big = project_proto::architect::Page {
        index: 0,
        size: 1000,
    };
    let vaults = state
        .vault_repo
        .list(big, None, None)
        .await
        .map_err(|e| eyre::eyre!("seed vault list: {e}"))?;
    if !vaults.items.is_empty() {
        info!(count = vaults.items.len(), "org vault already seeded");
        return Ok(());
    }
    let v = state
        .vault_repo
        .create(VaultCreate {
            name: "Org".into(),
            root_path: None,
            use_markdown_links: false,
            new_link_format: "shortest".into(),
            attachment_folder_path: "".into(),
            default_view_mode: "source".into(),
            config_json: "{}".into(),
        })
        .await
        .map_err(|e| eyre::eyre!("seed vault create: {e}"))?;
    info!(vault_id = %v.id, "seeded Org vault");
    Ok(())
}

fn env_truthy(key: &str) -> bool {
    matches!(
        std::env::var(key).ok().as_deref(),
        Some("1") | Some("true") | Some("TRUE") | Some("yes")
    )
}
