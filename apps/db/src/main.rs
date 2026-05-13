//! `task-db` — database control plane. See the package comment in
//! Cargo.toml for the subcommands.

use task_db::{WORKSPACE_DOC_ID, default_database_url, open_and_migrate, seed};
use tracing::info;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "task_db=info".into()),
        )
        .init();

    let cmd = std::env::args().nth(1).unwrap_or_else(|| "all".into());
    let database_url = default_database_url();
    info!(%database_url, ?cmd, "task-db");

    let persistence = open_and_migrate(&database_url).await?;

    match cmd.as_str() {
        "up" | "migrate" => { /* migrations already ran above */ }
        "seed" | "all" | "" => {
            seed::run(persistence, WORKSPACE_DOC_ID).await?;
        }
        other => {
            eprintln!("unknown subcommand: {other}");
            eprintln!("usage: task-db [up|seed|all]");
            std::process::exit(2);
        }
    }
    Ok(())
}
