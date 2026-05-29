//! Reference axum + vox server binary.
//!
//! This is a thin shell: it picks a backend (sqlite or in-memory, via
//! cargo features), runs migrations if needed, then hands the repo to
//! `app_server::vox_router`. All HTTP/vox wiring lives in the lib so the
//! e2e tests mount the exact same router.

use app_server::vox_router;
use tracing::info;

/// Compact, readable logging. The default keeps our own logs at debug and
/// everything else at info, while muting the noisy neighbours: the
/// per-query sqlx/sea-orm chatter and — importantly — cranelift-jit, which
/// `info!`-logs the full CLIF IR of every function the vox JIT compiles.
/// Raise any of them with e.g. `RUST_LOG=sqlx=info`, or override the whole
/// filter with `RUST_LOG=...` for ad-hoc debugging.
fn init_tracing() {
    let filter = tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| {
        [
            "info",
            "app_server=debug",
            "sqlx=warn",
            "sqlx::query=warn",
            "sea_orm=warn",
            // cranelift-jit dumps each compiled function's CLIF at info.
            "cranelift_jit=warn",
            "cranelift_codegen=warn",
        ]
        .join(",")
        .into()
    });
    tracing_subscriber::fmt()
        .with_env_filter(filter)
        .with_target(false) // drop the `sqlx::query:` / module prefix
        .compact()
        .init();
}

// ── Backend-specific state ────────────────────────────────────────────

#[cfg(feature = "backend-db")]
mod backend {
    use example::backend_db::{ExampleRepoStorage, Migrator};
    use sea_orm::{Database, DatabaseConnection};
    use sea_orm_migration::MigratorTrait;

    pub type Repo = ExampleRepoStorage<DatabaseConnection>;

    pub async fn init() -> eyre::Result<(Repo, String)> {
        let database_url = std::env::var("DATABASE_URL")
            .unwrap_or_else(|_| "sqlite://./example.db?mode=rwc".into());
        let db = Database::connect(&database_url).await?;
        Migrator::up(&db, None).await?;
        Ok((
            ExampleRepoStorage::new(db),
            format!("sqlite ({database_url})"),
        ))
    }
}

// Picks backend-memory only when backend-db isn't also enabled — keeps
// `cargo build --all-features` resolvable. In practice each binary
// enables exactly one backend feature.
#[cfg(all(not(feature = "backend-db"), feature = "backend-memory"))]
mod backend {
    use example::backend_memory::ExampleRepoMemory;

    pub type Repo = ExampleRepoMemory;

    pub async fn init() -> eyre::Result<(Repo, String)> {
        Ok((ExampleRepoMemory::new(), "in-memory".into()))
    }
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    init_tracing();

    let bind_addr = std::env::var("BIND_ADDR").unwrap_or_else(|_| "0.0.0.0:4040".into());
    let (repo, backend_label) = backend::init().await?;
    info!(backend = %backend_label, %bind_addr, "starting example server");

    let app = vox_router(repo);

    let listener = tokio::net::TcpListener::bind(&bind_addr).await?;
    info!("HTTP listening on http://{bind_addr}");
    info!("  Health:  http://{bind_addr}/api/health");
    info!("  Vox WS:  ws://{bind_addr}/vox");
    axum::serve(listener, app).await?;
    Ok(())
}
