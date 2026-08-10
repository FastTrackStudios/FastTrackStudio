//! Why the registry owns its own SQLite file.
//!
//! Two SeaORM migrators over one database share a single
//! `seaql_migrations` table, and the second one to run **silently
//! applies nothing** — no error, no table. Colocating the registry
//! with the agent-task queue cost an afternoon exactly once; these
//! tests make sure it costs nobody a second one.

use agent_proto::backend::{AgentBackend, BackendKind};
use agent_proto::runner::{Capability, RunnerProfile, RunnerScope};
use agent_proto::service::backends::Backends;
use sea_orm::{ConnectionTrait, Database, DatabaseConnection, Statement};
use sea_orm_migration::MigratorTrait;

fn runner(id: &str) -> AgentBackend {
    AgentBackend {
        id: id.into(),
        label: id.into(),
        kind: BackendKind::CliBridge,
        config_json: String::new(),
        registered_at: chrono::Utc::now(),
        last_seen: None,
        runner: RunnerProfile {
            id: id.into(),
            capabilities: vec![Capability::Records, Capability::Build],
            scope: RunnerScope::unrestricted(),
            max_concurrent: 2,
        },
    }
}

async fn has_table(conn: &DatabaseConnection, name: &str) -> bool {
    conn.query_all(Statement::from_string(
        conn.get_database_backend(),
        format!("SELECT name FROM sqlite_master WHERE type='table' AND name='{name}'"),
    ))
    .await
    .unwrap()
    .len()
        == 1
}

/// The arrangement the server uses: the registry has its own
/// database, so its migrator owns its own `seaql_migrations`.
#[tokio::test]
async fn the_registry_migrates_cleanly_in_its_own_database() {
    let conn = Database::connect("sqlite::memory:").await.unwrap();
    agent_runners::Migrator::up(&conn, None).await.unwrap();
    assert!(has_table(&conn, "agent_backends").await);

    let store = agent_runners::Store::new(conn);
    store.upsert_backend(runner("thebattleship")).await.unwrap();
    assert_eq!(store.list_backends().await.unwrap().len(), 1);
}

/// Re-running the migrator is a no-op, as it is on every boot.
#[tokio::test]
async fn re_running_the_migrator_is_idempotent() {
    let conn = Database::connect("sqlite::memory:").await.unwrap();
    for _ in 0..3 {
        agent_runners::Migrator::up(&conn, None).await.unwrap();
    }
    let store = agent_runners::Store::new(conn);
    store.upsert_backend(runner("thebattleship")).await.unwrap();
    assert_eq!(store.list_backends().await.unwrap().len(), 1);
}

/// The hazard itself, pinned so the behaviour is documented rather
/// than rediscovered: run the queue's migrator first and the
/// registry's second over one database, and the registry's table is
/// never created — **and no error is raised**. That silence is what
/// makes this worth a test.
///
/// If a future SeaORM starts erroring here instead, this test fails
/// and the comment above the server wiring can be relaxed.
#[tokio::test]
async fn two_migrators_over_one_database_silently_skip_the_second() {
    let conn = Database::connect("sqlite::memory:").await.unwrap();

    agent_tasks::Migrator::up(&conn, None).await.unwrap();
    let second = agent_runners::Migrator::up(&conn, None).await;

    assert!(
        second.is_ok(),
        "the failure mode is silence, not an error: {second:?}"
    );
    assert!(
        !has_table(&conn, "agent_backends").await,
        "if this now passes, SeaORM changed and the registry could share a file again"
    );
}
