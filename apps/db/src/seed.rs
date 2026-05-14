//! Seed the project CRDT with fake data, then compact into a
//! snapshot the sync-demo server can hydrate clients from.

use std::time::Duration;

use crdt::{CrdtDoc, Persistence};
use crdt_seaorm::SeaOrmPersistence;
use tracing::info;
use uuid::Uuid;

/// Run the seed flow against an already-migrated persistence handle.
pub async fn run(persistence: SeaOrmPersistence, workspace_doc_id: Uuid) -> eyre::Result<()> {
    info!(%workspace_doc_id, "seeding…");

    let cdoc = CrdtDoc::open(workspace_doc_id, persistence.clone()).await?;

    seed_all(&cdoc).await?;

    tokio::time::sleep(Duration::from_millis(250)).await;

    cdoc.compact(workspace_doc_id).await?;

    let snapshot = persistence.load_snapshot(workspace_doc_id).await?;
    info!(
        bytes = snapshot.as_ref().map(|s| s.len()).unwrap_or(0),
        "compacted; sync-demo can now hydrate clients from this snapshot"
    );
    Ok(())
}

async fn seed_all(cdoc: &CrdtDoc) -> eyre::Result<()> {
    info!("seeding project + task fakes…");

    let project_repo = project_crdt::ProjectRepoLoro::new(cdoc);
    let task_repo = project_crdt::TaskRepoLoro::new(cdoc);
    let cycle_repo = project_crdt::CycleRepoLoro::new(cdoc);
    let milestone_repo = project_crdt::MilestoneRepoLoro::new(cdoc);
    project_proto::seed_fake_project(&project_repo, 10usize).await?;
    project_proto::seed_fake_task(&task_repo, 80usize).await?;
    project_proto::seed_fake_cycle(&cycle_repo, 8usize).await?;
    project_proto::seed_fake_milestone(&milestone_repo, 15usize).await?;
    info!("  project: 10 projects, 80 tasks, 8 cycles, 15 milestones");

    Ok(())
}
