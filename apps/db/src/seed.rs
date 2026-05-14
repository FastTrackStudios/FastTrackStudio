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
    use project_proto::architect::Page;
    use project_proto::architect::fake::rand::seq::IndexedRandom;
    use project_proto::architect::fake::{Fake, Faker};
    use project_proto::{ProjectRepo, TaskCreate, TaskRepo};

    info!("seeding project + task fakes…");

    let project_repo = project_crdt::ProjectRepoLoro::new(cdoc);
    let task_repo = project_crdt::TaskRepoLoro::new(cdoc);

    // Seed 10 projects via the architect-emitted faker, then capture
    // their ids so the tasks we generate next reference real
    // projects (otherwise `project_id` is just a random Uuid that
    // matches nothing — fine for type-correctness, useless for
    // demos).
    project_proto::seed_fake_project(&project_repo, 10usize).await?;
    let projects = project_repo
        .list(
            Page {
                index: 0,
                size: 1000,
            },
            None,
            None,
        )
        .await?;
    let project_ids: Vec<Uuid> = projects.items.iter().map(|p| p.id).collect();

    // Hand-roll task creation so each task's `project_id` lands on
    // an actual project. `TaskCreate: Dummy<Faker>` because the
    // wire struct opts into `#[derive(fake::Dummy)]` under the
    // `fake` feature; the architect macro forwards each field's
    // `#[dummy(faker = "...")]` attr onto the create payload.
    let mut rng = project_proto::architect::fake::rand::rng();
    for _ in 0..80 {
        let mut task: TaskCreate = Faker.fake();
        task.project_id = *project_ids
            .choose(&mut rng)
            .expect("at least one project seeded");
        // Subtask + cycle linkage stay None so the demo stays flat.
        task.parent_id = None;
        task.cycle_id = None;
        task_repo.create(task).await?;
    }
    info!(
        "  project: {} projects, 80 tasks (all linked to a real project)",
        project_ids.len()
    );

    Ok(())
}
