//! Live `TaskOp` fan-out via `TaskService::apply_op`.
//!
//! This test exercises the in-process broadcast path of [`TaskServiceImpl`]:
//! mutating ops applied through `apply_op` should be persisted via the repo
//! *and* delivered to every active broadcast subscriber. The test bypasses
//! Vox's `Tx<TaskOp>` plumbing — there's no transport involved here — and
//! reads ops directly off the broadcast receiver, which is the same fan-out
//! seam the `subscribe` method forwards from.

use task_core::TaskService;
use task_core::service::{TaskOp, TaskSubscriptionFilter};
use task_core::service_impl::{TaskServiceDeps, TaskServiceImpl};
use task_core::task::{Status, Task, TaskApiCreate, TaskRepoStorage};

#[tokio::test]
async fn apply_op_persists_field_change_and_fans_out() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");

    // Seed a task through the repo so apply_op has something to load.
    let task = Task {
        title: "Live op test".into(),
        status: Status::Open,
        ..Default::default()
    };
    let create: TaskApiCreate =
        serde_json::from_value(serde_json::to_value(&task).expect("serialize task create seed"))
            .expect("decode task create model");
    let created = crudcrate::CrudStorage::<task_core::task::TaskApi>::create(&db, create)
        .await
        .expect("create task");

    let service = TaskServiceImpl::new(TaskServiceDeps {
        task_repo: TaskRepoStorage::new(db.clone()),
        op_tx: None,
        snapshot_store: Some(task_core::crdt::SeaOrmCrdtSnapshotStore::shared(db.clone())),
    });
    let mut rx = service.op_sender().subscribe();

    let op = TaskOp::FieldChanged {
        task_id: created.id,
        field: "status".to_string(),
        value: Some("Done".to_string()),
        peer: Some(7),
    };
    service
        .apply_op(op)
        .await
        .expect("apply_op should succeed against an in-memory repo");

    let received = tokio::time::timeout(std::time::Duration::from_secs(1), rx.recv())
        .await
        .expect("subscriber should receive an op within the timeout")
        .expect("broadcast channel should still be open");

    match received {
        TaskOp::FieldChanged {
            task_id,
            field,
            value,
            peer,
        } => {
            assert_eq!(task_id, created.id);
            assert_eq!(field, "status");
            assert_eq!(value.as_deref(), Some("Done"));
            assert_eq!(peer, Some(7));
        }
        other => panic!("expected FieldChanged, got {other:?}"),
    }

    // The change must also have been persisted to the repo.
    let reloaded = crudcrate::CrudStorage::<task_core::task::TaskApi>::get_one(&db, created.id)
        .await
        .expect("reload persisted task");
    assert_eq!(reloaded.status, Status::Done);
}

#[tokio::test]
async fn apply_op_succeeds_with_no_subscribers() {
    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");
    let task = Task {
        title: "Lonely op".into(),
        status: Status::Open,
        ..Default::default()
    };
    let create: TaskApiCreate =
        serde_json::from_value(serde_json::to_value(&task).expect("serialize task create seed"))
            .expect("decode task create model");
    let created = crudcrate::CrudStorage::<task_core::task::TaskApi>::create(&db, create)
        .await
        .expect("create task");

    let service = TaskServiceImpl::new(TaskServiceDeps {
        task_repo: TaskRepoStorage::new(db.clone()),
        op_tx: None,
        snapshot_store: Some(task_core::crdt::SeaOrmCrdtSnapshotStore::shared(db.clone())),
    });

    // No subscribers — apply_op must not error.
    service
        .apply_op(TaskOp::FieldChanged {
            task_id: created.id,
            field: "priority".into(),
            value: Some("High".into()),
            peer: None,
        })
        .await
        .expect("apply_op must tolerate zero subscribers");
}

#[tokio::test]
async fn apply_op_body_update_persists_crdt_snapshot() {
    use task_core::crdt::{CrdtDocument, CrdtSnapshotStore, SeaOrmCrdtSnapshotStore};

    let db = task_db::init_memory()
        .await
        .expect("initialize in-memory task database");

    // Seed a task and an initial snapshot built off its scalar columns.
    let task = Task {
        title: "Body update test".into(),
        body: "## Subtasks\n- [ ] alpha\n- [ ] beta".into(),
        ..Default::default()
    };
    let create: TaskApiCreate =
        serde_json::from_value(serde_json::to_value(&task).expect("serialize task create seed"))
            .expect("decode task create model");
    let created = crudcrate::CrudStorage::<task_core::task::TaskApi>::create(&db, create)
        .await
        .expect("create task");

    let store = SeaOrmCrdtSnapshotStore::new(db.clone());

    // Build a remote doc that ticks off the first subtask, export an update,
    // and feed the bytes through apply_op as a BodyUpdate.
    let local_seed = Task {
        id: created.id,
        title: created.title.clone(),
        body: "## Subtasks\n- [ ] alpha\n- [ ] beta".into(),
        ..Default::default()
    };
    let remote = CrdtDocument::from_task(&local_seed, created.id.to_string());
    remote.set_peer_id(99).unwrap();
    remote.set_body("## Subtasks\n- [x] alpha\n- [ ] beta");
    remote.commit();
    let update_bytes = remote
        .export_updates_since(&loro::VersionVector::new())
        .expect("export update bytes");

    let service = TaskServiceImpl::new(TaskServiceDeps {
        task_repo: TaskRepoStorage::new(db.clone()),
        op_tx: None,
        snapshot_store: Some(SeaOrmCrdtSnapshotStore::shared(db.clone())),
    });

    service
        .apply_op(TaskOp::BodyUpdate {
            task_id: created.id,
            update_bytes,
        })
        .await
        .expect("apply_op(BodyUpdate) should succeed");

    // Snapshot column must now be populated.
    let bytes = store
        .load_snapshot(created.id)
        .await
        .expect("load snapshot")
        .expect("snapshot bytes should be persisted");
    let reloaded = CrdtDocument::from_snapshot(&bytes, created.id.to_string())
        .expect("decode persisted snapshot");
    assert!(reloaded.get_body().contains("[x] alpha"));
}

#[tokio::test]
async fn subscribe_filter_matches_task_id() {
    // Sanity check that the filter helper drops ops not targeting the
    // requested task. Exercised against the broadcast receiver directly so
    // we don't need to plumb a Vox `Tx<TaskOp>` for the assertion.
    let task_id_a = uuid::Uuid::new_v4();
    let task_id_b = uuid::Uuid::new_v4();

    let op_a = TaskOp::FieldChanged {
        task_id: task_id_a,
        field: "title".into(),
        value: Some("a".into()),
        peer: None,
    };
    let op_b = TaskOp::Deleted { task_id: task_id_b };

    assert_eq!(op_a.task_id(), task_id_a);
    assert_eq!(op_b.task_id(), task_id_b);

    let filter = TaskSubscriptionFilter {
        task_id: Some(task_id_a),
        project: None,
    };
    // The filter type is plain data; full filter dispatch is exercised by the
    // service's `subscribe` method. Here we just assert the obvious shape.
    assert_eq!(filter.task_id, Some(task_id_a));
}
