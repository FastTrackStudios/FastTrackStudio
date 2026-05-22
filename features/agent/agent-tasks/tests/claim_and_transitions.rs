//! End-to-end coverage for the claim path + Done-blocked-by-deps
//! transition. These are the bits that would silently corrupt the
//! queue if they regressed, so they get a dedicated test file.

use agent_proto::AgentError;
use agent_proto::tasks::{AgentTask, AgentTaskLinkKind, AgentTaskStatus, Queue, QueueFilter};
use chrono::Utc;
use tempfile::TempDir;

use agent_tasks::Store;

fn empty_task(queue_id: &str, title: &str) -> AgentTask {
    AgentTask {
        id: String::new(),
        queue_id: queue_id.into(),
        title: title.into(),
        description: String::new(),
        status: AgentTaskStatus::Ready,
        priority: 2,
        labels: Vec::new(),
        source_task: String::new(),
        project: String::new(),
        agent_profile: String::new(),
        claimed_by: String::new(),
        claim_expires_at: None,
        worker_pid: 0,
        result_blob: String::new(),
        linked_session_id: String::new(),
        created_at: Utc::now(),
        updated_at: Utc::now(),
        archived_at: None,
    }
}

fn new_store() -> (TempDir, Store) {
    let tmp = TempDir::new().expect("tempdir");
    let path = tmp.path().join("q.db");
    let store = Store::open(&path).expect("open");
    let q = Queue {
        id: "q1".into(),
        label: "Default".into(),
        assignees: Vec::new(),
        created_at: Utc::now(),
        updated_at: Utc::now(),
    };
    store.upsert_queue(q).expect("upsert");
    (tmp, store)
}

#[test]
fn claim_succeeds_then_second_claim_conflicts() {
    let (_tmp, store) = new_store();
    let t = store.upsert_agent_task(empty_task("q1", "first")).unwrap();
    let claimed = store.claim_agent_task(&t.id, "worker-a").unwrap();
    assert_eq!(claimed.status, AgentTaskStatus::Running);
    assert_eq!(claimed.claimed_by, "worker-a");
    let err = store.claim_agent_task(&t.id, "worker-b").unwrap_err();
    assert!(matches!(err, agent_tasks::TasksDbError::Conflict(_)));
}

#[test]
fn cannot_set_running_directly() {
    let (_tmp, store) = new_store();
    let t = store.upsert_agent_task(empty_task("q1", "x")).unwrap();
    let err = store
        .set_agent_task_status(&t.id, AgentTaskStatus::Running)
        .unwrap_err();
    assert!(matches!(
        err,
        agent_tasks::TasksDbError::InvalidTransition(_)
    ));
}

#[test]
fn cannot_complete_while_blocked_by_dep() {
    let (_tmp, store) = new_store();
    let parent = store.upsert_agent_task(empty_task("q1", "parent")).unwrap();
    let child = store.upsert_agent_task(empty_task("q1", "child")).unwrap();
    store
        .add_agent_task_link(&parent.id, &child.id, AgentTaskLinkKind::Blocks)
        .unwrap();
    let err = store.complete_agent_task(&child.id, "{}").unwrap_err();
    assert!(matches!(
        err,
        agent_tasks::TasksDbError::InvalidTransition(_)
    ));

    // Parent done → child can now complete.
    store.complete_agent_task(&parent.id, "{}").unwrap();
    let done = store.complete_agent_task(&child.id, "{}").unwrap();
    assert_eq!(done.status, AgentTaskStatus::Done);
}

#[test]
fn done_clears_claim_state() {
    let (_tmp, store) = new_store();
    let t = store.upsert_agent_task(empty_task("q1", "x")).unwrap();
    store.claim_agent_task(&t.id, "worker-a").unwrap();
    let done = store.complete_agent_task(&t.id, r#"{"ok":true}"#).unwrap();
    assert_eq!(done.status, AgentTaskStatus::Done);
    assert_eq!(done.claimed_by, "");
    assert!(done.claim_expires_at.is_none());
    assert_eq!(done.result_blob, r#"{"ok":true}"#);
}

#[test]
fn filter_excludes_archived_by_default() {
    let (_tmp, store) = new_store();
    let a = store.upsert_agent_task(empty_task("q1", "a")).unwrap();
    let b = store.upsert_agent_task(empty_task("q1", "b")).unwrap();
    store
        .set_agent_task_status(&b.id, AgentTaskStatus::Archived)
        .unwrap();
    let snap = store
        .read_queue("q1", QueueFilter::default_filter())
        .unwrap();
    assert_eq!(snap.tasks.len(), 1, "archived should be hidden");
    assert_eq!(snap.tasks[0].id, a.id);

    let mut with_arch = QueueFilter::default_filter();
    with_arch.include_archived = true;
    let snap2 = store.read_queue("q1", with_arch).unwrap();
    assert_eq!(snap2.tasks.len(), 2);
}

#[test]
fn agent_error_conversion_for_conflict() {
    let (_tmp, store) = new_store();
    let t = store.upsert_agent_task(empty_task("q1", "x")).unwrap();
    store.claim_agent_task(&t.id, "worker-a").unwrap();
    let err: AgentError = store
        .claim_agent_task(&t.id, "worker-b")
        .unwrap_err()
        .into();
    assert!(matches!(err, AgentError::Conflict(_)));
}

// Helper since the proto's `QueueFilter` doesn't derive `Default`
// (all-empty-strings is a meaningful filter that needs to be
// explicit).
trait FilterDefault {
    fn default_filter() -> Self;
}
impl FilterDefault for QueueFilter {
    fn default_filter() -> Self {
        QueueFilter {
            assignee: String::new(),
            include_archived: false,
            only_handle: String::new(),
            linked_session_id: String::new(),
            agent_profile: String::new(),
        }
    }
}
