//! Native integration tests for the `project` feature. One ephemeral
//! `CrdtDoc` hosts every entity type — proves they coexist under one
//! root and survive cross-entity sync round-trips.

#![cfg(test)]

use architect::Page;
use chrono::{Duration, Utc};
use loro::ExportMode;
use project_crdt::{CrdtDoc, CycleRepoLoro, MilestoneRepoLoro, ProjectRepoLoro, TaskRepoLoro};
use project_proto::{
    CycleCreate, CycleRepo, MilestoneCreate, MilestoneRepo, ProjectCreate, ProjectRepo, TaskCreate,
    TaskRepo,
};
use uuid::Uuid;

async fn seed_project_with_cycle(p: &ProjectRepoLoro, c: &CycleRepoLoro) -> (Uuid, Uuid) {
    let project = p
        .create(ProjectCreate {
            name: "Demo Project".into(),
            description: Some("for tests".into()),
            status: "active".into(),
            project_type: Some("audio-production".into()),
            color: None,
            owner: Some("alice".into()),
        })
        .await
        .unwrap();
    let cycle = c
        .create(CycleCreate {
            project_id: project.id,
            name: "Sprint 1".into(),
            start_date: Utc::now(),
            end_date: Utc::now() + Duration::days(14),
            status: "active".into(),
        })
        .await
        .unwrap();
    (project.id, cycle.id)
}

#[tokio::test]
async fn project_create_then_get() {
    let doc = CrdtDoc::ephemeral();
    let repo = ProjectRepoLoro::new(&doc);

    let p = repo
        .create(ProjectCreate {
            name: "Tape".into(),
            description: None,
            status: "active".into(),
            project_type: Some("audio-production".into()),
            color: Some("#ff8800".into()),
            owner: Some("cody".into()),
        })
        .await
        .unwrap();
    let got = repo.get(p.id).await.unwrap();
    assert_eq!(got.name, "Tape");
    assert_eq!(got.project_type.as_deref(), Some("audio-production"));
    assert_eq!(got.color.as_deref(), Some("#ff8800"));
}

#[tokio::test]
async fn task_round_trip_with_full_payload() {
    let doc = CrdtDoc::ephemeral();
    let projects = ProjectRepoLoro::new(&doc);
    let cycles = CycleRepoLoro::new(&doc);
    let tasks = TaskRepoLoro::new(&doc);

    let (project_id, cycle_id) = seed_project_with_cycle(&projects, &cycles).await;

    let t = tasks
        .create(TaskCreate {
            project_id,
            parent_id: None,
            cycle_id: Some(cycle_id),
            title: "Mix bus EQ".into(),
            description: Some("Subtractive EQ on the bus".into()),
            status: "in-progress".into(),
            priority: "high".into(),
            assignee: Some("cody".into()),
            estimate_minutes: Some(90),
            due_date: Some(Utc::now() + Duration::days(2)),
            tags: vec!["mix".into(), "eq".into()],
            sort_index: 0,
            completed_at: None,
            agent_run_id: None,
            branch_name: None,
            pr_urls: Vec::new(),
            commit_refs: Vec::new(),
        })
        .await
        .unwrap();

    let got = tasks.get(t.id).await.unwrap();
    assert_eq!(got.title, "Mix bus EQ");
    assert_eq!(got.project_id, project_id);
    assert_eq!(got.cycle_id, Some(cycle_id));
    assert_eq!(got.priority, "high");
    assert_eq!(got.tags, vec!["mix".to_string(), "eq".to_string()]);
    assert_eq!(got.estimate_minutes, Some(90));
}

#[tokio::test]
async fn milestone_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let projects = ProjectRepoLoro::new(&doc);
    let milestones = MilestoneRepoLoro::new(&doc);

    let p = projects
        .create(ProjectCreate {
            name: "Album".into(),
            description: None,
            status: "active".into(),
            project_type: None,
            color: None,
            owner: None,
        })
        .await
        .unwrap();

    let target = Utc::now() + Duration::days(60);
    let m = milestones
        .create(MilestoneCreate {
            project_id: p.id,
            name: "Mixing complete".into(),
            description: Some("All tracks bounced to stems".into()),
            target_date: Some(target),
            completed_at: None,
        })
        .await
        .unwrap();

    let got = milestones.get(m.id).await.unwrap();
    assert_eq!(got.name, "Mixing complete");
    assert_eq!(got.project_id, p.id);
    assert!(got.target_date.is_some());
}

#[tokio::test]
async fn all_four_entities_coexist_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let projects = ProjectRepoLoro::new(&doc);
    let tasks = TaskRepoLoro::new(&doc);
    let cycles = CycleRepoLoro::new(&doc);
    let milestones = MilestoneRepoLoro::new(&doc);

    let p = projects
        .create(ProjectCreate {
            name: "Coexist".into(),
            description: None,
            status: "active".into(),
            project_type: None,
            color: None,
            owner: None,
        })
        .await
        .unwrap();

    cycles
        .create(CycleCreate {
            project_id: p.id,
            name: "C1".into(),
            start_date: Utc::now(),
            end_date: Utc::now() + Duration::days(7),
            status: "active".into(),
        })
        .await
        .unwrap();

    tasks
        .create(TaskCreate {
            project_id: p.id,
            parent_id: None,
            cycle_id: None,
            title: "T1".into(),
            description: None,
            status: "open".into(),
            priority: "medium".into(),
            assignee: None,
            estimate_minutes: None,
            due_date: None,
            tags: vec![],
            sort_index: 0,
            completed_at: None,
            agent_run_id: None,
            branch_name: None,
            pr_urls: Vec::new(),
            commit_refs: Vec::new(),
        })
        .await
        .unwrap();

    milestones
        .create(MilestoneCreate {
            project_id: p.id,
            name: "M1".into(),
            description: None,
            target_date: None,
            completed_at: None,
        })
        .await
        .unwrap();

    // Each repo lists only its own root container — no cross-talk.
    assert_eq!(
        projects
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
    assert_eq!(
        tasks
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
    assert_eq!(
        cycles
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
    assert_eq!(
        milestones
            .list(
                Page {
                    index: 0,
                    size: 100
                },
                None,
                None
            )
            .await
            .unwrap()
            .total,
        1
    );
}

#[tokio::test]
async fn replicas_converge_across_all_entities() {
    let a = CrdtDoc::ephemeral();
    let b = CrdtDoc::ephemeral();
    let pa = ProjectRepoLoro::new(&a);
    let pb = ProjectRepoLoro::new(&b);
    let ta = TaskRepoLoro::new(&a);
    let tb = TaskRepoLoro::new(&b);

    let proj_a = pa
        .create(ProjectCreate {
            name: "A".into(),
            description: None,
            status: "active".into(),
            project_type: None,
            color: None,
            owner: None,
        })
        .await
        .unwrap();
    let proj_b = pb
        .create(ProjectCreate {
            name: "B".into(),
            description: None,
            status: "active".into(),
            project_type: None,
            color: None,
            owner: None,
        })
        .await
        .unwrap();
    ta.create(TaskCreate {
        project_id: proj_a.id,
        parent_id: None,
        cycle_id: None,
        title: "from-a".into(),
        description: None,
        status: "open".into(),
        priority: "low".into(),
        assignee: None,
        estimate_minutes: None,
        due_date: None,
        tags: vec![],
        sort_index: 0,
        completed_at: None,
        agent_run_id: None,
        branch_name: None,
        pr_urls: Vec::new(),
        commit_refs: Vec::new(),
    })
    .await
    .unwrap();
    tb.create(TaskCreate {
        project_id: proj_b.id,
        parent_id: None,
        cycle_id: None,
        title: "from-b".into(),
        description: None,
        status: "open".into(),
        priority: "low".into(),
        assignee: None,
        estimate_minutes: None,
        due_date: None,
        tags: vec![],
        sort_index: 0,
        completed_at: None,
        agent_run_id: None,
        branch_name: None,
        pr_urls: Vec::new(),
        commit_refs: Vec::new(),
    })
    .await
    .unwrap();

    let ab = pa.doc().export(ExportMode::all_updates()).unwrap();
    let bb = pb.doc().export(ExportMode::all_updates()).unwrap();
    pb.doc().import(&ab).unwrap();
    pa.doc().import(&bb).unwrap();

    // Both Project and Task counts should converge to 2 on each side.
    assert_eq!(
        pa.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        pb.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        ta.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        tb.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
}

// ── Phase A: agent + git Task field round-trips ──────────────────────

#[tokio::test]
async fn task_agent_and_git_fields_round_trip() {
    use project_proto::{CommitRef, TaskUpdate};

    let doc = CrdtDoc::ephemeral();
    let projects = ProjectRepoLoro::new(&doc);
    let tasks = TaskRepoLoro::new(&doc);

    let p = projects
        .create(ProjectCreate {
            name: "Hermes-test".into(),
            description: None,
            status: "active".into(),
            project_type: None,
            color: None,
            owner: None,
        })
        .await
        .unwrap();

    let now = Utc::now();
    let run_id = Uuid::new_v4();
    let commit_a = CommitRef {
        sha: "a".repeat(40),
        message: "feat: add CSV export".into(),
        url: Some("https://github.com/Codys-Wright/Task/commit/aaaaaaaa".into()),
        authored_at: now,
    };
    let commit_b = CommitRef {
        sha: "b".repeat(40),
        message: "fix: handle empty rows".into(),
        url: None,
        authored_at: now,
    };

    let t = tasks
        .create(TaskCreate {
            project_id: p.id,
            parent_id: None,
            cycle_id: None,
            title: "Hermes wiring".into(),
            description: None,
            status: "in-progress".into(),
            priority: "high".into(),
            assignee: None,
            estimate_minutes: None,
            due_date: None,
            tags: vec![],
            sort_index: 0,
            completed_at: None,
            agent_run_id: Some(run_id),
            branch_name: Some("cwright/PRJ-3F9A-add-csv-export".into()),
            pr_urls: vec![
                "https://github.com/Codys-Wright/Task/pull/42".into(),
                "https://github.com/Codys-Wright/Task/pull/43".into(),
            ],
            commit_refs: vec![commit_a.clone(), commit_b.clone()],
        })
        .await
        .unwrap();

    let got = tasks.get(t.id).await.unwrap();
    assert_eq!(got.agent_run_id, Some(run_id));
    assert_eq!(
        got.branch_name.as_deref(),
        Some("cwright/PRJ-3F9A-add-csv-export")
    );
    assert_eq!(got.pr_urls.len(), 2);
    assert_eq!(got.commit_refs.len(), 2);
    assert_eq!(got.commit_refs[0], commit_a);
    assert_eq!(got.commit_refs[1], commit_b);

    // Patching commit_refs replaces the whole list.
    let updated = tasks
        .update(
            t.id,
            TaskUpdate {
                commit_refs: Some(vec![commit_b.clone()]),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    assert_eq!(updated.commit_refs, vec![commit_b]);
}

#[test]
fn branch_slug_matches_doc_example() {
    let id = Uuid::parse_str("3f9a1234-5678-90ab-cdef-1234567890ab").unwrap();
    let s = project_proto::branch_slug("cwright", "PRJ", id, "Add CSV export");
    assert_eq!(s, "cwright/PRJ-3F9A-add-csv-export");
}
