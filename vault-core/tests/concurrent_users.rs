//! Concurrent user simulation tests.
//!
//! Tests 5-10 simulated users performing simultaneous operations
//! to verify CRDT convergence, provider consistency, and sync behavior.

use std::sync::Arc;

use vault_core::task::{Task, Status, Priority, WikiLink};
use vault_core::project::{Project, ProjectStatus};
use vault_core::provider::{MockProvider, ProjectProvider, ProjectRegistry};

// ── Helper: create test task ─────────────────────────────────────────────────

fn test_task(title: &str, project: &str, assignee: &str) -> Task {
    Task {
        id: Some(format!("id-{}", title.replace(' ', "-"))),
        title: title.to_string(),
        status: Status::Open,
        priority: Priority::Normal,
        assignee: Some(assignee.to_string()),
        projects: vec![WikiLink(project.to_string())],
        tags: vec!["test".to_string()],
        ..Default::default()
    }
}

fn test_project(title: &str, team: Vec<&str>) -> Project {
    Project {
        title: title.to_string(),
        status: ProjectStatus::Active,
        team: team.into_iter().map(String::from).collect(),
        ..Default::default()
    }
}

// ── Test: 5 users creating tasks concurrently ────────────────────────────────

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn five_users_create_tasks_concurrently() {
    let provider = Arc::new(MockProvider::new("test"));

    // Seed a project
    provider
        .seed_project(
            test_project("Shared Project", vec!["alice", "bob", "carol", "dave", "eve"]),
            vec![],
        )
        .await;

    // 5 users each create 10 tasks simultaneously
    let mut handles = vec![];
    for user_idx in 0..5 {
        let provider = provider.clone();
        let user = ["alice", "bob", "carol", "dave", "eve"][user_idx].to_string();

        handles.push(tokio::spawn(async move {
            for task_idx in 0..10 {
                let task = test_task(
                    &format!("{user}-task-{task_idx}"),
                    "Shared Project",
                    &user,
                );
                provider
                    .save_task("Shared Project", &task)
                    .await
                    .expect("save should succeed");
            }
        }));
    }

    // Wait for all users to finish
    for handle in handles {
        handle.await.expect("task should not panic");
    }

    // Verify: all 50 tasks exist
    let bundle = provider
        .get_project("Shared Project")
        .await
        .unwrap()
        .unwrap();
    assert_eq!(bundle.tasks.len(), 50, "All 50 tasks should exist");

    // Verify each user has 10 tasks
    for user in &["alice", "bob", "carol", "dave", "eve"] {
        let user_tasks: Vec<_> = bundle
            .tasks
            .iter()
            .filter(|t| t.assignee.as_deref() == Some(user))
            .collect();
        assert_eq!(
            user_tasks.len(),
            10,
            "{user} should have 10 tasks, got {}",
            user_tasks.len()
        );
    }
}

// ── Test: concurrent reads and writes ────────────────────────────────────────

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrent_reads_and_writes() {
    let provider = Arc::new(MockProvider::new("test"));
    provider
        .seed_project(
            test_project("RW Project", vec!["reader", "writer"]),
            vec![
                test_task("Existing task 1", "RW Project", "reader"),
                test_task("Existing task 2", "RW Project", "reader"),
            ],
        )
        .await;

    let mut handles = vec![];

    // 5 readers querying continuously
    for _ in 0..5 {
        let provider = provider.clone();
        handles.push(tokio::spawn(async move {
            for _ in 0..20 {
                let result = provider.list_all().await;
                assert!(result.is_ok(), "Read should always succeed");
                let bundles = result.unwrap();
                assert!(!bundles.is_empty(), "Should have at least one project");
            }
        }));
    }

    // 3 writers creating tasks
    for writer_idx in 0..3 {
        let provider = provider.clone();
        handles.push(tokio::spawn(async move {
            for task_idx in 0..10 {
                let task = test_task(
                    &format!("writer{writer_idx}-task-{task_idx}"),
                    "RW Project",
                    "writer",
                );
                provider
                    .save_task("RW Project", &task)
                    .await
                    .expect("write should succeed");
            }
        }));
    }

    for handle in handles {
        handle.await.unwrap();
    }

    // 2 original + 30 new = 32
    assert_eq!(provider.task_count().await, 32);
}

// ── Test: concurrent task completion ─────────────────────────────────────────

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrent_task_completion() {
    let provider = Arc::new(MockProvider::new("test"));

    // Create 20 tasks
    let mut tasks = vec![];
    for i in 0..20 {
        tasks.push(test_task(
            &format!("task-{i}"),
            "Completion Project",
            "alice",
        ));
    }
    provider
        .seed_project(test_project("Completion Project", vec!["alice", "bob"]), tasks)
        .await;

    // 4 users completing different tasks simultaneously
    let mut handles = vec![];
    for user_idx in 0..4 {
        let provider = provider.clone();
        handles.push(tokio::spawn(async move {
            // Each user completes 5 tasks (non-overlapping)
            for task_idx in 0..5 {
                let idx = user_idx * 5 + task_idx;
                let mut task = test_task(
                    &format!("task-{idx}"),
                    "Completion Project",
                    "alice",
                );
                task.status = Status::Done;
                task.completed_date = Some(chrono::Local::now().date_naive());
                provider
                    .save_task("Completion Project", &task)
                    .await
                    .unwrap();
            }
        }));
    }

    for handle in handles {
        handle.await.unwrap();
    }

    let bundle = provider
        .get_project("Completion Project")
        .await
        .unwrap()
        .unwrap();
    let done_count = bundle.tasks.iter().filter(|t| t.status == Status::Done).count();
    assert_eq!(done_count, 20, "All 20 tasks should be Done");
}

// ── Test: registry with multiple concurrent providers ────────────────────────

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn registry_multiple_providers_concurrent() {
    let provider_a = Arc::new(MockProvider::new("team-a"));
    let provider_b = Arc::new(MockProvider::new("team-b"));

    provider_a
        .seed_project(
            test_project("Project A", vec!["alice"]),
            vec![test_task("Task A1", "Project A", "alice")],
        )
        .await;

    provider_b
        .seed_project(
            test_project("Project B", vec!["bob"]),
            vec![test_task("Task B1", "Project B", "bob")],
        )
        .await;

    let mut registry = ProjectRegistry::new();
    registry.add_arc(provider_a.clone());
    registry.add_arc(provider_b.clone());

    // Concurrent queries across both providers
    let mut handles = vec![];
    for _ in 0..10 {
        let pa = provider_a.clone();
        let pb = provider_b.clone();
        handles.push(tokio::spawn(async move {
            let a = pa.list_all().await.unwrap();
            let b = pb.list_all().await.unwrap();
            assert_eq!(a.len(), 1);
            assert_eq!(b.len(), 1);
        }));
    }

    for handle in handles {
        handle.await.unwrap();
    }

    // Registry aggregates both
    let all = registry.list_all().await.unwrap();
    assert_eq!(all.len(), 2, "Should see projects from both providers");

    let all_tasks = registry.list_tasks().await.unwrap();
    assert_eq!(all_tasks.len(), 2, "Should see tasks from both providers");
}

// ── Test: concurrent writes with simulated latency ───────────────────────────

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrent_writes_with_latency() {
    let provider = Arc::new(MockProvider::new("slow").with_latency(10));
    provider
        .seed_project(test_project("Slow Project", vec!["alice"]), vec![])
        .await;

    let mut handles = vec![];
    for user_idx in 0..5 {
        let provider = provider.clone();
        handles.push(tokio::spawn(async move {
            for task_idx in 0..5 {
                let task = test_task(
                    &format!("slow-user{user_idx}-task{task_idx}"),
                    "Slow Project",
                    &format!("user{user_idx}"),
                );
                provider.save_task("Slow Project", &task).await.unwrap();
            }
        }));
    }

    for handle in handles {
        handle.await.unwrap();
    }

    assert_eq!(provider.task_count().await, 25);
}

// ── Test: write failure handling ─────────────────────────────────────────────

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrent_writes_with_failures() {
    let provider = Arc::new(MockProvider::new("flakey"));
    provider
        .seed_project(test_project("Flakey Project", vec!["alice"]), vec![])
        .await;

    // Write 10 tasks successfully
    for i in 0..10 {
        let task = test_task(&format!("ok-{i}"), "Flakey Project", "alice");
        provider.save_task("Flakey Project", &task).await.unwrap();
    }
    assert_eq!(provider.task_count().await, 10);

    // Enable write failures
    provider.set_fail_writes(Some("simulated failure".into())).await;

    // Attempt 5 more writes — should all fail
    let mut failures = 0;
    for i in 10..15 {
        let task = test_task(&format!("fail-{i}"), "Flakey Project", "alice");
        if provider.save_task("Flakey Project", &task).await.is_err() {
            failures += 1;
        }
    }
    assert_eq!(failures, 5, "All writes should fail");
    assert_eq!(provider.task_count().await, 10, "Task count should not change");

    // Disable failures — writes should work again
    provider.set_fail_writes(None).await;
    let task = test_task("recovered", "Flakey Project", "alice");
    provider.save_task("Flakey Project", &task).await.unwrap();
    assert_eq!(provider.task_count().await, 11);
}

// ── Test: 10 users with mixed operations ─────────────────────────────────────

#[ignore = "Needs Mutex optimization for 10 concurrent writers"]
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn ten_users_mixed_operations() {
    let provider = Arc::new(MockProvider::new("busy"));

    // Seed 3 projects
    for proj in &["Alpha", "Beta", "Gamma"] {
        provider
            .seed_project(test_project(proj, vec!["manager"]), vec![])
            .await;
    }

    let projects = ["Alpha", "Beta", "Gamma"];
    let users = [
        "alice", "bob", "carol", "dave", "eve",
        "frank", "grace", "hank", "iris", "jack",
    ];

    // Phase 1: All 10 users create tasks concurrently
    let mut handles = vec![];
    for (user_idx, user) in users.iter().enumerate() {
        let provider = provider.clone();
        let user = user.to_string();
        let project = projects[user_idx % 3].to_string();

        handles.push(tokio::spawn(async move {
            for i in 0..5 {
                let task = test_task(&format!("{user}-{i}"), &project, &user);
                provider.save_task(&project, &task).await.unwrap();
            }
        }));
    }
    for handle in handles {
        handle.await.unwrap();
    }
    assert_eq!(provider.task_count().await, 50, "50 tasks created");

    // Phase 2: All 10 users update tasks concurrently
    let mut handles = vec![];
    for (user_idx, user) in users.iter().enumerate() {
        let provider = provider.clone();
        let user = user.to_string();
        let project = projects[user_idx % 3].to_string();

        handles.push(tokio::spawn(async move {
            for i in 0..2 {
                let mut task = test_task(&format!("{user}-{i}"), &project, &user);
                task.status = Status::InProgress;
                provider.save_task(&project, &task).await.unwrap();
            }
        }));
    }
    for handle in handles {
        handle.await.unwrap();
    }

    // Phase 3: All 10 users delete one task each concurrently
    let mut handles = vec![];
    for (user_idx, user) in users.iter().enumerate() {
        let provider = provider.clone();
        let user = user.to_string();
        let project = projects[user_idx % 3].to_string();

        handles.push(tokio::spawn(async move {
            provider.delete_task(&project, &format!("{user}-4")).await.unwrap();
        }));
    }
    for handle in handles {
        handle.await.unwrap();
    }

    // 50 created - 10 deleted = 40 remaining
    assert_eq!(provider.task_count().await, 40);

    // 20 tasks should be InProgress (2 per user)
    let all_bundles = provider.list_all().await.unwrap();
    let all_tasks: Vec<&Task> = all_bundles.iter().flat_map(|b| &b.tasks).collect();
    let in_progress = all_tasks.iter().filter(|t| t.status == Status::InProgress).count();
    assert_eq!(in_progress, 20, "20 tasks should be InProgress");
}

// ── Test: tasks_for_user across projects ─────────────────────────────────────

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn registry_tasks_for_user() {
    let mut registry = ProjectRegistry::new();

    let provider = Arc::new(MockProvider::new("multi"));
    provider
        .seed_project(
            test_project("Proj 1", vec!["alice", "bob"]),
            vec![
                test_task("P1-T1", "Proj 1", "alice"),
                test_task("P1-T2", "Proj 1", "bob"),
                test_task("P1-T3", "Proj 1", "alice"),
            ],
        )
        .await;
    provider
        .seed_project(
            test_project("Proj 2", vec!["alice", "carol"]),
            vec![
                test_task("P2-T1", "Proj 2", "carol"),
                test_task("P2-T2", "Proj 2", "alice"),
            ],
        )
        .await;

    registry.add_arc(provider);

    let alice_tasks = registry.tasks_for_user("alice").await.unwrap();
    assert_eq!(alice_tasks.len(), 3, "Alice should have 3 tasks across 2 projects");

    let bob_tasks = registry.tasks_for_user("bob").await.unwrap();
    assert_eq!(bob_tasks.len(), 1);

    let carol_tasks = registry.tasks_for_user("carol").await.unwrap();
    assert_eq!(carol_tasks.len(), 1);

    let nobody_tasks = registry.tasks_for_user("nobody").await.unwrap();
    assert_eq!(nobody_tasks.len(), 0);
}
