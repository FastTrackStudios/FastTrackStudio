//! Integration test for `AgentServiceImpl` against `MockIntegration`.
//!
//! Validates the full MVP RPC surface:
//! - `start_run` creates a Loro-backed `AgentRun`, dispatches it to
//!   the registered integration, and patches in `external_id` +
//!   `external_url` on success.
//! - `cancel` transitions a queued/running run to `cancelled`.
//! - `approve_tool` / `deny_tool` patch a pending `ToolCall` row.
//!
//! Per goal P2: closes the "Hermes adapter via AgentService + tests"
//! requirement using the existing `MockIntegration` plugin (no real
//! Hermes process required).

use std::sync::Arc;

use agent::server::{AgentRunRepoLoro, ToolCallRepoLoro};
use agent::{AgentServiceImpl, PluginRouter};
use agent_hermes::mock::MockIntegration;
use agent_proto::integration::IntegrationRegistry;
use agent_proto::{
    AgentRunRepo, AgentService, RunStatus, ToolCallCreate, ToolCallRepo,
};
use chrono::Utc;
use crdt::CrdtDoc;
use uuid::Uuid;

/// Test router — maps every kind to the `"mock"` plugin (the
/// MockIntegration's `name()`). The production router maps to
/// `"hermes"`; we override here so tests don't need a live Hermes.
struct MockRouter;
impl PluginRouter for MockRouter {
    fn plugin_for_kind(&self, _kind: &str) -> &'static str {
        "mock"
    }
}

fn build_service() -> (AgentServiceImpl, AgentRunRepoLoro, ToolCallRepoLoro) {
    let doc = CrdtDoc::ephemeral();
    let runs = AgentRunRepoLoro::new(&doc);
    let tool_calls = ToolCallRepoLoro::new(&doc);
    let mut registry = IntegrationRegistry::new();
    registry.register(Arc::new(MockIntegration::new()));
    let service = AgentServiceImpl::new(runs.clone(), tool_calls.clone(), registry)
        .with_router(Arc::new(MockRouter));
    (service, runs, tool_calls)
}

#[tokio::test]
async fn start_run_creates_run_and_dispatches_to_mock() {
    let (service, runs, _tools) = build_service();
    let run = service
        .start_run("Summarize the README".into(), "hermes".into(), None)
        .await
        .expect("start_run");

    // The created row should have an external_id from the mock and
    // status flipped past queued.
    let fetched = runs.get(run.id).await.expect("get");
    assert!(
        fetched.external_id.is_some(),
        "expected external_id to be patched in after dispatch"
    );
    assert_eq!(fetched.integration.as_deref(), Some("mock"));
    assert_eq!(fetched.status, RunStatus::Starting.as_str());
}

#[tokio::test]
async fn start_run_rejects_empty_prompt() {
    let (service, _runs, _tools) = build_service();
    let err = service
        .start_run("   ".into(), "hermes".into(), None)
        .await
        .unwrap_err();
    match err {
        agent_proto::AgentServiceError::InvalidInput(msg) => assert!(msg.contains("prompt")),
        other => panic!("expected InvalidInput, got {other:?}"),
    }
}

#[tokio::test]
async fn start_run_rejects_unknown_plugin() {
    // Build a service with NO plugins registered.
    let doc = CrdtDoc::ephemeral();
    let runs = AgentRunRepoLoro::new(&doc);
    let tools = ToolCallRepoLoro::new(&doc);
    let registry = IntegrationRegistry::new();
    let service = AgentServiceImpl::new(runs, tools, registry);

    let err = service
        .start_run("hi".into(), "anything".into(), None)
        .await
        .unwrap_err();
    match err {
        agent_proto::AgentServiceError::InvalidInput(msg) => {
            assert!(msg.contains("hermes") || msg.contains("plugin"))
        }
        other => panic!("expected InvalidInput, got {other:?}"),
    }
}

#[tokio::test]
async fn cancel_transitions_to_cancelled() {
    let (service, runs, _tools) = build_service();
    let run = service
        .start_run("Run something".into(), "hermes".into(), None)
        .await
        .expect("start_run");

    service.cancel(run.id).await.expect("cancel");
    let after = runs.get(run.id).await.expect("get");
    assert_eq!(after.status, RunStatus::Cancelled.as_str());
    assert!(
        after.completed_at.is_some(),
        "cancel should stamp completed_at"
    );
}

#[tokio::test]
async fn cancel_terminal_run_is_noop() {
    let (service, runs, _tools) = build_service();
    let run = service
        .start_run("Done early".into(), "hermes".into(), None)
        .await
        .expect("start_run");
    // Force terminal state directly via repo.
    let patch = agent_proto::AgentRunUpdate {
        status: Some(RunStatus::Completed.as_str().into()),
        completed_at: Some(Some(Utc::now())),
        ..empty_run_update()
    };
    runs.update(run.id, patch).await.expect("force complete");
    // Idempotent cancel call should succeed without flipping status.
    service.cancel(run.id).await.expect("cancel terminal");
    let after = runs.get(run.id).await.expect("get");
    assert_eq!(after.status, RunStatus::Completed.as_str());
}

#[tokio::test]
async fn approve_tool_flips_pending_to_approved() {
    let (service, _runs, tool_calls) = build_service();
    let run_id = Uuid::new_v4();
    let tc = tool_calls
        .create(ToolCallCreate {
            run_id,
            seq: 1,
            name: "Bash".into(),
            args_json: "{\"command\":\"ls\"}".into(),
            result_json: None,
            status: "pending".into(),
            started_at: None,
            completed_at: None,
            approval_required: true,
        })
        .await
        .expect("create tool call");

    service.approve_tool(tc.id).await.expect("approve");
    let after = tool_calls.get(tc.id).await.expect("get");
    assert_eq!(after.status, "approved");
}

#[tokio::test]
async fn approve_non_pending_tool_errors() {
    let (service, _runs, tool_calls) = build_service();
    let tc = tool_calls
        .create(ToolCallCreate {
            run_id: Uuid::new_v4(),
            seq: 1,
            name: "Bash".into(),
            args_json: "{}".into(),
            result_json: None,
            status: "running".into(),
            started_at: None,
            completed_at: None,
            approval_required: false,
        })
        .await
        .expect("create");
    let err = service.approve_tool(tc.id).await.unwrap_err();
    match err {
        agent_proto::AgentServiceError::InvalidInput(_) => {}
        other => panic!("expected InvalidInput, got {other:?}"),
    }
}

#[tokio::test]
async fn deny_tool_records_reason() {
    let (service, _runs, tool_calls) = build_service();
    let tc = tool_calls
        .create(ToolCallCreate {
            run_id: Uuid::new_v4(),
            seq: 1,
            name: "Bash".into(),
            args_json: "{\"command\":\"rm -rf /\"}".into(),
            result_json: None,
            status: "pending".into(),
            started_at: None,
            completed_at: None,
            approval_required: true,
        })
        .await
        .expect("create");
    service
        .deny_tool(tc.id, Some("nope".into()))
        .await
        .expect("deny");
    let after = tool_calls.get(tc.id).await.expect("get");
    assert_eq!(after.status, "denied");
    let result = after.result_json.expect("result_json");
    assert!(result.contains("\"denied\":true"));
    assert!(result.contains("\"reason\":\"nope\""));
}

#[tokio::test]
async fn deny_tool_without_reason_is_minimal_json() {
    let (service, _runs, tool_calls) = build_service();
    let tc = tool_calls
        .create(ToolCallCreate {
            run_id: Uuid::new_v4(),
            seq: 1,
            name: "Edit".into(),
            args_json: "{}".into(),
            result_json: None,
            status: "pending".into(),
            started_at: None,
            completed_at: None,
            approval_required: true,
        })
        .await
        .expect("create");
    service.deny_tool(tc.id, None).await.expect("deny");
    let after = tool_calls.get(tc.id).await.expect("get");
    assert_eq!(after.status, "denied");
    assert_eq!(after.result_json.as_deref(), Some("{\"denied\":true}"));
}

fn empty_run_update() -> agent_proto::AgentRunUpdate {
    agent_proto::AgentRunUpdate {
        name: None,
        kind: None,
        prompt: None,
        status: None,
        task_id: None,
        started_at: None,
        completed_at: None,
        result: None,
        error_message: None,
        tokens_used: None,
        cost_cents: None,
        tags: None,
        integration: None,
        external_id: None,
        external_url: None,
        log_cursor: None,
        parent_run_id: None,
        worktree_path: None,
        git_repo_connection_id: None,
        spawned_from_message_id: None,
        input_tokens: None,
        output_tokens: None,
        cache_read_tokens: None,
        cache_creation_tokens: None,
        cost_cents_estimate: None,
        tool_call_count: None,
        assistant_message_count: None,
        max_tokens: None,
        max_tool_calls: None,
        max_wall_seconds: None,
    }
}
