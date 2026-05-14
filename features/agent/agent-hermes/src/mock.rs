//! Deterministic, in-memory `AgentIntegration` impl.
//!
//! For demos and integration tests — **not** for production. Lets the
//! UI exercise the full dispatch → progress → completion lifecycle
//! without a live Hermes dashboard.
//!
//! State machine (T = dispatch instant):
//!
//! - T+0s   : status = Queued                (event: "task created")
//! - T+1s   : status = Running, started_at = now (event: "worker started")
//! - T+2s   : log    "Cloning workspace..."
//! - T+3s   : log    "Running tool: shell"
//! - T+4s   : log    "Tool result: ok"
//! - T+5s   : log    "Completing task"
//! - T+6s   : status = Succeeded, completed_at = now,
//!           result = "Done. Created branch and opened PR."
//!           fake branch_name + pr_urls + commit_refs

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use agent_proto::AgentRun;
use agent_proto::integration::{
    AgentIntegration, AgentLogLineDto, AgentRunStatus, CommitRefDto, DispatchAck, DispatchRequest,
    EventSink, IntegrationCaps, IntegrationError, RunSnapshot, ShutdownSignal,
};
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use tokio::sync::{Mutex, broadcast};
use uuid::Uuid;

#[derive(Clone, Debug)]
struct MockRun {
    run_id: Uuid,
    status: AgentRunStatus,
    started_at: Option<DateTime<Utc>>,
    completed_at: Option<DateTime<Utc>>,
    logs: Vec<AgentLogLineDto>,
    result_summary: Option<String>,
    branch_name: Option<String>,
    pr_urls: Vec<String>,
    commit_refs: Vec<CommitRefDto>,
}

#[derive(Clone, Debug)]
enum MockEvent {
    StatusChange { run_id: Uuid, snapshot: RunSnapshot },
    Log { run_id: Uuid, line: AgentLogLineDto },
}

struct MockState {
    runs: HashMap<String, MockRun>,
    counter: u64,
    events: broadcast::Sender<MockEvent>,
}

/// Demo-only `AgentIntegration` that progresses every dispatched run
/// through a scripted timeline. Capabilities advertise
/// `can_stream_logs = true` so the UI's live-log panel renders even
/// in demo mode.
pub struct MockIntegration {
    inner: Arc<Mutex<MockState>>,
}

impl Default for MockIntegration {
    fn default() -> Self {
        Self::new()
    }
}

impl MockIntegration {
    pub fn new() -> Self {
        let (tx, _) = broadcast::channel(256);
        Self {
            inner: Arc::new(Mutex::new(MockState {
                runs: HashMap::new(),
                counter: 0,
                events: tx,
            })),
        }
    }
}

#[async_trait]
impl AgentIntegration for MockIntegration {
    fn name(&self) -> &'static str {
        "mock"
    }

    fn capabilities(&self) -> IntegrationCaps {
        IntegrationCaps {
            can_cancel: true,
            can_stream_logs: true,
            can_resume: false,
            two_way_sync: false,
        }
    }

    async fn dispatch(&self, req: DispatchRequest) -> Result<DispatchAck, IntegrationError> {
        let (external_id, events_tx) = {
            let mut state = self.inner.lock().await;
            state.counter += 1;
            let external_id = format!("mock-{:04}", state.counter);
            let run = MockRun {
                run_id: req.idempotency_key,
                status: AgentRunStatus::Queued,
                started_at: None,
                completed_at: None,
                logs: Vec::new(),
                result_summary: None,
                branch_name: None,
                pr_urls: Vec::new(),
                commit_refs: Vec::new(),
            };
            state.runs.insert(external_id.clone(), run);
            (external_id, state.events.clone())
        };

        let inner = self.inner.clone();
        let run_id = req.idempotency_key;
        let ext = external_id.clone();
        tokio::spawn(async move {
            scripted_timeline(inner, events_tx, run_id, ext).await;
        });

        let now = Utc::now();
        let run = AgentRun {
            id: req.idempotency_key,
            name: req.task.title.clone(),
            kind: req.agent_kind.clone(),
            prompt: req.prompt.clone(),
            status: AgentRunStatus::Queued.as_str().to_string(),
            task_id: Some(req.task.id),
            started_at: None,
            completed_at: None,
            result: None,
            error_message: None,
            tokens_used: None,
            cost_cents: None,
            tags: req.task.tags.clone(),
            integration: Some("mock".to_string()),
            external_id: Some(external_id.clone()),
            external_url: Some(format!("mock://runs/{external_id}")),
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
            tool_call_count: 0,
            assistant_message_count: 0,
            max_tokens: None,
            max_tool_calls: None,
            max_wall_seconds: None,
            created_at: now,
            updated_at: now,
        };
        Ok(DispatchAck {
            external_id: external_id.clone(),
            external_url: Some(format!("mock://runs/{external_id}")),
            run,
        })
    }

    async fn cancel(&self, external_id: &str) -> Result<(), IntegrationError> {
        let mut state = self.inner.lock().await;
        let run = state
            .runs
            .get_mut(external_id)
            .ok_or(IntegrationError::NotFound)?;
        run.status = AgentRunStatus::Cancelled;
        run.completed_at = Some(Utc::now());
        let snapshot = snapshot_of(run);
        let run_id = run.run_id;
        let _ = state
            .events
            .send(MockEvent::StatusChange { run_id, snapshot });
        Ok(())
    }

    async fn fetch_run(&self, external_id: &str) -> Result<RunSnapshot, IntegrationError> {
        let state = self.inner.lock().await;
        let run = state
            .runs
            .get(external_id)
            .ok_or(IntegrationError::NotFound)?;
        Ok(snapshot_of(run))
    }

    async fn fetch_logs(
        &self,
        external_id: &str,
        after: Option<DateTime<Utc>>,
    ) -> Result<Vec<AgentLogLineDto>, IntegrationError> {
        let state = self.inner.lock().await;
        let run = state
            .runs
            .get(external_id)
            .ok_or(IntegrationError::NotFound)?;
        Ok(run
            .logs
            .iter()
            .filter(|l| after.map_or(true, |a| l.at > a))
            .cloned()
            .collect())
    }

    async fn run_event_loop(
        &self,
        sink: EventSink,
        shutdown: ShutdownSignal,
    ) -> Result<(), IntegrationError> {
        let mut rx = {
            let state = self.inner.lock().await;
            state.events.subscribe()
        };
        loop {
            if shutdown.is_set() {
                return Ok(());
            }
            tokio::select! {
                _ = tokio::time::sleep(Duration::from_millis(250)) => {}
                msg = rx.recv() => {
                    match msg {
                        Ok(MockEvent::StatusChange { run_id, snapshot }) => {
                            if let Err(err) = sink.inner.patch_run(run_id, snapshot).await {
                                tracing::warn!(?err, "mock patch_run failed");
                            }
                        }
                        Ok(MockEvent::Log { run_id, line }) => {
                            if let Err(err) = sink.inner.append_log(run_id, line).await {
                                tracing::warn!(?err, "mock append_log failed");
                            }
                        }
                        Err(broadcast::error::RecvError::Lagged(_)) => {
                            tracing::warn!("mock event channel lagged; some events dropped");
                        }
                        Err(broadcast::error::RecvError::Closed) => return Ok(()),
                    }
                }
            }
        }
    }
}

fn snapshot_of(run: &MockRun) -> RunSnapshot {
    RunSnapshot {
        status: run.status,
        started_at: run.started_at,
        completed_at: run.completed_at,
        result_summary: run.result_summary.clone(),
        error: None,
        tokens_used: Some(1234),
        cost_cents: Some(7),
        branch_name: run.branch_name.clone(),
        pr_urls: run.pr_urls.clone(),
        commit_refs: run.commit_refs.clone(),
    }
}

async fn scripted_timeline(
    state: Arc<Mutex<MockState>>,
    tx: broadcast::Sender<MockEvent>,
    run_id: Uuid,
    external_id: String,
) {
    if let Some(snap) = with_run(&state, &external_id, snapshot_of).await {
        let _ = tx.send(MockEvent::StatusChange {
            run_id,
            snapshot: snap,
        });
    }
    log_line(
        &state,
        &tx,
        &external_id,
        run_id,
        "info",
        "mock",
        "task created",
    )
    .await;

    tokio::time::sleep(Duration::from_secs(1)).await;
    if cancelled(&state, &external_id).await {
        return;
    }
    transition(&state, &external_id, |r| {
        r.status = AgentRunStatus::Running;
        r.started_at = Some(Utc::now());
    })
    .await;
    if let Some(snap) = with_run(&state, &external_id, snapshot_of).await {
        let _ = tx.send(MockEvent::StatusChange {
            run_id,
            snapshot: snap,
        });
    }
    log_line(
        &state,
        &tx,
        &external_id,
        run_id,
        "info",
        "mock.runner",
        "worker started",
    )
    .await;

    for (delay, level, text) in [
        (1u64, "stdout", "Cloning workspace..."),
        (1, "tool", "Running tool: shell"),
        (1, "stdout", "Tool result: ok"),
        (1, "info", "Completing task"),
    ] {
        tokio::time::sleep(Duration::from_secs(delay)).await;
        if cancelled(&state, &external_id).await {
            return;
        }
        log_line(
            &state,
            &tx,
            &external_id,
            run_id,
            level,
            "mock.runner",
            text,
        )
        .await;
    }

    tokio::time::sleep(Duration::from_secs(1)).await;
    if cancelled(&state, &external_id).await {
        return;
    }
    transition(&state, &external_id, |r| {
        r.status = AgentRunStatus::Succeeded;
        r.completed_at = Some(Utc::now());
        r.result_summary = Some("Done. Created branch and opened PR.".to_string());
        r.branch_name = Some("mock/feature".to_string());
        r.pr_urls = vec!["https://example.invalid/pr/42".to_string()];
        r.commit_refs = vec![CommitRefDto {
            sha: "deadbeefcafefeed".to_string(),
            message: "mock: scripted commit".to_string(),
            url: Some("https://example.invalid/commit/deadbeef".to_string()),
            authored_at: Utc::now(),
        }];
    })
    .await;
    if let Some(snap) = with_run(&state, &external_id, snapshot_of).await {
        let _ = tx.send(MockEvent::StatusChange {
            run_id,
            snapshot: snap,
        });
    }
}

async fn cancelled(state: &Arc<Mutex<MockState>>, external_id: &str) -> bool {
    let st = state.lock().await;
    st.runs
        .get(external_id)
        .map_or(true, |r| matches!(r.status, AgentRunStatus::Cancelled))
}

async fn transition<F: FnOnce(&mut MockRun)>(
    state: &Arc<Mutex<MockState>>,
    external_id: &str,
    f: F,
) {
    let mut st = state.lock().await;
    if let Some(r) = st.runs.get_mut(external_id) {
        f(r);
    }
}

async fn with_run<R, F: FnOnce(&MockRun) -> R>(
    state: &Arc<Mutex<MockState>>,
    external_id: &str,
    f: F,
) -> Option<R> {
    let st = state.lock().await;
    st.runs.get(external_id).map(f)
}

async fn log_line(
    state: &Arc<Mutex<MockState>>,
    tx: &broadcast::Sender<MockEvent>,
    external_id: &str,
    run_id: Uuid,
    level: &str,
    source: &str,
    text: &str,
) {
    let line = AgentLogLineDto {
        at: Utc::now(),
        level: level.to_string(),
        source: source.to_string(),
        text: text.to_string(),
        external_event_id: None,
    };
    {
        let mut st = state.lock().await;
        if let Some(r) = st.runs.get_mut(external_id) {
            r.logs.push(line.clone());
        }
    }
    let _ = tx.send(MockEvent::Log { run_id, line });
}
