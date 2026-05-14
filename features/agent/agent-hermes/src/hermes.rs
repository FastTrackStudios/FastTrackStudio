//! `AgentIntegration` impl for the Hermes dashboard.
//!
//! Talks REST for dispatch / cancel / fetch and a WebSocket frame
//! loop for live event streaming.

use std::time::Duration;

use agent_proto::AgentRun;
use agent_proto::integration::{
    AgentIntegration, AgentLogLineDto, AgentRunStatus, CommitRefDto, DispatchAck, DispatchRequest,
    EventSink, IntegrationCaps, IntegrationError, RunSnapshot, ShutdownSignal,
};
use async_trait::async_trait;
use chrono::{DateTime, Utc};
use futures_util::{SinkExt, StreamExt};
use tokio_tungstenite::tungstenite::Message;
use tokio_tungstenite::tungstenite::client::IntoClientRequest;
use tokio_tungstenite::tungstenite::http::HeaderValue;
use uuid::Uuid;

use crate::client::{
    HermesClient, HermesCreateTaskRequest, HermesTask, HermesTaskEvent, HermesTaskPatch,
    HermesTaskRun,
};
use crate::config::HermesConfig;

/// Hermes integration plugin. Construct via [`HermesIntegration::new`].
pub struct HermesIntegration {
    config: HermesConfig,
    client: HermesClient,
}

impl HermesIntegration {
    pub fn new(config: HermesConfig) -> Self {
        let client = HermesClient::new(config.clone());
        Self { config, client }
    }

    /// Format the user-facing "Open in Hermes" URL for a task.
    fn task_url(&self, id: &str) -> Option<String> {
        let mut url = self.config.base_url.clone();
        url.set_path(&format!("/kanban/tasks/{id}"));
        Some(url.to_string())
    }
}

#[async_trait]
impl AgentIntegration for HermesIntegration {
    fn name(&self) -> &'static str {
        "hermes"
    }

    fn capabilities(&self) -> IntegrationCaps {
        IntegrationCaps {
            can_cancel: true,
            can_stream_logs: true,
            can_resume: true,
            two_way_sync: true,
        }
    }

    async fn dispatch(&self, req: DispatchRequest) -> Result<DispatchAck, IntegrationError> {
        // Strip optional "hermes-profile:" prefix the caller may use to
        // disambiguate plugin from upstream profile.
        let assignee = req
            .agent_kind
            .strip_prefix("hermes-profile:")
            .map(|s| s.to_string())
            .or_else(|| Some(req.agent_kind.clone()))
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| self.config.default_profile.clone());

        let body = {
            let tags_joined = req.task.tags.join(", ");
            let description = req.task.description.clone().unwrap_or_default();
            format!(
                "{}\n\n# Task\n{}\n\n# Acceptance\n{}",
                req.prompt, description, tags_joined
            )
        };

        let create_req = HermesCreateTaskRequest {
            title: req.task.title.clone(),
            body: Some(body),
            assignee: Some(assignee),
            priority: req.task.priority.clone(),
            idempotency_key: Some(req.idempotency_key.to_string()),
            skills: if req.task.tags.is_empty() {
                None
            } else {
                Some(req.task.tags.clone())
            },
            workspace_kind: req
                .workspace_hint
                .as_ref()
                .map(|h| workspace_kind_str(h.kind).to_string()),
            workspace_path: req.workspace_hint.as_ref().and_then(|h| h.path.clone()),
            max_runtime_seconds: None,
        };

        let task = self.client.create_task(create_req).await?;
        // Nudge the dispatcher — Hermes' default dispatch loop ticks on
        // a timer, but POST /dispatch wakes it immediately.
        if let Err(err) = self
            .client
            .dispatch_board(self.config.default_board.as_deref())
            .await
        {
            tracing::warn!(?err, "POST /dispatch failed after create_task (non-fatal)");
        }

        let external_url = self.task_url(&task.id);
        let now = Utc::now();
        let run = AgentRun {
            id: req.idempotency_key,
            name: task.title.clone(),
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
            integration: Some("hermes".to_string()),
            external_id: Some(task.id.clone()),
            external_url: external_url.clone(),
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
            external_id: task.id,
            external_url,
            run,
        })
    }

    async fn cancel(&self, external_id: &str) -> Result<(), IntegrationError> {
        let patch = HermesTaskPatch {
            status: Some("blocked".to_string()),
            block_reason: Some("cancelled by task-architect".to_string()),
            ..Default::default()
        };
        self.client.patch_task(external_id, patch).await?;
        // Best-effort: reclaim releases worker grip so the runner exits.
        if let Err(err) = self.client.reclaim_task(external_id).await {
            tracing::warn!(%external_id, ?err, "reclaim after cancel failed");
        }
        Ok(())
    }

    async fn fetch_run(&self, external_id: &str) -> Result<RunSnapshot, IntegrationError> {
        let task = self.client.get_task(external_id).await?;
        Ok(snapshot_from_task(&task))
    }

    async fn fetch_logs(
        &self,
        external_id: &str,
        after: Option<DateTime<Utc>>,
    ) -> Result<Vec<AgentLogLineDto>, IntegrationError> {
        let task = self.client.get_task(external_id).await?;
        let mut lines: Vec<AgentLogLineDto> = Vec::new();

        if let Some(events) = task.events.as_ref() {
            for ev in events {
                if !is_log_event(&ev.kind) {
                    continue;
                }
                let at = parse_iso(&ev.created_at).unwrap_or_else(Utc::now);
                if let Some(cutoff) = after {
                    if at <= cutoff {
                        continue;
                    }
                }
                let (level, source, text) = event_to_log_fields(ev);
                lines.push(AgentLogLineDto {
                    at,
                    level,
                    source,
                    text,
                    external_event_id: Some(ev.id),
                });
            }
        }

        // Also pull raw stdout in case the runner is configured to skip
        // structured events. Each newline → its own log line.
        match self.client.get_task_log(external_id, 1_048_576).await {
            Ok(raw) => {
                let now = Utc::now();
                for chunk in raw.split('\n') {
                    if chunk.is_empty() {
                        continue;
                    }
                    lines.push(AgentLogLineDto {
                        at: now,
                        level: "stdout".to_string(),
                        source: "hermes.runner".to_string(),
                        text: chunk.to_string(),
                        external_event_id: None,
                    });
                }
            }
            Err(err) => {
                tracing::debug!(?err, %external_id, "raw log fetch failed (non-fatal)");
            }
        }

        lines.sort_by_key(|l| l.at);
        Ok(lines)
    }

    async fn run_event_loop(
        &self,
        sink: EventSink,
        shutdown: ShutdownSignal,
    ) -> Result<(), IntegrationError> {
        let mut backoff_secs: u64 = 1;
        let mut cursor: Option<i64> = None; // in-memory only for v1

        while !shutdown.is_set() {
            match self.connect_and_pump(&sink, cursor).await {
                Ok(new_cursor) => {
                    cursor = new_cursor.or(cursor);
                    backoff_secs = 1;
                }
                Err(IntegrationError::Auth(msg)) => {
                    tracing::error!(%msg, "hermes WS auth failed; bailing event loop");
                    return Err(IntegrationError::Auth(msg));
                }
                Err(err) => {
                    tracing::warn!(?err, backoff_secs, "hermes WS disconnected; will retry");
                }
            }
            // Backoff with 30s cap; honour shutdown between ticks.
            for _ in 0..backoff_secs {
                if shutdown.is_set() {
                    return Ok(());
                }
                tokio::time::sleep(Duration::from_secs(1)).await;
            }
            backoff_secs = (backoff_secs * 2).min(30);
        }
        Ok(())
    }
}

impl HermesIntegration {
    /// Open a WS connection, pump frames into the sink until the
    /// stream ends or errs. Returns the latest cursor we saw so the
    /// caller can resume.
    async fn connect_and_pump(
        &self,
        sink: &EventSink,
        since: Option<i64>,
    ) -> Result<Option<i64>, IntegrationError> {
        let ws_url = self.config.ws_url(since);
        // We prefer the Authorization header over the `?token=` query
        // param — both are accepted by Hermes but the header survives
        // reverse-proxy access-log redaction.
        let mut req = ws_url
            .as_str()
            .into_client_request()
            .map_err(|e| IntegrationError::Network(e.to_string()))?;
        req.headers_mut().insert(
            "Authorization",
            HeaderValue::from_str(&format!("Bearer {}", self.config.session_token))
                .map_err(|e| IntegrationError::Network(e.to_string()))?,
        );
        req.headers_mut().insert(
            "Cookie",
            HeaderValue::from_str(&format!("session_token={}", self.config.session_token))
                .map_err(|e| IntegrationError::Network(e.to_string()))?,
        );

        let (mut socket, response) = tokio_tungstenite::connect_async(req)
            .await
            .map_err(map_ws_err)?;
        tracing::debug!(status = %response.status(), "hermes WS connected");

        let mut latest_cursor: Option<i64> = since;

        while let Some(msg) = socket.next().await {
            let msg = msg.map_err(|e| IntegrationError::Network(e.to_string()))?;
            match msg {
                Message::Text(text) => {
                    let frame: WsFrame = match serde_json::from_str(&text) {
                        Ok(v) => v,
                        Err(err) => {
                            tracing::warn!(?err, %text, "hermes WS frame decode failed");
                            continue;
                        }
                    };
                    if let Some(c) = frame.cursor {
                        latest_cursor = Some(c);
                    }
                    for ev in frame.events {
                        if let Err(err) = self.apply_event(sink, &ev).await {
                            tracing::warn!(?err, "apply_event failed");
                        }
                    }
                }
                Message::Ping(p) => {
                    if let Err(err) = socket.send(Message::Pong(p)).await {
                        return Err(IntegrationError::Network(err.to_string()));
                    }
                }
                Message::Close(_) => break,
                _ => {}
            }
        }
        Ok(latest_cursor)
    }

    async fn apply_event(
        &self,
        sink: &EventSink,
        ev: &HermesTaskEvent,
    ) -> Result<(), IntegrationError> {
        // We need our local run_id; the architect side keys runs by
        // `idempotency_key`. The plugin doesn't have access to the
        // task → run_id mapping directly — we rely on the EventSink
        // implementation in task-server to look up by external_id
        // through the run repo.
        //
        // For now: try to parse `idempotency_key` from the event
        // payload (Hermes echoes it back on run.* events). Fall back
        // to fetching the task. Failing both, skip.
        let run_id = match extract_run_id(ev) {
            Some(id) => id,
            None => {
                // Fetch the task; its top-level idempotency_key maps
                // 1:1 with our AgentRun.id.
                match self.client.get_task(&ev.task_id).await {
                    Ok(task) => match task
                        .idempotency_key
                        .as_deref()
                        .and_then(|s| Uuid::parse_str(s).ok())
                    {
                        Some(id) => id,
                        None => return Ok(()),
                    },
                    Err(err) => {
                        tracing::debug!(?err, task_id = %ev.task_id, "no run mapping; skipping");
                        return Ok(());
                    }
                }
            }
        };

        if is_log_event(&ev.kind) {
            let at = parse_iso(&ev.created_at).unwrap_or_else(Utc::now);
            let (level, source, text) = event_to_log_fields(ev);
            sink.inner
                .append_log(
                    run_id,
                    AgentLogLineDto {
                        at,
                        level,
                        source,
                        text,
                        external_event_id: Some(ev.id),
                    },
                )
                .await?;
        }

        // Any structural event triggers a fetch_run + patch so the
        // doc reflects new status/run summary/tokens/etc. Cheap
        // enough for v1; FUTURE: build a snapshot directly from
        // the event payload to avoid the extra round-trip.
        if matches!(
            ev.kind.as_str(),
            "run.started" | "run.completed" | "run.failed" | "run.blocked" | "status.changed"
        ) {
            if let Ok(task) = self.client.get_task(&ev.task_id).await {
                let snapshot = snapshot_from_task(&task);
                let was_completed = ev.kind == "run.completed";
                let task_uuid_from_payload = ev
                    .payload
                    .get("architect_task_id")
                    .and_then(|v| v.as_str())
                    .and_then(|s| Uuid::parse_str(s).ok());
                sink.inner.patch_run(run_id, snapshot).await?;

                if was_completed && task.status == "done" {
                    if let Some(task_uuid) = task_uuid_from_payload {
                        sink.inner
                            .maybe_mirror_task_status(task_uuid, "done")
                            .await?;
                    }
                }
            }
        }
        Ok(())
    }
}

// ── Helpers ───────────────────────────────────────────────────────────

#[derive(serde::Deserialize)]
struct WsFrame {
    #[serde(default)]
    events: Vec<HermesTaskEvent>,
    #[serde(default)]
    cursor: Option<i64>,
}

fn workspace_kind_str(k: agent_proto::integration::WorkspaceKind) -> &'static str {
    use agent_proto::integration::WorkspaceKind as W;
    match k {
        W::Scratch => "scratch",
        W::Worktree => "worktree",
        W::Dir => "dir",
    }
}

fn is_log_event(kind: &str) -> bool {
    matches!(kind, "stdout" | "stderr" | "tool" | "run.log" | "comment")
}

fn event_to_log_fields(ev: &HermesTaskEvent) -> (String, String, String) {
    let level = match ev.kind.as_str() {
        "stderr" => "stderr",
        "stdout" | "run.log" => "stdout",
        "tool" => "tool",
        _ => "info",
    }
    .to_string();
    let source = format!("hermes.{}", ev.kind);
    let text = ev
        .payload
        .get("message")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .or_else(|| {
            ev.payload
                .get("text")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string())
        })
        .unwrap_or_else(|| ev.payload.to_string());
    (level, source, text)
}

fn parse_iso(s: &str) -> Option<DateTime<Utc>> {
    DateTime::parse_from_rfc3339(s)
        .ok()
        .map(|dt| dt.with_timezone(&Utc))
}

fn extract_run_id(ev: &HermesTaskEvent) -> Option<Uuid> {
    ev.payload
        .get("idempotency_key")
        .and_then(|v| v.as_str())
        .and_then(|s| Uuid::parse_str(s).ok())
}

fn snapshot_from_task(task: &HermesTask) -> RunSnapshot {
    let latest_run = task.runs.as_ref().and_then(|r| r.last());
    let outcome = latest_run.and_then(|r| r.outcome.as_deref());
    let status = map_status(&task.status, outcome);

    let started_at = task.started_at.as_deref().and_then(parse_iso).or_else(|| {
        latest_run
            .and_then(|r| r.started_at.as_deref())
            .and_then(parse_iso)
    });
    let completed_at = task
        .completed_at
        .as_deref()
        .and_then(parse_iso)
        .or_else(|| {
            latest_run
                .and_then(|r| r.ended_at.as_deref())
                .and_then(parse_iso)
        });

    let (tokens_used, cost_cents) = task
        .events
        .as_ref()
        .map(|evs| extract_usage(evs))
        .unwrap_or((None, None));

    let (branch_name, pr_urls, commit_refs) = task
        .events
        .as_ref()
        .map(|evs| extract_git(evs))
        .unwrap_or_default();

    RunSnapshot {
        status,
        started_at,
        completed_at,
        result_summary: task
            .result
            .clone()
            .or_else(|| latest_run.and_then(|r| r.summary.clone())),
        error: task
            .last_failure_error
            .clone()
            .or_else(|| latest_run.and_then(|r| r.error.clone())),
        tokens_used,
        cost_cents,
        branch_name,
        pr_urls,
        commit_refs,
    }
}

fn extract_usage(events: &[HermesTaskEvent]) -> (Option<u32>, Option<u32>) {
    let mut tokens = None;
    let mut cost = None;
    for ev in events {
        if ev.kind == "usage" || ev.kind == "run.completed" {
            // tokens
            let meta = ev.payload.get("metadata").unwrap_or(&ev.payload);
            if tokens.is_none() {
                tokens = meta
                    .get("tokens_used")
                    .and_then(|v| v.as_u64())
                    .map(|n| n as u32);
            }
            if cost.is_none() {
                cost = meta
                    .get("cost_cents")
                    .and_then(|v| v.as_u64())
                    .map(|n| n as u32);
            }
        }
    }
    (tokens, cost)
}

fn extract_git(events: &[HermesTaskEvent]) -> (Option<String>, Vec<String>, Vec<CommitRefDto>) {
    let mut branch = None;
    let mut prs: Vec<String> = Vec::new();
    let mut commits: Vec<CommitRefDto> = Vec::new();
    for ev in events {
        if !ev.kind.starts_with("git.") {
            continue;
        }
        match ev.kind.as_str() {
            "git.branch" => {
                if branch.is_none() {
                    branch = ev
                        .payload
                        .get("branch")
                        .and_then(|v| v.as_str())
                        .map(|s| s.to_string());
                }
            }
            "git.pr" | "git.pull_request" => {
                if let Some(url) = ev.payload.get("url").and_then(|v| v.as_str()) {
                    prs.push(url.to_string());
                }
            }
            "git.commit" => {
                let sha = ev
                    .payload
                    .get("sha")
                    .and_then(|v| v.as_str())
                    .unwrap_or_default()
                    .to_string();
                if sha.is_empty() {
                    continue;
                }
                let message = ev
                    .payload
                    .get("message")
                    .and_then(|v| v.as_str())
                    .unwrap_or_default()
                    .to_string();
                let url = ev
                    .payload
                    .get("url")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string());
                let authored_at = ev
                    .payload
                    .get("authored_at")
                    .and_then(|v| v.as_str())
                    .and_then(parse_iso)
                    .unwrap_or_else(Utc::now);
                commits.push(CommitRefDto {
                    sha,
                    message,
                    url,
                    authored_at,
                });
            }
            _ => {}
        }
    }
    (branch, prs, commits)
}

/// Map Hermes' task status + the latest run outcome into our typed
/// `AgentRunStatus`. Kept as a free function so unit tests don't need
/// to construct the whole integration.
pub fn map_status(hermes_status: &str, run_outcome: Option<&str>) -> AgentRunStatus {
    match (hermes_status, run_outcome) {
        ("triage" | "todo" | "ready", _) => AgentRunStatus::Queued,
        ("running", _) => AgentRunStatus::Running,
        ("blocked", _) => AgentRunStatus::Blocked,
        ("archived", _) => AgentRunStatus::Archived,
        ("done", Some("completed")) => AgentRunStatus::Succeeded,
        ("done", Some("crashed" | "gave_up" | "timed_out" | "spawn_failed")) => {
            AgentRunStatus::Failed
        }
        ("done", Some("reclaimed")) => AgentRunStatus::Cancelled,
        _ => AgentRunStatus::Queued,
    }
}

fn map_ws_err(e: tokio_tungstenite::tungstenite::Error) -> IntegrationError {
    use tokio_tungstenite::tungstenite::Error as E;
    match e {
        E::Http(resp) => {
            let status = resp.status();
            let body = resp
                .body()
                .as_ref()
                .map(|b| String::from_utf8_lossy(b).into_owned())
                .unwrap_or_default();
            if status == 401 || status == 403 {
                IntegrationError::Auth(body)
            } else {
                IntegrationError::Upstream {
                    status: status.as_u16(),
                    body,
                }
            }
        }
        other => IntegrationError::Network(other.to_string()),
    }
}

// (Optional internal re-export removed — `HermesTaskRun` is private
// and not part of the public surface yet.)
#[allow(dead_code)]
type _HermesTaskRunReexport = HermesTaskRun;
