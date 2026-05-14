//! `AgentServiceImpl` — concrete server-side implementation of
//! `agent_proto::AgentService`.
//!
//! Wires the four MVP RPCs (`start_run`, `cancel`, `approve_tool`,
//! `deny_tool`) through the existing `IntegrationRegistry` so any
//! plugin implementing `AgentIntegration` (Hermes, mock, future
//! backends) handles the heavy lifting. Run state is persisted via
//! the Loro-backed `AgentRunRepoLoro` + `ToolCallRepoLoro`.
//!
//! Provider selection is by `kind`: the service maps the `kind`
//! string to a plugin name via the registry's `get(kind)` lookup.
//! Today every kind that maps to a Hermes-backed model uses the
//! `"hermes"` plugin; the kind itself just records which model
//! family the user picked. Hermes handles claude-code / codex /
//! gemini / etc. internally — Task doesn't ship per-backend
//! adapters.

use std::sync::Arc;

use agent_crdt::{AgentRunRepoLoro, ToolCallRepoLoro};
use agent_proto::integration::{
    DispatchRequest, IntegrationRegistry, TaskSummary, WorkspaceHint, WorkspaceKind,
};
use agent_proto::{
    AgentRun, AgentRunCreate, AgentRunRepo, AgentRunUpdate, AgentService, AgentServiceError,
    RunStatus, ToolCallRepo, ToolCallUpdate, validate_status_transition,
};
use chrono::Utc;
use uuid::Uuid;

/// Hooks the service uses to figure out which integration to dispatch
/// to. Decouples kind-to-plugin routing from the registry itself so
/// tests can swap the policy.
pub trait PluginRouter: Send + Sync {
    /// Map an agent `kind` (`"claude-code"`, `"codex"`, …) to a plugin
    /// name registered in the `IntegrationRegistry` (typically
    /// `"hermes"` for everything in MVP, since Hermes routes models
    /// internally).
    fn plugin_for_kind(&self, kind: &str) -> &'static str;
}

/// Default router: every kind maps to the `"hermes"` plugin. Override
/// when more plugins land.
pub struct HermesOnlyRouter;

impl PluginRouter for HermesOnlyRouter {
    fn plugin_for_kind(&self, _kind: &str) -> &'static str {
        "hermes"
    }
}

#[derive(Clone)]
pub struct AgentServiceImpl {
    runs: AgentRunRepoLoro,
    tool_calls: ToolCallRepoLoro,
    registry: IntegrationRegistry,
    router: Arc<dyn PluginRouter>,
}

impl AgentServiceImpl {
    pub fn new(
        runs: AgentRunRepoLoro,
        tool_calls: ToolCallRepoLoro,
        registry: IntegrationRegistry,
    ) -> Self {
        Self {
            runs,
            tool_calls,
            registry,
            router: Arc::new(HermesOnlyRouter),
        }
    }

    pub fn with_router(mut self, router: Arc<dyn PluginRouter>) -> Self {
        self.router = router;
        self
    }

    fn map_int_err(e: agent_proto::integration::IntegrationError) -> AgentServiceError {
        match e {
            agent_proto::integration::IntegrationError::NotFound => AgentServiceError::NotFound,
            other => AgentServiceError::Internal(other.to_string()),
        }
    }
}

impl AgentService for AgentServiceImpl {
    async fn start_run(
        &self,
        prompt: String,
        kind: String,
        worktree_path: Option<String>,
    ) -> Result<AgentRun, AgentServiceError> {
        if prompt.trim().is_empty() {
            return Err(AgentServiceError::InvalidInput(
                "prompt must not be empty".into(),
            ));
        }
        if kind.trim().is_empty() {
            return Err(AgentServiceError::InvalidInput(
                "kind must not be empty".into(),
            ));
        }

        let plugin_name = self.router.plugin_for_kind(&kind);
        let plugin = self.registry.get(plugin_name).ok_or_else(|| {
            AgentServiceError::InvalidInput(format!(
                "no integration registered for plugin `{plugin_name}` (kind=`{kind}`)"
            ))
        })?;

        // Persist the run with status=queued first so the UI sees it
        // before the dispatch network call completes.
        let create = AgentRunCreate {
            name: prompt
                .lines()
                .next()
                .unwrap_or("agent run")
                .chars()
                .take(80)
                .collect(),
            kind: kind.clone(),
            prompt: prompt.clone(),
            status: RunStatus::Queued.as_str().into(),
            task_id: None,
            started_at: None,
            completed_at: None,
            result: None,
            error_message: None,
            tokens_used: None,
            cost_cents: None,
            tags: Vec::new(),
            integration: Some(plugin_name.into()),
            external_id: None,
            external_url: None,
            log_cursor: None,
            parent_run_id: None,
            worktree_path: worktree_path.clone(),
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
        };
        let run = self
            .runs
            .create(create)
            .await
            .map_err(|e| AgentServiceError::Internal(e.to_string()))?;

        // Build a minimal dispatch request — workspace + task summary
        // are placeholders for the MVP; richer wiring comes when
        // task/project context flows into start_run.
        let req = DispatchRequest {
            idempotency_key: run.id,
            agent_kind: kind,
            prompt,
            task: TaskSummary {
                id: run.id,
                project_id: None,
                title: run.name.clone(),
                description: None,
                priority: None,
                tags: Vec::new(),
                branch_name: None,
            },
            workspace_hint: Some(WorkspaceHint {
                kind: WorkspaceKind::Worktree,
                path: worktree_path,
                git_repo_owner: None,
                git_repo_name: None,
                git_base_branch: None,
            }),
        };

        let ack = plugin.dispatch(req).await.map_err(Self::map_int_err)?;

        // Patch the run with external_id + external_url + transition
        // to starting; the integration's event loop will move it to
        // running once the upstream side actually starts.
        let _next_status = RunStatus::Starting;
        let _ = validate_status_transition(RunStatus::Queued, RunStatus::Starting)?;
        let patch = AgentRunUpdate {
            name: None,
            kind: None,
            prompt: None,
            status: Some(RunStatus::Starting.as_str().into()),
            task_id: None,
            started_at: None,
            completed_at: None,
            result: None,
            error_message: None,
            tokens_used: None,
            cost_cents: None,
            tags: None,
            integration: None,
            external_id: Some(Some(ack.external_id)),
            external_url: Some(ack.external_url),
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
        };
        let run = self
            .runs
            .update(run.id, patch)
            .await
            .map_err(|e| AgentServiceError::Internal(e.to_string()))?;

        Ok(run)
    }

    async fn cancel(&self, run_id: Uuid) -> Result<(), AgentServiceError> {
        let run = self
            .runs
            .get(run_id)
            .await
            .map_err(|_| AgentServiceError::NotFound)?;
        let cur = RunStatus::parse(&run.status).unwrap_or(RunStatus::Queued);
        if cur.is_terminal() {
            // Already terminal — idempotent no-op per spec.
            return Ok(());
        }
        validate_status_transition(cur, RunStatus::Cancelled)?;

        // Ask the integration to cancel upstream (best-effort; if the
        // plugin isn't reachable we still flip status locally so the
        // user's intent doesn't get lost).
        if let (Some(plugin_name), Some(ext)) =
            (run.integration.as_deref(), run.external_id.as_deref())
        {
            if let Some(plugin) = self.registry.get(plugin_name) {
                let _ = plugin.cancel(ext).await; // best-effort
            }
        }

        let patch = AgentRunUpdate {
            status: Some(RunStatus::Cancelled.as_str().into()),
            completed_at: Some(Some(Utc::now())),
            ..agent_run_update_empty()
        };
        self.runs
            .update(run_id, patch)
            .await
            .map_err(|e| AgentServiceError::Internal(e.to_string()))?;
        Ok(())
    }

    async fn approve_tool(&self, tool_call_id: Uuid) -> Result<(), AgentServiceError> {
        let tc = self
            .tool_calls
            .get(tool_call_id)
            .await
            .map_err(|_| AgentServiceError::NotFound)?;
        if tc.status != "pending" {
            return Err(AgentServiceError::InvalidInput(format!(
                "tool call status is `{}`; only `pending` calls can be approved",
                tc.status
            )));
        }
        let patch = ToolCallUpdate {
            status: Some("approved".into()),
            ..tool_call_update_empty()
        };
        self.tool_calls
            .update(tool_call_id, patch)
            .await
            .map_err(|e| AgentServiceError::Internal(e.to_string()))?;
        Ok(())
    }

    async fn deny_tool(
        &self,
        tool_call_id: Uuid,
        reason: Option<String>,
    ) -> Result<(), AgentServiceError> {
        let tc = self
            .tool_calls
            .get(tool_call_id)
            .await
            .map_err(|_| AgentServiceError::NotFound)?;
        if tc.status != "pending" {
            return Err(AgentServiceError::InvalidInput(format!(
                "tool call status is `{}`; only `pending` calls can be denied",
                tc.status
            )));
        }
        let result_json = match reason {
            Some(r) => {
                // Minimal JSON-string escape — keeps the proto crate
                // free of a serde_json dep. The reason is short
                // free-text from the user so the slow path is fine.
                let esc: String = r.chars().flat_map(escape_json_char).collect();
                format!("{{\"denied\":true,\"reason\":\"{esc}\"}}")
            }
            None => "{\"denied\":true}".into(),
        };
        let patch = ToolCallUpdate {
            status: Some("denied".into()),
            result_json: Some(Some(result_json)),
            ..tool_call_update_empty()
        };
        self.tool_calls
            .update(tool_call_id, patch)
            .await
            .map_err(|e| AgentServiceError::Internal(e.to_string()))?;
        Ok(())
    }
}

fn agent_run_update_empty() -> AgentRunUpdate {
    AgentRunUpdate {
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

fn escape_json_char(c: char) -> Vec<char> {
    match c {
        '"' => vec!['\\', '"'],
        '\\' => vec!['\\', '\\'],
        '\n' => vec!['\\', 'n'],
        '\r' => vec!['\\', 'r'],
        '\t' => vec!['\\', 't'],
        c if (c as u32) < 0x20 => format!("\\u{:04x}", c as u32).chars().collect(),
        c => vec![c],
    }
}

fn tool_call_update_empty() -> ToolCallUpdate {
    ToolCallUpdate {
        run_id: None,
        seq: None,
        name: None,
        args_json: None,
        result_json: None,
        status: None,
        started_at: None,
        completed_at: None,
        approval_required: None,
    }
}
