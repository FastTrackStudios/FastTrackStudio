//! Typed HTTP wrapper around Hermes' dashboard REST API.
//!
//! Every method sends both `Authorization: Bearer …` and a
//! `Cookie: session_token=…` header. Hermes accepts either; sending
//! both is defensive across dashboard versions per the integration
//! research spec.
//!
//! All non-2xx responses are mapped to
//! [`agent_proto::integration::IntegrationError`] so callers don't
//! need to know about `reqwest`.

use agent_proto::integration::IntegrationError;
use reqwest::{Client, Method, Response, StatusCode, header};
use serde::{Deserialize, Serialize};

use crate::config::HermesConfig;

// ── Wire types ────────────────────────────────────────────────────────

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HermesTask {
    pub id: String,
    pub title: String,
    #[serde(default)]
    pub body: Option<String>,
    #[serde(default)]
    pub assignee: Option<String>,
    /// `triage|todo|ready|running|blocked|done|archived`
    pub status: String,
    #[serde(default)]
    pub priority: Option<String>,
    #[serde(default)]
    pub workspace_kind: Option<String>,
    #[serde(default)]
    pub workspace_path: Option<String>,
    #[serde(default)]
    pub current_run_id: Option<i64>,
    /// ISO-8601 string. Left as `String` so we don't force a chrono
    /// dep on every caller; the integration layer parses when needed.
    #[serde(default)]
    pub started_at: Option<String>,
    #[serde(default)]
    pub completed_at: Option<String>,
    #[serde(default)]
    pub idempotency_key: Option<String>,
    #[serde(default)]
    pub result: Option<String>,
    #[serde(default)]
    pub last_failure_error: Option<String>,
    #[serde(default)]
    pub max_runtime_seconds: Option<i64>,
    #[serde(default)]
    pub skills: Option<serde_json::Value>,
    #[serde(default)]
    pub events: Option<Vec<HermesTaskEvent>>,
    #[serde(default)]
    pub runs: Option<Vec<HermesTaskRun>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HermesTaskEvent {
    pub id: i64,
    pub task_id: String,
    #[serde(default)]
    pub run_id: Option<i64>,
    /// `"run.started"` | `"run.completed"` | `"comment"` | `"stdout"`
    /// | `"stderr"` | `"tool"` | `"run.log"` | `"git.branch"` | …
    pub kind: String,
    #[serde(default)]
    pub payload: serde_json::Value,
    pub created_at: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct HermesTaskRun {
    pub id: i64,
    pub task_id: String,
    #[serde(default)]
    pub profile: Option<String>,
    #[serde(default)]
    pub step_key: Option<String>,
    pub status: String,
    #[serde(default)]
    pub started_at: Option<String>,
    #[serde(default)]
    pub ended_at: Option<String>,
    /// `completed|crashed|gave_up|timed_out|spawn_failed|reclaimed`
    #[serde(default)]
    pub outcome: Option<String>,
    #[serde(default)]
    pub summary: Option<String>,
    #[serde(default)]
    pub error: Option<String>,
    #[serde(default)]
    pub metadata: Option<serde_json::Value>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct HermesCreateTaskRequest {
    pub title: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub assignee: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub priority: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub idempotency_key: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub skills: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace_kind: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace_path: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_runtime_seconds: Option<i64>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct HermesTaskPatch {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub body: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub priority: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub block_reason: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub assignee: Option<String>,
}

// ── Client ────────────────────────────────────────────────────────────

/// Typed REST client targeting one Hermes dashboard. Cheap to clone
/// (the underlying `reqwest::Client` is `Arc<Inner>`).
#[derive(Clone, Debug)]
pub struct HermesClient {
    config: HermesConfig,
    http: Client,
}

impl HermesClient {
    pub fn new(config: HermesConfig) -> Self {
        let http = Client::builder()
            .user_agent(concat!(
                "task-architect/agent-hermes/",
                env!("CARGO_PKG_VERSION")
            ))
            .build()
            .expect("reqwest client builds");
        Self { config, http }
    }

    /// Apply Hermes' dual auth scheme (Bearer header + Cookie) and
    /// return the configured `RequestBuilder`. Keeps every call site
    /// auth-consistent.
    fn auth(&self, method: Method, path: &str) -> reqwest::RequestBuilder {
        let url = self.config.api_url(path);
        self.http
            .request(method, url)
            .header(
                header::AUTHORIZATION,
                format!("Bearer {}", self.config.session_token),
            )
            .header(
                header::COOKIE,
                format!("session_token={}", self.config.session_token),
            )
    }

    pub async fn create_task(
        &self,
        req: HermesCreateTaskRequest,
    ) -> Result<HermesTask, IntegrationError> {
        let resp = self
            .auth(Method::POST, "/api/plugins/kanban/tasks")
            .json(&req)
            .send()
            .await
            .map_err(net)?;
        let resp = check_status(resp).await?;
        resp.json::<HermesTask>().await.map_err(dec)
    }

    pub async fn dispatch_board(&self, board: Option<&str>) -> Result<(), IntegrationError> {
        let mut url = self.config.api_url("/api/plugins/kanban/dispatch");
        if let Some(b) = board {
            url.query_pairs_mut().append_pair("board", b);
        }
        let resp = self
            .http
            .post(url)
            .header(
                header::AUTHORIZATION,
                format!("Bearer {}", self.config.session_token),
            )
            .header(
                header::COOKIE,
                format!("session_token={}", self.config.session_token),
            )
            .send()
            .await
            .map_err(net)?;
        check_status(resp).await?;
        Ok(())
    }

    pub async fn get_task(&self, id: &str) -> Result<HermesTask, IntegrationError> {
        let path = format!("/api/plugins/kanban/tasks/{id}");
        let resp = self.auth(Method::GET, &path).send().await.map_err(net)?;
        let resp = check_status(resp).await?;
        resp.json::<HermesTask>().await.map_err(dec)
    }

    pub async fn patch_task(
        &self,
        id: &str,
        patch: HermesTaskPatch,
    ) -> Result<(), IntegrationError> {
        let path = format!("/api/plugins/kanban/tasks/{id}");
        let resp = self
            .auth(Method::PATCH, &path)
            .json(&patch)
            .send()
            .await
            .map_err(net)?;
        check_status(resp).await?;
        Ok(())
    }

    pub async fn reclaim_task(&self, id: &str) -> Result<(), IntegrationError> {
        let path = format!("/api/plugins/kanban/tasks/{id}/reclaim");
        let resp = self.auth(Method::POST, &path).send().await.map_err(net)?;
        check_status(resp).await?;
        Ok(())
    }

    /// Returns raw stdout/stderr concatenated. Empty string when the
    /// run hasn't produced any output yet.
    pub async fn get_task_log(&self, id: &str, tail: usize) -> Result<String, IntegrationError> {
        let path = format!("/api/plugins/kanban/tasks/{id}/log");
        let mut url = self.config.api_url(&path);
        url.query_pairs_mut().append_pair("tail", &tail.to_string());
        let resp = self
            .http
            .get(url)
            .header(
                header::AUTHORIZATION,
                format!("Bearer {}", self.config.session_token),
            )
            .header(
                header::COOKIE,
                format!("session_token={}", self.config.session_token),
            )
            .send()
            .await
            .map_err(net)?;
        let resp = check_status(resp).await?;
        resp.text().await.map_err(dec)
    }
}

// ── helpers ───────────────────────────────────────────────────────────

fn net(e: reqwest::Error) -> IntegrationError {
    IntegrationError::Network(e.to_string())
}

fn dec(e: reqwest::Error) -> IntegrationError {
    IntegrationError::Decode(e.to_string())
}

/// Map non-2xx HTTP statuses to typed [`IntegrationError`] variants.
async fn check_status(resp: Response) -> Result<Response, IntegrationError> {
    let status = resp.status();
    if status.is_success() {
        return Ok(resp);
    }
    let code = status.as_u16();
    let body = resp.text().await.unwrap_or_default();
    Err(match status {
        StatusCode::UNAUTHORIZED | StatusCode::FORBIDDEN => IntegrationError::Auth(body),
        StatusCode::NOT_FOUND => IntegrationError::NotFound,
        StatusCode::CONFLICT => IntegrationError::Conflict(body),
        StatusCode::BAD_REQUEST | StatusCode::UNPROCESSABLE_ENTITY => {
            IntegrationError::BadRequest(body)
        }
        _ => IntegrationError::Upstream { status: code, body },
    })
}
