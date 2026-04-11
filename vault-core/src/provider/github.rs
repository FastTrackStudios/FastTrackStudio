//! GitHub Issues bidirectional sync.
//!
//! Syncs vault-core tasks with GitHub Issues via the REST API.
//! This is a sync module (like `nextcloud_sync`) — not a full `ProjectProvider`.
//!
//! - **Pull**: fetches open/closed issues and maps them to `Task` structs.
//! - **Push**: updates issue state (open/closed) when a task's status changes.
//! - **Create**: creates new GitHub issues from local tasks.

use crate::service::VaultError;
use crate::task::{Priority, Status, Task, WikiLink};
use serde::Deserialize;

// ── Configuration ───────────────────────────────────────────────────────────

/// Configuration for GitHub integration.
#[derive(Debug, Clone)]
pub struct GitHubConfig {
    /// Personal access token.
    pub token: String,
    /// Repository in `owner/repo` form (e.g. `"FastTrackStudios/task"`).
    pub repo: String,
    /// API base URL (default: `"https://api.github.com"`).
    pub api_url: String,
}

impl GitHubConfig {
    /// Create a config with the default GitHub API URL.
    pub fn new(token: impl Into<String>, repo: impl Into<String>) -> Self {
        Self {
            token: token.into(),
            repo: repo.into(),
            api_url: "https://api.github.com".to_string(),
        }
    }
}

// ── API response types (serde) ──────────────────────────────────────────────

#[derive(Debug, Deserialize)]
struct GitHubIssue {
    number: u64,
    title: String,
    body: Option<String>,
    state: String,
    assignee: Option<GitHubUser>,
    labels: Vec<GitHubLabel>,
    milestone: Option<GitHubMilestone>,
    // Pull requests also appear in /issues — skip them.
    pull_request: Option<serde_json::Value>,
}

#[derive(Debug, Deserialize)]
struct GitHubUser {
    login: String,
}

#[derive(Debug, Deserialize)]
struct GitHubLabel {
    name: String,
}

#[derive(Debug, Deserialize)]
struct GitHubMilestone {
    title: String,
}

#[derive(Debug, Deserialize)]
struct GitHubCreateIssueResponse {
    number: u64,
}

// ── Sync result ─────────────────────────────────────────────────────────────

/// Summary of a sync operation.
#[derive(Debug, Clone, Default)]
pub struct GitHubSyncResult {
    pub issues_pulled: usize,
    pub tasks_created: usize,
    pub statuses_pushed: usize,
    pub errors: Vec<String>,
}

// ── GitHubSync client ───────────────────────────────────────────────────────

/// GitHub sync client.
pub struct GitHubSync {
    config: GitHubConfig,
    http: reqwest::Client,
}

impl GitHubSync {
    /// Create a new sync client.
    pub fn new(config: GitHubConfig) -> Self {
        let http = reqwest::Client::builder()
            .user_agent("vault-core/0.1")
            .build()
            .expect("failed to build reqwest client");
        Self { config, http }
    }

    // ── helpers ─────────────────────────────────────────────────────────

    /// Build the base URL for the configured repo's issues endpoint.
    fn issues_url(&self) -> String {
        format!("{}/repos/{}/issues", self.config.api_url, self.config.repo)
    }

    /// Attach auth + accept headers to a request builder.
    fn auth(&self, req: reqwest::RequestBuilder) -> reqwest::RequestBuilder {
        req.header("Authorization", format!("Bearer {}", self.config.token))
            .header("Accept", "application/vnd.github+json")
            .header("X-GitHub-Api-Version", "2022-11-28")
    }

    /// Map a `GitHubIssue` to a vault-core `Task`.
    fn issue_to_task(issue: &GitHubIssue) -> Task {
        let status = match issue.state.as_str() {
            "closed" => Status::Done,
            _ => Status::Open,
        };

        // Map label names that look like priorities.
        let priority = issue
            .labels
            .iter()
            .find_map(|l| match l.name.to_lowercase().as_str() {
                "urgent" | "priority: critical" => Some(Priority::Urgent),
                "high" | "priority: high" => Some(Priority::High),
                "medium" | "priority: medium" => Some(Priority::Normal),
                "low" | "priority: low" => Some(Priority::Low),
                _ => None,
            })
            .unwrap_or(Priority::None);

        let tags: Vec<String> = issue
            .labels
            .iter()
            .map(|l| l.name.clone())
            .collect();

        let projects: Vec<WikiLink> = issue
            .milestone
            .as_ref()
            .map(|m| vec![WikiLink(m.title.clone())])
            .unwrap_or_default();

        let assignee = issue.assignee.as_ref().map(|u| u.login.clone());

        Task {
            title: issue.title.clone(),
            status,
            priority,
            tags,
            projects,
            assignee,
            body: issue.body.clone().unwrap_or_default(),
            external_id: Some(issue.number.to_string()),
            external_source: Some("github".to_string()),
            ..Default::default()
        }
    }

    // ── public API ──────────────────────────────────────────────────────

    /// Pull all open issues from the repo as tasks.
    ///
    /// Paginates through all pages (100 per page) and skips pull requests.
    pub async fn pull_issues(&self) -> Result<Vec<Task>, VaultError> {
        let mut tasks = Vec::new();
        let mut page: u32 = 1;

        loop {
            let url = format!("{}?state=open&per_page=100&page={page}", self.issues_url());
            let resp = self
                .auth(self.http.get(&url))
                .send()
                .await
                .map_err(|e| VaultError::IoError(format!("GitHub API request failed: {e}")))?;

            if !resp.status().is_success() {
                let status = resp.status();
                let body = resp
                    .text()
                    .await
                    .unwrap_or_else(|_| "<no body>".to_string());
                return Err(VaultError::IoError(format!(
                    "GitHub API returned {status}: {body}"
                )));
            }

            let issues: Vec<GitHubIssue> = resp
                .json()
                .await
                .map_err(|e| VaultError::ParseError(format!("failed to parse issues: {e}")))?;

            if issues.is_empty() {
                break;
            }

            for issue in &issues {
                // The /issues endpoint also returns pull requests — skip them.
                if issue.pull_request.is_some() {
                    continue;
                }
                tasks.push(Self::issue_to_task(issue));
            }

            // If fewer than 100 results, we've reached the last page.
            if issues.len() < 100 {
                break;
            }
            page += 1;
        }

        Ok(tasks)
    }

    /// Push a task's status to its linked GitHub issue.
    ///
    /// Only operates on tasks where `external_source == "github"` and
    /// `external_id` is set. Closes the issue if the task is `Done` or
    /// `Cancelled`; reopens it otherwise.
    pub async fn push_status(&self, task: &Task) -> Result<(), VaultError> {
        let number = task
            .external_id
            .as_deref()
            .ok_or_else(|| VaultError::NotFound("task has no external_id".to_string()))?;

        let is_github = task
            .external_source
            .as_deref()
            .map(|s| s == "github")
            .unwrap_or(false);

        if !is_github {
            return Err(VaultError::NotFound(
                "task external_source is not github".to_string(),
            ));
        }

        let state = match task.status {
            Status::Done | Status::Cancelled => "closed",
            _ => "open",
        };

        let url = format!("{}/{number}", self.issues_url());
        let body = serde_json::json!({ "state": state });

        let resp = self
            .auth(self.http.patch(&url))
            .json(&body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("GitHub PATCH failed: {e}")))?;

        if !resp.status().is_success() {
            let status = resp.status();
            let text = resp
                .text()
                .await
                .unwrap_or_else(|_| "<no body>".to_string());
            return Err(VaultError::IoError(format!(
                "GitHub PATCH /issues/{number} returned {status}: {text}"
            )));
        }

        Ok(())
    }

    /// Create a GitHub issue from a task.
    ///
    /// Returns the newly created issue number.
    pub async fn create_issue(&self, task: &Task) -> Result<u64, VaultError> {
        let labels: Vec<&str> = task.tags.iter().map(|s| s.as_str()).collect();
        let assignees: Vec<&str> = task
            .assignee
            .as_deref()
            .into_iter()
            .collect();

        let body = serde_json::json!({
            "title": task.title,
            "body": if task.body.is_empty() { None } else { Some(&task.body) },
            "labels": labels,
            "assignees": assignees,
        });

        let resp = self
            .auth(self.http.post(&self.issues_url()))
            .json(&body)
            .send()
            .await
            .map_err(|e| VaultError::IoError(format!("GitHub POST failed: {e}")))?;

        if !resp.status().is_success() {
            let status = resp.status();
            let text = resp
                .text()
                .await
                .unwrap_or_else(|_| "<no body>".to_string());
            return Err(VaultError::IoError(format!(
                "GitHub POST /issues returned {status}: {text}"
            )));
        }

        let created: GitHubCreateIssueResponse = resp
            .json()
            .await
            .map_err(|e| VaultError::ParseError(format!("failed to parse create response: {e}")))?;

        Ok(created.number)
    }

    /// Full bidirectional sync.
    ///
    /// 1. Pulls all open issues from GitHub.
    /// 2. For each remote issue not present in `local_tasks` (by `external_id`),
    ///    yields a new `Task` in the result.
    /// 3. For each local task with `external_source = "github"`, pushes its
    ///    status if it differs from the remote state.
    pub async fn sync(&self, local_tasks: &[Task]) -> Result<GitHubSyncResult, VaultError> {
        let mut result = GitHubSyncResult::default();

        // ── Pull ────────────────────────────────────────────────────────
        let remote_issues = self.pull_issues().await?;
        result.issues_pulled = remote_issues.len();

        // Build a set of issue numbers already tracked locally.
        let local_external_ids: std::collections::HashSet<&str> = local_tasks
            .iter()
            .filter(|t| t.external_source.as_deref() == Some("github"))
            .filter_map(|t| t.external_id.as_deref())
            .collect();

        // New issues not yet in local tasks.
        for issue_task in &remote_issues {
            if let Some(ref eid) = issue_task.external_id {
                if !local_external_ids.contains(eid.as_str()) {
                    result.tasks_created += 1;
                }
            }
        }

        // ── Push ────────────────────────────────────────────────────────
        // For each local github-linked task, check if status diverges from remote.
        let remote_by_number: std::collections::HashMap<&str, &Task> = remote_issues
            .iter()
            .filter_map(|t| t.external_id.as_deref().map(|id| (id, t)))
            .collect();

        for local in local_tasks {
            if local.external_source.as_deref() != Some("github") {
                continue;
            }
            let eid = match local.external_id.as_deref() {
                Some(id) => id,
                None => continue,
            };

            // Determine the expected remote state from the local status.
            let local_closed = matches!(local.status, Status::Done | Status::Cancelled);

            // Determine current remote state (if we pulled it).
            let remote_closed = remote_by_number
                .get(eid)
                .map(|r| matches!(r.status, Status::Done | Status::Cancelled))
                // If the issue wasn't in our pull (e.g. already closed and we
                // only pulled open), assume it matches so we still push.
                .unwrap_or(!local_closed);

            if local_closed != remote_closed {
                match self.push_status(local).await {
                    Ok(()) => result.statuses_pushed += 1,
                    Err(e) => result.errors.push(format!("push issue #{eid}: {e}")),
                }
            }
        }

        Ok(result)
    }
}
