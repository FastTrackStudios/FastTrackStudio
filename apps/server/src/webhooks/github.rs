//! GitHub webhook receiver.
//!
//! Mounted at `/webhooks/github/{repo_path}` where `repo_path` is the
//! `GitRepoConnection.webhook_path` suffix (e.g. `gh-7f3a`). That
//! lookup yields the sealed webhook secret; we unseal, recompute
//! `sha256=hmac(secret, raw_body)`, and compare in constant time
//! against `X-Hub-Signature-256`.
//!
//! Event dispatch is shallow on purpose — v1 cares about
//! `pull_request` (open / close / synchronize) and `push` (scan
//! commits for `[A-Z]{2,4}-[A-F0-9]{4}` task short_ids). All other
//! event types log + 200.

use std::sync::Arc;

use agent_proto::{GitRepoConnection, GitRepoConnectionRepo};
use architect::Page;
use axum::{
    Router,
    body::Bytes,
    extract::{Path, State},
    http::{HeaderMap, StatusCode},
    routing::post,
};
use chrono::Utc;
use hmac::{Hmac, Mac};
use project_proto::{CommitRef, TaskRepo, TaskUpdate};
use regex::Regex;
use sha2::Sha256;
use subtle::ConstantTimeEq;
use uuid::Uuid;

use crate::AppState;

pub fn router() -> Router<AppState> {
    Router::new().route("/{repo_path}", post(handle_github))
}

#[derive(thiserror::Error, Debug)]
#[allow(dead_code)]
enum HandlerError {
    #[error("repo not configured")]
    UnknownRepo,
    #[error("missing X-Hub-Signature-256")]
    MissingSignature,
    #[error("signature verification failed")]
    BadSignature,
    #[error("seal/unseal: {0}")]
    Seal(String),
    #[error("payload parse: {0}")]
    Parse(String),
    #[error("internal: {0}")]
    Internal(String),
}

async fn handle_github(
    State(state): State<AppState>,
    Path(repo_path): Path<String>,
    headers: HeaderMap,
    body: Bytes,
) -> Result<StatusCode, (StatusCode, String)> {
    let body_vec: Vec<u8> = body.to_vec();
    let headers_json = super::headers_to_json_string(&headers);

    // 1. Look up the GitRepoConnection by webhook_path. Inserted
    //    pre-verification so failed-auth attempts are still visible.
    let conn = match find_repo_by_webhook_path(&state, &repo_path).await {
        Ok(Some(c)) => c,
        Ok(None) => {
            let row_id = state
                .webhook_inbox
                .insert("github", headers_json, body_vec, false)
                .await;
            state
                .webhook_inbox
                .finish(row_id, Some("unknown repo_path".to_string()))
                .await;
            return Err((StatusCode::NOT_FOUND, "unknown repo".to_string()));
        }
        Err(e) => {
            tracing::error!(?e, "github webhook: lookup failed");
            return Err((StatusCode::INTERNAL_SERVER_ERROR, "lookup failed".into()));
        }
    };

    // 2. Verify signature.
    let signature_ok = match verify_signature(&state, &conn, &headers, &body_vec).await {
        Ok(ok) => ok,
        Err(e) => {
            tracing::warn!(?e, "github webhook: signature verify error");
            false
        }
    };

    let row_id = state
        .webhook_inbox
        .insert("github", headers_json, body_vec.clone(), signature_ok)
        .await;

    if !signature_ok {
        state
            .webhook_inbox
            .finish(row_id, Some("signature failed".to_string()))
            .await;
        return Err((StatusCode::UNAUTHORIZED, "bad signature".to_string()));
    }

    // 3. Dispatch on X-GitHub-Event.
    let event = headers
        .get("x-github-event")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("")
        .to_string();

    if event == "ping" {
        state.webhook_inbox.finish(row_id, None).await;
        return Ok(StatusCode::OK);
    }

    let payload: serde_json::Value = match serde_json::from_slice(&body_vec) {
        Ok(v) => v,
        Err(e) => {
            state
                .webhook_inbox
                .finish(row_id, Some(format!("bad json: {e}")))
                .await;
            return Err((StatusCode::BAD_REQUEST, "bad json".into()));
        }
    };

    let dispatch_result = match event.as_str() {
        "pull_request" => handle_pull_request(payload, state.clone()).await,
        "push" => handle_push(payload, state.clone()).await,
        other => {
            tracing::info!(event = other, "github webhook: ignoring event");
            Ok(())
        }
    };

    match dispatch_result {
        Ok(()) => {
            state.webhook_inbox.finish(row_id, None).await;
            Ok(StatusCode::OK)
        }
        Err(e) => {
            let msg = e.to_string();
            tracing::warn!(?e, "github webhook handler failed");
            state.webhook_inbox.finish(row_id, Some(msg.clone())).await;
            // We still return 200 here — GitHub retries on 5xx and
            // we don't want a runaway retry storm for a dispatch
            // problem. Audit row carries the error.
            Ok(StatusCode::OK)
        }
    }
}

async fn find_repo_by_webhook_path(
    state: &AppState,
    repo_path: &str,
) -> Result<Option<GitRepoConnection>, HandlerError> {
    let repo = state.git_repo_repo.clone();
    let list = repo
        .list(
            Page {
                index: 0,
                size: 1000,
            },
            None,
            None,
        )
        .await
        .map_err(|e| HandlerError::Internal(format!("list git_repo_connections: {e}")))?;
    Ok(list.items.into_iter().find(|c| c.webhook_path == repo_path))
}

async fn verify_signature(
    state: &AppState,
    conn: &GitRepoConnection,
    headers: &HeaderMap,
    body: &[u8],
) -> Result<bool, HandlerError> {
    let sig_header = headers
        .get("x-hub-signature-256")
        .and_then(|v| v.to_str().ok())
        .ok_or(HandlerError::MissingSignature)?;
    let expected_hex = sig_header
        .strip_prefix("sha256=")
        .ok_or(HandlerError::MissingSignature)?;

    let sealing = state
        .sealing
        .as_ref()
        .ok_or_else(|| HandlerError::Seal("sealing not configured".to_string()))?;
    let secret = sealing
        .unseal(&conn.webhook_secret_hash)
        .map_err(|e| HandlerError::Seal(e.to_string()))?;

    type HmacSha256 = Hmac<Sha256>;
    let mut mac = HmacSha256::new_from_slice(&secret)
        .map_err(|e| HandlerError::Internal(format!("hmac init: {e}")))?;
    mac.update(body);
    let computed = mac.finalize().into_bytes();
    let expected = hex::decode(expected_hex).map_err(|_| HandlerError::BadSignature)?;
    if expected.len() != computed.len() {
        return Ok(false);
    }
    Ok(expected.ct_eq(&computed).into())
}

async fn find_task_by_branch(
    state: &AppState,
    branch_name: &str,
) -> Result<Option<Uuid>, HandlerError> {
    // v1: linear scan. FUTURE: index by branch_name in a Loro map.
    let list = state
        .task_repo
        .list(
            Page {
                index: 0,
                size: 10_000,
            },
            None,
            None,
        )
        .await
        .map_err(|e| HandlerError::Internal(format!("list tasks: {e}")))?;
    Ok(list
        .items
        .into_iter()
        .find(|t| t.branch_name.as_deref() == Some(branch_name))
        .map(|t| t.id))
}

async fn handle_pull_request(
    payload: serde_json::Value,
    state: AppState,
) -> Result<(), HandlerError> {
    let action = payload["action"].as_str().unwrap_or("").to_string();
    let head_ref = payload["pull_request"]["head"]["ref"]
        .as_str()
        .unwrap_or("")
        .to_string();
    let pr_url = payload["pull_request"]["html_url"]
        .as_str()
        .unwrap_or("")
        .to_string();
    let merged = payload["pull_request"]["merged"].as_bool().unwrap_or(false);
    let draft = payload["pull_request"]["draft"].as_bool().unwrap_or(false);

    if head_ref.is_empty() {
        return Ok(());
    }
    let task_id = match find_task_by_branch(&state, &head_ref).await? {
        Some(id) => id,
        None => return Ok(()), // unrelated PR
    };

    let task = state
        .task_repo
        .get(task_id)
        .await
        .map_err(|e| HandlerError::Internal(format!("get task: {e}")))?;

    // Build the new pr_urls (dedup append).
    let mut new_pr_urls = task.pr_urls.clone();
    if !pr_url.is_empty() && !new_pr_urls.contains(&pr_url) {
        new_pr_urls.push(pr_url.clone());
    }

    // Status state machine.
    let (status_to_set, completed_at_to_set) = match action.as_str() {
        "opened" | "ready_for_review" if !draft => {
            if task.status.eq_ignore_ascii_case("todo") {
                (Some("doing".to_string()), None)
            } else {
                (None, None)
            }
        }
        "closed" if merged => {
            if !task.status.eq_ignore_ascii_case("done") {
                (Some("done".to_string()), Some(Some(Utc::now())))
            } else {
                (None, None)
            }
        }
        _ => (None, None),
    };

    let upd = TaskUpdate {
        status: status_to_set,
        completed_at: completed_at_to_set,
        pr_urls: if new_pr_urls.len() == task.pr_urls.len() {
            None
        } else {
            Some(new_pr_urls)
        },
        ..Default::default()
    };
    state
        .task_repo
        .update(task_id, upd)
        .await
        .map_err(|e| HandlerError::Internal(format!("update task: {e}")))?;
    Ok(())
}

async fn handle_push(payload: serde_json::Value, state: AppState) -> Result<(), HandlerError> {
    let empty: Vec<serde_json::Value> = Vec::new();
    let commits = payload["commits"].as_array().unwrap_or(&empty);
    if commits.is_empty() {
        return Ok(());
    }

    // Matches "PRJ-7F3A" — 2..4 ASCII uppercase letters, dash, 4 hex chars.
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    let re = RE.get_or_init(|| {
        Regex::new(r"\b([A-Z]{2,4}-[A-F0-9]{4})\b").expect("static regex compiles")
    });

    // v1: list tasks once and bucket by short_id.
    let task_list = state
        .task_repo
        .list(
            Page {
                index: 0,
                size: 10_000,
            },
            None,
            None,
        )
        .await
        .map_err(|e| HandlerError::Internal(format!("list tasks: {e}")))?;

    for commit in commits {
        let sha = commit["id"].as_str().unwrap_or("").to_string();
        let full_msg = commit["message"].as_str().unwrap_or("");
        let first_line = full_msg.lines().next().unwrap_or("").to_string();
        let url = commit["url"].as_str().map(|s| s.to_string());
        let authored_at = commit["timestamp"]
            .as_str()
            .and_then(|s| chrono::DateTime::parse_from_rfc3339(s).ok())
            .map(|d| d.with_timezone(&Utc))
            .unwrap_or_else(Utc::now);

        let mut matched_task_ids: Vec<Uuid> = Vec::new();
        for cap in re.captures_iter(full_msg) {
            let token = cap.get(1).unwrap().as_str();
            for t in &task_list.items {
                if let Some(branch) = &t.branch_name {
                    if branch.contains(token) && !matched_task_ids.contains(&t.id) {
                        matched_task_ids.push(t.id);
                    }
                }
            }
        }

        for task_id in matched_task_ids {
            // Re-fetch fresh; another commit in this same push may have
            // already added a ref.
            let task = match state.task_repo.get(task_id).await {
                Ok(t) => t,
                Err(_) => continue,
            };
            if task.commit_refs.iter().any(|c| c.sha == sha) {
                continue;
            }
            let mut new_commits = task.commit_refs.clone();
            new_commits.push(CommitRef {
                sha: sha.clone(),
                message: first_line.clone(),
                url: url.clone(),
                authored_at,
            });
            let upd = TaskUpdate {
                commit_refs: Some(new_commits),
                ..Default::default()
            };
            let _ = state.task_repo.update(task_id, upd).await;
        }
    }
    Ok(())
}

// `Arc<...>` keeps the lints happy even when individual helpers don't use it.
#[allow(dead_code)]
fn _ensure_arc_used(_: Arc<()>) {}
