//! Hermes webhook receiver (belt-and-suspenders with the WS event loop).
//!
//! Mounted at `/webhooks/hermes/event`. Verifies HMAC-SHA256 against
//! `X-Webhook-Signature` using the configured `hermes_webhook_secret`.
//! Event dispatch is shallow: for `run.*` events we derive a
//! `RunSnapshot` from the payload fields and call
//! `sink.patch_run(run_id, snapshot)`; for `task.status_changed` we
//! call `sink.maybe_mirror_task_status`. Everything else logs + 200.

use agent_proto::integration::{AgentRunStatus, RunSnapshot};
use axum::{
    Router,
    body::Bytes,
    extract::State,
    http::{HeaderMap, StatusCode},
    routing::post,
};
use hmac::{Hmac, Mac};
use sha2::Sha256;
use subtle::ConstantTimeEq;
use uuid::Uuid;

use crate::AppState;

pub fn router() -> Router<AppState> {
    Router::new().route("/event", post(handle_hermes))
}

async fn handle_hermes(
    State(state): State<AppState>,
    headers: HeaderMap,
    body: Bytes,
) -> Result<StatusCode, (StatusCode, String)> {
    let body_vec: Vec<u8> = body.to_vec();
    let headers_json = super::headers_to_json_string(&headers);

    let signature_ok = verify_signature(&state, &headers, &body_vec);
    let row_id = state
        .webhook_inbox
        .insert("hermes", headers_json, body_vec.clone(), signature_ok)
        .await;

    if !signature_ok {
        state
            .webhook_inbox
            .finish(row_id, Some("signature failed".to_string()))
            .await;
        return Err((StatusCode::UNAUTHORIZED, "bad signature".to_string()));
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

    let result = dispatch_event(&state, payload).await;
    match result {
        Ok(()) => {
            state.webhook_inbox.finish(row_id, None).await;
            Ok(StatusCode::OK)
        }
        Err(e) => {
            state.webhook_inbox.finish(row_id, Some(e.clone())).await;
            tracing::warn!(error = %e, "hermes webhook dispatch error");
            // Stay 200 — see github.rs rationale.
            Ok(StatusCode::OK)
        }
    }
}

fn verify_signature(state: &AppState, headers: &HeaderMap, body: &[u8]) -> bool {
    let Some(secret) = state.hermes_webhook_secret.as_deref() else {
        // No secret configured → reject all signed traffic. (We do
        // NOT default-allow; if the operator hasn't set up Hermes
        // webhooks they shouldn't be reachable.)
        return false;
    };
    let Some(sig_header) = headers
        .get("x-webhook-signature")
        .and_then(|v| v.to_str().ok())
    else {
        return false;
    };
    let expected_hex = sig_header.strip_prefix("sha256=").unwrap_or(sig_header);
    let Ok(expected) = hex::decode(expected_hex) else {
        return false;
    };
    type HmacSha256 = Hmac<Sha256>;
    let Ok(mut mac) = HmacSha256::new_from_slice(secret.as_bytes()) else {
        return false;
    };
    mac.update(body);
    let computed = mac.finalize().into_bytes();
    if expected.len() != computed.len() {
        return false;
    }
    expected.ct_eq(&computed).into()
}

async fn dispatch_event(state: &AppState, payload: serde_json::Value) -> Result<(), String> {
    let kind = payload["event"]["kind"]
        .as_str()
        .or_else(|| payload["kind"].as_str())
        .unwrap_or("")
        .to_string();

    let sink = match &state.event_sink {
        Some(s) => s.clone(),
        None => return Err("event_sink not configured".to_string()),
    };

    match kind.as_str() {
        "run.queued" | "run.running" | "run.succeeded" | "run.failed" | "run.cancelled"
        | "run.completed" | "run.blocked" | "run.archived" => {
            let run_id = parse_uuid(&payload, &["event", "run_id"])
                .or_else(|| parse_uuid(&payload, &["run_id"]))
                .ok_or_else(|| "missing run_id".to_string())?;
            let snapshot = snapshot_from_payload(&payload, &kind);
            sink.inner
                .patch_run(run_id, snapshot)
                .await
                .map_err(|e| format!("patch_run: {e}"))?;
            Ok(())
        }
        "task.status_changed" => {
            let task_id = parse_uuid(&payload, &["event", "task_id"])
                .or_else(|| parse_uuid(&payload, &["task_id"]))
                .ok_or_else(|| "missing task_id".to_string())?;
            let new_status = payload["event"]["new_status"]
                .as_str()
                .or_else(|| payload["new_status"].as_str())
                .unwrap_or("")
                .to_string();
            if new_status.is_empty() {
                return Err("missing new_status".to_string());
            }
            sink.inner
                .maybe_mirror_task_status(task_id, &new_status)
                .await
                .map_err(|e| format!("mirror status: {e}"))?;
            Ok(())
        }
        other => {
            tracing::info!(event = other, "hermes webhook: ignoring event");
            Ok(())
        }
    }
}

fn parse_uuid(payload: &serde_json::Value, path: &[&str]) -> Option<Uuid> {
    let mut cur = payload;
    for p in path {
        cur = cur.get(*p)?;
    }
    cur.as_str().and_then(|s| Uuid::parse_str(s).ok())
}

fn snapshot_from_payload(payload: &serde_json::Value, kind: &str) -> RunSnapshot {
    let status = match kind {
        "run.queued" => AgentRunStatus::Queued,
        "run.running" => AgentRunStatus::Running,
        "run.blocked" => AgentRunStatus::Blocked,
        "run.failed" => AgentRunStatus::Failed,
        "run.cancelled" => AgentRunStatus::Cancelled,
        "run.archived" => AgentRunStatus::Archived,
        "run.succeeded" | "run.completed" => AgentRunStatus::Succeeded,
        _ => AgentRunStatus::Queued,
    };
    let ev = payload.get("event").unwrap_or(payload);
    let parse_dt = |v: &serde_json::Value| -> Option<chrono::DateTime<chrono::Utc>> {
        v.as_str()
            .and_then(|s| chrono::DateTime::parse_from_rfc3339(s).ok())
            .map(|d| d.with_timezone(&chrono::Utc))
    };
    RunSnapshot {
        status,
        started_at: parse_dt(&ev["started_at"]),
        completed_at: parse_dt(&ev["completed_at"]),
        result_summary: ev["result"].as_str().map(|s| s.to_string()),
        error: ev["error"].as_str().map(|s| s.to_string()),
        tokens_used: ev["tokens_used"].as_u64().map(|n| n as u32),
        cost_cents: ev["cost_cents"].as_u64().map(|n| n as u32),
        branch_name: ev["branch_name"].as_str().map(|s| s.to_string()),
        pr_urls: ev["pr_urls"]
            .as_array()
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(|s| s.to_string()))
                    .collect()
            })
            .unwrap_or_default(),
        commit_refs: Vec::new(),
    }
}
