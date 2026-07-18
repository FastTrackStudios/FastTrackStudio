//! HTTP bridge for the watchOS Task app.
//!
//! watchOS forbids WebSockets outside audio sessions (Apple TN3135), so the
//! watch cannot speak vox. This mounts plain axum routes that call the org's
//! `timer` + `inbox` backends **in-process** — the same concrete backends the
//! vox layer wraps in [`crate::org_layer_router`]. It is the Task counterpart
//! of FTS's `engine_watch.rs`, following the `EngineHost::extend` precedent
//! architect ships for watch remotes.
//!
//! Auth is a single static device token (`TASK_WATCH_TOKEN`) presented as
//! `Authorization: Bearer <token>` — enough for a personal watch over HTTPS.
//! Identity is the deterministic local owner
//! (`user_id = v5(org_id, "task-local-owner")`), matching `task_ui::chrome::
//! owner_id` and the CLI's `timer_owner_id`, so watch entries land in the same
//! keyspace as the desktop and CLI.

use axum::{
    Json, Router,
    extract::{Path, State},
    http::{HeaderMap, StatusCode},
    response::{IntoResponse, Response},
    routing::{get, post},
};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::AppState;
use inbox_proto::{Inbox, InboxItem};
use timer_proto::{StartTimerRequest, TimerService};

/// Namespace tag for the deterministic single-user owner id. MUST match
/// `task_ui::chrome::owner_id` / the CLI `timer_owner_id` so the watch,
/// web UI, and CLI all key timer rows on the same user.
fn owner_id(org_id: Uuid) -> Uuid {
    Uuid::new_v5(&org_id, b"task-local-owner")
}

/// Validate the `Authorization: Bearer <TASK_WATCH_TOKEN>` header. Returns
/// `Err(response)` (401/503) when the token is absent, unset, or wrong.
fn authorize(headers: &HeaderMap) -> Result<(), Response> {
    let expected = std::env::var("TASK_WATCH_TOKEN").unwrap_or_default();
    if expected.is_empty() {
        return Err((
            StatusCode::SERVICE_UNAVAILABLE,
            "watch bridge disabled: TASK_WATCH_TOKEN unset",
        )
            .into_response());
    }
    let presented = headers
        .get(axum::http::header::AUTHORIZATION)
        .and_then(|v| v.to_str().ok())
        .and_then(|v| v.strip_prefix("Bearer "))
        .unwrap_or("");
    // Length-then-bytes compare; not constant-time, but the token is a
    // high-entropy secret over TLS.
    if presented == expected {
        Ok(())
    } else {
        Err((StatusCode::UNAUTHORIZED, "bad or missing bearer token").into_response())
    }
}

/// Resolve `(org backend state, org_id, user_id)` for a slug, or a 404.
fn resolve(state: &AppState, slug: &str) -> Result<(crate::OrgAppState, Uuid, Uuid), Response> {
    let org = state
        .org(slug)
        .ok_or_else(|| (StatusCode::NOT_FOUND, format!("org `{slug}` not hosted")).into_response())?;
    let org_id = state
        .data_root
        .org(slug)
        .manifest()
        .map_err(|e| {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                format!("org `{slug}` manifest: {e}"),
            )
                .into_response()
        })?
        .id;
    let user_id = owner_id(org_id);
    Ok((org, org_id, user_id))
}

// ── Wire DTOs (the watch's JSON contract) ────────────────────────────────

#[derive(Deserialize, Default)]
struct StartBody {
    /// What's being worked on — free text (dictated on the watch). Optional.
    #[serde(default)]
    description: String,
}

#[derive(Deserialize)]
struct CaptureBody {
    /// The fleeting note body (dictated on the watch).
    body: String,
}

/// The subset of a `WorkSession` the watch renders.
#[derive(Serialize)]
struct TimerDto {
    id: String,
    description: String,
    /// RFC-3339 start instant — the watch computes elapsed from this.
    start_time: String,
    /// Present once stopped.
    end_time: Option<String>,
    running: bool,
}

impl From<timer_proto::WorkSession> for TimerDto {
    fn from(w: timer_proto::WorkSession) -> Self {
        TimerDto {
            id: w.id.to_string(),
            description: w.description,
            start_time: w.start_time.to_rfc3339(),
            end_time: w.end_time.map(|t| t.to_rfc3339()),
            running: w.end_time.is_none(),
        }
    }
}

#[derive(Serialize)]
struct CaptureDto {
    id: String,
    created: String,
}

// ── Handlers ─────────────────────────────────────────────────────────────

async fn timer_start(
    State(state): State<AppState>,
    Path(slug): Path<String>,
    headers: HeaderMap,
    body: Option<Json<StartBody>>,
) -> Response {
    if let Err(r) = authorize(&headers) {
        return r;
    }
    let (org, org_id, user_id) = match resolve(&state, &slug) {
        Ok(v) => v,
        Err(r) => return r,
    };
    let description = body.map(|Json(b)| b.description).unwrap_or_default();
    let req = StartTimerRequest {
        user_id,
        org_id,
        project_id: None,
        project_path: String::new(),
        task_note_path: String::new(),
        description,
    };
    match org.timer.start_timer(req).await {
        Ok(ws) => Json(TimerDto::from(ws)).into_response(),
        Err(e) => (StatusCode::CONFLICT, e.to_string()).into_response(),
    }
}

async fn timer_stop(
    State(state): State<AppState>,
    Path(slug): Path<String>,
    headers: HeaderMap,
) -> Response {
    if let Err(r) = authorize(&headers) {
        return r;
    }
    let (org, _org_id, user_id) = match resolve(&state, &slug) {
        Ok(v) => v,
        Err(r) => return r,
    };
    match org.timer.stop_timer(user_id).await {
        Ok(ws) => Json(TimerDto::from(ws)).into_response(),
        // No active timer is a normal, non-error outcome for the watch.
        Err(_) => StatusCode::NO_CONTENT.into_response(),
    }
}

async fn timer_active(
    State(state): State<AppState>,
    Path(slug): Path<String>,
    headers: HeaderMap,
) -> Response {
    if let Err(r) = authorize(&headers) {
        return r;
    }
    let (org, _org_id, user_id) = match resolve(&state, &slug) {
        Ok(v) => v,
        Err(r) => return r,
    };
    match org.timer.active_timer(user_id).await {
        Ok(Some(ws)) => Json(Some(TimerDto::from(ws))).into_response(),
        Ok(None) => Json(None::<TimerDto>).into_response(),
        Err(e) => (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()).into_response(),
    }
}

async fn inbox_capture(
    State(state): State<AppState>,
    Path(slug): Path<String>,
    headers: HeaderMap,
    Json(body): Json<CaptureBody>,
) -> Response {
    if let Err(r) = authorize(&headers) {
        return r;
    }
    let (org, _org_id, _user_id) = match resolve(&state, &slug) {
        Ok(v) => v,
        Err(r) => return r,
    };
    let text = body.body.trim();
    if text.is_empty() {
        return (StatusCode::BAD_REQUEST, "empty note").into_response();
    }
    let id = Uuid::new_v4().to_string();
    let created = chrono::Utc::now().to_rfc3339();
    let item = InboxItem::capture(id.clone(), text, "watch", created.clone());
    match org.inbox.upsert_inbox_item(&item) {
        Ok(()) => (StatusCode::CREATED, Json(CaptureDto { id, created })).into_response(),
        Err(e) => (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()).into_response(),
    }
}

/// The watch bridge routes, to be `.merge`d into the top-level [`crate::router`]
/// (whose trailing `.with_state(state)` supplies the [`AppState`]).
pub fn watch_router() -> Router<AppState> {
    Router::new()
        .route("/org/{slug}/watch/v1/timer/start", post(timer_start))
        .route("/org/{slug}/watch/v1/timer/stop", post(timer_stop))
        .route("/org/{slug}/watch/v1/timer/active", get(timer_active))
        .route("/org/{slug}/watch/v1/inbox", post(inbox_capture))
}
