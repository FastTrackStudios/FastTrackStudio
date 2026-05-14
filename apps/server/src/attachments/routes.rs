//! Phase 7 — HTTP routes for blob upload + download.
//!
//! Two endpoints, both verifying a `?token=…` signed by the
//! server's keypair before doing any work:
//!
//! - `PUT /blobs/upload?upload_id=<uuid>&token=<sig>` — reads the
//!   request body, hashes (sha256), persists to the `ObjectStore`
//!   under the hash. Looks up the pending session by `upload_id`,
//!   consumes it, writes a catalog row, and returns the hash in
//!   the response.
//! - `GET /blobs/download/{content_hash}?token=<sig>` — verifies
//!   the token's subject matches the path, streams the blob.

use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use attachments_proto::AttachmentMeta;
use axum::Router;
use axum::body::Bytes;
use axum::extract::{Path, Query, State};
use axum::http::StatusCode;
use axum::response::IntoResponse;
use axum::routing::{get, put};
use serde::Deserialize;
use sha2::{Digest, Sha256};
use uuid::Uuid;

use crate::attachments::service::AttachmentServiceImpl;
use crate::attachments::signed_url::{BlobToken, BlobTokenPurpose};

/// Router carrying the two blob routes. Mounted by `task-server`'s
/// top-level router alongside `/health` + `/vox`.
pub fn attachment_router() -> Router<AttachmentRouteState> {
    Router::new()
        .route("/blobs/upload", put(handle_upload))
        .route("/blobs/download/{content_hash}", get(handle_download))
}

/// Sliver of `AppState` the routes need. Avoids depending on the
/// full state shape so the attachments module stays decoupled.
#[derive(Clone)]
pub struct AttachmentRouteState {
    pub service: Arc<AttachmentServiceImpl>,
}

fn now_unix() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
}

#[derive(Debug, Deserialize)]
pub struct UploadQuery {
    upload_id: Uuid,
    token: String,
}

async fn handle_upload(
    State(state): State<AttachmentRouteState>,
    Query(q): Query<UploadQuery>,
    body: Bytes,
) -> impl IntoResponse {
    // 1. Token check.
    let tok = match BlobToken::verify(&q.token, &state.service.keypair, now_unix()) {
        Ok(t) => t,
        Err(e) => {
            tracing::warn!(?e, "blob upload: token verify failed");
            return (StatusCode::UNAUTHORIZED, "bad token").into_response();
        }
    };
    if !matches!(tok.purpose, BlobTokenPurpose::Upload) || tok.upload_id() != Some(q.upload_id) {
        return (StatusCode::FORBIDDEN, "wrong-purpose token").into_response();
    }

    // 2. Pending session?
    let Some(pending) = state.service.peek_pending(&q.upload_id) else {
        return (StatusCode::NOT_FOUND, "no pending upload for that id").into_response();
    };
    // Optional sanity: caller said size_bytes; reject grossly
    // mismatched bodies (we don't enforce exactness — the schema
    // may have lied; just block runaway uploads).
    let body_len = body.len() as u64;
    if pending.size_bytes != 0 && body_len > pending.size_bytes.saturating_mul(2) {
        return (
            StatusCode::PAYLOAD_TOO_LARGE,
            format!("body {body_len} exceeds 2x declared {}", pending.size_bytes),
        )
            .into_response();
    }

    // 3. Hash + persist.
    let mut hasher = Sha256::new();
    hasher.update(&body);
    let hash = hex_encode(&hasher.finalize());
    if let Err(e) = state.service.store.put_blob(&hash, &body).await {
        tracing::error!(?e, "blob upload: store write failed");
        return (StatusCode::INTERNAL_SERVER_ERROR, "store error").into_response();
    }

    // 4. Consume the pending session, write the catalog row.
    let pending = state.service.take_pending(&q.upload_id).unwrap_or(pending);
    let meta = AttachmentMeta {
        content_hash: hash.clone(),
        filename: pending.filename,
        mime_type: pending.mime_type,
        size_bytes: body_len,
        doc_id: pending.doc_id,
    };
    state.service.record_commit(meta);

    (StatusCode::OK, hash).into_response()
}

async fn handle_download(
    State(state): State<AttachmentRouteState>,
    Path(content_hash): Path<String>,
    Query(q): Query<DownloadQuery>,
) -> impl IntoResponse {
    let tok = match BlobToken::verify(&q.token, &state.service.keypair, now_unix()) {
        Ok(t) => t,
        Err(e) => {
            tracing::warn!(?e, "blob download: token verify failed");
            return (StatusCode::UNAUTHORIZED, "bad token").into_response();
        }
    };
    if !matches!(tok.purpose, BlobTokenPurpose::Download)
        || tok.content_hash() != Some(content_hash.as_str())
    {
        return (StatusCode::FORBIDDEN, "wrong-purpose token").into_response();
    }
    match state.service.store.get_blob(&content_hash).await {
        Ok(bytes) => {
            let meta = state.service.catalog.get(&content_hash);
            let mime = meta
                .as_ref()
                .map(|m| m.mime_type.clone())
                .unwrap_or_else(|| "application/octet-stream".into());
            let headers = [(axum::http::header::CONTENT_TYPE, mime)];
            (StatusCode::OK, headers, bytes).into_response()
        }
        Err(attachments_proto::AttachmentError::NotFound) => {
            (StatusCode::NOT_FOUND, "no such blob").into_response()
        }
        Err(e) => {
            tracing::error!(?e, "blob download: store read failed");
            (StatusCode::INTERNAL_SERVER_ERROR, "store error").into_response()
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct DownloadQuery {
    token: String,
}

fn hex_encode(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        s.push_str(&format!("{b:02x}"));
    }
    s
}
