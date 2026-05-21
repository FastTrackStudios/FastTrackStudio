//! File-replication sync for the vault layer.
//!
//! Phase 2 of the sync architecture (see
//! `plans/sync-architecture.md`): per-file get / put / delete
//! with sha256-based conflict detection. No structural
//! awareness of markdown content — that lives in `vault` +
//! the editor. The server is a thin store keyed by
//! `(vault_id, rel_path)` → blob + sha256 + mtime.
//!
//! Modeled on
//! [`vrtmrz/obsidian-livesync`](https://github.com/vrtmrz/obsidian-livesync)'s
//! HTTP shape but storage is the filesystem (one folder per
//! vault under [`vault_root`]) rather than CouchDB. Easier to
//! debug; we don't need revision history at this phase.
//!
//! Conflict policy: **last-writer-wins with `If-Match`**. The
//! client sends the sha it last saw (`If-Match: <hex>`); if
//! the server's current sha differs, the response is `412
//! Precondition Failed` with the server's content and sha so
//! the client can show a 3-way merge. New files send
//! `If-Match: *` to mean "create only, fail on existing".
//!
//! Routes:
//! - `GET    /vault/:id/manifest`     → list of all files +
//!   sha + mtime.
//! - `GET    /vault/:id/file/*path`   → file content.
//! - `PUT    /vault/:id/file/*path`   → upload; `If-Match`
//!   guards.
//! - `DELETE /vault/:id/file/*path`   → remove; `If-Match`
//!   guards.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use axum::Router;
use axum::extract::ws::{Message, WebSocket, WebSocketUpgrade};
use axum::extract::{Path as AxumPath, State};
use axum::http::{HeaderMap, HeaderValue, StatusCode, header};
use axum::response::{IntoResponse, Response};
use axum::routing::{any, delete, get, put};
use serde::Serialize;
use sha2::{Digest, Sha256};
use tokio::sync::{Mutex, RwLock, broadcast};

/// Shared state for the vault-sync routes. One process can
/// serve many vaults — `root` is the parent directory under
/// which each vault lives at `{root}/{vault_id}/`. A `Mutex`
/// per-vault would be more granular, but we keep it simple
/// for v1.
#[derive(Clone)]
pub struct VaultSyncState {
    pub root: PathBuf,
    /// Single coarse lock — serializes writes across vaults.
    /// Read paths don't take it; conflicts use sha-then-write
    /// inside the lock. Refine to per-vault when latency
    /// matters.
    write_lock: Arc<Mutex<()>>,
    /// Per-vault broadcast channels. PUT / DELETE handlers
    /// publish a [`VaultEvent`] here; the WS endpoint
    /// subscribes per connection. Capacity 256 — bursts of
    /// rapid edits get coalesced client-side.
    channels: Arc<RwLock<HashMap<String, broadcast::Sender<VaultEvent>>>>,
}

impl VaultSyncState {
    pub fn new(root: PathBuf) -> std::io::Result<Self> {
        std::fs::create_dir_all(&root)?;
        Ok(Self {
            root,
            write_lock: Arc::new(Mutex::new(())),
            channels: Arc::new(RwLock::new(HashMap::new())),
        })
    }

    /// Get (or lazily create) the broadcast sender for a vault.
    async fn channel(&self, vault_id: &str) -> broadcast::Sender<VaultEvent> {
        if let Some(tx) = self.channels.read().await.get(vault_id) {
            return tx.clone();
        }
        let mut chans = self.channels.write().await;
        if let Some(tx) = chans.get(vault_id) {
            return tx.clone();
        }
        let (tx, _rx) = broadcast::channel::<VaultEvent>(256);
        chans.insert(vault_id.to_string(), tx.clone());
        tx
    }

    fn vault_dir(&self, vault_id: &str) -> PathBuf {
        self.root.join(vault_id)
    }

    fn file_path(&self, vault_id: &str, rel: &str) -> Result<PathBuf, SyncError> {
        // Refuse anything that could traverse out of the vault
        // dir. `rel` arrives URL-decoded from the router; reject
        // `..`, absolute paths, and Windows drive letters
        // defensively.
        if rel.is_empty()
            || rel.starts_with('/')
            || rel.starts_with('\\')
            || rel.contains("..")
            || rel.contains(':')
        {
            return Err(SyncError::BadPath);
        }
        Ok(self.vault_dir(vault_id).join(rel))
    }
}

/// Public router. Mount onto the main `Router` with
/// `.merge(vault_sync_router().with_state(vault_state))`.
/// Returned without state attached so the caller can wire it
/// up alongside the rest of the router builder; matches the
/// `attachments::attachment_router` pattern.
pub fn router() -> Router<VaultSyncState> {
    Router::new()
        .route("/vault/{vault_id}/manifest", get(manifest))
        .route("/vault/{vault_id}/subscribe", any(subscribe_ws))
        .route("/vault/{vault_id}/file/{*path}", get(get_file))
        .route("/vault/{vault_id}/file/{*path}", put(put_file))
        .route("/vault/{vault_id}/file/{*path}", delete(delete_file))
}

/// One change event on a vault. Sent to every WS subscriber
/// whenever a PUT or DELETE handler completes successfully.
/// JSON-serialized over the wire — same shape clients see in
/// manifest entries, plus an `op` discriminator.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "op", rename_all = "snake_case")]
pub enum VaultEvent {
    /// File was created or modified. `sha256` is the new
    /// content's digest; clients can skip the pull when their
    /// local sha already matches (echo from their own push).
    Put {
        path: String,
        sha256: String,
        mtime_ms: i64,
        size: u64,
    },
    /// File was removed.
    Delete { path: String },
}

async fn subscribe_ws(
    State(state): State<VaultSyncState>,
    AxumPath(vault_id): AxumPath<String>,
    ws: WebSocketUpgrade,
) -> Response {
    let tx = state.channel(&vault_id).await;
    let mut rx = tx.subscribe();
    ws.on_upgrade(move |socket| async move {
        run_subscriber(socket, &mut rx).await;
    })
}

async fn run_subscriber(mut socket: WebSocket, rx: &mut broadcast::Receiver<VaultEvent>) {
    loop {
        tokio::select! {
            // Forward broadcast events as JSON text frames.
            msg = rx.recv() => match msg {
                Ok(evt) => {
                    let json = match serde_json::to_string(&evt) {
                        Ok(s) => s,
                        Err(_) => continue,
                    };
                    if socket.send(Message::Text(json.into())).await.is_err() {
                        return;
                    }
                }
                Err(broadcast::error::RecvError::Lagged(_)) => {
                    // Subscriber missed events because we hit
                    // the channel cap. Send a hint so the
                    // client knows to re-pull the manifest.
                    let _ = socket
                        .send(Message::Text(r#"{"op":"resync"}"#.into()))
                        .await;
                }
                Err(broadcast::error::RecvError::Closed) => return,
            },
            // Drain inbound (we don't expect client → server
            // messages on this socket; pong frames pass
            // through). Closing the WS ends the loop.
            inbound = socket.recv() => match inbound {
                Some(Ok(Message::Close(_))) | None => return,
                Some(Ok(_)) => continue,
                Some(Err(_)) => return,
            }
        }
    }
}

#[derive(Serialize)]
struct ManifestEntry {
    path: String,
    sha256: String,
    mtime_ms: i64,
    size: u64,
}

#[derive(Serialize)]
struct Manifest {
    vault_id: String,
    files: Vec<ManifestEntry>,
}

async fn manifest(
    State(state): State<VaultSyncState>,
    AxumPath(vault_id): AxumPath<String>,
) -> Result<axum::Json<Manifest>, SyncError> {
    let dir = state.vault_dir(&vault_id);
    if !dir.exists() {
        return Ok(axum::Json(Manifest {
            vault_id,
            files: Vec::new(),
        }));
    }
    let mut entries = Vec::new();
    collect(&dir, &dir, &mut entries)?;
    Ok(axum::Json(Manifest {
        vault_id,
        files: entries,
    }))
}

fn collect(root: &Path, dir: &Path, out: &mut Vec<ManifestEntry>) -> Result<(), SyncError> {
    for entry in std::fs::read_dir(dir).map_err(SyncError::io)? {
        let entry = entry.map_err(SyncError::io)?;
        let path = entry.path();
        if path.is_dir() {
            collect(root, &path, out)?;
            continue;
        }
        let rel = path
            .strip_prefix(root)
            .map_err(|_| SyncError::Internal("strip prefix".into()))?
            .to_string_lossy()
            .replace('\\', "/");
        let bytes = std::fs::read(&path).map_err(SyncError::io)?;
        let sha256 = sha256_hex(&bytes);
        let mtime_ms = std::fs::metadata(&path)
            .map_err(SyncError::io)?
            .modified()
            .ok()
            .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
            .map(|d| d.as_millis() as i64)
            .unwrap_or(0);
        out.push(ManifestEntry {
            path: rel,
            sha256,
            mtime_ms,
            size: bytes.len() as u64,
        });
    }
    Ok(())
}

async fn get_file(
    State(state): State<VaultSyncState>,
    AxumPath((vault_id, rel)): AxumPath<(String, String)>,
) -> Result<Response, SyncError> {
    let abs = state.file_path(&vault_id, &rel)?;
    if !abs.exists() {
        return Err(SyncError::NotFound);
    }
    let bytes = std::fs::read(&abs).map_err(SyncError::io)?;
    let sha = sha256_hex(&bytes);
    let mtime_ms = std::fs::metadata(&abs)
        .map_err(SyncError::io)?
        .modified()
        .ok()
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0);
    let mut headers = HeaderMap::new();
    headers.insert("X-Vault-Sha256", HeaderValue::from_str(&sha).unwrap());
    headers.insert("X-Vault-Mtime-Ms", HeaderValue::from(mtime_ms));
    headers.insert(
        header::CONTENT_TYPE,
        HeaderValue::from_static("application/octet-stream"),
    );
    Ok((StatusCode::OK, headers, bytes).into_response())
}

async fn put_file(
    State(state): State<VaultSyncState>,
    AxumPath((vault_id, rel)): AxumPath<(String, String)>,
    headers: HeaderMap,
    body: axum::body::Bytes,
) -> Result<Response, SyncError> {
    let abs = state.file_path(&vault_id, &rel)?;
    let if_match = headers
        .get("If-Match")
        .and_then(|v| v.to_str().ok())
        .map(str::to_string);
    let _g = state.write_lock.lock().await;
    let existing_sha = if abs.exists() {
        let bytes = std::fs::read(&abs).map_err(SyncError::io)?;
        Some(sha256_hex(&bytes))
    } else {
        None
    };
    // If-Match semantics:
    //   `Some("*")` → create only; fail if file exists.
    //   `Some(hex)` → update only; fail unless the server's
    //                 current sha matches.
    //   `None`      → unconditional write (client is overwriting
    //                 blindly; only safe on first push of a
    //                 brand-new vault).
    if let Some(want) = &if_match {
        match (want.as_str(), existing_sha.as_deref()) {
            ("*", Some(_)) => return conflict(&abs, existing_sha.as_deref()),
            (h, Some(have)) if h != have => return conflict(&abs, existing_sha.as_deref()),
            _ => {}
        }
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(SyncError::io)?;
    }
    // Atomic write via temp+rename so concurrent reads never
    // see a half-written body.
    let tmp = abs.with_extension(format!(
        "{}.tmp.{}",
        abs.extension().and_then(|s| s.to_str()).unwrap_or(""),
        std::process::id()
    ));
    std::fs::write(&tmp, &body).map_err(SyncError::io)?;
    std::fs::rename(&tmp, &abs).map_err(SyncError::io)?;
    let new_sha = sha256_hex(&body);
    let mtime_ms = std::fs::metadata(&abs)
        .ok()
        .and_then(|m| m.modified().ok())
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0);
    // Drop the write lock before publishing — broadcast::send
    // is non-blocking, but keeps the critical section minimal.
    drop(_g);
    let tx = state.channel(&vault_id).await;
    let _ = tx.send(VaultEvent::Put {
        path: rel.clone(),
        sha256: new_sha.clone(),
        mtime_ms,
        size: body.len() as u64,
    });
    let mut resp_headers = HeaderMap::new();
    resp_headers.insert("X-Vault-Sha256", HeaderValue::from_str(&new_sha).unwrap());
    Ok((StatusCode::OK, resp_headers, new_sha).into_response())
}

async fn delete_file(
    State(state): State<VaultSyncState>,
    AxumPath((vault_id, rel)): AxumPath<(String, String)>,
    headers: HeaderMap,
) -> Result<StatusCode, SyncError> {
    let abs = state.file_path(&vault_id, &rel)?;
    let if_match = headers
        .get("If-Match")
        .and_then(|v| v.to_str().ok())
        .map(str::to_string);
    let _g = state.write_lock.lock().await;
    if !abs.exists() {
        return Ok(StatusCode::NOT_FOUND);
    }
    if let Some(want) = if_match {
        let bytes = std::fs::read(&abs).map_err(SyncError::io)?;
        let have = sha256_hex(&bytes);
        if want != have {
            return Err(SyncError::Conflict {
                server_sha: have,
                server_bytes: bytes,
            });
        }
    }
    std::fs::remove_file(&abs).map_err(SyncError::io)?;
    drop(_g);
    let tx = state.channel(&vault_id).await;
    let _ = tx.send(VaultEvent::Delete { path: rel.clone() });
    Ok(StatusCode::OK)
}

fn conflict(abs: &Path, existing_sha: Option<&str>) -> Result<Response, SyncError> {
    let bytes = std::fs::read(abs).map_err(SyncError::io)?;
    Err(SyncError::Conflict {
        server_sha: existing_sha.unwrap_or("").to_string(),
        server_bytes: bytes,
    })
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    let out = h.finalize();
    let mut hex = String::with_capacity(out.len() * 2);
    for b in out {
        use std::fmt::Write;
        let _ = write!(hex, "{b:02x}");
    }
    hex
}

#[derive(Debug, thiserror::Error)]
pub enum SyncError {
    #[error("io: {0}")]
    Io(String),
    #[error("bad path")]
    BadPath,
    #[error("not found")]
    NotFound,
    #[error("conflict")]
    Conflict {
        server_sha: String,
        server_bytes: Vec<u8>,
    },
    #[error("internal: {0}")]
    Internal(String),
}

impl SyncError {
    fn io(e: std::io::Error) -> Self {
        Self::Io(e.to_string())
    }
}

impl IntoResponse for SyncError {
    fn into_response(self) -> Response {
        match self {
            SyncError::NotFound => (StatusCode::NOT_FOUND, "not found").into_response(),
            SyncError::BadPath => (StatusCode::BAD_REQUEST, "bad path").into_response(),
            SyncError::Io(m) => (StatusCode::INTERNAL_SERVER_ERROR, m).into_response(),
            SyncError::Internal(m) => (StatusCode::INTERNAL_SERVER_ERROR, m).into_response(),
            SyncError::Conflict {
                server_sha,
                server_bytes,
            } => {
                let mut headers = HeaderMap::new();
                headers.insert(
                    "X-Vault-Sha256",
                    HeaderValue::from_str(&server_sha).unwrap(),
                );
                (StatusCode::PRECONDITION_FAILED, headers, server_bytes).into_response()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::body::Body;
    use axum::http::Request;
    use tower::util::ServiceExt;

    fn make_state() -> (tempfile::TempDir, VaultSyncState) {
        let tmp = tempfile::tempdir().unwrap();
        let state = VaultSyncState::new(tmp.path().to_path_buf()).unwrap();
        (tmp, state)
    }

    fn make_app(state: VaultSyncState) -> Router {
        super::router().with_state(state)
    }

    #[tokio::test]
    async fn manifest_empty_vault() {
        let (_tmp, state) = make_state();
        let app = make_app(state);
        let resp = app
            .oneshot(
                Request::builder()
                    .uri("/vault/v1/manifest")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(resp.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn put_then_get_round_trips() {
        let (_tmp, state) = make_state();
        let app = make_app(state);
        let put = app
            .clone()
            .oneshot(
                Request::builder()
                    .method("PUT")
                    .uri("/vault/v1/file/hello.md")
                    .header("If-Match", "*")
                    .body(Body::from("body"))
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(put.status(), StatusCode::OK);
        let get = app
            .oneshot(
                Request::builder()
                    .uri("/vault/v1/file/hello.md")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(get.status(), StatusCode::OK);
        let body = axum::body::to_bytes(get.into_body(), 1024).await.unwrap();
        assert_eq!(&body[..], b"body");
    }

    #[tokio::test]
    async fn if_match_star_refuses_existing_file() {
        let (_tmp, state) = make_state();
        let app = make_app(state);
        let _ = app
            .clone()
            .oneshot(
                Request::builder()
                    .method("PUT")
                    .uri("/vault/v1/file/x.md")
                    .header("If-Match", "*")
                    .body(Body::from("first"))
                    .unwrap(),
            )
            .await
            .unwrap();
        let resp = app
            .oneshot(
                Request::builder()
                    .method("PUT")
                    .uri("/vault/v1/file/x.md")
                    .header("If-Match", "*")
                    .body(Body::from("second"))
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(resp.status(), StatusCode::PRECONDITION_FAILED);
    }

    #[tokio::test]
    async fn if_match_with_stale_sha_returns_412() {
        let (_tmp, state) = make_state();
        let app = make_app(state);
        let _ = app
            .clone()
            .oneshot(
                Request::builder()
                    .method("PUT")
                    .uri("/vault/v1/file/x.md")
                    .header("If-Match", "*")
                    .body(Body::from("v1"))
                    .unwrap(),
            )
            .await
            .unwrap();
        let stale = app
            .oneshot(
                Request::builder()
                    .method("PUT")
                    .uri("/vault/v1/file/x.md")
                    .header("If-Match", "deadbeef")
                    .body(Body::from("v2"))
                    .unwrap(),
            )
            .await
            .unwrap();
        assert_eq!(stale.status(), StatusCode::PRECONDITION_FAILED);
    }

    #[tokio::test]
    async fn rejects_traversal_attempts() {
        let (_tmp, state) = make_state();
        let app = make_app(state);
        // `..` in the rel path gets normalized by Axum's path
        // matcher, so trigger our explicit guard by sending a
        // suspicious literal.
        let resp = app
            .oneshot(
                Request::builder()
                    .method("PUT")
                    .uri("/vault/v1/file/..%2Fescape.md")
                    .body(Body::from("x"))
                    .unwrap(),
            )
            .await
            .unwrap();
        assert!(matches!(
            resp.status(),
            StatusCode::BAD_REQUEST | StatusCode::NOT_FOUND
        ));
    }

    #[tokio::test]
    async fn put_broadcasts_event() {
        let (_tmp, state) = make_state();
        let tx = state.channel("v1").await;
        let mut rx = tx.subscribe();
        let app = make_app(state);
        let _ = app
            .oneshot(
                Request::builder()
                    .method("PUT")
                    .uri("/vault/v1/file/hello.md")
                    .header("If-Match", "*")
                    .body(Body::from("hi"))
                    .unwrap(),
            )
            .await
            .unwrap();
        let evt = tokio::time::timeout(std::time::Duration::from_secs(1), rx.recv())
            .await
            .expect("event timeout")
            .expect("recv ok");
        match evt {
            VaultEvent::Put { path, size, .. } => {
                assert_eq!(path, "hello.md");
                assert_eq!(size, 2);
            }
            other => panic!("expected Put, got {other:?}"),
        }
    }

    #[tokio::test]
    async fn delete_broadcasts_event() {
        let (_tmp, state) = make_state();
        let app = make_app(state.clone());
        let _ = app
            .clone()
            .oneshot(
                Request::builder()
                    .method("PUT")
                    .uri("/vault/v1/file/gone.md")
                    .header("If-Match", "*")
                    .body(Body::from("x"))
                    .unwrap(),
            )
            .await
            .unwrap();
        let tx = state.channel("v1").await;
        let mut rx = tx.subscribe();
        let _ = app
            .oneshot(
                Request::builder()
                    .method("DELETE")
                    .uri("/vault/v1/file/gone.md")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();
        let evt = tokio::time::timeout(std::time::Duration::from_secs(1), rx.recv())
            .await
            .expect("event timeout")
            .expect("recv ok");
        match evt {
            VaultEvent::Delete { path } => assert_eq!(path, "gone.md"),
            other => panic!("expected Delete, got {other:?}"),
        }
    }
}
