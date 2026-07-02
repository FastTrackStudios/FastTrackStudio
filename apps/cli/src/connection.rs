//! Vox client connection to a running FTS REAPER extension.
//!
//! Connects to the Unix socket published by `fts-extensions`
//! (`socket_publisher::publish_extension_socket`). One REAPER process
//! per socket; discovery picks the most-recently-modified
//! `/tmp/fts-daw-*.sock` when no explicit path is given.
//!
//! Used by every "live" CLI subcommand (mode get/set, take-rank, …).
//! Returns a raw `vox::Caller` so the caller can construct any
//! generated service client (`SessionModeServiceClient`,
//! `TakeRankingServiceClient`, …) on top.

use std::path::{Path, PathBuf};

use eyre::{Result, WrapErr, eyre};

/// Pick a socket and open a Vox connection against it.
///
/// Resolution order for the socket path:
/// 1. `socket_override` if `Some(path)` — used by the global `--socket`
///    flag for explicit selection.
/// 2. `$FTS_SOCKET` env var — used by test rigs and scripts.
/// 3. Auto-discovery: most-recently-modified `/tmp/fts-daw-*.sock`
///    that's actually connectable (stale sockets get cleaned up).
pub async fn connect(socket_override: Option<&Path>) -> Result<vox::Caller> {
    let path = resolve_socket(socket_override).await?;
    open_session(&path).await
}

async fn resolve_socket(socket_override: Option<&Path>) -> Result<PathBuf> {
    if let Some(path) = socket_override {
        return Ok(path.to_path_buf());
    }
    if let Ok(env_path) = std::env::var("FTS_SOCKET") {
        return Ok(PathBuf::from(env_path));
    }
    discover_newest_socket().await
}

/// Scan `/tmp/fts-daw-*.sock` and return the newest (by mtime) that's
/// actually connectable. Stale sockets from crashed REAPER processes
/// are removed as they're discovered.
async fn discover_newest_socket() -> Result<PathBuf> {
    let mut candidates: Vec<(PathBuf, std::time::SystemTime)> = Vec::new();
    let entries = std::fs::read_dir("/tmp").wrap_err("read /tmp")?;
    for entry in entries.flatten() {
        let path = entry.path();
        let Some(name) = path.file_name().and_then(|n| n.to_str()) else {
            continue;
        };
        if !(name.starts_with("fts-daw-") && name.ends_with(".sock")) {
            continue;
        }
        if name.contains(".bootstrap.") {
            continue;
        }
        let Ok(meta) = entry.metadata() else { continue };
        let Ok(mtime) = meta.modified() else { continue };
        candidates.push((path, mtime));
    }
    // Newest first.
    candidates.sort_by(|a, b| b.1.cmp(&a.1));

    for (path, _) in candidates {
        if tokio::net::UnixStream::connect(&path).await.is_ok() {
            return Ok(path);
        }
        // Stale — clean up so the next run doesn't see it.
        let _ = std::fs::remove_file(&path);
    }

    Err(eyre!(
        "No connectable FTS REAPER socket found in /tmp (expected /tmp/fts-daw-<pid>.sock). \
         Is REAPER running with the fts-extensions plugin loaded?"
    ))
}

/// Minimal `FromVoxLane` client that captures the extension service
/// lane's `Caller` (vox 0.10 replacement for the removed `NoopClient`
/// and manual `open_connection` bootstrap). The lane's service name
/// is carried as metadata automatically; the extension's LayerRouter
/// dispatches by method id, so any generated service client can be
/// constructed on top of the returned `Caller`.
#[derive(Clone)]
struct SessionLaneClient {
    caller: vox::Caller,
}

impl vox::FromVoxLane for SessionLaneClient {
    const SERVICE_NAME: &'static str = "session-cli";

    fn from_vox_lane(caller: vox::Caller, _connection: Option<vox::ConnectionHandle>) -> Self {
        Self { caller }
    }
}

async fn open_session(path: &Path) -> Result<vox::Caller> {
    let t0 = std::time::Instant::now();
    let stream = tokio::net::UnixStream::connect(path)
        .await
        .wrap_err_with(|| format!("connect to {}", path.display()))?;
    let t_socket = t0.elapsed();
    let link = vox_stream::StreamLink::unix(stream);

    // vox 0.10 lane model: establish the connection, then open the
    // session service lane, yielding a ready-to-use `Caller`. The
    // connection's run loop is spawned internally by
    // `establish_connection`, so dropping the handle here does not tear
    // the session down — the lane's `Caller` keeps it alive.
    let t1 = std::time::Instant::now();
    let connection = vox::initiator_on(link)
        .establish_connection()
        .await
        .map_err(|e| eyre!("vox handshake failed: {e:?}"))?;
    let t_establish = t1.elapsed();

    let t2 = std::time::Instant::now();
    let client = connection
        .open_lane::<SessionLaneClient>()
        .await
        .map_err(|e| eyre!("open_lane failed: {e:?}"))?;
    let t_open = t2.elapsed();
    drop(connection);

    // Trace-level so it stays out of normal output; enable with
    // `RUST_LOG=session_cli::connection=debug` (or `=trace`).
    tracing::debug!(
        socket_us = t_socket.as_micros() as u64,
        establish_us = t_establish.as_micros() as u64,
        open_us = t_open.as_micros() as u64,
        total_us = t0.elapsed().as_micros() as u64,
        "vox open_session timing"
    );
    Ok(client.caller)
}
