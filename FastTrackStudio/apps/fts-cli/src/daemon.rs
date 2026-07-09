//! `ftsd` — long-lived process that holds the Vox connection so
//! one-shot `fts` commands don't have to pay the 870 ms cranelift JIT
//! cost on every invocation.
//!
//! # Why
//!
//! Vox's `BareConduit::new` JIT-compiles encoders/decoders for the
//! entire MessageFamily on first use (~1.2M cranelift trace lines,
//! ~870 ms wall on this machine). That cost is amortized across the
//! lifetime of the connection — for the TUI it pays off in
//! microseconds; for a one-shot CLI it dominates everything.
//!
//! The daemon pays it once at startup; CLI clients talk to the
//! daemon over a Unix socket using a tiny facet/postcard protocol
//! that *never imports vox-jit*, so they stay in the ~10 ms
//! cold-start range.
//!
//! # Protocol
//!
//! Length-prefixed framing: `[u32 LE length] [postcard payload]`.
//! Postcard is a compact binary encoding; facet drives serialization
//! reflectively (no JIT). Each side reads frames one at a time, so
//! the protocol is implicitly request/response.
//!
//! Adding a command means one variant on each of [`Request`] and
//! [`Response`] plus a match arm in [`dispatch`]. No codegen, no
//! schema files.

use std::path::PathBuf;

use eyre::{Result, WrapErr, eyre};
use facet::Facet;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{UnixListener, UnixStream};

// ── Protocol ────────────────────────────────────────────────────────

/// Daemon-side command set. New variants must also be handled in
/// [`dispatch`] — `match` exhaustiveness will scream if you forget.
#[derive(Debug, Facet)]
#[repr(u8)]
pub enum Request {
    /// Liveness check — daemon responds with [`Response::Pong`].
    Ping,
    /// `SessionModeService::current_mode` → `Response::Value`.
    ModeGet,
    /// `SessionModeService::set_mode(slug)` → `Response::Ok` or `Err`.
    ModeSet(String),
    /// `SessionModeService::list_modes` → `Response::List`.
    ModeList,
    /// `Transport::play_pause(Current)` → `Response::Ok` or `Err`.
    PlayPause,
    /// `Transport::pause(Current)`.
    Pause,
    /// `Transport::stop(Current)`.
    Stop,
    /// Ask the daemon to shut down gracefully. Acks with `Ok` and
    /// then closes the listener.
    Shutdown,
}

/// Daemon responses. `Err(String)` carries human-readable detail so
/// the CLI can surface it without needing typed error mappings.
///
/// The payload fields are only read via the `Debug` derive (for
/// diagnostic logging), which `dead_code` doesn't count as a real use.
#[derive(Debug, Facet)]
#[repr(u8)]
#[allow(dead_code)]
pub enum Response {
    Pong,
    Ok,
    Value(String),
    List(Vec<String>),
    Err(String),
}

// ── Socket discovery ────────────────────────────────────────────────

/// Default daemon socket: `$XDG_RUNTIME_DIR/fts-daemon.sock`, falling
/// back to `/tmp/fts-daemon-{uid}.sock`. One daemon per user.
pub fn default_socket_path() -> PathBuf {
    if let Some(rt) = std::env::var_os("XDG_RUNTIME_DIR") {
        let mut p = PathBuf::from(rt);
        p.push("fts-daemon.sock");
        return p;
    }
    // SAFETY: getuid() is signal-safe and infallible. Falls back to
    // a stable per-uid path under /tmp.
    let uid = unsafe { libc::getuid() };
    PathBuf::from(format!("/tmp/fts-daemon-{uid}.sock"))
}

// ── Framing ─────────────────────────────────────────────────────────

/// Read one length-prefixed postcard frame and deserialize to `T`.
/// Frames are capped at 1 MiB to bound memory if the peer goes wild.
pub async fn read_frame<T>(stream: &mut UnixStream) -> Result<T>
where
    T: for<'a> Facet<'a>,
{
    let mut len_buf = [0u8; 4];
    stream
        .read_exact(&mut len_buf)
        .await
        .wrap_err("read frame length")?;
    let len = u32::from_le_bytes(len_buf) as usize;
    if len > 1 << 20 {
        return Err(eyre!("daemon frame too large: {len} bytes"));
    }
    let mut buf = vec![0u8; len];
    stream
        .read_exact(&mut buf)
        .await
        .wrap_err("read frame body")?;
    facet_postcard::from_slice::<T>(&buf).map_err(|e| eyre!("decode frame: {e:?}"))
}

/// Encode `msg` as postcard, prefix with u32 LE length, write all.
pub async fn write_frame<T>(stream: &mut UnixStream, msg: &T) -> Result<()>
where
    T: Facet<'static>,
{
    let body = facet_postcard::to_vec(msg).map_err(|e| eyre!("encode frame: {e:?}"))?;
    let len = body.len() as u32;
    stream
        .write_all(&len.to_le_bytes())
        .await
        .wrap_err("write frame length")?;
    stream.write_all(&body).await.wrap_err("write frame body")?;
    stream.flush().await.wrap_err("flush frame")?;
    Ok(())
}

// ── Client side ─────────────────────────────────────────────────────

/// Try one round-trip to the daemon. Returns `Ok(None)` when no
/// daemon is reachable (no socket file, or refused connection) so
/// the caller can fall back to a direct Vox connection. Reserves
/// `Err` for actual protocol failures the user should see.
pub async fn try_call(request: Request) -> Result<Option<Response>> {
    let socket = default_socket_path();
    if !socket.exists() {
        return Ok(None);
    }
    let mut stream = match UnixStream::connect(&socket).await {
        Ok(s) => s,
        Err(_) => return Ok(None),
    };
    write_frame(&mut stream, &request)
        .await
        .wrap_err("send daemon request")?;
    let resp: Response = read_frame(&mut stream)
        .await
        .wrap_err("read daemon response")?;
    Ok(Some(resp))
}

// ── Server side ─────────────────────────────────────────────────────

/// Run the daemon: bind the socket, open a Vox connection to the
/// running fts-extensions, accept clients forever (each in its own
/// task). The Vox `Caller` is cloned per client; vox itself
/// multiplexes the in-flight requests on one socket.
pub async fn serve(socket_path: PathBuf, vox_socket: Option<PathBuf>) -> Result<()> {
    // Bind first so other invocations see the socket even before we
    // finish connecting to fts-extensions (some clients only ping).
    let _ = std::fs::remove_file(&socket_path);
    let listener = UnixListener::bind(&socket_path)
        .wrap_err_with(|| format!("bind daemon socket at {}", socket_path.display()))?;
    tracing::info!(socket = %socket_path.display(), "ftsd listening");

    let caller = session_cli::connection::connect(vox_socket.as_deref())
        .await
        .wrap_err("daemon connect to fts-extensions")?;
    tracing::info!("ftsd connected to fts-extensions; ready");

    // Shutdown is cooperative: a client's `Shutdown` request flips
    // this and the accept loop exits after the in-flight tasks
    // drain. A simple atomic is enough since we never race on it
    // outside this loop.
    let shutdown = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));

    loop {
        if shutdown.load(std::sync::atomic::Ordering::Acquire) {
            break;
        }
        // accept() awaits without blocking the shutdown flag; we
        // check the flag again after each accept to break promptly.
        let (stream, _) = match listener.accept().await {
            Ok(pair) => pair,
            Err(e) => {
                tracing::warn!(error = %e, "ftsd accept failed");
                continue;
            }
        };
        let caller = caller.clone();
        let shutdown = shutdown.clone();
        tokio::spawn(async move {
            if let Err(e) = handle_client(stream, caller, shutdown).await {
                tracing::debug!(error = ?e, "ftsd client disconnected");
            }
        });
    }
    let _ = std::fs::remove_file(&socket_path);
    Ok(())
}

async fn handle_client(
    mut stream: UnixStream,
    caller: vox::Caller,
    shutdown: std::sync::Arc<std::sync::atomic::AtomicBool>,
) -> Result<()> {
    loop {
        let req: Request = read_frame(&mut stream).await?;
        let is_shutdown = matches!(req, Request::Shutdown);
        let resp = dispatch(req, &caller).await;
        write_frame(&mut stream, &resp).await?;
        if is_shutdown {
            shutdown.store(true, std::sync::atomic::Ordering::Release);
            return Ok(());
        }
    }
}

async fn dispatch(req: Request, caller: &vox::Caller) -> Response {
    use daw_proto::ProjectContext;
    use daw_proto::transport::TransportClient;
    use session_proto::services::SessionModeServiceClient;

    match req {
        Request::Ping => Response::Pong,
        Request::Shutdown => Response::Ok,
        Request::ModeGet => {
            let client = SessionModeServiceClient::new(caller.clone());
            match client.current_mode().await {
                Ok(slug) => Response::Value(slug),
                Err(e) => Response::Err(format!("mode get: {e:?}")),
            }
        }
        Request::ModeSet(slug) => {
            let client = SessionModeServiceClient::new(caller.clone());
            match client.set_mode(slug).await {
                Ok(()) => Response::Ok,
                Err(e) => Response::Err(format!("mode set: {e:?}")),
            }
        }
        Request::ModeList => {
            let client = SessionModeServiceClient::new(caller.clone());
            match client.list_modes().await {
                Ok(list) => Response::List(list),
                Err(e) => Response::Err(format!("mode list: {e:?}")),
            }
        }
        Request::PlayPause => {
            let client = TransportClient::new(caller.clone());
            map_daw(client.play_pause(ProjectContext::Current).await)
        }
        Request::Pause => {
            let client = TransportClient::new(caller.clone());
            map_daw(client.pause(ProjectContext::Current).await)
        }
        Request::Stop => {
            let client = TransportClient::new(caller.clone());
            map_daw(client.stop(ProjectContext::Current).await)
        }
    }
}

/// Collapse the two-level `Result<DawResult<()>, VoxError>` returned
/// by every architect::rpc client method into a flat `Response`.
fn map_daw<R, E1: std::fmt::Debug, E2: std::fmt::Debug>(
    res: Result<Result<R, E1>, E2>,
) -> Response {
    match res {
        Ok(Ok(_)) => Response::Ok,
        Ok(Err(e)) => Response::Err(format!("daw error: {e:?}")),
        Err(e) => Response::Err(format!("rpc error: {e:?}")),
    }
}
