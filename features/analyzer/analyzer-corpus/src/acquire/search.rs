//! Candidate lookup through YouTube Music's search endpoint.
//!
//! Acquisition is bounded by a per-IP **request quota**, so the number
//! of requests spent per song decides how much of the corpus lands
//! before everything stops for a couple of hours. Resolution used to
//! dominate that budget: driving the search through yt-dlp costs a
//! search page plus one full metadata extraction per candidate, about
//! six requests, all of it spent merely choosing which upload to take.
//!
//! The search endpoint answers with twenty fully-described results in
//! **one** request. Fewer requests and more candidates, which is the
//! rare change that makes both quota and match quality better.
//!
//! It is reached through a small Python helper (`resolver.py`) because
//! the endpoint's request signing and deeply-nested response shape are
//! maintained by `ytmusicapi`, and reimplementing that here would be a
//! standing liability for no gain. The helper is a long-running process
//! speaking JSON lines, so interpreter startup and the API handshake
//! are paid once rather than per song.

use std::path::PathBuf;
use std::process::Stdio;

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::{Child, ChildStdin, ChildStdout, Command};
use tokio::sync::Mutex;

use super::score::Candidate;

#[derive(Serialize)]
struct Request<'a> {
    id: u64,
    query: &'a str,
}

#[derive(Deserialize)]
struct Response {
    #[allow(dead_code)]
    id: Option<u64>,
    #[serde(default)]
    candidates: Vec<Candidate>,
    #[serde(default)]
    error: Option<String>,
}

/// A running resolver process.
pub struct MusicSearch {
    io: Mutex<Io>,
    next_id: std::sync::atomic::AtomicU64,
}

struct Io {
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
    // Held so the process is killed when the search is dropped.
    _child: Child,
}

impl MusicSearch {
    /// Start the helper.
    ///
    /// `limit` is how many candidates each search returns. It costs
    /// nothing extra — they arrive in the same single response — so it
    /// is set generously; more candidates only improves the odds the
    /// real master is among them.
    pub async fn spawn(limit: usize) -> Result<MusicSearch> {
        let script = Self::script_path();
        anyhow::ensure!(
            script.is_file(),
            "resolver helper not found at {} (set $CORPUS_RESOLVER to override)",
            script.display()
        );

        let mut child = Command::new("uv")
            .args(["run", "--quiet", "--with", "ytmusicapi", "python"])
            .arg(&script)
            .arg(limit.to_string())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .context("spawning the resolver helper (is `uv` on PATH?)")?;

        let stdin = child.stdin.take().context("resolver stdin")?;
        let stdout = BufReader::new(child.stdout.take().context("resolver stdout")?);

        Ok(MusicSearch {
            io: Mutex::new(Io {
                stdin,
                stdout,
                _child: child,
            }),
            next_id: std::sync::atomic::AtomicU64::new(1),
        })
    }

    fn script_path() -> PathBuf {
        std::env::var("CORPUS_RESOLVER")
            .map(PathBuf::from)
            .unwrap_or_else(|_| {
                PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("resolver.py")
            })
    }

    /// Look up candidates for one query.
    ///
    /// Serialised, because the protocol is one request and one response
    /// over a single pipe. That is not a bottleneck: a search takes well
    /// under a second, so this sustains far more songs per minute than
    /// downloading them ever will.
    pub async fn resolve(&self, query: &str) -> Result<Vec<Candidate>> {
        let id = self
            .next_id
            .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        let mut line = serde_json::to_string(&Request { id, query })?;
        line.push('\n');

        let mut io = self.io.lock().await;
        io.stdin
            .write_all(line.as_bytes())
            .await
            .context("writing to the resolver")?;
        io.stdin.flush().await.context("flushing to the resolver")?;

        let mut buf = String::new();
        let n = io
            .stdout
            .read_line(&mut buf)
            .await
            .context("reading from the resolver")?;
        if n == 0 {
            bail!("resolver exited");
        }

        let resp: Response =
            serde_json::from_str(buf.trim()).context("parsing the resolver's reply")?;
        if let Some(e) = resp.error {
            bail!("resolver: {e}");
        }
        Ok(resp.candidates)
    }
}
