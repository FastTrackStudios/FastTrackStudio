//! Phase-2 vault-sync client.
//!
//! Wraps the four task-server endpoints (`manifest`, `file
//! GET/PUT/DELETE`) and the `subscribe` WebSocket. Mirrors
//! `apps/server/src/vault_sync.rs` on the wire format —
//! `VaultEvent` and `ManifestEntry` are intentionally
//! duplicated rather than shared via a `*-proto` crate, since
//! the wire shape is small and stable and a shared crate would
//! drag the server's deps into the wasm web build later.
//!
//! ## Conflict semantics
//! Every `put_file` call requires an `IfMatch` value:
//! - `IfMatch::CreateOnly` (`*`) — fail with `Error::Conflict`
//!   if the server already has this path.
//! - `IfMatch::Sha(_)` — fail with `Error::Conflict` if the
//!   server's current sha doesn't match.
//! - `IfMatch::Force` — unconditional overwrite. Only safe for
//!   the very first push of a brand-new vault; everyday writes
//!   should always pin a sha.
//!
//! On conflict, the error carries the server's current bytes
//! and sha so callers can run a 3-way merge without an extra
//! round trip.

use std::time::Duration;

pub use bytes::{self, Bytes};
use futures_util::{SinkExt, StreamExt};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use tokio::net::TcpStream;
use tokio_tungstenite::{MaybeTlsStream, WebSocketStream, tungstenite::Message};

/// One entry in `GET /vault/{id}/manifest`.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct ManifestEntry {
    pub path: String,
    pub sha256: String,
    pub mtime_ms: i64,
    pub size: u64,
}

/// Full manifest response. `vault_id` echoes the path the
/// caller used so a client juggling multiple vaults can
/// double-check what came back.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct Manifest {
    pub vault_id: String,
    pub files: Vec<ManifestEntry>,
}

/// Live event from `GET /vault/{id}/subscribe`. Matches the
/// server's `vault_sync::VaultEvent` shape verbatim.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(tag = "op", rename_all = "snake_case")]
pub enum VaultEvent {
    Put {
        path: String,
        sha256: String,
        mtime_ms: i64,
        size: u64,
    },
    Delete {
        path: String,
    },
    /// Server hint after a `Lagged` broadcast — re-pull the
    /// manifest to catch missed events.
    Resync,
}

/// Conditional-write modes for `put_file` / `delete_file`.
#[derive(Clone, Debug)]
pub enum IfMatch {
    CreateOnly,
    Sha(String),
    Force,
}

impl IfMatch {
    fn header_value(&self) -> Option<&str> {
        match self {
            IfMatch::CreateOnly => Some("*"),
            IfMatch::Sha(s) => Some(s.as_str()),
            IfMatch::Force => None,
        }
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("http: {0}")]
    Http(#[from] reqwest::Error),
    #[error("ws: {0}")]
    Ws(#[from] tokio_tungstenite::tungstenite::Error),
    #[error("url: {0}")]
    Url(#[from] url::ParseError),
    #[error("json: {0}")]
    Json(#[from] serde_json::Error),
    #[error("not found")]
    NotFound,
    #[error("conflict (server sha {server_sha})")]
    Conflict {
        server_sha: String,
        server_bytes: Bytes,
    },
    #[error("server error {status}: {body}")]
    Server { status: u16, body: String },
}

/// Client for a single (base_url, vault_id) pair. Cheap to
/// clone — the underlying `reqwest::Client` pools connections.
#[derive(Clone, Debug)]
pub struct VaultClient {
    base: url::Url,
    vault_id: String,
    http: reqwest::Client,
}

impl VaultClient {
    /// `base_url` is the task-server origin, e.g.
    /// `http://localhost:4000`. `vault_id` is the path
    /// component used by every route (the server treats it as
    /// an opaque directory name under its root).
    pub fn new(base_url: &str, vault_id: impl Into<String>) -> Result<Self, Error> {
        let base = url::Url::parse(base_url)?;
        let http = reqwest::Client::builder()
            .timeout(Duration::from_secs(30))
            .build()?;
        Ok(Self {
            base,
            vault_id: vault_id.into(),
            http,
        })
    }

    pub fn vault_id(&self) -> &str {
        &self.vault_id
    }

    pub fn base_url(&self) -> &url::Url {
        &self.base
    }

    fn url(&self, suffix: &str) -> Result<url::Url, Error> {
        Ok(self
            .base
            .join(&format!("/vault/{}{}", self.vault_id, suffix))?)
    }

    /// Pull the full manifest. The file list is unordered.
    pub async fn manifest(&self) -> Result<Manifest, Error> {
        let url = self.url("/manifest")?;
        let resp = self.http.get(url).send().await?;
        let status = resp.status();
        if !status.is_success() {
            let body = resp.text().await.unwrap_or_default();
            return Err(Error::Server {
                status: status.as_u16(),
                body,
            });
        }
        Ok(resp.json().await?)
    }

    /// Fetch one file. Returns `Error::NotFound` if absent.
    pub async fn get_file(&self, rel_path: &str) -> Result<Bytes, Error> {
        let url = self.url(&format!("/file/{}", rel_path))?;
        let resp = self.http.get(url).send().await?;
        match resp.status().as_u16() {
            200 => Ok(resp.bytes().await?),
            404 => Err(Error::NotFound),
            other => Err(Error::Server {
                status: other,
                body: resp.text().await.unwrap_or_default(),
            }),
        }
    }

    /// Push one file. On 412 returns `Error::Conflict` with
    /// the server's current bytes and sha — no second request
    /// needed to fetch them.
    pub async fn put_file(
        &self,
        rel_path: &str,
        body: Bytes,
        if_match: IfMatch,
    ) -> Result<String, Error> {
        let url = self.url(&format!("/file/{}", rel_path))?;
        let mut req = self.http.put(url).body(body);
        if let Some(v) = if_match.header_value() {
            req = req.header("If-Match", v);
        }
        let resp = req.send().await?;
        match resp.status().as_u16() {
            200 => {
                let sha = resp
                    .headers()
                    .get("X-Vault-Sha256")
                    .and_then(|h| h.to_str().ok())
                    .map(str::to_string);
                let text = resp.text().await?;
                Ok(sha.unwrap_or(text))
            }
            412 => {
                let server_sha = resp
                    .headers()
                    .get("X-Vault-Sha256")
                    .and_then(|h| h.to_str().ok())
                    .unwrap_or("")
                    .to_string();
                let bytes = resp.bytes().await.unwrap_or_default();
                Err(Error::Conflict {
                    server_sha,
                    server_bytes: bytes,
                })
            }
            other => Err(Error::Server {
                status: other,
                body: resp.text().await.unwrap_or_default(),
            }),
        }
    }

    /// Delete one file. `IfMatch::Sha(_)` is honored; the
    /// other variants behave as on PUT (`CreateOnly` is
    /// nonsensical here and the server simply ignores it for
    /// DELETE).
    pub async fn delete_file(&self, rel_path: &str, if_match: IfMatch) -> Result<(), Error> {
        let url = self.url(&format!("/file/{}", rel_path))?;
        let mut req = self.http.delete(url);
        if let Some(v) = if_match.header_value() {
            req = req.header("If-Match", v);
        }
        let resp = req.send().await?;
        match resp.status().as_u16() {
            200 | 404 => Ok(()),
            412 => {
                let server_sha = resp
                    .headers()
                    .get("X-Vault-Sha256")
                    .and_then(|h| h.to_str().ok())
                    .unwrap_or("")
                    .to_string();
                let bytes = resp.bytes().await.unwrap_or_default();
                Err(Error::Conflict {
                    server_sha,
                    server_bytes: bytes,
                })
            }
            other => Err(Error::Server {
                status: other,
                body: resp.text().await.unwrap_or_default(),
            }),
        }
    }

    /// Open the live-events WebSocket. Rewrites `http→ws` and
    /// `https→wss`. The returned `Subscription` is a `Stream`
    /// of `VaultEvent`s; drop it to close the socket.
    pub async fn subscribe(&self) -> Result<Subscription, Error> {
        let mut url = self.url("/subscribe")?;
        let scheme = match url.scheme() {
            "http" => "ws",
            "https" => "wss",
            other => other,
        }
        .to_string();
        url.set_scheme(&scheme).map_err(|_| Error::Server {
            status: 0,
            body: "bad scheme".into(),
        })?;
        let (socket, _resp) = tokio_tungstenite::connect_async(url.as_str()).await?;
        Ok(Subscription { socket })
    }
}

/// Live WebSocket subscription. Wraps the underlying
/// tungstenite stream and decodes each text frame as a
/// [`VaultEvent`]. Binary / ping / pong frames pass through
/// silently; the server only ever sends text.
pub struct Subscription {
    socket: WebSocketStream<MaybeTlsStream<TcpStream>>,
}

impl Subscription {
    /// Wait for the next event. Returns `Ok(None)` when the
    /// server closes the socket cleanly.
    pub async fn next_event(&mut self) -> Result<Option<VaultEvent>, Error> {
        while let Some(msg) = self.socket.next().await {
            match msg? {
                Message::Text(text) => {
                    let evt: VaultEvent = serde_json::from_str(&text)?;
                    return Ok(Some(evt));
                }
                Message::Close(_) => return Ok(None),
                Message::Ping(p) => {
                    let _ = self.socket.send(Message::Pong(p)).await;
                }
                // Binary / Pong / Frame — ignore.
                _ => continue,
            }
        }
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_put_event() {
        let s = r#"{"op":"put","path":"a.md","sha256":"abc","mtime_ms":1,"size":2}"#;
        let evt: VaultEvent = serde_json::from_str(s).unwrap();
        assert_eq!(
            evt,
            VaultEvent::Put {
                path: "a.md".into(),
                sha256: "abc".into(),
                mtime_ms: 1,
                size: 2
            }
        );
    }

    #[test]
    fn parses_delete_event() {
        let s = r#"{"op":"delete","path":"gone.md"}"#;
        let evt: VaultEvent = serde_json::from_str(s).unwrap();
        assert_eq!(
            evt,
            VaultEvent::Delete {
                path: "gone.md".into()
            }
        );
    }

    #[test]
    fn parses_resync_event() {
        let s = r#"{"op":"resync"}"#;
        let evt: VaultEvent = serde_json::from_str(s).unwrap();
        assert_eq!(evt, VaultEvent::Resync);
    }

    #[test]
    fn url_shaping() {
        let c = VaultClient::new("http://localhost:4000", "v1").unwrap();
        assert_eq!(
            c.url("/manifest").unwrap().as_str(),
            "http://localhost:4000/vault/v1/manifest"
        );
        assert_eq!(
            c.url("/file/notes/a.md").unwrap().as_str(),
            "http://localhost:4000/vault/v1/file/notes/a.md"
        );
    }

    #[test]
    fn if_match_header_values() {
        assert_eq!(IfMatch::CreateOnly.header_value(), Some("*"));
        assert_eq!(IfMatch::Sha("abc".into()).header_value(), Some("abc"));
        assert_eq!(IfMatch::Force.header_value(), None);
    }
}
