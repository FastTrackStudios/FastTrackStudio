//! Minimal cross-cutting helpers for the vertical-slice CLI.
//!
//! Holds [`RemoteVoxConfig`] — endpoint resolution — and
//! [`LiveSession`] — a synced local `CrdtDoc` over `WorkspaceSync`.

use std::sync::Arc;

use crdt::CrdtDoc;
use futures::channel::mpsc::{UnboundedReceiver, unbounded};
use project_proto::{UpdateBytes, WorkspaceSyncClient};

#[derive(Debug, Clone)]
pub(crate) struct RemoteVoxConfig {
    pub(crate) vox_url: String,
    pub(crate) display_url: String,
}

impl RemoteVoxConfig {
    pub(crate) fn from_args(
        server: String,
        session_token: Option<String>,
        organization_id: Option<String>,
    ) -> eyre::Result<Self> {
        let base = normalize_vox_url(&server);
        let mut vox_url = base.clone();
        let mut display_url = base;
        if let Some(token) = session_token.as_deref().filter(|s| !s.is_empty()) {
            append_query_param(&mut vox_url, "token", token);
            append_query_param(&mut display_url, "token", "<redacted>");
        }
        if let Some(org) = organization_id.as_deref().filter(|s| !s.is_empty()) {
            append_query_param(&mut vox_url, "organization_id", org);
            append_query_param(&mut display_url, "organization_id", org);
        }
        Ok(Self {
            vox_url,
            display_url,
        })
    }

    /// Open a typed vox client by establishing a session against the
    /// configured endpoint. The concrete client type is inferred from
    /// the call site (e.g. `let c: TaskRepoClient = remote.connect().await?`).
    pub(crate) async fn connect<C>(&self) -> eyre::Result<C>
    where
        C: vox::FromVoxSession,
    {
        vox::connect(&self.vox_url)
            .establish()
            .await
            .map_err(|e| eyre::eyre!("Remote Vox connection failed: {e}"))
    }
}

/// A local `CrdtDoc` pre-loaded with the server's current snapshot,
/// plus an upload pipeline that ships any local mutation back to the
/// server via `WorkspaceSync::apply_update`.
///
/// Mirrors the web client (`project-ui::live`) so the CLI and the
/// browser route share one transport. After running a command, call
/// [`LiveSession::flush`] to push pending local updates before exit.
pub(crate) struct LiveSession {
    pub doc: Arc<CrdtDoc>,
    apply_client: WorkspaceSyncClient,
    upload_rx: UnboundedReceiver<Vec<u8>>,
}

impl LiveSession {
    pub(crate) async fn open(remote: &RemoteVoxConfig) -> eyre::Result<Self> {
        let doc = Arc::new(CrdtDoc::ephemeral());

        // Wire local-update capture before anything can mutate.
        let (upload_tx, upload_rx) = unbounded::<Vec<u8>>();
        let sub = doc.loro().subscribe_local_update(Box::new(move |bytes| {
            let _ = upload_tx.unbounded_send(bytes.to_vec());
            true
        }));
        // Subscription handle must outlive the doc. Process exits at
        // command end, so leaking matches the doc's lifetime.
        std::mem::forget(sub);

        // Two sessions: `subscribe` holds its session for the stream
        // lifetime, so `apply_update` needs its own.
        let apply_client: WorkspaceSyncClient = remote.connect().await?;
        let sub_client: WorkspaceSyncClient = remote.connect().await?;

        let (tx, mut rx) = vox::channel::<UpdateBytes>();
        tokio::spawn(async move {
            if let Err(e) = sub_client.subscribe(tx).await {
                tracing::debug!(?e, "subscribe stream ended");
            }
        });

        // Wait for the snapshot (the first message the server sends).
        let doc_for_load = doc.clone();
        match rx.recv().await {
            Ok(Some(msg)) => doc_for_load
                .apply_remote(&msg.get().0)
                .map_err(|e| eyre::eyre!("apply_remote (snapshot): {e}"))?,
            Ok(None) => return Err(eyre::eyre!("sync stream closed before snapshot")),
            Err(e) => return Err(eyre::eyre!("snapshot recv: {e:?}")),
        }
        // Drop rx — one-shot CLI commands don't follow the live stream.

        Ok(Self {
            doc,
            apply_client,
            upload_rx,
        })
    }

    /// Drain any locally-produced update bytes through the server's
    /// `apply_update` RPC. Loro's local-update callback fires
    /// synchronously on commit, so by the time the caller's
    /// mutations have `.await`-ed, every byte chunk is already in
    /// the queue.
    pub(crate) async fn flush(mut self) -> eyre::Result<()> {
        loop {
            match self.upload_rx.try_recv() {
                Ok(bytes) => {
                    self.apply_client
                        .apply_update(UpdateBytes(bytes))
                        .await
                        .map_err(|e| eyre::eyre!("apply_update: {e}"))?;
                }
                Err(_) => return Ok(()),
            }
        }
    }
}

fn normalize_vox_url(server: &str) -> String {
    let trimmed = server.trim().trim_end_matches('/');
    if trimmed.starts_with("ws://") || trimmed.starts_with("wss://") {
        trimmed.to_string()
    } else if let Some(rest) = trimmed.strip_prefix("https://") {
        format!("wss://{}/vox", rest.trim_end_matches("/vox"))
    } else if let Some(rest) = trimmed.strip_prefix("http://") {
        format!("ws://{}/vox", rest.trim_end_matches("/vox"))
    } else {
        format!("ws://{}/vox", trimmed.trim_end_matches("/vox"))
    }
}

fn append_query_param(url: &mut String, key: &str, value: &str) {
    let separator = if url.contains('?') { '&' } else { '?' };
    url.push(separator);
    url.push_str(key);
    url.push('=');
    url.push_str(&percent_encode_query_value(value));
}

fn percent_encode_query_value(value: &str) -> String {
    let mut out = String::new();
    for byte in value.bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'.' | b'_' | b'~' => {
                out.push(byte as char);
            }
            _ => out.push_str(&format!("%{byte:02X}")),
        }
    }
    out
}
