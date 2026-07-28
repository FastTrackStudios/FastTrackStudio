//! `EmailSync` impl backed by an IMAP server via `async-imap`.
//! Connections are opened on demand per call; a future pass
//! pools them. Folder aliases (`email-config::FolderAliases`)
//! are honored at the wire boundary, same shape as
//! `email-maildir::Backend`.

use std::collections::HashMap;
use std::sync::Arc;

use architect::vox;
use email_config::{BackendKind, FolderAliases, SmtpConfig, TlsMode};
use email_proto::{
    Account, Draft, EmailEvent, EmailSync, EmailSyncError, Envelope, FlagDelta, Folder, Message,
    SeqRange,
};
use futures::StreamExt;
use tokio::sync::{Mutex, RwLock, broadcast};

use crate::connect::{self, ConnectError, ImapSession};
use crate::parse;

/// One configured IMAP account. Holds the connection
/// parameters + folder alias map; the actual session is
/// opened per-op until we add pooling.
struct AccountState {
    account: Account,
    host: String,
    port: u16,
    tls: TlsMode,
    username: String,
    password: email_secret::Secret,
    aliases: FolderAliases,
    smtp: Option<SmtpConfig>,
}

/// IMAP backend. Cheap to `Clone` — all internals are `Arc`'d.
#[derive(Clone, architect::HasDispatcher)]
pub struct Backend {
    accounts: Arc<HashMap<String, AccountState>>,
    /// Per-account broadcast sender, lazily created on first
    /// `subscribe`. Same shape as `vault::sync::Backend` +
    /// `email-maildir::Backend`.
    channels: Arc<RwLock<HashMap<String, broadcast::Sender<EmailEvent>>>>,
    /// Fan-out hub behind the `#[subscribe] fn changes` stream.
    /// Every event that goes onto a per-account broadcast channel
    /// is published here too, wrapped with its `account` so
    /// subscribers — who see every account this backend serves —
    /// can filter. Sliding mailbox: a slow subscriber loses its
    /// oldest queued events and re-pulls on reconnect, which is
    /// what `EmailEvent::Resync` asks for anyway.
    changes: architect::PubSub<email_proto::EmailChange>,
    /// Tokio runtime needed inside the sync `EmailSync` methods.
    /// We use `block_on` via `TokioBlockingDispatcher`; this
    /// handle gives us access to the same runtime the backend
    /// was built on.
    runtime: tokio::runtime::Handle,
    /// Coarse per-account session lock. IMAP sessions are
    /// single-stream; serialize ops until we add a pool.
    locks: Arc<RwLock<HashMap<String, Arc<Mutex<()>>>>>,
}

impl Backend {
    /// Build a backend from one or more
    /// [`email_config::AccountConfig`] entries. Skips configs
    /// whose `BackendKind` isn't `Imap`. The current tokio
    /// runtime handle is captured at build time and reused for
    /// every blocking call.
    pub fn from_configs<I>(configs: I) -> Result<Self, &'static str>
    where
        I: IntoIterator<Item = email_config::AccountConfig>,
    {
        let runtime = tokio::runtime::Handle::try_current()
            .map_err(|_| "Backend::from_configs must be called from a tokio runtime")?;

        let mut accounts = HashMap::new();
        for cfg in configs {
            let BackendKind::Imap {
                host,
                port,
                tls,
                username,
                password,
                submit,
            } = cfg.backend.clone()
            else {
                continue;
            };
            let account = cfg.to_account();
            accounts.insert(
                account.id.0.clone(),
                AccountState {
                    account,
                    host,
                    port,
                    tls,
                    username,
                    password,
                    aliases: cfg.folder_aliases.clone(),
                    smtp: submit,
                },
            );
        }

        Ok(Self {
            accounts: Arc::new(accounts),
            channels: Arc::new(RwLock::new(HashMap::new())),
            changes: architect::PubSub::sliding(256),
            runtime,
            locks: Arc::new(RwLock::new(HashMap::new())),
        })
    }

    fn state(&self, account: &str) -> Result<&AccountState, EmailSyncError> {
        self.accounts
            .get(account)
            .ok_or(EmailSyncError::UnknownAccount)
    }

    async fn account_lock(&self, account: &str) -> Arc<Mutex<()>> {
        if let Some(l) = self.locks.read().await.get(account) {
            return l.clone();
        }
        let mut w = self.locks.write().await;
        w.entry(account.to_string())
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone()
    }

    /// Per-account broadcast sender for live events.
    pub async fn channel(&self, account: &str) -> broadcast::Sender<EmailEvent> {
        if let Some(tx) = self.channels.read().await.get(account) {
            return tx.clone();
        }
        let mut chans = self.channels.write().await;
        if let Some(tx) = chans.get(account) {
            return tx.clone();
        }
        let (tx, _rx) = broadcast::channel::<EmailEvent>(256);
        chans.insert(account.to_string(), tx.clone());
        tx
    }

    /// Announce a change on both paths: `account`'s in-process
    /// broadcast channel and the wire hub. Call only once the
    /// mailbox actually changed — subscribers re-read on the event.
    pub async fn emit(&self, account: &str, event: EmailEvent) {
        let _ = self.channel(account).await.send(event.clone());
        self.changes.publish(email_proto::EmailChange {
            account: account.to_string(),
            event,
        });
    }

    /// Start a long-lived IDLE loop on `folder` (alias name).
    /// Returns a `JoinHandle` callers (typically `email-sync`)
    /// can abort to stop the loop. Server responses break IDLE
    /// every ~28 minutes (under the RFC 2177 30-minute cap) so
    /// the session never goes stale; on each break we emit
    /// `EmailEvent::Resync` — on the per-account broadcast AND
    /// the wire hub the `changes` stream serves — and re-enter
    /// IDLE.
    ///
    /// Emitting `Resync` instead of fine-grained events is
    /// intentional for phase 1 — `email-sync`'s next poll cycle
    /// will pick up the actual deltas. A future pass will parse
    /// IDLE's untagged EXISTS / EXPUNGE / FETCH responses and
    /// emit the matching `NewMessage` / `Deleted` /
    /// `FlagsChanged` events directly.
    pub async fn start_idle(
        &self,
        account: &str,
        folder: &str,
    ) -> Result<tokio::task::JoinHandle<()>, EmailSyncError> {
        let state = self.state(account)?;
        let resolved = state.aliases.resolve(folder).to_string();
        let backend = self.clone();
        let account = account.to_string();
        let handle = tokio::spawn(async move {
            backend.idle_loop(account, resolved).await;
        });
        Ok(handle)
    }

    /// Continuous IDLE driver. Reconnects on any error with a
    /// short backoff so a transient network blip doesn't kill
    /// the watcher.
    async fn idle_loop(self, account: String, folder: String) {
        const IDLE_TIMEOUT: std::time::Duration = std::time::Duration::from_secs(28 * 60);
        const RECONNECT_BACKOFF: std::time::Duration = std::time::Duration::from_secs(5);

        loop {
            let state = match self.state(&account) {
                Ok(s) => s,
                Err(_) => return,
            };
            let session = match self.open(state).await {
                Ok(s) => s,
                Err(err) => {
                    tracing::warn!(%err, "idle: open failed, backing off");
                    tokio::time::sleep(RECONNECT_BACKOFF).await;
                    continue;
                }
            };
            // The whole IDLE cycle takes a separate borrow of
            // the session, so wrap it in a block + reassign.
            let session_result = run_idle_cycle(session, &folder, IDLE_TIMEOUT).await;
            match session_result {
                Ok(()) => {
                    // Break of IDLE = server told us something
                    // changed (or the timeout fired). Either way
                    // the safe answer is `Resync` — let
                    // `email-sync` (in-process) and every wire
                    // subscriber re-pull deltas. Keep idling
                    // regardless of who is listening: the wire hub
                    // has no "no subscribers" signal, and a
                    // subscriber can attach at any time.
                    self.emit(&account, EmailEvent::Resync).await;
                }
                Err(err) => {
                    tracing::warn!(%err, "idle: cycle failed, backing off");
                    tokio::time::sleep(RECONNECT_BACKOFF).await;
                }
            }
        }
    }

    /// Open + login. Used inside every op for now; pooling
    /// lands in the IDLE pass.
    async fn open(&self, state: &AccountState) -> Result<ImapSession, EmailSyncError> {
        let password = state
            .password
            .resolve()
            .await
            .map_err(|_| EmailSyncError::Auth)?;
        connect::connect_and_login(
            &state.host,
            state.port,
            state.tls,
            &state.username,
            &password,
        )
        .await
        .map_err(map_connect_err)
    }

    /// Drive one operation. Each opens a fresh session,
    /// performs the op, then drops the session. Sufficient for
    /// phase 2; pooling lands next.
    async fn run_list_folders(&self, state: &AccountState) -> Result<Vec<Folder>, EmailSyncError> {
        let lock = self.account_lock(&state.account.id.0).await;
        let _g = lock.lock().await;
        let mut session = self.open(state).await?;
        let mut folders = Vec::new();
        let mut stream = session
            .list(Some(""), Some("*"))
            .await
            .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
        while let Some(item) = stream.next().await {
            let m = item.map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
            let backend_name = m.name().to_string();
            let delim = m.delimiter().unwrap_or("/").to_string();
            let role = infer_role(&backend_name);
            // Translate backend → UI (alias) before reporting.
            let ui_name = state
                .aliases
                .alias_for(&backend_name)
                .map_or_else(|| backend_name.clone(), str::to_string);
            folders.push(Folder {
                id: ui_name.clone(),
                name: ui_name,
                delimiter: delim,
                role,
                message_count: None,
                unread_count: None,
            });
        }
        drop(stream);
        let _ = session.logout().await;
        Ok(folders)
    }

    async fn run_fetch_envelopes(
        &self,
        state: &AccountState,
        folder: &str,
        range: SeqRange,
    ) -> Result<Vec<Envelope>, EmailSyncError> {
        // Translate UI/alias → backend name.
        let resolved = state.aliases.resolve(folder).to_string();

        let lock = self.account_lock(&state.account.id.0).await;
        let _g = lock.lock().await;
        let mut session = self.open(state).await?;
        let mailbox = session
            .select(&resolved)
            .await
            .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;

        let last_uid = mailbox.uid_next.unwrap_or(1).saturating_sub(1);
        let seq = match range {
            SeqRange::All => "1:*".to_string(),
            SeqRange::Recent(n) => {
                let start = last_uid.saturating_sub(n.saturating_sub(1));
                format!("{}:{}", start.max(1), last_uid.max(1))
            }
            SeqRange::Range { from, to } => format!("{}:{}", from.max(1), to.max(1)),
        };

        let mut envs = Vec::new();
        let mut stream = session
            .uid_fetch(&seq, "(UID FLAGS RFC822.SIZE BODY.PEEK[HEADER])")
            .await
            .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
        while let Some(item) = stream.next().await {
            let fetch = item.map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
            let header = fetch.header().unwrap_or(&[]).to_vec();
            let flags: Vec<String> = fetch.flags().map(|f| format!("{f:?}")).collect();
            let size = u64::from(fetch.size.unwrap_or(0));
            // IMAP UID isn't a Message-ID, but we use it as a
            // stable secondary key when the header lacks one.
            let uid_synth = fetch.uid.map(|u| format!("<uid-{u}@imap.local>"));
            match parse::envelope_from_bytes(&header, folder, flags, uid_synth, size) {
                Ok(env) => envs.push(env),
                Err(err) => tracing::warn!(error = %err, "envelope parse failed"),
            }
        }
        drop(stream);
        let _ = session.logout().await;
        envs.sort_by(|a, b| b.date_ms.cmp(&a.date_ms));
        Ok(envs)
    }

    async fn run_fetch_message(
        &self,
        state: &AccountState,
        message_id: &str,
    ) -> Result<Message, EmailSyncError> {
        // The proto identifies messages by RFC2822 Message-ID;
        // IMAP indexes by UID. Search per-mailbox until we
        // find one — `email-store` will short-circuit this via
        // its index later.
        let folders = self.run_list_folders(state).await?;
        for folder in folders {
            let backend_name = state.aliases.resolve(&folder.id).to_string();
            let lock = self.account_lock(&state.account.id.0).await;
            let _g = lock.lock().await;
            let mut session = self.open(state).await?;
            if session.select(&backend_name).await.is_err() {
                continue;
            }
            let needle = format!(
                "HEADER Message-ID \"{}\"",
                message_id.trim_matches(|c| c == '<' || c == '>')
            );
            let uids: Vec<u32> = if let Ok(u) = session.uid_search(&needle).await {
                u.into_iter().collect()
            } else {
                let _ = session.logout().await;
                continue;
            };
            if uids.is_empty() {
                let _ = session.logout().await;
                continue;
            }
            let uid = uids[0];
            let (body, flags, size) = {
                let mut stream = session
                    .uid_fetch(uid.to_string(), "(UID FLAGS RFC822.SIZE BODY.PEEK[])")
                    .await
                    .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
                let Some(item) = stream.next().await else {
                    drop(stream);
                    let _ = session.logout().await;
                    continue;
                };
                let fetch = item.map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
                let body = fetch.body().unwrap_or(&[]).to_vec();
                let flags: Vec<String> = fetch.flags().map(|f| format!("{f:?}")).collect();
                let size = u64::from(fetch.size.unwrap_or(0));
                (body, flags, size)
            };
            let _ = session.logout().await;
            return parse::message_from_bytes(&body, &folder.id, flags, size);
        }
        Err(EmailSyncError::NotFound)
    }

    /// Locate `(folder, uid)` for one Message-ID across every
    /// folder on the account. Returns the **backend** folder
    /// name (already alias-resolved) so callers can re-use it
    /// directly with `session.select`. O(folders) — the
    /// `email-store` index avoids this in steady state.
    async fn locate_uid(
        &self,
        state: &AccountState,
        message_id: &str,
    ) -> Result<(String, u32), EmailSyncError> {
        let folders = self.run_list_folders(state).await?;
        for folder in folders {
            let backend_name = state.aliases.resolve(&folder.id).to_string();
            let lock = self.account_lock(&state.account.id.0).await;
            let _g = lock.lock().await;
            let mut session = self.open(state).await?;
            if session.select(&backend_name).await.is_err() {
                let _ = session.logout().await;
                continue;
            }
            let needle = format!(
                "HEADER Message-ID \"{}\"",
                message_id.trim_matches(|c| c == '<' || c == '>')
            );
            let uids: Vec<u32> = if let Ok(u) = session.uid_search(&needle).await {
                u.into_iter().collect()
            } else {
                let _ = session.logout().await;
                continue;
            };
            let _ = session.logout().await;
            if let Some(uid) = uids.first() {
                return Ok((backend_name, *uid));
            }
        }
        Err(EmailSyncError::NotFound)
    }

    async fn run_set_flags(
        &self,
        state: &AccountState,
        message_id: &str,
        delta: FlagDelta,
    ) -> Result<(), EmailSyncError> {
        let (folder, uid) = self.locate_uid(state, message_id).await?;
        let lock = self.account_lock(&state.account.id.0).await;
        let _g = lock.lock().await;
        let mut session = self.open(state).await?;
        session
            .select(&folder)
            .await
            .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;

        let uid_seq = uid.to_string();
        if !delta.add.is_empty() {
            let flags = delta.add.join(" ");
            let cmd = format!("+FLAGS ({flags})");
            // `uid_store` returns a stream of updated FETCH
            // responses; drive it to completion + discard.
            let mut stream = session
                .uid_store(&uid_seq, &cmd)
                .await
                .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
            while stream.next().await.is_some() {}
        }
        if !delta.remove.is_empty() {
            let flags = delta.remove.join(" ");
            let cmd = format!("-FLAGS ({flags})");
            let mut stream = session
                .uid_store(&uid_seq, &cmd)
                .await
                .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
            while stream.next().await.is_some() {}
        }
        let _ = session.logout().await;
        Ok(())
    }

    async fn run_move_message(
        &self,
        state: &AccountState,
        message_id: &str,
        dest: &str,
    ) -> Result<(), EmailSyncError> {
        let (source_folder, uid) = self.locate_uid(state, message_id).await?;
        // Caller's `dest` is the alias/UI name; translate.
        let dest_backend = state.aliases.resolve(dest).to_string();
        let lock = self.account_lock(&state.account.id.0).await;
        let _g = lock.lock().await;
        let mut session = self.open(state).await?;
        session
            .select(&source_folder)
            .await
            .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;

        // Prefer UID MOVE (RFC 6851). async-imap's
        // `uid_mv` issues the command but doesn't gate on the
        // server advertising the MOVE capability — if the server
        // doesn't support it, we fall back to UID COPY + STORE
        // \Deleted + UID EXPUNGE.
        if let Err(err) = session.uid_mv(uid.to_string(), &dest_backend).await {
            tracing::debug!(?err, "UID MOVE failed, falling back to COPY+EXPUNGE");
            session
                .uid_copy(uid.to_string(), &dest_backend)
                .await
                .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
            {
                let s = session
                    .uid_store(uid.to_string(), "+FLAGS (\\Deleted)")
                    .await
                    .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
                let mut s = Box::pin(s);
                while s.next().await.is_some() {}
            }
            {
                let e = session
                    .uid_expunge(uid.to_string())
                    .await
                    .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
                let mut e = Box::pin(e);
                while e.next().await.is_some() {}
            }
        }
        let _ = session.logout().await;
        Ok(())
    }

    async fn run_delete_message(
        &self,
        state: &AccountState,
        message_id: &str,
    ) -> Result<(), EmailSyncError> {
        let (folder, uid) = self.locate_uid(state, message_id).await?;
        let lock = self.account_lock(&state.account.id.0).await;
        let _g = lock.lock().await;
        let mut session = self.open(state).await?;
        session
            .select(&folder)
            .await
            .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
        // Scope each `&mut session` borrow to drain its stream
        // before issuing the next command — async-imap streams
        // hold the session borrow, and `uid_expunge`'s stream is
        // not Unpin so we pin it on the heap.
        {
            let s = session
                .uid_store(uid.to_string(), "+FLAGS (\\Deleted)")
                .await
                .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
            let mut s = Box::pin(s);
            while s.next().await.is_some() {}
        }
        {
            let e = session
                .uid_expunge(uid.to_string())
                .await
                .map_err(|e| EmailSyncError::Protocol(e.to_string()))?;
            let mut e = Box::pin(e);
            while e.next().await.is_some() {}
        }
        let _ = session.logout().await;
        Ok(())
    }

    async fn run_append_draft(
        &self,
        state: &AccountState,
        draft: Draft,
    ) -> Result<String, EmailSyncError> {
        let (bytes, message_id) = email_smtp::build_message(&draft)
            .map_err(|e| EmailSyncError::Protocol(format!("draft build: {e}")))?;
        // Look up the Drafts folder via the alias map; fall
        // back to the literal name `Drafts` when unaliased.
        let drafts_folder = state.aliases.resolve("Drafts").to_string();

        let lock = self.account_lock(&state.account.id.0).await;
        let _g = lock.lock().await;
        let mut session = self.open(state).await?;
        session
            .append(&drafts_folder, Some("(\\Draft)"), None, &bytes)
            .await
            .map_err(|e| EmailSyncError::Protocol(format!("APPEND: {e}")))?;
        let _ = session.logout().await;
        Ok(message_id)
    }

    async fn run_send(&self, state: &AccountState, draft: Draft) -> Result<String, EmailSyncError> {
        let smtp = state.smtp.clone().ok_or_else(|| {
            EmailSyncError::Unsupported(
                "imap: send requires SmtpConfig on the account (submit field)".into(),
            )
        })?;
        let sender = email_smtp::SmtpSender::new(smtp);
        let message_id = sender
            .send(&draft)
            .await
            .map_err(|e| EmailSyncError::Protocol(format!("smtp: {e}")))?;

        // After a successful submit, append a sent copy to the
        // server's Sent folder. Best-effort — the message is
        // already on the wire; an APPEND failure shouldn't
        // surface as a send failure.
        let sent_folder = state.aliases.resolve("Sent").to_string();
        if let Ok((bytes, _)) = email_smtp::build_message(&draft) {
            let lock = self.account_lock(&state.account.id.0).await;
            let _g = lock.lock().await;
            match self.open(state).await {
                Ok(mut session) => {
                    let _ = session
                        .append(&sent_folder, Some("(\\Seen)"), None, &bytes)
                        .await;
                    let _ = session.logout().await;
                }
                Err(err) => {
                    tracing::warn!(?err, "append sent-copy: open failed");
                }
            }
        }

        Ok(message_id)
    }
}

/// Run one IDLE round on `session`: SELECT, IDLE for at most
/// `timeout`, then DONE. Drops the session when complete (the
/// caller opens a fresh one for each cycle so a stale TLS
/// connection doesn't accumulate).
async fn run_idle_cycle(
    mut session: ImapSession,
    folder: &str,
    timeout: std::time::Duration,
) -> Result<(), EmailSyncError> {
    session
        .select(folder)
        .await
        .map_err(|e| EmailSyncError::Protocol(format!("idle select: {e}")))?;
    let mut idle = session.idle();
    idle.init()
        .await
        .map_err(|e| EmailSyncError::Protocol(format!("idle init: {e}")))?;
    let (idle_wait, _interrupt) = idle.wait_with_timeout(timeout);
    // We discard the response detail — `idle_loop` translates
    // any break into `EmailEvent::Resync` and lets `email-sync`
    // re-pull the deltas. Parsing the untagged EXISTS / EXPUNGE
    // / FETCH details for fine-grained events is the next pass.
    let _ = idle_wait.await;
    let mut session = idle
        .done()
        .await
        .map_err(|e| EmailSyncError::Protocol(format!("idle done: {e}")))?;
    let _ = session.logout().await;
    Ok(())
}

impl EmailSync for Backend {
    fn accounts(&self) -> Result<Vec<Account>, EmailSyncError> {
        Ok(self.accounts.values().map(|s| s.account.clone()).collect())
    }

    fn list_folders(&self, account: &str) -> Result<Vec<Folder>, EmailSyncError> {
        let state = self.state(account)?;
        // The trait is sync; we hop into the runtime via
        // `block_on`. Same pattern as `vault::sync::Backend`'s
        // tokio bridge.
        let backend = self.clone();
        let account = state.account.id.0.clone();
        self.runtime.block_on(async move {
            let state = backend.state(&account)?;
            backend.run_list_folders(state).await
        })
    }

    fn fetch_envelopes(
        &self,
        account: &str,
        folder: &str,
        range: SeqRange,
    ) -> Result<Vec<Envelope>, EmailSyncError> {
        let backend = self.clone();
        let account = account.to_string();
        let folder = folder.to_string();
        self.runtime.block_on(async move {
            let state = backend.state(&account)?;
            backend.run_fetch_envelopes(state, &folder, range).await
        })
    }

    fn fetch_message(&self, account: &str, message_id: &str) -> Result<Message, EmailSyncError> {
        let backend = self.clone();
        let account = account.to_string();
        let message_id = message_id.to_string();
        self.runtime.block_on(async move {
            let state = backend.state(&account)?;
            backend.run_fetch_message(state, &message_id).await
        })
    }

    fn fetch_attachment(
        &self,
        account: &str,
        message_id: &str,
        part: &str,
    ) -> Result<Vec<u8>, EmailSyncError> {
        // Re-fetch the whole message, then descend the parsed
        // MIME structure. Wasteful but correct; the cache in
        // `email-store` lets us skip the second hit.
        let msg = self.fetch_message(account, message_id)?;
        let _ = msg; // body bytes aren't kept on Message; will
        // need a fetch path that returns raw rfc822.
        // For now require the cache layer.
        let _ = part;
        Err(EmailSyncError::Unsupported(
            "imap: fetch_attachment via direct call needs email-store cache (phase 5)".into(),
        ))
    }

    fn set_flags(
        &self,
        account: &str,
        message_id: &str,
        delta: FlagDelta,
    ) -> Result<(), EmailSyncError> {
        let backend = self.clone();
        let account = account.to_string();
        let message_id = message_id.to_string();
        self.runtime.block_on(async move {
            let state = backend.state(&account)?;
            backend.run_set_flags(state, &message_id, delta).await
        })
    }

    fn move_message(
        &self,
        account: &str,
        message_id: &str,
        dest_folder: &str,
    ) -> Result<(), EmailSyncError> {
        let backend = self.clone();
        let account = account.to_string();
        let message_id = message_id.to_string();
        let dest = dest_folder.to_string();
        self.runtime.block_on(async move {
            let state = backend.state(&account)?;
            backend.run_move_message(state, &message_id, &dest).await
        })
    }

    fn delete_message(&self, account: &str, message_id: &str) -> Result<(), EmailSyncError> {
        let backend = self.clone();
        let account = account.to_string();
        let message_id = message_id.to_string();
        self.runtime.block_on(async move {
            let state = backend.state(&account)?;
            backend.run_delete_message(state, &message_id).await
        })
    }

    fn append_draft(&self, account: &str, draft: Draft) -> Result<String, EmailSyncError> {
        let backend = self.clone();
        let account = account.to_string();
        self.runtime.block_on(async move {
            let state = backend.state(&account)?;
            backend.run_append_draft(state, draft).await
        })
    }

    fn send(&self, account: &str, draft: Draft) -> Result<String, EmailSyncError> {
        let backend = self.clone();
        let account = account.to_string();
        self.runtime.block_on(async move {
            let state = backend.state(&account)?;
            backend.run_send(state, draft).await
        })
    }
}

fn map_connect_err(e: ConnectError) -> EmailSyncError {
    match e {
        ConnectError::Tcp(s) | ConnectError::Greeting(s) => EmailSyncError::Network(s),
        ConnectError::Tls(s) => EmailSyncError::Network(format!("tls: {s}")),
        ConnectError::Login(_) => EmailSyncError::Auth,
        ConnectError::StarttlsUnsupported => {
            EmailSyncError::Unsupported("starttls not yet implemented".into())
        }
        ConnectError::PlaintextRefused => EmailSyncError::Unsupported("plaintext refused".into()),
    }
}

fn infer_role(name: &str) -> Option<email_proto::FolderRole> {
    let lower = name.to_ascii_lowercase();
    let leaf = lower.rsplit(['/', '.']).next().unwrap_or(&lower);
    match leaf {
        "inbox" => Some(email_proto::FolderRole::Inbox),
        "drafts" | "draft" => Some(email_proto::FolderRole::Drafts),
        "sent" | "sent items" | "sent mail" | "sent messages" => {
            Some(email_proto::FolderRole::Sent)
        }
        "trash" | "deleted" | "deleted items" | "bin" => Some(email_proto::FolderRole::Trash),
        "junk" | "spam" => Some(email_proto::FolderRole::Junk),
        "archive" | "archives" | "all mail" => Some(email_proto::FolderRole::Archive),
        "outbox" => Some(email_proto::FolderRole::Outbox),
        "flagged" | "starred" => Some(email_proto::FolderRole::Flagged),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn infer_role_matches_common_names() {
        assert_eq!(infer_role("INBOX"), Some(email_proto::FolderRole::Inbox));
        assert_eq!(infer_role("Sent"), Some(email_proto::FolderRole::Sent));
        assert_eq!(
            infer_role("[Gmail]/Sent Mail"),
            Some(email_proto::FolderRole::Sent)
        );
        assert_eq!(infer_role("Lists.rust-users"), None);
    }
}

/// The `#[subscribe]` backend contract: the hub the stream host
/// attaches subscriber sinks to. The IDLE watcher publishes into
/// it — an IMAP server breaking IDLE means "something changed",
/// which is exactly `EmailEvent::Resync`.
impl email_proto::EmailSyncStreamSource for Backend {
    fn changes_hub(&self) -> &architect::PubSub<email_proto::EmailChange> {
        &self.changes
    }
}
