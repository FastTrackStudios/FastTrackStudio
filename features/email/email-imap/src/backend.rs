//! `EmailSync` impl backed by an IMAP server via `async-imap`.
//! Connections are opened on demand per call; a future pass
//! pools them. Folder aliases (`email-config::FolderAliases`)
//! are honored at the wire boundary, same shape as
//! `email-maildir::Backend`.

use std::collections::HashMap;
use std::sync::Arc;

use architect::HasDispatcher;
use architect::dispatch::TokioBlockingDispatcher;
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

/// One configured IMAP account. Holds the connection parameters
/// + folder alias map; the actual session is opened per-op
/// until we add pooling.
struct AccountState {
    account: Account,
    host: String,
    port: u16,
    tls: TlsMode,
    username: String,
    password: email_secret::Secret,
    aliases: FolderAliases,
    _smtp: Option<SmtpConfig>,
}

/// IMAP backend. Cheap to `Clone` — all internals are `Arc`'d.
#[derive(Clone)]
pub struct Backend {
    accounts: Arc<HashMap<String, AccountState>>,
    /// Per-account broadcast sender, lazily created on first
    /// `subscribe`. Same shape as `vault::sync::Backend` +
    /// `email-maildir::Backend`.
    channels: Arc<RwLock<HashMap<String, broadcast::Sender<EmailEvent>>>>,
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
                    _smtp: submit,
                },
            );
        }

        Ok(Self {
            accounts: Arc::new(accounts),
            channels: Arc::new(RwLock::new(HashMap::new())),
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
                .map(str::to_string)
                .unwrap_or_else(|| backend_name.clone());
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
            let flags: Vec<String> = fetch.flags().map(|f| format!("{:?}", f)).collect();
            let size = fetch.size.unwrap_or(0) as u64;
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
            let uids: Vec<u32> = match session.uid_search(&needle).await {
                Ok(u) => u.into_iter().collect(),
                Err(_) => {
                    let _ = session.logout().await;
                    continue;
                }
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
                let flags: Vec<String> = fetch.flags().map(|f| format!("{:?}", f)).collect();
                let size = fetch.size.unwrap_or(0) as u64;
                (body, flags, size)
            };
            let _ = session.logout().await;
            return parse::message_from_bytes(&body, &folder.id, flags, size);
        }
        Err(EmailSyncError::NotFound)
    }
}

impl HasDispatcher for Backend {
    type Dispatcher = TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        TokioBlockingDispatcher
    }
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
        _account: &str,
        _message_id: &str,
        _delta: FlagDelta,
    ) -> Result<(), EmailSyncError> {
        Err(EmailSyncError::Unsupported(
            "imap: set_flags lands in phase 3".into(),
        ))
    }

    fn move_message(
        &self,
        _account: &str,
        _message_id: &str,
        _dest_folder: &str,
    ) -> Result<(), EmailSyncError> {
        Err(EmailSyncError::Unsupported(
            "imap: move_message lands in phase 3".into(),
        ))
    }

    fn delete_message(&self, _account: &str, _message_id: &str) -> Result<(), EmailSyncError> {
        Err(EmailSyncError::Unsupported(
            "imap: delete_message lands in phase 3".into(),
        ))
    }

    fn append_draft(&self, _account: &str, _draft: Draft) -> Result<String, EmailSyncError> {
        Err(EmailSyncError::Unsupported(
            "imap: append_draft lands in phase 3".into(),
        ))
    }

    fn send(&self, _account: &str, _draft: Draft) -> Result<String, EmailSyncError> {
        Err(EmailSyncError::Unsupported(
            "imap: send lives in `email-smtp`".into(),
        ))
    }

    async fn subscribe(&self, account: String, tx: vox::Tx<EmailEvent>) {
        if self.state(&account).is_err() {
            let _ = tx.close(Default::default()).await;
            return;
        }
        let sender = self.channel(&account).await;
        let mut rx = sender.subscribe();
        loop {
            match rx.recv().await {
                Ok(evt) => {
                    if tx.send(evt).await.is_err() {
                        return;
                    }
                }
                Err(broadcast::error::RecvError::Closed) => return,
                Err(broadcast::error::RecvError::Lagged(_)) => {
                    if tx.send(EmailEvent::Resync).await.is_err() {
                        return;
                    }
                }
            }
        }
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
