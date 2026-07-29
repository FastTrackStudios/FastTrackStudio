//! `EmailSync` impl backed by a Maildir tree on disk. Mirrors
//! the layered shape of `vault::sync::Backend`.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use email_config::FolderAliases;
use email_proto::{
    Account, Draft, EmailEvent, EmailSync, EmailSyncError, Envelope, FlagDelta, Folder, Message,
    SeqRange,
};
use maildir::Maildir;
use tokio::sync::{RwLock, broadcast};

use crate::folder::{FolderName, infer_role};
use crate::parse;

/// Filesystem-backed `EmailSync` implementation. Cheap to
/// `Clone` — internals are `Arc`'d. One backend serves
/// one-or-more accounts; each account maps to a root directory
/// shaped like a Maildir++ tree (root = INBOX, sibling `.Foo`
/// dirs for sub-mailboxes).
#[derive(Clone, architect::HasDispatcher)]
pub struct Backend {
    accounts: Arc<HashMap<String, AccountState>>,
    /// Per-account broadcast sender, lazily created on first
    /// `subscribe`. Same shape + capacity as
    /// `vault::sync::Backend`.
    channels: Arc<RwLock<HashMap<String, broadcast::Sender<EmailEvent>>>>,
    /// Fan-out hub behind the `#[subscribe] fn changes` stream.
    /// Every event that goes onto a per-account broadcast channel
    /// is published here too, wrapped with its `account` so
    /// subscribers — who see every account this backend serves —
    /// can filter. Sliding mailbox: a slow subscriber loses its
    /// oldest queued events and re-pulls on reconnect, which is
    /// what `EmailEvent::Resync` asks for anyway.
    changes: architect::PubSub<email_proto::EmailChange>,
}

struct AccountState {
    account: Account,
    root: PathBuf,
    /// Wire-side ↔ backend-side folder name translation. Empty
    /// by default; populated from `AccountConfig::folder_aliases`.
    aliases: FolderAliases,
}

impl Backend {
    /// Build a backend serving a single account rooted at
    /// `root`. The directory + its `cur/new/tmp` subdirs are
    /// created on demand. No folder aliases — see
    /// [`Backend::single_with_aliases`] when the account config
    /// declares them.
    pub fn single(account: Account, root: PathBuf) -> std::io::Result<Self> {
        Self::single_with_aliases(account, root, FolderAliases::new())
    }

    /// Build a backend with an explicit folder-alias map.
    /// `aliases` is consulted in both directions: incoming
    /// folder names are translated to backend names before
    /// disk I/O; outgoing folder listings are translated back
    /// to alias names for the UI.
    pub fn single_with_aliases(
        account: Account,
        root: PathBuf,
        aliases: FolderAliases,
    ) -> std::io::Result<Self> {
        ensure_maildir(&root)?;
        let mut accounts = HashMap::with_capacity(1);
        let id = account.id.0.clone();
        accounts.insert(
            id,
            AccountState {
                account,
                root,
                aliases,
            },
        );
        Ok(Self {
            accounts: Arc::new(accounts),
            channels: Arc::new(RwLock::new(HashMap::new())),
            changes: architect::PubSub::sliding(256),
        })
    }

    /// Build a backend from a pre-built `(Account, root, aliases)`
    /// set. All roots must exist as Maildir trees (use
    /// [`Backend::single_with_aliases`] to bootstrap).
    pub fn with_accounts<I>(entries: I) -> Self
    where
        I: IntoIterator<Item = (Account, PathBuf, FolderAliases)>,
    {
        let mut accounts = HashMap::new();
        for (account, root, aliases) in entries {
            accounts.insert(
                account.id.0.clone(),
                AccountState {
                    account,
                    root,
                    aliases,
                },
            );
        }
        Self {
            accounts: Arc::new(accounts),
            channels: Arc::new(RwLock::new(HashMap::new())),
            changes: architect::PubSub::sliding(256),
        }
    }

    fn state(&self, account: &str) -> Result<&AccountState, EmailSyncError> {
        self.accounts
            .get(account)
            .ok_or(EmailSyncError::UnknownAccount)
    }

    /// Get-or-create the per-account broadcast sender.
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

    /// Announce a committed change on both paths: `account`'s
    /// in-process broadcast channel and the wire hub. Call only
    /// after the mailbox actually changed — subscribers re-read on
    /// the event.
    pub async fn emit(&self, account: &str, event: EmailEvent) {
        let _ = self.channel(account).await.send(event.clone());
        self.changes.publish(email_proto::EmailChange {
            account: account.to_string(),
            event,
        });
    }
}

impl EmailSync for Backend {
    fn accounts(&self) -> Result<Vec<Account>, EmailSyncError> {
        Ok(self.accounts.values().map(|s| s.account.clone()).collect())
    }

    fn list_folders(&self, account: &str) -> Result<Vec<Folder>, EmailSyncError> {
        let state = self.state(account)?;
        let mut folders = Vec::new();

        // INBOX is the root itself.
        let (m, u) = folder_counts(&state.root);
        folders.push(Folder {
            id: FolderName::INBOX.into(),
            name: FolderName::INBOX.into(),
            delimiter: ".".into(),
            role: Some(email_proto::FolderRole::Inbox),
            message_count: Some(m),
            unread_count: Some(u),
        });

        // Sibling `.Foo` dirs are sub-mailboxes.
        let dir = match std::fs::read_dir(&state.root) {
            Ok(d) => d,
            Err(_) => return Ok(folders),
        };
        for entry in dir.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let name = match entry.file_name().into_string() {
                Ok(s) => s,
                Err(_) => continue,
            };
            let Some(backend_name) = FolderName::from_maildir_dir_name(&name) else {
                continue;
            };
            if !is_maildir(&path) {
                continue;
            }
            let (m, u) = folder_counts(&path);
            // Translate backend → UI/alias before reporting. If
            // the user has aliased `[Gmail]/Sent Mail` to
            // `Sent`, the UI sees `Sent` here.
            let ui_name = state
                .aliases
                .alias_for(&backend_name.0)
                .map_or_else(|| backend_name.0.clone(), str::to_string);
            folders.push(Folder {
                id: ui_name.clone(),
                name: ui_name.clone(),
                delimiter: ".".into(),
                role: infer_role(&ui_name),
                message_count: Some(m),
                unread_count: Some(u),
            });
        }

        Ok(folders)
    }

    fn fetch_envelopes(
        &self,
        account: &str,
        folder: &str,
        range: SeqRange,
    ) -> Result<Vec<Envelope>, EmailSyncError> {
        let state = self.state(account)?;
        // Translate UI/alias → backend name before resolving on
        // disk. Pass-through when no alias is registered.
        let resolved = state.aliases.resolve(folder);
        let path = FolderName(resolved.to_string())
            .to_path(&state.root)
            .ok_or(EmailSyncError::NotFound)?;
        if !is_maildir(&path) {
            return Err(EmailSyncError::NotFound);
        }
        let md = Maildir::from(path);
        let mut envs: Vec<Envelope> = Vec::new();
        for entry in md.list_cur().chain(md.list_new()) {
            let entry = match entry {
                Ok(e) => e,
                Err(err) => {
                    tracing::warn!(error = %err, "skipping unreadable maildir entry");
                    continue;
                }
            };
            let bytes = match std::fs::read(entry.path()) {
                Ok(b) => b,
                Err(e) => {
                    tracing::warn!(error = %e, "read failed");
                    continue;
                }
            };
            let flags = entry.flags().chars().map(|c| c.to_string()).collect();
            match parse::envelope_from_bytes(&bytes, folder, flags) {
                Ok(env) => envs.push(env),
                Err(err) => tracing::warn!(error = %err, "parse failed"),
            }
        }

        // Newest-first; slice per requested range.
        envs.sort_by(|a, b| b.date_ms.cmp(&a.date_ms));
        let envs = match range {
            SeqRange::All => envs,
            SeqRange::Recent(n) => envs.into_iter().take(n as usize).collect(),
            SeqRange::Range { from, to } => {
                let from = from as usize;
                let to = to as usize;
                envs.into_iter()
                    .enumerate()
                    .filter(|(i, _)| *i >= from && *i <= to)
                    .map(|(_, e)| e)
                    .collect()
            }
        };
        Ok(envs)
    }

    fn fetch_message(&self, account: &str, message_id: &str) -> Result<Message, EmailSyncError> {
        let state = self.state(account)?;
        // Scan INBOX + sub-mailboxes for a matching Message-ID.
        // O(N) — acceptable for the read-only walker; the SQLite
        // index in `email-store` makes this O(1) later.
        let folders = self.list_folders(account)?;
        for folder in folders {
            // `folder.id` is the alias-side name; resolve to
            // the backend name before touching disk.
            let backend_name = state.aliases.resolve(&folder.id).to_string();
            let Some(path) = FolderName(backend_name).to_path(&state.root) else {
                continue;
            };
            if !is_maildir(&path) {
                continue;
            }
            let md = Maildir::from(path);
            for entry in md.list_cur().chain(md.list_new()) {
                let entry = match entry {
                    Ok(e) => e,
                    Err(_) => continue,
                };
                let bytes = match std::fs::read(entry.path()) {
                    Ok(b) => b,
                    Err(_) => continue,
                };
                let flags: Vec<String> = entry.flags().chars().map(|c| c.to_string()).collect();
                let Ok(env) = parse::envelope_from_bytes(&bytes, &folder.id, flags.clone()) else {
                    continue;
                };
                if env.message_id == message_id
                    || env.message_id.trim_matches(|c| c == '<' || c == '>')
                        == message_id.trim_matches(|c| c == '<' || c == '>')
                {
                    return parse::message_from_bytes(&bytes, &folder.id, flags);
                }
            }
        }
        Err(EmailSyncError::NotFound)
    }

    fn fetch_attachment(
        &self,
        account: &str,
        message_id: &str,
        part: &str,
    ) -> Result<Vec<u8>, EmailSyncError> {
        // Re-resolve to bytes via the same scan. Future: index
        // by message-id in email-store.
        let state = self.state(account)?;
        let folders = self.list_folders(account)?;
        for folder in folders {
            // `folder.id` is the alias-side name; resolve to
            // the backend name before touching disk.
            let backend_name = state.aliases.resolve(&folder.id).to_string();
            let Some(path) = FolderName(backend_name).to_path(&state.root) else {
                continue;
            };
            if !is_maildir(&path) {
                continue;
            }
            let md = Maildir::from(path);
            for entry in md.list_cur().chain(md.list_new()) {
                let entry = match entry {
                    Ok(e) => e,
                    Err(_) => continue,
                };
                let bytes = match std::fs::read(entry.path()) {
                    Ok(b) => b,
                    Err(_) => continue,
                };
                let Ok(env) = parse::envelope_from_bytes(&bytes, &folder.id, Vec::new()) else {
                    continue;
                };
                if env.message_id == message_id {
                    return parse::attachment_bytes(&bytes, part);
                }
            }
        }
        Err(EmailSyncError::NotFound)
    }

    fn set_flags(
        &self,
        _account: &str,
        _message_id: &str,
        _delta: FlagDelta,
    ) -> Result<(), EmailSyncError> {
        Err(EmailSyncError::Unsupported(
            "maildir: set_flags lands in phase 2".into(),
        ))
    }

    fn move_message(
        &self,
        _account: &str,
        _message_id: &str,
        _dest_folder: &str,
    ) -> Result<(), EmailSyncError> {
        Err(EmailSyncError::Unsupported(
            "maildir: move_message lands in phase 2".into(),
        ))
    }

    fn delete_message(&self, _account: &str, _message_id: &str) -> Result<(), EmailSyncError> {
        Err(EmailSyncError::Unsupported(
            "maildir: delete_message lands in phase 2".into(),
        ))
    }

    fn append_draft(&self, _account: &str, _draft: Draft) -> Result<String, EmailSyncError> {
        Err(EmailSyncError::Unsupported(
            "maildir: append_draft lands in phase 3".into(),
        ))
    }

    fn send(&self, _account: &str, _draft: Draft) -> Result<String, EmailSyncError> {
        Err(EmailSyncError::Unsupported(
            "maildir: send lives in `email-smtp`".into(),
        ))
    }
}

/// The `#[subscribe]` backend contract: the hub the stream host
/// attaches subscriber sinks to. Publishing happens in
/// [`Backend::emit`].
///
/// Nothing publishes yet on this backend: every maildir mutation
/// (`set_flags` / `move_message` / `delete_message`) is still
/// `Unsupported`, and there is no filesystem watcher on the mail
/// root — so the stream is correct and silent until one of those
/// lands. `email-imap` already publishes from its IDLE loop.
impl email_proto::EmailSyncStreamSource for Backend {
    fn changes_hub(&self) -> &architect::PubSub<email_proto::EmailChange> {
        &self.changes
    }
}

/// Create `cur` / `new` / `tmp` under `root` if missing.
fn ensure_maildir(root: &std::path::Path) -> std::io::Result<()> {
    std::fs::create_dir_all(root.join("cur"))?;
    std::fs::create_dir_all(root.join("new"))?;
    std::fs::create_dir_all(root.join("tmp"))?;
    Ok(())
}

fn is_maildir(path: &std::path::Path) -> bool {
    path.join("cur").is_dir() && path.join("new").is_dir()
}

/// Quick counts by directory listing — no parse needed. The
/// `cur`/Seen-flag check is approximate (any file whose
/// info-section lacks `S` is treated as unread).
fn folder_counts(path: &std::path::Path) -> (u32, u32) {
    let mut total = 0u32;
    let mut unread = 0u32;
    for sub in ["new", "cur"] {
        let dir = path.join(sub);
        let Ok(entries) = std::fs::read_dir(&dir) else {
            continue;
        };
        for entry in entries.flatten() {
            if !entry.path().is_file() {
                continue;
            }
            total += 1;
            let name = entry.file_name().to_string_lossy().into_owned();
            // Maildir info section: `…:2,FLAGS` — `S` means
            // Seen. Files in `new/` are never seen yet.
            let seen = sub == "cur"
                && name
                    .split(":2,")
                    .nth(1)
                    .is_some_and(|info| info.contains('S'));
            if !seen {
                unread += 1;
            }
        }
    }
    (total, unread)
}

#[cfg(test)]
mod tests {
    use super::*;
    use email_proto::{Account, AccountId};

    fn write_msg(path: &std::path::Path, name: &str, body: &str) {
        std::fs::write(path.join(name), body).unwrap();
    }

    fn fixture() -> (tempfile::TempDir, Backend, Account) {
        let dir = tempfile::tempdir().unwrap();
        let account = Account {
            id: AccountId("scratch".into()),
            name: "scratch".into(),
            address: "you@example.com".into(),
            display_name: None,
        };
        let backend = Backend::single(account.clone(), dir.path().to_path_buf()).unwrap();
        // INBOX message.
        write_msg(
            &dir.path().join("new"),
            "1700000000.M1P1.host",
            "Message-ID: <a@example.com>\r\n\
             From: Alice <alice@example.com>\r\n\
             To: you@example.com\r\n\
             Subject: Hello\r\n\
             Date: Mon, 14 Nov 2023 12:00:00 +0000\r\n\
             \r\n\
             Body text.\r\n",
        );
        // Sent sub-mailbox.
        let sent = dir.path().join(".Sent");
        std::fs::create_dir_all(sent.join("cur")).unwrap();
        std::fs::create_dir_all(sent.join("new")).unwrap();
        std::fs::create_dir_all(sent.join("tmp")).unwrap();
        write_msg(
            &sent.join("cur"),
            "1700000001.M1P2.host:2,S",
            "Message-ID: <b@example.com>\r\n\
             From: you@example.com\r\n\
             To: Bob <bob@example.com>\r\n\
             Subject: Re: Hello\r\n\
             In-Reply-To: <a@example.com>\r\n\
             Date: Mon, 14 Nov 2023 13:00:00 +0000\r\n\
             \r\n\
             Reply.\r\n",
        );
        (dir, backend, account)
    }

    #[test]
    fn list_folders_returns_inbox_and_sent() {
        let (_dir, backend, account) = fixture();
        let folders = backend.list_folders(&account.id.0).unwrap();
        let names: Vec<_> = folders.iter().map(|f| f.id.as_str()).collect();
        assert!(names.contains(&"INBOX"), "got: {names:?}");
        assert!(names.contains(&"Sent"), "got: {names:?}");
    }

    #[test]
    fn fetch_envelopes_parses_headers() {
        let (_dir, backend, account) = fixture();
        let envs = backend
            .fetch_envelopes(&account.id.0, "INBOX", SeqRange::All)
            .unwrap();
        assert_eq!(envs.len(), 1);
        assert_eq!(envs[0].subject, "Hello");
        assert_eq!(envs[0].from[0].email, "alice@example.com");
        assert!(envs[0].message_id.contains("a@example.com"));
    }

    #[test]
    fn fetch_message_by_id_finds_across_folders() {
        let (_dir, backend, account) = fixture();
        let msg = backend
            .fetch_message(&account.id.0, "<b@example.com>")
            .unwrap();
        assert_eq!(msg.envelope.subject, "Re: Hello");
        assert_eq!(msg.envelope.folder, "Sent");
        assert!(msg.references.iter().any(|r| r.contains("a@example.com")));
    }

    #[test]
    fn folder_aliases_translate_in_both_directions() {
        // Backend folder name is `Gmail-Sent` (simulating a
        // server-side weird name). The user aliases it to
        // `Sent`, expects `Sent` everywhere in the UI.
        let dir = tempfile::tempdir().unwrap();
        let account = Account {
            id: AccountId("scratch".into()),
            name: "scratch".into(),
            address: "you@example.com".into(),
            display_name: None,
        };
        let mut aliases = FolderAliases::new();
        aliases.insert("Sent", "Gmail-Sent");
        let backend =
            Backend::single_with_aliases(account.clone(), dir.path().to_path_buf(), aliases)
                .unwrap();

        // Build the backend-named folder on disk.
        let weird = dir.path().join(".Gmail-Sent");
        std::fs::create_dir_all(weird.join("cur")).unwrap();
        std::fs::create_dir_all(weird.join("new")).unwrap();
        std::fs::create_dir_all(weird.join("tmp")).unwrap();
        std::fs::write(
            weird.join("cur").join("1700000010.M.host:2,S"),
            "Message-ID: <c@example.com>\r\n\
             From: you@example.com\r\n\
             To: c@example.com\r\n\
             Subject: Aliased send\r\n\
             Date: Mon, 14 Nov 2023 14:00:00 +0000\r\n\
             \r\n\
             body\r\n",
        )
        .unwrap();

        // list_folders reports the alias name, not the backend name.
        let folders = backend.list_folders(&account.id.0).unwrap();
        let ids: Vec<_> = folders.iter().map(|f| f.id.as_str()).collect();
        assert!(ids.contains(&"Sent"), "got: {ids:?}");
        assert!(!ids.contains(&"Gmail-Sent"), "got: {ids:?}");

        // fetch_envelopes accepts the alias name.
        let envs = backend
            .fetch_envelopes(&account.id.0, "Sent", SeqRange::All)
            .unwrap();
        assert_eq!(envs.len(), 1);
        assert_eq!(envs[0].subject, "Aliased send");
        // Envelope echoes the caller's folder name (alias), not
        // the backend name.
        assert_eq!(envs[0].folder, "Sent");

        // fetch_message also finds it via the aliased listing.
        let msg = backend
            .fetch_message(&account.id.0, "<c@example.com>")
            .unwrap();
        assert_eq!(msg.envelope.folder, "Sent");
    }

    #[test]
    fn folder_counts_track_seen_flag() {
        let (_dir, backend, account) = fixture();
        let folders = backend.list_folders(&account.id.0).unwrap();
        let inbox = folders.iter().find(|f| f.id == "INBOX").unwrap();
        assert_eq!(inbox.message_count, Some(1));
        assert_eq!(inbox.unread_count, Some(1));
        let sent = folders.iter().find(|f| f.id == "Sent").unwrap();
        assert_eq!(sent.message_count, Some(1));
        assert_eq!(sent.unread_count, Some(0));
    }
}
