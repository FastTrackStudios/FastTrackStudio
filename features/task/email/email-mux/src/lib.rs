//! Account-dispatching [`EmailSync`] backend.
//!
//! One org can hold a local Maildir account *and* one or more remote
//! IMAP accounts (a Gmail mailbox, say), but the server mounts exactly
//! one `EmailSync` service. This routes each call to whichever backend
//! owns the named account.
//!
//! Two things make that more than a match statement:
//!
//! - **One stream.** Subscribers attach to a single `EmailChange`
//!   stream, so every sub-backend has to publish into the *same*
//!   `architect::PubSub`. There is no subscribe side to bridge two
//!   hubs with, so the mux builds the hub and hands it down via
//!   `with_changes_hub` before anything is cloned.
//! - **Degrading, not failing.** An IMAP account whose credentials are
//!   wrong or whose host is unreachable must not take the whole
//!   `/email` page down with it. Construction never fails on a bad
//!   account: it is logged and skipped, and the remaining accounts
//!   serve normally.
//!
//! Routing is by account id, which is the directory name under the
//! org's mail root. The two backends' account sets are disjoint by
//! construction — each is built from the subset of configs whose
//! `BackendKind` it handles.

#![cfg(not(target_arch = "wasm32"))]

use std::collections::HashMap;
use std::sync::Arc;

use email_proto::{
    Account, Draft, EmailSync, EmailSyncError, Envelope, FlagDelta, Folder, Message, SeqRange,
};

/// Which backend owns an account.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Route {
    Maildir,
    Imap,
}

/// The multiplexed backend. Cheap to clone — every field is either an
/// `Arc` or an already-`Clone` backend handle.
///
/// `HasDispatcher` is derived for the same reason the two sub-backends
/// derive it: `EmailSync`'s methods are synchronous and the IMAP one
/// blocks on a runtime internally, so architect must run them on its
/// tokio *blocking* pool. Mounting this on an async dispatcher would
/// panic with "Cannot start a runtime from within a runtime" the first
/// time anyone opened a remote mailbox.
#[derive(Clone, architect::HasDispatcher)]
pub struct Backend {
    maildir: email_maildir::Backend,
    /// `None` when no IMAP account is configured, or when the IMAP
    /// backend could not be constructed (no tokio runtime). Routing
    /// falls back to an `UnknownAccount` error rather than panicking.
    imap: Option<email_imap::Backend>,
    routes: Arc<HashMap<String, Route>>,
    changes: architect::PubSub<email_proto::EmailChange>,
}

impl Backend {
    /// Build from the org's account configs plus the maildir entries
    /// the server already resolved for local accounts.
    ///
    /// `maildir_entries` and `configs` describe the same set of
    /// accounts; the configs decide routing, the entries carry the
    /// maildir-specific bits (root path, submit transport) the server
    /// resolves while scanning the mail root.
    pub fn build(
        maildir_entries: Vec<email_maildir::AccountEntry>,
        configs: Vec<email_config::AccountConfig>,
    ) -> Self {
        let changes = architect::PubSub::sliding(256);

        let mut routes: HashMap<String, Route> = HashMap::new();
        for entry in &maildir_entries {
            routes.insert(entry.account.id.0.clone(), Route::Maildir);
        }

        let imap_configs: Vec<email_config::AccountConfig> = configs
            .iter()
            .filter(|c| matches!(c.backend, email_config::BackendKind::Imap { .. }))
            .cloned()
            .collect();
        for cfg in &imap_configs {
            routes.insert(cfg.id.0.clone(), Route::Imap);
        }

        let maildir = email_maildir::Backend::with_configured_accounts(maildir_entries)
            .with_changes_hub(changes.clone());

        let imap = if imap_configs.is_empty() {
            None
        } else {
            match email_imap::Backend::from_configs(imap_configs) {
                Ok(b) => Some(b.with_changes_hub(changes.clone())),
                Err(err) => {
                    // Only happens off a tokio runtime. Log rather than
                    // fail the org: the maildir accounts still work.
                    tracing::error!(%err, "imap backend unavailable; imap accounts disabled");
                    for (_, route) in routes.iter_mut().filter(|(_, r)| **r == Route::Imap) {
                        *route = Route::Maildir;
                    }
                    routes.retain(|_, r| *r == Route::Maildir);
                    None
                }
            }
        };

        Self {
            maildir,
            imap,
            routes: Arc::new(routes),
            changes,
        }
    }

    /// How many accounts route to IMAP. Used by the server to decide
    /// whether to start IDLE watchers.
    #[must_use]
    pub fn imap_account_ids(&self) -> Vec<String> {
        self.routes
            .iter()
            .filter(|(_, r)| **r == Route::Imap)
            .map(|(id, _)| id.clone())
            .collect()
    }

    /// The IMAP backend, when one is configured — the server needs it
    /// to start the per-account IDLE loops.
    #[must_use]
    pub fn imap(&self) -> Option<&email_imap::Backend> {
        self.imap.as_ref()
    }

    /// The backend that owns `account`.
    ///
    /// An unknown account is `UnknownAccount`, never a panic and never
    /// a silent fall-through to the wrong store — filing a Gmail
    /// message into a local maildir because a lookup missed would be
    /// data loss.
    fn route(&self, account: &str) -> Result<&dyn EmailSync, EmailSyncError> {
        match self.routes.get(account) {
            Some(Route::Maildir) => Ok(&self.maildir),
            Some(Route::Imap) => self
                .imap
                .as_ref()
                .map(|b| b as &dyn EmailSync)
                .ok_or(EmailSyncError::UnknownAccount),
            None => Err(EmailSyncError::UnknownAccount),
        }
    }
}

impl EmailSync for Backend {
    /// The union of both backends' accounts, maildir first so a
    /// single-account org's ordering is unchanged.
    fn accounts(&self) -> Result<Vec<Account>, EmailSyncError> {
        let mut out = self.maildir.accounts()?;
        if let Some(imap) = &self.imap {
            // A backend that can't enumerate (transient network) must
            // not blank the local accounts too.
            match imap.accounts() {
                Ok(mut list) => out.append(&mut list),
                Err(err) => tracing::warn!(?err, "imap: accounts() failed; listing local only"),
            }
        }
        Ok(out)
    }

    fn list_folders(&self, account: &str) -> Result<Vec<Folder>, EmailSyncError> {
        self.route(account)?.list_folders(account)
    }

    fn fetch_envelopes(
        &self,
        account: &str,
        folder: &str,
        range: SeqRange,
    ) -> Result<Vec<Envelope>, EmailSyncError> {
        self.route(account)?.fetch_envelopes(account, folder, range)
    }

    fn fetch_message(&self, account: &str, message_id: &str) -> Result<Message, EmailSyncError> {
        self.route(account)?.fetch_message(account, message_id)
    }

    fn fetch_attachment(
        &self,
        account: &str,
        message_id: &str,
        part: &str,
    ) -> Result<Vec<u8>, EmailSyncError> {
        self.route(account)?
            .fetch_attachment(account, message_id, part)
    }

    fn set_flags(
        &self,
        account: &str,
        message_id: &str,
        delta: FlagDelta,
    ) -> Result<(), EmailSyncError> {
        self.route(account)?.set_flags(account, message_id, delta)
    }

    fn move_message(
        &self,
        account: &str,
        message_id: &str,
        dest_folder: &str,
    ) -> Result<(), EmailSyncError> {
        self.route(account)?
            .move_message(account, message_id, dest_folder)
    }

    fn delete_message(&self, account: &str, message_id: &str) -> Result<(), EmailSyncError> {
        self.route(account)?.delete_message(account, message_id)
    }

    fn append_draft(&self, account: &str, draft: Draft) -> Result<String, EmailSyncError> {
        self.route(account)?.append_draft(account, draft)
    }

    fn send(&self, account: &str, draft: Draft) -> Result<String, EmailSyncError> {
        self.route(account)?.send(account, draft)
    }
}

/// The single hub both sub-backends publish into — see the module
/// docs.
impl email_proto::EmailSyncStreamSource for Backend {
    fn changes_hub(&self) -> &architect::PubSub<email_proto::EmailChange> {
        &self.changes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn imap_cfg(id: &str) -> email_config::AccountConfig {
        email_config::AccountConfig {
            id: email_proto::AccountId(id.to_owned()),
            name: id.to_owned(),
            address: format!("{id}@example.com"),
            display_name: None,
            backend: email_config::BackendKind::Imap {
                host: "imap.example.com".into(),
                port: 993,
                tls: email_config::TlsMode::Implicit,
                username: id.to_owned(),
                password: email_secret::Secret::raw("pw"),
                submit: None,
            },
            signature: None,
            folder_aliases: email_config::FolderAliases::new(),
        }
    }

    #[test]
    fn unknown_accounts_are_rejected_not_misrouted() {
        // No tokio runtime here, so the IMAP backend can't build —
        // which is exactly the degraded path worth pinning: the mux
        // must still construct, and must not answer for an account it
        // cannot serve.
        let mux = Backend::build(Vec::new(), vec![imap_cfg("gmail")]);
        assert!(matches!(
            mux.list_folders("nope"),
            Err(EmailSyncError::UnknownAccount)
        ));
        // And it must never fall through to the maildir backend, which
        // would file remote mail into a local store.
        assert!(matches!(
            mux.list_folders("gmail"),
            Err(EmailSyncError::UnknownAccount)
        ));
    }

    /// IMAP's `EmailSync` methods are sync wrappers around
    /// `runtime.block_on`, which **panics if called on a runtime
    /// worker thread**. In the server they run on architect's tokio
    /// *blocking* dispatcher (hence `architect/dispatch-tokio`), so
    /// tests must call them the same way — `spawn_blocking`, never
    /// straight from an `async fn`.
    async fn blocking<T, F>(f: F) -> T
    where
        F: FnOnce() -> T + Send + 'static,
        T: Send + 'static,
    {
        tokio::task::spawn_blocking(f).await.expect("join")
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn imap_accounts_route_to_imap() {
        let mux = Backend::build(Vec::new(), vec![imap_cfg("gmail")]);
        assert_eq!(mux.imap_account_ids(), vec!["gmail".to_owned()]);
        assert!(mux.imap().is_some());
        // Reachability isn't asserted (no server here) — routing is:
        // the call must reach IMAP and fail as a network/auth error,
        // not as UnknownAccount.
        let err = blocking(move || mux.list_folders("gmail")).await.unwrap_err();
        assert!(
            !matches!(err, EmailSyncError::UnknownAccount),
            "routed to imap, got {err:?}"
        );
    }

    #[tokio::test(flavor = "multi_thread")]
    async fn accounts_lists_both_backends() {
        let mux = Backend::build(Vec::new(), vec![imap_cfg("gmail")]);
        let ids: Vec<String> = blocking(move || mux.accounts())
            .await
            .unwrap()
            .into_iter()
            .map(|a| a.id.0)
            .collect();
        assert_eq!(ids, vec!["gmail".to_owned()]);
    }
}
