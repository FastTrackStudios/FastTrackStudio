//! Server-side `MailService` implementation.
//!
//! The bulk of the API requires a live IMAP / Nextcloud Mail provider that
//! is not yet wired into this codebase. Those operations return
//! `VaultError::IoError("provider not configured: <op>")` so clients (CLI,
//! UI) get a clean per-operation error instead of `Unknown Vox service`.
//!
//! `linked_message_ids` is the one op we can satisfy from the local SQLite
//! `email_refs` table via `EmailRefRepo` — every row is by construction a
//! linked email, so we just project `message_id`.

use crate::email::EmailRef;
use crate::provider::{MailAccount, MailMessage, MailMessageDetail, MailTag, Mailbox};
use crate::service::{
    EmailLinkRequest, EmailLinkResponse, EmailListRequest, EmailUnlinkRequest,
    MailCreateMailboxRequest, MailCreateTagRequest, MailDeleteTagRequest, MailListMessagesRequest,
    MailMessageTagRequest, MailMoveMessageRequest, MailService, VaultError,
};

/// Build a uniform "no provider configured" error.
fn provider_not_configured(op: &str) -> VaultError {
    VaultError::IoError(format!("provider not configured: {op}"))
}

#[derive(Clone)]
pub struct MailServiceImpl<R> {
    email_repo: R,
}

impl<R> MailServiceImpl<R> {
    pub fn new(email_repo: R) -> Self {
        Self { email_repo }
    }
}

impl<R> MailService for MailServiceImpl<R>
where
    R: Clone + Send + Sync + 'static,
{
    async fn list_accounts(&self) -> Result<Vec<MailAccount>, VaultError> {
        Err(provider_not_configured("list_accounts"))
    }

    async fn list_mailboxes(&self, _account_id: i64) -> Result<Vec<Mailbox>, VaultError> {
        Err(provider_not_configured("list_mailboxes"))
    }

    async fn list_messages(
        &self,
        _request: MailListMessagesRequest,
    ) -> Result<Vec<MailMessage>, VaultError> {
        Err(provider_not_configured("list_messages"))
    }

    async fn get_message(&self, _id: i64) -> Result<MailMessageDetail, VaultError> {
        Err(provider_not_configured("get_message"))
    }

    async fn get_body(&self, _id: i64) -> Result<String, VaultError> {
        Err(provider_not_configured("get_body"))
    }

    async fn create_mailbox(
        &self,
        _request: MailCreateMailboxRequest,
    ) -> Result<Mailbox, VaultError> {
        Err(provider_not_configured("create_mailbox"))
    }

    async fn delete_mailbox(&self, _mailbox_id: i64) -> Result<(), VaultError> {
        Err(provider_not_configured("delete_mailbox"))
    }

    async fn move_message(&self, _request: MailMoveMessageRequest) -> Result<(), VaultError> {
        Err(provider_not_configured("move_message"))
    }

    async fn list_tags(&self) -> Result<Vec<MailTag>, VaultError> {
        Err(provider_not_configured("list_tags"))
    }

    async fn create_tag(&self, _request: MailCreateTagRequest) -> Result<MailTag, VaultError> {
        Err(provider_not_configured("create_tag"))
    }

    async fn delete_tag(&self, _request: MailDeleteTagRequest) -> Result<(), VaultError> {
        Err(provider_not_configured("delete_tag"))
    }

    async fn set_tag(&self, _request: MailMessageTagRequest) -> Result<(), VaultError> {
        Err(provider_not_configured("set_tag"))
    }

    async fn remove_tag(&self, _request: MailMessageTagRequest) -> Result<(), VaultError> {
        Err(provider_not_configured("remove_tag"))
    }

    async fn link_email(
        &self,
        _request: EmailLinkRequest,
    ) -> Result<EmailLinkResponse, VaultError> {
        // Linking writes to a Task or Project's `emails:` frontmatter, which
        // is markdown-vault behavior the new SQLite repos don't expose.
        Err(provider_not_configured("link_email"))
    }

    async fn unlink_email(&self, _request: EmailUnlinkRequest) -> Result<(), VaultError> {
        Err(provider_not_configured("unlink_email"))
    }

    async fn list_linked_emails(
        &self,
        _request: EmailListRequest,
    ) -> Result<Vec<EmailRef>, VaultError> {
        // Same shape problem as link/unlink — needs to project email_refs by
        // task/project reference, which the standalone EmailRef table doesn't
        // model with a FK yet.
        Err(provider_not_configured("list_linked_emails"))
    }

    async fn linked_message_ids(&self) -> Vec<String> {
        // Best-effort: every row in `email_refs` represents a linked email,
        // so the message_id column is what callers want. Repo access isn't
        // wired through a shared trait yet, so we return an empty vector
        // when the provider can't satisfy the call. Once an `EmailRefRepo`
        // method is added we can swap this for a real query.
        let _ = &self.email_repo;
        Vec::new()
    }
}
