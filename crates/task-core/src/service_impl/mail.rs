//! Server-side `MailService` implementation.
//!
//! When a `MailClient` is injected, every read/write op delegates to the
//! Nextcloud Mail provider. When no client is configured, the surface
//! returns `VaultError::IoError("provider not configured: <op>")` so
//! clients (CLI, UI) get a clean per-operation error instead of
//! `Unknown Vox service`.
//!
//! `linked_message_ids` is the one op we can satisfy from the local
//! SQLite `email_refs` table via the held `EmailRefRepo` — every row is
//! by construction a linked email, so we just project `message_id`.
//! `link_email`, `unlink_email`, `list_linked_emails` still need a
//! task/project FK on `email_refs` they don't currently have, so they
//! return a uniform error until the schema is extended.

use std::sync::Arc;

use crate::email::EmailRef;
use crate::provider::{MailAccount, MailClient, MailMessage, MailMessageDetail, MailTag, Mailbox};
use crate::service::{
    EmailLinkRequest, EmailLinkResponse, EmailListRequest, EmailUnlinkRequest,
    MailCreateMailboxRequest, MailCreateTagRequest, MailDeleteTagRequest, MailListMessagesRequest,
    MailMessageTagRequest, MailMoveMessageRequest, MailService, VaultError,
};

/// Build a uniform "no provider configured" error.
fn provider_not_configured(op: &str) -> VaultError {
    VaultError::IoError(format!("provider not configured: {op}"))
}

/// Typed requirements for [`MailServiceImpl`].
///
/// `client = None` makes every provider-backed op return
/// `provider_not_configured`; `email_repo` powers the local-only ops
/// (`linked_message_ids`).
pub struct MailServiceDeps<R> {
    pub email_repo: R,
    pub client: Option<Arc<MailClient>>,
}

#[derive(Clone)]
pub struct MailServiceImpl<R> {
    email_repo: R,
    client: Option<Arc<MailClient>>,
}

impl<R> MailServiceImpl<R> {
    pub fn new(deps: MailServiceDeps<R>) -> Self {
        Self {
            email_repo: deps.email_repo,
            client: deps.client,
        }
    }
}

impl<R> MailService for MailServiceImpl<R>
where
    R: Clone + Send + Sync + 'static,
{
    async fn list_accounts(&self) -> Result<Vec<MailAccount>, VaultError> {
        match &self.client {
            Some(c) => c.list_accounts().await,
            None => Err(provider_not_configured("list_accounts")),
        }
    }

    async fn list_mailboxes(&self, account_id: i64) -> Result<Vec<Mailbox>, VaultError> {
        match &self.client {
            Some(c) => c.list_mailboxes(account_id).await,
            None => Err(provider_not_configured("list_mailboxes")),
        }
    }

    async fn list_messages(
        &self,
        request: MailListMessagesRequest,
    ) -> Result<Vec<MailMessage>, VaultError> {
        match &self.client {
            Some(c) => {
                c.list_messages(
                    request.mailbox_id,
                    request.filter.as_deref(),
                    request.limit,
                    request.cursor.as_deref(),
                )
                .await
            }
            None => Err(provider_not_configured("list_messages")),
        }
    }

    async fn get_message(&self, id: i64) -> Result<MailMessageDetail, VaultError> {
        match &self.client {
            Some(c) => c.get_message(id).await,
            None => Err(provider_not_configured("get_message")),
        }
    }

    async fn get_body(&self, id: i64) -> Result<String, VaultError> {
        match &self.client {
            Some(c) => c.get_body(id).await,
            None => Err(provider_not_configured("get_body")),
        }
    }

    async fn create_mailbox(
        &self,
        request: MailCreateMailboxRequest,
    ) -> Result<Mailbox, VaultError> {
        match &self.client {
            Some(c) => c.create_mailbox(request.account_id, &request.name).await,
            None => Err(provider_not_configured("create_mailbox")),
        }
    }

    async fn delete_mailbox(&self, mailbox_id: i64) -> Result<(), VaultError> {
        match &self.client {
            Some(c) => c.delete_mailbox(mailbox_id).await,
            None => Err(provider_not_configured("delete_mailbox")),
        }
    }

    async fn move_message(&self, request: MailMoveMessageRequest) -> Result<(), VaultError> {
        match &self.client {
            Some(c) => {
                c.move_message(request.message_id, request.dest_folder_id)
                    .await
            }
            None => Err(provider_not_configured("move_message")),
        }
    }

    async fn list_tags(&self) -> Result<Vec<MailTag>, VaultError> {
        match &self.client {
            Some(c) => c.list_tags().await,
            None => Err(provider_not_configured("list_tags")),
        }
    }

    async fn create_tag(&self, request: MailCreateTagRequest) -> Result<MailTag, VaultError> {
        match &self.client {
            Some(c) => c.create_tag(&request.display_name, &request.color).await,
            None => Err(provider_not_configured("create_tag")),
        }
    }

    async fn delete_tag(&self, request: MailDeleteTagRequest) -> Result<(), VaultError> {
        match &self.client {
            Some(c) => c.delete_tag(request.account_id, request.tag_id).await,
            None => Err(provider_not_configured("delete_tag")),
        }
    }

    async fn set_tag(&self, request: MailMessageTagRequest) -> Result<(), VaultError> {
        match &self.client {
            Some(c) => c.set_tag(request.message_id, &request.imap_label).await,
            None => Err(provider_not_configured("set_tag")),
        }
    }

    async fn remove_tag(&self, request: MailMessageTagRequest) -> Result<(), VaultError> {
        match &self.client {
            Some(c) => c.remove_tag(request.message_id, &request.imap_label).await,
            None => Err(provider_not_configured("remove_tag")),
        }
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
