//! `email-proto` — wire contract for the email-sync feature.
//!
//! - [`Account`] / [`AccountId`] — per-mailbox identity
//! - [`Folder`] / [`FolderRole`] — mailbox listing entries
//! - [`Envelope`] / [`Message`] / [`Draft`] / [`Addr`] —
//!   payload shapes
//! - [`Flag`] / [`FlagDelta`] / [`SeqRange`] — operation args
//! - [`EmailEvent`] — live change events for subscribers
//! - [`EmailSyncError`] — trait-boundary error type
//! - [`EmailSync`] — the service trait, decorated with
//!   `#[architect::rpc]`
//!
//! The architect macro derives the async vox face from the
//! sync `EmailSync` trait: backends impl `EmailSync` directly,
//! in-process callers use it as a plain sync API, and remote
//! callers reach the same surface via the auto-emitted
//! [`EmailSyncClient`] over vox.
//!
//! Mount the server-side backend with [`serve`], or compose
//! through [`Service`] into an `architect::Services` bundle.
//! Mirrors the shape of `vault-proto`.

mod account;
mod draft;
mod envelope;
mod error;
mod event;
mod flag;
mod folder;
mod message;
mod range;
mod service;

pub use account::{Account, AccountId};
pub use draft::{Attachment, AttachmentMeta, Draft};
pub use envelope::{Addr, Envelope};
pub use error::EmailSyncError;
pub use event::EmailEvent;
pub use flag::{Flag, FlagDelta};
pub use folder::{Folder, FolderRole};
pub use message::{Message, MessageId, ThreadId};
pub use range::SeqRange;
pub use service::{EmailSync, EmailSyncRpc};

// architect-emitted vox bits from the auto-generated mirror
// trait. Re-exported with shorter aliases (`Dispatcher`,
// `descriptor`) so consumer mounting code reads
// `email_proto::descriptor()` and `email_proto::serve(state)`
// rather than juggling the underscored mirror names directly.
#[cfg(feature = "vox")]
pub use service::{
    EmailSyncClient, EmailSyncRpcDispatcher as Dispatcher, Service,
    email_sync_rpc_service_descriptor as descriptor, layer, serve,
};
