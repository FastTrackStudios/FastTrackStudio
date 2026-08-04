//! Local Maildir backend. Read/write a Maildir tree on disk.
//! First backend implemented — lets the rest of the stack
//! develop against a fixture mailbox with no network.
//!
//! Layout convention:
//! - Each account maps to one root directory.
//! - The root itself is the **INBOX** maildir
//!   (`<root>/{cur,new,tmp}`).
//! - Subfolders use the Maildir++ convention: a sibling
//!   directory whose name starts with `.`. Hierarchy is encoded
//!   in the name with `.` separators
//!   (`.Lists.rust-users`).
//!
//! The read side (`list_folders` / `fetch_envelopes` /
//! `fetch_message` / `fetch_attachment`) walks the tree
//! directly. `send` composes an outbound [`Submit`] transport
//! (SMTP via `email-smtp` in production) with a Sent-folder
//! maildir write + a `NewMessage` event on the changes stream.
//! The remaining writes (`set_flags` / `move_message` /
//! `delete_message` / `append_draft`) return
//! [`email_proto::EmailSyncError::Unsupported`] until phase 2/3;
//! `subscribe` is wired through a per-account broadcast channel
//! but the FS watcher attachment lands later (mirrors
//! `vault::sync::Backend::start_watcher`).

#![cfg(not(target_arch = "wasm32"))]

mod backend;
mod folder;
mod parse;
mod submit;

pub use backend::{AccountEntry, Backend};
pub use folder::FolderName;
pub use submit::Submit;
