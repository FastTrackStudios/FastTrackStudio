//! Storage-agnostic project provider system.
//!
//! The `ProjectProvider` trait abstracts how projects and tasks are stored,
//! allowing the same task management logic to work across:
//!
//! - Local filesystem (NFS mounts, local directories)
//! - Obsidian vaults (personal task files)
//! - S3-compatible object storage (AWS S3, MinIO, R2)
//! - WebDAV / Nextcloud file shares
//! - Git repositories
//!
//! Each provider reads/writes the same `project.md` + `tasks/*.md` format,
//! just on different storage backends.
//!
//! # Architecture
//!
//! ```text
//! ProjectRegistry
//!   ├── LocalProvider      /mnt/starcommand/Projects/
//!   ├── VaultProvider      ~/Documents/The Observatory/TaskNotes/
//!   ├── S3Provider         s3://fasttrack-projects/
//!   ├── WebDavProvider     https://cloud.example.com/remote.php/dav/
//!   └── GitProvider        github.com/FastTrackStudios/projects
//! ```

mod traits;
mod local;
mod vault;
mod registry;
mod s3;
mod webdav;
mod nextcloud;
pub mod nextcloud_sync;
pub mod github;
pub mod talk;
pub mod invoice_ninja;

pub use traits::*;
pub use local::LocalProvider;
pub use vault::VaultProvider;
pub use registry::ProjectRegistry;
pub use s3::{S3Provider, S3Config};
pub use webdav::{WebDavProvider, WebDavConfig};
pub mod mock;
pub use mock::MockProvider;
pub use nextcloud::{NextcloudProvider, NextcloudConfig};
pub use github::{GitHubSync, GitHubConfig, GitHubSyncResult};
pub use talk::{Message as TalkMessage, Room as TalkRoom, TalkClient, TalkConfig};
pub use invoice_ninja::{
    CreatedInvoice, InvoiceDraft, InvoiceLine, InvoiceNinjaClient as InvoiceNinjaClientRow,
    InvoiceNinjaClientApi, InvoiceNinjaConfig,
};
