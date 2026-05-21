//! `vault-proto` — wire contract for the vault-sync
//! feature.
//!
//! - [`Manifest`] / [`ManifestEntry`] — the file listing
//! - [`FileBytes`] / [`PutAck`] / [`IfMatch`] — file payloads
//!   + the conditional-write mode
//! - [`VaultEvent`] — live change events for subscribers
//! - [`VaultSyncError`] — trait-boundary error type
//! - [`VaultSync`] — the service trait, decorated with
//!   `#[architect::rpc]`
//!
//! The architect macro derives the async vox face from the
//! sync `VaultSync` trait: backends impl `VaultSync` directly,
//! in-process callers use it as a plain sync API, and remote
//! callers reach the same surface via the auto-emitted
//! [`VaultSyncClient`] over vox.
//!
//! Mount the server-side backend with [`serve`], or compose
//! through [`Service`] into an `architect::Services` bundle.

mod error;
mod event;
mod file;
mod manifest;
mod service;

pub use error::VaultSyncError;
pub use event::VaultEvent;
pub use file::{FileBytes, IfMatch, PutAck};
pub use manifest::{Manifest, ManifestEntry};
pub use service::{VaultSync, VaultSyncRpc};

// architect-emitted vox bits from the auto-generated mirror
// trait. Re-exported with shorter aliases (`Dispatcher`,
// `descriptor`) so consumer mounting code reads
// `vault_proto::descriptor()` and
// `vault_proto::serve(state)` rather than juggling the
// underscored mirror names directly. Mirrors the daw
// `marker::serve(Reaper)` shape.
#[cfg(feature = "vox")]
pub use service::{
    Service, VaultSyncClient, VaultSyncRpcDispatcher as Dispatcher, layer, serve,
    vault_sync_rpc_service_descriptor as descriptor,
};
