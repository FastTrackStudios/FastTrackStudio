// architect's HasDispatcher/rpc derives emit cfg-gated blocks; allow at
// crate scope (same convention as `milestone` / `task`).
#![allow(unexpected_cfgs)]

//! Server-side half of the Files feature (issue #259, ADR 0001 —
//! `apps/task/docs/adr/0001-files-version-store-jj-cas.md`). The
//! wasm-clean wire surface ([`files_proto::FilesService`] + its model
//! types) lives in the sibling `files-proto` crate; this crate is
//! [`FilesBackend`] — the version-store-backed implementation — plus
//! the plumbing it needs: `registry` (root identity, persisted
//! alongside the version stores) and `repo_open` (opening/reopening
//! a root's jj repo).
//!
//! Issue #260 added the **cadence engine** on top: [`cadence`] decides
//! when a root's session takes an auto-snapshot and when it ends in a
//! Session checkpoint, [`ignore`] holds the per-root Ignore set that
//! keeps junk out of the store entirely, and [`certify`] is the stat
//! sandwich that stops a file being written right now from entering a
//! version torn.

mod backend;
pub mod cadence;
pub mod certify;
mod checkpoint;
mod consts;
mod error;
pub mod ignore;
mod registry;
mod repo_open;
mod scan;

pub use backend::{Captured, FilesBackend};
pub use cadence::{CadenceConfig, CadenceEngine, Clock, SystemClock, TestClock};
pub use error::{Error, Result};
pub use files_proto::service;
pub use ignore::IgnoreSet;

pub use files_proto::{
    BrowseEntry, ChainEntry, CheckpointInfo, FileRootInfo, FilesError, FilesEvent, FilesService,
    RootFlavor, SavePoint, SnapshotInfo,
};

// architect-emitted vox bits: the async client / dispatcher / descriptor
// / serve helpers. Mount sites stitch the descriptor + `serve` into the
// org router; the CLI / web UI bind the client.
pub use files_proto::{
    FilesDispatcher, FilesServiceBridge, FilesServiceClient, files_service_descriptor,
    files_service_layer, serve_files_service,
};

// `#[subscribe] fn events` stream sibling — live root/checkpoint
// changes. Mount `files_service_stream_layer(backend)` next to the base
// service; subscribers drive a `FilesServiceStreamClient`.
pub use files_proto::{
    FilesServiceStream, FilesServiceStreamClient, FilesServiceStreamSource,
    files_service_stream_layer, files_stream_descriptor, serve_files_service_stream,
};
