//! `project-proto` — wire contract for the project feature.
//!
//! Minimal vertical-slice domain:
//!
//! - [`Project`] — the container, just `{ id, name }`.
//! - [`Task`] — work item bound to a project, just `{ id,
//!   project_id, title, done }`.
//!
//! Each entity is a separate `architect::Entity` with its own Repo
//! trait (auto-emitted: `*RepoClient`, `*RepoDispatcher`,
//! `*Create`, `*Update`, `*List`, etc.).
//!
//! [`WorkspaceSync`] is a hand-written `#[vox::service]` trait
//! that bridges the server's `LoroDoc` update stream into a vox
//! `Tx<UpdateBytes>` channel and accepts client pushes via
//! `apply_update`. That's the realtime+collaborative layer.

pub use architect;

mod project;
mod share;
mod sync;
mod task;

pub use project::*;
pub use share::*;
pub use sync::*;
pub use task::*;
