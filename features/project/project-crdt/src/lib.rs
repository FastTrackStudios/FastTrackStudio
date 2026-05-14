//! Loro-backed source-of-truth for the project feature.
//!
//! Two entities, two `EntityCrdt` impls, two `*RepoLoro` newtypes —
//! one file per entity. Minimal vertical-slice domain so the sync
//! stack can be verified end-to-end without the noise of a full PM
//! model.

mod project;
mod task;

pub use crdt::{CrdtDoc, LoroRepo};
pub use project::{ProjectEntity, ProjectRepoLoro};
pub use task::{TaskEntity, TaskRepoLoro};
