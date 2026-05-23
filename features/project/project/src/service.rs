//! `ProjectService` — wire surface for browsing projects.
//!
//! Read-only for now (the UI's primary need). Mutation
//! verbs follow when the project-editor route lands; the
//! trait shape mirrors `CookbookService` so the additions
//! land the same way.

use facet::Facet;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use uuid::Uuid;

use crate::model::ProjectInfo;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet, Error)]
#[repr(u8)]
pub enum ProjectError {
    /// Project with the given id / path doesn't exist on the
    /// server.
    #[error("not found: {0}")]
    NotFound(String),
    /// Caller asked for a malformed id or path.
    #[error("bad request: {0}")]
    BadRequest(String),
    /// Anything else — filesystem hiccup, parse failure on a
    /// page the server thought was a project, etc.
    #[error("io: {0}")]
    Io(String),
}

#[architect::rpc]
pub trait ProjectService {
    /// Every project page found under the org's vault. Order
    /// is implementation-defined; callers sort if they need
    /// stability (the typical UI groups by `parent_id` first
    /// anyway).
    fn list(&self) -> Result<Vec<ProjectInfo>, ProjectError>;

    /// One project by stable UUID. Use over [`Self::get_by_path`]
    /// when the caller already has the id — renaming the
    /// markdown file won't break the lookup.
    fn get(&self, id: Uuid) -> Result<ProjectInfo, ProjectError>;

    /// One project by vault-relative path. Convenience for
    /// the link-resolution layer, which sees paths long
    /// before it sees ids.
    fn get_by_path(&self, path: &str) -> Result<ProjectInfo, ProjectError>;
}
