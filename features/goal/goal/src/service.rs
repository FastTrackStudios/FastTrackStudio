//! `GoalService` — wire surface for browsing goals.
//!
//! Read-only for now (mirrors `ProjectService`). Mutation
//! verbs land when the goal editor route arrives.

use facet::Facet;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use uuid::Uuid;

use crate::model::Goal;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet, Error)]
#[repr(u8)]
pub enum GoalError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("bad request: {0}")]
    BadRequest(String),
    #[error("io: {0}")]
    Io(String),
}

#[architect::rpc]
pub trait GoalService {
    /// Every `type: goal` page under the org's vault.
    fn list(&self) -> Result<Vec<Goal>, GoalError>;

    /// One goal by stable UUID.
    fn get(&self, id: Uuid) -> Result<Goal, GoalError>;

    /// One goal by vault-relative path.
    fn get_by_path(&self, path: &str) -> Result<Goal, GoalError>;
}
