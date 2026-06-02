//! `MilestoneService` — wire surface for reading + mutating
//! project milestones. Same shape as the other entity services.

use facet::Facet;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use uuid::Uuid;

use crate::model::Milestone;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet, Error)]
#[repr(u8)]
pub enum MilestoneError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("already exists: {0}")]
    AlreadyExists(String),
    #[error("bad request: {0}")]
    BadRequest(String),
    #[error("io: {0}")]
    Io(String),
}

#[architect::rpc]
pub trait MilestoneService {
    /// Every milestone under the org's vault. Filter
    /// client-side by `project_id` / `goal_id` / status.
    fn list(&self) -> Result<Vec<Milestone>, MilestoneError>;

    fn get(&self, id: Uuid) -> Result<Milestone, MilestoneError>;

    fn get_by_path(&self, path: &str) -> Result<Milestone, MilestoneError>;

    /// Create a milestone. `project_id` is required. Backend
    /// resolves the project's slug to derive the default
    /// `path` (`Projects/<slug>/milestones/<ms-slug>.md`)
    /// when `path` is empty.
    fn create(&self, milestone: Milestone) -> Result<Milestone, MilestoneError>;

    fn update(&self, milestone: Milestone) -> Result<Milestone, MilestoneError>;

    fn rename(&self, id: Uuid, new_path: &str) -> Result<Milestone, MilestoneError>;

    /// Remove. Refuses if any task carries this `milestone_id`
    /// — clear the link on those tasks first.
    fn delete(&self, id: Uuid) -> Result<(), MilestoneError>;
}
