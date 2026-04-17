//! Modules (epics/features) — logical groupings that span cycles.
//!
//! A module groups related tasks across time. Unlike cycles which are
//! time-boxed, modules are feature-scoped.
//!
//! Stored as `modules/<name>.md` in the project folder.

use chrono::NaiveDate;
use facet::Facet;

/// A feature module / epic.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Module {
    pub title: String,
    pub description: Option<String>,
    pub start_date: Option<NaiveDate>,
    pub target_date: Option<NaiveDate>,

    /// Module lead.
    pub lead: Option<String>,

    /// Team members working on this module.
    #[facet(default)]
    pub members: Vec<String>,

    /// Task titles or IDs in this module.
    #[facet(default)]
    pub tasks: Vec<String>,

    /// Status.
    pub status: ModuleStatus,

    pub sort_order: Option<f64>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum ModuleStatus {
    #[default]
    Backlog,
    Planned,
    InProgress,
    Paused,
    Completed,
    Cancelled,
}
