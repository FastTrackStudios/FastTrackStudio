//! Cycles (sprints) — time-boxed containers for tasks.
//!
//! A cycle has a start and end date and contains a subset of project tasks.
//! Tasks can belong to at most one active cycle.
//!
//! Stored as `cycles/<name>.md` in the project folder.

use chrono::NaiveDate;
use facet::Facet;

/// A time-boxed work cycle (sprint).
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Cycle {
    pub title: String,
    pub description: Option<String>,
    pub start_date: Option<NaiveDate>,
    pub end_date: Option<NaiveDate>,

    /// Who owns/manages this cycle.
    pub owned_by: Option<String>,

    /// Task titles or IDs included in this cycle.
    #[facet(default)]
    pub tasks: Vec<String>,

    /// Status: planned, active, completed, cancelled.
    pub status: CycleStatus,

    /// Progress snapshot for reporting.
    pub total_tasks: Option<u32>,
    pub completed_tasks: Option<u32>,

    pub sort_order: Option<f64>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum CycleStatus {
    #[default]
    Planned,
    Active,
    Completed,
    Cancelled,
}

impl Cycle {
    pub fn is_active(&self) -> bool {
        self.status == CycleStatus::Active
    }

    pub fn progress(&self) -> f64 {
        match (self.completed_tasks, self.total_tasks) {
            (Some(done), Some(total)) if total > 0 => done as f64 / total as f64,
            _ => 0.0,
        }
    }

    pub fn days_remaining(&self) -> Option<i64> {
        let today = chrono::Local::now().date_naive();
        self.end_date.map(|end| (end - today).num_days())
    }
}
