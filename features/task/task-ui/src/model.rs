//! Wasm-friendly mirror of `task::TaskInfo`.
//!
//! Field names match TaskNotes frontmatter so a vault-backed
//! consumer can convert via `From` once the sibling `task`
//! crate's wasm gating is lifted (or a `task-proto` crate is
//! extracted).

use chrono::{DateTime, NaiveDate, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TaskInfo {
    pub id: Uuid,
    pub title: String,
    /// Raw status string — see [`Status::from_str`].
    pub status: String,
    /// Raw priority string — see [`Priority::from_str`].
    pub priority: String,
    /// ISO date (YYYY-MM-DD) or full timestamp.
    pub due: Option<String>,
    pub scheduled: Option<String>,
    pub tags: Vec<String>,
    pub contexts: Vec<String>,
    pub projects: Vec<String>,
    pub time_estimate: Option<u32>,
    pub time_entries: Vec<TimeEntry>,
    pub recurrence: Option<String>,
    pub completed_date: Option<NaiveDate>,
    pub date_created: Option<DateTime<Utc>>,
    pub date_modified: Option<DateTime<Utc>>,
    pub details: String,
}

impl TaskInfo {
    #[must_use]
    pub fn new(title: impl Into<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            title: title.into(),
            status: Status::Open.as_str().to_string(),
            priority: Priority::Normal.as_str().to_string(),
            due: None,
            scheduled: None,
            tags: vec![],
            contexts: vec![],
            projects: vec![],
            time_estimate: None,
            time_entries: vec![],
            recurrence: None,
            completed_date: None,
            date_created: Some(chrono::Utc::now()),
            date_modified: Some(chrono::Utc::now()),
            details: String::new(),
        }
    }

    #[must_use]
    pub fn status_enum(&self) -> Status {
        Status::from_str(&self.status).unwrap_or(Status::Open)
    }

    #[must_use]
    pub fn priority_enum(&self) -> Priority {
        Priority::from_str(&self.priority).unwrap_or(Priority::Normal)
    }

    /// Parse `due` as a `NaiveDate`. Accepts plain `YYYY-MM-DD`
    /// or strips the time off a full ISO timestamp.
    #[must_use]
    pub fn due_date(&self) -> Option<NaiveDate> {
        let s = self.due.as_deref()?;
        s.get(..10).and_then(|d| d.parse::<NaiveDate>().ok())
    }

    /// Canonical state group for this task's raw status —
    /// progress classification goes through this, not the
    /// `Status` enum, so custom status names (e.g. `shipped`,
    /// `qa-review`) classify sensibly. Uses the default
    /// registry; components are project-agnostic (props-driven),
    /// so per-project registries resolve upstream.
    #[must_use]
    pub fn state_group(&self) -> project::StateGroup {
        project::resolve_state_group(None, &self.status)
    }

    #[must_use]
    pub fn is_done(&self) -> bool {
        self.state_group() == project::StateGroup::Completed
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct TimeEntry {
    #[serde(rename = "startTime")]
    pub start_time: DateTime<Utc>,
    #[serde(rename = "endTime", skip_serializing_if = "Option::is_none", default)]
    pub end_time: Option<DateTime<Utc>>,
}

/// Built-in status set — parsing accepts any string and
/// falls back to the raw form so unknown custom statuses
/// (`waiting-on-design`, `qa`, etc.) round-trip.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Status {
    Open,
    InProgress,
    Waiting,
    Done,
    Cancelled,
}

impl Status {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Open => "open",
            Self::InProgress => "in-progress",
            Self::Waiting => "waiting",
            Self::Done => "done",
            Self::Cancelled => "cancelled",
        }
    }

    #[must_use]
    pub fn label(self) -> &'static str {
        match self {
            Self::Open => "Open",
            Self::InProgress => "In progress",
            Self::Waiting => "Waiting",
            Self::Done => "Done",
            Self::Cancelled => "Cancelled",
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "open" | "todo" | "none" => Some(Self::Open),
            "in-progress" | "in_progress" | "doing" => Some(Self::InProgress),
            "waiting" | "blocked" => Some(Self::Waiting),
            "done" | "completed" | "complete" => Some(Self::Done),
            "cancelled" | "canceled" => Some(Self::Cancelled),
            _ => None,
        }
    }

    #[must_use]
    pub fn is_done(self) -> bool {
        matches!(self, Self::Done)
    }

    /// Default board column order — left to right in the kanban.
    pub const BOARD_ORDER: &'static [Self] =
        &[Self::Open, Self::InProgress, Self::Waiting, Self::Done];
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Priority {
    None,
    Low,
    Normal,
    High,
    Critical,
}

impl Priority {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::None => "none",
            Self::Low => "low",
            Self::Normal => "normal",
            Self::High => "high",
            Self::Critical => "critical",
        }
    }

    #[must_use]
    pub fn label(self) -> &'static str {
        match self {
            Self::None => "None",
            Self::Low => "Low",
            Self::Normal => "Normal",
            Self::High => "High",
            Self::Critical => "Critical",
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "none" | "" => Some(Self::None),
            "low" => Some(Self::Low),
            "normal" | "medium" | "med" => Some(Self::Normal),
            "high" => Some(Self::High),
            "critical" | "urgent" => Some(Self::Critical),
            _ => None,
        }
    }
}
