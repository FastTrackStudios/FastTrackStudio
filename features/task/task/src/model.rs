//! `TaskInfo` data model.
//!
//! Mirrors `callumalpass/tasknotes`'s `TaskInfo` interface
//! (`src/types.ts:453`) so existing `TaskNotes` vaults round-trip.
//! Field names are the same — `due` not `dueDate`, `scheduled`
//! not `start`, `timeEntries` not `time_entries`. Configurable
//! field-mapping (à la `TaskNotes`' `FieldMapper`) is a future
//! addition; v1 fixes the names to `TaskNotes` defaults.

use chrono::{DateTime, NaiveDate, Utc};
use facet::Facet;
use serde::{Deserialize, Serialize};

/// One task. Most fields are optional — the discriminator is
/// usually just `tags: [..., task]` or `type: task` on the page.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct TaskInfo {
    /// Vault-relative path of the markdown file backing this task
    /// (e.g. `tasks/buy-milk.md`). Populated by the scanner; not
    /// serialized into frontmatter.
    #[serde(skip)]
    pub path: String,

    pub title: String,

    /// `"open"` / `"in-progress"` / `"done"` etc. Free-form so
    /// custom statuses (e.g. `"waiting"`, `"blocked"`) round-trip.
    /// Parsing prefers the [`Status`] enum but stores the raw
    /// string so unknown values survive.
    pub status: String,

    /// `"none"` / `"low"` / `"normal"` / `"high"` / `"critical"`
    /// — free-form like `status`.
    #[serde(default = "default_priority")]
    pub priority: String,

    /// Due date (YYYY-MM-DD or full ISO timestamp).
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub due: Option<String>,

    /// When you plan to work on it (YYYY-MM-DD or ISO).
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub scheduled: Option<String>,

    /// Frontmatter `tags:` array. `"task"` is the conventional
    /// discriminator; everything else is user-defined.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub tags: Vec<String>,

    /// GTD-style contexts (e.g. `"@shopping"`, `"@dev"`).
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub contexts: Vec<String>,

    /// Project wikilinks (e.g. `"[[Website Redesign]]"`).
    /// Stored verbatim — the wikilink-to-page resolution lives in
    /// `vault-obsidian::links`.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub projects: Vec<String>,

    /// Estimated work in minutes.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "timeEstimate"
    )]
    pub time_estimate: Option<u32>,

    /// One row per work session. Append-only; reading + summing
    /// is the consumer's job.
    #[serde(skip_serializing_if = "Vec::is_empty", default, rename = "timeEntries")]
    pub time_entries: Vec<TimeEntry>,

    /// RFC 5545 RRULE (e.g. `"FREQ=WEEKLY;BYDAY=MO"`).
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub recurrence: Option<String>,

    /// `"scheduled"` (fixed) or `"completion"` (flexible). Only
    /// meaningful when `recurrence` is set.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "recurrence_anchor"
    )]
    pub recurrence_anchor: Option<String>,

    /// YYYY-MM-DD dates when each recurrence instance completed.
    #[serde(
        skip_serializing_if = "Vec::is_empty",
        default,
        rename = "complete_instances"
    )]
    pub complete_instances: Vec<String>,

    /// YYYY-MM-DD when this task moved to `status: done`.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "completedDate"
    )]
    pub completed_date: Option<NaiveDate>,

    /// File-created ISO timestamp. Re-derived from `file.ctime`
    /// when missing — kept in the frontmatter so it round-trips
    /// across machines (mtime / ctime is per-filesystem).
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateCreated"
    )]
    pub date_created: Option<DateTime<Utc>>,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateModified"
    )]
    pub date_modified: Option<DateTime<Utc>>,

    /// Body text (the markdown after the frontmatter close
    /// fence). Populated by `parse_page`; written verbatim by
    /// `serialize_task`.
    #[serde(skip)]
    pub details: String,
}

fn default_priority() -> String {
    "normal".to_string()
}

/// Single time-tracking session. `endTime` is `None` while the
/// timer is running.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct TimeEntry {
    #[serde(rename = "startTime")]
    pub start_time: DateTime<Utc>,
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "endTime")]
    pub end_time: Option<DateTime<Utc>>,
}

/// Built-in status values. Parsing accepts any string;
/// these are the recognized canonical forms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Status {
    Open,
    InProgress,
    Done,
    Cancelled,
    Waiting,
}

impl Status {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Open => "open",
            Self::InProgress => "in-progress",
            Self::Done => "done",
            Self::Cancelled => "cancelled",
            Self::Waiting => "waiting",
        }
    }

    /// Parse the canonical status set. Returns `None` for
    /// unknown statuses — callers keep the raw string.
    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "open" | "todo" | "none" => Some(Self::Open),
            "in-progress" | "in_progress" | "doing" => Some(Self::InProgress),
            "done" | "completed" | "complete" => Some(Self::Done),
            "cancelled" | "canceled" => Some(Self::Cancelled),
            "waiting" | "blocked" => Some(Self::Waiting),
            _ => None,
        }
    }

    #[must_use]
    pub fn is_done(self) -> bool {
        matches!(self, Self::Done)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
