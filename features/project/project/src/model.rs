//! `ProjectInfo` — parsed project frontmatter.
//!
//! Field names are camelCase on disk to match TaskNotes
//! conventions: `defaultRateCents`, `billableDefault`,
//! `clientId`. The model rejects nothing — unrecognized
//! frontmatter keys round-trip through the body untouched
//! (we serialize only the named fields; anything else stays
//! in the original raw file when the caller uses surgical
//! edits via `vault-obsidian::set_property`).

use chrono::{DateTime, Utc};
use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// One project. Lives as `Projects/<slug>.md` in the vault.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ProjectInfo {
    /// Vault-relative path. Not serialized into frontmatter
    /// (it'd duplicate the on-disk position).
    #[serde(skip)]
    pub path: String,

    /// Stable identity. Generated on first write; never
    /// re-derived from the path. Downstream features (timer,
    /// finances) reference projects by this UUID, so renaming
    /// the markdown file doesn't orphan their rows.
    pub id: Uuid,

    pub title: String,

    /// One of [`Status`] as a stringly-typed slug; we accept
    /// any value so backends can add finer states without a
    /// schema bump.
    #[serde(default = "default_status")]
    pub status: String,

    /// Priority slug (`p0`..`p4`, `urgent`, `high`, `normal`,
    /// `low`, `lowest`). Defaults to `normal`. Used by the
    /// agent-dispatch cron to map onto the AgentTask priority
    /// when a project is dispatched wholesale.
    #[serde(default = "default_priority")]
    pub priority: String,

    /// Project lead / responsible party. Free-text (often a
    /// `[[User Name]]` wikilink). Multiple leads → join with
    /// `, ` in the frontmatter.
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub lead: String,

    /// Tags. `project` is conventionally one of them, but
    /// not required — the scanner uses `type: project` OR
    /// `tags: [..., project]` as the discriminator.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub tags: Vec<String>,

    /// Free-text description / body. Same convention as
    /// `task::TaskInfo::details`: everything after the
    /// frontmatter close fence lives here.
    #[serde(skip)]
    pub details: String,

    // ── Billing ─────────────────────────────────────────────
    /// Billable client (UUID) — points at a row in the
    /// `timer_clients` DB table. Empty / nil for internal /
    /// non-billable projects.
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "clientId")]
    pub client_id: Option<Uuid>,

    /// `true` if work on this project defaults to billable.
    /// Individual work sessions can still override.
    #[serde(default, rename = "billableDefault")]
    pub billable_default: bool,

    /// ISO 4217 currency code. Empty = non-billable or use
    /// org default. Mixing currencies within one project is
    /// forbidden — open a separate project.
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub currency: String,

    /// Default hourly rate in cents. `0` = no project-level
    /// default; the rate cascade falls back to org / member
    /// rates. Snapshotted into `WorkSession.rate_cents` on
    /// close so retroactively changing the project rate
    /// doesn't re-bill old work.
    #[serde(default, rename = "defaultRateCents")]
    pub default_rate_cents: i64,

    /// Estimated total time in seconds. Drives "X of Y hours"
    /// indicators in the timer UI. `0` = no estimate.
    #[serde(default, rename = "estimatedSeconds")]
    pub estimated_seconds: i64,

    // ── Agent dispatch ──────────────────────────────────────
    /// Default agent profile for tasks dispatched under this
    /// project. Empty = inherit from the task note.
    #[serde(
        skip_serializing_if = "String::is_empty",
        default,
        rename = "agentProfile"
    )]
    pub agent_profile: String,

    // ── UI ──────────────────────────────────────────────────
    /// Hex `#RRGGBB`. Empty = UI auto-picks from title hash.
    /// Used by the kanban + timer reports for column / pill
    /// colours.
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub color: String,

    /// `false` while active. `true` once the project is
    /// closed out — kept on disk for historical timesheet
    /// integrity; new work sessions against an archived
    /// project are refused by the timer service.
    #[serde(default)]
    pub archived: bool,

    // ── Timestamps ──────────────────────────────────────────
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
}

fn default_status() -> String {
    Status::Active.as_str().to_string()
}

fn default_priority() -> String {
    "normal".to_string()
}

/// Built-in status values. Parsing accepts any string; these
/// are the recognized canonical forms.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Status {
    /// Default for newly-created projects.
    Active,
    /// Hand-off complete, awaiting client sign-off, etc.
    OnHold,
    /// Finished + invoiced.
    Done,
    /// Cancelled without delivery.
    Cancelled,
}

impl Status {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Active => "active",
            Self::OnHold => "on_hold",
            Self::Done => "done",
            Self::Cancelled => "cancelled",
        }
    }

    #[allow(clippy::should_implement_trait)]
    #[must_use]
    pub fn from_str(s: &str) -> Option<Self> {
        Some(match s.to_ascii_lowercase().as_str() {
            "active" | "open" | "in_progress" => Self::Active,
            "on_hold" | "on-hold" | "paused" | "waiting" => Self::OnHold,
            "done" | "complete" | "completed" | "shipped" => Self::Done,
            "cancelled" | "canceled" | "abandoned" => Self::Cancelled,
            _ => return None,
        })
    }

    /// `true` once the project no longer accepts new work.
    #[must_use]
    pub fn is_closed(self) -> bool {
        matches!(self, Self::Done | Self::Cancelled)
    }
}
