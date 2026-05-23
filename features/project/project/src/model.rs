//! `ProjectInfo` — canonical project entity.
//!
//! Source of truth: markdown vault page (`Projects/<slug>.md`)
//! with YAML frontmatter. The struct doubles as an
//! `architect::Entity`, so under the `server` feature the
//! SeaORM Model + `ProjectInfoRepoStorage<C>` are emitted —
//! the same schema mounts as a row without rewriting any
//! fields. See `feedback_architect_entity_default`.
//!
//! Field-level `#[serde(...)]` attributes drive frontmatter
//! shape (camelCase, skip-empty); they're stripped from the
//! synthetic `Create`/`Update` payloads by the architect
//! derive (it filters serde out of `forward_attrs`).

use chrono::{DateTime, Utc};
use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// `Vec<String>` newtype so architect can store it as a JSON
/// column. `Vec<T>` can't impl `From<Vec<T>> for sea_orm::Value`
/// directly (orphan rule); the `JsonField` derive emits the
/// four `sea_orm::Value` / `TryGetable` / `ValueType` /
/// `Nullable` impls via a `serde_json` round-trip.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(
    architect::JsonField, Debug, Clone, Default, PartialEq, Eq, Facet, Serialize, Deserialize,
)]
#[repr(transparent)]
#[serde(transparent)]
pub struct Tags(pub Vec<String>);

impl From<Vec<String>> for Tags {
    fn from(v: Vec<String>) -> Self {
        Self(v)
    }
}

impl Tags {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// One project. Lives as `Projects/<slug>.md` in the vault.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(architect::Entity, Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[architect(table_name = "projects", repo)]
pub struct ProjectInfo {
    /// Stable identity. Generated on first write; never
    /// re-derived from the path. Downstream features (timer,
    /// finance) reference projects by this UUID, so renaming
    /// the markdown file doesn't orphan their rows.
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Vault-relative path. Not serialized into frontmatter
    /// (it'd duplicate the on-disk position). Under the
    /// `server` feature this becomes a DB column too, so
    /// reverse lookups (find project by path) are queryable.
    #[serde(skip)]
    #[architect(filterable, sortable)]
    pub path: String,

    #[architect(filterable, sortable, fulltext)]
    pub title: String,

    /// One of [`Status`] as a stringly-typed slug; we accept
    /// any value so backends can add finer states without a
    /// schema bump.
    #[serde(default = "default_status")]
    #[architect(filterable, sortable)]
    pub status: String,

    /// Priority slug (`p0`..`p4`, `urgent`, `high`, `normal`,
    /// `low`, `lowest`). Defaults to `normal`. Used by the
    /// agent-dispatch cron to map onto the AgentTask priority
    /// when a project is dispatched wholesale.
    #[serde(default = "default_priority")]
    #[architect(filterable, sortable)]
    pub priority: String,

    /// Project lead / responsible party. Free-text (often a
    /// `[[User Name]]` wikilink). Multiple leads → join with
    /// `, ` in the frontmatter.
    #[serde(skip_serializing_if = "String::is_empty", default)]
    pub lead: String,

    /// Tags. `project` is conventionally one of them, but
    /// not required — the scanner uses `type: project` OR
    /// `tags: [..., project]` as the discriminator.
    #[serde(skip_serializing_if = "Tags::is_empty", default)]
    #[architect(json)]
    pub tags: Tags,

    /// Federation pointer — `Some("@tombrooksmusic/png-worship-collective-album")`
    /// means this row is a *reference* to the canonical
    /// project owned by another org (e.g. a collaboration
    /// where one org leads and others participate). The
    /// resolver follows the link for full details.
    /// `None` for locally-owned projects.
    ///
    /// Mirror the same value in the body as a `[[@org/slug]]`
    /// wikilink so the page reads correctly in vanilla
    /// Obsidian. See `plans/federated-task-platform.md`
    /// § federated wiki resolution for the `@org/slug`
    /// syntax.
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "sameAs")]
    #[architect(filterable)]
    pub same_as: Option<String>,

    /// Free-text description / body. Same convention as
    /// `task::TaskInfo::details`: everything after the
    /// frontmatter close fence lives here. Persisted in the
    /// DB column for full-text search; the markdown file
    /// remains the authoring surface.
    #[serde(skip)]
    #[architect(fulltext)]
    pub details: String,

    // ── Billing ─────────────────────────────────────────────
    /// Billable client (UUID) — points at a row in the
    /// `timer_clients` DB table. `None` for internal /
    /// non-billable projects (nullable column under SeaORM).
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "clientId")]
    #[architect(filterable)]
    pub client_id: Option<Uuid>,

    /// `true` if work on this project defaults to billable.
    /// Individual work sessions can still override.
    #[serde(default, rename = "billableDefault")]
    #[architect(filterable)]
    pub billable_default: bool,

    /// ISO 4217 currency code. Empty = non-billable or use
    /// org default. Mixing currencies within one project is
    /// forbidden — open a separate project.
    #[serde(skip_serializing_if = "String::is_empty", default)]
    #[architect(filterable)]
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
    #[architect(filterable)]
    pub archived: bool,

    // ── Timestamps ──────────────────────────────────────────
    /// Frontmatter aliases: `dateCreated` / `dateModified`
    /// (kept for back-compat with TaskNotes-style notes).
    /// Optional in the wire format — fresh projects might
    /// not have either set yet; the DB stores them nullable.
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
