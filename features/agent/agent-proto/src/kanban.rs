//! Kanban — mirrors hermes-webui's board model
//! (`api/kanban_bridge.py`). Boards have a fixed column
//! sequence; cards carry parent/child links + comments;
//! cards can optionally link to a session (`linked_session_id`)
//! so an agent's work shows up on the board.
//!
//! The agent does **not** auto-move cards in the Hermes
//! model — humans drag them. Task can opt into agent-driven
//! card transitions later, but the proto leaves it as a
//! curator action.

use chrono::{DateTime, Utc};
use facet::Facet;

/// Default column slugs. Backends are free to override on
/// a per-board basis.
pub const DEFAULT_COLUMNS: &[&str] = &[
    "triage", "todo", "ready", "running", "blocked", "done", "archived",
];

#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct Board {
    /// Stable slug. `"default"` is the canonical fallback.
    pub id: String,
    pub label: String,
    /// Ordered column slugs. The first column is the
    /// inbox; the last is typically archive.
    pub columns: Vec<String>,
    /// Tenant slugs configured for this board (multi-org
    /// scenarios). Empty = single tenant.
    pub tenants: Vec<String>,
    /// Known assignee handles. Backends keep this list
    /// updated as cards are claimed.
    pub assignees: Vec<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub struct BoardView {
    pub board: Board,
    /// Cards grouped by column. Order matches `Board::columns`.
    pub columns: Vec<ColumnView>,
    /// Latest event id observed when this view was built —
    /// clients pass it back on subsequent reads to detect
    /// drift.
    pub latest_event_id: u64,
    /// Whether the board is read-only for the caller.
    pub read_only: bool,
}

#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub struct ColumnView {
    pub name: String,
    pub cards: Vec<Card>,
}

#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub struct Card {
    pub id: String,
    pub board_id: String,
    pub title: String,
    /// Markdown body.
    pub description: String,
    /// Which column the card currently sits in. Matches
    /// one of `Board::columns`.
    pub column: String,
    /// Handle of the responsible party. Empty = unassigned.
    pub assignee: String,
    /// Tenant slug (multi-org). Empty = default tenant.
    pub tenant: String,
    /// 0.0–1.0 completion estimate. `-1.0` ⇒ unknown.
    pub progress: f32,
    /// Optional session this card is being worked in.
    /// Empty when no session is linked.
    pub linked_session_id: String,
    /// Optional project id (sometimes a card is project-
    /// scoped, sometimes board-only).
    pub project_id: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Parent → child dependency. The child is blocked until
/// the parent reaches `done` (or the curator overrides).
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct CardLink {
    pub parent_id: String,
    pub child_id: String,
    pub created_at: DateTime<Utc>,
}

/// One comment on a card. Comments are append-only; edits
/// land as new comments.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct CardComment {
    pub id: String,
    pub card_id: String,
    pub author: String,
    pub text: String,
    pub created_at: DateTime<Utc>,
}

/// Filter applied when reading a board view.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct BoardFilter {
    /// Restrict to a specific tenant. Empty = all.
    pub tenant: String,
    /// Restrict to a specific assignee. Empty = all.
    pub assignee: String,
    /// Include archived cards.
    pub include_archived: bool,
    /// `"only_mine"` shortcut — caller's handle (resolved
    /// by the backend). Empty = no filter.
    pub only_handle: String,
    /// Restrict to cards linked to a session.
    pub linked_session_id: String,
}
