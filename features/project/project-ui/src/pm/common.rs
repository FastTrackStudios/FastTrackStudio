//! Shared helpers used across the `pm` submodules.
//!
//! Pure functions; no state, no IO. Keep this thin — anything that
//! needs RSX or a signal belongs in a component module.

use chrono::{DateTime, Utc};
use fts_ui::prelude::StatusBadgeVariant;
use uuid::Uuid;

/// Short relative-time formatter used by the Git panel and likely
/// other future surfaces. Mirrors the bucketing used in
/// `agent-ui::hermes_kit::common::format_relative_time` so the two
/// look identical side-by-side.
pub fn relative_time(t: DateTime<Utc>, now: DateTime<Utc>) -> String {
    let delta = now.signed_duration_since(t);
    let secs = delta.num_seconds();
    if secs < 60 {
        return "just now".to_string();
    }
    let mins = delta.num_minutes();
    if mins < 60 {
        return format!("{mins}m ago");
    }
    let hours = delta.num_hours();
    if hours < 24 {
        return format!("{hours}h ago");
    }
    if hours < 48 {
        return "yesterday".to_string();
    }
    t.format("%b %-d").to_string()
}

/// Short, stable, human-recognisable ID chip — first 4 hex of the UUID
/// uppercased, prefixed `PRJ-`. Designed for table rows and tooltips.
pub fn task_id_chip(id: Uuid) -> String {
    let hex = id.simple().to_string();
    let head: String = hex.chars().take(4).collect();
    format!("PRJ-{}", head.to_uppercase())
}

/// Map a free-form status string to a `StatusBadgeVariant`. Linear /
/// shadcn convention: `done`/`completed` neutral, `in-progress` warning,
/// `blocked`/`cancelled` danger, anything else success (active default).
pub fn status_to_badge_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "done" | "completed" | "archived" => StatusBadgeVariant::Neutral,
        "in-progress" | "in-review" | "active" => StatusBadgeVariant::Success,
        "todo" | "planning" | "upcoming" | "on-hold" => StatusBadgeVariant::Warning,
        "blocked" | "cancelled" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral,
    }
}

/// Deterministically map an optional project color (hex like `#06b6d4`)
/// to a Tailwind background class from a fixed 12-color palette. The
/// hex is parsed if it parses; otherwise we fall back to a hash of the
/// raw string. Empty / `None` → `bg-primary`.
pub fn color_swatch_class(color: Option<&str>) -> String {
    let palette = [
        "bg-red-500",
        "bg-orange-500",
        "bg-amber-500",
        "bg-yellow-500",
        "bg-lime-500",
        "bg-green-500",
        "bg-emerald-500",
        "bg-teal-500",
        "bg-cyan-500",
        "bg-sky-500",
        "bg-blue-500",
        "bg-violet-500",
    ];
    let s = match color {
        Some(s) if !s.trim().is_empty() => s.trim(),
        _ => return "bg-primary".to_string(),
    };
    // Cheap stable hash — sum of bytes, modulo palette length.
    let n: usize = s.bytes().map(|b| b as usize).sum();
    palette[n % palette.len()].to_string()
}

/// Tiny substring-based fuzzy score. Case-insensitive. Returns `None`
/// when the query doesn't appear in any haystack (so callers can skip
/// the item), or `Some(score)` where lower is better.
///
/// Score = byte offset of the first match, with prefix matches
/// (offset 0) ranked best. Identical to a naive contains-and-rank
/// — deliberately simple so we don't pull in a fuzzy crate just for
/// the command palette.
pub fn fuzzy_match(query: &str, haystack: &str) -> Option<usize> {
    let q = query.trim().to_lowercase();
    if q.is_empty() {
        return Some(0);
    }
    let h = haystack.to_lowercase();
    h.find(&q)
}
