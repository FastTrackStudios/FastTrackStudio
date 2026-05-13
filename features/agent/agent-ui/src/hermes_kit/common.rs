//! Pure helpers shared across the hermes_kit components.
//!
//! No Dioxus state, no async, no HTTP — just classifiers and
//! formatters so the visual layer stays small.

use chrono::{DateTime, Utc};
use fts_ui::prelude::StatusBadgeVariant;

/// Map an `AgentRun.status` string onto the design-system badge
/// variant. Unknown values fall back to `Neutral`.
pub fn run_status_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "running" | "succeeded" | "completed" | "success" => StatusBadgeVariant::Success,
        "blocked" => StatusBadgeVariant::Warning,
        "failed" | "error" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral, // queued / cancelled / archived / anything else
    }
}

/// Colour class for a log line based on its level. Unknown levels
/// fall back to the muted foreground.
pub fn log_level_class(level: &str) -> &'static str {
    match level {
        "info" => "text-muted-foreground",
        "tool" => "text-sky-400",
        "stdout" => "text-foreground",
        "stderr" => "text-amber-400",
        "error" => "text-rose-400",
        _ => "text-muted-foreground",
    }
}

/// Format a timestamp as a short human-readable relative string.
///
/// Buckets: "just now" (<60s), "Nm ago" (<60m), "Nh ago" (<24h),
/// "yesterday" (<48h), then "%b %-d".
pub fn format_relative_time(t: DateTime<Utc>, now: DateTime<Utc>) -> String {
    let delta = now.signed_duration_since(t);
    let secs = delta.num_seconds();
    if secs < 0 {
        // Future timestamps render as "just now" — the producer is
        // ahead of our local clock; no need to expose negative deltas
        // to the user.
        return "just now".to_string();
    }
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

/// `$0.42`, `$12.05`. No locale awareness — this is a developer
/// dashboard, not a billing surface.
pub fn format_cost_cents(cents: u32) -> String {
    let dollars = (cents as f64) / 100.0;
    format!("${dollars:.2}")
}

/// `123 tokens`, `1.2k tokens`, `15.3k tokens`. Switches to a
/// thousands suffix above 1000 so headers don't wrap.
pub fn format_tokens(n: u32) -> String {
    if n < 1000 {
        format!("{n} tokens")
    } else {
        let k = (n as f64) / 1000.0;
        format!("{k:.1}k tokens")
    }
}
