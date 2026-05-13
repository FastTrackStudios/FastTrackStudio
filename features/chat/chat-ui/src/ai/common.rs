//! Pure helpers shared across the AI-chat components.
//!
//! No Dioxus state, no async — just classifiers and formatters so the
//! visual layer stays small and testable.

use agent_proto::AgentConversation;
use chrono::{DateTime, Utc};

/// Single-letter avatar fallback. Stable across re-renders so the
/// fallback bg colour doesn't flicker.
pub fn role_avatar_letter(role: &str) -> &'static str {
    match role {
        "user" => "U",
        "assistant" => "A",
        "system" => "S",
        "tool" => "T",
        _ => "?",
    }
}

/// Human label for the role chip / metadata row.
pub fn role_label(role: &str) -> &'static str {
    match role {
        "user" => "You",
        "assistant" => "Assistant",
        "system" => "System",
        "tool" => "Tool",
        _ => "Unknown",
    }
}

/// Short relative-time string. Buckets:
/// - `<60s`     -> "just now"
/// - `<60m`     -> "Nm ago"
/// - `<24h`     -> "Nh ago"
/// - `<48h`     -> "yesterday"
/// - older      -> "%b %-d" (e.g. "May 12")
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

/// `15.3k`, `1.2M`, `42`. No "tokens" suffix — caller decides the
/// trailing label.
pub fn format_token_count(n: u32) -> String {
    if n < 1_000 {
        format!("{n}")
    } else if n < 1_000_000 {
        let k = (n as f64) / 1_000.0;
        format!("{k:.1}k")
    } else {
        let m = (n as f64) / 1_000_000.0;
        format!("{m:.1}M")
    }
}

/// `$0.42`, `$12.05`. No locale awareness — developer dashboard, not a
/// billing surface.
pub fn format_cost(cents: u32) -> String {
    let dollars = (cents as f64) / 100.0;
    format!("${dollars:.2}")
}

/// Conversation-list grouping buckets. Mirrors ChatGPT / Claude.ai
/// sidebar conventions.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConvGroup {
    Today,
    Yesterday,
    LastWeek,
    LastMonth,
    Older,
}

impl ConvGroup {
    pub fn as_str(self) -> &'static str {
        match self {
            ConvGroup::Today => "Today",
            ConvGroup::Yesterday => "Yesterday",
            ConvGroup::LastWeek => "Last 7 Days",
            ConvGroup::LastMonth => "Last 30 Days",
            ConvGroup::Older => "Older",
        }
    }

    fn order(self) -> u8 {
        match self {
            ConvGroup::Today => 0,
            ConvGroup::Yesterday => 1,
            ConvGroup::LastWeek => 2,
            ConvGroup::LastMonth => 3,
            ConvGroup::Older => 4,
        }
    }
}

fn classify(t: DateTime<Utc>, now: DateTime<Utc>) -> ConvGroup {
    let delta = now.signed_duration_since(t);
    let hours = delta.num_hours();
    if hours < 24 {
        ConvGroup::Today
    } else if hours < 48 {
        ConvGroup::Yesterday
    } else if delta.num_days() < 7 {
        ConvGroup::LastWeek
    } else if delta.num_days() < 30 {
        ConvGroup::LastMonth
    } else {
        ConvGroup::Older
    }
}

/// Bucket `AgentConversation`s by `updated_at` into Today / Yesterday
/// / Last 7 Days / Last 30 Days / Older. Returns groups in display
/// order; within each group, the input order is preserved (caller
/// should pre-sort newest-first).
pub fn group_conversations_by_day(
    convs: &[AgentConversation],
    now: DateTime<Utc>,
) -> Vec<(ConvGroup, Vec<AgentConversation>)> {
    use std::collections::BTreeMap;
    let mut buckets: BTreeMap<u8, (ConvGroup, Vec<AgentConversation>)> = BTreeMap::new();
    for c in convs {
        let g = classify(c.updated_at, now);
        buckets
            .entry(g.order())
            .or_insert_with(|| (g, Vec::new()))
            .1
            .push(c.clone());
    }
    buckets.into_values().collect()
}
