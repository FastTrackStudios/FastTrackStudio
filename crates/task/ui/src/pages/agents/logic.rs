//! Pure, tested derivation logic for the agent command center —
//! the t3code `*.logic.ts` pattern: components stay thin `rsx!`,
//! everything decidable lives here as plain functions.

use agent_proto::session::SessionStatus;

/// Weighted completion ranking (CodexMonitor's `scoreMatch`):
/// exact > name-prefix > substring > subsequence. `None` = no match.
pub fn score_match(query: &str, name: &str) -> Option<i32> {
    let q = query.to_lowercase();
    let n = name.to_lowercase();
    if q.is_empty() {
        return Some(0);
    }
    if n == q {
        return Some(110);
    }
    if n.starts_with(&q) {
        return Some(95);
    }
    if n.contains(&q) {
        return Some(70);
    }
    // Subsequence: every query char appears in order.
    let mut chars = n.chars();
    if q.chars().all(|qc| chars.by_ref().any(|nc| nc == qc)) {
        return Some(50);
    }
    None
}

/// Rank rows by `score_match` over their name, stable-sorted by
/// score desc then name asc, capped at `max`.
pub fn rank_by<T>(items: &[T], query: &str, name_of: impl Fn(&T) -> String, max: usize) -> Vec<T>
where
    T: Clone,
{
    let mut scored: Vec<(i32, String, T)> = items
        .iter()
        .filter_map(|it| {
            let name = name_of(it);
            score_match(query, &name).map(|s| (s, name, it.clone()))
        })
        .collect();
    scored.sort_by(|a, b| b.0.cmp(&a.0).then_with(|| a.1.cmp(&b.1)));
    scored.into_iter().take(max).map(|(_, _, it)| it).collect()
}

/// One rail status pill (t3code's priority-ranked model, adapted
/// to our single-valued `SessionStatus`).
pub struct StatusPill {
    pub label: &'static str,
    /// Tailwind classes for the dot.
    pub dot: &'static str,
    pub pulse: bool,
}

pub fn status_pill(status: SessionStatus) -> Option<StatusPill> {
    match status {
        SessionStatus::AwaitingUser => Some(StatusPill {
            label: "Awaiting input",
            dot: "bg-amber-500",
            pulse: true,
        }),
        SessionStatus::Running => Some(StatusPill {
            label: "Working",
            dot: "bg-primary",
            pulse: true,
        }),
        SessionStatus::Errored => Some(StatusPill {
            label: "Failed",
            dot: "bg-destructive",
            pulse: false,
        }),
        SessionStatus::Idle | SessionStatus::Cancelled => None,
    }
}

/// Deterministic hash→HSL for stable per-identity colors
/// (CodexMonitor's subagent pills). Returns (hue, sat%, light%).
pub fn hash_hsl(identity: &str) -> (u32, u32, u32) {
    let mut h: u32 = 2166136261;
    for b in identity.bytes() {
        h ^= u32::from(b);
        h = h.wrapping_mul(16777619);
    }
    (h % 360, 55 + (h / 360) % 20, 45 + (h / 7200) % 15)
}

/// Free-context percentage for the ring; `None` when the window is
/// unknown (UI falls back to a raw counter).
pub fn context_free_percent(used_tokens: u64, context_window: u64) -> Option<f64> {
    if context_window == 0 {
        return None;
    }
    let used = (used_tokens as f64 / context_window as f64 * 100.0).min(100.0);
    Some(100.0 - used)
}

/// Inline style for the CSS-only context ring (CodexMonitor's
/// conic-gradient trick: one percentage drives both the arc sweep
/// and the red→green hue).
pub fn context_ring_style(free_percent: f64) -> String {
    let hue = 120.0 * free_percent / 100.0;
    format!(
        "background: radial-gradient(closest-side, var(--card, #111) 60%, transparent 61%), \
         conic-gradient(hsl({hue:.0} 70% 45%) {free_percent:.1}%, \
         color-mix(in srgb, currentColor 15%, transparent) 0);"
    )
}

pub fn fmt_elapsed(secs: i64) -> String {
    format!("{}:{:02}", secs / 60, secs % 60)
}

pub fn fmt_tokens(n: u64) -> String {
    if n >= 1000 {
        format!("{:.1}k", n as f64 / 1000.0)
    } else {
        n.to_string()
    }
}

/// "44m" / "11h" / "4d" style relative timestamps for the rail.
pub fn relative_time(t: chrono::DateTime<chrono::Utc>) -> String {
    let secs = (chrono::Utc::now() - t).num_seconds().max(0);
    match secs {
        0..=59 => "now".to_string(),
        60..=3599 => format!("{}m", secs / 60),
        3600..=86_399 => format!("{}h", secs / 3600),
        _ => format!("{}d", secs / 86_400),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn score_orders_exact_prefix_substring_subsequence() {
        assert_eq!(score_match("learn", "learn"), Some(110));
        assert_eq!(score_match("lea", "learn"), Some(95));
        assert_eq!(score_match("arn", "learn"), Some(70));
        assert_eq!(score_match("lrn", "learn"), Some(50));
        assert_eq!(score_match("xyz", "learn"), None);
    }

    #[test]
    fn rank_by_sorts_and_caps() {
        let items = vec!["learn", "clean", "lean-startup", "unrelated"];
        let ranked = rank_by(&items, "lean", |s| (*s).to_string(), 2);
        // "lean-startup" is a prefix match (95); "clean" substring (70);
        // "learn" subsequence (50); "unrelated" none.
        assert_eq!(ranked, vec!["lean-startup", "clean"]);
    }

    #[test]
    fn hash_hsl_is_stable_and_in_range() {
        let a = hash_hsl("researcher");
        assert_eq!(a, hash_hsl("researcher"));
        assert!(a.0 < 360 && a.1 <= 75 && a.2 <= 60);
        assert_ne!(a, hash_hsl("curator"));
    }

    #[test]
    fn context_percent_handles_unknown_and_overflow() {
        assert_eq!(context_free_percent(500, 0), None);
        assert_eq!(context_free_percent(250, 1000), Some(75.0));
        assert_eq!(context_free_percent(5000, 1000), Some(0.0));
    }
}
