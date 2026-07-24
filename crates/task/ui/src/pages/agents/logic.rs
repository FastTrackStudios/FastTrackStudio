//! Pure, tested derivation logic for the agent command center —
//! the t3code `*.logic.ts` pattern: components stay thin `rsx!`,
//! everything decidable lives here as plain functions.

use agent_proto::service::discovery::ModelInfo;
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

/// Group models by provider for the picker's brand rail
/// (hermes-desktop's `groupModelsByProvider`): stable provider
/// order = first appearance, models sorted with the ACTIVE model
/// floated first (3-tier rank: active → default → rest, stable).
pub fn group_models(models: &[ModelInfo], current: &str) -> Vec<(String, String, Vec<ModelInfo>)> {
    let mut order: Vec<(String, String)> = Vec::new();
    let mut by_provider: std::collections::HashMap<String, Vec<ModelInfo>> =
        std::collections::HashMap::new();
    for m in models {
        if !order.iter().any(|(id, _)| *id == m.provider_id) {
            let name = if m.provider_name.is_empty() {
                m.provider_id.clone()
            } else {
                m.provider_name.clone()
            };
            order.push((m.provider_id.clone(), name));
        }
        by_provider
            .entry(m.provider_id.clone())
            .or_default()
            .push(m.clone());
    }
    order
        .into_iter()
        .map(|(id, name)| {
            let mut ms = by_provider.remove(&id).unwrap_or_default();
            ms.sort_by_key(|m| {
                if m.id == current {
                    0u8
                } else if m.is_default {
                    1
                } else {
                    2
                }
            });
            (id, name, ms)
        })
        .collect()
}

/// Compact cost badge: "$0.25/$2" per Mtok, or None when unknown.
pub fn cost_badge(cost_in: f64, cost_out: f64) -> Option<String> {
    if cost_in <= 0.0 && cost_out <= 0.0 {
        return None;
    }
    fn c(v: f64) -> String {
        if v >= 10.0 {
            format!("${v:.0}")
        } else {
            format!("${v:.2}")
                .trim_end_matches('0')
                .trim_end_matches('.')
                .to_string()
        }
    }
    Some(format!("{}/{}", c(cost_in), c(cost_out)))
}

pub fn fmt_elapsed(secs: i64) -> String {
    format!("{}:{:02}", secs / 60, secs % 60)
}

/// Tool-call wall time: sub-second in ms, then seconds, then m/s.
pub fn fmt_duration(ms: u32) -> String {
    match ms {
        0..=999 => format!("{ms}ms"),
        1000..=59_999 => format!("{:.1}s", f64::from(ms) / 1000.0),
        _ => {
            let secs = ms / 1000;
            format!("{}m{:02}s", secs / 60, secs % 60)
        }
    }
}

/// Accrued spend for a session. Sub-cent turns still deserve a
/// number — "$0.00" reads as free, which it isn't.
pub fn fmt_cost(usd: f32) -> Option<String> {
    if usd <= 0.0 {
        return None;
    }
    if usd < 0.01 {
        return Some(format!("${usd:.4}"));
    }
    Some(format!("${usd:.2}"))
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

    fn mi(provider: &str, id: &str, default: bool) -> ModelInfo {
        ModelInfo {
            backend_id: "hermes".into(),
            id: id.into(),
            label: String::new(),
            is_default: default,
            context_length: 0,
            provider_id: provider.into(),
            provider_name: provider.to_uppercase(),
            reasoning: false,
            cost_in_per_mtok: 0.0,
            cost_out_per_mtok: 0.0,
        }
    }

    #[test]
    fn group_models_keeps_order_and_floats_active() {
        let models = vec![
            mi("hermes", "hermes", true),
            mi("openai", "openai/gpt-a", false),
            mi("openai", "openai/gpt-b", false),
            mi("anthropic", "anthropic/claude", false),
        ];
        let groups = group_models(&models, "openai/gpt-b");
        assert_eq!(groups.len(), 3);
        assert_eq!(groups[0].0, "hermes");
        assert_eq!(groups[1].0, "openai");
        // Active model floats first within its provider.
        assert_eq!(groups[1].2[0].id, "openai/gpt-b");
        assert_eq!(groups[2].1, "ANTHROPIC");
    }

    #[test]
    fn cost_badge_formats_and_hides_unknown() {
        assert_eq!(cost_badge(0.0, 0.0), None);
        assert_eq!(cost_badge(0.25, 2.0), Some("$0.25/$2".into()));
        assert_eq!(cost_badge(15.0, 75.0), Some("$15/$75".into()));
    }

    #[test]
    fn fmt_duration_switches_units() {
        assert_eq!(fmt_duration(0), "0ms");
        assert_eq!(fmt_duration(842), "842ms");
        assert_eq!(fmt_duration(1500), "1.5s");
        assert_eq!(fmt_duration(75_000), "1m15s");
    }

    #[test]
    fn fmt_cost_keeps_sub_cent_spend_visible() {
        assert_eq!(fmt_cost(0.0), None);
        assert_eq!(fmt_cost(0.0004), Some("$0.0004".into()));
        assert_eq!(fmt_cost(1.239), Some("$1.24".into()));
    }

    #[test]
    fn context_percent_handles_unknown_and_overflow() {
        assert_eq!(context_free_percent(500, 0), None);
        assert_eq!(context_free_percent(250, 1000), Some(75.0));
        assert_eq!(context_free_percent(5000, 1000), Some(0.0));
    }
}
