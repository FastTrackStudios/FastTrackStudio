//! Contextual relevance — which Active tasks deserve attention *right
//! now*. See `plans/relevancy-and-inbox.md`.
//!
//! Rides on the GTD `contexts` field: a task carrying **gate
//! contexts** (`@morning`, `@home`, `@phone`, …) is visible only when
//! the caller's [`RelevanceContext`] satisfies at least one of them;
//! a task with no gate contexts is always relevant; a task due or
//! scheduled today (or overdue) always shows — deadlines trump gates.
//!
//! Pure functions on wire types, deliberately UI-free: the server
//! applies them inside `TaskService::query` (CLI path) and the web UI
//! calls the same functions client-side against its optimistic store.

use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

use crate::model::{TaskInfo, is_due_on_or_before, status_is_open};

/// The caller's situation, every field optional — an empty context
/// hides all gated tasks and keeps everything ungated.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub struct RelevanceContext {
    /// Local wall-clock time as `HH:MM` (the *caller's* clock — the
    /// server never guesses a timezone). Drives the time-window
    /// contexts.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub local_hhmm: Option<String>,
    /// Local date as `YYYY-MM-DD`, for the due/scheduled override.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub local_date: Option<String>,
    /// Where the user is (`home`, `studio`, `errands`, …) — matched
    /// against `@<location>` contexts, ASCII-case-insensitive.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub location: Option<String>,
    /// What they're on (`phone`, `computer`, …) — matched against
    /// `@<device>` contexts.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub device: Option<String>,
    /// Project of the currently-running timer session; its tasks
    /// rank first.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub active_project: Option<Uuid>,
}

/// The built-in time-window contexts: `(@name, [(start, end)])` in
/// minutes-since-midnight, end-exclusive.
///
/// v1 fixed windows — personal windows move to the per-user prefs
/// entity later (see the plan).
const TIME_WINDOWS: &[(&str, &[(u16, u16)])] = &[
    ("morning", &[(5 * 60, 10 * 60)]),
    ("mealprep", &[(11 * 60, 13 * 60), (17 * 60, 19 * 60)]),
    ("evening", &[(20 * 60, 24 * 60)]),
];

/// Parse `HH:MM` to minutes-since-midnight. Garbage → `None`.
fn parse_hhmm(s: &str) -> Option<u16> {
    let (h, m) = s.split_once(':')?;
    let h: u16 = h.parse().ok()?;
    let m: u16 = m.parse().ok()?;
    (h < 24 && m < 60).then_some(h * 60 + m)
}

/// Strip the GTD `@` sigil and lowercase — `@Morning` → `morning`.
fn context_name(raw: &str) -> String {
    raw.trim().trim_start_matches('@').to_ascii_lowercase()
}

/// Whether `name` is a time-window context currently in-window.
/// Unknown names are not time contexts (returns `None`).
fn time_window_matches(name: &str, now_min: u16) -> Option<bool> {
    TIME_WINDOWS
        .iter()
        .find(|(w, _)| *w == name)
        .map(|(_, spans)| spans.iter().any(|&(a, b)| now_min >= a && now_min < b))
}

/// Does the context satisfy one gate context name?
fn gate_matches(name: &str, ctx: &RelevanceContext) -> bool {
    if let Some(now) = ctx.local_hhmm.as_deref().and_then(parse_hhmm) {
        if let Some(hit) = time_window_matches(name, now) {
            return hit;
        }
    } else if time_window_matches(name, 0).is_some() {
        // Time-window context but the caller supplied no clock —
        // treat as out-of-window (routines only show when asked
        // "what's relevant now", never in a timeless query).
        return false;
    }
    let eq = |v: &Option<String>| v.as_deref().is_some_and(|v| v.eq_ignore_ascii_case(name));
    eq(&ctx.location) || eq(&ctx.device)
}

/// Whether the task is **relevant** under `ctx`. Assumes the caller
/// already scoped to Active tasks (see [`status_is_open`]) — done
/// tasks are neither relevant nor irrelevant, just filtered upstream.
#[must_use]
pub fn is_relevant(task: &TaskInfo, ctx: &RelevanceContext) -> bool {
    // Deadlines trump gates: due/scheduled today or overdue always shows.
    if let Some(today) = ctx.local_date.as_deref() {
        if is_due_on_or_before(task.due.as_deref(), task.scheduled.as_deref(), today) {
            return true;
        }
    }
    let gates: Vec<String> = task.contexts.iter().map(|c| context_name(c)).collect();
    if gates.is_empty() {
        return true;
    }
    gates.iter().any(|g| gate_matches(g, ctx))
}

/// Ordering weight — smaller sorts first. Active-timer project tasks
/// lead, then due/overdue, then everything else in the caller's
/// existing order (stable sorts keep it).
#[must_use]
pub fn relevance_rank(task: &TaskInfo, ctx: &RelevanceContext) -> u8 {
    if ctx.active_project.is_some() && task.project_id == ctx.active_project {
        return 0;
    }
    if let Some(today) = ctx.local_date.as_deref() {
        if is_due_on_or_before(task.due.as_deref(), task.scheduled.as_deref(), today) {
            return 1;
        }
    }
    2
}

/// The shared "Active + Relevant" pipeline: keep open tasks that are
/// relevant, stably ordered by [`relevance_rank`]. Both the server's
/// `query` filter and the web store's client-side view call this.
pub fn filter_relevant(tasks: &mut Vec<TaskInfo>, ctx: &RelevanceContext) {
    tasks.retain(|t| status_is_open(&t.status) && is_relevant(t, ctx));
    tasks.sort_by_key(|t| relevance_rank(t, ctx));
}

#[cfg(test)]
mod tests {
    use super::*;

    fn task(contexts: &[&str]) -> TaskInfo {
        let mut t = crate::capture("Test");
        t.contexts = contexts.iter().map(ToString::to_string).collect();
        t
    }

    fn at(hhmm: &str) -> RelevanceContext {
        RelevanceContext {
            local_hhmm: Some(hhmm.to_owned()),
            local_date: Some("2026-07-01".to_owned()),
            ..RelevanceContext::default()
        }
    }

    #[test]
    fn ungated_tasks_are_always_relevant() {
        assert!(is_relevant(&task(&[]), &at("14:00")));
        assert!(is_relevant(&task(&[]), &RelevanceContext::default()));
    }

    #[test]
    fn routine_contexts_gate_by_time_window() {
        let brush = task(&["@morning", "@evening"]);
        assert!(is_relevant(&brush, &at("07:30")));
        assert!(is_relevant(&brush, &at("21:00")));
        assert!(!is_relevant(&brush, &at("14:00")));
        // No clock in the context → routines hidden.
        assert!(!is_relevant(&brush, &RelevanceContext::default()));
    }

    #[test]
    fn mealprep_has_two_windows() {
        let lunch = task(&["@mealprep"]);
        assert!(is_relevant(&lunch, &at("12:00")));
        assert!(is_relevant(&lunch, &at("18:00")));
        assert!(!is_relevant(&lunch, &at("15:00")));
    }

    #[test]
    fn location_and_device_gates_match_case_insensitively() {
        let errand = task(&["@errands"]);
        let mut ctx = at("14:00");
        assert!(!is_relevant(&errand, &ctx));
        ctx.location = Some("Errands".to_owned());
        assert!(is_relevant(&errand, &ctx));

        let call = task(&["@phone"]);
        ctx.device = Some("phone".to_owned());
        assert!(is_relevant(&call, &ctx));
    }

    #[test]
    fn deadline_trumps_gates() {
        let mut overdue = task(&["@morning"]);
        overdue.due = Some("2026-06-30".to_owned());
        assert!(is_relevant(&overdue, &at("14:00")));
    }

    #[test]
    fn pipeline_filters_done_and_ranks_active_project_first() {
        let pid = Uuid::new_v4();
        let mut a = task(&[]);
        a.title = "other".into();
        let mut b = task(&[]);
        b.title = "on the clock".into();
        b.project_id = Some(pid);
        let mut done = task(&[]);
        done.status = "done".into();
        let mut hidden = task(&["@morning"]);
        hidden.title = "routine".into();

        let mut rows = vec![a, done, hidden, b];
        let ctx = RelevanceContext {
            active_project: Some(pid),
            ..at("14:00")
        };
        filter_relevant(&mut rows, &ctx);
        let titles: Vec<&str> = rows.iter().map(|t| t.title.as_str()).collect();
        assert_eq!(titles, vec!["on the clock", "other"]);
    }
}
