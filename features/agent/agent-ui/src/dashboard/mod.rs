//! Per-run **dashboard** for the agent feature — CodexMonitor-style.
//!
//! Distinct from the existing `AgentRunDashboard` in `lib.rs` (which is
//! summary cards + a list). This module ships the **operator** view:
//! a sortable, filterable run table at `/agent/dashboard`, and a
//! detail pane at `/agent/dashboard/:run_id` showing logs, tool calls,
//! and (when linked) the spawning conversation. Plus a new-run modal.
//!
//! Components are dumb: data in, events out. The `task-ui` route
//! crates fetch from the repo / subscribe to the bus and pump props.

pub mod inline_diff;
pub mod new_run_dialog;
pub mod run_detail;
pub mod run_list;

pub use inline_diff::InlineDiff;
pub use new_run_dialog::{NewRunDialog, NewRunDraft};
pub use run_detail::{RunDetailPane, RunDetailView};
pub use run_list::{AgentDashboard, DashboardFilters, DashboardSort, SortField};

use agent_proto::RunStatus;
use fts_ui::prelude::*;

/// Map `RunStatus` (or the wire string) to the four StatusBadge
/// variants per AGENTS.md hard rule. Unknown strings map to Neutral.
pub fn status_to_badge_variant(status: &str) -> StatusBadgeVariant {
    match RunStatus::parse(status) {
        Some(RunStatus::Completed) => StatusBadgeVariant::Success,
        Some(RunStatus::Failed | RunStatus::TimedOut) => StatusBadgeVariant::Danger,
        Some(RunStatus::Running | RunStatus::Starting | RunStatus::AwaitingInput) => {
            StatusBadgeVariant::Warning
        }
        Some(RunStatus::Cancelled | RunStatus::Queued | RunStatus::Paused) => {
            StatusBadgeVariant::Neutral
        }
        None => StatusBadgeVariant::Neutral,
    }
}

/// Live-first sort key for runs: lower = earlier in the default sort.
/// Matches the spec: running > awaiting-input > starting > queued >
/// terminal.
pub fn live_first_rank(status: &str) -> u8 {
    match RunStatus::parse(status) {
        Some(RunStatus::Running) => 0,
        Some(RunStatus::AwaitingInput) => 1,
        Some(RunStatus::Starting) => 2,
        Some(RunStatus::Paused) => 3,
        Some(RunStatus::Queued) => 4,
        Some(RunStatus::Completed) => 5,
        Some(RunStatus::Failed) => 6,
        Some(RunStatus::TimedOut) => 7,
        Some(RunStatus::Cancelled) => 8,
        None => 9,
    }
}

/// Format an `Option<i64>` cents value as a USD currency string.
pub fn fmt_cost_cents(cents: Option<i64>) -> String {
    match cents {
        Some(c) => format!("${:.2}", (c as f64) / 100.0),
        None => "—".into(),
    }
}

/// Human elapsed duration in `Xm Ys` form. Returns "—" for None.
pub fn fmt_elapsed_seconds(seconds: Option<i64>) -> String {
    match seconds {
        Some(s) if s < 60 => format!("{s}s"),
        Some(s) if s < 3600 => format!("{}m {}s", s / 60, s % 60),
        Some(s) => format!("{}h {}m", s / 3600, (s % 3600) / 60),
        None => "—".into(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fts_ui::prelude::StatusBadgeVariant;

    #[test]
    fn status_mapping() {
        // StatusBadgeVariant doesn't impl Debug, so we use `matches!`
        // for the assertions rather than `assert_eq!`.
        assert!(matches!(
            status_to_badge_variant("completed"),
            StatusBadgeVariant::Success
        ));
        assert!(matches!(
            status_to_badge_variant("failed"),
            StatusBadgeVariant::Danger
        ));
        assert!(matches!(
            status_to_badge_variant("running"),
            StatusBadgeVariant::Warning
        ));
        assert!(matches!(
            status_to_badge_variant("queued"),
            StatusBadgeVariant::Neutral
        ));
        assert!(matches!(
            status_to_badge_variant("garbage"),
            StatusBadgeVariant::Neutral
        ));
    }

    #[test]
    fn live_first_ordering() {
        let mut statuses = vec!["completed", "running", "queued", "awaiting-input", "failed"];
        statuses.sort_by_key(|s| live_first_rank(s));
        assert_eq!(
            statuses,
            vec!["running", "awaiting-input", "queued", "completed", "failed"]
        );
    }

    #[test]
    fn cost_formatting() {
        assert_eq!(fmt_cost_cents(None), "—");
        assert_eq!(fmt_cost_cents(Some(0)), "$0.00");
        assert_eq!(fmt_cost_cents(Some(150)), "$1.50");
        assert_eq!(fmt_cost_cents(Some(12345)), "$123.45");
    }

    #[test]
    fn elapsed_formatting() {
        assert_eq!(fmt_elapsed_seconds(None), "—");
        assert_eq!(fmt_elapsed_seconds(Some(45)), "45s");
        assert_eq!(fmt_elapsed_seconds(Some(125)), "2m 5s");
        assert_eq!(fmt_elapsed_seconds(Some(7325)), "2h 2m");
    }
}
