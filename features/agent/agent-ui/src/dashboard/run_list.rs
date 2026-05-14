//! `AgentDashboard` — sortable, filterable run-list view.
//! Spec ref: `agent.dashboard.run-list`, `agent.dashboard.notifications`.

use agent_proto::{AgentRun, AgentRunCreate};
use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Bot, Plus};
use fts_ui::prelude::*;
use uuid::Uuid;

use super::{fmt_cost_cents, fmt_elapsed_seconds, live_first_rank, status_to_badge_variant};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SortField {
    /// Spec default — live-first by status, then started_at desc.
    StatusLiveFirst,
    Kind,
    StartedAt,
    CompletedAt,
    CostEstimate,
    ToolCallCount,
}

impl SortField {
    pub fn label(self) -> &'static str {
        match self {
            Self::StatusLiveFirst => "Status (live first)",
            Self::Kind => "Kind",
            Self::StartedAt => "Started",
            Self::CompletedAt => "Completed",
            Self::CostEstimate => "Cost",
            Self::ToolCallCount => "Tool calls",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub struct DashboardFilters {
    /// Empty = match any status. Multi-select.
    pub statuses: Vec<String>,
    /// Empty = match any kind. Multi-select.
    pub kinds: Vec<String>,
    pub has_error_only: bool,
    pub awaiting_input_only: bool,
}

impl DashboardFilters {
    pub fn matches(&self, run: &AgentRun) -> bool {
        if !self.statuses.is_empty() && !self.statuses.iter().any(|s| s == &run.status) {
            return false;
        }
        if !self.kinds.is_empty() && !self.kinds.iter().any(|k| k == &run.kind) {
            return false;
        }
        if self.has_error_only && run.error_message.is_none() {
            return false;
        }
        if self.awaiting_input_only && run.status != "awaiting-input" {
            return false;
        }
        true
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DashboardSort {
    pub field: SortField,
    pub descending: bool,
}

impl Default for DashboardSort {
    fn default() -> Self {
        Self {
            field: SortField::StatusLiveFirst,
            descending: false,
        }
    }
}

fn elapsed_seconds(run: &AgentRun) -> Option<i64> {
    let start = run.started_at?;
    let end = run.completed_at.unwrap_or_else(Utc::now);
    Some((end - start).num_seconds().max(0))
}

fn sort_runs(runs: &mut [AgentRun], sort: DashboardSort) {
    match sort.field {
        SortField::StatusLiveFirst => {
            runs.sort_by(|a, b| {
                live_first_rank(&a.status)
                    .cmp(&live_first_rank(&b.status))
                    .then_with(|| b.started_at.cmp(&a.started_at))
            });
        }
        SortField::Kind => runs.sort_by(|a, b| a.kind.cmp(&b.kind)),
        SortField::StartedAt => runs.sort_by(|a, b| a.started_at.cmp(&b.started_at)),
        SortField::CompletedAt => runs.sort_by(|a, b| a.completed_at.cmp(&b.completed_at)),
        SortField::CostEstimate => {
            runs.sort_by(|a, b| a.cost_cents_estimate.cmp(&b.cost_cents_estimate));
        }
        SortField::ToolCallCount => {
            runs.sort_by(|a, b| a.tool_call_count.cmp(&b.tool_call_count));
        }
    }
    if sort.descending {
        runs.reverse();
    }
}

/// Operator dashboard — sortable run table with filter facets and a
/// "New Run" action that opens the create dialog.
#[component]
pub fn AgentDashboard(
    runs: Vec<AgentRun>,
    on_open_run: EventHandler<Uuid>,
    on_create: EventHandler<AgentRunCreate>,
) -> Element {
    let sort = use_signal(DashboardSort::default);
    let filters = use_signal(DashboardFilters::default);
    let mut dialog_open = use_signal(|| false);

    let mut visible: Vec<AgentRun> = runs
        .iter()
        .filter(|r| filters.read().matches(r))
        .cloned()
        .collect();
    sort_runs(&mut visible, *sort.read());

    let total = runs.len();
    let shown = visible.len();

    rsx! {
        VStack { class: "gap-4 p-4 md:p-6",
            HStack { class: "items-center justify-between",
                HStack { class: "gap-2 items-center",
                    Bot { size: 22 }
                    Heading { level: HeadingLevel::H1, "Agent dashboard" }
                }
                Button {
                    on_click: move |_| dialog_open.set(true),
                    HStack { class: "gap-1 items-center",
                        Plus { size: 16 }
                        "New Run"
                    }
                }
            }
            Text { variant: TextVariant::Muted,
                "{shown} of {total} runs shown."
            }

            FilterBar { filters }

            if visible.is_empty() && total == 0 {
                EmptyState {
                    message: "No agent runs yet. Start one →",
                    icon: rsx! { Bot { size: 32 } },
                }
            } else if visible.is_empty() {
                EmptyState {
                    message: "No runs match the current filters.",
                    icon: rsx! { Bot { size: 32 } },
                }
            } else {
                RunTable {
                    runs: visible,
                    sort,
                    on_open_run,
                }
            }

            if dialog_open() {
                super::NewRunDialog {
                    on_submit: move |draft: super::NewRunDraft| {
                        on_create.call(draft.into_create());
                        dialog_open.set(false);
                    },
                    on_dismiss: move |_| dialog_open.set(false),
                }
            }
        }
    }
}

#[component]
fn FilterBar(filters: Signal<DashboardFilters>) -> Element {
    let mut has_error = filters.read().has_error_only;
    let mut awaiting = filters.read().awaiting_input_only;
    rsx! {
        HStack { class: "gap-3 items-center flex-wrap",
            // Status multi-toggle as segmented control over the
            // common buckets. Multi-select via segmented isn't a
            // first-class fts-ui primitive yet; we expose the
            // common single-status filter via SegmentedControl with
            // "all" as the escape hatch.
            SegmentedControl {
                value: status_summary(&filters.read()),
                on_change: move |v: String| {
                    let mut f = filters.write();
                    f.statuses = match v.as_str() {
                        "all" => Vec::new(),
                        other => vec![other.to_string()],
                    };
                },
                options: vec![
                    ("all".to_string(), "All".to_string()),
                    ("running".to_string(), "Running".to_string()),
                    ("awaiting-input".to_string(), "Awaiting".to_string()),
                    ("queued".to_string(), "Queued".to_string()),
                    ("completed".to_string(), "Completed".to_string()),
                    ("failed".to_string(), "Failed".to_string()),
                ],
            }
            Button {
                variant: if has_error { ButtonVariant::Primary } else { ButtonVariant::Outline },
                on_click: move |_| {
                    has_error = !has_error;
                    filters.write().has_error_only = has_error;
                },
                if has_error { "Errors only ✓" } else { "Errors only" }
            }
            Button {
                variant: if awaiting { ButtonVariant::Primary } else { ButtonVariant::Outline },
                on_click: move |_| {
                    awaiting = !awaiting;
                    filters.write().awaiting_input_only = awaiting;
                },
                if awaiting { "Awaiting input ✓" } else { "Awaiting input" }
            }
        }
    }
}

fn status_summary(f: &DashboardFilters) -> String {
    match f.statuses.len() {
        0 => "all".into(),
        1 => f.statuses[0].clone(),
        _ => "all".into(),
    }
}

#[component]
fn RunTable(
    runs: Vec<AgentRun>,
    sort: Signal<DashboardSort>,
    on_open_run: EventHandler<Uuid>,
) -> Element {
    rsx! {
        div { class: "overflow-x-auto rounded-md border border-border bg-card",
            table { class: "w-full text-sm",
                thead { class: "bg-muted/40 text-left",
                    tr {
                        SortableTh { field: SortField::StatusLiveFirst, sort, label: "Status" }
                        th { class: "px-3 py-2", "Name" }
                        SortableTh { field: SortField::Kind, sort, label: "Kind" }
                        SortableTh { field: SortField::StartedAt, sort, label: "Started" }
                        SortableTh { field: SortField::CompletedAt, sort, label: "Completed" }
                        SortableTh { field: SortField::ToolCallCount, sort, label: "Tools" }
                        SortableTh { field: SortField::CostEstimate, sort, label: "Cost" }
                    }
                }
                tbody {
                    for run in runs.iter().cloned() {
                        RunRow {
                            key: "{run.id}",
                            run: run.clone(),
                            on_open: move |id| on_open_run.call(id),
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn SortableTh(field: SortField, sort: Signal<DashboardSort>, label: String) -> Element {
    let active = sort.read().field == field;
    let arrow = if active {
        if sort.read().descending { "▼" } else { "▲" }
    } else {
        ""
    };
    let label_node = format!("{label} {arrow}");
    rsx! {
        th {
            class: "px-3 py-2 cursor-pointer select-none",
            onclick: move |_| {
                let mut s = sort.write();
                if s.field == field {
                    s.descending = !s.descending;
                } else {
                    s.field = field;
                    s.descending = false;
                }
            },
            "{label_node}"
        }
    }
}

#[component]
fn RunRow(run: AgentRun, on_open: EventHandler<Uuid>) -> Element {
    let id = run.id;
    let variant = status_to_badge_variant(&run.status);
    let elapsed = fmt_elapsed_seconds(elapsed_seconds(&run));
    let started = run
        .started_at
        .map(|t| t.format("%b %d %H:%M").to_string())
        .unwrap_or_else(|| "—".into());
    let completed = run
        .completed_at
        .map(|t| t.format("%b %d %H:%M").to_string())
        .unwrap_or_else(|| "—".into());
    let cost = fmt_cost_cents(run.cost_cents_estimate);
    let status_label = run.status.clone();
    let name = run.name.clone();
    let kind = run.kind.clone();
    let tools = run.tool_call_count;
    rsx! {
        tr {
            class: "border-t border-border hover:bg-muted/30 cursor-pointer",
            onclick: move |_| on_open.call(id),
            td { class: "px-3 py-2",
                StatusBadge { variant, label: status_label }
            }
            td { class: "px-3 py-2 font-medium", "{name}" }
            td { class: "px-3 py-2 text-muted-foreground", "{kind}" }
            td { class: "px-3 py-2 text-muted-foreground", "{started}" }
            td { class: "px-3 py-2 text-muted-foreground",
                {if run.completed_at.is_some() { rsx! { "{completed}" } } else { rsx! { "{elapsed}" } }}
            }
            td { class: "px-3 py-2 text-muted-foreground", "{tools}" }
            td { class: "px-3 py-2 font-mono text-xs", "{cost}" }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::{Duration, Utc};

    fn run(status: &str, kind: &str, started_offset_min: i64) -> AgentRun {
        let now = Utc::now();
        AgentRun {
            id: Uuid::new_v4(),
            name: format!("{kind} run"),
            kind: kind.into(),
            prompt: "...".into(),
            status: status.into(),
            task_id: None,
            started_at: Some(now - Duration::minutes(started_offset_min)),
            completed_at: None,
            result: None,
            error_message: None,
            tokens_used: None,
            cost_cents: None,
            tags: Vec::new(),
            integration: None,
            external_id: None,
            external_url: None,
            log_cursor: None,
            parent_run_id: None,
            worktree_path: None,
            git_repo_connection_id: None,
            spawned_from_message_id: None,
            input_tokens: None,
            output_tokens: None,
            cache_read_tokens: None,
            cache_creation_tokens: None,
            cost_cents_estimate: None,
            tool_call_count: 0,
            assistant_message_count: 0,
            max_tokens: None,
            max_tool_calls: None,
            max_wall_seconds: None,
            created_at: now,
            updated_at: now,
        }
    }

    #[test]
    fn live_first_default_sort() {
        let mut runs = vec![
            run("completed", "claude-code", 30),
            run("running", "hermes", 5),
            run("queued", "codex", 1),
            run("awaiting-input", "hermes", 10),
            run("failed", "claude-code", 60),
        ];
        sort_runs(&mut runs, DashboardSort::default());
        let order: Vec<&str> = runs.iter().map(|r| r.status.as_str()).collect();
        assert_eq!(
            order,
            vec!["running", "awaiting-input", "queued", "completed", "failed"]
        );
    }

    #[test]
    fn filter_status_multi() {
        let runs = vec![
            run("running", "hermes", 1),
            run("completed", "hermes", 5),
            run("failed", "codex", 10),
        ];
        let f = DashboardFilters {
            statuses: vec!["running".into(), "failed".into()],
            ..Default::default()
        };
        let kept: Vec<&str> = runs
            .iter()
            .filter(|r| f.matches(r))
            .map(|r| r.status.as_str())
            .collect();
        assert_eq!(kept, vec!["running", "failed"]);
    }

    #[test]
    fn filter_awaiting_input() {
        let runs = vec![
            run("running", "hermes", 1),
            run("awaiting-input", "hermes", 2),
            run("completed", "hermes", 3),
        ];
        let f = DashboardFilters {
            awaiting_input_only: true,
            ..Default::default()
        };
        let kept = runs.iter().filter(|r| f.matches(r)).count();
        assert_eq!(kept, 1);
    }

    #[test]
    fn filter_has_error() {
        let mut a = run("failed", "hermes", 1);
        a.error_message = Some("boom".into());
        let b = run("completed", "hermes", 2);
        let runs = vec![a, b];
        let f = DashboardFilters {
            has_error_only: true,
            ..Default::default()
        };
        let kept = runs.iter().filter(|r| f.matches(r)).count();
        assert_eq!(kept, 1);
    }
}
