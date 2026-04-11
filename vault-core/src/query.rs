// r[impl query.model]
use std::cmp::Reverse;

use facet::Facet;

use chrono::Datelike as _;
use crate::task::{Priority, Status, Task};

#[derive(Debug, Clone, Facet)]
pub struct Query {
    #[facet(default)]
    pub filters: Vec<Filter>,
    pub sort: Sort,
    pub limit: Option<usize>,
    /// Optional grouping dimension applied after filter+sort.
    #[facet(default)]
    pub group: Option<Group>,
}

impl Default for Query {
    fn default() -> Self {
        Self {
            filters: vec![],
            sort: Sort::Urgency,
            limit: None,
            group: None,
        }
    }
}

// r[impl query.group]
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum Group {
    ByProject,
    ByContext,
    ByArea,
    ByStatus,
    ByPriority,
    ByDue,
    ByScheduled,
    ByTag,
}

/// A named bucket of tasks produced by `Query::execute_grouped`.
#[derive(Debug, Clone)]
pub struct TaskGroup {
    pub key: String,
    pub tasks: Vec<Task>,
}

/// The result of a grouped query execution.
#[derive(Debug, Clone)]
pub struct GroupedTasks {
    pub groups: Vec<TaskGroup>,
}

// r[impl query.filter]
#[derive(Debug, Clone, Facet)]
#[repr(C)]
pub enum Filter {
    NotComplete,
    NotBlocked,
    NotArchived,
    NotCancelled,
    DueToday,
    DueThisWeek,
    Overdue,
    ScheduledToday,
    HasStarted,
    HasProject(String),
    HasContext(String),
    HasTag(String),
    HasArea(String),
    Status(Status),
    Priority(Priority),
    ExternalSource(String),
    // r[impl query.filter.text]
    TitleContains(String),
    // r[impl query.filter.date-range]
    DueBetween { start: String, end: String },
    ScheduledBetween { start: String, end: String },
    CreatedBetween { start: String, end: String },
}

// r[impl query.sort]
#[derive(Debug, Clone, Default, Facet)]
#[repr(u8)]
pub enum Sort {
    #[default]
    Urgency,
    Priority,
    Due,
    Scheduled,
    DateCreated,
    DateModified,
    Title,
    Status,
    SortOrder,
    TotalTimeLogged,
    TimeEstimate,
}

impl Query {
    // r[impl query.execute]
    pub fn execute<'a>(&self, tasks: &'a [Task]) -> Vec<&'a Task> {
        let today = chrono::Local::now().date_naive();

        let mut results: Vec<&Task> = tasks
            .iter()
            .filter(|t| t.has_started())
            .filter(|t| self.filters.iter().all(|f| f.matches(t, today)))
            .collect();

        // r[impl query.sort.default]
        match self.sort {
            Sort::Urgency => results.sort_by_key(|t| Reverse(t.urgency_score())),
            Sort::Due => results.sort_by(|a, b| nulls_last(a.due, b.due)),
            Sort::Scheduled => results.sort_by(|a, b| nulls_last(a.scheduled, b.scheduled)),
            Sort::Title => results.sort_by(|a, b| a.title.cmp(&b.title)),
            Sort::DateCreated => results.sort_by(|a, b| nulls_last(a.date_created, b.date_created)),
            Sort::DateModified => results.sort_by(|a, b| nulls_last(a.date_modified, b.date_modified)),
            Sort::SortOrder => results.sort_by(|a, b| nulls_last(a.sort_order.as_deref(), b.sort_order.as_deref())),
            Sort::Priority => results.sort_by_key(|t| std::cmp::Reverse(t.priority.weight())),
            Sort::TotalTimeLogged => results.sort_by_key(|t| Reverse(t.total_time_logged())),
            Sort::TimeEstimate => results.sort_by(|a, b| nulls_last(a.time_estimate, b.time_estimate)),
            Sort::Status => results.sort_by_key(|t| match t.status {
                Status::InProgress => 0,
                Status::Open | Status::None => 1,
                Status::OnHold => 2,
                Status::Planned => 3,
                Status::Done => 4,
                Status::Cancelled => 5,
                Status::Archived => 6,
            }),
        }

        if let Some(limit) = self.limit {
            results.truncate(limit);
        }

        results
    }
}

/// r[impl query.sort.nulls] — nulls sort last regardless of direction.
fn nulls_last<T: Ord>(a: Option<T>, b: Option<T>) -> std::cmp::Ordering {
    match (a, b) {
        (None, None) => std::cmp::Ordering::Equal,
        (None, Some(_)) => std::cmp::Ordering::Greater,
        (Some(_), None) => std::cmp::Ordering::Less,
        (Some(x), Some(y)) => x.cmp(&y),
    }
}

impl Filter {
    fn matches(&self, task: &Task, today: chrono::NaiveDate) -> bool {
        match self {
            Filter::NotComplete => !task.is_complete(),
            Filter::NotBlocked => !task.is_blocked(),
            Filter::NotArchived => task.status != Status::Archived,
            Filter::NotCancelled => task.status != Status::Cancelled,
            Filter::DueToday => task.due.map(|d| d == today).unwrap_or(false),
            Filter::DueThisWeek => task
                .due
                .map(|d| d >= today && d <= today + chrono::Duration::days(7))
                .unwrap_or(false),
            Filter::Overdue => task.is_overdue(),
            Filter::ScheduledToday => task.scheduled.map(|d| d == today).unwrap_or(false),
            Filter::HasStarted => task.has_started(),
            Filter::HasProject(p) => task.projects.iter().any(|l| l.0 == *p),
            Filter::HasContext(c) => task.contexts.iter().any(|ctx| ctx == c),
            Filter::HasTag(tag) => task.tags.iter().any(|t| t == tag),
            Filter::HasArea(a) => task.areas.iter().any(|l| l.0 == *a),
            Filter::Status(s) => &task.status == s,
            Filter::Priority(p) => &task.priority == p,
            Filter::ExternalSource(src) => task.external_source.as_deref() == Some(src.as_str()),
            Filter::TitleContains(q) => task.title.to_lowercase().contains(&q.to_lowercase()),
            Filter::DueBetween { start, end } => task.due.map(|d| {
                let s = start.parse::<chrono::NaiveDate>().ok();
                let e = end.parse::<chrono::NaiveDate>().ok();
                s.map_or(true, |s| d >= s) && e.map_or(true, |e| d <= e)
            }).unwrap_or(false),
            Filter::ScheduledBetween { start, end } => task.scheduled.map(|d| {
                let s = start.parse::<chrono::NaiveDate>().ok();
                let e = end.parse::<chrono::NaiveDate>().ok();
                s.map_or(true, |s| d >= s) && e.map_or(true, |e| d <= e)
            }).unwrap_or(false),
            // r[impl query.filter.date-range]
            Filter::CreatedBetween { start, end } => task.date_created.map(|dt| {
                let d = dt.date_naive();
                let s = start.parse::<chrono::NaiveDate>().ok();
                let e = end.parse::<chrono::NaiveDate>().ok();
                s.map_or(true, |s| d >= s) && e.map_or(true, |e| d <= e)
            }).unwrap_or(false),
        }
    }
}

// ── Grouping helpers ──────────────────────────────────────────────────────────

impl Query {
    // r[impl query.group]
    /// Execute the query and partition results into named groups.
    pub fn execute_grouped(&self, tasks: &[Task]) -> GroupedTasks {
        let today = chrono::Local::now().date_naive();
        let sorted: Vec<Task> = self.execute(tasks).into_iter().cloned().collect();

        let Some(ref group_dim) = self.group else {
            return GroupedTasks {
                groups: vec![TaskGroup { key: String::new(), tasks: sorted }],
            };
        };

        // Pre-populate ordered keys for date buckets.
        let mut buckets: Vec<(String, Vec<Task>)> = Vec::new();
        if matches!(group_dim, Group::ByDue | Group::ByScheduled) {
            for key in ["Overdue", "Today", "Tomorrow", "This Week", "This Month", "Later", "No Date"] {
                buckets.push((key.to_string(), vec![]));
            }
        }

        for task in sorted {
            let keys = group_keys(group_dim, &task, today);
            for key in keys {
                if let Some(pos) = buckets.iter().position(|(k, _)| k == &key) {
                    buckets[pos].1.push(task.clone());
                } else {
                    buckets.push((key, vec![task.clone()]));
                }
            }
        }

        GroupedTasks {
            groups: buckets
                .into_iter()
                .filter(|(_, v)| !v.is_empty())
                .map(|(key, tasks)| TaskGroup { key, tasks })
                .collect(),
        }
    }
}

fn group_keys(group: &Group, task: &Task, today: chrono::NaiveDate) -> Vec<String> {
    match group {
        Group::ByProject => {
            if task.projects.is_empty() {
                vec!["—".to_string()]
            } else {
                task.projects.iter().map(|p| p.0.clone()).collect()
            }
        }
        Group::ByContext => {
            if task.contexts.is_empty() {
                vec!["—".to_string()]
            } else {
                task.contexts.clone()
            }
        }
        Group::ByArea => {
            if task.areas.is_empty() {
                vec!["—".to_string()]
            } else {
                task.areas.iter().map(|a| a.0.clone()).collect()
            }
        }
        Group::ByTag => {
            if task.tags.is_empty() {
                vec!["—".to_string()]
            } else {
                task.tags.clone()
            }
        }
        Group::ByStatus => vec![status_label(&task.status).to_string()],
        Group::ByPriority => vec![priority_label(&task.priority).to_string()],
        Group::ByDue => vec![date_bucket(task.due, today)],
        Group::ByScheduled => vec![date_bucket(task.scheduled, today)],
    }
}

fn date_bucket(date: Option<chrono::NaiveDate>, today: chrono::NaiveDate) -> String {
    match date {
        None => "No Date".to_string(),
        Some(d) if d < today => "Overdue".to_string(),
        Some(d) if d == today => "Today".to_string(),
        Some(d) if d == today + chrono::Duration::days(1) => "Tomorrow".to_string(),
        Some(d) if d <= today + chrono::Duration::days(7) => "This Week".to_string(),
        Some(d) if d.month() == today.month() && d.year() == today.year() => "This Month".to_string(),
        Some(_) => "Later".to_string(),
    }
}

fn status_label(s: &Status) -> &'static str {
    match s {
        Status::None => "—",
        Status::Open => "Open",
        Status::InProgress => "In Progress",
        Status::OnHold => "On Hold",
        Status::Planned => "Planned",
        Status::Done => "Done",
        Status::Cancelled => "Cancelled",
        Status::Archived => "Archived",
    }
}

fn priority_label(p: &Priority) -> &'static str {
    match p {
        Priority::None => "—",
        Priority::Low => "Low",
        Priority::Normal => "Normal",
        Priority::High => "High",
        Priority::Urgent => "Urgent",
    }
}
