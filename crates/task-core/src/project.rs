// r[impl project.schema]
use chrono::NaiveDate;
use facet::Facet;

use crate::task::{Status, Task, WikiLink};

/// A project note in the vault.
///
/// Projects live as `project.md` files (or `<Title>.md` with a `type: project`
/// frontmatter discriminator) inside a vault.  The schema is intentionally
/// Obsidian-compatible: every field serialises as plain YAML frontmatter.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Project {
    pub title: String,
    pub status: ProjectStatus,
    pub start: Option<NaiveDate>,
    pub due: Option<NaiveDate>,
    #[facet(default)]
    pub up: Vec<WikiLink>,
    #[facet(default)]
    pub tags: Vec<String>,
    pub description: Option<String>,
    pub workflow: Option<String>,
    pub workflow_stage: Option<String>,

    // ── Extended fields (project-vault) ──────────────────────────────
    /// Organization/workspace this project belongs to (e.g. "fasttrackaudio",
    /// "fasttrackstudio", "just-friends", "personal").
    pub organization: Option<String>,

    /// Logical area this project belongs to (e.g. "Music", "Engineering").
    pub area: Option<String>,

    /// Freeform project type (e.g. "audio-production", "video", "web",
    /// "design").  Enables type-specific views and integrations.
    pub project_type: Option<String>,

    /// Team members working on this project.
    #[facet(default)]
    pub team: Vec<String>,

    /// GitHub (or other forge) repository slug, e.g. "FastTrackStudios/task".
    pub repo: Option<String>,

    /// Local development path for code projects, e.g. "~/Development/Task".
    pub dev_path: Option<String>,

    /// References to other projects (not children — linked resources).
    #[facet(default)]
    pub references: Vec<WikiLink>,

    // ── Plane-inspired additions ────────────────────────────────────
    /// Short identifier for human-readable task IDs (e.g. "PRJ" → PRJ-1, PRJ-2).
    pub identifier: Option<String>,

    /// Next sequence number for tasks in this project.
    pub next_sequence: Option<u32>,

    /// Project lead (distinct from team members).
    pub lead: Option<String>,

    /// Default assignee for new tasks.
    pub default_assignee: Option<String>,

    /// Emoji icon (unicode, e.g. "🎵").
    pub emoji: Option<String>,

    /// Cover image URL or path.
    pub cover_image: Option<String>,

    /// Auto-archive tasks completed longer than N months ago (0 = disabled).
    pub archive_in_months: Option<u32>,

    /// Soft deletion timestamp.
    pub deleted_at: Option<chrono::DateTime<chrono::Utc>>,

    /// Whether this project is archived.
    pub archived_at: Option<chrono::DateTime<chrono::Utc>>,

    /// User favorites — who has bookmarked this project.
    #[facet(default)]
    pub favorited_by: Vec<String>,

    // ── Billing ──────────────────────────────────────────────────────
    /// Client this project is billed to (wikilink to a client note).
    pub client: Option<WikiLink>,

    /// Default billable rate in cents/hour for time entries on this
    /// project's tasks. Overrides the client's default rate; overridden
    /// by TimeEntry.billable_rate.
    pub default_rate: Option<u32>,

    /// Invoice Ninja id of the corresponding IN project (opaque hashed
    /// string). Set when the project was pushed to IN.
    pub invoice_ninja_id: Option<String>,

    // ── Email routing ────────────────────────────────────────────────
    /// Matcher hints the bot uses to decide incoming mail belongs to
    /// this project. Typical values: mail-client tag labels ("project:
    /// montreal-album"), subject-line tokens ("[MA]"), or sender
    /// patterns. The bot (Jarvis) reads these and does its own matching
    /// — the schema just stores the hints.
    #[facet(default)]
    pub email_tags: Vec<String>,

    /// Emails linked to this project. Populated by the bot or manual
    /// `task email link` calls.
    #[facet(default)]
    pub emails: Vec<crate::email::EmailRef>,

    /// Markdown body for project notes and threaded comments.
    #[facet(skip)]
    #[facet(default)]
    pub body: Option<String>,
}

// r[impl project.status]
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum ProjectStatus {
    /// The project is being planned but work hasn't started.
    Planning,
    /// Active work in progress (default).
    Active,
    /// Temporarily paused.
    OnHold,
    /// All deliverables done.
    Completed,
    /// Shelved / no longer relevant.
    Archived,
}

impl Default for ProjectStatus {
    fn default() -> Self {
        ProjectStatus::Active
    }
}

impl Project {
    pub fn is_archived(&self) -> bool {
        self.status == ProjectStatus::Archived
            || self.tags.iter().any(|t| t == "archive" || t == "archived")
    }

    pub fn is_active(&self) -> bool {
        self.status == ProjectStatus::Active
    }

    // r[impl project.computed.is-overdue]
    pub fn is_overdue(&self) -> bool {
        !self.is_archived()
            && self
                .due
                .map(|d| d < chrono::Local::now().date_naive())
                .unwrap_or(false)
    }
}

/// Computed stats for a project derived from its associated tasks.
// r[impl project.computed.task-counts]
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ProjectStats {
    pub open_task_count: u32,
    pub completed_task_count: u32,
}

impl ProjectStats {
    pub fn from_tasks(tasks: &[&Task]) -> Self {
        let completed = tasks.iter().filter(|t| t.is_complete()).count() as u32;
        let open = tasks.len() as u32 - completed;
        Self {
            open_task_count: open,
            completed_task_count: completed,
        }
    }

    pub fn total(&self) -> u32 {
        self.open_task_count + self.completed_task_count
    }

    // r[impl project.computed.completion-percent]
    pub fn completion_percent(&self) -> Option<f32> {
        if self.total() == 0 {
            return None;
        }
        Some(self.completed_task_count as f32 / self.total() as f32 * 100.0)
    }
}

/// Dashboard bucket for ranking projects by urgency.
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum ProjectDashboardBucket {
    Overdue,
    DueSoon,
    Active,
    NoOpenTasks,
}

impl ProjectDashboardBucket {
    fn rank(&self) -> u8 {
        match self {
            ProjectDashboardBucket::Overdue => 0,
            ProjectDashboardBucket::DueSoon => 1,
            ProjectDashboardBucket::Active => 2,
            ProjectDashboardBucket::NoOpenTasks => 3,
        }
    }
}

/// Computed dashboard entry for one project.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ProjectDashboardEntry {
    pub project: Project,
    pub stats: ProjectStats,
    pub next_task: Option<Task>,
    pub completion_percent: Option<f32>,
    pub overdue_task_count: u32,
    pub most_overdue_days: Option<i32>,
    pub next_task_urgency: Option<i32>,
    pub bucket: ProjectDashboardBucket,
}

/// Build a dashboard view for the active, non-archived projects in a vault.
pub fn project_dashboard(projects: &[Project], tasks: &[Task]) -> Vec<ProjectDashboardEntry> {
    let today = chrono::Local::now().date_naive();
    let mut entries: Vec<ProjectDashboardEntry> = projects
        .iter()
        .filter(|project| project.is_active() && !project.is_archived())
        .map(|project| {
            let project_tasks: Vec<&Task> = tasks
                .iter()
                .filter(|task| task.projects.iter().any(|p| p.0 == project.title))
                .collect();
            let stats = ProjectStats::from_tasks(&project_tasks);
            let next_task = next_task(&project.title, tasks).cloned();
            let overdue_task_count = project_tasks
                .iter()
                .filter(|task| task.is_overdue())
                .count() as u32;
            let most_overdue_days = project_tasks
                .iter()
                .filter_map(|task| task.days_until_due().map(|days| days.saturating_neg()))
                .max();
            let bucket = if stats.open_task_count == 0 {
                ProjectDashboardBucket::NoOpenTasks
            } else if overdue_task_count > 0 {
                ProjectDashboardBucket::Overdue
            } else if project
                .due
                .map(|due| (due - today).num_days())
                .map(|days| days <= 7)
                .unwrap_or(false)
            {
                ProjectDashboardBucket::DueSoon
            } else {
                ProjectDashboardBucket::Active
            };

            ProjectDashboardEntry {
                project: project.clone(),
                stats: stats.clone(),
                next_task_urgency: next_task.as_ref().map(|task| task.urgency_score()),
                completion_percent: stats.completion_percent(),
                overdue_task_count,
                most_overdue_days,
                next_task,
                bucket,
            }
        })
        .collect();

    entries.sort_by(|a, b| {
        a.bucket
            .rank()
            .cmp(&b.bucket.rank())
            .then_with(|| match a.bucket {
                ProjectDashboardBucket::Overdue => b
                    .most_overdue_days
                    .unwrap_or(i32::MIN)
                    .cmp(&a.most_overdue_days.unwrap_or(i32::MIN)),
                ProjectDashboardBucket::DueSoon => a.project.due.cmp(&b.project.due),
                ProjectDashboardBucket::Active => b
                    .next_task_urgency
                    .unwrap_or(i32::MIN)
                    .cmp(&a.next_task_urgency.unwrap_or(i32::MIN)),
                ProjectDashboardBucket::NoOpenTasks => a.project.title.cmp(&b.project.title),
            })
            .then_with(|| a.project.title.cmp(&b.project.title))
    });

    entries
}

/// Selects the single most actionable next task for a project.
// r[impl project.computed.next-task]
pub fn next_task<'a>(project_title: &str, tasks: &'a [Task]) -> Option<&'a Task> {
    let mut candidates: Vec<&Task> = tasks
        .iter()
        .filter(|t| {
            t.projects.iter().any(|p| p.0 == project_title)
                && !t.is_complete()
                && t.status != Status::Cancelled
                && t.status != Status::Archived
                && !t.is_blocked()
                && t.has_started()
        })
        .collect();

    candidates.sort_by(|a, b| {
        b.urgency_score()
            .cmp(&a.urgency_score())
            .then_with(|| a.due.cmp(&b.due))
            .then_with(|| a.scheduled.cmp(&b.scheduled))
            .then_with(|| a.date_created.cmp(&b.date_created))
    });

    candidates.into_iter().next()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn task_for(project: &str, title: &str, due: Option<i64>, status: Status) -> Task {
        let today = chrono::Local::now().date_naive();
        Task {
            title: title.to_string(),
            status,
            priority: crate::task::Priority::Normal,
            projects: vec![WikiLink(project.to_string())],
            due: due.map(|days| today + chrono::Duration::days(days)),
            ..Default::default()
        }
    }

    #[test]
    fn dashboard_sorts_by_urgency_buckets() {
        let today = chrono::Local::now().date_naive();
        let projects = vec![
            Project {
                title: "Active".into(),
                status: ProjectStatus::Active,
                ..Default::default()
            },
            Project {
                title: "Overdue".into(),
                status: ProjectStatus::Active,
                ..Default::default()
            },
            Project {
                title: "Due soon".into(),
                status: ProjectStatus::Active,
                due: Some(today + chrono::Duration::days(3)),
                ..Default::default()
            },
            Project {
                title: "No open".into(),
                status: ProjectStatus::Active,
                ..Default::default()
            },
            Project {
                title: "Archived".into(),
                status: ProjectStatus::Archived,
                ..Default::default()
            },
        ];

        let mut complete = task_for("No open", "Done", Some(1), Status::Done);
        complete.completed_date = Some(today);
        let tasks = vec![
            task_for("Overdue", "Fix build", Some(-5), Status::InProgress),
            task_for("Due soon", "Prep release", Some(2), Status::InProgress),
            task_for("Active", "General work", Some(12), Status::InProgress),
            complete,
        ];

        let dashboard = project_dashboard(&projects, &tasks);
        let titles: Vec<_> = dashboard
            .iter()
            .map(|entry| entry.project.title.as_str())
            .collect();
        assert_eq!(titles, vec!["Overdue", "Due soon", "Active", "No open"]);

        let overdue = &dashboard[0];
        assert_eq!(overdue.bucket, ProjectDashboardBucket::Overdue);
        assert_eq!(overdue.overdue_task_count, 1);
        assert_eq!(
            overdue.next_task.as_ref().map(|task| task.title.as_str()),
            Some("Fix build")
        );

        let due_soon = &dashboard[1];
        assert_eq!(due_soon.bucket, ProjectDashboardBucket::DueSoon);
        assert_eq!(
            due_soon.project.due,
            Some(today + chrono::Duration::days(3))
        );

        let no_open = &dashboard[3];
        assert_eq!(no_open.bucket, ProjectDashboardBucket::NoOpenTasks);
        assert_eq!(no_open.stats.open_task_count, 0);
        assert_eq!(no_open.completion_percent, Some(100.0));
    }
}
