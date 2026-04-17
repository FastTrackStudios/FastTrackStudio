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
