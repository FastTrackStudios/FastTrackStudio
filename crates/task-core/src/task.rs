// r[impl task.schema]
use chrono::{DateTime, NaiveDate, Utc};
use facet::Facet;

/// A TaskNotes-compatible task stored as a vault .md file.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Task {
    // r[impl task.id]
    #[facet(default)]
    pub id: Option<String>,
    pub title: String,
    pub status: Status,
    pub priority: Priority,
    #[facet(default)]
    pub projects: Vec<WikiLink>,
    #[facet(default)]
    pub contexts: Vec<String>,
    #[facet(default)]
    pub tags: Vec<String>,
    #[facet(default)]
    pub areas: Vec<WikiLink>,
    pub due: Option<NaiveDate>,
    pub scheduled: Option<NaiveDate>,
    pub start: Option<NaiveDate>,
    pub due_time: Option<String>,
    pub date_created: Option<DateTime<Utc>>,
    pub date_modified: Option<DateTime<Utc>>,
    pub completed_date: Option<NaiveDate>,
    pub time_estimate: Option<u32>,
    #[facet(default)]
    pub time_entries: Vec<TimeEntry>,
    pub pomodoro_count: Option<u32>,
    pub recurrence: Option<String>,
    pub recurrence_anchor: RecurrenceAnchor,
    #[facet(default)]
    pub completed_instances: Vec<String>,
    #[facet(default)]
    pub skipped_instances: Vec<String>,
    #[facet(default)]
    pub blocked_by: Vec<TaskDependency>,
    #[facet(default)]
    pub blocking: Vec<WikiLink>,
    #[facet(default)]
    pub reminders: Vec<Reminder>,
    pub sort_order: Option<String>,
    pub external_id: Option<String>,
    pub external_source: Option<String>,

    // ── Collaboration fields ─────────────────────────────────────────

    /// Who this task is assigned to (username, e.g. "codywright").
    pub assignee: Option<String>,

    /// Multiple assignees (in addition to primary assignee).
    #[facet(default)]
    pub assignees: Vec<String>,

    /// Who created this task.
    pub created_by: Option<String>,

    // ── Sequence ID (human-readable, per-project) ───────────────────

    /// Human-readable sequence number within the project (e.g. 42 → "PRJ-42").
    pub sequence_id: Option<u32>,

    // ── Type ────────────────────────────────────────────────────────

    /// Issue type: task, bug, story, epic, feature, chore, etc.
    pub issue_type: Option<String>,

    // ── Relations ───────────────────────────────────────────────────

    /// Rich relations to other tasks.
    #[facet(default)]
    pub relations: Vec<TaskRelation>,

    // ── Subscribers / watchers ──────────────────────────────────────

    /// Users watching this task for updates.
    #[facet(default)]
    pub subscribers: Vec<String>,

    // ── Reactions ───────────────────────────────────────────────────

    /// Emoji reactions on this task (e.g. "👍:cody", "🎉:amy").
    #[facet(default)]
    pub reactions: Vec<Reaction>,

    // ── Soft deletion ───────────────────────────────────────────────

    /// Soft delete timestamp. If set, this task is "deleted" but recoverable.
    pub deleted_at: Option<DateTime<Utc>>,

    // ── Draft ───────────────────────────────────────────────────────

    /// Whether this is a draft (work-in-progress, not yet visible to team).
    #[facet(default)]
    pub is_draft: bool,

    // ── Content ──────────────────────────────────────────────────────

    /// Markdown body content (after frontmatter).
    #[facet(skip)]
    #[facet(default)]
    pub body: String,
}

// r[impl task.status]
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum Status {
    None,
    Open,
    InProgress,
    OnHold,
    Planned,
    Done,
    Cancelled,
    Archived,
}

impl Default for Status {
    fn default() -> Self {
        Status::Open
    }
}

impl Status {
    pub fn is_completion(&self) -> bool {
        matches!(self, Status::Done)
    }
}

// r[impl task.priority]
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum Priority {
    None,
    Low,
    Normal,
    High,
    Urgent,
}

impl Default for Priority {
    fn default() -> Self {
        Priority::None
    }
}

impl Priority {
    pub fn weight(&self) -> i32 {
        match self {
            Priority::None => 0,
            Priority::Low => 1,
            Priority::Normal => 2,
            Priority::High => 3,
            Priority::Urgent => 5,
        }
    }
}

/// A wikilink reference `[[Note Name]]`
#[derive(Debug, Clone, PartialEq, Facet)]
#[facet(transparent)]
pub struct WikiLink(pub String);

/// A structured task dependency per RFC 9253.
// r[impl task.blocked-by]
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct TaskDependency {
    pub uid: String,
    pub reltype: DependencyRelType,
    /// Optional ISO 8601 duration gap, e.g. "P1D".
    pub gap: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum DependencyRelType {
    FinishToStart,
    FinishToFinish,
    StartToStart,
    StartToFinish,
}

impl Default for DependencyRelType {
    fn default() -> Self {
        DependencyRelType::FinishToStart
    }
}

/// A logged work session.
// r[impl task.time.entries]
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct TimeEntry {
    pub start_time: DateTime<Utc>,
    pub end_time: Option<DateTime<Utc>>,
    pub description: Option<String>,
}

// r[impl task.recurrence.anchor]
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum RecurrenceAnchor {
    /// Next occurrence from scheduled date — fixed calendar.
    Scheduled,
    /// Next occurrence from completion date — flexible.
    Completion,
}

impl Default for RecurrenceAnchor {
    fn default() -> Self {
        RecurrenceAnchor::Scheduled
    }
}

// r[impl task.reminders]
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub enum Reminder {
    Relative {
        id: String,
        related_to: ReminderAnchor,
        /// ISO 8601 duration, e.g. "-PT30M".
        offset: String,
        description: Option<String>,
    },
    Absolute {
        id: String,
        absolute_time: DateTime<Utc>,
        description: Option<String>,
    },
}

#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum ReminderAnchor {
    Due,
    Scheduled,
}

// ── Rich relations (Plane-inspired) ─────────────────────────────────────────

/// A typed relation between two tasks.
/// Stored in frontmatter as:
/// ```yaml
/// relations:
///   - target: "[[Other Task]]"
///     relation_type: BlockedBy
/// ```
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct TaskRelation {
    /// The related task (wiki-link or ID).
    pub target: String,
    /// Type of relationship.
    pub relation_type: RelationType,
}

/// Relationship types between tasks (bidirectional pairs).
#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum RelationType {
    /// This task is blocked by the target.
    #[default]
    BlockedBy,
    /// This task blocks the target.
    Blocking,
    /// Related but no dependency.
    RelatesTo,
    /// This is a duplicate of the target.
    DuplicateOf,
    /// The target implements this task (parent → child spec).
    ImplementedBy,
    /// This task implements the target spec.
    Implements,
    /// Must start before the target starts.
    StartBefore,
    /// Must start after the target starts.
    StartAfter,
    /// Must finish before the target finishes.
    FinishBefore,
    /// Must finish after the target finishes.
    FinishAfter,
}

// ── Reactions ───────────────────────────────────────────────────────────────

/// An emoji reaction on a task or comment.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Reaction {
    /// Emoji (unicode, e.g. "👍", "🎉", "🔥").
    pub emoji: String,
    /// Who reacted.
    pub user: String,
}

impl Task {
    // r[impl task.urgency]
    pub fn urgency_score(&self) -> i32 {
        let priority_weight = self.priority.weight();
        let days_until = self.days_until_due().or_else(|| self.days_until_scheduled());

        match days_until {
            None => priority_weight,
            // r[impl task.urgency.overdue-boost]
            Some(d) if d < 0 => priority_weight + 10 + d.unsigned_abs() as i32,
            Some(d) => priority_weight + (10 - d).max(0),
        }
    }

    // r[impl task.computed.days-until]
    pub fn days_until_due(&self) -> Option<i32> {
        let today = chrono::Local::now().date_naive();
        Some((self.due? - today).num_days() as i32)
    }

    pub fn days_until_scheduled(&self) -> Option<i32> {
        let today = chrono::Local::now().date_naive();
        Some((self.scheduled? - today).num_days() as i32)
    }

    // r[impl task.computed.overdue]
    pub fn is_overdue(&self) -> bool {
        !self.status.is_completion()
            && self
                .due
                .map(|d| d < chrono::Local::now().date_naive())
                .unwrap_or(false)
    }

    pub fn is_complete(&self) -> bool {
        self.status.is_completion()
    }

    // r[impl task.computed.is-blocked]
    pub fn is_blocked(&self) -> bool {
        !self.blocked_by.is_empty()
            || self.relations.iter().any(|r| r.relation_type == RelationType::BlockedBy)
    }

    pub fn is_deleted(&self) -> bool {
        self.deleted_at.is_some()
    }

    /// Get the display ID (e.g. "PRJ-42") given a project identifier prefix.
    pub fn display_id(&self, prefix: &str) -> String {
        match self.sequence_id {
            Some(seq) => format!("{prefix}-{seq}"),
            None => self.title.clone(),
        }
    }

    /// All users involved with this task (assignee + assignees + subscribers + created_by).
    pub fn all_participants(&self) -> Vec<&str> {
        let mut users = Vec::new();
        if let Some(ref a) = self.assignee {
            users.push(a.as_str());
        }
        for a in &self.assignees {
            if !users.contains(&a.as_str()) {
                users.push(a.as_str());
            }
        }
        for s in &self.subscribers {
            if !users.contains(&s.as_str()) {
                users.push(s.as_str());
            }
        }
        if let Some(ref c) = self.created_by {
            if !users.contains(&c.as_str()) {
                users.push(c.as_str());
            }
        }
        users
    }

    pub fn has_started(&self) -> bool {
        self.start
            .map(|s| s <= chrono::Local::now().date_naive())
            .unwrap_or(true)
    }

    // r[impl task.time.total-logged]
    /// Total time logged across all completed time entries, in minutes.
    pub fn total_time_logged(&self) -> u32 {
        self.time_entries
            .iter()
            .filter_map(|e| {
                let end = e.end_time?;
                let mins = (end - e.start_time).num_minutes();
                if mins > 0 { Some(mins as u32) } else { None }
            })
            .sum()
    }

    // r[impl task.time.efficiency]
    /// Ratio of time logged to time estimate. None if no estimate or estimate is zero.
    pub fn efficiency_ratio(&self) -> Option<f32> {
        let estimate = self.time_estimate? as f32;
        if estimate == 0.0 {
            return None;
        }
        Some(self.total_time_logged() as f32 / estimate)
    }

    /// Parse subtasks from the body text.
    /// Looks for lines matching `- [ ] text` or `- [x] text`.
    pub fn subtasks(&self) -> Vec<Subtask> {
        self.body
            .lines()
            .filter_map(|line| {
                let trimmed = line.trim_start();
                if let Some(rest) = trimmed.strip_prefix("- [ ] ") {
                    Some(Subtask {
                        title: rest.to_string(),
                        done: false,
                    })
                } else if let Some(rest) = trimmed.strip_prefix("- [x] ") {
                    Some(Subtask {
                        title: rest.to_string(),
                        done: true,
                    })
                } else {
                    None
                }
            })
            .collect()
    }

    /// Count of completed subtasks.
    pub fn subtasks_done(&self) -> usize {
        self.subtasks().iter().filter(|s| s.done).count()
    }

    /// Total subtask count.
    pub fn subtask_count(&self) -> usize {
        self.subtasks().len()
    }

    /// Subtask progress as a fraction (0.0 to 1.0). None if no subtasks.
    pub fn subtask_progress(&self) -> Option<f32> {
        let total = self.subtask_count();
        if total == 0 {
            return None;
        }
        Some(self.subtasks_done() as f32 / total as f32)
    }
}

/// A parsed subtask from the markdown body.
#[derive(Debug, Clone, PartialEq)]
pub struct Subtask {
    pub title: String,
    pub done: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn subtask_parsing() {
        let task = Task {
            body: "## Checklist\n- [x] Done thing\n- [ ] Not done\n- [x] Also done\nSome other text\n- [ ] Another".into(),
            ..Default::default()
        };
        assert_eq!(task.subtask_count(), 4);
        assert_eq!(task.subtasks_done(), 2);
        assert_eq!(task.subtask_progress(), Some(0.5));
    }

    #[test]
    fn no_subtasks() {
        let task = Task {
            body: "Just some notes".into(),
            ..Default::default()
        };
        assert_eq!(task.subtask_count(), 0);
        assert_eq!(task.subtask_progress(), None);
    }
}
