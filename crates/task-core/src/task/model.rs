// r[impl task.schema]
use chrono::{DateTime, NaiveDate, Utc};
use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use sea_orm::sea_query::{
    ArrayType, ColumnType, Nullable, StringLen, Value, ValueType, ValueTypeErr,
};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use utoipa::ToSchema;
use uuid::Uuid;

macro_rules! json_vec_type {
    ($name:ident, $item:ty) => {
        #[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
        #[serde(transparent)]
        pub struct $name(pub Vec<$item>);

        impl Default for $name {
            fn default() -> Self {
                Self(Vec::new())
            }
        }

        impl $name {
            pub fn into_inner(self) -> Vec<$item> {
                self.0
            }
        }

        impl Deref for $name {
            type Target = Vec<$item>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl DerefMut for $name {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }

        impl From<Vec<$item>> for $name {
            fn from(value: Vec<$item>) -> Self {
                Self(value)
            }
        }

        impl FromIterator<$item> for $name {
            fn from_iter<I: IntoIterator<Item = $item>>(iter: I) -> Self {
                Self(iter.into_iter().collect())
            }
        }

        impl IntoIterator for $name {
            type Item = $item;
            type IntoIter = std::vec::IntoIter<$item>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.into_iter()
            }
        }

        impl<'a> IntoIterator for &'a $name {
            type Item = &'a $item;
            type IntoIter = std::slice::Iter<'a, $item>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.iter()
            }
        }

        impl<'a> IntoIterator for &'a mut $name {
            type Item = &'a mut $item;
            type IntoIter = std::slice::IterMut<'a, $item>;

            fn into_iter(self) -> Self::IntoIter {
                self.0.iter_mut()
            }
        }

        impl From<$name> for Value {
            fn from(value: $name) -> Self {
                Value::Json(Some(Box::new(
                    serde_json::to_value(value.0).unwrap_or(serde_json::Value::Array(Vec::new())),
                )))
            }
        }

        impl Nullable for $name {
            fn null() -> Value {
                Value::Json(None)
            }
        }

        impl TryGetable for $name {
            fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
                let value: serde_json::Value = res.try_get_by(idx)?;
                let items = serde_json::from_value(value).map_err(|err| {
                    TryGetError::DbErr(sea_orm::DbErr::Type(format!(
                        "failed to deserialize JSON array: {err}"
                    )))
                })?;
                Ok(Self(items))
            }
        }

        impl ValueType for $name {
            fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
                match value {
                    Value::Json(Some(value)) => serde_json::from_value(*value)
                        .map(Self)
                        .map_err(|_| ValueTypeErr),
                    _ => Err(ValueTypeErr),
                }
            }

            fn type_name() -> String {
                stringify!($name).to_string()
            }

            fn array_type() -> ArrayType {
                ArrayType::Json
            }

            fn column_type() -> ColumnType {
                ColumnType::Json
            }
        }
    };
}

/// Re-exported here so existing imports keep working. Canonical home is
/// [`crate::property::JsonObject`] — the Facet/SeaORM bridge for any
/// JSON column that participates in crudcrate's auto-derived models.
pub use crate::property::JsonObject as JsonProperties;

json_vec_type!(StringList, String);
json_vec_type!(WikiLinkList, WikiLink);
json_vec_type!(TimeEntryList, TimeEntry);
json_vec_type!(TaskDependencyList, TaskDependency);
json_vec_type!(ReminderList, Reminder);
json_vec_type!(TaskRelationList, TaskRelation);
json_vec_type!(ReactionList, Reaction);
json_vec_type!(EmailRefList, crate::email::EmailRef);

/// A TaskNotes-compatible task stored as a vault .md file.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Default,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
    ToSchema,
)]
#[sea_orm(table_name = "tasks")]
#[crudcrate(
    api_struct = "TaskApi",
    generate_vox_service,
    name_singular = "task",
    name_plural = "tasks"
)]
pub struct Model {
    // r[impl task.id]
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(
        primary_key,
        exclude(create),
        on_create = uuid::Uuid::new_v4()
    )]
    pub id: Uuid,
    #[crudcrate(filterable, sortable, fulltext)]
    pub title: String,
    #[crudcrate(filterable, sortable)]
    pub status: Status,
    #[crudcrate(filterable, sortable)]
    pub priority: Priority,
    #[facet(default)]
    pub projects: WikiLinkList,
    #[facet(default)]
    pub contexts: StringList,
    #[facet(default)]
    pub tags: StringList,
    #[facet(default)]
    pub areas: WikiLinkList,
    pub due: Option<NaiveDate>,
    pub scheduled: Option<NaiveDate>,
    pub start: Option<NaiveDate>,
    pub due_time: Option<String>,
    pub date_created: Option<DateTime<Utc>>,
    pub date_modified: Option<DateTime<Utc>>,
    pub completed_date: Option<NaiveDate>,
    pub time_estimate: Option<u32>,
    #[facet(default)]
    pub time_entries: TimeEntryList,
    pub pomodoro_count: Option<u32>,
    pub recurrence: Option<String>,
    pub recurrence_anchor: RecurrenceAnchor,
    #[facet(default)]
    pub completed_instances: StringList,
    #[facet(default)]
    pub skipped_instances: StringList,
    #[facet(default)]
    pub blocked_by: TaskDependencyList,
    #[facet(default)]
    pub blocking: WikiLinkList,
    #[facet(default)]
    pub reminders: ReminderList,
    pub sort_order: Option<String>,
    pub external_id: Option<String>,
    pub external_source: Option<String>,

    // ── Collaboration fields ─────────────────────────────────────────
    /// Who this task is assigned to (username, e.g. "codywright").
    #[crudcrate(filterable)]
    pub assignee: Option<String>,

    /// Multiple assignees (in addition to primary assignee).
    #[facet(default)]
    pub assignees: StringList,

    /// Who created this task.
    #[crudcrate(filterable)]
    pub created_by: Option<String>,

    // ── Sequence ID (human-readable, per-project) ───────────────────
    /// Human-readable sequence number within the project (e.g. 42 → "PRJ-42").
    pub sequence_id: Option<u32>,

    // ── Type ────────────────────────────────────────────────────────
    /// Issue type: task, bug, story, epic, feature, chore, etc.
    #[crudcrate(filterable)]
    pub issue_type: Option<String>,

    // ── Relations ───────────────────────────────────────────────────
    /// Rich relations to other tasks.
    #[facet(default)]
    pub relations: TaskRelationList,

    // ── Subscribers / watchers ──────────────────────────────────────
    /// Users watching this task for updates.
    #[facet(default)]
    pub subscribers: StringList,

    // ── Reactions ───────────────────────────────────────────────────
    /// Emoji reactions on this task (e.g. "👍:cody", "🎉:amy").
    #[facet(default)]
    pub reactions: ReactionList,

    // ── Properties (Obsidian frontmatter sidecar) ────────────────────
    /// Workflow-defined frontmatter keys. Free-form `key → value` map
    /// stored as JSON. Excluded from list responses to keep them lean;
    /// fetched via single-entity get or PropertyService.
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[serde(default)]
    #[sea_orm(column_type = "Json", default_value = "{}")]
    pub properties: JsonProperties,

    // ── Soft deletion ───────────────────────────────────────────────
    /// Soft delete timestamp. If set, this task is "deleted" but recoverable.
    pub deleted_at: Option<DateTime<Utc>>,

    // ── Draft ───────────────────────────────────────────────────────
    /// Whether this is a draft (work-in-progress, not yet visible to team).
    #[facet(default)]
    #[crudcrate(filterable)]
    pub is_draft: bool,

    // ── Email routing ────────────────────────────────────────────────
    /// Matcher hints the bot uses to route incoming mail to this task.
    /// Typical values: tag labels ("task:mix-v3"), subject tokens,
    /// sender patterns. The bot (Jarvis) reads these and does its own
    /// matching.
    #[facet(default)]
    pub email_tags: StringList,

    /// Emails linked to this task. Reference-only, body stays in
    /// Nextcloud Mail.
    #[facet(default)]
    pub emails: EmailRefList,

    // ── Content ──────────────────────────────────────────────────────
    /// Markdown body content (after frontmatter).
    #[facet(skip)]
    #[facet(default)]
    pub body: String,

    // ── CRDT snapshot ────────────────────────────────────────────────
    /// Optional Loro snapshot bytes for the CRDT view of this task.
    /// Server-internal: never crosses the wire and is not part of the
    /// API surface generated by crudcrate.
    #[facet(skip)]
    #[facet(default)]
    #[serde(default, skip_serializing)]
    #[crudcrate(exclude(create, update, list, one))]
    pub crdt_snapshot: Option<Vec<u8>>,
}

// r[impl task.status]
#[derive(
    Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize, ToSchema, EnumIter, DeriveActiveEnum,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(32))")]
#[repr(u8)]
#[derive(Default)]
pub enum Status {
    #[sea_orm(string_value = "none")]
    None,
    #[sea_orm(string_value = "open")]
    #[default]
    Open,
    #[sea_orm(string_value = "in_progress")]
    InProgress,
    #[sea_orm(string_value = "on_hold")]
    OnHold,
    #[sea_orm(string_value = "planned")]
    Planned,
    #[sea_orm(string_value = "done")]
    Done,
    #[sea_orm(string_value = "cancelled")]
    Cancelled,
    #[sea_orm(string_value = "archived")]
    Archived,
}

impl Status {
    pub fn is_completion(&self) -> bool {
        matches!(self, Status::Done)
    }
}

// r[impl task.priority]
#[derive(
    Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize, ToSchema, EnumIter, DeriveActiveEnum,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(16))")]
#[repr(u8)]
#[derive(Default)]
pub enum Priority {
    #[sea_orm(string_value = "none")]
    #[default]
    None,
    #[sea_orm(string_value = "low")]
    Low,
    #[sea_orm(string_value = "normal")]
    Normal,
    #[sea_orm(string_value = "high")]
    High,
    #[sea_orm(string_value = "urgent")]
    Urgent,
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
#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
#[facet(transparent)]
pub struct WikiLink(pub String);

impl From<WikiLink> for Value {
    fn from(value: WikiLink) -> Self {
        Value::String(Some(Box::new(value.0)))
    }
}

impl Nullable for WikiLink {
    fn null() -> Value {
        Value::String(None)
    }
}

impl TryGetable for WikiLink {
    fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
        let value: Option<String> = res.try_get_by(idx)?;
        Ok(Self(value.unwrap_or_default()))
    }
}

impl ValueType for WikiLink {
    fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
        match value {
            Value::String(Some(value)) => Ok(Self(*value)),
            _ => Err(ValueTypeErr),
        }
    }

    fn type_name() -> String {
        stringify!(WikiLink).to_string()
    }

    fn array_type() -> ArrayType {
        ArrayType::String
    }

    fn column_type() -> ColumnType {
        ColumnType::String(StringLen::None)
    }
}

/// A structured task dependency per RFC 9253.
// r[impl task.blocked-by]
#[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
pub struct TaskDependency {
    pub uid: String,
    pub reltype: DependencyRelType,
    /// Optional ISO 8601 duration gap, e.g. "P1D".
    pub gap: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
#[repr(u8)]
#[derive(Default)]
pub enum DependencyRelType {
    #[default]
    FinishToStart,
    FinishToFinish,
    StartToStart,
    StartToFinish,
}

/// A logged work session.
///
/// A running timer is represented by `end_time == None`. Once stopped, `end_time`
/// is set and the duration can be computed. Billable rate is stored in cents per
/// hour (matches solidtime's integer-money convention; no floats).
// r[impl task.time.entries]
#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct TimeEntry {
    /// Stable unique ID — required for CRDT merging of the time-entry list.
    #[facet(default)]
    pub id: String,
    /// User who tracked the time.
    #[facet(default)]
    pub user: Option<String>,
    pub start_time: DateTime<Utc>,
    pub end_time: Option<DateTime<Utc>>,
    pub description: Option<String>,
    /// Whether this entry is billable.
    #[facet(default)]
    pub billable: bool,
    /// Billable rate override, in cents per hour. Falls back to project/member/org.
    #[facet(default)]
    pub billable_rate: Option<u32>,
    /// Per-entry tags (separate from task-level tags).
    #[facet(default)]
    pub tags: Vec<String>,
    /// Source if imported from another tool (e.g. "toggl", "clockify").
    #[facet(default)]
    pub imported_from: Option<String>,

    // ── Invoicing ─────────────────────────────────────────────────────
    /// When this entry was rolled up into an invoice. Non-None means
    /// "don't include in future invoice generation runs".
    #[facet(default)]
    pub invoiced_at: Option<DateTime<Utc>>,

    /// Hashed Invoice Ninja invoice id this entry was billed on.
    #[facet(default)]
    pub invoice_ninja_invoice_id: Option<String>,
}

impl TimeEntry {
    /// A running timer has no `end_time`.
    pub fn is_running(&self) -> bool {
        self.end_time.is_none()
    }

    /// Duration in minutes. Returns 0 for running timers.
    pub fn duration_minutes(&self) -> u32 {
        match self.end_time {
            Some(end) => {
                let mins = (end - self.start_time).num_minutes();
                if mins > 0 { mins as u32 } else { 0 }
            }
            None => 0,
        }
    }

    /// Elapsed minutes for a running timer, measured from `now`.
    pub fn elapsed_minutes(&self, now: DateTime<Utc>) -> u32 {
        let mins = (now - self.start_time).num_minutes();
        if mins > 0 { mins as u32 } else { 0 }
    }

    /// Billable amount in cents, given a resolved rate (cents/hour).
    pub fn amount_cents(&self, resolved_rate: u32) -> u64 {
        let mins = self.duration_minutes() as u64;
        (mins * resolved_rate as u64) / 60
    }
}

// r[impl task.recurrence.anchor]
#[derive(
    Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize, ToSchema, EnumIter, DeriveActiveEnum,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(16))")]
#[repr(u8)]
#[derive(Default)]
pub enum RecurrenceAnchor {
    /// Next occurrence from scheduled date — fixed calendar.
    #[sea_orm(string_value = "scheduled")]
    #[default]
    Scheduled,
    /// Next occurrence from completion date — flexible.
    #[sea_orm(string_value = "completion")]
    Completion,
}

// r[impl task.reminders]
#[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
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

#[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
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
#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct TaskRelation {
    /// The related task (wiki-link or ID).
    pub target: String,
    /// Type of relationship.
    pub relation_type: RelationType,
}

/// Relationship types between tasks (bidirectional pairs).
#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
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
#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct Reaction {
    /// Emoji (unicode, e.g. "👍", "🎉", "🔥").
    pub emoji: String,
    /// Who reacted.
    pub user: String,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

impl Model {
    pub fn id_ref(&self) -> String {
        self.id.to_string()
    }

    pub fn matches_reference(&self, reference: &str) -> bool {
        self.id.to_string() == reference || self.title.eq_ignore_ascii_case(reference)
    }

    // r[impl task.urgency]
    pub fn urgency_score(&self) -> i32 {
        let priority_weight = self.priority.weight();
        let days_until = self
            .days_until_due()
            .or_else(|| self.days_until_scheduled());

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
            || self
                .relations
                .iter()
                .any(|r| r.relation_type == RelationType::BlockedBy)
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
        self.time_entries.iter().map(|e| e.duration_minutes()).sum()
    }

    /// The currently-running time entry, if any.
    pub fn running_timer(&self) -> Option<&TimeEntry> {
        self.time_entries.iter().find(|e| e.is_running())
    }

    /// Total billable amount in cents, given a fallback rate (applied to entries
    /// that don't have their own `billable_rate`). Non-billable entries contribute 0.
    pub fn billable_amount_cents(&self, fallback_rate: u32) -> u64 {
        self.time_entries
            .iter()
            .filter(|e| e.billable)
            .map(|e| e.amount_cents(e.billable_rate.unwrap_or(fallback_rate)))
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
                } else {
                    trimmed.strip_prefix("- [x] ").map(|rest| Subtask {
                        title: rest.to_string(),
                        done: true,
                    })
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
        let task = Model {
            body: "## Checklist\n- [x] Done thing\n- [ ] Not done\n- [x] Also done\nSome other text\n- [ ] Another".into(),
            ..Default::default()
        };
        assert_eq!(task.subtask_count(), 4);
        assert_eq!(task.subtasks_done(), 2);
        assert_eq!(task.subtask_progress(), Some(0.5));
    }

    #[test]
    fn no_subtasks() {
        let task = Model {
            body: "Just some notes".into(),
            ..Default::default()
        };
        assert_eq!(task.subtask_count(), 0);
        assert_eq!(task.subtask_progress(), None);
    }

    #[test]
    fn running_timer_and_billable() {
        use chrono::TimeZone;
        let start = Utc.with_ymd_and_hms(2026, 4, 17, 9, 0, 0).unwrap();
        let end = Utc.with_ymd_and_hms(2026, 4, 17, 10, 30, 0).unwrap();

        let finished = TimeEntry {
            id: "e1".into(),
            user: Some("cody".into()),
            start_time: start,
            end_time: Some(end),
            billable: true,
            billable_rate: Some(12_000), // $120/hr
            ..Default::default()
        };
        assert_eq!(finished.duration_minutes(), 90);
        assert_eq!(finished.amount_cents(12_000), 18_000); // 1.5h * $120 = $180

        let running = TimeEntry {
            id: "e2".into(),
            user: Some("cody".into()),
            start_time: start,
            end_time: None,
            ..Default::default()
        };
        assert!(running.is_running());
        assert_eq!(running.duration_minutes(), 0);

        let task = Model {
            time_entries: vec![finished, running.clone()].into(),
            ..Default::default()
        };
        assert!(task.running_timer().is_some());
        // Only the finished billable entry contributes.
        assert_eq!(task.billable_amount_cents(10_000), 18_000);
    }

    #[test]
    fn facet_json_serializes_task() {
        let task = Model {
            title: "Serializable task".into(),
            tags: vec!["wire".to_string()].into(),
            projects: vec![WikiLink("Project".into())].into(),
            ..Default::default()
        };

        let json = facet_json::to_string(&task).expect("task should serialize");
        assert!(json.contains("Serializable task"));
    }
}
