//! Top-level view tree.

mod detail;
mod detail_full;
mod kanban;
mod links;
mod list;
mod palette;
mod quick_add;
mod row;
mod session_history;
mod subtasks;
mod tasks_app;
mod time;
mod workflow;

pub use detail_full::{
    resolve_links, short_id, ClaimState, LinkedTaskRef, SubtaskRow, TaskDetailFull,
    TaskDetailFullProps,
};
pub use links::{LinkChips, LinkChipsProps};
pub use session_history::{
    activity_label, merge_session_events, payload_preview, SessionEvent, SessionHistory,
    SessionHistoryProps,
};
pub use subtasks::{subtask_summary, SubtasksBoard, SubtasksBoardProps};
pub use tasks_app::{TasksApp, TasksAppProps};
pub use time::{
    format_minutes, recurrence_summary, sum_logged_minutes, TimeSection, TimeSectionProps,
};
pub use workflow::{estimate_label, WorkflowSection, WorkflowSectionProps};
