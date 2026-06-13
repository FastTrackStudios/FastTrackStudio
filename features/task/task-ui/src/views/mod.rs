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
    ClaimState, LinkedTaskRef, SubtaskRow, TaskDetailFull, TaskDetailFullProps, resolve_links,
    short_id,
};
pub use links::{LinkChips, LinkChipsProps};
pub use session_history::{
    SessionEvent, SessionHistory, SessionHistoryProps, activity_label, merge_session_events,
    payload_preview,
};
pub use subtasks::{SubtasksBoard, SubtasksBoardProps, subtask_summary};
pub use tasks_app::{TasksApp, TasksAppProps};
pub use time::{
    TimeSection, TimeSectionProps, format_minutes, recurrence_summary, sum_logged_minutes,
};
pub use workflow::{WorkflowSection, WorkflowSectionProps, estimate_label};
