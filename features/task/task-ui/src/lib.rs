//! Tasks UI — TaskNotes-inspired list + kanban over a flat
//! `Vec<TaskInfo>`.
//!
//! This crate is intentionally storage-agnostic. The sibling
//! `task` crate (file-backed, desktop-only today) is not in the
//! dep tree because it gates its model behind
//! `#[cfg(not(target_arch = "wasm32"))]`. We re-define a
//! wasm-friendly mirror of its `TaskInfo` shape here so the web
//! build compiles. Field names match the TaskNotes frontmatter
//! verbatim so the eventual swap is mechanical.

//!
//! The detail-page family ([`TaskDetailFull`] + sections) is the
//! exception: it consumes the full persistence shape
//! (`task::TaskInfo` + `WorkflowAttrs`, `workflows_proto`
//! audit records) directly via props — the page layer already
//! holds those rows, so no second mirror is needed there.

pub mod markdown;
pub mod model;
pub mod mutation;
pub mod store;
pub mod views;

pub use markdown::{Markdown, MarkdownProps, parse_markdown};
pub use model::{Priority, Status, TaskInfo, TimeEntry, clock_label, duration_label};
pub use mutation::TaskMutation;
pub use store::{TaskState, apply};
pub use views::{
    ClaimState, LinkChips, LinkedTaskRef, SessionEvent, SessionHistory, SubtaskRow, SubtasksBoard,
    TaskDetailFull, TaskDetailFullProps, TasksApp, TasksAppProps, TimeSection, WorkflowSection,
};
