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

pub mod model;
pub mod mutation;
pub mod store;
pub mod views;

pub use model::{Priority, Status, TaskInfo, TimeEntry};
pub use mutation::TaskMutation;
pub use store::{TaskState, apply};
pub use views::{TasksApp, TasksAppProps};
