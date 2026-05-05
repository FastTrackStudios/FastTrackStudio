//! Task entity re-export.
//!
//! The canonical task database entity lives in `task-core::task::model` so the
//! model and its generated crudcrate surface are defined once. `task-db` keeps
//! this module for migration/relation paths such as `super::task::Entity`.

pub use task_core::task::model::*;
