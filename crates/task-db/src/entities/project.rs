//! Project entity re-export.
//!
//! The canonical project database entity lives in `task-core::project::model`
//! so the domain model and generated crudcrate surface are defined once.

pub use task_core::project::model::*;
