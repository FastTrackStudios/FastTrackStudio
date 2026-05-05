//! Task feature module.
//!
//! `model` contains the canonical task model used by markdown/local/CRDT and
//! SQL-backed workflows.

pub mod model;

pub use model::{
    DependencyRelType, Model, Priority, Reaction, RecurrenceAnchor, RelationType, Reminder,
    ReminderAnchor, Status, TaskDependency, TaskRelation, TimeEntry, WikiLink,
};

pub type Task = Model;
