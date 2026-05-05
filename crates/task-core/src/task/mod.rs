//! Task feature module.
//!
//! `model` contains the canonical task model used by markdown/local/CRDT and
//! SQL-backed workflows.

pub mod model;

pub use model::*;

pub type Task = Model;
