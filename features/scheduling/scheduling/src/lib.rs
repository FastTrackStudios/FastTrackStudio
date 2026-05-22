//! `scheduling` — markdown round-trip + in-memory backend.
//!
//! Sits on top of `scheduling-proto`'s wire types. Two halves:
//!
//! 1. **Markdown ↔ proto** — parse + write day templates, event
//!    types, schedules, and bookings as plain markdown files inside
//!    a `vault::Vault`. Frontmatter carries the structured fields;
//!    body is free-form notes.
//! 2. **`InMemoryScheduler`** — a `SchedulingService` impl backed
//!    by `IndexMap`s. Used by the task-ui demo route + the test
//!    suite; a real `VaultScheduler` lands in a follow-up.
//!
//! Native-only because the parse/write paths touch the disk-
//! backed vault. The UI crate (`scheduling-ui`) builds for wasm
//! and stays decoupled.

#![cfg(not(target_arch = "wasm32"))]

pub mod memory;
pub mod model;
pub mod parse;
pub mod scan;
pub mod write;

pub use memory::InMemoryScheduler;
pub use model::{FrontmatterKind, frontmatter_kind};
pub use parse::ParseError;
pub use scan::ScanError;
pub use write::WriteError;

pub use scheduling_proto::*;
