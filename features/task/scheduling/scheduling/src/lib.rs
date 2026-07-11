//! `scheduling` — markdown round-trip + backends.
//!
//! Sits on top of `scheduling-proto`'s wire types. Two halves:
//!
//! 1. **Markdown ↔ proto** — parse + write day templates, event
//!    types, schedules, and bookings as plain markdown files inside
//!    a `vault::Vault`. Frontmatter carries the structured fields;
//!    body is free-form notes.
//! 2. **Backends** — `InMemoryScheduler` (tests + demo) and
//!    `VaultScheduler` (disk-backed; impls every capability
//!    sub-trait against a real `vault::Vault` + a
//!    `store-proto::KvStore` / `LogStore` for sidecar state).
//!
//! Native-only because the parse/write paths touch the disk-
//! backed vault. The UI crate (`scheduling-ui`) builds for wasm
//! and stays decoupled.

#![cfg(not(target_arch = "wasm32"))]

pub mod memory;
pub mod model;
pub mod parse;
pub mod scan;
pub mod slots;
pub mod vault_scheduler;
pub mod write;

pub use memory::InMemoryScheduler;
pub use model::{FrontmatterKind, frontmatter_kind};
pub use parse::ParseError;
pub use scan::ScanError;
pub use vault_scheduler::{VaultScheduler, VaultSchedulerError};
pub use write::WriteError;

pub use scheduling_proto::*;
