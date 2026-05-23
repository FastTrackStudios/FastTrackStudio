// architect's Entity derive emits cfg-gated blocks; allow
// at crate scope.
#![allow(unexpected_cfgs)]

//! `task` — first-party task feature.
//!
//! Tasks are plain markdown pages with YAML frontmatter living
//! inside a `vault::Vault`. The schema mirrors
//! [callumalpass/tasknotes](https://github.com/callumalpass/tasknotes)
//! so existing `TaskNotes` vaults round-trip into Task without
//! conversion.
//!
//! Surface:
//! - [`TaskInfo`] — the parsed task model.
//! - [`Status`] / [`Priority`] — configurable enums (default set
//!   mirrors `TaskNotes` defaults).
//! - [`parse_page`] — `vault::VaultPage` → `TaskInfo`.
//! - [`serialize_task`] — `TaskInfo` → markdown bytes.
//! - [`scan_vault`] — collect every `type: task` (or
//!   `tags: [task]`) page from a `vault::Vault`.
//! - [`capture`] — minimal natural-language capture: parse
//!   `"Buy milk tomorrow #errands @shopping"` into a
//!   `TaskInfo`. Date keywords: today / tomorrow / next-<day>.
//!
//! Higher-level views (kanban, calendar) ride on `vault-live`'s
//! `.base` query DSL via formulas + filters; they live in
//! `task-ui` (future) and don't need anything from this crate
//! beyond `TaskInfo`.

#![cfg(not(target_arch = "wasm32"))]

pub mod capture;
pub mod model;
pub mod parse;
pub mod scan;
pub mod write;

pub use capture::capture;
pub use model::{Priority, Status, TaskInfo, TimeEntry};
pub use parse::{ParseError, parse_page, parse_str};
pub use scan::scan_vault;
pub use write::{WriteError, serialize_task, write_task};
