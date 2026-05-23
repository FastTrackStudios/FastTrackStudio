// architect's `Entity` derive emits a `#[cfg(feature = "vox")]`
// block; we don't expose a vox feature here (project is a
// wire/file format, not a service trait) so the gated code
// never compiles, but rustc still flags the cfg as unknown.
// Allow at crate scope until architect grows opt-out support.
#![allow(unexpected_cfgs)]

//! `project` — first-party project feature.
//!
//! Projects are plain markdown pages living under
//! `Projects/*.md` in the vault. Frontmatter carries the
//! identity (stable `id:` UUID), display fields (title,
//! description, color), status, and the billing knobs that
//! downstream features (`timer`, `finances`,
//! `agent-dispatch`) need to know about a project without
//! opening a database.
//!
//! ## Why on disk, not in a database
//!
//! Same answer as `task`: the markdown file is the user's
//! mental model and the lowest-friction surface for editing.
//! Downstream features that need stable references use the
//! `id:` field in the frontmatter (a UUID set on first save),
//! not the file path — so renaming a project file doesn't
//! break the timer rows or invoice lines that reference it.
//!
//! ## Surface
//!
//! - [`ProjectInfo`] — the parsed project model.
//! - [`Status`] — a configurable enum (default set mirrors
//!   common project lifecycles).
//! - [`parse_page`] / [`parse_str`] — `VaultPage` → `ProjectInfo`.
//! - [`serialize_project`] / [`write_project`] —
//!   `ProjectInfo` → markdown bytes on disk.
//! - [`scan_vault`] — collect every `type: project` page from
//!   a `vault::Vault`.
//! - [`looks_like_project`] — discriminator used by the
//!   scanner.

#![cfg(not(target_arch = "wasm32"))]

pub mod model;
pub mod parse;
pub mod scan;
pub mod write;

pub use model::{ProjectInfo, Status};
pub use parse::{ParseError, looks_like_project, parse_page, parse_str};
pub use scan::scan_vault;
pub use write::{WriteError, serialize_project, write_project};
