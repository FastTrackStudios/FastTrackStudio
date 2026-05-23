// architect's Entity derive emits cfg-gated blocks; allow
// at crate scope.
#![allow(unexpected_cfgs)]

//! `goal` — long-term aspirations + their decomposition.
//!
//! A [`Goal`] is a typed entity distinct from
//! [`project::ProjectInfo`]: it carries a `target_date` and a
//! `kind` (`lifetime` / `yearly` / `quarterly` / `cycle` /
//! `weekly`) so a multi-year ambition like "Buy a house" can
//! decompose into a yearly goal ("Save $X by EOY"), which
//! decomposes into quarterly milestones, which decompose into
//! cycle-level work — all in the same entity, linked by
//! `parent_id`.
//!
//! Markdown frontmatter stays the source of truth. The
//! SeaORM Model emitted under `--features server` provides
//! indexed lookups (by status, by target date, by parent) for
//! UI surfaces that need to filter without re-walking the
//! vault.
//!
//! Convention: goal pages live at
//! `vault/Goals/<slug>.md` for top-level lifetime goals, and
//! `vault/Goals/<parent-slug>/<slug>.md` for nested
//! decompositions. The `parent_id` field is what the DB
//! reads; the folder layout is just for human navigation.
//!
//! See `plans/cyclic-life-calendar.md` for the planning
//! system goals plug into.

mod model;
mod parse;
mod write;

pub use model::{Goal, Kind, Status, Tags};
pub use parse::{ParseError, looks_like_goal, parse_goal, parse_page};
pub use write::{WriteError, default_goal_path, serialize_goal, write_goal};
