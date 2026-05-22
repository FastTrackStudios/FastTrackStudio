//! `timer-proto` — billable time-tracking wire contract.
//!
//! Modeled on [SolidTime](https://github.com/solidtime-io/solidtime).
//! Same conceptual entities, with one big rename:
//!
//! - **Client** — the customer being billed.
//! - **Project** — *NOT* a separate entity here. The
//!   `features/project` crate already defines projects as
//!   markdown pages with stable `id:` UUIDs in their
//!   frontmatter. A [`WorkSession`] references the project
//!   via that UUID (`project_id`) plus the vault path
//!   (`project_path`); the timer DB does not duplicate the
//!   project metadata. This is the same pattern
//!   `agent-tasks` uses for `source_task`.
//! - **WorkSession** — one tracked period of work.
//!   (SolidTime calls this `TimeEntry`; we use the longer
//!   name to disambiguate from `task::TimeEntry`.)
//! - **Tag** — labels, many-to-many with `WorkSession`.
//! - **Rate** — hourly billable rate, two levels:
//!   `ProjectMemberRate` (per project + user) and
//!   `OrgMemberRate` (per org + user). The cascade falls
//!   through to the project note's `defaultRateCents`
//!   field, which `features/project` reads off disk.
//!
//! ## Storage
//!
//! Every entity uses `#[derive(architect::Entity)]`. Each
//! gets the wasm-clean wire struct, `<Name>Create` /
//! `<Name>Update` / `<Name>List` payloads, the `<Name>Repo`
//! `#[vox::service]` trait, and (with `--features server`)
//! the SeaORM `Model`/`Entity`/`Column`/`Relation`/`ActiveModel`
//! plus `<Name>RepoStorage<C>`.
//!
//! ## Money
//!
//! Monetary values are integer cents (`i64`) plus an ISO 4217
//! `currency: String`. Currency lives on the project note;
//! the work session snapshots it on close.
//!
//! ## Active-timer invariant
//!
//! At most one [`WorkSession`] per `user_id` with `end_time =
//! NULL`. Enforced at the DB level via a partial unique index
//! in the `timer` crate's migrations.

pub mod client;
pub mod error;
pub mod rate;
pub mod service;
pub mod session;
pub mod tag;

pub use client::Client;
pub use error::TimerError;
pub use rate::{OrgMemberRate, ProjectMemberRate};
pub use service::TimerService;
pub use session::{WorkSession, WorkSessionFilter};
pub use tag::{Tag, WorkSessionTag};
