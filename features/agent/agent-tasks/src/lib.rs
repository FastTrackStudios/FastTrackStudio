//! SQLite-backed agent-task queues.
//!
//! Implements `agent_proto::service::tasks::AgentTaskQueue`
//! against per-queue SQLite files. See `plans/agent-dispatch.md`
//! for the full design.
//!
//! ## Surface
//!
//! - [`Store`] — opens a queue directory and exposes the trait
//!   impl. Backed by `rusqlite`.
//! - [`schema::SCHEMA_SQL`] — single source of truth for the DDL.
//! - [`error::TasksDbError`] — internal error type that converts
//!   into `AgentError` for the trait surface.
//!
//! Concurrency: each `Store` owns one `Connection` behind a
//! `Mutex`. The dispatcher's atomic-claim path uses a single
//! `UPDATE … WHERE claim_lock IS NULL` so two workers can't
//! double-claim; the rest of the API is serialized by the mutex
//! and is fine for the single-user / single-server case.
//!
//! Multi-machine deployment (the planned server-side dispatcher)
//! runs ONE `Store` per queue file on the server; clients reach
//! it via vox RPCs through `agent-proto`.

mod conn;
mod error;
mod schema;
mod store;

pub use error::TasksDbError;
pub use store::Store;
