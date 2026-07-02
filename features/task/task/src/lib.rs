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
//! - [`parse_page`] — `vault_proto::VaultPage` → `TaskInfo`.
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
//!
//! ## Wasm
//!
//! The wire/file-format surface (`model`, `parse`, `service` and
//! its `vox` client) compiles on `wasm32` so `task-ui` can drive
//! a `TaskServiceClient` from the browser. The FS-dependent
//! modules (`scan`, `write`, `backend` — they open a
//! `vault::Vault` or touch `std::fs`) sit behind a
//! `not(target_arch = "wasm32")` gate so the wasm crate graph
//! never inherits `vault`'s tokio-net transitive. Mirrors
//! `project`.

pub mod capture;
pub mod model;
pub mod parse;
pub mod relations;
pub mod relevance;
pub mod service;

// FS-dependent modules (vault::Vault, std::fs walks). The
// wasm-targeted UI imports the wire types + RPC client only,
// not these.
#[cfg(not(target_arch = "wasm32"))]
pub mod scan;
#[cfg(not(target_arch = "wasm32"))]
pub mod write;

#[cfg(not(target_arch = "wasm32"))]
pub mod backend;

pub use capture::capture;
pub use model::{
    Priority, Relation, RelationKind, Status, TaskInfo, TimeEntry, is_due_on_or_before,
    status_is_open, status_is_terminal,
};
pub use relations::{ReverseRelation, arrange_families};
pub use relevance::{RelevanceContext, filter_relevant, is_relevant, relevance_rank};
// Workflow actor/audit types referenced by `WorkflowAttrs` — re-exported
// so UI consumers don't need their own workflows-proto dep.
pub use parse::{ParseError, parse_page, parse_str};
pub use service::{
    TaskError, TaskEvent, TaskListFilter, TaskReverseRelations, TaskService, TaskServiceRpc,
};
pub use workflows_proto;

#[cfg(not(target_arch = "wasm32"))]
pub use backend::TaskBackend;
#[cfg(not(target_arch = "wasm32"))]
pub use scan::scan_vault;
#[cfg(not(target_arch = "wasm32"))]
pub use write::{WriteError, serialize_task, write_task};

#[cfg(feature = "vox")]
pub use service::{
    Service as TaskServiceBridge, TaskServiceClient, TaskServiceRpcDispatcher as TaskDispatcher,
    layer as task_service_layer, serve as serve_task_service,
    task_service_rpc_service_descriptor as task_service_descriptor,
};

// `#[subscribe] fn events` stream sibling — live task changes.
// Mount `task_service_stream_layer(backend)` next to the base
// service; subscribers drive a `TaskServiceStreamClient`.
#[cfg(feature = "vox")]
pub use service::{
    TaskServiceStream, TaskServiceStreamClient, TaskServiceStreamSource,
    stream_layer as task_service_stream_layer, stream_serve as serve_task_service_stream,
    task_service_stream_service_descriptor as task_stream_descriptor,
};
