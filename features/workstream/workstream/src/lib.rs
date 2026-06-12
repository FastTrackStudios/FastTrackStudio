#![allow(unexpected_cfgs)]

//! `workstream` — the project-scoped parent-with-swarm
//! construct that replaces the `epic` tag.
//!
//! The wasm-clean wire surface ([`Workstream`] / [`Status`] /
//! [`WorkstreamRollup`] + the [`WorkstreamService`] RPC trait)
//! lives in the sibling [`workstream_proto`] crate; this crate
//! sits on top of it and owns the vault-backed side:
//! - [`parse_page`] / [`looks_like_workstream`] — vault page →
//!   `Workstream`
//! - [`serialize_workstream`] / [`write_workstream`] /
//!   [`default_workstream_path`] — writer + path helper
//! - [`rollup`] — pure derived-progress engine over
//!   `task::TaskInfo` rows
//! - [`WorkstreamBackend`] — server impl of
//!   [`WorkstreamService`] (CRUD + rollup + event stream)
//!
//! See `Workstream` doc-comments for the lead / members /
//! rollup design.

pub mod model;
pub mod parse;
pub mod rollup;
pub mod service;
pub mod write;

#[cfg(not(target_arch = "wasm32"))]
pub mod backend;

pub use model::{AgentRefList, Links, Status, Workstream};
pub use parse::{ParseError, looks_like_workstream, parse_page, parse_workstream};
pub use rollup::{estimate_points, rollup};
pub use service::{
    WorkstreamError, WorkstreamEvent, WorkstreamRollup, WorkstreamService, WorkstreamWithRollup,
};
pub use write::{WriteError, default_workstream_path, serialize_workstream, write_workstream};

#[cfg(not(target_arch = "wasm32"))]
pub use backend::WorkstreamBackend;

#[cfg(feature = "vox")]
pub use workstream_proto::{
    WorkstreamServiceBridge, WorkstreamServiceClient, WorkstreamServiceStreamClient,
    serve_workstream_service, workstream_service_descriptor, workstream_service_layer,
    workstream_service_stream_layer, workstream_stream_descriptor,
};
#[cfg(feature = "vox")]
pub use workstream_proto::{
    WorkstreamServiceRpc, WorkstreamServiceStreamSource,
    WorkstreamDispatcher,
};
