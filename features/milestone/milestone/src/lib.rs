#![allow(unexpected_cfgs)]

//! `milestone` — project-scoped, GitHub-Projects-style
//! checkpoint entity.
//!
//! Surface:
//! - [`Milestone`] / [`Status`] — model + canonical states
//! - [`parse_page`] / [`looks_like_milestone`] — vault page →
//!   `Milestone`
//! - [`serialize_milestone`] / [`write_milestone`] /
//!   [`default_milestone_path`] — writer + path helper
//! - [`MilestoneService`] (RPC trait) +
//!   [`MilestoneBackend`] (server impl)
//!
//! See `Milestone` doc-comments for the project / goal /
//! Forgejo-sync rollup design.

pub mod model;
pub mod parse;
pub mod write;

#[cfg(not(target_arch = "wasm32"))]
pub mod backend;
pub mod service;

pub use model::{Milestone, Status, Tags};
pub use parse::{ParseError, looks_like_milestone, parse_milestone, parse_page};
pub use service::{MilestoneError, MilestoneService, MilestoneServiceRpc};
pub use write::{WriteError, default_milestone_path, serialize_milestone, write_milestone};

#[cfg(not(target_arch = "wasm32"))]
pub use backend::MilestoneBackend;

#[cfg(feature = "vox")]
pub use service::{
    MilestoneServiceClient, MilestoneServiceRpcDispatcher as MilestoneDispatcher,
    Service as MilestoneServiceBridge, layer as milestone_service_layer,
    milestone_service_rpc_service_descriptor as milestone_service_descriptor,
    serve as serve_milestone_service,
};
