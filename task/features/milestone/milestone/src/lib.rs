#![allow(unexpected_cfgs)]

//! `milestone` — project-scoped, GitHub-Projects-style
//! checkpoint entity.
//!
//! The wasm-clean wire surface ([`Milestone`] / [`Status`] +
//! the [`MilestoneService`] RPC trait) lives in the sibling
//! [`milestone_proto`] crate; this crate sits on top of it and
//! owns the vault-backed side:
//! - [`parse_page`] / [`looks_like_milestone`] — vault page →
//!   `Milestone`
//! - [`serialize_milestone`] / [`write_milestone`] /
//!   [`default_milestone_path`] — writer + path helper
//! - [`MilestoneBackend`] — server impl of [`MilestoneService`]
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
pub use service::{MilestoneError, MilestoneService};
pub use write::{WriteError, default_milestone_path, serialize_milestone, write_milestone};

#[cfg(not(target_arch = "wasm32"))]
pub use backend::MilestoneBackend;

#[cfg(feature = "vox")]
pub use milestone_proto::{
    MilestoneServiceBridge, MilestoneServiceClient, milestone_service_descriptor,
    milestone_service_layer, serve_milestone_service,
};
#[cfg(feature = "vox")]
pub use service::{MilestoneServiceRpc, MilestoneServiceRpcDispatcher as MilestoneDispatcher};
