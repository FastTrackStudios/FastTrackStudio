//! Project — types, service traits, events.
//!
//! `ProjectContext` is the per-method scope marker every architect::rpc
//! service uses. `ProjectInfo` is the metadata struct. `ProjectEvent`
//! is the lifecycle event stream. `ProjectService` is the async vox
//! service; `Project` is the sync per-project handle trait.

mod event;
mod service;
mod types;

pub use event::ProjectEvent;
pub use service::{
    ProjectService, ProjectServiceClient, ProjectServiceDispatcher, Projects,
    project_service_service_descriptor,
};
pub use types::{ProjectContext, ProjectInfo};
