mod core;
#[cfg(not(target_arch = "wasm32"))]
mod router;
pub mod service;

pub use core::Project;
#[cfg(not(target_arch = "wasm32"))]
pub use router::create_project_http_router;

// Roam service exports
pub use service::{ProjectCommand, ProjectEvent, ProjectInfo, ProjectService};
