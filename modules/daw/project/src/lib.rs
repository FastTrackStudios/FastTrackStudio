mod project;
mod router;
pub mod app;

pub use project::{ProjectActions, ProjectError, ProjectProvider};
pub use router::create_project_http_router;
pub use app::{DawProject, DesktopProjectManager};
