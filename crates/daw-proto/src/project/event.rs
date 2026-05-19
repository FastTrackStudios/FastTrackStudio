//! Project lifecycle events.

use super::ProjectInfo;
use facet::Facet;

/// Events emitted when project state changes.
#[repr(u8)]
#[derive(Debug, Clone, Facet)]
pub enum ProjectEvent {
    /// A project was opened/added.
    Opened(ProjectInfo),
    /// A project was closed (contains the GUID).
    Closed(String),
    /// The active/current project changed (contains new current project
    /// GUID, or None).
    CurrentChanged(Option<String>),
    /// A project's metadata was modified.
    Changed(ProjectInfo),
    /// Full project list refresh (e.g., after reconnection).
    ProjectsChanged(Vec<ProjectInfo>),
}
