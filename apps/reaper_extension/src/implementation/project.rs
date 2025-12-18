//! REAPER Project Wrapper
//!
//! Wraps REAPER projects using the Project<T> generic type with ReaperTransport.

use daw::project::Project;
use reaper_high::Project as ReaperProject;
use fts::setlist::infra::reaper::ReaperTransport;

/// REAPER project wrapper that implements TransportActions
pub type ReaperProjectWrapper = Project<ReaperTransport>;

/// Helper function to create a REAPER project wrapper from a REAPER project
/// 
/// `file_path` should be the project's file path if available (from enum_projects result)
pub fn create_reaper_project_wrapper(
    reaper_project: ReaperProject,
    file_path: Option<&std::path::Path>,
) -> ReaperProjectWrapper {
    let transport = ReaperTransport::new(reaper_project);
    
    // Get project name from file path or use "Untitled"
    let (name, path) = if let Some(file_path) = file_path {
        let name = file_path
            .file_stem()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_else(|| "unsaved".to_string());
        (name, Some(file_path.to_string_lossy().to_string()))
    } else {
        ("unsaved".to_string(), None)
    };
    
    let mut project = Project::new(name, transport);
    if let Some(path) = path {
        project.set_path(path);
    }
    
    project
}

