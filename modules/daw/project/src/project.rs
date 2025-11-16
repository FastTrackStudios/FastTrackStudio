use ts_rs::TS;

/// Core project actions trait that any project implementation can use
pub trait ProjectActions {
    fn get_name(&self) -> &str;
    fn set_name(&mut self, name: String);
}

/// Trait for containers that can provide projects by name/id
pub trait ProjectProvider {
    type Project: ProjectActions;
    type Error;

    fn get_project(&self, name: &str) -> Result<&Self::Project, Self::Error>;
    fn get_project_mut(&mut self, name: &str) -> Result<&mut Self::Project, Self::Error>;
    fn list_projects(&self) -> Vec<String>;
    fn create_project(&mut self, name: String) -> Result<&mut Self::Project, Self::Error>;
}

/// Error type for project operations
#[derive(Debug, Clone, serde::Deserialize, TS)]
#[ts(export)]
pub enum ProjectError {
    NotFound(String),
    AlreadyExists(String),
    InvalidName(String),
    Other(String),
}

impl std::fmt::Display for ProjectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProjectError::NotFound(name) => write!(f, "Project '{}' not found", name),
            ProjectError::AlreadyExists(name) => write!(f, "Project '{}' already exists", name),
            ProjectError::InvalidName(name) => write!(f, "Invalid project name: '{}'", name),
            ProjectError::Other(msg) => write!(f, "Project error: {}", msg),
        }
    }
}

impl std::error::Error for ProjectError {}

// Serde support
impl serde::Serialize for ProjectError {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}
