//! Project registry — aggregates multiple providers into a unified view.
//!
//! The registry is the single entry point for the UI and service layer.
//! It queries all registered providers and merges results.

use std::sync::Arc;

use super::traits::*;
use crate::project::Project;
use crate::service::VaultError;
use crate::task::Task;

/// Aggregates multiple `ProjectProvider`s into a unified project/task view.
///
/// ```rust,ignore
/// let mut registry = ProjectRegistry::new();
/// registry.add(LocalProvider::new("starcommand", "Star Command", "/mnt/starcommand/Projects"));
/// registry.add(VaultProvider::new("personal", "Personal", "~/Documents/The Observatory/TaskNotes"));
///
/// // Queries all providers
/// let all_projects = registry.list_all().await?;
/// ```
pub struct ProjectRegistry {
    providers: Vec<Arc<dyn ProjectProvider>>,
}

impl ProjectRegistry {
    pub fn new() -> Self {
        Self {
            providers: Vec::new(),
        }
    }

    /// Register a provider.
    pub fn add(&mut self, provider: impl ProjectProvider + 'static) {
        self.providers.push(Arc::new(provider));
    }

    /// Register a provider wrapped in Arc (for sharing).
    pub fn add_arc(&mut self, provider: Arc<dyn ProjectProvider>) {
        self.providers.push(provider);
    }

    /// Get all registered providers.
    pub fn providers(&self) -> &[Arc<dyn ProjectProvider>] {
        &self.providers
    }

    /// Get a provider by name.
    pub fn get(&self, name: &str) -> Option<&Arc<dyn ProjectProvider>> {
        self.providers.iter().find(|p| p.info().name == name)
    }

    /// List all provider infos.
    pub fn list_providers(&self) -> Vec<ProviderInfo> {
        self.providers.iter().map(|p| p.info().clone()).collect()
    }

    // ── Aggregated queries ───────────────────────────────────────────

    /// List all projects from all providers.
    pub async fn list_projects(&self) -> Result<Vec<Project>, VaultError> {
        let mut all = Vec::new();
        for provider in &self.providers {
            match provider.list_projects().await {
                Ok(projects) => all.extend(projects),
                Err(e) => {
                    tracing::warn!(
                        provider = %provider.info().name,
                        error = %e,
                        "Failed to list projects"
                    );
                }
            }
        }
        Ok(all)
    }

    /// Load all projects with tasks from all providers.
    pub async fn list_all(&self) -> Result<Vec<ProjectBundle>, VaultError> {
        let mut all = Vec::new();
        for provider in &self.providers {
            match provider.list_all().await {
                Ok(bundles) => all.extend(bundles),
                Err(e) => {
                    tracing::warn!(
                        provider = %provider.info().name,
                        error = %e,
                        "Failed to list project bundles"
                    );
                }
            }
        }
        Ok(all)
    }

    /// List all tasks from all providers.
    pub async fn list_tasks(&self) -> Result<Vec<Task>, VaultError> {
        let mut all = Vec::new();
        for provider in &self.providers {
            match provider.list_tasks().await {
                Ok(tasks) => all.extend(tasks),
                Err(e) => {
                    tracing::warn!(
                        provider = %provider.info().name,
                        error = %e,
                        "Failed to list tasks"
                    );
                }
            }
        }
        Ok(all)
    }

    /// Find a project across all providers.
    pub async fn get_project(&self, title: &str) -> Result<Option<ProjectBundle>, VaultError> {
        for provider in &self.providers {
            if let Ok(Some(bundle)) = provider.get_project(title).await {
                return Ok(Some(bundle));
            }
        }
        Ok(None)
    }

    /// Save a task — routes to the correct provider based on project location.
    pub async fn save_task(&self, project_title: &str, task: &Task) -> Result<(), VaultError> {
        // Find which provider owns this project
        for provider in &self.providers {
            if let Ok(Some(_)) = provider.get_project(project_title).await {
                return provider.save_task(project_title, task).await;
            }
        }
        Err(VaultError::NotFound(format!(
            "No provider has project '{}'",
            project_title
        )))
    }

    /// Create a project on a specific provider.
    pub async fn create_project(
        &self,
        provider_name: &str,
        project: &Project,
    ) -> Result<String, VaultError> {
        let provider = self.get(provider_name).ok_or_else(|| {
            VaultError::NotFound(format!("Provider '{}' not found", provider_name))
        })?;
        provider.create_project(project).await
    }

    /// Get tasks filtered by assignee across all providers.
    pub async fn tasks_for_user(&self, username: &str) -> Result<Vec<Task>, VaultError> {
        let all_tasks = self.list_tasks().await?;
        Ok(all_tasks
            .into_iter()
            .filter(|t| t.assignee.as_deref() == Some(username))
            .collect())
    }

    /// Get tasks grouped by provider source.
    pub async fn tasks_by_source(&self) -> Result<Vec<(ProviderInfo, Vec<Task>)>, VaultError> {
        let mut result = Vec::new();
        for provider in &self.providers {
            let info = provider.info().clone();
            let tasks = provider.list_tasks().await.unwrap_or_default();
            result.push((info, tasks));
        }
        Ok(result)
    }
}

impl Default for ProjectRegistry {
    fn default() -> Self {
        Self::new()
    }
}
