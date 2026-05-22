//! `git-config` — persisted bindings between Task entities and
//! forge entities.
//!
//! - [`RepoBinding`] — `project_id` <-> `RepoId`. One project
//!   may bind to N repos; one repo to N projects.
//! - [`IssueLink`] — `task_id` <-> `(RepoId, number, kind)`.
//!   One task may link to N issues/PRs.
//!
//! First pass ships types + an in-memory [`Store`] so the rest
//! of the feature can build and integrate. `SQLite` persistence
//! lands next (matching `email-config`'s store).

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use facet::Facet;
use git_proto::RepoId;
use serde::{Deserialize, Serialize};

/// Whether an `IssueLink` points at an issue or a pull request.
/// Forge-level numbers may share a namespace (GitHub) or not
/// (Forgejo) — record the kind so lookups know which trait to
/// route to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Facet, Serialize, Deserialize)]
#[repr(u8)]
pub enum LinkKind {
    Issue,
    Pull,
}

/// One project <-> one forge repo. Many of these per project
/// and per repo are allowed.
#[derive(Debug, Clone, Facet, Serialize, Deserialize)]
pub struct RepoBinding {
    /// Task-side identifier — currently the project markdown
    /// page's stable id. Opaque to this crate.
    pub project_id: String,
    pub repo: RepoId,
}

/// One task <-> one issue or pull request.
#[derive(Debug, Clone, Facet, Serialize, Deserialize)]
pub struct IssueLink {
    pub task_id: String,
    pub repo: RepoId,
    pub number: u64,
    pub kind: LinkKind,
}

#[derive(Debug, thiserror::Error)]
pub enum ConfigError {
    #[error("storage error: {0}")]
    Storage(String),
}

/// Persisted binding store. First-pass impl is in-memory; the
/// trait surface is what callers depend on, so swapping in a
/// SQLite-backed implementation later is mechanical.
pub trait BindingStore: Send + Sync {
    fn add_repo_binding(&self, binding: RepoBinding) -> Result<(), ConfigError>;
    fn remove_repo_binding(&self, project_id: &str, repo: &RepoId) -> Result<(), ConfigError>;
    fn repos_for_project(&self, project_id: &str) -> Result<Vec<RepoId>, ConfigError>;
    fn projects_for_repo(&self, repo: &RepoId) -> Result<Vec<String>, ConfigError>;

    fn add_issue_link(&self, link: IssueLink) -> Result<(), ConfigError>;
    fn remove_issue_link(&self, task_id: &str, number: u64) -> Result<(), ConfigError>;
    fn issues_for_task(&self, task_id: &str) -> Result<Vec<IssueLink>, ConfigError>;
    fn tasks_for_issue(&self, repo: &RepoId, number: u64) -> Result<Vec<String>, ConfigError>;
}

/// In-memory store. Cheap to `Clone`; all state is `Arc`'d.
#[derive(Clone, Default)]
pub struct MemoryStore {
    inner: Arc<Mutex<MemoryInner>>,
}

#[derive(Default)]
struct MemoryInner {
    repo_bindings: Vec<RepoBinding>,
    issue_links: Vec<IssueLink>,
    /// Indices kept as `HashMap` so reads don't scan vectors.
    /// Tiny crate's invariant: the indices are rebuilt
    /// alongside every write.
    by_project: HashMap<String, Vec<usize>>,
    by_task: HashMap<String, Vec<usize>>,
}

impl MemoryStore {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

impl BindingStore for MemoryStore {
    fn add_repo_binding(&self, binding: RepoBinding) -> Result<(), ConfigError> {
        let mut inner = self.inner.lock().unwrap();
        let idx = inner.repo_bindings.len();
        inner
            .by_project
            .entry(binding.project_id.clone())
            .or_default()
            .push(idx);
        inner.repo_bindings.push(binding);
        Ok(())
    }

    fn remove_repo_binding(&self, project_id: &str, repo: &RepoId) -> Result<(), ConfigError> {
        let mut inner = self.inner.lock().unwrap();
        inner
            .repo_bindings
            .retain(|b| !(b.project_id == project_id && &b.repo == repo));
        // Reindex — vectors are small enough that rebuilding
        // from scratch is fine until we move to SQLite.
        let MemoryInner {
            repo_bindings,
            by_project,
            ..
        } = &mut *inner;
        by_project.clear();
        for (i, b) in repo_bindings.iter().enumerate() {
            by_project.entry(b.project_id.clone()).or_default().push(i);
        }
        Ok(())
    }

    fn repos_for_project(&self, project_id: &str) -> Result<Vec<RepoId>, ConfigError> {
        let inner = self.inner.lock().unwrap();
        Ok(inner
            .by_project
            .get(project_id)
            .map(|idxs| {
                idxs.iter()
                    .map(|i| inner.repo_bindings[*i].repo.clone())
                    .collect()
            })
            .unwrap_or_default())
    }

    fn projects_for_repo(&self, repo: &RepoId) -> Result<Vec<String>, ConfigError> {
        let inner = self.inner.lock().unwrap();
        Ok(inner
            .repo_bindings
            .iter()
            .filter(|b| &b.repo == repo)
            .map(|b| b.project_id.clone())
            .collect())
    }

    fn add_issue_link(&self, link: IssueLink) -> Result<(), ConfigError> {
        let mut inner = self.inner.lock().unwrap();
        let idx = inner.issue_links.len();
        inner
            .by_task
            .entry(link.task_id.clone())
            .or_default()
            .push(idx);
        inner.issue_links.push(link);
        Ok(())
    }

    fn remove_issue_link(&self, task_id: &str, number: u64) -> Result<(), ConfigError> {
        let mut inner = self.inner.lock().unwrap();
        inner
            .issue_links
            .retain(|l| !(l.task_id == task_id && l.number == number));
        let MemoryInner {
            issue_links,
            by_task,
            ..
        } = &mut *inner;
        by_task.clear();
        for (i, l) in issue_links.iter().enumerate() {
            by_task.entry(l.task_id.clone()).or_default().push(i);
        }
        Ok(())
    }

    fn issues_for_task(&self, task_id: &str) -> Result<Vec<IssueLink>, ConfigError> {
        let inner = self.inner.lock().unwrap();
        Ok(inner
            .by_task
            .get(task_id)
            .map(|idxs| idxs.iter().map(|i| inner.issue_links[*i].clone()).collect())
            .unwrap_or_default())
    }

    fn tasks_for_issue(&self, repo: &RepoId, number: u64) -> Result<Vec<String>, ConfigError> {
        let inner = self.inner.lock().unwrap();
        Ok(inner
            .issue_links
            .iter()
            .filter(|l| &l.repo == repo && l.number == number)
            .map(|l| l.task_id.clone())
            .collect())
    }
}
