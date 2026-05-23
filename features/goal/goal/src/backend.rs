//! Server-side [`GoalService`] backend — walks the vault for
//! `type: goal` pages.
//!
//! Cheap to `Clone`. Each call re-opens the vault; future
//! optimization: cache with mtime check (mirror cookbook /
//! project backend).

use std::path::{Path, PathBuf};

use architect::HasDispatcher;
use architect::dispatch::TokioBlockingDispatcher;
use uuid::Uuid;
use vault::Vault;

use crate::model::Goal;
use crate::parse::{looks_like_goal, parse_page};
use crate::service::{GoalError, GoalService};

#[derive(Debug, Clone)]
pub struct GoalBackend {
    vault_root: PathBuf,
}

impl GoalBackend {
    #[must_use]
    pub fn new(vault_root: impl Into<PathBuf>) -> Self {
        Self {
            vault_root: vault_root.into(),
        }
    }

    #[must_use]
    pub fn vault_root(&self) -> &Path {
        &self.vault_root
    }

    fn list_inner(&self) -> Result<Vec<Goal>, GoalError> {
        let vault = Vault::open(&self.vault_root)
            .map_err(|e| GoalError::Io(format!("open vault {}: {e}", self.vault_root.display())))?;
        let mut out = Vec::new();
        for page in &vault.pages {
            let proto = page.to_proto();
            if !looks_like_goal(&proto) {
                continue;
            }
            match parse_page(&proto) {
                Ok(g) => out.push(g),
                Err(e) => tracing::warn!(path = %page.rel_path, ?e, "goal parse failed"),
            }
        }
        Ok(out)
    }
}

impl HasDispatcher for GoalBackend {
    type Dispatcher = TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        TokioBlockingDispatcher
    }
}

impl GoalService for GoalBackend {
    fn list(&self) -> Result<Vec<Goal>, GoalError> {
        self.list_inner()
    }

    fn get(&self, id: Uuid) -> Result<Goal, GoalError> {
        self.list_inner()?
            .into_iter()
            .find(|g| g.id == id)
            .ok_or_else(|| GoalError::NotFound(id.to_string()))
    }

    fn get_by_path(&self, path: &str) -> Result<Goal, GoalError> {
        self.list_inner()?
            .into_iter()
            .find(|g| g.path == path)
            .ok_or_else(|| GoalError::NotFound(path.to_owned()))
    }
}
