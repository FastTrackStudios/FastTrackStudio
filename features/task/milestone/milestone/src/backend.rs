//! Server-side [`MilestoneService`] backend. Walks the vault
//! on each call; resolves the owning project's slug when
//! `path` defaults need filling.

use std::path::{Path, PathBuf};

use chrono::Utc;
use uuid::Uuid;
use vault::Vault;

use crate::model::Milestone;
use crate::parse::{looks_like_milestone, parse_page};
use crate::service::{MilestoneError, MilestoneService};
use crate::write::{default_milestone_path, write_milestone};

#[derive(Debug, Clone, architect::HasDispatcher)]
pub struct MilestoneBackend {
    vault_root: PathBuf,
}

impl MilestoneBackend {
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

    fn list_inner(&self) -> Result<Vec<Milestone>, MilestoneError> {
        let vault = Vault::open(&self.vault_root).map_err(|e| {
            MilestoneError::Io(format!("open vault {}: {e}", self.vault_root.display()))
        })?;
        let mut out = Vec::new();
        for page in &vault.pages {
            let proto = page.to_proto();
            if !looks_like_milestone(&proto) {
                continue;
            }
            match parse_page(&proto) {
                Ok(m) => out.push(m),
                Err(e) => tracing::warn!(path = %page.rel_path, ?e, "milestone parse failed"),
            }
        }
        Ok(out)
    }

    /// Resolve the owning project's vault-relative path
    /// (e.g. `Projects/Health/Health.md`). Used by `create`
    /// to derive the default milestone path. Walks the vault
    /// for the one `type: project` page whose frontmatter
    /// `id` matches.
    fn project_path(&self, project_id: Uuid) -> Result<String, MilestoneError> {
        let vault = Vault::open(&self.vault_root).map_err(|e| {
            MilestoneError::Io(format!("open vault {}: {e}", self.vault_root.display()))
        })?;
        for page in &vault.pages {
            let Some((map, _)) = vault_entity::frontmatter::mapping(&page.raw) else {
                continue;
            };
            if vault_entity::yaml::str_at(&map, "type").as_deref() != Some("project") {
                continue;
            }
            let id_match = vault_entity::yaml::str_at(&map, "id")
                .and_then(|s| Uuid::parse_str(&s).ok())
                == Some(project_id);
            if id_match {
                return Ok(page.rel_path.clone());
            }
        }
        Err(MilestoneError::BadRequest(format!(
            "no project with id {project_id} in this vault"
        )))
    }
}

impl MilestoneService for MilestoneBackend {
    fn list(&self) -> Result<Vec<Milestone>, MilestoneError> {
        self.list_inner()
    }

    fn get(&self, id: Uuid) -> Result<Milestone, MilestoneError> {
        self.list_inner()?
            .into_iter()
            .find(|m| m.id == id)
            .ok_or_else(|| MilestoneError::NotFound(id.to_string()))
    }

    fn get_by_path(&self, path: &str) -> Result<Milestone, MilestoneError> {
        self.list_inner()?
            .into_iter()
            .find(|m| m.path == path)
            .ok_or_else(|| MilestoneError::NotFound(path.to_owned()))
    }

    fn create(&self, mut m: Milestone) -> Result<Milestone, MilestoneError> {
        if m.title.trim().is_empty() {
            return Err(MilestoneError::BadRequest("title is required".into()));
        }
        if m.project_id.is_nil() {
            return Err(MilestoneError::BadRequest("project_id is required".into()));
        }
        if m.id.is_nil() {
            m.id = Uuid::new_v4();
        }
        if m.path.is_empty() {
            let project_rel = self.project_path(m.project_id)?;
            m.path = default_milestone_path(&project_rel, &m.title);
        }
        let abs = self.vault_root.join(&m.path);
        if abs.exists() {
            return Err(MilestoneError::AlreadyExists(m.path.clone()));
        }
        write_milestone(&self.vault_root, &mut m, false)
            .map_err(|e| MilestoneError::Io(format!("write: {e}")))?;
        Ok(m)
    }

    fn update(&self, milestone: Milestone) -> Result<Milestone, MilestoneError> {
        let existing = self
            .list_inner()?
            .into_iter()
            .find(|m| m.id == milestone.id)
            .ok_or_else(|| MilestoneError::NotFound(milestone.id.to_string()))?;
        let mut next = milestone;
        next.path = existing.path;
        next.date_created = existing.date_created.or(next.date_created);
        next.date_modified = Some(Utc::now());
        write_milestone(&self.vault_root, &mut next, true)
            .map_err(|e| MilestoneError::Io(format!("write: {e}")))?;
        Ok(next)
    }

    fn rename(&self, id: Uuid, new_path: &str) -> Result<Milestone, MilestoneError> {
        if new_path.is_empty() || new_path.contains("..") || new_path.starts_with('/') {
            return Err(MilestoneError::BadRequest(format!("bad path: {new_path}")));
        }
        let mut m = self
            .list_inner()?
            .into_iter()
            .find(|m| m.id == id)
            .ok_or_else(|| MilestoneError::NotFound(id.to_string()))?;
        let from = self.vault_root.join(&m.path);
        let to = self.vault_root.join(new_path);
        if to.exists() {
            return Err(MilestoneError::AlreadyExists(new_path.to_owned()));
        }
        if let Some(parent) = to.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| MilestoneError::Io(format!("mkdir: {e}")))?;
        }
        std::fs::rename(&from, &to).map_err(|e| MilestoneError::Io(format!("rename: {e}")))?;
        m.path = new_path.to_owned();
        m.date_modified = Some(Utc::now());
        write_milestone(&self.vault_root, &mut m, true)
            .map_err(|e| MilestoneError::Io(format!("write: {e}")))?;
        Ok(m)
    }

    fn delete(&self, id: Uuid) -> Result<(), MilestoneError> {
        let m = self
            .list_inner()?
            .into_iter()
            .find(|m| m.id == id)
            .ok_or_else(|| MilestoneError::NotFound(id.to_string()))?;
        // Refuse if any task points at this milestone — drop
        // the link first, otherwise downstream rollups would
        // silently float to the project root.
        let vault = Vault::open(&self.vault_root)
            .map_err(|e| MilestoneError::Io(format!("open vault: {e}")))?;
        for page in &vault.pages {
            let Some((map, _)) = vault_entity::frontmatter::mapping(&page.raw) else {
                continue;
            };
            if vault_entity::yaml::str_at(&map, "type").as_deref() != Some("task") {
                continue;
            }
            let linked = vault_entity::yaml::str_at(&map, "milestoneId")
                .and_then(|s| Uuid::parse_str(&s).ok())
                == Some(id);
            if linked {
                return Err(MilestoneError::BadRequest(format!(
                    "{} is referenced by task {}; unlink first",
                    m.title, page.rel_path
                )));
            }
        }
        let abs = self.vault_root.join(&m.path);
        std::fs::remove_file(&abs)
            .map_err(|e| MilestoneError::Io(format!("remove {}: {e}", abs.display())))?;
        Ok(())
    }
}
