//! Vault-side curation for Files: reading and writing the
//! [`NamedVersion`] / [`ProjectVersion`] entities that reference a
//! root's commits (issue #261).
//!
//! **Live scan, no index.** Every call opens the org vault fresh
//! (`Vault::open`) and scans it, exactly like `WorkstreamBackend` and
//! the other vault-backed slices — there is no second authority to
//! keep in sync, and an entity that arrived by vault replication (or a
//! producer's own text editor) is visible on the very next call. That
//! is what "replicate with the Vault and re-resolve on any device"
//! means concretely: these are ordinary vault files, and nothing
//! caches them.
//!
//! The generic CRUD comes from `vault-entity`'s
//! [`VaultEntityStore`]; the per-root path layout, the per-root name
//! uniqueness check, and Project Version numbering are what live here.

use std::path::{Path, PathBuf};

use chrono::Utc;
use files_proto::{NamedVersion, ProjectVersion};
use uuid::Uuid;
use vault::Vault;
use vault_entity::store::{VaultEntity, VaultEntityStore};

use crate::entity::{
    FILES_FOLDER, NAMED_SUBFOLDER, NamedVersions, PROJECT_SUBFOLDER, ProjectVersions,
};
use crate::error::{Error, Result};

/// The org vault holding Files' curated version entities.
#[derive(Debug, Clone)]
pub struct VaultVersions {
    vault_root: PathBuf,
}

impl VaultVersions {
    pub fn new(vault_root: impl Into<PathBuf>) -> Self {
        Self {
            vault_root: vault_root.into(),
        }
    }

    pub fn vault_root(&self) -> &Path {
        &self.vault_root
    }

    /// A store over a freshly scanned vault. The directory is created
    /// on demand: a brand-new org has no vault on disk until something
    /// writes into it, and listing versions must be an empty list
    /// rather than an error there.
    fn store<E: VaultEntity>(&self) -> Result<VaultEntityStore<E>> {
        std::fs::create_dir_all(&self.vault_root)?;
        let vault = Vault::open(&self.vault_root)
            .map_err(|e| Error::Repo(format!("open vault {}: {e}", self.vault_root.display())))?;
        Ok(VaultEntityStore::new(vault))
    }

    // ── Named Versions ────────────────────────────────────────────

    /// Every Named Version, newest first; `root_id` filters to one
    /// root.
    pub fn named_versions(&self, root_id: Option<Uuid>) -> Result<Vec<NamedVersion>> {
        let mut list = self.store::<NamedVersions>()?.list();
        if let Some(root_id) = root_id {
            list.retain(|v| v.root_id == root_id);
        }
        list.sort_by(|a, b| b.created_at.cmp(&a.created_at).then(a.name.cmp(&b.name)));
        Ok(list)
    }

    pub fn named_version(&self, id: Uuid) -> Result<NamedVersion> {
        self.store::<NamedVersions>()?
            .get_by_uuid(id)
            .ok_or_else(|| Error::NotFound(format!("named version {id}")))
    }

    /// Write a new Named Version page. `root_name` only shapes the
    /// page's folder; the reference itself is `(root_id, change_id)`.
    pub fn create_named_version(
        &self,
        root_id: Uuid,
        root_name: &str,
        name: String,
        change_id: String,
        commit_id: String,
    ) -> Result<NamedVersion> {
        let existing = self.named_versions(Some(root_id))?;
        if existing.iter().any(|v| v.name == name) {
            return Err(Error::AlreadyExists(format!(
                "root {root_id} already has a Named Version called {name:?}"
            )));
        }
        let store = self.store::<NamedVersions>()?;
        let folder = root_folder(root_name, NAMED_SUBFOLDER);
        let path = store.with_vault(|vault| {
            unique_path(vault, &folder, &vault_entity::slugify(&name, "version"))
        });
        let model = NamedVersion {
            id: Uuid::new_v4(),
            path,
            name,
            root_id,
            change_id,
            commit_id,
            note: String::new(),
            created_at: Utc::now(),
        };
        store.create(model).map_err(entity_err)
    }

    pub fn delete_named_version(&self, id: Uuid) -> Result<()> {
        self.store::<NamedVersions>()?
            .delete(&id.to_string())
            .map_err(entity_err)
    }

    // ── Project Versions ──────────────────────────────────────────

    /// Every Project Version of `root_id`, oldest number first.
    pub fn project_versions(&self, root_id: Uuid) -> Result<Vec<ProjectVersion>> {
        let mut list = self.store::<ProjectVersions>()?.list();
        list.retain(|v| v.root_id == root_id);
        list.sort_by_key(|v| v.number);
        Ok(list)
    }

    /// Write the next Project Version of `root_id`. Numbering is
    /// `max(existing) + 1` over the *scanned* pages, so a version that
    /// arrived by vault replication is counted too; the first one is
    /// v1.
    pub fn create_project_version(
        &self,
        root_id: Uuid,
        root_name: &str,
        label: Option<String>,
        change_id: String,
        commit_id: String,
    ) -> Result<ProjectVersion> {
        let number = self
            .project_versions(root_id)?
            .iter()
            .map(|v| v.number)
            .max()
            .unwrap_or(0)
            + 1;
        let store = self.store::<ProjectVersions>()?;
        let folder = root_folder(root_name, PROJECT_SUBFOLDER);
        let stem = match &label {
            Some(label) => format!("v{number}-{}", vault_entity::slugify(label, "iteration")),
            None => format!("v{number}"),
        };
        let path = store.with_vault(|vault| unique_path(vault, &folder, &stem));
        let model = ProjectVersion {
            id: Uuid::new_v4(),
            path,
            root_id,
            number,
            label,
            change_id,
            commit_id,
            started_at: Utc::now(),
        };
        store.create(model).map_err(entity_err)
    }
}

/// `Files/<root-slug>/<sub>` — every version entity of one root lives
/// together, so a producer browsing the vault sees a project's
/// curation in one place and two roots can share a version name.
fn root_folder(root_name: &str, sub: &str) -> String {
    format!(
        "{FILES_FOLDER}/{}/{sub}",
        vault_entity::slugify(root_name, "root")
    )
}

/// `<folder>/<stem>.md`, suffixed `-2`, `-3`, … past a page that is
/// already there. Two roots whose names slugify the same, or a page a
/// human wrote by hand, must never make a write fail.
fn unique_path(vault: &Vault, folder: &str, stem: &str) -> String {
    let taken = |candidate: &str| vault.pages.iter().any(|p| p.rel_path == candidate);
    let first = format!("{folder}/{stem}.md");
    if !taken(&first) {
        return first;
    }
    for n in 2..1000 {
        let candidate = format!("{folder}/{stem}-{n}.md");
        if !taken(&candidate) {
            return candidate;
        }
    }
    format!("{folder}/{stem}-{}.md", Uuid::new_v4())
}

fn entity_err(e: vault_entity::EntityError) -> Error {
    match e {
        vault_entity::EntityError::NotFound(m) => Error::NotFound(m),
        vault_entity::EntityError::AlreadyExists(m) => Error::AlreadyExists(m),
        vault_entity::EntityError::BadRequest(m) => Error::BadRequest(m),
        vault_entity::EntityError::Io(m) => Error::Repo(format!("vault: {m}")),
    }
}
