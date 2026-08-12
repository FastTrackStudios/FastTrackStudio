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

/// One Vault reference into a root's store, as
/// [`VaultVersions::protect_refs`] read it — the page it came from
/// (so a failure names the file a human has to fix) and the commit it
/// claims.
#[derive(Debug, Clone)]
pub struct VersionEntityRef {
    pub page: String,
    pub commit_id: String,
}

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

    /// A store over a freshly scanned vault, for a *read*. A vault
    /// that isn't on disk yet (a brand-new org writes nothing until
    /// something asks it to) reads as empty rather than erroring, and
    /// nothing is created — creating directories is a write's job.
    fn read_store<E: VaultEntity>(&self) -> Result<VaultEntityStore<E>> {
        if !self.vault_root.exists() {
            let empty = Vault::from_entries(&self.vault_root, Vec::new())
                .map_err(|e| Error::Repo(format!("empty vault snapshot: {e}")))?;
            return Ok(VaultEntityStore::new(empty));
        }
        self.write_store()
    }

    /// A store over a freshly scanned vault, for a *write*: the vault
    /// root is created if missing, and a vault that can't be read is
    /// an error rather than an empty list.
    fn write_store<E: VaultEntity>(&self) -> Result<VaultEntityStore<E>> {
        std::fs::create_dir_all(&self.vault_root)?;
        let vault = Vault::open(&self.vault_root)
            .map_err(|e| Error::Repo(format!("open vault {}: {e}", self.vault_root.display())))?;
        Ok(VaultEntityStore::new(vault))
    }

    /// Every version entity of `root_id`, from **one** vault scan — the
    /// input `gc_root`'s protect set is built from, and the one read
    /// where a page must never be silently skipped.
    ///
    /// The ordinary list paths above go through `VaultEntityStore::scan`,
    /// which logs and drops a page it can't parse. That is right for a
    /// listing (one broken page must not blank the UI) and catastrophic
    /// for GC: ADR 0001 makes the Vault "the authority on immortality",
    /// so a reference the Vault holds but this process failed to read
    /// must stop the sweep rather than quietly forfeit that content's
    /// protection.
    ///
    /// **But only for pages that are identifiably this root's.** The
    /// strict half is scoped to `Files/<root-slug>/`, where this crate
    /// writes them. Everywhere else, a page that fails to parse is
    /// logged and skipped, because "matches" is a loose test: the
    /// shared `VaultEntity::matches` accepts `type:` *or* a `tags:`
    /// entry, so an ordinary note a user tagged `files-named-version`
    /// is claimed by this walk, and a page missing `rootId` belongs to
    /// no root at all. Without the scope, one such page anywhere in the
    /// org vault would wedge GC for *every* root — the exact blast
    /// radius the log-and-skip arm downstream exists to prevent.
    pub fn protect_refs(&self, root_id: Uuid, root_name: &str) -> Result<Vec<VersionEntityRef>> {
        let store = self.read_store::<NamedVersions>()?;
        let owned_prefix = format!(
            "{FILES_FOLDER}/{}/",
            vault_entity::slugify(root_name, "root")
        );
        store.with_vault(|vault| {
            let mut out = Vec::new();
            for page in &vault.pages {
                let parsed = if NamedVersions::matches(page) {
                    NamedVersions::from_page(page).map(|v| (v.root_id, v.commit_id))
                } else if ProjectVersions::matches(page) {
                    ProjectVersions::from_page(page).map(|v| (v.root_id, v.commit_id))
                } else {
                    continue;
                };
                let (page_root_id, commit_id) = match parsed {
                    Ok(v) => v,
                    Err(e) if page.rel_path.starts_with(&owned_prefix) => {
                        return Err(strict_parse_err(page, e));
                    }
                    Err(e) => {
                        tracing::warn!(
                            page = %page.rel_path,
                            ?e,
                            "a page claiming a Files version type is unreadable; it is not in \
                             this root's folder, so it cannot be one of its references"
                        );
                        continue;
                    }
                };
                if page_root_id == root_id {
                    out.push(VersionEntityRef {
                        page: page.rel_path.clone(),
                        commit_id,
                    });
                }
            }
            Ok(out)
        })
    }

    // ── Named Versions ────────────────────────────────────────────

    /// Every Named Version, newest first; `root_id` filters to one
    /// root.
    pub fn named_versions(&self, root_id: Option<Uuid>) -> Result<Vec<NamedVersion>> {
        let mut list = self.read_store::<NamedVersions>()?.list();
        if let Some(root_id) = root_id {
            list.retain(|v| v.root_id == root_id);
        }
        list.sort_by(|a, b| b.created_at.cmp(&a.created_at).then(a.name.cmp(&b.name)));
        Ok(list)
    }

    pub fn named_version(&self, id: Uuid) -> Result<NamedVersion> {
        self.read_store::<NamedVersions>()?
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
        // One snapshot for both the uniqueness check and the path
        // choice: two scans would leave a window where the name was
        // free in the first and taken in the second.
        let store = self.write_store::<NamedVersions>()?;
        let taken = store
            .list()
            .into_iter()
            .any(|v| v.root_id == root_id && v.name == name);
        if taken {
            return Err(Error::AlreadyExists(format!(
                "root {root_id} already has a Named Version called {name:?}"
            )));
        }
        let folder = root_folder(root_name, NAMED_SUBFOLDER);
        let path = store.with_vault(|vault| {
            self.unique_path(vault, &folder, &vault_entity::slugify(&name, "version"))
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
        self.write_store::<NamedVersions>()?
            .delete(&id.to_string())
            .map_err(entity_err)
    }

    // ── Project Versions ──────────────────────────────────────────

    /// Every Project Version in the vault, any root — ONE scan, for
    /// callers that need the lineage of a whole list of roots
    /// (`list_roots`' badge overlay, issue #266) rather than of one.
    pub fn all_project_versions(&self) -> Result<Vec<ProjectVersion>> {
        Ok(self.read_store::<ProjectVersions>()?.list())
    }

    /// Every Project Version of `root_id`, oldest number first.
    pub fn project_versions(&self, root_id: Uuid) -> Result<Vec<ProjectVersion>> {
        let mut list = self.read_store::<ProjectVersions>()?.list();
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
        // `ProjectVersions::from_page` tolerates a missing `commitId`
        // (a page written by hand mid-edit still has to load), so the
        // writer is where that shape gets refused — otherwise this
        // crate would itself produce references that name nothing.
        if commit_id.trim().is_empty() {
            return Err(Error::BadRequest(
                "a Project Version must reference a commit".into(),
            ));
        }
        // One snapshot for both the numbering and the path (see
        // `create_named_version`).
        let store = self.write_store::<ProjectVersions>()?;
        let number = store
            .list()
            .into_iter()
            .filter(|v| v.root_id == root_id)
            .map(|v| v.number)
            .max()
            .unwrap_or(0)
            + 1;
        let folder = root_folder(root_name, PROJECT_SUBFOLDER);
        let stem = match &label {
            Some(label) => format!("v{number}-{}", vault_entity::slugify(label, "iteration")),
            None => format!("v{number}"),
        };
        let path = store.with_vault(|vault| self.unique_path(vault, &folder, &stem));
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

    /// `<folder>/<stem>.md`, suffixed `-2`, `-3`, … past a page that is
    /// already there. Two roots whose names slugify the same, or a page
    /// a human wrote by hand, must never make a write fail — and the
    /// on-disk check matters as much as the snapshot one: `create_page`
    /// only consults the in-memory page list, so a file another writer
    /// created since this snapshot would otherwise be overwritten.
    fn unique_path(&self, vault: &Vault, folder: &str, stem: &str) -> String {
        let taken = |candidate: &str| {
            vault.pages.iter().any(|p| p.rel_path == candidate)
                || self.vault_root.join(candidate).exists()
        };
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

fn strict_parse_err(page: &vault::VaultPage, e: vault_entity::ParseError) -> Error {
    Error::BadRequest(format!(
        "{}: unreadable Files version page ({e}) — refusing to compute a GC protect set that \
         might silently forfeit the version it references; fix or remove the page",
        page.rel_path
    ))
}

fn entity_err(e: vault_entity::EntityError) -> Error {
    match e {
        vault_entity::EntityError::NotFound(m) => Error::NotFound(m),
        vault_entity::EntityError::AlreadyExists(m) => Error::AlreadyExists(m),
        vault_entity::EntityError::BadRequest(m) => Error::BadRequest(m),
        vault_entity::EntityError::Io(m) => Error::Repo(format!("vault: {m}")),
    }
}
