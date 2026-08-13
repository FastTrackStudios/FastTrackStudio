//! The org tree resolver (issue #304): ONE unified namespace over an
//! org's Projects / Vault / Wiki / Assets, consumed identically by
//! the explorer RPC and (wave 2) the WebDAV mount — the app and a
//! mounted network share always show the same tree.
//!
//! Area semantics:
//! - `Projects/` — a JOIN: every vault project folder (`Projects/*`
//!   and `Albums/*`), with a virtual `Media/` entry when a File Root
//!   is registered to the project (by name, or `Album — <name>`).
//!   Descending into `Media/` resolves to [`TreeNode::Root`] — the
//!   client mounts the full root explorer there.
//! - `Vault/`, `Wiki/` — the physical directory tree, straight
//!   through (no extra lens level; surfacing tags into the tree is a
//!   later exploration).
//! - `Assets/` — the org's loose files: the Files area with the
//!   registered root directories filtered out.

use std::path::{Path, PathBuf};

use files_proto::{BrowseEntry, TreeNode};

use crate::backend::FilesBackend;
use crate::error::Error;

/// The four top-level areas, in display order.
const AREAS: [&str; 4] = ["Projects", "Vault", "Wiki", "Assets"];

impl FilesBackend {
    pub(crate) fn tree_browse_inner(&self, path: String) -> Result<TreeNode, Error> {
        let segments: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
        // Confinement first: the tree serves virtual paths, but every
        // segment still walks real directories underneath.
        if segments.iter().any(|s| *s == "." || *s == "..") {
            return Err(Error::BadRequest(format!("{path}: path escapes")));
        }

        match segments.split_first() {
            None => Ok(TreeNode::Listing(
                AREAS.iter().map(|a| virtual_dir(a)).collect(),
            )),
            Some((&"Projects", rest)) => self.projects_area(rest),
            Some((&"Vault", rest)) => markdown_area(&self.vault_root_dir(), rest),
            Some((&"Wiki", rest)) => markdown_area(&self.wiki_root_dir(), rest),
            Some((&"Assets", rest)) => self.assets_area(rest),
            Some((other, _)) => Err(Error::NotFound(format!("{other}: no such area"))),
        }
    }

    /// The org's vault directory (the versions store knows it).
    fn vault_root_dir(&self) -> PathBuf {
        self.vault_root().to_path_buf()
    }

    /// The org's wiki directory — a sibling of the vault under the
    /// org dir (the server roots the wiki slice there too).
    fn wiki_root_dir(&self) -> PathBuf {
        self.vault_root()
            .parent()
            .map(|org| org.join("wiki"))
            .unwrap_or_else(|| self.vault_root().join("wiki"))
    }

    // ── Projects: the vault ⋈ roots join ─────────────────────────

    fn projects_area(&self, rest: &[&str]) -> Result<TreeNode, Error> {
        let vault = self.vault_root_dir();
        match rest.split_first() {
            // `Projects/` — every project folder, both homes.
            None => {
                let mut entries = Vec::new();
                for home in ["Projects", "Albums"] {
                    let dir = vault.join(home);
                    if !dir.is_dir() {
                        continue;
                    }
                    for entry in std::fs::read_dir(&dir)? {
                        let entry = entry?;
                        if entry.file_type()?.is_dir() {
                            entries.push(virtual_dir(&entry.file_name().to_string_lossy()));
                        }
                    }
                }
                entries.sort_by(|a, b| a.name.cmp(&b.name));
                Ok(TreeNode::Listing(entries))
            }
            Some((project, rest)) => {
                let Some(home) = ["Projects", "Albums"]
                    .into_iter()
                    .find(|h| vault.join(h).join(project).is_dir())
                else {
                    return Err(Error::NotFound(format!("{project}: no such project")));
                };
                let project_dir = vault.join(home).join(project);
                let media_root = self.project_media_root(project);

                match rest.split_first() {
                    // `Projects/<name>/` — the project's own notes
                    // plus the virtual Media/ door to its root.
                    None => {
                        let mut entries = Self::list_dir(&project_dir, true, true)?;
                        if media_root.is_some() {
                            entries.push(virtual_dir("Media"));
                            entries.sort_by(|a, b| a.name.cmp(&b.name));
                        }
                        Ok(TreeNode::Listing(entries))
                    }
                    // `Projects/<name>/Media[/…]` — the root's live
                    // tree; the client takes over with the full root
                    // explorer.
                    Some((&"Media", media_rest)) => {
                        let root = media_root.ok_or_else(|| {
                            Error::NotFound(format!("{project}: no File Root registered"))
                        })?;
                        Ok(TreeNode::Root {
                            id: root,
                            subpath: media_rest.join("/"),
                        })
                    }
                    // `Projects/<name>/<notes…>` — plain vault dirs.
                    Some(_) => dir_node(&project_dir, rest),
                }
            }
        }
    }

    /// The root registered to a project, matched by name — the exact
    /// project name, or the album spelling (`Album — <name>`).
    fn project_media_root(&self, project: &str) -> Option<uuid::Uuid> {
        let album_name = format!("Album — {project}");
        self.registry_list()
            .into_iter()
            .find(|r| r.name == project || r.name == album_name)
            .map(|r| r.id)
    }

    // ── Assets: loose files (the Files area minus root dirs) ─────

    fn assets_area(&self, rest: &[&str]) -> Result<TreeNode, Error> {
        let base = self.confine_root().to_path_buf();
        if rest.is_empty() {
            // Top level: hide the registered roots' own directories —
            // they surface through Projects/, not as loose files.
            let root_dirs: Vec<PathBuf> = self
                .registry_list()
                .into_iter()
                .map(|r| PathBuf::from(r.path))
                .collect();
            let mut entries = Self::list_dir(&base, true, true)?;
            entries.retain(|e| {
                let full = base.join(&e.name);
                !root_dirs.iter().any(|r| r == &full)
            });
            return Ok(TreeNode::Listing(entries));
        }
        dir_node(&base, rest)
    }
}

// ── markdown areas (Vault, Wiki) ──────────────────────────────────

/// The physical directory tree, straight through. A missing area dir
/// (an org that never grew a wiki) is an empty listing, not an error.
fn markdown_area(base: &Path, rest: &[&str]) -> Result<TreeNode, Error> {
    if !base.is_dir() {
        if rest.is_empty() {
            return Ok(TreeNode::Listing(Vec::new()));
        }
        return Err(Error::NotFound(format!(
            "{}: not a directory",
            rest.join("/")
        )));
    }
    dir_node(base, rest)
}

/// A physical directory listing under `base`, `rest` segments deep.
fn dir_node(base: &Path, rest: &[&str]) -> Result<TreeNode, Error> {
    let mut dir = base.to_path_buf();
    for segment in rest {
        dir.push(segment);
    }
    if !dir.is_dir() {
        return Err(Error::NotFound(format!(
            "{}: not a directory",
            rest.join("/")
        )));
    }
    Ok(TreeNode::Listing(FilesBackend::list_dir(&dir, true, true)?))
}

// ── entry constructors ────────────────────────────────────────────

fn virtual_dir(name: &str) -> BrowseEntry {
    BrowseEntry {
        name: name.to_string(),
        is_dir: true,
        size: None,
        stub: false,
        divergent: false,
    }
}
