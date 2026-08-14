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
                    let Ok(dir) = confined_dir(&vault, &[home]) else {
                        continue;
                    };
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
                    // plus the virtual Media/ door to its root. A
                    // physical `Media` dir must not double the entry
                    // (duplicate names would also collide as Dioxus
                    // keys client-side).
                    None => {
                        let mut entries =
                            Self::list_dir(&confined_dir(&project_dir, &[])?, true, true)?;
                        if media_root.is_some() && !entries.iter().any(|e| e.name == "Media") {
                            entries.push(virtual_dir("Media"));
                            entries.sort_by(|a, b| a.name.cmp(&b.name));
                        }
                        Ok(TreeNode::Listing(entries))
                    }
                    // `Projects/<name>/Media[/…]` — the root's live
                    // tree when one is registered (the physical dir,
                    // if any, is shadowed by the handoff); a plain
                    // vault dir otherwise.
                    Some((&"Media", media_rest)) => match media_root {
                        Some(root) => Ok(TreeNode::Root {
                            id: root,
                            subpath: media_rest.join("/"),
                        }),
                        None => dir_node(&project_dir, rest),
                    },
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
        let dir = confined_dir(&base, rest)?;
        // Hide registered roots at EVERY depth — a root created in a
        // subdirectory surfaces through Projects/, never as loose
        // files. Two guards: the registry's canonical paths, and the
        // on-disk root marker (catches a root whose registered path
        // spelling differs from the canonical one).
        let root_dirs: Vec<PathBuf> = self
            .registry_list()
            .into_iter()
            .filter_map(|r| PathBuf::from(r.path).canonicalize().ok())
            .collect();
        let mut entries = Self::list_dir(&dir, true, true)?;
        entries.retain(|e| {
            if !e.is_dir {
                return true;
            }
            let full = dir.join(&e.name);
            if full.join(crate::consts::MARKER_FILE).exists() {
                return false;
            }
            match full.canonicalize() {
                Ok(canonical) => !root_dirs.iter().any(|r| r == &canonical),
                Err(_) => true,
            }
        });
        Ok(TreeNode::Listing(entries))
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
    Ok(TreeNode::Listing(FilesBackend::list_dir(
        &confined_dir(base, rest)?,
        true,
        true,
    )?))
}

/// Resolve `rest` under `base` with REAL confinement: canonicalize
/// both and require the target to stay inside the base. The literal
/// `..` scan upstream catches lazy escapes; this catches symlinks —
/// a link inside the vault pointing at `~/.ssh` (synced content, a
/// shared volume) must not hand its listing to every org member.
/// Every sibling browse surface confines; the tree is no exception.
fn confined_dir(base: &Path, rest: &[&str]) -> Result<PathBuf, Error> {
    let canonical_base = base
        .canonicalize()
        .map_err(|e| Error::NotFound(format!("{}: {e}", base.display())))?;
    let mut dir = canonical_base.clone();
    for segment in rest {
        dir.push(segment);
    }
    let resolved = dir
        .canonicalize()
        .map_err(|_| Error::NotFound(format!("{}: not a directory", rest.join("/"))))?;
    if !resolved.starts_with(&canonical_base) {
        return Err(Error::BadRequest(format!(
            "{}: path escapes the area",
            rest.join("/")
        )));
    }
    if !resolved.is_dir() {
        return Err(Error::NotFound(format!(
            "{}: not a directory",
            rest.join("/")
        )));
    }
    Ok(resolved)
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
