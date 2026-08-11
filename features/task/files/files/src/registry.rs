//! The server's own index of known File Roots, persisted as JSON beside
//! its version-store repos (`<data_dir>/roots.json`). Together with the
//! marker file each root carries in its own live tree
//! ([`crate::backend::MARKER_FILE`]), this is the "entity" half of
//! ADR 0001 / the glossary's "File Root — a first-class vault entity":
//! a full Vault-entity integration (frontmatter note, sync) is future
//! work past this ticket's RPC-surface scope, but identity already
//! survives a restart through this file plus the marker.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use files_proto::FileRootInfo;
use uuid::Uuid;

use crate::error::Result;

#[derive(Debug)]
pub struct Registry {
    path: PathBuf,
    roots: Mutex<HashMap<Uuid, FileRootInfo>>,
}

impl Registry {
    pub fn open(data_dir: &Path) -> Result<Self> {
        std::fs::create_dir_all(data_dir)?;
        let path = data_dir.join("roots.json");
        let roots = if path.exists() {
            let bytes = std::fs::read(&path)?;
            let list: Vec<FileRootInfo> = serde_json::from_slice(&bytes)?;
            list.into_iter().map(|r| (r.id, r)).collect()
        } else {
            HashMap::new()
        };
        Ok(Self {
            path,
            roots: Mutex::new(roots),
        })
    }

    fn persist(&self, roots: &HashMap<Uuid, FileRootInfo>) -> Result<()> {
        let mut list: Vec<&FileRootInfo> = roots.values().collect();
        list.sort_by(|a, b| a.id.cmp(&b.id));
        let bytes = serde_json::to_vec_pretty(&list)?;
        std::fs::write(&self.path, bytes)?;
        Ok(())
    }

    pub fn insert(&self, root: FileRootInfo) -> Result<()> {
        let mut roots = self.roots.lock().expect("registry lock poisoned");
        roots.insert(root.id, root);
        self.persist(&roots)
    }

    pub fn get(&self, id: Uuid) -> Option<FileRootInfo> {
        self.roots
            .lock()
            .expect("registry lock poisoned")
            .get(&id)
            .cloned()
    }

    pub fn list(&self) -> Vec<FileRootInfo> {
        let mut v: Vec<_> = self
            .roots
            .lock()
            .expect("registry lock poisoned")
            .values()
            .cloned()
            .collect();
        v.sort_by(|a, b| a.name.cmp(&b.name));
        v
    }

    /// A registered root that would conflict with a new root at
    /// `path` — exact match, an existing root nested inside `path`, or
    /// `path` nested inside an existing root, checked both directions
    /// since containment is symmetric-in-effect for versioning:
    /// "roots never overlap on disk" (glossary "File Root"). The
    /// marker-file check on disk covers the exact-match case for a
    /// root this registry hasn't seen yet; this covers the ancestor/
    /// descendant cases, which no marker file can detect.
    pub fn conflicting_root(&self, path: &Path) -> Option<FileRootInfo> {
        self.roots
            .lock()
            .expect("registry lock poisoned")
            .values()
            .find(|r| {
                let existing = Path::new(&r.path);
                path == existing || path.starts_with(existing) || existing.starts_with(path)
            })
            .cloned()
    }
}
