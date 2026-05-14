//! Phase 4 — basename index for wiki-link → user resolution.
//!
//! Pages in `vault://org` carry a page title. When an ACL entry
//! says `[[Cody]]`, the access check needs to look up which auth
//! user that page represents. Convention (formalized fully in
//! Phase 5): a "person" page in the org vault carries
//! `auth_user_id` in its frontmatter; the basename index maps
//! the page title to that uuid.
//!
//! Phase 4 ships the trait + an in-memory impl. Phase 5 will wire
//! it to Knowledge vault updates so the index rebuilds whenever a
//! person-page is created/renamed.

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use uuid::Uuid;

/// Resolver from wiki-link basename (e.g. "Cody") to an auth user.
pub trait BasenameIndex: Send + Sync + 'static {
    fn resolve(&self, basename: &str) -> Option<Uuid>;
}

/// In-memory impl used by tests + as the Phase-4 default. Phase 5
/// replaces with a Loro-backed version that observes vault changes.
#[derive(Debug, Default, Clone)]
pub struct MemoryBasenameIndex {
    inner: Arc<RwLock<HashMap<String, Uuid>>>,
}

impl MemoryBasenameIndex {
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert / overwrite. Title is compared case-sensitively to
    /// match Obsidian + Logseq behavior.
    pub fn upsert(&self, basename: impl Into<String>, user_id: Uuid) {
        self.inner.write().unwrap().insert(basename.into(), user_id);
    }

    pub fn remove(&self, basename: &str) {
        self.inner.write().unwrap().remove(basename);
    }

    /// Drop every entry. The indexer (Phase 5b) calls this before a
    /// full rebuild so removed person-pages don't keep ghost entries.
    pub fn clear(&self) {
        self.inner.write().unwrap().clear();
    }

    pub fn len(&self) -> usize {
        self.inner.read().unwrap().len()
    }

    pub fn is_empty(&self) -> bool {
        self.inner.read().unwrap().is_empty()
    }
}

impl BasenameIndex for MemoryBasenameIndex {
    fn resolve(&self, basename: &str) -> Option<Uuid> {
        self.inner.read().unwrap().get(basename).copied()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn upsert_resolve_remove() {
        let idx = MemoryBasenameIndex::new();
        let alice = Uuid::new_v4();
        let bob = Uuid::new_v4();
        idx.upsert("Alice", alice);
        idx.upsert("Bob", bob);
        assert_eq!(idx.resolve("Alice"), Some(alice));
        assert_eq!(idx.resolve("Bob"), Some(bob));
        assert_eq!(idx.resolve("Eve"), None);
        // Case-sensitive.
        assert_eq!(idx.resolve("alice"), None);

        idx.remove("Alice");
        assert_eq!(idx.resolve("Alice"), None);
        assert_eq!(idx.len(), 1);
    }
}
