//! In-memory page + block store. Mirrors Logseq's
//! `frontend.db.*` namespaces.
//!
//! The store is a pure data layer — no Dioxus, no async — so it's
//! easy to test. The `handler` module mutates this through
//! `AppState.vault` and writes the mutation back to disk via
//! `format::write_page`.

mod block;
mod file;
mod page;

pub use block::{Block, new_block};
pub use file::{load_vault, write_page, write_vault};
pub use page::{Page, new_page};

use std::collections::HashMap;
use uuid::Uuid;

/// Whole vault — pages + their blocks. Source of truth on disk;
/// this struct is just an in-memory cache rebuilt from
/// `~/Task/{pages,journals}/*.md` on launch and on watcher events.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Vault {
    pub pages: Vec<Page>,
    pub blocks: Vec<Block>,
}

impl Vault {
    pub fn page_by_id(&self, id: Uuid) -> Option<&Page> {
        self.pages.iter().find(|p| p.id == id)
    }

    pub fn page_by_basename(&self, basename: &str) -> Option<&Page> {
        self.pages
            .iter()
            .find(|p| p.basename.eq_ignore_ascii_case(basename))
    }

    /// Top-level blocks of a page, sorted by sort_key.
    pub fn root_blocks(&self, page_id: Uuid) -> Vec<&Block> {
        let mut bs: Vec<&Block> = self
            .blocks
            .iter()
            .filter(|b| b.page_id == page_id && b.parent_id.is_none())
            .collect();
        bs.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
        bs
    }

    /// Direct children of a block, sorted.
    pub fn children(&self, parent: Uuid) -> Vec<&Block> {
        let mut bs: Vec<&Block> = self
            .blocks
            .iter()
            .filter(|b| b.parent_id == Some(parent))
            .collect();
        bs.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
        bs
    }

    /// Build a block-by-id lookup. Useful for handlers that touch
    /// arbitrary blocks by uuid.
    pub fn block_index(&self) -> HashMap<Uuid, usize> {
        self.blocks
            .iter()
            .enumerate()
            .map(|(i, b)| (b.id, i))
            .collect()
    }
}
