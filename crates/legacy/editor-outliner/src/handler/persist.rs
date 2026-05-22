//! Persistence layer — every mutation flushes the affected page
//! back to its `.md` file. Logseq batches writes via a debounced
//! save loop; for now we do a synchronous write per mutation,
//! which is fine because file I/O is fast for a few KB of text
//! and avoids any "did my edit persist?" ambiguity.

use dioxus::prelude::*;
use uuid::Uuid;

use crate::db::{Block, Page, write_page};
use crate::state::AppState;

/// Save the page that owns `page_id` to disk. No-op when the
/// vault has no root path (wasm builds, unconfigured installs).
pub fn save_page(state: AppState, page_id: Uuid) {
    let root = match state.vault_root.peek().clone() {
        Some(r) => r,
        None => return,
    };
    let vault = state.vault.read();
    let Some(page) = vault.pages.iter().find(|p| p.id == page_id).cloned() else {
        return;
    };
    let blocks: Vec<Block> = vault
        .blocks
        .iter()
        .filter(|b| b.page_id == page_id)
        .cloned()
        .collect();
    drop(vault);
    if let Err(e) = write_page(&root, &page, &blocks) {
        tracing::warn!(?e, ?page_id, "save_page failed");
    }
}

/// Look up the page id for a block. Useful from handlers that
/// only know about a block + need to flush its containing page.
pub fn page_for_block(state: AppState, block_id: Uuid) -> Option<Uuid> {
    state
        .vault
        .read()
        .blocks
        .iter()
        .find(|b| b.id == block_id)
        .map(|b| b.page_id)
}

/// Variant of `Page`/`Block` import for callers that already have
/// the values — kept here to mirror Logseq's `frontend.handler.file`.
#[allow(dead_code)]
pub fn write_explicit(state: AppState, page: &Page, blocks: &[Block]) {
    let Some(root) = state.vault_root.peek().clone() else {
        return;
    };
    if let Err(e) = write_page(&root, page, blocks) {
        tracing::warn!(?e, page = ?page.id, "write_explicit failed");
    }
}
