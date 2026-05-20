//! Block-level actions: edit content, split at caret, indent /
//! outdent, delete. Mirrors `frontend.handler.block` /
//! `frontend.handler.editor` block-tree operations.

use dioxus::prelude::*;
use uuid::Uuid;

use crate::db::{Block, new_block};
use crate::state::AppState;

/// Replace the named block's `content` with `new_content`.
/// Returns silently if the block doesn't exist (lost-update
/// races shouldn't crash the editor).
pub fn update_block_content(state: AppState, block_id: Uuid, new_content: String) {
    let mut v = state.vault;
    let mut guard = v.write();
    if let Some(b) = guard.blocks.iter_mut().find(|b| b.id == block_id) {
        b.content = new_content;
        b.updated_at = chrono::Utc::now();
    }
}

/// Split a block at the given character offset. The original
/// block keeps the prefix; a new sibling block is created
/// immediately after it with the suffix as its content. Returns
/// the new block's id so the caller can switch edit-mode to it.
pub fn split_block(state: AppState, block_id: Uuid, offset: usize, text: &str) -> Option<Uuid> {
    let off = offset.min(text.len());
    let (left, right) = text.split_at(off);
    let mut v = state.vault;
    let mut guard = v.write();
    let target = guard.blocks.iter().find(|b| b.id == block_id).cloned()?;
    let new_sort = next_sort_key(&guard.blocks, target.parent_id, &target.sort_key);
    if let Some(b) = guard.blocks.iter_mut().find(|b| b.id == block_id) {
        b.content = left.to_string();
        b.updated_at = chrono::Utc::now();
    }
    let mut nb = new_block(target.page_id, target.parent_id, right.to_string());
    nb.sort_key = new_sort;
    let new_id = nb.id;
    guard.blocks.push(nb);
    Some(new_id)
}

/// Make this block a child of its previous sibling (Logseq's
/// Tab behavior). No-op when there is no previous sibling.
pub fn indent_block(state: AppState, block_id: Uuid) {
    let mut v = state.vault;
    let mut guard = v.write();
    let target = match guard.blocks.iter().find(|b| b.id == block_id).cloned() {
        Some(t) => t,
        None => return,
    };
    let mut siblings: Vec<&Block> = guard
        .blocks
        .iter()
        .filter(|b| b.parent_id == target.parent_id && b.page_id == target.page_id)
        .collect();
    siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let pos = siblings.iter().position(|b| b.id == target.id);
    let Some(pos) = pos else {
        return;
    };
    if pos == 0 {
        return;
    }
    let new_parent = siblings[pos - 1].id;
    let last_child_key = guard
        .blocks
        .iter()
        .filter(|b| b.parent_id == Some(new_parent))
        .map(|b| b.sort_key.clone())
        .max();
    let new_sort = match last_child_key {
        Some(k) => format!("{k}m"),
        None => "m".to_string(),
    };
    if let Some(b) = guard.blocks.iter_mut().find(|b| b.id == block_id) {
        b.parent_id = Some(new_parent);
        b.sort_key = new_sort;
        b.updated_at = chrono::Utc::now();
    }
}

/// Move this block up one level (Shift-Tab). No-op when the
/// block is already at the root.
pub fn outdent_block(state: AppState, block_id: Uuid) {
    let mut v = state.vault;
    let mut guard = v.write();
    let target = match guard.blocks.iter().find(|b| b.id == block_id).cloned() {
        Some(t) => t,
        None => return,
    };
    let Some(parent_id) = target.parent_id else {
        return;
    };
    let parent = match guard.blocks.iter().find(|b| b.id == parent_id).cloned() {
        Some(p) => p,
        None => return,
    };
    let new_sort = format!("{}m", parent.sort_key);
    if let Some(b) = guard.blocks.iter_mut().find(|b| b.id == block_id) {
        b.parent_id = parent.parent_id;
        b.sort_key = new_sort;
        b.updated_at = chrono::Utc::now();
    }
}

/// Remove a block. Children are reparented to its parent so we
/// don't orphan a subtree on accidental delete.
pub fn delete_block(state: AppState, block_id: Uuid) {
    let mut v = state.vault;
    let mut guard = v.write();
    let target = match guard.blocks.iter().find(|b| b.id == block_id).cloned() {
        Some(t) => t,
        None => return,
    };
    let new_parent = target.parent_id;
    for b in guard.blocks.iter_mut() {
        if b.parent_id == Some(block_id) {
            b.parent_id = new_parent;
        }
    }
    guard.blocks.retain(|b| b.id != block_id);
}

/// Pick a sort_key strictly between `after_key` and the next
/// sibling's key (or just past it when at the tail).
fn next_sort_key(blocks: &[Block], parent: Option<Uuid>, after_key: &str) -> String {
    let mut siblings: Vec<&Block> = blocks.iter().filter(|b| b.parent_id == parent).collect();
    siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let next = siblings
        .iter()
        .skip_while(|b| b.sort_key.as_str() <= after_key)
        .next()
        .map(|b| b.sort_key.clone());
    match next {
        Some(n) if n > after_key.to_string() + "m" => format!("{after_key}m"),
        Some(_) => format!("{after_key}m"),
        None => format!("{after_key}m"),
    }
}
