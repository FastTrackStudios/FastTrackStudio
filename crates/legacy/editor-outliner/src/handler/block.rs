//! Block-level actions: edit content, split at caret, indent /
//! outdent, delete. Mirrors `frontend.handler.block` /
//! `frontend.handler.editor` block-tree operations.

use dioxus::prelude::*;
use uuid::Uuid;

use crate::db::{Block, Vault, new_block};
use crate::handler::persist::{page_for_block, save_page};
use crate::state::AppState;

/// Replace the named block's `content` with `new_content`.
/// Returns silently if the block doesn't exist (lost-update
/// races shouldn't crash the editor).
pub fn update_block_content(state: AppState, block_id: Uuid, new_content: String) {
    let mut v = state.vault;
    {
        let mut guard = v.write();
        if let Some(b) = guard.blocks.iter_mut().find(|b| b.id == block_id) {
            b.content = new_content;
            b.updated_at = chrono::Utc::now();
        }
    }
    if let Some(page_id) = page_for_block(state, block_id) {
        save_page(state, page_id);
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
    let (new_id, page_id) = {
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
        let page_id = target.page_id;
        guard.blocks.push(nb);
        (new_id, page_id)
    };
    save_page(state, page_id);
    Some(new_id)
}

/// Make this block a child of its previous sibling (Logseq's
/// Tab behavior). No-op when there is no previous sibling.
pub fn indent_block(state: AppState, block_id: Uuid) {
    let mut v = state.vault;
    let page_id = {
        let mut guard = v.write();
        match indent_block_pure(&mut guard, block_id) {
            Some(pid) => pid,
            None => return,
        }
    };
    save_page(state, page_id);
}

/// Pure-Vault version of [`indent_block`] — testable without a
/// Dioxus runtime. Returns the affected page id when something
/// actually moved.
pub fn indent_block_pure(vault: &mut crate::db::Vault, block_id: Uuid) -> Option<Uuid> {
    let target = vault.blocks.iter().find(|b| b.id == block_id).cloned()?;
    let mut siblings: Vec<&Block> = vault
        .blocks
        .iter()
        .filter(|b| b.parent_id == target.parent_id && b.page_id == target.page_id)
        .collect();
    siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let pos = siblings.iter().position(|b| b.id == target.id)?;
    if pos == 0 {
        return None;
    }
    let new_parent = siblings[pos - 1].id;
    let last_child_key = vault
        .blocks
        .iter()
        .filter(|b| b.parent_id == Some(new_parent))
        .map(|b| b.sort_key.clone())
        .max();
    let new_sort = match last_child_key {
        Some(k) => format!("{k}m"),
        None => "m".to_string(),
    };
    if let Some(b) = vault.blocks.iter_mut().find(|b| b.id == block_id) {
        b.parent_id = Some(new_parent);
        b.sort_key = new_sort;
        b.updated_at = chrono::Utc::now();
    }
    Some(target.page_id)
}

/// Move this block up one level (Shift-Tab). No-op when the
/// block is already at the root.
pub fn outdent_block(state: AppState, block_id: Uuid) {
    let mut v = state.vault;
    let page_id = {
        let mut guard = v.write();
        match outdent_block_pure(&mut guard, block_id) {
            Some(pid) => pid,
            None => return,
        }
    };
    save_page(state, page_id);
}

/// Pure-Vault outdent — returns the affected page id, None when
/// the block is already at the root.
pub fn outdent_block_pure(vault: &mut crate::db::Vault, block_id: Uuid) -> Option<Uuid> {
    let target = vault.blocks.iter().find(|b| b.id == block_id).cloned()?;
    let parent_id = target.parent_id?;
    let parent = vault.blocks.iter().find(|b| b.id == parent_id).cloned()?;
    let new_sort = format!("{}m", parent.sort_key);
    if let Some(b) = vault.blocks.iter_mut().find(|b| b.id == block_id) {
        b.parent_id = parent.parent_id;
        b.sort_key = new_sort;
        b.updated_at = chrono::Utc::now();
    }
    Some(target.page_id)
}

/// Remove a block. Children are reparented to its parent so we
/// don't orphan a subtree on accidental delete.
pub fn delete_block(state: AppState, block_id: Uuid) {
    let mut v = state.vault;
    let page_id = {
        let mut guard = v.write();
        match delete_block_pure(&mut guard, block_id) {
            Some(pid) => pid,
            None => return,
        }
    };
    save_page(state, page_id);
}

/// Pure-Vault delete — reparents children to the deleted block's
/// parent so we don't orphan a subtree.
pub fn delete_block_pure(vault: &mut crate::db::Vault, block_id: Uuid) -> Option<Uuid> {
    let target = vault.blocks.iter().find(|b| b.id == block_id).cloned()?;
    let new_parent = target.parent_id;
    for b in vault.blocks.iter_mut() {
        if b.parent_id == Some(block_id) {
            b.parent_id = new_parent;
        }
    }
    vault.blocks.retain(|b| b.id != block_id);
    Some(target.page_id)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::db::{Vault, new_block, new_page};

    fn setup() -> (Vault, Vec<Uuid>) {
        // Three sibling blocks on one page: A, B, C.
        let page = new_page("Test");
        let page_id = page.id;
        let a = {
            let mut b = new_block(page_id, None, "A");
            b.sort_key = "a".into();
            b
        };
        let bb = {
            let mut b = new_block(page_id, None, "B");
            b.sort_key = "b".into();
            b
        };
        let c = {
            let mut b = new_block(page_id, None, "C");
            b.sort_key = "c".into();
            b
        };
        let ids = vec![a.id, bb.id, c.id];
        let mut vault = Vault::default();
        vault.pages.push(page);
        vault.blocks.extend([a, bb, c]);
        (vault, ids)
    }

    #[test]
    fn indent_b_under_a() {
        let (mut vault, ids) = setup();
        let page_id = indent_block_pure(&mut vault, ids[1]);
        assert!(page_id.is_some());
        let b = vault.blocks.iter().find(|b| b.id == ids[1]).unwrap();
        assert_eq!(b.parent_id, Some(ids[0]));
        let c = vault.blocks.iter().find(|b| b.id == ids[2]).unwrap();
        assert_eq!(c.parent_id, None);
    }

    #[test]
    fn indent_first_sibling_noop() {
        let (mut vault, ids) = setup();
        let res = indent_block_pure(&mut vault, ids[0]);
        assert!(res.is_none());
        let a = vault.blocks.iter().find(|b| b.id == ids[0]).unwrap();
        assert_eq!(a.parent_id, None);
    }

    #[test]
    fn outdent_round_trip() {
        let (mut vault, ids) = setup();
        indent_block_pure(&mut vault, ids[1]);
        outdent_block_pure(&mut vault, ids[1]);
        let b = vault.blocks.iter().find(|b| b.id == ids[1]).unwrap();
        assert_eq!(b.parent_id, None);
    }

    #[test]
    fn outdent_top_level_noop() {
        let (mut vault, ids) = setup();
        let res = outdent_block_pure(&mut vault, ids[0]);
        assert!(res.is_none());
    }

    #[test]
    fn prev_block_at_top_is_none() {
        let (vault, ids) = setup();
        assert_eq!(prev_block_in_outline(&vault, ids[0]), None);
    }

    #[test]
    fn prev_block_previous_sibling() {
        let (vault, ids) = setup();
        // B's prev is A (no children).
        assert_eq!(prev_block_in_outline(&vault, ids[1]), Some(ids[0]));
    }

    #[test]
    fn prev_block_descends_into_prior_subtree() {
        let (mut vault, ids) = setup();
        // Make B a child of A. Now C's prev should be B
        // (A's last descendant), not A.
        indent_block_pure(&mut vault, ids[1]);
        assert_eq!(prev_block_in_outline(&vault, ids[2]), Some(ids[1]));
    }

    #[test]
    fn prev_block_first_child_is_parent() {
        let (mut vault, ids) = setup();
        // B is A's child; B's prev should be A itself.
        indent_block_pure(&mut vault, ids[1]);
        assert_eq!(prev_block_in_outline(&vault, ids[1]), Some(ids[0]));
    }

    #[test]
    fn delete_reparents_children() {
        let (mut vault, ids) = setup();
        // Make B and C children of A so the test can show
        // them reparenting back to the root on A's delete.
        indent_block_pure(&mut vault, ids[1]);
        indent_block_pure(&mut vault, ids[2]);
        let c = vault.blocks.iter().find(|b| b.id == ids[2]).unwrap();
        assert_eq!(c.parent_id, Some(ids[0]));

        delete_block_pure(&mut vault, ids[0]);
        assert!(vault.blocks.iter().find(|b| b.id == ids[0]).is_none());
        let b = vault.blocks.iter().find(|b| b.id == ids[1]).unwrap();
        let c = vault.blocks.iter().find(|b| b.id == ids[2]).unwrap();
        assert_eq!(b.parent_id, None);
        assert_eq!(c.parent_id, None);
    }
}

/// The block that visually appears immediately above `block_id`
/// in the outline. Mirrors Logseq's
/// `util/get-prev-block-non-collapsed`:
///
/// - If there's a previous sibling, follow it down to its
///   deepest last descendant (visually that's the block right
///   above us).
/// - Otherwise the parent (or `None` if the block is at the top
///   of the page).
pub fn prev_block_in_outline(vault: &Vault, block_id: Uuid) -> Option<Uuid> {
    let target = vault.blocks.iter().find(|b| b.id == block_id)?;
    let mut siblings: Vec<&Block> = vault
        .blocks
        .iter()
        .filter(|b| b.parent_id == target.parent_id && b.page_id == target.page_id)
        .collect();
    siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let pos = siblings.iter().position(|b| b.id == target.id)?;
    if pos > 0 {
        Some(last_descendant(vault, siblings[pos - 1].id))
    } else {
        target.parent_id
    }
}

/// The block that visually appears immediately *below* `block_id`.
/// Mirrors Logseq's `get-next-block-non-collapsed`:
///
/// - If the block has children, the first child.
/// - Else the next sibling.
/// - Else walk up: the first ancestor that has a following
///   sibling — that sibling is the answer.
/// - Else `None` (block is the last in the page).
pub fn next_block_in_outline(vault: &Vault, block_id: Uuid) -> Option<Uuid> {
    // First child?
    let mut children: Vec<&Block> = vault
        .blocks
        .iter()
        .filter(|b| b.parent_id == Some(block_id))
        .collect();
    if !children.is_empty() {
        children.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
        return Some(children[0].id);
    }
    // Walk up looking for a next sibling.
    let mut cursor = block_id;
    loop {
        let cur = vault.blocks.iter().find(|b| b.id == cursor)?;
        let mut siblings: Vec<&Block> = vault
            .blocks
            .iter()
            .filter(|b| b.parent_id == cur.parent_id && b.page_id == cur.page_id)
            .collect();
        siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
        let pos = siblings.iter().position(|b| b.id == cur.id)?;
        if let Some(next) = siblings.get(pos + 1) {
            return Some(next.id);
        }
        cursor = cur.parent_id?;
    }
}

/// Swap this block with its previous sibling (Cmd-Up). No-op when
/// at the top of its sibling group.
pub fn move_block_up(state: AppState, block_id: Uuid) {
    let mut v = state.vault;
    let page_id = {
        let mut guard = v.write();
        match move_block_up_pure(&mut guard, block_id) {
            Some(pid) => pid,
            None => return,
        }
    };
    save_page(state, page_id);
}

/// Swap this block with its next sibling (Cmd-Down). No-op when
/// at the bottom of its sibling group.
pub fn move_block_down(state: AppState, block_id: Uuid) {
    let mut v = state.vault;
    let page_id = {
        let mut guard = v.write();
        match move_block_down_pure(&mut guard, block_id) {
            Some(pid) => pid,
            None => return,
        }
    };
    save_page(state, page_id);
}

pub fn move_block_up_pure(vault: &mut Vault, block_id: Uuid) -> Option<Uuid> {
    let target = vault.blocks.iter().find(|b| b.id == block_id).cloned()?;
    let mut siblings: Vec<Block> = vault
        .blocks
        .iter()
        .filter(|b| b.parent_id == target.parent_id && b.page_id == target.page_id)
        .cloned()
        .collect();
    siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let pos = siblings.iter().position(|b| b.id == target.id)?;
    if pos == 0 {
        return None;
    }
    let prev_key = siblings[pos - 1].sort_key.clone();
    let cur_key = siblings[pos].sort_key.clone();
    let now = chrono::Utc::now();
    for b in vault.blocks.iter_mut() {
        if b.id == target.id {
            b.sort_key = prev_key.clone();
            b.updated_at = now;
        } else if b.id == siblings[pos - 1].id {
            b.sort_key = cur_key.clone();
            b.updated_at = now;
        }
    }
    Some(target.page_id)
}

pub fn move_block_down_pure(vault: &mut Vault, block_id: Uuid) -> Option<Uuid> {
    let target = vault.blocks.iter().find(|b| b.id == block_id).cloned()?;
    let mut siblings: Vec<Block> = vault
        .blocks
        .iter()
        .filter(|b| b.parent_id == target.parent_id && b.page_id == target.page_id)
        .cloned()
        .collect();
    siblings.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let pos = siblings.iter().position(|b| b.id == target.id)?;
    if pos + 1 >= siblings.len() {
        return None;
    }
    let next_key = siblings[pos + 1].sort_key.clone();
    let cur_key = siblings[pos].sort_key.clone();
    let now = chrono::Utc::now();
    for b in vault.blocks.iter_mut() {
        if b.id == target.id {
            b.sort_key = next_key.clone();
            b.updated_at = now;
        } else if b.id == siblings[pos + 1].id {
            b.sort_key = cur_key.clone();
            b.updated_at = now;
        }
    }
    Some(target.page_id)
}

fn last_descendant(vault: &Vault, block_id: Uuid) -> Uuid {
    let mut children: Vec<&Block> = vault
        .blocks
        .iter()
        .filter(|b| b.parent_id == Some(block_id))
        .collect();
    if children.is_empty() {
        return block_id;
    }
    children.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    last_descendant(vault, children.last().unwrap().id)
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
