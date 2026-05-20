//! Edit-mode state transitions. Mirrors a slim subset of
//! Logseq's `frontend.handler.editor` (the part that manages
//! "which block is currently being edited").

use dioxus::prelude::*;
use uuid::Uuid;

use crate::state::AppState;

/// Switch the named block into edit mode. Any other block that
/// was being edited exits — only one is editable at a time.
pub fn enter_edit(state: AppState, block_id: Uuid) {
    state.editing_block.clone().set(Some(block_id));
}

/// Exit edit mode for the active block. No-op when nothing is
/// being edited.
pub fn exit_edit(state: AppState) {
    state.editing_block.clone().set(None);
}
