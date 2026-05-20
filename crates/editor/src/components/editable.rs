//! Block-edit textarea. Controlled component — `value` is bound
//! to the in-memory block content; oninput pushes every keystroke
//! straight through `update_block_content`. No CRDT, no async
//! eval — the Dioxus controlled-textarea pattern is the whole
//! story. Mirrors `frontend.components.editor.box`.

use dioxus::prelude::*;
use uuid::Uuid;

use crate::handler::{
    delete_block, exit_edit, indent_block, outdent_block, split_block, update_block_content,
};
use crate::state::AppState;

#[component]
pub fn EditableBlock(block_id: Uuid) -> Element {
    let state = use_context::<AppState>();
    let vault = state.vault.read();
    let value = vault
        .blocks
        .iter()
        .find(|b| b.id == block_id)
        .map(|b| b.content.clone())
        .unwrap_or_default();
    drop(vault);

    let rows = value.lines().count().max(1).min(40) as i64;
    let on_input = move |e: Event<FormData>| {
        update_block_content(state, block_id, e.value());
    };
    let on_blur = move |_: Event<FocusData>| {
        exit_edit(state);
    };
    let on_keydown = move |e: Event<KeyboardData>| {
        let key = e.key();
        let mods = e.modifiers();
        match key {
            // Enter: split the block at the caret. v1 splits at
            // end-of-content; a caret-aware version follows once
            // we wire a selectionchange listener.
            Key::Enter if !mods.shift() => {
                e.prevent_default();
                let v = state
                    .vault
                    .read()
                    .blocks
                    .iter()
                    .find(|b| b.id == block_id)
                    .map(|b| b.content.clone())
                    .unwrap_or_default();
                let len = v.len();
                if let Some(new_id) = split_block(state, block_id, len, &v) {
                    state.editing_block.clone().set(Some(new_id));
                }
            }
            // Tab: indent the block — make it a child of its
            // previous sibling. Shift-Tab: outdent — move one
            // level closer to root.
            Key::Tab => {
                e.prevent_default();
                if mods.shift() {
                    outdent_block(state, block_id);
                } else {
                    indent_block(state, block_id);
                }
            }
            // Backspace on an empty block deletes it and exits
            // edit mode. Logseq's same rule.
            Key::Backspace => {
                let is_empty = state
                    .vault
                    .read()
                    .blocks
                    .iter()
                    .find(|b| b.id == block_id)
                    .map(|b| b.content.is_empty())
                    .unwrap_or(false);
                if is_empty {
                    e.prevent_default();
                    delete_block(state, block_id);
                    exit_edit(state);
                }
            }
            Key::Escape => {
                e.prevent_default();
                exit_edit(state);
            }
            _ => {}
        }
    };
    let on_mount = move |elem: Event<MountedData>| {
        spawn(async move {
            let _ = elem.data().set_focus(true).await;
        });
    };

    rsx! {
        textarea {
            class: "editor-textarea",
            rows: "{rows}",
            value: "{value}",
            oninput: on_input,
            onkeydown: on_keydown,
            onblur: on_blur,
            onmounted: on_mount,
        }
    }
}
