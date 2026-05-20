//! Block-edit textarea. Controlled component — `value` is bound
//! to the in-memory block content; oninput pushes every keystroke
//! straight through `update_block_content`. No CRDT, no async
//! eval — the Dioxus controlled-textarea pattern is the whole
//! story. Mirrors `frontend.components.editor.box`.

use dioxus::prelude::*;
use uuid::Uuid;

use crate::handler::{exit_edit, split_block, update_block_content};
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
        // Enter splits the block at the caret.
        if matches!(e.key(), Key::Enter) && !e.modifiers().shift() {
            e.prevent_default();
            // We don't have the caret offset here without an
            // eval round-trip; for v1 we split at end-of-line
            // (= end of value). Refinement: stash caret on a
            // selectionchange listener and read it sync.
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
        if matches!(e.key(), Key::Escape) {
            e.prevent_default();
            exit_edit(state);
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
