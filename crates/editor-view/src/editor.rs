//! The `<Editor>` Dioxus component. v0 scaffold — renders the
//! current document's text into a `contenteditable` div and
//! syncs typing back via `oninput`.
//!
//! What's intentionally missing in v0 (lands in subsequent
//! commits, each its own well-scoped change):
//!
//! - Selection round-trip (DOM Selection ↔ Rust `Selection`).
//!   For now selection lives in the browser; we only mirror text.
//! - Decoration rendering (Mark / Replace / Widget / Line).
//! - Keymap dispatch (we let the browser handle keys for now).
//! - History / undo.
//!
//! Today this component is the smallest thing that proves the
//! `EditorState → render → input event → Transaction → new
//! EditorState` loop works end-to-end.

use dioxus::prelude::*;
use editor_state::{Changes, EditorState, TransactionSpec};

/// Props: a Signal carrying the current `EditorState`. The
/// component reads it for rendering and writes a new state when
/// the user types.
///
/// Owning the state via Signal (rather than internally) means
/// the embedding app can: persist it, send transactions from
/// remote peers, snapshot for undo, etc. — the editor is just a
/// view.
#[component]
pub fn Editor(state: Signal<EditorState>) -> Element {
    let text = state.read().doc.to_string();

    let on_input = move |evt: Event<FormData>| {
        // v0: full-text replacement. The DOM hands us the new
        // string; we diff naively by creating a Changes that
        // wipes the old text and inserts the new. Later commits
        // will compute a minimal diff (or read InputEvent
        // ranges) so undo and CRDT ops are well-shaped.
        let new_text = evt.value();
        let old = state.read().clone();
        let old_len = old.doc.len();
        let changes = Changes::replace(0..old_len, new_text);
        state.set(old.update(TransactionSpec::new().changes(changes)));
    };

    rsx! {
        div {
            class: "editor-root",
            contenteditable: "true",
            spellcheck: "false",
            oninput: on_input,
            "{text}"
        }
    }
}
