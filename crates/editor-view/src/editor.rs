//! The `<Editor>` Dioxus component. v0 scaffold — renders the
//! current document's text into a `<textarea>` and syncs typing
//! back via `oninput`.
//!
//! **Why textarea and not contenteditable in v0?** Dioxus's
//! `Event<FormData>::value()` reads the `.value` DOM property,
//! which exists on `<textarea>` / `<input>` but **not** on a
//! contenteditable element — that text lives in `.textContent`.
//! Using `evt.value()` on contenteditable returns `""` on every
//! keystroke, so the doc immediately collapses to empty (we hit
//! this; the logs are diagnostic). Textarea avoids the issue
//! entirely and proves the State → render → input → Transaction
//! loop with no JS bridge. Contenteditable comes back in v1
//! when we add decoration rendering — at that point we need
//! `document::eval` to read `textContent` AND careful caret
//! preservation across re-renders, both worthwhile but out of
//! scope for v0.
//!
//! What's intentionally missing in v0 (each its own future commit):
//!
//! - Selection round-trip (DOM Selection ↔ Rust `Selection`).
//!   For now selection lives in the browser; we only mirror text.
//! - Decoration rendering (Mark / Replace / Widget / Line).
//!   This is what forces the contenteditable swap.
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
        let new_len = new_text.len();
        tracing::debug!(
            old_len,
            new_len,
            delta = new_len as isize - old_len as isize,
            "editor.input"
        );
        let changes = Changes::replace(0..old_len, new_text);
        state.set(old.update(TransactionSpec::new().changes(changes)));
    };

    // `rows` grows to fit content so multi-line input doesn't
    // hide rows. Capped so a huge paste doesn't take over the
    // viewport. Newline-count + 1 because `str::lines()` ignores
    // a trailing newline (the Shift-Enter-just-pressed case).
    let rows = (text.bytes().filter(|c| *c == b'\n').count() + 1).clamp(4, 40) as i64;

    rsx! {
        textarea {
            class: "editor-root",
            spellcheck: "false",
            rows: "{rows}",
            value: "{text}",
            oninput: on_input,
        }
    }
}
