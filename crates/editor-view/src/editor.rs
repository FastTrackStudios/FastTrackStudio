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

use std::sync::atomic::{AtomicU64, Ordering};

use dioxus::prelude::*;
use editor_state::{Changes, EditorState, Range, Selection, TransactionSpec};

/// Per-instance id allocator. Each `<Editor>` mounts with a
/// unique `data-editor-id`, used by the JS bridge to find the
/// textarea this component owns (multiple editors per page is
/// fine — they get distinct ids).
static EDITOR_INSTANCE: AtomicU64 = AtomicU64::new(0);

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
    let editor_id = use_hook(|| {
        let n = EDITOR_INSTANCE.fetch_add(1, Ordering::Relaxed);
        format!("editor-{n}")
    });

    // Selection round-trip. Once on mount we install JS event
    // listeners on the textarea that push `[selectionStart,
    // selectionEnd]` back to Rust via `dioxus.send` whenever the
    // caret moves (typing, arrow keys, click, focus, drag-select).
    // The recv loop turns each into a selection-only
    // transaction. `dioxus-send` keeps the channel open across
    // many messages — perfect for a long-lived stream.
    {
        let id = editor_id.clone();
        use_hook(move || {
            spawn(async move {
                let script = format!(
                    r#"
                    (function() {{
                        function attach() {{
                            const ta = document.querySelector('[data-editor-id="{id}"]');
                            if (!ta) {{ setTimeout(attach, 30); return; }}
                            const send = () => dioxus.send([
                                ta.selectionStart || 0,
                                ta.selectionEnd   || 0,
                            ]);
                            ta.addEventListener('input',     send);
                            ta.addEventListener('keyup',     send);
                            ta.addEventListener('mouseup',   send);
                            ta.addEventListener('focus',     send);
                            ta.addEventListener('select',    send);
                            send(); // initial fire
                        }}
                        attach();
                    }})();
                    "#
                );
                let mut handle = document::eval(&script);
                while let Ok(v) = handle.recv::<serde_json::Value>().await {
                    let Some(arr) = v.as_array() else { continue };
                    if arr.len() != 2 {
                        continue;
                    }
                    let s = arr[0].as_u64().unwrap_or(0) as usize;
                    let e = arr[1].as_u64().unwrap_or(0) as usize;
                    let cur = state.read().clone();
                    let cur_primary = cur.selection.primary();
                    if cur_primary.anchor == s && cur_primary.head == e {
                        // No-op selection update — skip the
                        // transaction so we don't churn signals
                        // on every keystroke (input fires twice
                        // — once from oninput, once from the JS
                        // listener).
                        continue;
                    }
                    tracing::trace!(
                        old_anchor = cur_primary.anchor,
                        old_head = cur_primary.head,
                        new_start = s,
                        new_end = e,
                        "editor.selection"
                    );
                    let new_sel = Selection::single(Range::new(s, e));
                    state.set(cur.update(TransactionSpec::new().selection(new_sel)));
                }
            });
        });
    }

    // DOM ← state: when something *other* than the user (a
    // command, undo, remote CRDT op) changes selection, push the
    // new range to the textarea so the caret matches. We skip
    // pure-typing changes by comparing against the last-seen DOM
    // selection — `setSelectionRange` on an already-correct
    // range is a no-op but the round-trip is still wasted work.
    //
    // Effect runs on every render where the selection signal
    // changed. The DOM read happens via JS; if it matches, we
    // bail. If it doesn't, we write.
    {
        let id = editor_id.clone();
        use_effect(move || {
            let p = state.read().selection.primary();
            let (s, e) = (p.from(), p.to());
            let script = format!(
                r#"
                (function() {{
                    const ta = document.querySelector('[data-editor-id="{id}"]');
                    if (!ta) return;
                    if (ta.selectionStart === {s} && ta.selectionEnd === {e}) return;
                    try {{ ta.setSelectionRange({s}, {e}); }} catch (_) {{}}
                }})();
                "#
            );
            let _ = document::eval(&script);
        });
    }

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
            "data-editor-id": "{editor_id}",
            spellcheck: "false",
            rows: "{rows}",
            value: "{text}",
            oninput: on_input,
        }
    }
}
