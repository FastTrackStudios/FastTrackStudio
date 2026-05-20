//! The `<Editor>` Dioxus component.
//!
//! v1 architecture: a `<div contenteditable="plaintext-only">`
//! whose text content is bound to `state.doc`. Phase A here is
//! plain text only (a single rendered text child). Phase B adds
//! decoration rendering as per-segment spans.
//!
//! ## Why contenteditable now (and how the cursor stays put)
//!
//! Contenteditable lets us render styled text (decorations) and
//! still get a real caret. Textarea can't show inline styles.
//!
//! The trick to not eating the cursor on every re-render is:
//! **render the same text Dioxus already has in the DOM**.
//! Typing flow:
//!
//!   1. user presses 'a' → browser updates DOM textContent to "...a"
//!   2. our JS bridge reads the new textContent, computes a diff
//!      against the old `state.doc`, and applies a Transaction
//!   3. Dioxus re-renders. The text child is `"{text}"` where
//!      text == the new doc, which equals what the DOM already
//!      contains. Dioxus's reconciler sees text node value
//!      unchanged → emits no DOM mutation → caret untouched.
//!
//! For programmatic edits (a command, undo, remote CRDT op) the
//! state diverges from the DOM. Dioxus updates the text node;
//! the browser parks the caret at offset 0. A `use_effect`
//! restores it from `state.selection` — but only when the DOM
//! text already matches state.doc (preventing the writeback
//! from fighting in-flight typing).

// Component props derive PartialEq on a `fn`-pointer field
// (DecorationSource); within a single binary fn-ptr equality is
// reliable enough for prop-diff purposes. The lint guards
// against codegen-unit splits that don't happen in our build.
#![allow(unpredictable_function_pointer_comparisons)]

use std::sync::atomic::{AtomicU64, Ordering};

use dioxus::prelude::*;
use editor_state::{
    Changes, DecoratedRange, EditorState, KeySpec, Keymap, Range, Selection, TransactionSpec,
};

use crate::render::{self, Segment};

/// Decoration source — a pure fn that produces decorations for
/// the current state. Multiple sources can be combined; the view
/// concatenates and sorts before rendering.
///
/// Conceptually mirrors CM6's `EditorView.decorations` facet —
/// extensions contribute decorations, the view merges. Using a
/// plain `fn` keeps the v1 surface tiny; we can swap to a trait
/// object for stateful sources later.
pub type DecorationSource = fn(&EditorState) -> Vec<DecoratedRange>;

/// Per-instance id allocator — each `<Editor>` mount gets a
/// unique `data-editor-id` for the JS bridge to find it.
static EDITOR_INSTANCE: AtomicU64 = AtomicU64::new(0);

/// `keymap` is optional. When `None` the browser handles every
/// key. When `Some`, each `onkeydown` looks for a matching
/// binding whose command returns `Some(spec)` and we
/// `preventDefault` + apply. Unmatched keys fall through.
#[component]
pub fn Editor(
    state: Signal<EditorState>,
    #[props(default)] keymap: Option<Keymap>,
    #[props(default)] decorations: Option<DecorationSource>,
) -> Element {
    let text = state.read().doc.to_string();
    // Run decoration sources, merge, sort. Empty / no source ⇒
    // one undecorated text segment covering the whole doc.
    let decs: Vec<DecoratedRange> = if let Some(src) = decorations {
        let mut v = src(&state.read());
        v.sort_by_key(|d| d.from);
        v
    } else {
        Vec::new()
    };
    let segments = render::render(&text, &decs);
    let editor_id = use_hook(|| {
        let n = EDITOR_INSTANCE.fetch_add(1, Ordering::Relaxed);
        format!("editor-{n}")
    });

    // ── DOM → state: input + selection bridge ────────────────────
    //
    // Once on mount we install a single JS listener that wires up
    // multiple DOM events and streams them back via dioxus.send.
    // Each `input` message carries the new textContent + the
    // current Selection so we can update doc and caret atomically.
    // Other events (keyup/mouseup/select/focus) carry just
    // selection for caret moves without text changes.
    {
        let id = editor_id.clone();
        use_hook(move || {
            spawn(async move {
                let script = format!(
                    r#"
                    (function() {{
                        function attach() {{
                            const el = document.querySelector('[data-editor-id="{id}"]');
                            if (!el) {{ setTimeout(attach, 30); return; }}
                            // For a single-text-node contenteditable
                            // the DOM selection's anchorOffset is a
                            // char offset in that text node — same as
                            // our byte offset for ASCII content. We'll
                            // upgrade to walking the DOM tree when we
                            // emit decoration spans.
                            function selOffsets() {{
                                const s = window.getSelection();
                                if (!s || s.rangeCount === 0) return [0, 0];
                                const r = s.getRangeAt(0);
                                // Walk to a numeric offset within el.
                                const off = (node, n) => {{
                                    if (!el.contains(node)) return null;
                                    let total = 0;
                                    const walker = document.createTreeWalker(
                                        el, NodeFilter.SHOW_TEXT, null
                                    );
                                    let t;
                                    while ((t = walker.nextNode())) {{
                                        if (t === node) return total + n;
                                        total += t.nodeValue.length;
                                    }}
                                    return null;
                                }};
                                const a = off(r.startContainer, r.startOffset);
                                const b = off(r.endContainer, r.endOffset);
                                if (a === null || b === null) return [0, 0];
                                return [a, b];
                            }}
                            function sendInput() {{
                                const [a, b] = selOffsets();
                                dioxus.send({{
                                    kind: 'input',
                                    text: el.textContent,
                                    sel: [a, b]
                                }});
                            }}
                            function sendSel() {{
                                const [a, b] = selOffsets();
                                dioxus.send({{ kind: 'sel', sel: [a, b] }});
                            }}
                            el.addEventListener('input',   sendInput);
                            el.addEventListener('keyup',   sendSel);
                            el.addEventListener('mouseup', sendSel);
                            el.addEventListener('select',  sendSel);
                            el.addEventListener('focus',   sendSel);
                            sendSel();
                        }}
                        attach();
                    }})();
                    "#
                );
                let mut handle = document::eval(&script);
                while let Ok(v) = handle.recv::<serde_json::Value>().await {
                    handle_bridge_msg(state, &v);
                }
            });
        });
    }

    // ── state → DOM: caret writeback for programmatic edits ──────
    //
    // Runs every render. Reads state.selection's primary range,
    // checks the live DOM against state.doc — if the DOM hasn't
    // caught up yet (still mid-typing), we skip so we don't fight
    // the user. When they match, we set the DOM Selection to the
    // state's caret. This is what lets `Mod-A` (select_all) and
    // any future cursor-moving command actually move the caret.
    {
        let id = editor_id.clone();
        use_effect(move || {
            let s = state.read();
            let doc = s.doc.to_string();
            let p = s.selection.primary();
            let from = p.from();
            let to = p.to();
            let doc_json = serde_json::to_string(&doc).unwrap_or_else(|_| "\"\"".into());
            let script = format!(
                r#"
                (function() {{
                    const el = document.querySelector('[data-editor-id="{id}"]');
                    if (!el) return;
                    // If the DOM hasn't caught up with state.doc
                    // yet, the user just typed and selection from
                    // state would be stale — skip and let the next
                    // tick handle it.
                    if (el.textContent !== {doc_json}) return;

                    // Build a Range that targets the requested
                    // byte/char offsets, walking text descendants.
                    const targetRange = document.createRange();
                    const setEnd = (offset, which) => {{
                        let remaining = offset;
                        const walker = document.createTreeWalker(
                            el, NodeFilter.SHOW_TEXT, null
                        );
                        let node;
                        while ((node = walker.nextNode())) {{
                            const len = node.nodeValue.length;
                            if (remaining <= len) {{
                                if (which === 'start') {{
                                    targetRange.setStart(node, remaining);
                                }} else {{
                                    targetRange.setEnd(node, remaining);
                                }}
                                return true;
                            }}
                            remaining -= len;
                        }}
                        // Past the end — pin to el.
                        if (which === 'start') {{
                            targetRange.setStart(el, el.childNodes.length);
                        }} else {{
                            targetRange.setEnd(el, el.childNodes.length);
                        }}
                        return true;
                    }};
                    setEnd({from}, 'start');
                    setEnd({to}, 'end');

                    const sel = window.getSelection();
                    // Skip if DOM already matches — `setBaseAndExtent`
                    // re-emits a selectionchange and our DOM→state
                    // bridge would treat that as a fresh edit.
                    if (sel && sel.rangeCount === 1) {{
                        const cur = sel.getRangeAt(0);
                        if (cur.startContainer === targetRange.startContainer
                            && cur.startOffset === targetRange.startOffset
                            && cur.endContainer === targetRange.endContainer
                            && cur.endOffset === targetRange.endOffset) {{
                            return;
                        }}
                    }}
                    sel.removeAllRanges();
                    sel.addRange(targetRange);
                }})();
                "#
            );
            let _ = document::eval(&script);
        });
    }

    // ── onkeydown: keymap dispatch ───────────────────────────────
    let keymap_for_keys = keymap.clone();
    let on_keydown = move |evt: Event<KeyboardData>| {
        let Some(ref km) = keymap_for_keys else {
            return;
        };
        let mods = evt.modifiers();
        let key_str = match evt.key() {
            Key::Character(c) => c,
            other => other.to_string(),
        };
        let press = KeySpec {
            key: key_str,
            ctrl: mods.ctrl(),
            alt: mods.alt(),
            shift: mods.shift(),
            meta: mods.meta(),
            r#mod: mods.ctrl() || mods.meta(),
        };
        let cur = state.read().clone();
        if let Some(spec) = km.dispatch(&press, &cur) {
            evt.prevent_default();
            tracing::debug!(?press, "editor.keymap.fire");
            state.set(cur.update(spec));
        }
    };

    rsx! {
        div {
            class: "editor-root",
            "data-editor-id": "{editor_id}",
            // `plaintext-only` strips formatting from paste +
            // disables rich-text execCommands. Chromium/WebKit
            // support it; Firefox falls back to "true". Cursor
            // can still be placed inside child spans under this
            // mode — which is exactly what's needed for
            // decorated text.
            contenteditable: "plaintext-only",
            spellcheck: "false",
            onkeydown: on_keydown,
            // Children: render the doc as the rendered segment
            // stream. For undecorated text this collapses to a
            // single text node — same Dioxus-reconciler-no-op
            // behavior that keeps the caret stable during
            // typing. For decorated text we emit `<span>`s with
            // class names; the browser still treats the result
            // as one continuous text stream for cursor purposes
            // (per `plaintext-only` semantics).
            for seg in segments.iter() {
                {render_segment(seg)}
            }
        }
    }
}

/// Convert one `Segment` into a Dioxus element.
///
/// - `Text` with empty class → bare text node (no wrapping
///   span) so the un-decorated common case still produces the
///   exact same DOM shape as Phase A. This is what lets typing
///   on a doc with no decorations stay a no-op for the
///   reconciler.
/// - `Text` with classes → `<span class="…">`.
/// - `Hidden` → nothing (omit from the DOM).
/// - `Widget` → `dangerous_inner_html` with the source's raw
///   HTML. v1 widgets are static strings; future widgets can
///   carry richer payloads.
fn render_segment(seg: &Segment) -> Element {
    match seg {
        Segment::Text { text, classes, .. } => {
            if classes.is_empty() {
                rsx! { "{text}" }
            } else {
                rsx! { span { class: "{classes}", "{text}" } }
            }
        }
        Segment::Hidden { .. } => rsx! {},
        Segment::Widget { html, .. } => rsx! {
            span {
                class: "editor-widget",
                contenteditable: "false",
                dangerous_inner_html: "{html}"
            }
        },
    }
}

/// Handle one `dioxus.send` message from the JS bridge. Lives
/// outside the component for testability — given a state Signal
/// and the parsed JSON, mutate.
fn handle_bridge_msg(mut state: Signal<EditorState>, v: &serde_json::Value) {
    let kind = v.get("kind").and_then(|k| k.as_str()).unwrap_or("");
    let sel = v.get("sel").and_then(|s| s.as_array());
    let (s_off, e_off) = match sel {
        Some(a) if a.len() == 2 => (
            a[0].as_u64().unwrap_or(0) as usize,
            a[1].as_u64().unwrap_or(0) as usize,
        ),
        _ => (0, 0),
    };

    match kind {
        "input" => {
            let new_text = v.get("text").and_then(|t| t.as_str()).unwrap_or("");
            let cur = state.read().clone();
            let old_text = cur.doc.to_string();
            if old_text == new_text {
                // Selection-only update (e.g., an empty input
                // event from IME). Treat like a `sel` message.
                push_selection(&mut state, &cur, s_off, e_off);
                return;
            }
            // Minimal diff: trim common prefix + suffix, then
            // splice the middle. Good enough for single
            // character typing AND multi-char paste/delete. CRDT
            // integration later can produce finer-grained ops.
            let changes = diff_text(&old_text, new_text);
            let new_len = new_text.len();
            let s_off = s_off.min(new_len);
            let e_off = e_off.min(new_len).max(s_off);
            tracing::debug!(
                old_len = old_text.len(),
                new_len,
                sel_start = s_off,
                sel_end = e_off,
                "editor.input"
            );
            let new_sel = Selection::single(Range::new(s_off, e_off));
            state.set(cur.update(
                TransactionSpec::new()
                    .changes(changes)
                    .selection(new_sel)
                    .annotate("origin", "input"),
            ));
        }
        "sel" => {
            let cur = state.read().clone();
            push_selection(&mut state, &cur, s_off, e_off);
        }
        _ => {}
    }
}

/// Push a selection-only transaction if the new range differs
/// from the current state.
fn push_selection(state: &mut Signal<EditorState>, cur: &EditorState, s: usize, e: usize) {
    let doc_len = cur.doc.len();
    let s = s.min(doc_len);
    let e = e.min(doc_len);
    let cur_primary = cur.selection.primary();
    if cur_primary.anchor == s && cur_primary.head == e {
        return;
    }
    tracing::trace!(
        old_anchor = cur_primary.anchor,
        old_head = cur_primary.head,
        new_start = s,
        new_end = e,
        "editor.selection"
    );
    let new_sel = Selection::single(Range::new(s, e));
    state.set(cur.update(
        TransactionSpec::new()
            .selection(new_sel)
            .annotate("origin", "input"),
    ));
}

/// Compute a minimal `Changes` between two strings by trimming
/// common prefix + suffix and replacing the diff in the middle.
/// O(n) — good enough for typing and small pastes; replace with
/// a proper diff algorithm later if we want minimal ops for
/// large pastes too.
fn diff_text(old: &str, new: &str) -> Changes {
    let ob = old.as_bytes();
    let nb = new.as_bytes();
    let mut start = 0;
    while start < ob.len() && start < nb.len() && ob[start] == nb[start] {
        start += 1;
    }
    let mut o_end = ob.len();
    let mut n_end = nb.len();
    while o_end > start && n_end > start && ob[o_end - 1] == nb[n_end - 1] {
        o_end -= 1;
        n_end -= 1;
    }
    // Walk back to a UTF-8 boundary in case our binary trim
    // landed in the middle of a multi-byte sequence.
    while start > 0 && !old.is_char_boundary(start) {
        start -= 1;
    }
    while o_end < ob.len() && !old.is_char_boundary(o_end) {
        o_end += 1;
    }
    while n_end < nb.len() && !new.is_char_boundary(n_end) {
        n_end += 1;
    }
    let inserted = &new[start..n_end];
    Changes::replace(start..o_end, inserted)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn diff_appends_one_char() {
        let c = diff_text("hello", "helloa");
        let cs: Vec<_> = c.iter().cloned().collect();
        assert_eq!(cs.len(), 1);
        let only = &cs[0];
        assert_eq!(only.from, 5);
        assert_eq!(only.to, 5);
        assert_eq!(only.inserted, "a");
    }

    #[test]
    fn diff_inserts_in_middle() {
        let c = diff_text("hello world", "hello big world");
        let cs: Vec<_> = c.iter().cloned().collect();
        assert_eq!(cs.len(), 1);
        let only = &cs[0];
        assert_eq!(only.from, 6);
        assert_eq!(only.to, 6);
        assert_eq!(only.inserted, "big ");
    }

    #[test]
    fn diff_deletes_one_char() {
        let c = diff_text("helloa", "hello");
        let cs: Vec<_> = c.iter().cloned().collect();
        assert_eq!(cs.len(), 1);
        let only = &cs[0];
        assert_eq!(only.from, 5);
        assert_eq!(only.to, 6);
        assert_eq!(only.inserted, "");
    }

    #[test]
    fn diff_replaces_range() {
        let c = diff_text("hello world", "hello RUST");
        let cs: Vec<_> = c.iter().cloned().collect();
        assert_eq!(cs.len(), 1);
        let only = &cs[0];
        assert_eq!(only.from, 6);
        assert_eq!(only.to, 11);
        assert_eq!(only.inserted, "RUST");
    }

    #[test]
    fn diff_identical_is_empty() {
        let c = diff_text("hello", "hello");
        let cs: Vec<_> = c.iter().cloned().collect();
        assert_eq!(cs.len(), 1);
        // Trim-and-replace algorithm returns a no-op
        // `replace(5..5, "")`. Functionally equivalent to empty
        // for our apply path; could collapse in a follow-up.
        let only = &cs[0];
        assert_eq!(only.from, only.to);
        assert_eq!(only.inserted, "");
    }
}
