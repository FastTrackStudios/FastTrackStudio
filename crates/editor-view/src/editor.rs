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
    Change, Changes, DecoratedRange, EditorState, KeySpec, Keymap, Range, Selection,
    TransactionSpec,
};

use crate::tile::build::build_tiles;
use crate::tile::render_dx::render_tile;
use crate::tile::visible::VisibleText;

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
    // Run decoration sources, merge, sort. Build the tile
    // tree — Phase 6 port of CM6's buildtile.ts. The flat-Vec
    // segment path that lived in `render::render` has been
    // replaced by the full tile-tree path through `build_tiles`
    // + `render_tile`.
    let decs: Vec<DecoratedRange> = if let Some(src) = decorations {
        let mut v = src(&state.read());
        v.sort_by_key(|d| d.from);
        v
    } else {
        Vec::new()
    };
    let (arena, doc_tile) = build_tiles(&text, &decs);
    let editor_id = use_hook(|| {
        let n = EDITOR_INSTANCE.fetch_add(1, Ordering::Relaxed);
        format!("editor-{n}")
    });

    // ── DOM → state: MutationObserver + selection bridge ─────────
    //
    // Ports CM6's `view/src/domobserver.ts` at v1 scope. A
    // `MutationObserver` on the editor root catches every kind
    // of edit (typing, paste, drag-drop, IME) — broader than
    // the `input` event we used before. On each mutation batch
    // we read the current textContent + selection and send to
    // Rust for diff + Transaction.
    //
    // Composition handling: between `compositionstart` and
    // `compositionend` we *skip* mutation handling — the IME's
    // intermediate states aren't useful and would corrupt the
    // doc mid-composition. On `compositionend` we flush a
    // single update with the final text. (CM6 does the same
    // pause-resume pattern.)
    //
    // Selection-only moves (keyup/mouseup/select/focus) still
    // flow via `sel` messages as in Phase 7.
    {
        let id = editor_id.clone();
        // Capture the decoration source so the spawn closure
        // can rebuild the tile tree + visible-text mirror when
        // diffing each input message.
        let deco_source = decorations;
        use_hook(move || {
            spawn(async move {
                let script = format!(
                    r#"
                    (function() {{
                        function attach() {{
                            const el = document.querySelector('[data-editor-id="{id}"]');
                            if (!el) {{ setTimeout(attach, 30); return; }}
                            // Tile-tree-aware DOM → doc offset
                            // translation. Walks up from the
                            // selection's anchor/focus to the
                            // nearest ancestor carrying
                            // `data-tile-pos` (set by render_dx.rs),
                            // reads that as the tile's start
                            // position in the doc, and adds the
                            // text-node offset within the tile.
                            //
                            // Replaces the old TreeWalker-based
                            // approach which assumed visible-text
                            // offset == doc offset — wrong as soon
                            // as Hidden (Replace) decorations exist.
                            // CM6's equivalent walk is in
                            // `view/src/docview.ts:282` (posFromDOM).
                            function tilePosOf(node) {{
                                let n = node;
                                while (n && n !== el) {{
                                    if (n.nodeType === 1 /* ELEMENT */
                                        && n.dataset && n.dataset.tilePos != null) {{
                                        return parseInt(n.dataset.tilePos, 10);
                                    }}
                                    n = n.parentNode;
                                }}
                                return 0; // fall back to doc start
                            }}
                            function selOffsets() {{
                                const s = window.getSelection();
                                if (!s || s.rangeCount === 0) return [0, 0];
                                const r = s.getRangeAt(0);
                                const a = tilePosOf(r.startContainer) + r.startOffset;
                                const b = tilePosOf(r.endContainer) + r.endOffset;
                                return [a, b];
                            }}
                            // Reconstruct doc text from the
                            // tile-tree-rendered DOM. Each LineTile
                            // renders as `<div class="cm-line">`;
                            // BreakAfter on a line means there's
                            // a `\n` between it and the next line.
                            // textContent alone *doesn't* include
                            // those newlines (it just concats text
                            // descendants), so naively reading
                            // textContent would drop every `\n` in
                            // the doc — diffs would then think
                            // the user deleted every newline on
                            // every keystroke.
                            function readText() {{
                                const lines = el.querySelectorAll('.cm-line');
                                if (!lines.length) return el.textContent;
                                return Array.from(lines)
                                    .map(l => l.textContent)
                                    .join('\n');
                            }}
                            function sendInput() {{
                                const [a, b] = selOffsets();
                                dioxus.send({{
                                    kind: 'input',
                                    text: readText(),
                                    sel: [a, b]
                                }});
                            }}
                            function sendSel() {{
                                // Skip during programmatic
                                // writes (Phase 10) and during
                                // the brief window after a DOM
                                // mutation where Selection may
                                // be orphaned and unreliable.
                                if (el.dataset.writing === '1') return;
                                if (el.dataset.muting === '1') return;
                                const [a, b] = selOffsets();
                                dioxus.send({{ kind: 'sel', sel: [a, b] }});
                            }}

                            // Composition guard. Mirrors CM6's
                            // pause-during-composition pattern:
                            // - sendInput skipped while composing
                            // - state→DOM selection writeback
                            //   also reads this flag (via the
                            //   `data-composing` attribute on the
                            //   root) and bails so it doesn't
                            //   fight the IME for the Selection
                            // - compositionend flushes one
                            //   sendInput with the final text
                            // - compositionend also notifies Rust
                            //   via a typed message so any
                            //   composition-aware code can react
                            let composing = false;
                            el.addEventListener('compositionstart', () => {{
                                composing = true;
                                el.dataset.composing = '1';
                                dioxus.send({{ kind: 'composition-start' }});
                            }});
                            el.addEventListener('compositionend', () => {{
                                composing = false;
                                delete el.dataset.composing;
                                sendInput();
                                dioxus.send({{ kind: 'composition-end' }});
                            }});

                            // MutationObserver — catches every
                            // kind of DOM change (typing, paste,
                            // drag-drop, IME). CM6 reference:
                            // `view/src/domobserver.ts:103+`
                            // (the `observe` method).
                            //
                            // beforeinput interception — ports
                            // CM6's `view/src/domchange.ts`
                            // strategy of authoring edits
                            // ourselves rather than reading the
                            // DOM back after a browser-chosen
                            // mutation. For inputType events we
                            // can map cleanly (Enter, Backspace,
                            // typed chars on some IMEs), we
                            // preventDefault and send a typed
                            // message; Rust applies the Change
                            // and Dioxus re-renders the DOM the
                            // way *we* want it (e.g., a new
                            // LineTile div, not a plain <br>).
                            //
                            // Inputs we don't recognize fall
                            // through to the MutationObserver
                            // path below.
                            el.addEventListener('beforeinput', evt => {{
                                if (composing) return;
                                const t = evt.inputType;
                                if (t === 'insertParagraph' || t === 'insertLineBreak') {{
                                    evt.preventDefault();
                                    dioxus.send({{
                                        kind: 'before-input-insert',
                                        text: '\n',
                                        sel: selOffsets(),
                                    }});
                                }}
                                // Other inputTypes fall through.
                            }});

                            // MutationObserver — full re-read on
                            // every mutation. The `muting` flag
                            // it sets ALSO suppresses
                            // selectionchange-driven sendSel for
                            // one frame, because Dioxus's
                            // node-replace renders (driven by
                            // decoration shape changes) orphan
                            // DOM Selection and emit a bogus
                            // selectionchange BEFORE the
                            // writeback effect can resync. That
                            // bogus event would otherwise
                            // clobber state.selection with the
                            // orphaned position.
                            // CM6's `domobserver` pauses around
                            // its own writes; the `muting` flag
                            // is the analogue. We DO NOT clear
                            // it on rAF — the selection
                            // writeback effect (use_effect on
                            // state changes) is responsible for
                            // clearing it once it has written
                            // the correct DOM Selection. That
                            // way any selectionchange fired
                            // during the writeback's
                            // setSelectionRange is also covered.
                            // If no writeback runs (e.g., the
                            // observer fire produced no state
                            // change), a safety rAF clears the
                            // flag so we don't permanently mute.
                            const mo = new MutationObserver(() => {{
                                if (composing) return;
                                el.dataset.muting = '1';
                                // Safety net — if no writeback
                                // arrives (no state change), the
                                // flag clears so future
                                // selectionchange isn't lost.
                                // Multiple rAFs ahead so the
                                // writeback (whose effect fires
                                // in the same tick as the
                                // resulting render) has time to
                                // win the race and clear early.
                                requestAnimationFrame(() => requestAnimationFrame(() => {{
                                    delete el.dataset.muting;
                                }}));
                                sendInput();
                            }});
                            mo.observe(el, {{
                                childList: true,
                                characterData: true,
                                subtree: true,
                            }});

                            // Selection-only events. `selectionchange`
                            // is the canonical event for caret
                            // movement (covers programmatic
                            // updates that keyup/mouseup miss).
                            // It fires on `document`, not the
                            // element — we filter to selections
                            // that intersect our editor.
                            //
                            // The state→DOM selection writeback
                            // effect sets `el.dataset.writing`
                            // around its setSelectionRange call;
                            // we skip the listener while that
                            // flag is set so our own write
                            // doesn't loop back through the
                            // bridge and clamp the state-side
                            // selection to whatever the browser
                            // could actually represent (which
                            // is shorter than state.doc when
                            // Hidden tiles are involved).
                            document.addEventListener('selectionchange', () => {{
                                // Skip during our own writes
                                // (Phase 10) and during the
                                // frame following a DOM mutation
                                // (Dioxus decoration churn —
                                // Selection may be orphaned and
                                // reading it would clobber state
                                // with garbage).
                                if (el.dataset.writing === '1') return;
                                if (el.dataset.muting === '1') return;
                                const s = window.getSelection();
                                if (s && s.anchorNode && el.contains(s.anchorNode)) {{
                                    sendSel();
                                }}
                            }});
                            el.addEventListener('keyup',   sendSel);
                            el.addEventListener('mouseup', sendSel);
                            el.addEventListener('focus',   sendSel);
                            sendSel();
                        }}
                        attach();
                    }})();
                    "#
                );
                let mut handle = document::eval(&script);
                while let Ok(v) = handle.recv::<serde_json::Value>().await {
                    handle_bridge_msg(state, deco_source, &v);
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
                    if (el.dataset.composing === '1') return;
                    // Compare the DOM's *visible* representation
                    // (line text joined with \n) against state.doc.
                    // `el.textContent` would miss the newlines
                    // between block-level `<div class="cm-line">`
                    // children and never match a multi-line doc.
                    // This is the same readText() shape the input
                    // bridge uses.
                    const visibleNow = (function() {{
                        const lines = el.querySelectorAll('.cm-line');
                        if (!lines.length) return el.textContent;
                        return Array.from(lines)
                            .map(l => l.textContent)
                            .join('\n');
                    }})();
                    if (visibleNow !== {doc_json}) return;

                    // Build a Range targeting the requested doc
                    // positions via tile lookup. Each rendered
                    // tile carries `data-tile-pos`; for a target
                    // doc position we find the tile whose
                    // `[pos, pos + length)` covers it, then place
                    // the Range inside its text descendant at
                    // `target - tile_pos`. Mirrors CM6's
                    // `domAtPos` (`docview.ts:320`).
                    const targetRange = document.createRange();
                    const tiles = el.querySelectorAll('[data-tile-pos]');
                    // Build a sorted (pos, end, element) list once.
                    const ranges = [];
                    tiles.forEach(node => {{
                        const pos = parseInt(node.dataset.tilePos, 10);
                        const text = node.firstChild;
                        const len = (text && text.nodeType === 3 /* TEXT */)
                            ? text.nodeValue.length
                            : 0;
                        if (len) ranges.push({{ pos, end: pos + len, node, text }});
                    }});
                    ranges.sort((a, b) => a.pos - b.pos);
                    function placeEdge(target, which) {{
                        for (const r of ranges) {{
                            if (target >= r.pos && target <= r.end) {{
                                const off = target - r.pos;
                                if (which === 'start') targetRange.setStart(r.text, off);
                                else                   targetRange.setEnd(r.text, off);
                                return;
                            }}
                        }}
                        // Past the last tile — pin to the editor.
                        if (which === 'start') targetRange.setStart(el, el.childNodes.length);
                        else                   targetRange.setEnd(el, el.childNodes.length);
                    }}
                    placeEdge({from}, 'start');
                    placeEdge({to}, 'end');

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
                    // CM6-style write boundary: while our own
                    // setSelectionRange is in flight, suppress
                    // observer/listener-driven sendSel. Same
                    // pattern as `muting` — but `writing` covers
                    // the writeback's own emitted selectionchange.
                    el.dataset.writing = '1';
                    sel.removeAllRanges();
                    sel.addRange(targetRange);
                    // Clear `muting` (set earlier by the
                    // MutationObserver that triggered this
                    // render) NOW that we've successfully
                    // resynced DOM Selection from state. Without
                    // this, a stale `muting='1'` would persist
                    // until the rAF safety-net fired.
                    delete el.dataset.muting;
                    requestAnimationFrame(() => {{
                        delete el.dataset.writing;
                    }});
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
            // Children: render the tile tree. Each composite
            // tile becomes a `<span data-tile-id="N">`; text
            // tiles emit a bare text node so the
            // Dioxus-reconciler-no-op trick (matching DOM
            // text → no DOM mutation) still holds during
            // typing. See `tile::render_dx::render_tile`.
            {render_tile(&arena, doc_tile)}
        }
    }
}

/// Handle one `dioxus.send` message from the JS bridge.
/// Decoration source is threaded through so we can rebuild the
/// tile tree + visible-text mirror to translate visible-space
/// edits back to doc space.
fn handle_bridge_msg(
    mut state: Signal<EditorState>,
    deco_source: Option<DecorationSource>,
    v: &serde_json::Value,
) {
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
            let new_visible = v.get("text").and_then(|t| t.as_str()).unwrap_or("");
            let cur = state.read().clone();
            // Rebuild the tile tree + visible-text mirror that
            // the DOM is currently showing, so we can diff
            // against the SAME visible text the browser just
            // mutated. Without this step, Hidden decorations
            // (markdown markers, etc.) would make state.doc
            // longer than DOM textContent and the diff would
            // attribute the difference to "user deleted hidden
            // bytes" on every keystroke.
            let decorations: Vec<DecoratedRange> = match deco_source {
                Some(src) => {
                    let mut v = src(&cur);
                    v.sort_by_key(|d| d.from);
                    v
                }
                None => Vec::new(),
            };
            let (arena, root) = build_tiles(&cur.doc.to_string(), &decorations);
            let old_visible = VisibleText::from_arena(&arena, root);
            if old_visible.text == new_visible {
                // No textual change — selection-only update.
                push_selection(&mut state, &cur, s_off, e_off);
                return;
            }
            // Diff in visible space.
            let vis_changes = diff_text(&old_visible.text, new_visible);
            if vis_changes.is_empty() {
                push_selection(&mut state, &cur, s_off, e_off);
                return;
            }
            // Translate each visible-space change to doc space.
            // s_off / e_off from the JS bridge already arrive in
            // doc space (via data-tile-pos), so they need no
            // mapping.
            let mut doc_changes: Vec<Change> = Vec::new();
            for c in vis_changes.iter() {
                let doc_from = old_visible.visible_to_doc(c.from);
                let doc_to = old_visible.visible_to_doc(c.to);
                doc_changes.push(Change {
                    from: doc_from,
                    to: doc_to,
                    inserted: c.inserted.clone(),
                });
            }
            let changes = Changes::from_sorted(doc_changes);
            let new_doc_len = cur.doc.len() as isize + changes
                .iter()
                .map(|c| c.delta())
                .sum::<isize>();
            let new_doc_len = new_doc_len.max(0) as usize;
            // Compute the new caret from the diff itself rather
            // than trust the sel field from the JS bridge. After
            // Dioxus's reconciler removes/re-creates the text
            // nodes our DOM Selection was anchored to, the
            // browser falls back to whatever position it can
            // find (often the start of the next/prev text tile),
            // and that bogus position used to leak into
            // state.selection — causing the cursor to jump
            // backward mid-typing. The diff knows the user's
            // INTENT: each Change ends at change.from +
            // inserted.len() in post-change doc space.
            let intended_caret = changes
                .iter()
                .last()
                .map(|c| {
                    // Pre-change `c.from` plus the inserted
                    // length plus accumulated delta from prior
                    // changes.
                    let prior_delta: isize = changes
                        .iter()
                        .take_while(|x| (*x).from < c.from)
                        .map(|x| x.delta())
                        .sum();
                    (c.from as isize + prior_delta + c.inserted.len() as isize)
                        .max(0) as usize
                })
                .unwrap_or(s_off)
                .min(new_doc_len);
            tracing::debug!(
                old_visible_len = old_visible.text.len(),
                new_visible_len = new_visible.len(),
                new_doc_len,
                intended_caret,
                js_sel_start = s_off,
                js_sel_end = e_off,
                "editor.input"
            );
            let new_sel = Selection::caret(intended_caret);
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
        "before-input-insert" => {
            // CM6-style author-the-edit-ourselves path. The JS
            // bridge preventDefault'd the browser's intended
            // mutation; we apply our own Change so the
            // resulting DOM is whatever the tile-tree render
            // produces (e.g., a new LineTile <div>, not a stray
            // browser <br>).
            //
            // s_off / e_off arrive in doc space (selOffsets
            // uses data-tile-pos). The insertion replaces any
            // selected range with the provided text.
            let text = v.get("text").and_then(|t| t.as_str()).unwrap_or("");
            let cur = state.read().clone();
            let doc_len = cur.doc.len();
            let from = s_off.min(doc_len);
            let to = e_off.min(doc_len).max(from);
            tracing::debug!(from, to, inserted_len = text.len(), "editor.before_input");
            let changes = Changes::replace(from..to, text);
            let caret = from + text.len();
            let new_sel = Selection::single(Range::caret(caret));
            state.set(cur.update(
                TransactionSpec::new()
                    .changes(changes)
                    .selection(new_sel)
                    .annotate("origin", "before-input"),
            ));
        }
        "composition-start" => {
            // Inform extensions (none yet) that composition has
            // begun. The actual pause behavior lives in JS via
            // the `data-composing` attribute the bridge sets.
            tracing::debug!("editor.composition.start");
        }
        "composition-end" => {
            tracing::debug!("editor.composition.end");
        }
        _ => {}
    }
}

/// Push a selection-only transaction if the new range differs
/// from the current state.
///
/// Clamp-detection guard: when state has a selection that
/// covers more doc than the DOM can currently represent (e.g.,
/// after `select_all` on a doc with Hidden markdown markers,
/// state is `(0, doc.len)` but DOM Selection clamps to visible
/// content's end), the next `selectionchange`/`keyup` would
/// otherwise read the clamped value and shrink state. We
/// recognize this as "DOM is a strict subset of cur" and skip
/// the update — state remains authoritative. Mirrors CM6's
/// `domobserver` ignoring DOM selection changes that are
/// derivable from current state.
fn push_selection(state: &mut Signal<EditorState>, cur: &EditorState, s: usize, e: usize) {
    let doc_len = cur.doc.len();
    let s = s.min(doc_len);
    let e = e.min(doc_len);
    let cur_primary = cur.selection.primary();
    if cur_primary.anchor == s && cur_primary.head == e {
        return;
    }
    // Clamp detection: cur has a non-caret selection extending
    // past where DOM ends (head/anchor at doc end), and the
    // incoming range is a subset of cur. Trust state.
    let cur_from = cur_primary.from();
    let cur_to = cur_primary.to();
    let incoming_from = s.min(e);
    let incoming_to = s.max(e);
    let cur_nontrivial = cur_from != cur_to;
    let incoming_is_subset = incoming_from >= cur_from && incoming_to <= cur_to;
    let cur_reaches_doc_end = cur_to == doc_len;
    if cur_nontrivial && incoming_is_subset && cur_reaches_doc_end && incoming_to < cur_to {
        tracing::trace!(
            cur_from,
            cur_to,
            incoming_from,
            incoming_to,
            "editor.selection.ignored_clamp"
        );
        return;
    }
    // Orphaned-selection guard: a (0, 0) coming in when cur is
    // non-zero is almost always the Dioxus reconciler removing
    // the text node our Selection was anchored to and the
    // browser falling back to editor-root position 0. Real
    // jumps-to-doc-start come from `Home` / arrow / click —
    // those events have separate paths (keyup, mouseup,
    // click) AND the user can re-place the caret if our guess
    // was wrong.
    if s == 0 && e == 0 && cur_primary.head != 0 && cur_primary.anchor != 0 {
        tracing::trace!(
            cur_anchor = cur_primary.anchor,
            cur_head = cur_primary.head,
            "editor.selection.ignored_orphan"
        );
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
