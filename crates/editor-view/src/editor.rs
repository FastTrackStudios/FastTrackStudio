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
use crate::tile::patch::build_patch;
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

/// Wall-clock milliseconds. wasm-safe: `Instant::now()` traps on
/// `wasm32-unknown-unknown`, so we hop through `performance.now()`
/// there. Used by perf-trace spans.
#[cfg(not(target_arch = "wasm32"))]
fn now_ms() -> f64 {
    static START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();
    let s = START.get_or_init(std::time::Instant::now);
    s.elapsed().as_secs_f64() * 1000.0
}
#[cfg(target_arch = "wasm32")]
fn now_ms() -> f64 {
    // Cheap call — browsers cache it. Falls back to 0 if for
    // some reason there's no `performance` (Workers etc.).
    web_sys::window()
        .and_then(|w| w.performance())
        .map(|p| p.now())
        .unwrap_or(0.0)
}

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
    // Tile-tree build moved into the imperative-patch
    // `use_effect` below. The component body itself no longer
    // computes a render — it just allocates the editor id and
    // schedules effects.
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
                            // Rust doc positions are UTF-8 byte
                            // offsets. JS string offsets are UTF-16
                            // code units. ASCII coincides, but
                            // anything outside Latin-1 (em-dash,
                            // emoji, CJK, …) makes them diverge.
                            // Convert at the boundary.
                            function utf16ToBytes(str, off) {{
                                let bytes = 0;
                                let i = 0;
                                while (i < off) {{
                                    const c = str.charCodeAt(i);
                                    if (c < 0x80) {{ bytes += 1; i += 1; }}
                                    else if (c < 0x800) {{ bytes += 2; i += 1; }}
                                    else if (c >= 0xD800 && c <= 0xDBFF) {{ bytes += 4; i += 2; }}
                                    else {{ bytes += 3; i += 1; }}
                                }}
                                return bytes;
                            }}
                            function bytesToUtf16(str, bytes) {{
                                let b = 0;
                                let i = 0;
                                while (i < str.length && b < bytes) {{
                                    const c = str.charCodeAt(i);
                                    if (c < 0x80) {{ b += 1; i += 1; }}
                                    else if (c < 0x800) {{ b += 2; i += 1; }}
                                    else if (c >= 0xD800 && c <= 0xDBFF) {{ b += 4; i += 2; }}
                                    else {{ b += 3; i += 1; }}
                                }}
                                return i;
                            }}
                            function utf8Len(str) {{ return utf16ToBytes(str, str.length); }}

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
                            // Sum visible chars contributed by `node`
                            // and all descendants. Doesn't include
                            // line-break "\n"s — those live between
                            // sibling `.cm-line` divs, and posFromDOM
                            // only descends *inside* one node.
                            function visTextLen(node) {{
                                if (!node) return 0;
                                if (node.nodeType === 3) return node.nodeValue.length;
                                let n = 0;
                                for (const c of node.childNodes) n += visTextLen(c);
                                return n;
                            }}
                            // Convert a (container, offset) pair to a
                            // doc-space position. Mirrors CM6's
                            // `posFromDOM` (`docview.ts:282`).
                            //
                            // - Text node: ancestor's data-tile-pos
                            //   + offset within text. The simple
                            //   case.
                            // - Element node: offset is a *child
                            //   index*. The cursor is just before
                            //   childNodes[offset]; we recurse into
                            //   either that child (offset 0) or the
                            //   previous sibling's end (offset > 0).
                            //   Browsers place the cursor at element
                            //   level when the click hits non-text
                            //   space inside a line div.
                            // Sum the doc-length each prior sibling
                            // of `node` contributes to its parent tile.
                            // Used to anchor positions when the
                            // browser has split text nodes within a
                            // line (typing inside an auto-paired
                            // bracket, IME composition, paste).
                            function priorSiblingBytes(node) {{
                                let bytes = 0;
                                let n = node.previousSibling;
                                while (n) {{
                                    if (n.nodeType === 3) {{
                                        bytes += utf8Len(n.nodeValue);
                                    }} else if (n.nodeType === 1) {{
                                        // Widget tile carries its doc
                                        // length explicitly; everything
                                        // else (mark spans, line
                                        // children) sums via recursion.
                                        if (n.dataset && n.dataset.tileLen != null) {{
                                            bytes += parseInt(n.dataset.tileLen, 10);
                                        }} else {{
                                            bytes += utf8Len(visText(n));
                                        }}
                                    }}
                                    n = n.previousSibling;
                                }}
                                return bytes;
                            }}
                            function posFromDOM(container, offset) {{
                                if (!container) return 0;
                                if (container.nodeType === 3) {{
                                    return tilePosOf(container)
                                        + priorSiblingBytes(container)
                                        + utf16ToBytes(container.nodeValue, offset);
                                }}
                                const kids = container.childNodes;
                                // Element-level "after last child":
                                // if the container itself is a
                                // tile (`data-tile-pos` +
                                // `data-tile-len`), return its
                                // END position. This accounts for
                                // Hidden ranges whose bytes have
                                // no visible characters but still
                                // occupy doc space (list markers,
                                // task markers, code fences).
                                // Without this, sendSel reports
                                // the widget's start position and
                                // state.selection collapses onto
                                // the *before-hidden* doc offset.
                                if (kids.length === 0
                                    && container.dataset
                                    && container.dataset.tilePos != null) {{
                                    const p = parseInt(container.dataset.tilePos, 10);
                                    const l = parseInt(container.dataset.tileLen || '0', 10);
                                    return p + (offset >= 1 ? l : 0);
                                }}
                                if (kids.length === 0) return tilePosOf(container);
                                if (offset >= kids.length) {{
                                    if (container.dataset
                                        && container.dataset.tilePos != null
                                        && container.dataset.tileLen != null) {{
                                        return parseInt(container.dataset.tilePos, 10)
                                            + parseInt(container.dataset.tileLen, 10);
                                    }}
                                    const last = kids[kids.length - 1];
                                    if (last.nodeType === 3) {{
                                        return tilePosOf(last) + utf8Len(last.nodeValue);
                                    }}
                                    let n = last;
                                    while (n.nodeType === 1 && n.lastChild) n = n.lastChild;
                                    if (n.nodeType === 3) {{
                                        return tilePosOf(n) + utf8Len(n.nodeValue);
                                    }}
                                    return tilePosOf(last);
                                }}
                                const next = kids[offset];
                                if (next.nodeType === 3) return tilePosOf(next);
                                let n = next;
                                while (n.nodeType === 1 && n.firstChild) n = n.firstChild;
                                if (n.nodeType === 3) return tilePosOf(n);
                                return tilePosOf(next);
                            }}
                            function selOffsets() {{
                                const s = window.getSelection();
                                if (!s || s.rangeCount === 0) return [0, 0];
                                // anchor/focus carry direction;
                                // start/end always sort. We send
                                // [anchor, head] so backward
                                // selections survive the round trip.
                                if (s.anchorNode && el.contains(s.anchorNode)) {{
                                    const a = posFromDOM(s.anchorNode, s.anchorOffset);
                                    const h = posFromDOM(s.focusNode, s.focusOffset);
                                    return [a, h];
                                }}
                                const r = s.getRangeAt(0);
                                const a = posFromDOM(r.startContainer, r.startOffset);
                                const b = posFromDOM(r.endContainer, r.endOffset);
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
                            // Reconstruct doc text from the
                            // tile-tree-rendered DOM, skipping any
                            // widget content (`contenteditable=
                            // false` spans rendered as decorations
                            // — checkboxes, bullet markers, code-
                            // block lang labels, copy buttons).
                            // Without the skip, widget characters
                            // leak into `textContent`, the input
                            // diff treats them as user-typed bytes
                            // and tries to insert them into
                            // state.doc.
                            function visText(node) {{
                                if (!node) return '';
                                if (node.nodeType === 3) return node.nodeValue;
                                if (node.nodeType !== 1) return '';
                                if (node.classList && node.classList.contains('editor-widget')) {{
                                    return '';
                                }}
                                let s = '';
                                for (const c of node.childNodes) s += visText(c);
                                return s;
                            }}
                            function readText() {{
                                const lines = el.querySelectorAll('.cm-line');
                                if (!lines.length) return visText(el);
                                return Array.from(lines).map(visText).join('\n');
                            }}
                            function sendInput() {{
                                const t0 = window.__cm_perf ? performance.now() : 0;
                                const [a, b] = selOffsets();
                                const text = readText();
                                if (window.__cm_perf) {{
                                    window.__cm_perf_summary.inputs += 1;
                                    const dt = performance.now() - t0;
                                    window.__cm_perf_summary.totalInputMs += dt;
                                    console.log(
                                        `[cm/perf] sendInput ${{dt.toFixed(2)}}ms`,
                                        'textLen=' + text.length + ' sel=' + a + ',' + b
                                    );
                                }}
                                dioxus.send({{
                                    kind: 'input',
                                    text: text,
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

                            // ── (dead) sync bracket helper ──
                            // Kept around for the perf-trace refactor
                            // — the sync path raced the patcher
                            // (re-render replaces text nodes, cursor
                            // reference lost), so brackets route
                            // through Rust today. The helpers below
                            // are still referenced by other code
                            // paths (peekNextChar etc.).
                            const OPENERS = {{ '(': ')', '[': ']', '{{': '}}' }};
                            const SAME = new Set(["'", '"', '`']);
                            const CLOSE_BEFORE = ')]}}>;,:';
                            function handleBracketSync(ch) {{
                                const sel = window.getSelection();
                                if (!sel || sel.rangeCount === 0) return false;
                                const r = sel.getRangeAt(0);
                                if (!el.contains(r.startContainer)) return false;
                                const isOpener = ch in OPENERS;
                                const isSame = SAME.has(ch);
                                const isCloser = !isOpener && !isSame
                                    && (ch === ')' || ch === ']' || ch === '}}');
                                if (!isOpener && !isSame && !isCloser) return false;

                                const collapsed = r.collapsed;
                                // For closer: skip-over if next char is the same.
                                if (isCloser && collapsed) {{
                                    const next = peekNextChar(r);
                                    if (next === ch) {{
                                        moveCursor(r, +1);
                                        return true;
                                    }}
                                    return false;
                                }}
                                // For same-char (quote): skip if next is the same.
                                if (isSame && collapsed) {{
                                    const next = peekNextChar(r);
                                    if (next === ch) {{
                                        moveCursor(r, +1);
                                        return true;
                                    }}
                                    // Only auto-pair when surrounded by
                                    // non-word chars / EOL.
                                    const prev = peekPrevChar(r);
                                    const wordRe = /[\w]/;
                                    if ((prev && wordRe.test(prev))
                                        || (next && wordRe.test(next))) {{
                                        return false;
                                    }}
                                }}
                                // For opener: check the "before" rule.
                                if (isOpener && collapsed) {{
                                    const next = peekNextChar(r);
                                    if (next && !/^\s$/.test(next)
                                        && !CLOSE_BEFORE.includes(next)) {{
                                        return false;
                                    }}
                                    // Suppress `[` auto-pair when
                                    // the user looks like they're
                                    // typing a task marker `- [`
                                    // / `* [` / `+ [` — the
                                    // closing `]` they type next
                                    // would otherwise be dropped
                                    // by skip-past, leaving a
                                    // stray `]` later.
                                    if (ch === '[') {{
                                        const lineText = enclosingLineText(r);
                                        if (lineText && /^[ ]*[-*+] $/.test(lineText)) {{
                                            return false;
                                        }}
                                    }}
                                }}
                                // Build the pair text. For same-char,
                                // the closer IS the opener.
                                const close = isOpener ? OPENERS[ch] : ch;
                                if (!collapsed) {{
                                    // Wrap selection.
                                    const text = r.toString();
                                    r.deleteContents();
                                    r.insertNode(document.createTextNode(ch + text + close));
                                    // Place caret at the end of the inserted (post-close).
                                    return true;
                                }}
                                // Empty caret — insert pair, leave caret between.
                                const pair = document.createTextNode(ch + close);
                                r.insertNode(pair);
                                const newRange = document.createRange();
                                newRange.setStart(pair, 1);
                                newRange.collapse(true);
                                sel.removeAllRanges();
                                sel.addRange(newRange);
                                // sendInput's diff will compute
                                // an intended-caret at the END of
                                // the inserted pair. We want the
                                // caret BETWEEN. Send a follow-up
                                // `sel` so once the input
                                // transaction commits, the
                                // selection lands where we want.
                                // Queued FIFO after the input msg,
                                // so the final state matches.
                                queueMicrotask(() => {{
                                    const [a, b] = selOffsets();
                                    dioxus.send({{ kind: 'sel', sel: [a, b] }});
                                }});
                                return true;
                            }}
                            // Return the visible source bytes of
                            // the line containing `r.startContainer`
                            // up to (but not including) the caret.
                            // Used by the bracket auto-pair to
                            // decide whether `[` should be a task-
                            // marker opener instead of a pair.
                            function enclosingLineText(r) {{
                                let n = r.startContainer;
                                while (n && n !== el && !(n.classList
                                    && n.classList.contains('cm-line'))) {{
                                    n = n.parentNode;
                                }}
                                if (!n || n === el) return '';
                                return visText(n);
                            }}
                            function peekNextChar(r) {{
                                const n = r.startContainer;
                                if (n.nodeType === 3) {{
                                    return n.nodeValue[r.startOffset] || '';
                                }}
                                const kid = n.childNodes[r.startOffset];
                                if (kid && kid.nodeType === 3) return kid.nodeValue[0] || '';
                                return '';
                            }}
                            function peekPrevChar(r) {{
                                const n = r.startContainer;
                                if (n.nodeType === 3) {{
                                    return r.startOffset > 0 ? n.nodeValue[r.startOffset - 1] : '';
                                }}
                                if (r.startOffset > 0) {{
                                    const prev = n.childNodes[r.startOffset - 1];
                                    if (prev && prev.nodeType === 3) {{
                                        return prev.nodeValue[prev.nodeValue.length - 1] || '';
                                    }}
                                }}
                                return '';
                            }}
                            function moveCursor(r, delta) {{
                                const n = r.startContainer;
                                if (n.nodeType !== 3) return;
                                const newRange = document.createRange();
                                newRange.setStart(n, r.startOffset + delta);
                                newRange.collapse(true);
                                const s = window.getSelection();
                                s.removeAllRanges();
                                s.addRange(newRange);
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
                            // CM6's strategy: intercept EVERY
                            // beforeinput event we can map
                            // cleanly. preventDefault, send the
                            // authored Change to Rust, let our
                            // reconciler put the result in the
                            // DOM. The browser never gets to
                            // modify our contenteditable — which
                            // eliminates the "browser writes a
                            // shape we have to reverse-engineer"
                            // problem completely.
                            //
                            // For inputTypes we don't recognize
                            // (e.g., insertFromPaste with rich
                            // HTML, formatBold/Italic, etc.) we
                            // fall through to the
                            // MutationObserver path, which reads
                            // the resulting DOM and computes a
                            // diff. That's a degraded path —
                            // works but exposes the empty-span
                            // bug — so the surface of cleanly-
                            // mapped types should grow over time.
                            // beforeinput interception is SCOPED
                            // to the inputTypes where the
                            // browser's default behavior is
                            // problematic (Enter creates non-
                            // portable DOM, and the "type into
                            // an empty Mark span" case crashes
                            // our reconciler). For ordinary
                            // typing we let the browser handle
                            // it and reconstruct via the
                            // MutationObserver — that path has
                            // years of test coverage and works
                            // well across decoration shapes.
                            //
                            // The compromise: we get correct
                            // Enter handling + a workaround for
                            // the empty-span case without
                            // redirecting *all* user typing
                            // through Rust (which exposes
                            // selOffsets edge cases we haven't
                            // fully chased).
                            el.addEventListener('beforeinput', evt => {{
                                if (composing) return;
                                const t = evt.inputType;
                                if (t === 'insertParagraph' || t === 'insertLineBreak') {{
                                    evt.preventDefault();
                                    // Route through the list-aware
                                    // Enter handler so pressing
                                    // Enter on a `- foo` / `1. foo`
                                    // / `- [x] foo` line continues
                                    // the list. Falls back to a
                                    // plain `\n` insert if the
                                    // line isn't a list item.
                                    dioxus.send({{
                                        kind: 'enter-continue-list',
                                        sel: selOffsets(),
                                    }});
                                    return;
                                }}
                                // Cursor inside an empty Mark
                                // span: the most common path to
                                // the "Dioxus reshape crash"
                                // bug. Intercept insertText here
                                // ONLY for this case — the empty
                                // span is the destabilizing
                                // factor, not typing in general.
                                if (t === 'insertText' && typeof evt.data === 'string') {{
                                    // Bracket / quote auto-pair —
                                    // routed through Rust. Sync-
                                    // in-JS would race the
                                    // re-render that follows; the
                                    // patcher replaces the
                                    // browser-inserted text node
                                    // with a fresh span and the
                                    // cursor reference is lost.
                                    if (/^[\(\[\{{\)\]\}}\'\"`]$/.test(evt.data)) {{
                                        evt.preventDefault();
                                        dioxus.send({{
                                            kind: 'insert-bracket',
                                            text: evt.data,
                                            sel: selOffsets(),
                                        }});
                                        return;
                                    }}
                                    const s = window.getSelection();
                                    let inEmptyMark = false;
                                    if (s && s.rangeCount > 0) {{
                                        let n = s.anchorNode;
                                        while (n && n !== el) {{
                                            // Properly-parenthesized:
                                            // "element with mark class AND
                                            //  (has no children OR has only
                                            //   an empty text node)".
                                            const isMark = n.nodeType === 1
                                                && n.classList
                                                && (n.classList.contains('md-bold')
                                                    || n.classList.contains('md-italic')
                                                    || n.classList.contains('md-code'));
                                            if (isMark) {{
                                                const c = n.firstChild;
                                                const isEmpty = !c
                                                    || (c.nodeType === 3
                                                        && c.nodeValue === '');
                                                if (isEmpty) {{
                                                    inEmptyMark = true;
                                                    break;
                                                }}
                                            }}
                                            n = n.parentNode;
                                        }}
                                    }}
                                    if (inEmptyMark) {{
                                        evt.preventDefault();
                                        dioxus.send({{
                                            kind: 'before-input-insert',
                                            text: evt.data,
                                            sel: selOffsets(),
                                        }});
                                    }}
                                }}
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
                            // CM6's `domobserver.ignore(f)`
                            // pattern: programmatic DOM writes
                            // call `observer.disconnect()`
                            // first, run their mutations, then
                            // `observe()` again — DISCARDING any
                            // queued records via takeRecords().
                            // This is more reliable than a
                            // muting flag because it stops the
                            // observer from queueing at all,
                            // eliminating the race where the
                            // observer's callback could fire
                            // mid-write.
                            const observeOpts = {{
                                childList: true,
                                characterData: true,
                                subtree: true,
                            }};
                            const mo = new MutationObserver(() => {{
                                if (composing) return;
                                el.dataset.muting = '1';
                                requestAnimationFrame(() => {{
                                    delete el.dataset.muting;
                                }});
                                sendInput();
                            }});
                            mo.observe(el, observeOpts);

                            // ── Imperative DOM patcher ──────
                            // Bypasses Dioxus's reconciler for
                            // the editor's contents. CM6 does
                            // this for the same reasons: VDOMs
                            // can't safely manage a
                            // contenteditable across the kinds
                            // of structural transitions
                            // decoration-aware rendering needs.
                            //
                            // `applyPatch(descs)` diffs the
                            // serialized tile tree against the
                            // live DOM and applies minimal
                            // mutations. Wrapped in
                            // disconnect/observe so the
                            // MutationObserver doesn't see our
                            // own writes.
                            function patchAttrs(elem, attrs) {{
                                const want = new Set();
                                for (const [k, v] of attrs) {{
                                    want.add(k);
                                    if (k === 'data-widget-html') {{
                                        if (elem.innerHTML !== v) elem.innerHTML = v;
                                    }} else if (elem.getAttribute(k) !== v) {{
                                        elem.setAttribute(k, v);
                                    }}
                                }}
                                // Remove attrs not in want.
                                for (let i = elem.attributes.length - 1; i >= 0; i--) {{
                                    const name = elem.attributes[i].name;
                                    if (!want.has(name)) {{
                                        elem.removeAttribute(name);
                                    }}
                                }}
                            }}

                            function patchChildren(parent, descs) {{
                                // Two-pass: collect existing
                                // children, then walk desired
                                // descs and either reuse-by-key,
                                // reuse-by-tag-at-position, or
                                // create-new. Trailing extras
                                // get removed.
                                let i = 0;
                                for (const d of descs) {{
                                    if (d.text !== undefined) {{
                                        const at = parent.childNodes[i];
                                        if (at && at.nodeType === 3) {{
                                            if (at.nodeValue !== d.text) {{
                                                // Chromium collapses
                                                // the Selection when
                                                // a Text node's data
                                                // is reassigned via
                                                // `nodeValue =` /
                                                // `data =`. Use
                                                // `replaceData` for
                                                // the minimal diff
                                                // so the cursor
                                                // stays where it
                                                // visually is.
                                                const oldT = at.nodeValue;
                                                const newT = d.text;
                                                let pre = 0;
                                                const maxPre = Math.min(oldT.length, newT.length);
                                                while (pre < maxPre
                                                    && oldT.charCodeAt(pre) === newT.charCodeAt(pre))
                                                    pre++;
                                                let suf = 0;
                                                const maxSuf = Math.min(
                                                    oldT.length - pre,
                                                    newT.length - pre
                                                );
                                                while (suf < maxSuf
                                                    && oldT.charCodeAt(oldT.length - 1 - suf)
                                                       === newT.charCodeAt(newT.length - 1 - suf))
                                                    suf++;
                                                const oldMid = oldT.length - pre - suf;
                                                const insMid = newT.substring(pre, newT.length - suf);
                                                at.replaceData(pre, oldMid, insMid);
                                            }}
                                        }} else {{
                                            const tn = document.createTextNode(d.text);
                                            parent.insertBefore(tn, at || null);
                                        }}
                                        i++;
                                        continue;
                                    }}
                                    const tag = d.tag;
                                    let key = null;
                                    for (const [k, v] of d.attrs) {{
                                        if (k === 'data-tile-pos') {{ key = v; break; }}
                                    }}
                                    // Try the element at `i`.
                                    let cand = parent.childNodes[i];
                                    if (cand
                                        && cand.nodeType === 1
                                        && cand.tagName.toLowerCase() === tag
                                        && (key == null
                                            || cand.dataset.tilePos === key)) {{
                                        patchAttrs(cand, d.attrs);
                                        if (tag !== 'br'
                                            && !d.attrs.some(([k]) => k === 'data-widget-html')) {{
                                            patchChildren(cand, d.kids || []);
                                        }}
                                        i++;
                                        continue;
                                    }}
                                    // Look ahead for a matching
                                    // keyed child elsewhere.
                                    let found = null;
                                    if (key != null) {{
                                        for (let j = i; j < parent.childNodes.length; j++) {{
                                            const c = parent.childNodes[j];
                                            if (c.nodeType === 1
                                                && c.tagName.toLowerCase() === tag
                                                && c.dataset
                                                && c.dataset.tilePos === key) {{
                                                found = c;
                                                break;
                                            }}
                                        }}
                                    }}
                                    if (found) {{
                                        parent.insertBefore(found, parent.childNodes[i] || null);
                                        patchAttrs(found, d.attrs);
                                        if (tag !== 'br'
                                            && !d.attrs.some(([k]) => k === 'data-widget-html')) {{
                                            patchChildren(found, d.kids || []);
                                        }}
                                        i++;
                                        continue;
                                    }}
                                    // Create.
                                    const elem = document.createElement(tag);
                                    patchAttrs(elem, d.attrs);
                                    if (tag !== 'br'
                                        && !d.attrs.some(([k]) => k === 'data-widget-html')) {{
                                        patchChildren(elem, d.kids || []);
                                    }}
                                    parent.insertBefore(elem, parent.childNodes[i] || null);
                                    i++;
                                }}
                                while (parent.childNodes.length > i) {{
                                    parent.removeChild(parent.lastChild);
                                }}
                            }}

                            // Selection placement — same data-tile-pos
                            // walk as the writeback effect, but
                            // run *inside* applyPatch so it
                            // happens AFTER DOM restructuring.
                            // Without this, a state change that
                            // reshapes the tile tree (e.g. live-
                            // preview hiding markdown markers when
                            // the caret leaves a span) destroys
                            // the cursor's anchor text node, the
                            // browser silently parks the cursor
                            // somewhere unrelated, and the next
                            // keystroke goes to the wrong place.
                            function placeSelection(anchor, head) {{
                                const tiles = el.querySelectorAll('[data-tile-pos]');
                                const textRanges = [];
                                const emptyTiles = [];
                                tiles.forEach(node => {{
                                    const pos = parseInt(node.dataset.tilePos, 10);
                                    const text = node.firstChild;
                                    if (text && text.nodeType === 3) {{
                                        const len = utf8Len(text.nodeValue);
                                        if (len) textRanges.push({{pos, end: pos + len, text}});
                                        else emptyTiles.push({{pos, node}});
                                    }} else {{
                                        emptyTiles.push({{pos, node}});
                                    }}
                                }});
                                textRanges.sort((a,b) => a.pos - b.pos);
                                emptyTiles.sort((a,b) => a.pos - b.pos);
                                // Returns [node, offset] for a doc
                                // byte position, using the same
                                // tile lookup the writeback used to.
                                function resolve(target) {{
                                    for (const t of textRanges) {{
                                        if (target > t.pos && target < t.end) {{
                                            return [t.text, bytesToUtf16(t.text.nodeValue, target - t.pos)];
                                        }}
                                    }}
                                    for (const t of emptyTiles) {{
                                        if (t.pos === target) return [t.node, 0];
                                    }}
                                    for (const t of textRanges) {{
                                        if (target >= t.pos && target <= t.end) {{
                                            return [t.text, bytesToUtf16(t.text.nodeValue, target - t.pos)];
                                        }}
                                    }}
                                    // Line-tile fallback: when typing a list
                                    // marker like `- ` empties the line of
                                    // text (the marker is fully replaced and
                                    // the only child is the bullet widget),
                                    // the cursor target may fall inside a
                                    // line tile but outside any text run.
                                    // Place it at the end of the matching
                                    // line div so the next keystroke lands
                                    // where the user expects.
                                    const lineTiles = el.querySelectorAll('div.cm-line[data-tile-pos]');
                                    for (const ln of lineTiles) {{
                                        const lpos = parseInt(ln.dataset.tilePos, 10);
                                        const llen = parseInt(ln.dataset.tileLen || '0', 10);
                                        if (target >= lpos && target <= lpos + llen) {{
                                            return [ln, ln.childNodes.length];
                                        }}
                                    }}
                                    return [el, el.childNodes.length];
                                }}
                                const [aNode, aOff] = resolve(anchor);
                                const [hNode, hOff] = resolve(head);
                                const sel = window.getSelection();
                                if (sel) {{
                                    // setBaseAndExtent preserves
                                    // direction (anchor → head).
                                    // Range/addRange would always
                                    // normalize to start<=end and
                                    // break shift+arrow-left.
                                    sel.setBaseAndExtent(aNode, aOff, hNode, hOff);
                                }}
                            }}

                            // ── perf logging ──
                            // Toggle in DevTools console with:
                            //   window.__cm_perf = true
                            // Then watch `[cm/perf]` lines.
                            function perfStart(label) {{
                                if (!window.__cm_perf) return 0;
                                return performance.now();
                            }}
                            function perfEnd(label, t0, extra) {{
                                if (!window.__cm_perf || t0 === 0) return;
                                const dt = performance.now() - t0;
                                console.log(
                                    `[cm/perf] {{label}} ${{dt.toFixed(2)}}ms`,
                                    extra || ''
                                );
                            }}
                            window.__cm_perf_summary = {{
                                patches: 0, totalPatchMs: 0,
                                inputs: 0, totalInputMs: 0,
                            }};

                            function applyPatch(payloadJson) {{
                                if (composing) return;
                                const t0 = perfStart('patch');
                                let payload;
                                try {{ payload = JSON.parse(payloadJson); }}
                                catch (_) {{ return; }}
                                const descs = payload.patches || payload;
                                const sel = payload.selection || null;
                                if (typeof payload.doc === 'string') {{
                                    window['__cm_doc_{id}'] = payload.doc;
                                }}
                                mo.disconnect();
                                el.dataset.writing = '1';
                                el.dataset.muting = '1';
                                try {{
                                    patchChildren(el, descs);
                                    if (sel) placeSelection(sel.anchor, sel.head);
                                }} finally {{
                                    mo.takeRecords();
                                    mo.observe(el, observeOpts);
                                    requestAnimationFrame(() => {{
                                        delete el.dataset.writing;
                                        delete el.dataset.muting;
                                    }});
                                }}
                                const dt = window.__cm_perf
                                    ? performance.now() - t0 : 0;
                                if (window.__cm_perf) {{
                                    window.__cm_perf_summary.patches += 1;
                                    window.__cm_perf_summary.totalPatchMs += dt;
                                    console.log(
                                        `[cm/perf] patch ${{dt.toFixed(2)}}ms`,
                                        `descs=${{descs.length}} docLen=${{(payload.doc||'').length}}`
                                    );
                                }}
                            }}
                            window['__cm_patch_{id}'] = function(descsJson) {{
                                window['__cm_pending_{id}'] = descsJson;
                                applyPatch(descsJson);
                            }};
                            // Replay anything the Dioxus effect
                            // stashed before the bridge attached.
                            const _pending = window['__cm_pending_{id}'];
                            if (_pending != null) applyPatch(_pending);


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
                            // Copy/cut: substitute the source
                            // markdown for the rendered DOM. The
                            // browser's default would copy what's
                            // visible (widget characters, hidden
                            // markers, etc.) which round-trips to
                            // nothing useful.
                            function sourceSlice() {{
                                const doc = window['__cm_doc_{id}'];
                                if (typeof doc !== 'string') return null;
                                const [a, b] = selOffsets();
                                const from = Math.min(a, b);
                                const to = Math.max(a, b);
                                if (from === to) return null;
                                // selOffsets returns byte offsets;
                                // slice the doc by codepoints by
                                // walking utf-8 byte counts.
                                let i = 0;
                                let bytes = 0;
                                let start = 0;
                                while (i < doc.length && bytes < from) {{
                                    const c = doc.charCodeAt(i);
                                    if (c < 0x80) {{ bytes += 1; i += 1; }}
                                    else if (c < 0x800) {{ bytes += 2; i += 1; }}
                                    else if (c >= 0xD800 && c <= 0xDBFF) {{ bytes += 4; i += 2; }}
                                    else {{ bytes += 3; i += 1; }}
                                }}
                                start = i;
                                while (i < doc.length && bytes < to) {{
                                    const c = doc.charCodeAt(i);
                                    if (c < 0x80) {{ bytes += 1; i += 1; }}
                                    else if (c < 0x800) {{ bytes += 2; i += 1; }}
                                    else if (c >= 0xD800 && c <= 0xDBFF) {{ bytes += 4; i += 2; }}
                                    else {{ bytes += 3; i += 1; }}
                                }}
                                return doc.slice(start, i);
                            }}
                            el.addEventListener('copy', evt => {{
                                const slice = sourceSlice();
                                if (slice == null) return;
                                evt.clipboardData.setData('text/plain', slice);
                                evt.preventDefault();
                            }});
                            el.addEventListener('cut', evt => {{
                                const slice = sourceSlice();
                                if (slice == null) return;
                                evt.clipboardData.setData('text/plain', slice);
                                evt.preventDefault();
                                // Let the existing input handlers
                                // delete the selected range as if
                                // the user pressed Backspace over it.
                                const [a, b] = selOffsets();
                                dioxus.send({{
                                    kind: 'before-input-delete-backward',
                                    sel: [Math.min(a,b), Math.max(a,b)],
                                }});
                            }});
                            // Link click handling. Plain click on a
                            // link/wikilink span navigates. To
                            // *edit* the link text instead, use
                            // the keyboard (arrow into it from an
                            // adjacent position). Mirrors
                            // Obsidian's preview-side behavior.
                            el.addEventListener('click', evt => {{
                                let n = evt.target;
                                while (n && n !== el) {{
                                    if (n.nodeType === 1 && n.dataset
                                        && n.dataset.copyFrom != null) {{
                                        evt.preventDefault();
                                        const from = parseInt(n.dataset.copyFrom, 10);
                                        const to   = parseInt(n.dataset.copyTo, 10);
                                        dioxus.send({{ kind: 'copy-range', from, to }});
                                        n.classList.add('copied');
                                        setTimeout(() => n.classList.remove('copied'), 800);
                                        return;
                                    }}
                                    if (n.nodeType === 1 && n.dataset
                                        && n.dataset.taskPos != null) {{
                                        evt.preventDefault();
                                        evt.stopPropagation();
                                        // Clicking a contenteditable=false
                                        // widget would otherwise create a
                                        // selection wrapping it; collapse it
                                        // so the user doesn't see a wide
                                        // highlight.
                                        const sel = window.getSelection();
                                        if (sel) sel.removeAllRanges();
                                        const p = parseInt(n.dataset.taskPos, 10);
                                        if (!isNaN(p)) {{
                                            dioxus.send({{ kind: 'task-toggle', pos: p }});
                                        }}
                                        return;
                                    }}
                                    if (n.nodeType === 1 && n.dataset
                                        && n.dataset.href) {{
                                        const href = n.dataset.href;
                                        // Clear caret state so the
                                        // link span goes back to
                                        // live-preview (the browser
                                        // had already parked the
                                        // caret inside it as part
                                        // of the click). Rust drops
                                        // selection to caret(0).
                                        dioxus.send({{ kind: 'link-clicked', href }});
                                        if (/^https?:/i.test(href)) {{
                                            window.open(href, '_blank', 'noopener');
                                        }}
                                        return;
                                    }}
                                    n = n.parentNode;
                                }}
                            }});
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

    // ── Imperative DOM patch effect ──────────────────────────
    //
    // Fires on every state change. Serializes the tile tree to
    // a `Patch` description and hands it to the JS-side
    // `__cm_patch_<id>` function set up in the bridge.
    //
    // Dioxus reconciler stays out of the editor's content
    // entirely — we render only an empty `<div data-editor-id>`
    // and the patcher fills + maintains everything inside.
    // CM6 model.
    {
        let id = editor_id.clone();
        let deco_source_patch = decorations;
        use_effect(move || {
            let _span = tracing::debug_span!("editor.patch_effect").entered();
            let s = state.read();
            let doc_len = s.doc.len();
            let t_decos = now_ms();
            let decorations: Vec<DecoratedRange> = match deco_source_patch {
                Some(src) => {
                    let mut v = src(&s);
                    v.sort_by_key(|d| d.from);
                    v
                }
                None => Vec::new(),
            };
            let deco_count = decorations.len();
            let decos_ms = now_ms() - t_decos;
            let t_build = now_ms();
            let (arena, root) = build_tiles(&s.doc.to_string(), &decorations);
            let build_ms = now_ms() - t_build;
            let t_patch = now_ms();
            let patch = build_patch(&arena, root);
            let patch_ms = now_ms() - t_patch;
            tracing::debug!(
                doc_len,
                deco_count,
                decos_ms = %format!("{:.2}", decos_ms),
                build_ms = %format!("{:.2}", build_ms),
                patch_ms = %format!("{:.2}", patch_ms),
                "editor.patch.build"
            );
            let primary = s.selection.primary();
            // Send anchor/head separately (not sorted) so JS uses
            // `setBaseAndExtent` and preserves direction. Otherwise
            // shift+arrow-left wouldn't extend backward — every
            // restored selection would have its head at the right
            // edge, and the keyboard would extend further right.
            // Send the full doc text along with the patch so the
            // JS-side copy handler can produce *source* markdown
            // when the user copies — the rendered DOM differs
            // (widgets replace markers, ` ``` ` fences are
            // hidden, etc.), and copying the rendered view would
            // be uneditable on paste.
            let payload = serde_json::json!({
                "patches": patch,
                "doc": s.doc.to_string(),
                "selection": {
                    "anchor": primary.anchor,
                    "head": primary.head,
                },
            });
            let patch_json = serde_json::to_string(&payload).unwrap_or_else(|_| "{}".into());
            let patch_json_lit =
                serde_json::to_string(&patch_json).unwrap_or_else(|_| "\"\"".into());
            let script = format!(
                r#"
                (function() {{
                    const payload = {patch_json_lit};
                    // Always stash the latest payload — the
                    // bridge consumes it on attach so we never
                    // race patches against in-flight retries.
                    window['__cm_pending_{id}'] = payload;
                    const fn = window['__cm_patch_{id}'];
                    if (typeof fn === 'function') fn(payload);
                }})();
                "#
            );
            let _ = document::eval(&script);
        });
    }

    rsx! {
        div {
            class: "editor-root",
            "data-editor-id": "{editor_id}",
            // `plaintext-only` strips formatting from paste +
            // disables rich-text execCommands.
            contenteditable: "plaintext-only",
            spellcheck: "false",
            onkeydown: on_keydown,
            // No children in rsx — Dioxus only sees / manages
            // the outer div's attributes. The interior is owned
            // by the imperative patcher above. Dioxus's
            // reconciler never touches our editor content.
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
                // Text-unchanged `input` event = Dioxus echo
                // (a mutation we caused via re-render, not user
                // typing). The selection on this message comes
                // from DOM Selection AFTER our reconciler may
                // have removed/recreated text nodes the cursor
                // was anchored to. That Selection is therefore
                // unreliable — it often points to a fallback
                // position (start of a sibling tile, parent
                // element offset, etc.) that has nothing to do
                // with user intent.
                //
                // CM6 has the same defense in `domobserver`:
                // selection reads that come from our own DOM
                // writes (rather than user actions) are ignored.
                // The dedicated `sel` channel
                // (selectionchange/keyup/mouseup/focus/click)
                // is the ONLY trusted source for selection
                // updates that aren't part of a Change.
                return;
            }
            // Diff in visible space.
            let vis_changes = diff_text(&old_visible.text, new_visible);
            if vis_changes.is_empty() {
                push_selection(&mut state, &cur, deco_source, s_off, e_off);
                return;
            }
            // Translate each visible-space change to doc space.
            //
            // Special-case the common typing path: when the
            // pre-typing selection is a caret and the diff is a
            // single insertion, anchor the insert at
            // `cur.selection.head` (doc space, authoritative)
            // instead of mapping through `visible_to_doc`. Without
            // this, typing right after a Hidden range — e.g.
            // after a list marker like `- [ ] ` whose bytes
            // contribute 0 visible chars — lands on the *start*
            // of the hidden range, dropping the typed text before
            // the marker bytes in the doc.
            let primary = cur.selection.primary();
            let vis_vec: Vec<Change> = vis_changes.iter().cloned().collect();
            let single_caret_insert = primary.anchor == primary.head
                && vis_vec.len() == 1
                && vis_vec[0].from == vis_vec[0].to;
            let mut doc_changes: Vec<Change> = Vec::new();
            if single_caret_insert {
                let head = primary.head.min(cur.doc.len());
                doc_changes.push(Change {
                    from: head,
                    to: head,
                    inserted: vis_vec[0].inserted.clone(),
                });
            } else {
                for c in vis_vec.iter() {
                    let doc_from = old_visible.visible_to_doc(c.from);
                    let doc_to = old_visible.visible_to_doc(c.to);
                    doc_changes.push(Change {
                        from: doc_from,
                        to: doc_to,
                        inserted: c.inserted.clone(),
                    });
                }
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
            push_selection(&mut state, &cur, deco_source, s_off, e_off);
        }
        "before-input-insert" => {
            // CM6-style author-the-edit-ourselves path. The JS
            // bridge preventDefault'd the browser's intended
            // mutation; we apply our own Change so the
            // resulting DOM is whatever the tile-tree render
            // produces (e.g., a new LineTile <div>, not a stray
            // browser <br>). For ordinary typing this means the
            // browser NEVER inserts into our contenteditable —
            // every visible byte got there because our
            // reconciler put it there from state.
            let text = v.get("text").and_then(|t| t.as_str()).unwrap_or("");
            let cur = state.read().clone();
            let doc_len = cur.doc.len();
            let from = s_off.min(doc_len);
            let to = e_off.min(doc_len).max(from);
            tracing::debug!(from, to, inserted_len = text.len(), "editor.before_input.insert");
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
        "before-input-delete-backward" => {
            // Backspace. Same author-the-edit strategy.
            let cur = state.read().clone();
            let doc_len = cur.doc.len();
            let from = s_off.min(doc_len);
            let to = e_off.min(doc_len).max(from);
            let (del_from, del_to) = if from == to {
                if from == 0 {
                    return;
                }
                // Step back by one UTF-8 char boundary. For
                // multi-byte (e.g., emoji), this stays valid.
                let doc_str = cur.doc.to_string();
                let mut back = from - 1;
                while back > 0 && !doc_str.is_char_boundary(back) {
                    back -= 1;
                }
                (back, from)
            } else {
                (from, to)
            };
            tracing::debug!(
                del_from,
                del_to,
                "editor.before_input.delete_backward"
            );
            let changes = Changes::delete(del_from..del_to);
            let new_sel = Selection::single(Range::caret(del_from));
            state.set(cur.update(
                TransactionSpec::new()
                    .changes(changes)
                    .selection(new_sel)
                    .annotate("origin", "before-input"),
            ));
        }
        "before-input-delete-forward" => {
            // Delete (forward).
            let cur = state.read().clone();
            let doc_len = cur.doc.len();
            let from = s_off.min(doc_len);
            let to = e_off.min(doc_len).max(from);
            let (del_from, del_to) = if from == to {
                if from >= doc_len {
                    return;
                }
                let doc_str = cur.doc.to_string();
                let mut fwd = from + 1;
                while fwd < doc_len && !doc_str.is_char_boundary(fwd) {
                    fwd += 1;
                }
                (from, fwd)
            } else {
                (from, to)
            };
            tracing::debug!(
                del_from,
                del_to,
                "editor.before_input.delete_forward"
            );
            let changes = Changes::delete(del_from..del_to);
            let new_sel = Selection::single(Range::caret(del_from));
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
        "insert-bracket" => {
            let text = v.get("text").and_then(|t| t.as_str()).unwrap_or("");
            let cur_clone = {
                let cur = state.read().clone();
                let doc_len = cur.doc.len();
                let from = s_off.min(doc_len);
                let to = e_off.min(doc_len).max(from);
                EditorState {
                    selection: Selection::single(Range::new(from, to)),
                    ..cur
                }
            };
            if let Some(spec) = editor_state::commands::insert_bracket(&cur_clone, text) {
                state.set(cur_clone.update(spec.annotate("origin", "insert-bracket")));
            } else {
                let p = cur_clone.selection.primary();
                let changes = editor_state::Changes::replace(p.from()..p.to(), text);
                let caret = p.from() + text.len();
                state.set(cur_clone.update(
                    editor_state::TransactionSpec::new()
                        .changes(changes)
                        .selection(editor_state::Selection::caret(caret))
                        .annotate("origin", "insert-bracket-plain"),
                ));
            }
        }
        "enter-continue-list" => {
            // The JS bridge intercepts Enter at the beforeinput
            // stage so the browser doesn't insert its own DOM
            // change. We re-apply the keymap's
            // `enter_continue_list` here against the selection
            // the browser saw at that moment — using
            // `state.selection` directly would race against the
            // in-flight `sel` message from the click that
            // preceded Enter.
            let cur_clone = {
                let cur = state.read().clone();
                let doc_len = cur.doc.len();
                let from = s_off.min(doc_len);
                let to = e_off.min(doc_len).max(from);
                EditorState {
                    selection: Selection::single(Range::new(from, to)),
                    ..cur
                }
            };
            if let Some(spec) = editor_state::commands::enter_continue_list(&cur_clone) {
                state.set(cur_clone.update(spec.annotate("origin", "enter")));
            }
        }
        "copy-range" => {
            // Code-block header copy button. We don't have a
            // clipboard API hooked up Rust-side yet, so bounce
            // the slice back to JS for `navigator.clipboard.writeText`.
            let from = v.get("from").and_then(|p| p.as_u64()).unwrap_or(0) as usize;
            let to = v.get("to").and_then(|p| p.as_u64()).unwrap_or(0) as usize;
            let cur = state.read();
            let doc = cur.doc.to_string();
            let from = from.min(doc.len());
            let to = to.min(doc.len()).max(from);
            let slice = doc[from..to].to_string();
            let escaped = slice.replace('\\', "\\\\").replace('`', "\\`");
            let script = format!(
                r#"navigator.clipboard && navigator.clipboard.writeText(`{escaped}`);"#
            );
            let _ = document::eval(&script);
        }
        "task-toggle" => {
            // Click on a task checkbox widget. `pos` is the byte
            // offset of the line start. Flip the `[ ]` ↔ `[x]`
            // bracket two bytes in.
            let pos = v.get("pos").and_then(|p| p.as_u64()).unwrap_or(0) as usize;
            let cur = state.read().clone();
            let doc = cur.doc.to_string();
            let bracket_idx = pos + 3; // `- [` → bracket at +3
            let next_char = doc.as_bytes().get(bracket_idx).copied();
            let new_char = match next_char {
                Some(b' ') => "x",
                Some(b'x') | Some(b'X') => " ",
                _ => return,
            };
            let changes = Changes::replace(bracket_idx..bracket_idx + 1, new_char);
            // Explicit caret so a queued `sel` message from the
            // browser's widget-click selection can't leave the
            // user staring at a wide highlight after the toggle.
            let new_sel = Selection::caret(bracket_idx + 1);
            state.set(cur.update(
                TransactionSpec::new()
                    .changes(changes)
                    .selection(new_sel)
                    .annotate("origin", "task-toggle"),
            ));
        }
        "link-clicked" => {
            // User clicked a link/wikilink. The browser placed
            // the caret inside the link body as part of the
            // click; that would re-reveal the link's markers
            // via `cursor_touches`. Drop selection to caret(0)
            // so live-preview applies cleanly.
            let cur = state.read().clone();
            state.set(cur.update(
                TransactionSpec::new()
                    .selection(Selection::caret(0))
                    .annotate("origin", "link-clicked"),
            ));
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
fn push_selection(
    state: &mut Signal<EditorState>,
    cur: &EditorState,
    deco_source: Option<DecorationSource>,
    s: usize,
    e: usize,
) {
    let doc_len = cur.doc.len();
    let mut s = s.min(doc_len);
    let mut e = e.min(doc_len);
    // Atomic-range snap: if either endpoint lands strictly
    // inside an atomic decoration, jump it to the nearer edge.
    // Mirrors CM6's `skipAtomicRanges` (`view/src/cursor.ts`).
    if let Some(src) = deco_source {
        let decs = src(cur);
        s = editor_state::decoration::skip_atomic(&decs, s);
        e = editor_state::decoration::skip_atomic(&decs, e);
    }
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
