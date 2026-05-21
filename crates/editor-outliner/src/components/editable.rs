//! Block-edit textarea. Controlled component — `value` is bound
//! to the in-memory block content; oninput pushes every keystroke
//! straight through `update_block_content`. No CRDT, no async
//! eval — the Dioxus controlled-textarea pattern is the whole
//! story. Mirrors `frontend.components.editor.box`.
//!
//! The slash-command popup is wired here too: typing `/` opens
//! the menu via `AppState.slash`; subsequent characters narrow
//! the filter; Arrow keys + Enter / click pick a command; Esc
//! closes.

use dioxus::prelude::*;
use uuid::Uuid;

use crate::components::{SlashMenu, slash_move, slash_run};
use crate::handler::{
    FormatResult, cycle_task_marker, delete_block, exit_edit, format_link, format_text,
    indent_block, next_block_in_outline, outdent_block, prev_block_in_outline, split_block,
    update_block_content,
};
use crate::state::{AppState, SlashState};

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

    // Newline count + 1 — `str::lines()` ignores a trailing
    // newline, so for "abc\n" it returns 1 even though the cursor
    // sits on a visually-second row. That makes a Shift-Enter
    // freshly-pressed line invisible. Count newline chars directly
    // so the textarea grows the moment the user hits Shift-Enter.
    let rows = (value.bytes().filter(|c| *c == b'\n').count() + 1).clamp(1, 40) as i64;

    // oninput pushes content through and (re)computes whether the
    // slash menu should be open + what its filter query is. Logseq
    // uses a regex against the prefix-up-to-caret; we approximate
    // by looking for the *last* `/` followed by no whitespace.
    let on_input = move |e: Event<FormData>| {
        let v = e.value();
        update_block_content(state, block_id, v.clone());
        let open = detect_slash(&v);
        let mut sig = state.slash;
        let next = open.map(|(slash_start, query)| {
            let prev_selected = sig.peek().as_ref().map(|s| s.selected).unwrap_or(0);
            SlashState {
                block_id,
                query,
                slash_start,
                selected: prev_selected,
            }
        });
        sig.set(next);
    };

    let on_blur = move |_: Event<FocusData>| {
        // Closing the slash menu on blur lets the user click into
        // the menu without it disappearing first (the menu's
        // onmousedown preventDefault keeps focus on the textarea
        // until the click resolves).
        state.slash.clone().set(None);
        exit_edit(state);
    };

    let on_keydown = move |e: Event<KeyboardData>| {
        let key = e.key();
        let mods = e.modifiers();
        let slash_open = state.slash.read().is_some();
        match key {
            // While the slash menu is open, ArrowUp/Down moves the
            // selection and Enter runs it. Escape closes the menu.
            Key::ArrowDown if slash_open => {
                e.prevent_default();
                slash_move(state, 1);
            }
            Key::ArrowUp if slash_open => {
                e.prevent_default();
                slash_move(state, -1);
            }
            Key::Enter if slash_open => {
                e.prevent_default();
                slash_run(state);
            }
            Key::Escape if slash_open => {
                e.prevent_default();
                state.slash.clone().set(None);
            }
            // Cmd/Ctrl + Enter — cycle the task marker
            // (none → TODO → DOING → DONE → none). Mirrors
            // Logseq's `cycle-todo!`. Must precede the plain
            // Enter case so the modifier wins.
            Key::Enter if (mods.meta() || mods.ctrl()) && !mods.shift() => {
                e.prevent_default();
                cycle_task_marker(state, block_id);
            }
            // Shift + Enter — insert a soft-break `\n` into the
            // current block at the cursor (Logseq's
            // `keydown-new-line-handler`, which `preventDefault`s
            // and calls `(insert "\n")` — see
            // `frontend.modules.shortcut.config :editor/new-line`).
            // We do the same explicitly so behavior matches across
            // renderers instead of relying on the browser to insert
            // a literal newline.
            Key::Enter if mods.shift() && !mods.meta() && !mods.ctrl() => {
                e.prevent_default();
                let id = block_id.simple().to_string();
                insert_at_cursor(&id, "\n");
            }
            // Enter: split the block at the caret. Reads the
            // live textarea selection + value so the split lands
            // exactly where the cursor is (Logseq's behavior).
            Key::Enter if !mods.shift() && !mods.meta() && !mods.ctrl() => {
                e.prevent_default();
                let id = block_id.simple().to_string();
                dioxus::core::spawn_forever(async move {
                    let Some((value, s, _en)) = read_textarea(&id).await else {
                        return;
                    };
                    if let Some(new_id) = split_block(state, block_id, s, &value) {
                        state.editing_block.clone().set(Some(new_id));
                        // New block starts in edit mode with the
                        // suffix as its content — caret at start.
                        let new_dom = new_id.simple().to_string();
                        park_caret(&new_dom, 0);
                    }
                });
            }
            // Tab: indent → child of previous sibling.
            // Shift-Tab: outdent → up one level.
            Key::Tab => {
                e.prevent_default();
                if mods.shift() {
                    outdent_block(state, block_id);
                } else {
                    indent_block(state, block_id);
                }
            }
            // Backspace: we always handle it ourselves so we can
            // do the merge-with-prev-block case (Logseq's
            // `delete-block-when-zero-pos!`). The browser never
            // sees the event.
            //
            // Three sub-cases, decided after we read the live
            // textarea selection asynchronously:
            //   1. Cursor at 0,0 → merge current block into
            //      previous (concat content, delete current,
            //      caret at the join point). No-op if no prev.
            //   2. Non-empty selection → delete the range.
            //   3. Cursor mid-text → delete one char before
            //      cursor.
            Key::Backspace => {
                e.prevent_default();
                let id = block_id.simple().to_string();
                dioxus::core::spawn_forever(async move {
                    let Some((s, en)) = read_selection(&id).await else {
                        return;
                    };
                    if s == 0 && en == 0 {
                        merge_with_prev(state, block_id);
                    } else {
                        delete_in_textarea(&id, s, en);
                    }
                });
            }
            // ArrowUp / ArrowDown — at block boundaries, jump to
            // the adjacent block. For single-line blocks the
            // boundary is always the whole block, so we handle
            // those synchronously; multi-line blocks fall through
            // to the browser's normal in-textarea arrow motion.
            // Logseq does the same: only crosses block boundaries
            // when the caret is on the first / last visual row.
            Key::ArrowUp | Key::ArrowDown if !mods.meta() && !mods.ctrl() && !mods.alt() => {
                let v = state.vault.read();
                let content = v
                    .blocks
                    .iter()
                    .find(|b| b.id == block_id)
                    .map(|b| b.content.clone())
                    .unwrap_or_default();
                let is_up = matches!(&key, Key::ArrowUp);
                let target = if is_up {
                    prev_block_in_outline(&v, block_id)
                } else {
                    next_block_in_outline(&v, block_id)
                };
                let target_len = target.and_then(|id| {
                    v.blocks
                        .iter()
                        .find(|b| b.id == id)
                        .map(|b| b.content.len())
                });
                drop(v);
                // Multi-line blocks: let the browser handle the
                // intra-block arrow first; a later improvement
                // will detect first/last row.
                if content.contains('\n') {
                    // fall through
                } else if let (Some(target_id), Some(tlen)) = (target, target_len) {
                    e.prevent_default();
                    state.editing_block.clone().set(Some(target_id));
                    let dom = target_id.simple().to_string();
                    let pos = if is_up { tlen } else { 0 };
                    park_caret(&dom, pos);
                }
            }
            // Cmd/Ctrl + Shift + ArrowUp/Down — move the current
            // block up or down within its sibling group. Logseq's
            // "move-up!" / "move-down!" outliner ops.
            Key::ArrowUp | Key::ArrowDown if (mods.meta() || mods.ctrl()) && mods.shift() => {
                e.prevent_default();
                if matches!(&key, Key::ArrowUp) {
                    crate::handler::move_block_up(state, block_id);
                } else {
                    crate::handler::move_block_down(state, block_id);
                }
            }
            // Cmd/Ctrl + B / I / E / K — inline format shortcuts.
            // Ports Logseq's format-text! algorithm. The whole
            // round trip (read selection → compute → write value
            // → set selection) runs in one synchronous JS eval so
            // the textarea can't unmount or shift focus mid-flight.
            Key::Character(ref c)
                if (mods.meta() || mods.ctrl())
                    && !mods.shift()
                    && matches!(c.as_str(), "b" | "B" | "i" | "I" | "e" | "E" | "k" | "K") =>
            {
                e.prevent_default();
                let kind = match c.as_str() {
                    "b" | "B" => FormatKind::Wrap("**"),
                    "i" | "I" => FormatKind::Wrap("*"),
                    "e" | "E" => FormatKind::Wrap("`"),
                    _ => FormatKind::Link,
                };
                let id = block_id.simple().to_string();
                apply_format_sync(&id, kind);
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

    let dom_id = block_id.simple().to_string();
    rsx! {
        div { class: "editor-edit-wrap", "data-edit-block": "{dom_id}",
            textarea {
                class: "editor-textarea",
                rows: "{rows}",
                value: "{value}",
                oninput: on_input,
                onkeydown: on_keydown,
                onblur: on_blur,
                onmounted: on_mount,
            }
            SlashMenu {}
        }
    }
}

/// Read the current `value` + `(selectionStart, selectionEnd)`
/// of the textarea hosting block `id` in one shot — useful when
/// the action depends on both (Enter-split, etc.) and we want an
/// atomic snapshot rather than two round-trips that could race.
async fn read_textarea(id: &str) -> Option<(String, usize, usize)> {
    // `dioxus.send(...)` is the only way JS hands a value back to
    // an awaiting Rust task in Dioxus 0.7 — the script's return
    // value is *not* picked up by `.recv()`. `.await` on the Eval
    // handle would call `.join()` which can also surface it but
    // requires the JS task to actually finish; the explicit
    // `dioxus.send` keeps the channel open and is what every
    // Dioxus example uses.
    let script = format!(
        r#"
        (function() {{
            const wrap = document.querySelector('[data-edit-block="{id}"]');
            if (!wrap) {{ dioxus.send(null); return; }}
            const ta = wrap.querySelector('textarea');
            if (!ta) {{ dioxus.send(null); return; }}
            dioxus.send([ta.value, ta.selectionStart || 0, ta.selectionEnd || 0]);
        }})();
        "#
    );
    let mut handle = document::eval(&script);
    match handle.recv::<serde_json::Value>().await {
        Ok(serde_json::Value::Array(a)) if a.len() == 3 => Some((
            a[0].as_str()?.to_string(),
            a[1].as_u64()? as usize,
            a[2].as_u64()? as usize,
        )),
        _ => None,
    }
}

/// Read the current `selectionStart` / `selectionEnd` of the
/// textarea hosting block `id`. Returns `None` when the textarea
/// isn't in the DOM (an unmount race) so callers can fall back
/// to end-of-content.
async fn read_selection(id: &str) -> Option<(usize, usize)> {
    let script = format!(
        r#"
        (function() {{
            const wrap = document.querySelector('[data-edit-block="{id}"]');
            if (!wrap) {{ dioxus.send(null); return; }}
            const ta = wrap.querySelector('textarea');
            if (!ta) {{ dioxus.send(null); return; }}
            dioxus.send([ta.selectionStart || 0, ta.selectionEnd || 0]);
        }})();
        "#
    );
    let mut handle = document::eval(&script);
    match handle.recv::<serde_json::Value>().await {
        Ok(serde_json::Value::Array(a)) if a.len() == 2 => {
            Some((a[0].as_u64()? as usize, a[1].as_u64()? as usize))
        }
        _ => None,
    }
}

/// Merge the current block into the previous-in-outline block:
/// append current content to prev, delete current, focus prev
/// with caret parked at the join point (= length of prev's old
/// content). No-op when there's no previous block (top of page).
///
/// Mirrors Logseq's `delete-block-inner!` "concat-prev-block?"
/// branch + `move-to-prev-block`.
fn merge_with_prev(state: AppState, block_id: Uuid) {
    let v = state.vault.read();
    let cur_content = match v.blocks.iter().find(|b| b.id == block_id) {
        Some(b) => b.content.clone(),
        None => return,
    };
    let Some(prev_id) = prev_block_in_outline(&v, block_id) else {
        return;
    };
    let prev_content = match v.blocks.iter().find(|b| b.id == prev_id) {
        Some(b) => b.content.clone(),
        None => return,
    };
    let join_pos = prev_content.len();
    let new_prev_content = format!("{prev_content}{cur_content}");
    drop(v);

    update_block_content(state, prev_id, new_prev_content);
    delete_block(state, block_id);
    state.editing_block.clone().set(Some(prev_id));
    let prev_dom = prev_id.simple().to_string();
    park_caret(&prev_dom, join_pos);
}

/// Insert `text` at the textarea's current cursor position (or
/// replace the selection if there is one) and place the caret
/// just after the inserted text. Mirrors Logseq's
/// `frontend.handler.editor/insert` — value mutated via the
/// native setter, native `input` event fired so Dioxus syncs.
fn insert_at_cursor(id: &str, text: &str) {
    let text_json = serde_json::to_string(text).unwrap_or_else(|_| "\"\"".to_string());
    let script = format!(
        r#"
        (function() {{
            const wrap = document.querySelector('[data-edit-block="{id}"]');
            if (!wrap) return;
            const ta = wrap.querySelector('textarea');
            if (!ta) return;
            const v = ta.value;
            const s = ta.selectionStart || 0;
            const e = ta.selectionEnd || 0;
            const t = {text_json};
            const newValue = v.slice(0, s) + t + v.slice(e);
            const setter = Object.getOwnPropertyDescriptor(
                window.HTMLTextAreaElement.prototype, 'value'
            ).set;
            setter.call(ta, newValue);
            ta.dispatchEvent(new Event('input', {{ bubbles: true }}));
            ta.focus();
            const caret = s + t.length;
            try {{ ta.setSelectionRange(caret, caret); }} catch (_) {{}}
        }})();
        "#
    );
    let _ = document::eval(&script);
}

/// Delete a span (or one char before cursor when `s == e`) from
/// the textarea's value and dispatch a native `input` event so
/// Dioxus oninput syncs the signal. Mirrors the synchronous
/// pattern used by `apply_format_sync`.
fn delete_in_textarea(id: &str, s: usize, e: usize) {
    let script = format!(
        r#"
        (function() {{
            const wrap = document.querySelector('[data-edit-block="{id}"]');
            if (!wrap) return;
            const ta = wrap.querySelector('textarea');
            if (!ta) return;
            const v = ta.value;
            let start, end;
            if ({s} === {e}) {{
                if ({s} === 0) return;
                start = {s} - 1; end = {s};
            }} else {{
                start = {s}; end = {e};
            }}
            const newValue = v.slice(0, start) + v.slice(end);
            const setter = Object.getOwnPropertyDescriptor(
                window.HTMLTextAreaElement.prototype, 'value'
            ).set;
            setter.call(ta, newValue);
            ta.dispatchEvent(new Event('input', {{ bubbles: true }}));
            ta.focus();
            try {{ ta.setSelectionRange(start, start); }} catch (_) {{}}
        }})();
        "#
    );
    let _ = document::eval(&script);
}

/// Focus the textarea hosting block `id` and park the caret at
/// `pos`. Polls a few frames because the textarea is mounted by
/// the next Dioxus render — it's not in the DOM at the moment
/// this is called (the block above us just switched from static
/// render to edit mode).
fn park_caret(id: &str, pos: usize) {
    let script = format!(
        r#"
        (function() {{
            let tries = 0;
            function tick() {{
                const wrap = document.querySelector('[data-edit-block="{id}"]');
                const ta = wrap && wrap.querySelector('textarea');
                if (ta) {{
                    ta.focus();
                    try {{ ta.setSelectionRange({pos}, {pos}); }} catch (_) {{}}
                    return;
                }}
                if (tries++ > 30) return;
                requestAnimationFrame(tick);
            }}
            requestAnimationFrame(tick);
        }})();
        "#
    );
    let _ = document::eval(&script);
}

#[derive(Clone, Copy)]
enum FormatKind {
    /// Symmetric wrap (`**`, `*`, `` ` ``). The whole format-text!
    /// dispatch lives in JS so the read+write is atomic.
    Wrap(&'static str),
    /// Cmd-K: insert `[]()` or `[sel]()` with caret in the URL.
    Link,
}

/// Synchronous Logseq-style format apply. One JS eval reads the
/// textarea selection, computes the new value + selection per
/// `format-text!`, mutates the DOM, fires a native `input` event
/// so Dioxus oninput syncs the signal, then parks the caret.
///
/// No async hop, no Rust round-trip → no race window in which the
/// textarea can re-render away under us. Mirrors Logseq's
/// `frontend.util/set-change-value` + `cursor/set-selection-to`.
fn apply_format_sync(id: &str, kind: FormatKind) {
    let (pattern_json, is_link) = match kind {
        FormatKind::Wrap(p) => (
            serde_json::to_string(p).unwrap_or_else(|_| "\"\"".to_string()),
            false,
        ),
        FormatKind::Link => ("null".to_string(), true),
    };
    let is_link_js = if is_link { "true" } else { "false" };
    let script = format!(
        r#"
        (function() {{
            const wrap = document.querySelector('[data-edit-block="{id}"]');
            if (!wrap) return;
            const ta = wrap.querySelector('textarea');
            if (!ta) return;
            const value = ta.value;
            const s = ta.selectionStart || 0;
            const e = ta.selectionEnd || 0;
            const pattern = {pattern_json};
            const isLink = {is_link_js};
            let newValue, selStart, selEnd;
            if (isLink) {{
                if (s === e) {{
                    newValue = value.slice(0, s) + "[]()" + value.slice(s);
                    selStart = s + 1; selEnd = s + 1;
                }} else {{
                    const sel = value.slice(s, e);
                    newValue = value.slice(0, s) + "[" + sel + "]()" + value.slice(e);
                    const pos = s + 1 + sel.length + 2; // after `[sel](`
                    selStart = pos; selEnd = pos;
                }}
            }} else {{
                const pc = pattern.length;
                const prefixStart = Math.max(0, s - pc);
                const patternPrefix = value.slice(prefixStart, s);
                const suffixEnd = Math.min(value.length, e + pc);
                const patternSuffix = value.slice(e, suffixEnd);
                const alreadyWrapped = pc > 0
                    && patternPrefix === pattern
                    && patternSuffix === pattern;
                if (alreadyWrapped) {{
                    newValue = value.slice(0, s - pc) + value.slice(s, e) + value.slice(e + pc);
                    selStart = s - pc; selEnd = e - pc;
                }} else if (s === e) {{
                    newValue = value.slice(0, s) + pattern + pattern + value.slice(s);
                    selStart = s + pc; selEnd = s + pc;
                }} else {{
                    newValue = value.slice(0, s) + pattern + value.slice(s, e) + pattern + value.slice(e);
                    selStart = e + pc; selEnd = e + pc;
                }}
            }}
            // Native setter so Dioxus's oninput listener picks it up.
            const setter = Object.getOwnPropertyDescriptor(
                window.HTMLTextAreaElement.prototype, 'value'
            ).set;
            setter.call(ta, newValue);
            ta.dispatchEvent(new Event('input', {{ bubbles: true }}));
            ta.focus();
            try {{ ta.setSelectionRange(selStart, selEnd); }} catch (_) {{}}
        }})();
        "#
    );
    let _ = document::eval(&script);
}

#[allow(dead_code)]
fn apply_format(id: &str, new_value: &str, start: usize, end: usize) {
    let value_json = serde_json::to_string(new_value).unwrap_or_else(|_| "\"\"".to_string());
    let script = format!(
        r#"
        (function() {{
            const wrap = document.querySelector('[data-edit-block="{id}"]');
            if (!wrap) return;
            const ta = wrap.querySelector('textarea');
            if (!ta) return;
            const v = {value_json};
            // React-style native setter so the framework picks up
            // the change via the dispatched `input` event.
            const setter = Object.getOwnPropertyDescriptor(
                window.HTMLTextAreaElement.prototype, 'value'
            ).set;
            setter.call(ta, v);
            ta.dispatchEvent(new Event('input', {{ bubbles: true }}));
            ta.focus();
            try {{ ta.setSelectionRange({start}, {end}); }} catch (_) {{}}
        }})();
        "#
    );
    let _ = document::eval(&script);
}

#[allow(dead_code)]
fn set_selection(id: &str, expected: &str, start: usize, end: usize) {
    // JSON-encode the expected string so embedded quotes / newlines
    // survive the script template.
    let expected_json = serde_json::to_string(expected).unwrap_or_else(|_| "\"\"".to_string());
    let script = format!(
        r#"
        (function() {{
            const expected = {expected_json};
            let tries = 0;
            function tick() {{
                const wrap = document.querySelector('[data-edit-block="{id}"]');
                if (!wrap) return;
                const ta = wrap.querySelector('textarea');
                if (!ta) return;
                if (ta.value === expected || tries > 12) {{
                    ta.focus();
                    try {{ ta.setSelectionRange({start}, {end}); }} catch (_) {{}}
                    return;
                }}
                tries++;
                requestAnimationFrame(tick);
            }}
            requestAnimationFrame(tick);
        }})();
        "#
    );
    let _ = document::eval(&script);
}

/// Find the position + body of an open slash-command pattern in
/// the current textarea value. Returns `Some((slash_start, query))`
/// when there's a `/` whose body (up to the end of the string)
/// contains no whitespace and no closing `/`. Returns None when
/// the menu shouldn't be open.
fn detect_slash(value: &str) -> Option<(usize, String)> {
    // Walk backward from the end looking for the last `/`. If we
    // hit whitespace first, no menu.
    let bytes = value.as_bytes();
    let mut i = value.len();
    while i > 0 {
        let c = bytes[i - 1];
        if c == b'/' {
            // The `/` must be at the start of the string or
            // preceded by whitespace — otherwise typing `https://`
            // would open the menu.
            let preceded_by_word = i >= 2 && !(bytes[i - 2] as char).is_whitespace();
            if preceded_by_word {
                return None;
            }
            let query = value[i..].to_string();
            return Some((i - 1, query));
        }
        // Newlines, tabs, regular whitespace close the menu.
        if (c as char).is_whitespace() {
            return None;
        }
        i -= 1;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detect_slash_at_start() {
        assert_eq!(detect_slash("/he"), Some((0, "he".to_string())));
    }

    #[test]
    fn detect_slash_after_space() {
        assert_eq!(detect_slash("hello /to"), Some((6, "to".to_string())));
    }

    #[test]
    fn detect_slash_inside_url_ignored() {
        assert_eq!(detect_slash("https://example.com"), None);
    }

    #[test]
    fn detect_slash_closed_by_space() {
        assert_eq!(detect_slash("/foo bar"), None);
    }

    #[test]
    fn detect_slash_no_slash() {
        assert_eq!(detect_slash("hello"), None);
    }
}
