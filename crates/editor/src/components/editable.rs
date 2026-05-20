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
    FormatResult, delete_block, exit_edit, format_link, format_text, indent_block, outdent_block,
    split_block, update_block_content,
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

    let rows = value.lines().count().max(1).min(40) as i64;

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
            // Enter: split the block at the caret (v1 splits at
            // end-of-content; caret-aware split comes next).
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
            // Backspace on an empty block deletes it.
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
            // Cmd/Ctrl + B / I / E / K — inline format shortcuts.
            // Ports Logseq's format-text! algorithm: wrap the
            // current selection in the pattern, or unwrap if it's
            // already wrapped. Empty selection lands the caret
            // between the two markers ("****" → caret at offset 2).
            Key::Character(ref c)
                if (mods.meta() || mods.ctrl())
                    && !mods.shift()
                    && matches!(c.as_str(), "b" | "B" | "i" | "I" | "e" | "E" | "k" | "K") =>
            {
                e.prevent_default();
                let pattern = match c.as_str() {
                    "b" | "B" => Some("**"),
                    "i" | "I" => Some("*"),
                    "e" | "E" => Some("`"),
                    _ => None, // Cmd-K → link
                };
                let id = block_id.simple().to_string();
                let cur_value = state
                    .vault
                    .read()
                    .blocks
                    .iter()
                    .find(|b| b.id == block_id)
                    .map(|b| b.content.clone())
                    .unwrap_or_default();
                let is_link = pattern.is_none();
                dioxus::core::spawn_forever(async move {
                    let (s, en) = read_selection(&id)
                        .await
                        .unwrap_or((cur_value.len(), cur_value.len()));
                    let FormatResult {
                        new_value,
                        selection,
                    } = if is_link {
                        format_link(&cur_value, s, en)
                    } else {
                        format_text(&cur_value, s, en, pattern.unwrap())
                    };
                    update_block_content(state, block_id, new_value);
                    set_selection(&id, selection.0, selection.1);
                });
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
        div { class: "editor-edit-wrap",
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

/// Read the current `selectionStart` / `selectionEnd` of the
/// textarea hosting block `id`. Returns `None` when the textarea
/// isn't in the DOM (an unmount race) so callers can fall back
/// to end-of-content.
async fn read_selection(id: &str) -> Option<(usize, usize)> {
    let script = format!(
        r#"
        (function() {{
            const wrap = document.querySelector('[data-edit-block="{id}"]');
            if (!wrap) return null;
            const ta = wrap.querySelector('textarea');
            if (!ta) return null;
            return [ta.selectionStart || 0, ta.selectionEnd || 0];
        }})()
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

/// Park the textarea selection at `(start, end)`. Runs in a
/// `requestAnimationFrame` so it lands after Dioxus re-applies
/// the `value` attribute on the next render.
fn set_selection(id: &str, start: usize, end: usize) {
    let script = format!(
        r#"
        requestAnimationFrame(function() {{
            const wrap = document.querySelector('[data-edit-block="{id}"]');
            if (!wrap) return;
            const ta = wrap.querySelector('textarea');
            if (!ta) return;
            ta.focus();
            try {{ ta.setSelectionRange({start}, {end}); }} catch (_) {{}}
        }});
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
