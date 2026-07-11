//! Slash-command popup. Mirrors Logseq's `commands-popup`:
//! filters the static command catalog by the substring after
//! `/`, lets the user navigate with arrow keys, runs the
//! selected command on Enter / click.
//!
//! The popup is mounted next to the editing textarea (positioned
//! by CSS) and reads its open state from
//! [`crate::state::AppState::slash`]. The textarea's keydown
//! handler routes ArrowUp / ArrowDown / Enter / Escape into the
//! menu when it's open instead of the usual block actions.

use dioxus::prelude::*;

use crate::handler::{CommandEntry, filter_commands, run_command};
use crate::state::{AppState, SlashState};

#[component]
pub fn SlashMenu() -> Element {
    let state = use_context::<AppState>();
    let slash = state.slash.read().clone();
    let Some(slash) = slash else {
        return rsx! { Fragment {} };
    };
    let hits = filter_commands(&slash.query);
    if hits.is_empty() {
        return rsx! {
            div { class: "slash-menu",
                div { class: "slash-empty", "No commands match." }
            }
        };
    }
    let selected = slash.selected.min(hits.len().saturating_sub(1));
    // Render rows grouped by `group` so the menu has section
    // headers like "Basic / Format / Heading / Task / Date /
    // Macro" matching Logseq.
    let mut current_group: Option<&str> = None;
    let mut row_idx: usize = 0;
    rsx! {
        div { class: "slash-menu",
            for c in hits.iter().cloned() {
                {
                    let show_header = current_group.map(|g| g != c.group).unwrap_or(true);
                    current_group = Some(c.group);
                    let is_selected = row_idx == selected;
                    let idx_for_click = row_idx;
                    let slash_clone = slash.clone();
                    let entry = c.clone();
                    row_idx += 1;
                    rsx! {
                        {
                            if show_header {
                                rsx! { div { class: "slash-group", "{entry.group}" } }
                            } else { rsx! {} }
                        }
                        div {
                            class: if is_selected { "slash-row selected" } else { "slash-row" },
                            onmousedown: move |e: Event<MouseData>| e.prevent_default(),
                            onclick: move |_| {
                                pick_command(state, slash_clone.clone(), entry.clone());
                            },
                            key: "{idx_for_click}",
                            div { class: "slash-row-label", "{entry.label}" }
                            if !entry.desc.is_empty() {
                                div { class: "slash-row-desc", "{entry.desc}" }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Execute a picked command + close the menu. Caller is also
/// responsible for refocusing the textarea after the re-render.
pub fn pick_command(state: AppState, slash: SlashState, entry: CommandEntry) {
    // Slash range is `/query` — start at the `/`, end at the
    // current caret which equals `slash_start + 1 + query.len()`.
    let end = slash.slash_start + 1 + slash.query.len();
    let _ = run_command(state, slash.block_id, slash.slash_start..end, entry.kind);
    state.slash.clone().set(None);
}

/// Move selection up by one row (Arrow Up while menu is open).
pub fn move_selection(state: AppState, delta: isize) {
    let mut sig = state.slash;
    let Some(mut cur) = sig.peek().clone() else {
        return;
    };
    let hits = filter_commands(&cur.query);
    if hits.is_empty() {
        return;
    }
    let len = hits.len() as isize;
    let new = (cur.selected as isize + delta).rem_euclid(len) as usize;
    cur.selected = new;
    sig.set(Some(cur));
}

/// Run whatever's currently selected (Enter while menu is open).
pub fn run_selected(state: AppState) {
    let cur = state.slash.peek().clone();
    let Some(cur) = cur else {
        return;
    };
    let hits = filter_commands(&cur.query);
    let Some(entry) = hits.get(cur.selected).cloned() else {
        return;
    };
    pick_command(state, cur, entry);
}
