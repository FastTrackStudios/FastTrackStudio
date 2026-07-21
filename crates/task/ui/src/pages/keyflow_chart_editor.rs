//! Keyflow **chart editor** pane for the setlist experience.
//!
//! Mounts the shared `editor::Editor` with keyflow syntax highlighting
//! (`editor_keyflow_lang::keyflow_decorations`) seeded from the CURRENT
//! song's chart text — the same source `SessionChartPane` renders on the
//! left. Charts-as-code: edit the keyflow on the right, read the engraved
//! chart on the left.
//!
//! v1 is a live-editable source view (highlighted). Wiring edits back to
//! the note's `chart.kf` through the vault mutate path — and re-driving
//! the engraved preview from the buffer — is the follow-up (issue #30
//! item 4). Keyed on the song index by the caller, so switching songs
//! remounts it with the new chart.

use dioxus::prelude::*;
use editor_keyflow_lang::{HighlightTheme, highlight_css, keyflow_decorations};
use session_ui::{ACTIVE_INDICES, SETLIST_STRUCTURE, SONG_CHARTS};

/// Resolve the current song's chart text — mirrors `SessionChartPane`'s
/// resolver (`SONG_CHARTS[project_guid]` then the song's own `chart_text`).
/// `read` on the structural signals so the seeding effect re-runs when the
/// setlist hydrates the charts (they arrive an async beat after mount); the
/// song index is `peek` (the caller keys this component on it), so a
/// per-tick `ACTIVE_INDICES` change doesn't re-fire the seed.
fn current_chart_text() -> String {
    let idx = ACTIVE_INDICES.peek().song_index.unwrap_or(0);
    let setlist = SETLIST_STRUCTURE.read();
    setlist
        .songs
        .get(idx)
        .and_then(|song| {
            SONG_CHARTS
                .read()
                .get(&song.project_guid)
                .map(|c| c.chart_text.clone())
                .or_else(|| song.chart_text.clone())
        })
        .unwrap_or_default()
}

#[component]
pub fn KeyflowChartEditor() -> Element {
    let mut state = use_signal(|| editor::EditorState::new(String::new()));
    let mut seeded = use_signal(|| false);
    let keymap = use_hook(editor::standard_markdown_keymap);
    // Per-token `.kf-*` color rules — injected once for this pane.
    let css = use_hook(|| highlight_css(&HighlightTheme::default_dark()));

    // Seed the buffer once the chart text is available (it hydrates just
    // after mount). Re-runs on SETLIST_STRUCTURE / SONG_CHARTS changes,
    // seeds exactly once.
    use_effect(move || {
        if *seeded.peek() {
            return;
        }
        let text = current_chart_text();
        if !text.trim().is_empty() {
            state.set(editor::EditorState::new(text));
            seeded.set(true);
        }
    });

    let empty = state.read().doc.to_string().trim().is_empty();

    rsx! {
        document::Style { {css} }
        div { class: "flex h-full min-h-0 flex-col",
            div { class: "flex shrink-0 items-center gap-2 border-b border-border px-3 py-1.5",
                span { class: "text-xs font-semibold text-foreground", "Keyflow source" }
                span { class: "text-[11px] text-muted-foreground", "charts as code" }
            }
            if empty {
                div { class: "p-4 text-sm text-muted-foreground",
                    "This song has no chart yet."
                }
            } else {
                // `editor-app props-collapsed` reuses the note editor's chrome
                // (the frontmatter widget is irrelevant here and stays hidden).
                div { class: "editor-app props-collapsed min-h-0 flex-1 overflow-auto",
                    div { class: "editor-frame editor-frame--flush",
                        editor::Editor {
                            state,
                            keymap,
                            decorations: editor::editor_view::DecorationSource::ptr(keyflow_decorations),
                        }
                    }
                }
            }
        }
    }
}
