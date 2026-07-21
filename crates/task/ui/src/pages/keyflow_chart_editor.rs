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

/// Keyflow chart source editor for one song. `source` is the resolved
/// chart text; the caller (SetlistBody) keys this component so it remounts
/// — and re-seeds — when the song changes *or* when the chart finishes
/// hydrating (empty → present).
#[component]
pub fn KeyflowChartEditor(source: String) -> Element {
    let mut state = use_signal(|| editor::EditorState::new(source.clone()));
    // Re-seed whenever the resolved chart changes — a song switch or the
    // charts hydrating (empty → present). `use_reactive` fires only on an
    // actual `source` change, so editing the buffer (which doesn't change
    // the setlist's stored chart) is preserved. This is what keeps the
    // editor current with the selected song regardless of remount timing.
    use_effect(use_reactive!(|source| {
        state.set(editor::EditorState::new(source));
    }));
    let keymap = use_hook(editor::standard_markdown_keymap);
    // Per-token `.kf-*` color rules — injected once for this pane.
    let css = use_hook(|| highlight_css(&HighlightTheme::default_dark()));

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
