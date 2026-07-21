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
use session_proto::SongChartHydration;
use session_ui::SONG_CHARTS;

/// Debounce before pushing an edit into the live chart (re-engrave is
/// heavy; a newer keystroke supersedes a pending push).
const RERENDER_DEBOUNCE_MS: u32 = 250;

/// Keyflow chart source editor for one song. `source` is the song's
/// original chart text; `guid` is its `project_guid` (the `SONG_CHARTS`
/// key the engraved chart reads). The caller keys this component so it
/// remounts — and re-seeds — when the song changes or the chart hydrates.
///
/// Edits push the buffer into `SONG_CHARTS[guid]` (debounced), so the
/// engraved chart on the left re-renders live. `source` deliberately reads
/// the song's own `chart_text` (not `SONG_CHARTS`), so this push can't loop
/// back into a re-seed.
#[component]
pub fn KeyflowChartEditor(source: String, guid: String) -> Element {
    let mut state = use_signal(|| editor::EditorState::new(source.clone()));
    // Re-seed only on an actual `source` change (song switch / hydration) —
    // buffer edits don't change `source`, so they're preserved.
    use_effect(use_reactive!(|source| {
        state.set(editor::EditorState::new(source));
    }));

    // Live re-render: push the buffer into SONG_CHARTS[guid] (debounced) so
    // the engraver on the left follows edits.
    let mut render_gen = use_signal(|| 0u64);
    let guid_live = guid.clone();
    use_effect(move || {
        let text = state.read().doc.to_string();
        let guid = guid_live.clone();
        let my_render_gen = render_gen.peek().wrapping_add(1);
        render_gen.set(my_render_gen);
        spawn(async move {
            #[cfg(target_arch = "wasm32")]
            gloo_timers::future::TimeoutFuture::new(RERENDER_DEBOUNCE_MS).await;
            if *render_gen.peek() != my_render_gen {
                return; // superseded by a newer edit
            }
            let mut charts = SONG_CHARTS.write();
            charts
                .entry(guid)
                .and_modify(|c| c.chart_text = text.clone())
                .or_insert_with(|| SongChartHydration {
                    project_guid: String::new(),
                    chart_text: text,
                    detected_chords: Vec::new(),
                    chart_fingerprint: String::new(),
                });
        });
    });

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
