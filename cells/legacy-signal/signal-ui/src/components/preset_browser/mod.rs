//! Full-screen preset browser dialog.
//!
//! Omnisphere-inspired multi-column browser with multiple browse modes,
//! faceted tag filtering, and multi-axis rating.

pub mod browser_state;
pub mod preview_panel;
pub mod result_item;
pub mod results_list;
pub mod search_panel;
pub mod tag_column;
pub mod tag_columns;
pub mod toolbar;

pub use self::preset_browser_dialog::PresetBrowserDialog;
pub use browser_state::BrowserState;
pub use preview_panel::PreviewPanel;

mod preset_browser_dialog {
    use crate::prelude::*;
    use crate::signals::{RIG_INFO, RIG_PRESET_BROWSER_OPEN};
    use signal_control::rating::PresetStatus;
    use signal_control::tags::browse::{BrowseLevel, BrowseMode, SortMode};
    use signal_control::tags::instrument_tags::registry_for_instrument;

    use super::browser_state::BrowserState;
    use super::preview_panel::PreviewPanel;
    use super::results_list::ResultsList;
    use super::search_panel::SearchPanel;
    use super::tag_columns::TagColumnView;
    use super::toolbar::BrowserToolbar;

    /// Full-screen preset browser dialog.
    ///
    /// Composes SearchPanel, TagColumnView, ResultsList, PreviewPanel, and
    /// BrowserToolbar into an Omnisphere-inspired overlay. Toggled by the
    /// `RIG_PRESET_BROWSER_OPEN` global signal.
    #[component]
    pub fn PresetBrowserDialog() -> Element {
        let mut state = use_signal(BrowserState::new);

        // Read instrument type from rig info for tag registry
        let instrument_name = RIG_INFO
            .read()
            .as_ref()
            .map(|info| info.instrument_type.to_string())
            .unwrap_or_else(|| "Guitar".to_string());

        let registry = RIG_INFO
            .read()
            .as_ref()
            .map(|info| registry_for_instrument(&info.instrument_type))
            .unwrap_or_else(|| {
                registry_for_instrument(&signal_control::rig::InstrumentType::Guitar)
            });

        let browse_mode = state.read().browse_mode;
        let browse_level = state.read().browse_level;
        let query = state.read().query.clone();
        let sort_mode = state.read().sort_mode;
        let status_filter = state.read().status_filter;
        let min_quality = state.read().min_quality;
        let selected_tags = state.read().selected_tags.clone();
        let selected_result_index = state.read().selected_result_index;
        let selected_scene_index = state.read().selected_scene_index;

        let browse_mode_name = browse_mode.display_name();
        let browse_level_name = browse_level.display_name();

        // Pre-extract all styles containing # hex colors (Dioxus RSX parser limitation)
        let overlay_style = "position: fixed; top: 0; left: 0; right: 0; bottom: 0; z-index: 9999; display: flex; flex-direction: column; background: #141414; color: #ddd; font-family: system-ui, sans-serif;";
        let header_style = "display: flex; align-items: center; justify-content: space-between; padding: 8px 16px; border-bottom: 1px solid #333; background: #1a1a1a; flex-shrink: 0;";
        let title_style = "font-size: 13px; font-weight: 700; letter-spacing: 1.5px; color: #eee;";
        let dropdown_style = "padding: 4px 12px; background: #2a2a2a; border: 1px solid #444; border-radius: 4px; color: #aaa; font-size: 12px; cursor: pointer;";
        let close_btn_style = "padding: 4px 10px; background: none; border: 1px solid #555; border-radius: 4px; color: #999; cursor: pointer; font-size: 14px; line-height: 1;";
        let body_style = "flex: 1; display: flex; overflow: hidden;";
        let center_style = "flex: 1; display: flex; flex-direction: column; overflow: hidden;";

        // Preview panel defaults (no preset selected yet)
        let preview_name = String::new();
        let preview_level = String::from("All");
        let preview_status = PresetStatus::Draft;
        let preview_tags: Vec<String> = Vec::new();
        let preview_scenes: Vec<(String, bool)> = Vec::new();

        rsx! {
            div {
                style: "{overlay_style}",

                // ── Top bar ──────────────────────────────────────────────────
                div {
                    style: "{header_style}",

                    // Left: title
                    span { style: "{title_style}", "PRESET BROWSER" }

                    // Center: browse mode + browse level dropdowns
                    div {
                        style: "display: flex; gap: 8px; align-items: center;",

                        // Browse mode (click to cycle)
                        div {
                            style: "{dropdown_style}",
                            onclick: move |_| {
                                let all = BrowseMode::all();
                                let idx = all.iter().position(|m| *m == browse_mode).unwrap_or(0);
                                let next = all[(idx + 1) % all.len()];
                                state.write().browse_mode = next;
                            },
                            "Mode: {browse_mode_name}"
                        }

                        // Browse level (click to cycle)
                        div {
                            style: "{dropdown_style}",
                            onclick: move |_| {
                                let all = BrowseLevel::all();
                                let idx = all.iter().position(|l| *l == browse_level).unwrap_or(0);
                                let next = all[(idx + 1) % all.len()];
                                state.write().browse_level = next;
                            },
                            "Level: {browse_level_name}"
                        }
                    }

                    // Right: close button
                    button {
                        style: "{close_btn_style}",
                        onclick: move |_| {
                            *RIG_PRESET_BROWSER_OPEN.write() = false;
                        },
                        "X"
                    }
                }

                // ── Body: three-column layout ────────────────────────────────
                div {
                    style: "{body_style}",

                    // Left: search panel
                    SearchPanel {
                        query: query.clone(),
                        sort_mode: sort_mode,
                        status_filter: status_filter,
                        min_quality: min_quality,
                        on_query_change: move |q: String| {
                            state.write().query = q;
                        },
                        on_sort_change: move |s: SortMode| {
                            state.write().sort_mode = s;
                        },
                        on_status_change: move |s: Option<PresetStatus>| {
                            state.write().status_filter = s;
                        },
                        on_quality_change: move |q: u8| {
                            state.write().min_quality = q;
                        },
                    }

                    // Center: tag columns (top) + results list (bottom)
                    div {
                        style: "{center_style}",

                        TagColumnView {
                            browse_mode: browse_mode,
                            registry: registry.clone(),
                            selected_tags: selected_tags.clone(),
                            instrument: instrument_name.clone(),
                            on_toggle: move |(col, tag_id)| {
                                state.write().toggle_tag(col, tag_id);
                            },
                        }

                        ResultsList {
                            results: Vec::new(),
                            selected_index: selected_result_index,
                            on_select: move |idx: usize| {
                                state.write().selected_result_index = Some(idx);
                            },
                            on_load: move |_idx: usize| {
                                // Load preset and close dialog
                                *RIG_PRESET_BROWSER_OPEN.write() = false;
                            },
                        }
                    }

                    // Right: preview panel
                    PreviewPanel {
                        name: preview_name,
                        level: preview_level,
                        status: preview_status,
                        quality: 0,
                        versatility: 0,
                        production: 0,
                        tags: preview_tags,
                        scenes: preview_scenes,
                        selected_scene: selected_scene_index,
                        on_scene_click: move |idx: usize| {
                            state.write().selected_scene_index = Some(idx);
                        },
                    }
                }

                // ── Bottom toolbar ───────────────────────────────────────────
                BrowserToolbar {
                    result_count: 0,
                    total_count: 0,
                    on_close: move |_| {
                        *RIG_PRESET_BROWSER_OPEN.write() = false;
                    },
                }
            }
        }
    }
}
