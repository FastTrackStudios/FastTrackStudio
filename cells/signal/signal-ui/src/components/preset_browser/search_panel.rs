//! Search panel — left sidebar with search and filter controls.

use crate::prelude::*;
use signal_control::rating::PresetStatus;
use signal_control::tags::browse::SortMode;

/// Props for the search panel.
#[derive(Props, Clone, PartialEq)]
pub struct SearchPanelProps {
    /// Current search query.
    pub query: String,
    /// Current sort mode.
    pub sort_mode: SortMode,
    /// Current status filter.
    pub status_filter: Option<PresetStatus>,
    /// Minimum quality filter.
    pub min_quality: u8,
    /// Search input handler.
    pub on_query_change: EventHandler<String>,
    /// Sort mode change handler.
    pub on_sort_change: EventHandler<SortMode>,
    /// Status filter change handler.
    pub on_status_change: EventHandler<Option<PresetStatus>>,
    /// Quality filter change handler.
    pub on_quality_change: EventHandler<u8>,
}

/// Search panel — left sidebar with search and filter controls.
#[component]
pub fn SearchPanel(props: SearchPanelProps) -> Element {
    let sort_name = props.sort_mode.display_name();
    let status_name = props
        .status_filter
        .map(|s| s.display_name())
        .unwrap_or("All");

    rsx! {
        div {
            class: "search-panel",
            style: "width: 160px; border-right: 1px solid #333; display: flex; flex-direction: column; background: #1e1e1e; padding: 12px;",

            // Search label
            div {
                style: "font-size: 10px; text-transform: uppercase; color: #666; margin-bottom: 6px; letter-spacing: 0.5px;",
                "Search"
            }

            // Search input
            input {
                r#type: "text",
                value: "{props.query}",
                placeholder: "Filter presets...",
                style: "width: 100%; padding: 6px 8px; background: #2a2a2a; border: 1px solid #444; border-radius: 4px; color: #ddd; font-size: 12px; outline: none; margin-bottom: 16px; box-sizing: border-box;",
                oninput: {
                    let on_change = props.on_query_change.clone();
                    move |evt: FormEvent| {
                        on_change.call(evt.value().clone());
                    }
                },
            }

            // Status filter
            div {
                style: "font-size: 10px; text-transform: uppercase; color: #666; margin-bottom: 6px; letter-spacing: 0.5px;",
                "Status"
            }
            div {
                style: "padding: 6px 8px; background: #2a2a2a; border: 1px solid #444; border-radius: 4px; color: #aaa; font-size: 12px; cursor: pointer; margin-bottom: 16px;",
                onclick: {
                    let on_status = props.on_status_change.clone();
                    let current = props.status_filter;
                    move |_| {
                        // Cycle through: All -> Draft -> Tweaking -> Ready -> Favorite -> Archived -> All
                        let next = match current {
                            None => Some(PresetStatus::Draft),
                            Some(PresetStatus::Draft) => Some(PresetStatus::Tweaking),
                            Some(PresetStatus::Tweaking) => Some(PresetStatus::Ready),
                            Some(PresetStatus::Ready) => Some(PresetStatus::Favorite),
                            Some(PresetStatus::Favorite) => Some(PresetStatus::Archived),
                            Some(PresetStatus::Archived) => None,
                        };
                        on_status.call(next);
                    }
                },
                "{status_name}"
            }

            // Rating filter
            div {
                style: "font-size: 10px; text-transform: uppercase; color: #666; margin-bottom: 6px; letter-spacing: 0.5px;",
                "Min Quality"
            }
            div {
                style: "padding: 6px 8px; background: #2a2a2a; border: 1px solid #444; border-radius: 4px; color: #aaa; font-size: 12px; cursor: pointer; margin-bottom: 16px;",
                onclick: {
                    let on_quality = props.on_quality_change.clone();
                    let current = props.min_quality;
                    move |_| {
                        on_quality.call((current + 1) % 6);
                    }
                },
                if props.min_quality == 0 {
                    "Any"
                } else {
                    "{props.min_quality}+"
                }
            }

            // Sort mode
            div {
                style: "font-size: 10px; text-transform: uppercase; color: #666; margin-bottom: 6px; letter-spacing: 0.5px;",
                "Sort"
            }
            div {
                style: "padding: 6px 8px; background: #2a2a2a; border: 1px solid #444; border-radius: 4px; color: #aaa; font-size: 12px; cursor: pointer;",
                onclick: {
                    let on_sort = props.on_sort_change.clone();
                    let current = props.sort_mode;
                    move |_| {
                        let all = SortMode::all();
                        let idx = all.iter().position(|s| *s == current).unwrap_or(0);
                        let next = all[(idx + 1) % all.len()];
                        on_sort.call(next);
                    }
                },
                "{sort_name}"
            }
        }
    }
}
