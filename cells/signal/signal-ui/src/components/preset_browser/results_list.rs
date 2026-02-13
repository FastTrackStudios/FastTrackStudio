//! Results list -- filtered and sorted preset list.

use crate::prelude::*;
use signal_control::RigPresetInfo;

/// Props for the results list.
#[derive(Props, Clone, PartialEq)]
pub struct ResultsListProps {
    /// Filtered and sorted preset results.
    pub results: Vec<RigPresetInfo>,
    /// Currently selected index.
    pub selected_index: Option<usize>,
    /// Click handler.
    pub on_select: EventHandler<usize>,
    /// Double-click handler (load preset).
    pub on_load: EventHandler<usize>,
}

/// Filtered/sorted preset results list with level badges.
#[component]
pub fn ResultsList(props: ResultsListProps) -> Element {
    rsx! {
        div {
            class: "results-list",
            style: "flex: 1; overflow-y: auto; background: #1a1a1a;",

            if props.results.is_empty() {
                div {
                    style: "padding: 24px; text-align: center; color: #666; font-size: 13px;",
                    "No presets match the current filters"
                }
            } else {
                for (i, preset) in props.results.iter().enumerate() {
                    super::result_item::ResultItem {
                        name: preset.name.clone(),
                        level: signal_control::tags::browse::BrowseLevel::default(),
                        quality: 0,
                        versatility: 0,
                        scene_count: preset.scenes.len(),
                        status: signal_control::rating::PresetStatus::default(),
                        is_selected: props.selected_index == Some(i),
                        index: i,
                        on_click: props.on_select.clone(),
                        on_double_click: props.on_load.clone(),
                    }
                }
            }
        }
    }
}
