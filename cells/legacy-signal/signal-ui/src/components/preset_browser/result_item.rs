//! Single preset result item.

use crate::prelude::*;
use signal_control::rating::PresetStatus;
use signal_control::tags::browse::BrowseLevel;

/// Props for a single result item.
#[derive(Props, Clone, PartialEq)]
pub struct ResultItemProps {
    /// Preset name.
    pub name: String,
    /// Hierarchy level badge.
    pub level: BrowseLevel,
    /// Quality rating (0-5).
    pub quality: u8,
    /// Versatility rating (0-5).
    pub versatility: u8,
    /// Number of scenes.
    pub scene_count: usize,
    /// Workflow status.
    pub status: PresetStatus,
    /// Whether this item is selected.
    pub is_selected: bool,
    /// Index in the results list.
    pub index: usize,
    /// Click handler.
    pub on_click: EventHandler<usize>,
    /// Double-click handler (load preset).
    pub on_double_click: EventHandler<usize>,
}

/// A single preset row in the results list.
#[component]
pub fn ResultItem(props: ResultItemProps) -> Element {
    let bg = if props.is_selected {
        "background: #2a2a2a;"
    } else {
        ""
    };
    let level_badge = props.level.display_name();
    let status_color = match props.status {
        PresetStatus::Draft => "#666",
        PresetStatus::Tweaking => "#EAB308",
        PresetStatus::Ready => "#22C55E",
        PresetStatus::Favorite => "#EC4899",
        PresetStatus::Archived => "#6B7280",
    };

    rsx! {
        div {
            class: "result-item",
            style: "display: flex; align-items: center; padding: 8px 12px; cursor: pointer; border-bottom: 1px solid #222; font-size: 12px; {bg}",
            onclick: {
                let on_click = props.on_click.clone();
                let index = props.index;
                move |_| on_click.call(index)
            },
            ondoubleclick: {
                let on_double_click = props.on_double_click.clone();
                let index = props.index;
                move |_| on_double_click.call(index)
            },

            // Name
            span {
                style: "flex: 1; color: #ddd; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;",
                "{props.name}"
            }

            // Level badge
            span {
                style: "padding: 2px 6px; background: #333; border-radius: 3px; font-size: 10px; color: #aaa; margin-right: 8px;",
                "{level_badge}"
            }

            // Status dot
            span {
                style: "width: 8px; height: 8px; border-radius: 50%; background: {status_color}; margin-right: 8px;",
            }

            // Rating
            span {
                style: "color: #888; font-size: 11px; margin-right: 8px; min-width: 50px;",
                "Q:{props.quality} V:{props.versatility}"
            }

            // Scene count
            span {
                style: "color: #666; font-size: 11px; min-width: 30px;",
                "{props.scene_count}sc"
            }
        }
    }
}
