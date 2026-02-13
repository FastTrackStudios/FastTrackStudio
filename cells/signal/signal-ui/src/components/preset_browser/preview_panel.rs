//! Preview panel — right sidebar showing selected preset details.

use crate::prelude::*;
use signal_control::rating::PresetStatus;

/// Props for the preview panel.
#[derive(Props, Clone, PartialEq)]
pub struct PreviewPanelProps {
    /// Selected preset name.
    pub name: String,
    /// Level display name.
    pub level: String,
    /// Workflow status.
    pub status: PresetStatus,
    /// Quality rating.
    pub quality: u8,
    /// Versatility rating.
    pub versatility: u8,
    /// Production rating.
    pub production: u8,
    /// Tag names.
    pub tags: Vec<String>,
    /// Scene names (with default marker).
    pub scenes: Vec<(String, bool)>,
    /// Selected scene index.
    pub selected_scene: Option<usize>,
    /// Scene click handler.
    pub on_scene_click: EventHandler<usize>,
}

/// Preview panel — right sidebar showing selected preset details.
#[component]
pub fn PreviewPanel(props: PreviewPanelProps) -> Element {
    let status_text = props.status.display_name();
    let status_color = match props.status {
        PresetStatus::Draft => "#666",
        PresetStatus::Tweaking => "#EAB308",
        PresetStatus::Ready => "#22C55E",
        PresetStatus::Favorite => "#EC4899",
        PresetStatus::Archived => "#6B7280",
    };

    rsx! {
        div {
            class: "preview-panel",
            style: "width: 240px; border-left: 1px solid #333; display: flex; flex-direction: column; background: #1e1e1e;",

            // Preset name
            div {
                style: "padding: 16px; border-bottom: 1px solid #333;",
                div {
                    style: "font-size: 15px; font-weight: 600; color: #eee; margin-bottom: 4px;",
                    "{props.name}"
                }
                div {
                    style: "font-size: 11px; color: #888;",
                    "Level: {props.level}"
                }
                div {
                    style: "font-size: 11px; color: {status_color}; margin-top: 2px;",
                    "Status: {status_text}"
                }
            }

            // Ratings
            div {
                style: "padding: 12px 16px; border-bottom: 1px solid #333;",
                div {
                    style: "font-size: 10px; text-transform: uppercase; color: #666; margin-bottom: 6px; letter-spacing: 0.5px;",
                    "Rating"
                }
                div {
                    style: "display: flex; gap: 12px; font-size: 12px; color: #aaa;",
                    span { "Q:{props.quality}" }
                    span { "V:{props.versatility}" }
                    span { "P:{props.production}" }
                }
            }

            // Tags
            if !props.tags.is_empty() {
                div {
                    style: "padding: 12px 16px; border-bottom: 1px solid #333;",
                    div {
                        style: "font-size: 10px; text-transform: uppercase; color: #666; margin-bottom: 6px; letter-spacing: 0.5px;",
                        "Tags"
                    }
                    div {
                        style: "display: flex; flex-wrap: wrap; gap: 4px;",
                        for tag_name in props.tags.iter() {
                            span {
                                style: "padding: 2px 8px; background: #333; border-radius: 3px; font-size: 11px; color: #bbb;",
                                "{tag_name}"
                            }
                        }
                    }
                }
            }

            // Scenes
            div {
                style: "padding: 12px 16px; flex: 1; overflow-y: auto;",
                div {
                    style: "font-size: 10px; text-transform: uppercase; color: #666; margin-bottom: 6px; letter-spacing: 0.5px;",
                    "Scenes"
                }
                for (i, (scene_name, is_default)) in props.scenes.iter().enumerate() {
                    {
                        let is_selected = props.selected_scene == Some(i);
                        let on_click = props.on_scene_click.clone();
                        let scene_name = scene_name.clone();
                        let is_default = *is_default;
                        let selection_style = if is_selected {
                            "background: #2a2a2a; color: white;"
                        } else {
                            "color: #999;"
                        };

                        rsx! {
                            div {
                                style: "padding: 6px 8px; cursor: pointer; font-size: 12px; border-radius: 3px; margin-bottom: 2px; {selection_style}",
                                onclick: move |_| on_click.call(i),
                                if is_default {
                                    span { style: "color: #EAB308; margin-right: 4px;", "* " }
                                }
                                "{scene_name}"
                            }
                        }
                    }
                }
            }
        }
    }
}
