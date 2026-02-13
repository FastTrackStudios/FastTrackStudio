//! Single tag column — a scrollable list of tags within one category.

use crate::prelude::*;
use signal_control::id::TagId;
use signal_control::tags::{Tag, TagCategory};
use std::collections::HashSet;

/// Props for a single tag column.
#[derive(Props, Clone, PartialEq)]
pub struct TagColumnProps {
    /// The category this column represents.
    pub category: TagCategory,
    /// Available tags in this category.
    pub tags: Vec<Tag>,
    /// Currently selected tag IDs.
    pub selected: HashSet<TagId>,
    /// Column index (for state management).
    pub column_index: usize,
    /// Callback when a tag is toggled.
    pub on_toggle: EventHandler<(usize, TagId)>,
}

/// A single scrollable tag column.
#[component]
pub fn TagColumn(props: TagColumnProps) -> Element {
    let category_name = props.category.display_name();
    let category_color = props.category.default_color().unwrap_or_default();

    let all_style = if props.selected.is_empty() {
        "padding: 6px 12px; cursor: pointer; font-size: 12px; color: white; background: #2a2a2a;"
    } else {
        "padding: 6px 12px; cursor: pointer; font-size: 12px; color: #aaa;"
    };

    rsx! {
        div {
            class: "tag-column",
            style: "display: flex; flex-direction: column; min-width: 160px; max-width: 200px; border-right: 1px solid #333;",

            // Column header
            div {
                class: "tag-column-header",
                style: "padding: 8px 12px; font-size: 11px; font-weight: 600; text-transform: uppercase; color: {category_color}; border-bottom: 2px solid {category_color}; letter-spacing: 0.5px;",
                "{category_name}"
            }

            // "All" option
            div {
                class: "tag-item",
                style: "{all_style}",
                onclick: {
                    let on_toggle = props.on_toggle.clone();
                    let column_index = props.column_index;
                    move |_| {
                        // Clear all selections in this column -- signaled by a nil TagId
                        on_toggle.call((column_index, TagId::from_uuid(uuid::Uuid::nil())));
                    }
                },
                "All"
            }

            // Tag list
            div {
                class: "tag-list",
                style: "flex: 1; overflow-y: auto;",
                for tag in props.tags.iter() {
                    {
                        let tag_id = tag.id;
                        let is_selected = props.selected.contains(&tag_id);
                        let on_toggle = props.on_toggle.clone();
                        let column_index = props.column_index;
                        let tag_name = tag.name.clone();

                        let item_style = if is_selected {
                            "padding: 6px 12px; cursor: pointer; font-size: 12px; background: #2a2a2a; color: white; font-weight: 500;"
                        } else {
                            "padding: 6px 12px; cursor: pointer; font-size: 12px; color: #999;"
                        };

                        rsx! {
                            div {
                                class: "tag-item",
                                style: "{item_style}",
                                onclick: move |_| {
                                    on_toggle.call((column_index, tag_id));
                                },
                                "{tag_name}"
                            }
                        }
                    }
                }
            }
        }
    }
}
