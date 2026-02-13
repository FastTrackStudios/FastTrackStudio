//! Tag column view — container for dynamic tag columns.

use crate::prelude::*;
use signal_control::id::TagId;
use signal_control::tags::browse::BrowseMode;
use signal_control::tags::{TagCategory, TagRegistry};
use std::collections::HashSet;

/// Props for the tag columns container.
#[derive(Props, Clone, PartialEq)]
pub struct TagColumnViewProps {
    /// Active browse mode.
    pub browse_mode: BrowseMode,
    /// Tag registry with all available tags.
    pub registry: TagRegistry,
    /// Selected tags per column.
    pub selected_tags: Vec<HashSet<TagId>>,
    /// Which instrument type's columns to use.
    pub instrument: String,
    /// Callback when a tag is toggled.
    pub on_toggle: EventHandler<(usize, TagId)>,
}

/// Container for dynamic tag columns that vary by browse mode.
#[component]
pub fn TagColumnView(props: TagColumnViewProps) -> Element {
    let columns: &[TagCategory] = match props.instrument.as_str() {
        "Guitar" => props.browse_mode.guitar_columns(),
        "Keys" => props.browse_mode.keys_columns(),
        "Bass" => props.browse_mode.bass_columns(),
        _ => props.browse_mode.guitar_columns(),
    };

    rsx! {
        div {
            class: "tag-columns-container",
            style: "display: flex; flex: 1; overflow-x: auto; border-bottom: 1px solid #333;",
            for (i, category) in columns.iter().enumerate() {
                {
                    let tags = props.registry.by_category(*category)
                        .into_iter()
                        .cloned()
                        .collect::<Vec<_>>();
                    let selected = props.selected_tags
                        .get(i)
                        .cloned()
                        .unwrap_or_default();

                    rsx! {
                        super::tag_column::TagColumn {
                            category: *category,
                            tags: tags,
                            selected: selected,
                            column_index: i,
                            on_toggle: props.on_toggle.clone(),
                        }
                    }
                }
            }
        }
    }
}
