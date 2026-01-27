//! Block Browser Component
//!
//! Browse and select individual block presets (e.g., TS-808, Clean Boost, Hall Reverb).
//! Blocks can be dropped into modules or added as standalone blocks in presets.

use dioxus::prelude::*;
use fts::rig::BlockPresetInfo;
use lucide_dioxus::Search;

use crate::context::use_block_service;

/// Block Browser component for browsing and selecting block presets
#[component]
pub fn BlockBrowser(
    /// Currently selected block preset ID
    selected_id: Option<uuid::Uuid>,
    /// Callback when a block is selected
    on_select: Option<Callback<uuid::Uuid>>,
) -> Element {
    let mut search_query = use_signal(String::new);
    let block_service = use_block_service();

    // Fetch all block presets
    let presets = use_resource(move || {
        let client = block_service.client.clone();
        async move {
            client.get_all_block_presets().await
        }
    });

    let filtered_presets = use_memo(move || {
        let query = search_query().to_lowercase();
        if let Some(all_presets) = presets.read().as_ref() {
            if query.is_empty() {
                all_presets.clone()
            } else {
                all_presets
                    .iter()
                    .filter(|p| p.name.to_lowercase().contains(&query))
                    .cloned()
                    .collect()
            }
        } else {
            vec![]
        }
    });

    rsx! {
        div { class: "flex flex-col h-full bg-card",
            // Header with search
            div { class: "p-4 border-b border-border",
                h2 { class: "text-lg font-semibold mb-3", "Block Browser" }
                p { class: "text-sm text-muted-foreground mb-3",
                    "Browse individual block presets"
                }

                // Search bar
                div { class: "relative",
                    Search { class: "absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-muted-foreground" }
                    input {
                        class: "w-full pl-9 pr-3 py-2 bg-background border border-border rounded-md text-sm placeholder:text-muted-foreground focus:outline-none focus:ring-2 focus:ring-primary",
                        r#type: "text",
                        placeholder: "Search blocks...",
                        value: "{search_query}",
                        oninput: move |evt| search_query.set(evt.value().clone()),
                    }
                }
            }

            // Block list
            div { class: "flex-1 overflow-y-auto p-4",
                match presets.read().as_ref() {
                    Some(_) => rsx! {
                        div { class: "space-y-1",
                            for preset in filtered_presets() {
                                BlockItem {
                                    preset: preset.clone(),
                                    is_selected: selected_id == Some(preset.id),
                                    on_select: on_select,
                                }
                            }
                        }
                    },
                    None => rsx! {
                        div { class: "text-sm text-muted-foreground p-4",
                            "Loading blocks..."
                        }
                    },
                }
            }

            // Footer with info
            div { class: "p-3 border-t border-border bg-muted/30",
                p { class: "text-xs text-muted-foreground text-center",
                    "Click a block to view details • Drag to add to module"
                }
            }
        }
    }
}

/// Individual block item
#[component]
fn BlockItem(
    preset: BlockPresetInfo,
    is_selected: bool,
    on_select: Option<Callback<uuid::Uuid>>,
) -> Element {
    let block_type_str = format!("{:?}", preset.block_type);
    let selected_class = if is_selected {
        "border-primary bg-primary/10"
    } else {
        "border-border hover:border-primary/50 hover:bg-muted/30"
    };

    rsx! {
        div {
            class: "p-3 bg-background border {selected_class} rounded-md transition-all cursor-pointer",
            onclick: move |_| {
                if let Some(cb) = on_select {
                    cb.call(preset.id);
                }
            },
            div { class: "flex items-start justify-between mb-1",
                h4 { class: "text-sm font-medium", "{preset.name}" }
                span { class: "text-xs text-muted-foreground px-2 py-0.5 bg-muted rounded",
                    "{block_type_str}"
                }
            }
            if preset.rating > 0 {
                div { class: "flex items-center gap-1 mt-1",
                    for _ in 0..preset.rating {
                        span { class: "text-yellow-500 text-xs", "★" }
                    }
                }
            }
        }
    }
}
