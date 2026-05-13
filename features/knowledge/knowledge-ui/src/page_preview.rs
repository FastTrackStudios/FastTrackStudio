//! `PagePreview` — hover-popover content showing a compact preview of
//! a page (title, tags + aliases chips, first ~12 blocks).
//!
//! Caller is responsible for fetching `top_blocks` (typically the first
//! 8–12 blocks in `sort_key` order). Renders read-only via
//! [`crate::common::render_block_readonly`].

use crate::common::render_block_readonly;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::FileText;
use fts_ui::prelude::*;
use knowledge_proto::{Block, Page};
use uuid::Uuid;

#[component]
pub fn PagePreview(
    page: Page,
    top_blocks: Vec<Block>,
    on_open: EventHandler<Uuid>,
    /// When `true`, no max-height + no "Open page" footer button.
    /// `PageEmbed` uses this; the hover-card uses the default `false`.
    #[props(default = false)]
    full_height: bool,
) -> Element {
    // Pull tags + aliases off the frontmatter blob.
    let aliases = page.aliases.clone();
    let tags = extract_tags_from_frontmatter(&page.frontmatter_json);

    let body_class = if full_height {
        "p-3 space-y-2"
    } else {
        "p-3 space-y-2 max-h-[400px] overflow-auto"
    };

    let page_id = page.id;

    rsx! {
        div { class: "rounded-md border border-border bg-popover text-popover-foreground shadow-md",
            // Header
            div { class: "flex items-center gap-2 border-b border-border px-3 py-2",
                FileText { size: 14 }
                span { class: "truncate text-sm font-medium", "{page.basename}" }
                span { class: "ml-auto text-xs text-muted-foreground", "{page.ext}" }
            }

            div { class: "{body_class}",
                // Chips row
                if !aliases.is_empty() || !tags.is_empty() {
                    div { class: "flex flex-wrap gap-1",
                        for alias in aliases.iter() {
                            Badge {
                                variant: BadgeVariant::Outline,
                                class: "text-xs".to_string(),
                                "alias: {alias}"
                            }
                        }
                        for tag in tags.iter() {
                            Badge {
                                variant: BadgeVariant::Secondary,
                                class: "text-xs".to_string(),
                                "#{tag}"
                            }
                        }
                    }
                }

                // Top blocks (read-only).
                for block in top_blocks.iter().take(if full_height { usize::MAX } else { 12 }) {
                    {render_block_readonly(block)}
                }
            }

            if !full_height {
                div { class: "flex items-center justify-end gap-2 border-t border-border px-3 py-2",
                    Button {
                        size: ButtonSize::Small,
                        variant: ButtonVariant::Secondary,
                        on_click: move |_| on_open.call(page_id),
                        "Open page"
                    }
                }
            }
        }
    }
}

/// Pull a flat list of tags off a frontmatter JSON blob. Handles both
/// `tags: "a"` and `tags: ["a", "b"]` shapes, and silently ignores
/// anything unexpected.
fn extract_tags_from_frontmatter(json: &str) -> Vec<String> {
    let Ok(v) = serde_json::from_str::<serde_json::Value>(json) else {
        return Vec::new();
    };
    let Some(obj) = v.as_object() else {
        return Vec::new();
    };
    let Some(t) = obj.get("tags") else {
        return Vec::new();
    };
    match t {
        serde_json::Value::String(s) => vec![s.clone()],
        serde_json::Value::Array(arr) => arr
            .iter()
            .filter_map(|x| x.as_str().map(str::to_string))
            .collect(),
        _ => Vec::new(),
    }
}
