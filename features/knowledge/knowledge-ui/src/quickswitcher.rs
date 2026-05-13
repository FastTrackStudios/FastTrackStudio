//! `Quickswitcher` — Cmd-O fuzzy palette over the active vault.
//!
//! Renders pages, blocks, and tags through a `CommandDialog`. Ranking
//! uses `nucleo-matcher` (Helix's fuzzy matcher). When the query is
//! empty the caller's `recent_pages` list seeds the top group. When the
//! query starts with `>` we flip into command mode.
//!
//! Pure dumb component: caller owns the data + the `open` signal.

use crate::common::{BlockBrief, PageBrief};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{FileText, Hash, Plus, Search as SearchIcon, Terminal};
use fts_ui::prelude::*;
use nucleo_matcher::{
    Matcher,
    pattern::{CaseMatching, Normalization, Pattern},
};
use uuid::Uuid;

/// The side-effect to fire when the user picks an item.
#[derive(Clone, Debug, PartialEq)]
pub enum QuickswitcherPick {
    Page(Uuid),
    Block(Uuid),
    Tag(String),
    /// Query matched nothing — offer to create a page with this title.
    CreatePage(String),
}

#[component]
pub fn Quickswitcher(
    open: Signal<bool>,
    pages: Vec<PageBrief>,
    blocks: Vec<BlockBrief>,
    tags: Vec<String>,
    recent_pages: Vec<Uuid>,
    on_pick: EventHandler<QuickswitcherPick>,
) -> Element {
    let query = use_signal(String::new);
    let q = query();
    let trimmed = q.trim();

    let command_mode = trimmed.starts_with('>');

    // ── Filter + rank ──────────────────────────────────────────────
    let pages_view = if trimmed.is_empty() {
        // Default order: recent pages first, then everything else.
        let mut by_id: std::collections::HashMap<Uuid, PageBrief> =
            pages.iter().cloned().map(|p| (p.id, p)).collect();
        let mut out: Vec<PageBrief> = Vec::new();
        for id in recent_pages.iter() {
            if let Some(p) = by_id.remove(id) {
                out.push(p);
            }
        }
        out.extend(pages.iter().filter(|p| by_id.contains_key(&p.id)).cloned());
        out.truncate(20);
        out
    } else if command_mode {
        Vec::new()
    } else {
        fuzzy_pages(&pages, trimmed)
    };

    let blocks_view = if trimmed.is_empty() || command_mode {
        Vec::new()
    } else {
        fuzzy_blocks(&blocks, trimmed)
    };

    let tags_view = if trimmed.is_empty() || command_mode {
        Vec::new()
    } else {
        fuzzy_tags(&tags, trimmed)
    };

    let show_create =
        !command_mode && !trimmed.is_empty() && pages_view.is_empty() && blocks_view.is_empty();
    let create_label_str = trimmed.to_string();
    let create_label_for_value = create_label_str.clone();
    let create_label_for_display = create_label_str.clone();

    rsx! {
        CommandDialog {
            open: open(),
            on_close: move |_| open.set(false),
            div { class: "flex items-center gap-2 border-b border-border px-3 py-2",
                if command_mode {
                    Terminal { size: 14 }
                } else {
                    SearchIcon { size: 14 }
                }
                CommandInput {
                    value: query,
                    placeholder: "Type to search — pages, blocks, tags. `>` for commands.",
                    class: "h-9 flex-1 border-0 bg-transparent text-sm outline-none focus:ring-0",
                }
            }

            CommandList {
                CommandEmpty { "No results." }

                if command_mode {
                    CommandGroup { heading: "Commands",
                        CommandItem {
                            value: "create page".to_string(),
                            on_select: move |_| {
                                open.set(false);
                                on_pick.call(QuickswitcherPick::CreatePage(String::new()));
                            },
                            Plus { size: 14 }
                            "Create new page"
                        }
                    }
                } else {
                    if !pages_view.is_empty() {
                        CommandGroup { heading: "Pages",
                            for p in pages_view.iter().cloned() {
                                {
                                    let pid = p.id;
                                    let path = p.path.clone();
                                    let title = p.basename.clone();
                                    let aliases = p.aliases.join(" ");
                                    rsx! {
                                        CommandItem {
                                            key: "p-{pid}",
                                            value: format!("{} {} {}", title, path, aliases),
                                            on_select: move |_| {
                                                open.set(false);
                                                on_pick.call(QuickswitcherPick::Page(pid));
                                            },
                                            FileText { size: 14 }
                                            div { class: "flex flex-col min-w-0",
                                                span { class: "truncate text-sm", "{title}" }
                                                span { class: "truncate text-xs text-muted-foreground", "{path}" }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if !blocks_view.is_empty() {
                        CommandSeparator {}
                        CommandGroup { heading: "Blocks",
                            for b in blocks_view.iter().cloned() {
                                {
                                    let bid = b.id;
                                    let preview = b.preview.clone();
                                    let page = b.page_basename.clone();
                                    rsx! {
                                        CommandItem {
                                            key: "b-{bid}",
                                            value: format!("{} {}", preview, page),
                                            on_select: move |_| {
                                                open.set(false);
                                                on_pick.call(QuickswitcherPick::Block(bid));
                                            },
                                            Hash { size: 14 }
                                            div { class: "flex flex-col min-w-0",
                                                span { class: "truncate text-sm", "{preview}" }
                                                span { class: "truncate text-xs text-muted-foreground", "in {page}" }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if !tags_view.is_empty() {
                        CommandSeparator {}
                        CommandGroup { heading: "Tags",
                            for tag in tags_view.iter().cloned() {
                                {
                                    let tag_for_select = tag.clone();
                                    let tag_for_value = tag.clone();
                                    let tag_for_label = tag.clone();
                                    rsx! {
                                        CommandItem {
                                            key: "t-{tag_for_value}",
                                            value: tag_for_value,
                                            on_select: move |_| {
                                                open.set(false);
                                                on_pick.call(QuickswitcherPick::Tag(tag_for_select.clone()));
                                            },
                                            Hash { size: 14 }
                                            span { "#{tag_for_label}" }
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if show_create {
                        CommandSeparator {}
                        CommandGroup { heading: "Actions",
                            CommandItem {
                                value: format!("create {create_label_for_value}"),
                                on_select: move |_| {
                                    open.set(false);
                                    on_pick.call(QuickswitcherPick::CreatePage(create_label_str.clone()));
                                },
                                Plus { size: 14 }
                                "Create page \"{create_label_for_display}\""
                            }
                        }
                    }
                }
            }
        }
    }
}

// ── Ranking helpers ────────────────────────────────────────────────────

fn fuzzy_pages(pages: &[PageBrief], query: &str) -> Vec<PageBrief> {
    let mut matcher = Matcher::new(nucleo_matcher::Config::DEFAULT);
    let pattern = Pattern::parse(query, CaseMatching::Smart, Normalization::Smart);
    let haystacks: Vec<String> = pages
        .iter()
        .map(|p| format!("{} {} {}", p.basename, p.path, p.aliases.join(" ")))
        .collect();
    let mut scored: Vec<(u32, PageBrief)> = haystacks
        .iter()
        .zip(pages.iter())
        .filter_map(|(h, p)| {
            pattern
                .score(nucleo_matcher::Utf32Str::Ascii(h.as_bytes()), &mut matcher)
                .map(|s| (s, p.clone()))
        })
        .collect();
    scored.sort_by(|a, b| b.0.cmp(&a.0));
    scored.into_iter().take(20).map(|(_, p)| p).collect()
}

fn fuzzy_blocks(blocks: &[BlockBrief], query: &str) -> Vec<BlockBrief> {
    let mut matcher = Matcher::new(nucleo_matcher::Config::DEFAULT);
    let pattern = Pattern::parse(query, CaseMatching::Smart, Normalization::Smart);
    let haystacks: Vec<String> = blocks
        .iter()
        .map(|b| format!("{} {}", b.preview, b.page_basename))
        .collect();
    let mut scored: Vec<(u32, BlockBrief)> = haystacks
        .iter()
        .zip(blocks.iter())
        .filter_map(|(h, b)| {
            pattern
                .score(nucleo_matcher::Utf32Str::Ascii(h.as_bytes()), &mut matcher)
                .map(|s| (s, b.clone()))
        })
        .collect();
    scored.sort_by(|a, b| b.0.cmp(&a.0));
    scored.into_iter().take(20).map(|(_, b)| b).collect()
}

fn fuzzy_tags(tags: &[String], query: &str) -> Vec<String> {
    let mut matcher = Matcher::new(nucleo_matcher::Config::DEFAULT);
    let pattern = Pattern::parse(query, CaseMatching::Smart, Normalization::Smart);
    let mut scored: Vec<(u32, String)> = tags
        .iter()
        .filter_map(|t| {
            pattern
                .score(nucleo_matcher::Utf32Str::Ascii(t.as_bytes()), &mut matcher)
                .map(|s| (s, t.clone()))
        })
        .collect();
    scored.sort_by(|a, b| b.0.cmp(&a.0));
    scored.into_iter().take(20).map(|(_, t)| t).collect()
}
