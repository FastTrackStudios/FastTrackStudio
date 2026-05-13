//! `PageView` — full page editor with title, frontmatter, outliner /
//! whiteboard, and Backlinks / Outgoing / Tags / Unresolved tabs.
//!
//! Dumb component: caller supplies the page row, the page's blocks, the
//! resolved backlinks list, the picker candidate pools, and a handful
//! of event handlers. We re-emit page-level patches via
//! `on_patch_frontmatter` + `on_rename_page` and forward outliner edits
//! through `on_block_patch` / `on_structural`.

use crate::backlinks::BacklinksPanel;
use crate::common::{BlockBrief, PageBrief};
use crate::editor::{BlockUpdate, OutlinerEditor, StructuralOp};
use crate::properties_panel::PropertiesPanel;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Hash, Link as LinkIcon, Link2Off};
use fts_ui::prelude::*;
use knowledge_proto::{Block, Page, Ref};
use std::collections::BTreeMap;
use uuid::Uuid;

#[component]
pub fn PageView(
    page: Page,
    blocks: Vec<Block>,
    backlinks: Vec<BlockBrief>,
    available_pages: Vec<PageBrief>,
    available_blocks: Vec<BlockBrief>,
    on_rename_page: EventHandler<String>,
    on_patch_frontmatter: EventHandler<Vec<(String, serde_json::Value)>>,
    on_block_patch: EventHandler<(Uuid, BlockUpdate)>,
    on_structural: EventHandler<StructuralOp>,
    on_open_page: EventHandler<Uuid>,
    on_open_block: EventHandler<Uuid>,
) -> Element {
    let frontmatter = parse_frontmatter(&page.frontmatter_json);
    let fm_open_default = !frontmatter.is_empty();
    let active_tab = use_signal(|| "backlinks".to_string());

    let is_canvas = page.ext == "canvas";

    // ── Header values ──────────────────────────────────────────────
    let title = page.basename.clone();

    // ── Refs (outgoing / unresolved / tag counts) ──────────────────
    let refs = collect_refs(&blocks);
    let outgoing = group_outgoing(&refs);
    let tag_counts = count_tags(&refs);
    let unresolved = find_unresolved(&refs, &available_pages);

    rsx! {
        div { class: "flex flex-col gap-4 p-4",
            // ── Title row ──────────────────────────────────────────
            div { class: "flex items-baseline gap-3",
                div { class: "text-2xl font-semibold",
                    InlineEdit {
                        value: title,
                        on_commit: move |new_title: String| {
                            if !new_title.trim().is_empty() {
                                on_rename_page.call(new_title);
                            }
                        },
                        placeholder: "Untitled".to_string(),
                    }
                }
                span { class: "text-xs text-muted-foreground", "{page.path}" }
                Badge {
                    variant: BadgeVariant::Outline,
                    class: "ml-auto text-xs".to_string(),
                    "{page.ext}"
                }
            }

            // ── Frontmatter ────────────────────────────────────────
            details { class: "rounded-md border border-border bg-card",
                open: fm_open_default,
                summary { class: "cursor-pointer select-none px-3 py-2 text-xs font-medium text-muted-foreground",
                    "Properties ({frontmatter.len()})"
                }
                div { class: "p-2",
                    PropertiesPanel {
                        entries: frontmatter.clone(),
                        on_change: move |entries| on_patch_frontmatter.call(entries),
                        on_add: move |_| {
                            let mut entries = frontmatter.clone();
                            entries.push(("new_property".to_string(), serde_json::Value::Null));
                            on_patch_frontmatter.call(entries);
                        },
                        editable: true,
                    }
                }
            }

            // ── Body (outliner / whiteboard) ───────────────────────
            if is_canvas {
                // FUTURE: import knowledge-ui::canvas::Whiteboard once
                // Phase B-canvas lands the component.
                EmptyState {
                    message: "Whiteboard rendering not yet wired (Phase B-canvas).".to_string(),
                }
            } else {
                OutlinerEditor {
                    page_id: page.id,
                    blocks: blocks.clone(),
                    available_pages,
                    available_blocks,
                    on_block_patch,
                    on_structural,
                    on_open_page,
                    on_open_block,
                }
            }

            // ── Tabs strip ─────────────────────────────────────────
            Tabs {
                value: Some(active_tab()),
                on_change: move |v: String| active_tab.clone().set(v),
                default_value: "backlinks".to_string(),
                TabList {
                    TabTrigger { value: "backlinks".to_string(), index: 0usize, "Backlinks ({backlinks.len()})" }
                    TabTrigger { value: "outgoing".to_string(), index: 1usize, "Outgoing ({outgoing.values().map(Vec::len).sum::<usize>()})" }
                    TabTrigger { value: "tags".to_string(), index: 2usize, "Tags ({tag_counts.len()})" }
                    TabTrigger { value: "unresolved".to_string(), index: 3usize, "Unresolved ({unresolved.len()})" }
                }
                TabContent { value: "backlinks".to_string(), index: 0usize,
                    BacklinksPanel { backlinks: backlinks.clone(), on_open_block }
                }
                TabContent { value: "outgoing".to_string(), index: 1usize,
                    OutgoingPanel { groups: outgoing, on_open_page }
                }
                TabContent { value: "tags".to_string(), index: 2usize,
                    TagsListPanel { tag_counts }
                }
                TabContent { value: "unresolved".to_string(), index: 3usize,
                    UnresolvedPanel { unresolved }
                }
            }
        }
    }
}

// ── Frontmatter parsing ────────────────────────────────────────────────

fn parse_frontmatter(json: &str) -> Vec<(String, serde_json::Value)> {
    // The `Page.frontmatter_json` is order-preserving (IndexMap-backed
    // per the proto contract). `serde_json::Value::Object` uses a
    // BTreeMap-ish ordering on standard features; with the `indexmap`
    // crate's `serde` integration the consumer would deserialize into
    // an `IndexMap<String, Value>`. For the panel we need an ordered
    // `Vec`, so we do that explicitly here.
    use indexmap::IndexMap;
    let parsed: IndexMap<String, serde_json::Value> =
        serde_json::from_str(json).unwrap_or_default();
    parsed.into_iter().collect()
}

// ── Outgoing refs ──────────────────────────────────────────────────────

fn collect_refs(blocks: &[Block]) -> Vec<Ref> {
    let mut out = Vec::new();
    for b in blocks.iter() {
        if let Ok(refs) = serde_json::from_str::<Vec<Ref>>(&b.refs_json) {
            out.extend(refs);
        }
    }
    out
}

fn group_outgoing(refs: &[Ref]) -> BTreeMap<&'static str, Vec<String>> {
    let mut groups: BTreeMap<&'static str, Vec<String>> = BTreeMap::new();
    for r in refs.iter() {
        match r {
            Ref::Link(l) => groups
                .entry("Wikilinks")
                .or_default()
                .push(l.target_linkpath.clone()),
            Ref::Embed(e) => groups
                .entry("Embeds")
                .or_default()
                .push(e.target_linkpath.clone()),
            Ref::Tag(t) => groups.entry("Tags").or_default().push(t.path.join("/")),
            Ref::Entity(e) => groups
                .entry("Entities")
                .or_default()
                .push(format!("{}/{}", e.kind, e.id)),
            Ref::BlockRef(b) => groups
                .entry("Block refs")
                .or_default()
                .push(b.target_block_id.to_string()),
        }
    }
    for v in groups.values_mut() {
        v.sort();
        v.dedup();
    }
    groups
}

fn count_tags(refs: &[Ref]) -> BTreeMap<String, u32> {
    let mut counts: BTreeMap<String, u32> = BTreeMap::new();
    for r in refs.iter() {
        if let Ref::Tag(t) = r {
            *counts.entry(t.path.join("/")).or_default() += 1;
        }
    }
    counts
}

fn find_unresolved(refs: &[Ref], pages: &[PageBrief]) -> Vec<String> {
    let known: std::collections::HashSet<String> = pages
        .iter()
        .flat_map(|p| {
            let mut v = vec![p.basename.clone(), p.path.clone()];
            v.extend(p.aliases.clone());
            v
        })
        .collect();
    let mut out: Vec<String> = refs
        .iter()
        .filter_map(|r| match r {
            Ref::Link(l) if !known.contains(&l.target_linkpath) => Some(l.target_linkpath.clone()),
            Ref::Embed(e) if !known.contains(&e.target_linkpath) => Some(e.target_linkpath.clone()),
            _ => None,
        })
        .collect();
    out.sort();
    out.dedup();
    out
}

#[component]
fn OutgoingPanel(
    groups: BTreeMap<&'static str, Vec<String>>,
    on_open_page: EventHandler<Uuid>,
) -> Element {
    // We surface link-targets as text only — resolving them to UUIDs
    // requires the same resolver the editor uses; the caller can hook
    // up `on_open_page` once they map link targets to ids.
    let _ = on_open_page;
    if groups.is_empty() {
        return rsx! { EmptyState { message: "No outgoing references.".to_string() } };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for (heading, items) in groups.into_iter() {
                Card {
                    CardHeader { class: "px-3 py-2".to_string(),
                        CardTitle { class: "text-sm".to_string(), "{heading} ({items.len()})" }
                    }
                    CardContent { class: "px-3 pb-3 pt-0 flex flex-wrap gap-1".to_string(),
                        for target in items.into_iter() {
                            Badge {
                                variant: BadgeVariant::Secondary,
                                class: "text-xs".to_string(),
                                LinkIcon { size: 10 }
                                "{target}"
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn TagsListPanel(tag_counts: BTreeMap<String, u32>) -> Element {
    if tag_counts.is_empty() {
        return rsx! { EmptyState { message: "No tags on this page.".to_string() } };
    }
    rsx! {
        div { class: "flex flex-wrap gap-1",
            for (tag, count) in tag_counts.into_iter() {
                Badge {
                    variant: BadgeVariant::Secondary,
                    class: "text-xs gap-1".to_string(),
                    Hash { size: 10 }
                    "{tag}"
                    span { class: "ml-1 text-muted-foreground", "{count}" }
                }
            }
        }
    }
}

#[component]
fn UnresolvedPanel(unresolved: Vec<String>) -> Element {
    if unresolved.is_empty() {
        return rsx! { EmptyState { message: "All references resolve.".to_string() } };
    }
    rsx! {
        div { class: "flex flex-col gap-1",
            for target in unresolved.into_iter() {
                div { class: "flex items-center gap-2 rounded-md border border-dashed border-border bg-muted/30 px-3 py-1.5 text-sm",
                    Link2Off { size: 12 }
                    span { class: "flex-1 truncate", "{target}" }
                    span { class: "text-xs text-muted-foreground italic",
                        "(create page →)"
                    }
                }
            }
        }
    }
}
