//! Hierarchical page tree built from the `up` frontmatter property.
//!
//! Inspired by the Obsidian VirtFolder plugin
//! (<https://github.com/gr0grig/obsidian-virt-folder>): every page
//! declares its parent(s) via an `up` property (wikilinks). Pages
//! with multiple `up` values appear under each parent. Pages with
//! no `up` are either **roots** (have descendants) or **orphans**
//! (no parents, no children).
//!
//! On-disk folders (`Folder` entity) are independent — the tree is
//! purely a property-driven view.

use std::collections::{HashMap, HashSet};

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronDown, ChevronRight};
use knowledge_proto::Page;
use uuid::Uuid;

/// Extract the list of parent page basenames from a page's
/// frontmatter `up` property. Accepts string or array of
/// wikilinks (`"[[Page]]"` / `"[[Page|alias]]"` / bare basename).
pub fn parents_of(page: &Page) -> Vec<String> {
    let fm: serde_json::Value =
        serde_json::from_str(&page.frontmatter_json).unwrap_or(serde_json::Value::Null);
    let raw = match fm.get("up") {
        Some(v) => v,
        None => return Vec::new(),
    };
    let strings: Vec<&str> = match raw {
        serde_json::Value::String(s) => vec![s.as_str()],
        serde_json::Value::Array(arr) => arr.iter().filter_map(|v| v.as_str()).collect(),
        _ => return Vec::new(),
    };
    strings
        .into_iter()
        .filter_map(|s| {
            let target = strip_wikilink(s);
            // Self-reference protection.
            if target.eq_ignore_ascii_case(&page.basename) {
                None
            } else if target.is_empty() {
                None
            } else {
                Some(target)
            }
        })
        .collect()
}

/// Strip `[[…]]` wrapping, `|alias` suffix, and `#heading` /
/// `^block-id` anchors. Returns the bare basename.
fn strip_wikilink(s: &str) -> String {
    let trimmed = s.trim();
    let inner = trimmed
        .strip_prefix("[[")
        .and_then(|s| s.strip_suffix("]]"))
        .unwrap_or(trimmed);
    let no_alias = inner.split('|').next().unwrap_or(inner);
    let no_anchor = no_alias.split(['#', '^']).next().unwrap_or(no_alias);
    no_anchor.trim().to_string()
}

/// Prebuilt indices for fast tree rendering.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct UpTreeIndex {
    /// page basename (lowercased) → page id
    pub by_basename: HashMap<String, Uuid>,
    /// page id → list of child page ids (sorted by basename)
    pub children_of: HashMap<Uuid, Vec<Uuid>>,
    /// Pages that have descendants but no `up`.
    pub roots: Vec<Uuid>,
    /// Pages with neither parents nor children.
    pub orphans: Vec<Uuid>,
    /// page id → page (for fast lookup during rendering)
    pub pages: HashMap<Uuid, Page>,
}

pub fn build_index(pages: &[Page]) -> UpTreeIndex {
    let mut idx = UpTreeIndex::default();
    for p in pages {
        idx.by_basename.insert(p.basename.to_lowercase(), p.id);
        idx.pages.insert(p.id, p.clone());
    }
    let mut has_parent: HashSet<Uuid> = HashSet::new();
    let mut has_children: HashSet<Uuid> = HashSet::new();
    for p in pages {
        let parents = parents_of(p);
        let mut linked_any_parent = false;
        for parent_name in parents {
            if let Some(parent_id) = idx.by_basename.get(&parent_name.to_lowercase()).copied() {
                idx.children_of.entry(parent_id).or_default().push(p.id);
                has_children.insert(parent_id);
                has_parent.insert(p.id);
                linked_any_parent = true;
            }
        }
        // If the page declared an `up` but the target doesn't exist
        // in this snapshot, still treat the page as having a
        // parent (just an unresolved one) so we don't double-render
        // it as a root.
        if !linked_any_parent && page_declares_up(p) {
            has_parent.insert(p.id);
        }
    }
    // Stable sort siblings by basename.
    for kids in idx.children_of.values_mut() {
        kids.sort_by(|a, b| {
            let an = idx.pages.get(a).map(|p| p.basename.as_str()).unwrap_or("");
            let bn = idx.pages.get(b).map(|p| p.basename.as_str()).unwrap_or("");
            an.to_lowercase().cmp(&bn.to_lowercase())
        });
    }
    for p in pages {
        let parented = has_parent.contains(&p.id);
        let parental = has_children.contains(&p.id);
        if !parented && parental {
            idx.roots.push(p.id);
        } else if !parented && !parental {
            idx.orphans.push(p.id);
        }
    }
    let sort_key = |id: &Uuid| {
        idx.pages
            .get(id)
            .map(|p| p.basename.to_lowercase())
            .unwrap_or_default()
    };
    idx.roots.sort_by_key(sort_key);
    idx.orphans.sort_by_key(sort_key);
    idx
}

fn page_declares_up(p: &Page) -> bool {
    let fm: serde_json::Value =
        serde_json::from_str(&p.frontmatter_json).unwrap_or(serde_json::Value::Null);
    matches!(fm.get("up"), Some(v) if !v.is_null())
}

/// Filter index against a search query — returns the set of page
/// ids that either match directly or have a descendant that
/// matches (so their ancestors stay expanded during search).
pub fn filter_match(idx: &UpTreeIndex, q: &str) -> Option<HashSet<Uuid>> {
    let q = q.trim().to_lowercase();
    if q.is_empty() {
        return None;
    }
    let mut keep: HashSet<Uuid> = HashSet::new();
    for (id, page) in &idx.pages {
        if page.basename.to_lowercase().contains(&q) {
            keep.insert(*id);
            // Walk up — for every parent of this page, mark it too.
            mark_ancestors(idx, *id, &mut keep, &mut HashSet::new());
        }
    }
    Some(keep)
}

fn mark_ancestors(idx: &UpTreeIndex, id: Uuid, keep: &mut HashSet<Uuid>, seen: &mut HashSet<Uuid>) {
    if !seen.insert(id) {
        return;
    }
    let Some(page) = idx.pages.get(&id) else {
        return;
    };
    for parent_name in parents_of(page) {
        if let Some(pid) = idx.by_basename.get(&parent_name.to_lowercase()).copied() {
            keep.insert(pid);
            mark_ancestors(idx, pid, keep, seen);
        }
    }
}

// ── rendering ─────────────────────────────────────────────────────

#[component]
pub fn UpTreeView(
    pages: Vec<Page>,
    active_page: Option<Uuid>,
    query: String,
    on_select: Callback<Uuid>,
) -> Element {
    let idx = build_index(&pages);
    let filter = filter_match(&idx, &query);
    // Per-node fold state, keyed by page id. Cleared when the
    // component remounts; fine for a first pass.
    let collapsed: Signal<HashSet<Uuid>> = use_signal(HashSet::new);
    let total_roots = idx.roots.len();
    let total_orphans = idx.orphans.len();
    rsx! {
        div { class: "flex flex-col gap-0.5 text-sm",
            if total_roots == 0 && total_orphans == 0 {
                div { class: "px-2 py-1 text-xs text-muted-foreground",
                    "No pages yet."
                }
            }
            for id in idx.roots.iter().copied() {
                {
                    if let Some(filt) = &filter {
                        if !filt.contains(&id) { return rsx! {}; }
                    }
                    rsx! {
                        UpTreeNode {
                            key: "{id}",
                            page_id: id,
                            idx: idx.clone(),
                            depth: 0,
                            active_page,
                            on_select,
                            collapsed,
                            filter: filter.clone(),
                            ancestors: HashSet::new(),
                        }
                    }
                }
            }
            if !idx.orphans.is_empty() {
                OrphansGroup {
                    orphan_ids: idx.orphans.clone(),
                    idx: idx.clone(),
                    active_page,
                    on_select,
                    filter: filter.clone(),
                }
            }
        }
    }
}

#[component]
fn UpTreeNode(
    page_id: Uuid,
    idx: UpTreeIndex,
    depth: usize,
    active_page: Option<Uuid>,
    on_select: Callback<Uuid>,
    collapsed: Signal<HashSet<Uuid>>,
    filter: Option<HashSet<Uuid>>,
    ancestors: HashSet<Uuid>,
) -> Element {
    // Cycle guard: if this page is already an ancestor in the
    // current walk, render only the row (no recursion).
    let is_cycle = ancestors.contains(&page_id);
    let Some(page) = idx.pages.get(&page_id).cloned() else {
        return rsx! {};
    };
    let children = idx.children_of.get(&page_id).cloned().unwrap_or_default();
    let has_children = !children.is_empty() && !is_cycle;
    let is_active = Some(page_id) == active_page;
    let folded = collapsed.read().contains(&page_id);
    let row_testid = format!("up-tree-row-{page_id}");
    let chevron_testid = format!("up-tree-fold-{page_id}");
    let row_cls = if is_active {
        "flex items-center gap-1 rounded px-1 py-0.5 cursor-pointer bg-accent text-accent-foreground"
    } else {
        "flex items-center gap-1 rounded px-1 py-0.5 cursor-pointer text-foreground/85 hover:bg-muted/60 hover:text-foreground transition-colors"
    };
    let pl = format!("padding-left: {}px", depth.min(12) * 12);
    let mut next_ancestors = ancestors.clone();
    next_ancestors.insert(page_id);
    rsx! {
        div {
            "data-testid": row_testid,
            "data-depth": "{depth}",
            class: row_cls,
            style: pl,
            // Fold chevron — only when there are children. Empty
            // pad otherwise so labels align.
            if has_children {
                span {
                    "data-testid": chevron_testid,
                    class: "h-4 w-4 flex-none inline-flex items-center justify-center text-muted-foreground",
                    onclick: move |e: Event<MouseData>| {
                        e.stop_propagation();
                        let mut set = collapsed.peek().clone();
                        if !set.remove(&page_id) {
                            set.insert(page_id);
                        }
                        collapsed.set(set);
                    },
                    if folded { ChevronRight { size: 12 } } else { ChevronDown { size: 12 } }
                }
            } else {
                span { class: "h-4 w-4 flex-none" }
            }
            span {
                class: "flex-1 min-w-0 truncate",
                title: page.basename.clone(),
                onclick: move |_| on_select.call(page_id),
                "{page.basename}"
            }
        }
        if has_children && !folded {
            for cid in children {
                {
                    // Filter: skip subtrees with no match anywhere.
                    if let Some(filt) = &filter {
                        if !filt.contains(&cid) { return rsx! {}; }
                    }
                    rsx! {
                        UpTreeNode {
                            key: "{cid}",
                            page_id: cid,
                            idx: idx.clone(),
                            depth: depth + 1,
                            active_page,
                            on_select,
                            collapsed,
                            filter: filter.clone(),
                            ancestors: next_ancestors.clone(),
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn OrphansGroup(
    orphan_ids: Vec<Uuid>,
    idx: UpTreeIndex,
    active_page: Option<Uuid>,
    on_select: Callback<Uuid>,
    filter: Option<HashSet<Uuid>>,
) -> Element {
    let mut open: Signal<bool> = use_signal(|| true);
    // Apply filter — if a filter is active and no orphan matches,
    // hide the whole group.
    let filtered: Vec<Uuid> = match &filter {
        Some(f) => orphan_ids
            .iter()
            .copied()
            .filter(|id| f.contains(id))
            .collect(),
        None => orphan_ids.clone(),
    };
    if filtered.is_empty() {
        return rsx! {};
    }
    rsx! {
        div { class: "mt-2 pt-2 border-t border-border/40",
            button {
                "data-testid": "up-tree-orphans-toggle",
                class: "w-full flex items-center gap-1 px-1 py-0.5 text-[11px] uppercase tracking-wider text-muted-foreground hover:text-foreground",
                onclick: move |_| {
                    let cur = *open.peek();
                    open.set(!cur);
                },
                span { class: "h-3 w-3 flex-none inline-flex items-center justify-center",
                    if *open.read() { ChevronDown { size: 10 } } else { ChevronRight { size: 10 } }
                }
                span { "Orphans · {filtered.len()}" }
            }
            if *open.read() {
                div { class: "flex flex-col gap-0.5 mt-1",
                    for id in filtered {
                        {
                            let Some(page) = idx.pages.get(&id).cloned() else { return rsx! {}; };
                            let is_active = Some(id) == active_page;
                            let cls = if is_active {
                                "flex items-center gap-1 rounded px-1 py-0.5 cursor-pointer bg-accent text-accent-foreground"
                            } else {
                                "flex items-center gap-1 rounded px-1 py-0.5 cursor-pointer text-foreground/85 hover:bg-muted/60 hover:text-foreground transition-colors"
                            };
                            let tid = format!("up-tree-orphan-{id}");
                            rsx! {
                                div {
                                    key: "{id}",
                                    "data-testid": tid,
                                    class: cls,
                                    style: "padding-left: 16px",
                                    onclick: move |_| on_select.call(id),
                                    span { class: "flex-1 min-w-0 truncate",
                                        title: page.basename.clone(),
                                        "{page.basename}"
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
