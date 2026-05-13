//! `TagExplorer` — hierarchical tag tree.
//!
//! Caller hands in a flat `tag_path → count` map (full slash-separated
//! paths). We build a trie and render each branch through fts-ui's
//! `Collapsible`. Sorting is by descending count then alphabetical.
//!
//! FUTURE: drag-to-rename, color editing, virtual rendering for vaults
//! with thousands of tags. fts-ui's `Collapsible` is fine for shallow
//! trees but every node is its own primitive so deeply nested vaults
//! may want a flat sidebar-style renderer instead.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronRight, Hash};
use fts_ui::prelude::*;
use std::collections::{BTreeMap, HashMap};

#[component]
pub fn TagExplorer(tag_counts: HashMap<String, u32>, on_open_tag: EventHandler<String>) -> Element {
    if tag_counts.is_empty() {
        return rsx! {
            EmptyState { message: "No tags in this vault yet.".to_string() }
        };
    }

    // Build the tag tree.
    let root = build_tree(&tag_counts);

    rsx! {
        div { class: "flex flex-col gap-0.5",
            for child in root.children.into_iter() {
                TagNodeView { node: child, on_open_tag: on_open_tag }
            }
        }
    }
}

#[component]
fn TagNodeView(node: TagNode, on_open_tag: EventHandler<String>) -> Element {
    let has_children = !node.children.is_empty();
    let path_for_click = node.path.clone();
    let path_for_display = node.path.clone();
    let label_for_button = node.label.clone();

    if has_children {
        rsx! {
            Collapsible { default_open: false,
                CollapsibleTrigger {
                    class: "flex w-full items-center gap-1 rounded px-1 py-0.5 hover:bg-accent/40 text-sm".to_string(),
                    span { class: "text-muted-foreground", ChevronRight { size: 12 } }
                    Hash { size: 12 }
                    span { class: "flex-1 truncate text-left", "{label_for_button}" }
                    span { class: "text-xs text-muted-foreground", "{node.count}" }
                }
                CollapsibleContent { class: "pl-3 pt-0.5 space-y-0.5".to_string(),
                    // The folded leaf also picks the parent tag itself.
                    button {
                        class: "flex w-full items-center gap-1 rounded px-1 py-0.5 hover:bg-accent/40 text-xs text-muted-foreground",
                        onclick: move |_| on_open_tag.call(path_for_click.clone()),
                        "(open #{path_for_display})"
                    }
                    for child in node.children.into_iter() {
                        TagNodeView { node: child, on_open_tag: on_open_tag }
                    }
                }
            }
        }
    } else {
        rsx! {
            button {
                class: "flex w-full items-center gap-1 rounded px-1 py-0.5 hover:bg-accent/40 text-sm",
                onclick: move |_| on_open_tag.call(path_for_click.clone()),
                Hash { size: 12 }
                span { class: "flex-1 truncate text-left", "{label_for_button}" }
                span { class: "text-xs text-muted-foreground", "{node.count}" }
            }
        }
    }
}

// ── Tree building ──────────────────────────────────────────────────────

#[derive(Clone, Debug, PartialEq)]
pub struct TagNode {
    pub label: String,
    pub path: String,
    pub count: u32,
    pub children: Vec<TagNode>,
}

fn build_tree(tag_counts: &HashMap<String, u32>) -> TagNode {
    // Use a map keyed by path so we can fold duplicate prefixes.
    let mut nodes: BTreeMap<String, (String, u32, Vec<String>)> = BTreeMap::new();

    for (tag, count) in tag_counts.iter() {
        let parts: Vec<&str> = tag.split('/').filter(|s| !s.is_empty()).collect();
        for i in 0..parts.len() {
            let path = parts[..=i].join("/");
            let label = parts[i].to_string();
            let entry = nodes.entry(path.clone()).or_insert((label, 0, Vec::new()));
            entry.1 += *count;
            if i + 1 < parts.len() {
                let child_path = parts[..=(i + 1)].join("/");
                if !entry.2.contains(&child_path) {
                    entry.2.push(child_path);
                }
            }
        }
    }

    fn build(path: &str, nodes: &BTreeMap<String, (String, u32, Vec<String>)>) -> Vec<TagNode> {
        let (_, _, children) = nodes.get(path).cloned().unwrap_or_default();
        let mut out: Vec<TagNode> = children
            .iter()
            .filter_map(|cp| {
                nodes.get(cp).map(|(label, count, _)| TagNode {
                    label: label.clone(),
                    path: cp.clone(),
                    count: *count,
                    children: build(cp, nodes),
                })
            })
            .collect();
        out.sort_by(|a, b| b.count.cmp(&a.count).then_with(|| a.label.cmp(&b.label)));
        out
    }

    // Roots: paths with no `/`.
    let root_paths: Vec<String> = nodes.keys().filter(|p| !p.contains('/')).cloned().collect();
    let mut root_children: Vec<TagNode> = root_paths
        .iter()
        .filter_map(|p| {
            nodes.get(p).map(|(label, count, _)| TagNode {
                label: label.clone(),
                path: p.clone(),
                count: *count,
                children: build(p, &nodes),
            })
        })
        .collect();
    root_children.sort_by(|a, b| b.count.cmp(&a.count).then_with(|| a.label.cmp(&b.label)));

    TagNode {
        label: String::new(),
        path: String::new(),
        count: 0,
        children: root_children,
    }
}
