//! The persistent vault explorer — Obsidian's file tree as the app's
//! main sidebar (plans/vault-views.md: the vault is the navigation
//! substrate; pages are views over it).
//!
//! Self-contained: fetches the home org's folder index, renders the
//! same virtual-folder tree the vault page builds, and *navigates*
//! on click (`VaultRoute { path }`) — selection is the current route,
//! so the explorer, deep links, wikilinks, and sidebar shortcuts all
//! agree on what "open" means. Editing affordances (create, move)
//! stay on the vault page; this is the map, not the workshop.

use std::rc::Rc;

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronRight, FileText, SquareKanban};
use fts_ui::prelude::*;

use crate::pages::vault::{TreeNode, build_tree, fetch_folder_index};
use crate::routes::Route;

/// How the explorer organizes the vault. `Tags` is the default —
/// hierarchical tags as virtual folders (`ops/inventory` nests), the
/// TagFolder model; `Folders` is the folder-note tree the vault page
/// also builds.
#[derive(Clone, Copy, PartialEq)]
enum ExplorerMode {
    Tags,
    Folders,
}

/// One virtual folder in the tag tree.
#[derive(Default)]
struct TagNode {
    children: std::collections::BTreeMap<String, TagNode>,
    pages: Vec<vault_proto::PageMeta>,
}

/// Build the tag tree: every page lands under each of its tags
/// (hierarchical on `/`); untagged pages are returned separately.
fn build_tag_tree(pages: &[vault_proto::PageMeta]) -> (TagNode, Vec<vault_proto::PageMeta>) {
    let mut root = TagNode::default();
    let mut untagged = Vec::new();
    for page in pages {
        if page.tags.is_empty() {
            untagged.push(page.clone());
            continue;
        }
        for tag in &page.tags {
            let mut node = &mut root;
            for seg in tag.split('/').filter(|s| !s.is_empty()) {
                node = node.children.entry(seg.to_string()).or_default();
            }
            node.pages.push(page.clone());
        }
    }
    fn sort(node: &mut TagNode) {
        node.pages
            .sort_by(|a, b| a.title.to_lowercase().cmp(&b.title.to_lowercase()));
        for child in node.children.values_mut() {
            sort(child);
        }
    }
    sort(&mut root);
    untagged.sort_by(|a, b| a.title.to_lowercase().cmp(&b.title.to_lowercase()));
    (root, untagged)
}

#[component]
pub fn VaultExplorer() -> Element {
    let org_list = use_context::<Signal<Vec<crate::orgs::OrgMeta>>>();
    let home = use_memo(move || crate::orgs::home_slug(&org_list.read()));
    let files = use_resource(move || {
        let slug = home();
        async move { fetch_folder_index(slug).await }
    });
    let tree = use_memo(move || match &*files.read_unchecked() {
        Some(Ok(pages)) => Some(Rc::new(build_tree(pages))),
        _ => None,
    });

    // Collapsed folder basenames / tag paths. Starts empty (all
    // expanded) — the vault is the home screen now; hiding it by
    // default hides the system.
    let collapsed = use_signal(std::collections::HashSet::<String>::new);
    // Virtual-folder organization: tags by default, folder notes on
    // toggle. FUTURE: persist on the prefs entity.
    let mut mode = use_signal(|| ExplorerMode::Tags);

    // Selection = the current route's vault path.
    let route = use_route::<Route>();
    let selected = match &route {
        Route::VaultRoute { path } => path.clone(),
        _ => String::new(),
    };

    rsx! {
        div { class: "flex h-full min-h-0 flex-col",
            div { class: "flex items-center justify-between px-3 pb-1 pt-3",
                span { class: "text-[0.7rem] font-semibold uppercase tracking-[0.18em] text-muted-foreground",
                    "Vault"
                }
                div { class: "flex items-center gap-0.5 rounded-md bg-muted/40 p-0.5",
                    for (m, label) in [(ExplorerMode::Tags, "Tags"), (ExplorerMode::Folders, "Folders")] {
                        button {
                            key: "{label}",
                            r#type: "button",
                            class: if mode() == m {
                                "rounded px-1.5 py-0.5 text-[0.65rem] font-medium bg-accent text-foreground"
                            } else {
                                "rounded px-1.5 py-0.5 text-[0.65rem] text-muted-foreground hover:text-foreground"
                            },
                            onclick: move |_| mode.set(m),
                            "{label}"
                        }
                    }
                }
            }
            div { class: "min-h-0 flex-1 overflow-y-auto pb-2",
                match &*files.read_unchecked() {
                    Some(Ok(pages)) if mode() == ExplorerMode::Tags => {
                        let (root, untagged) = build_tag_tree(pages);
                        rsx! {
                            nav { class: "flex flex-col gap-px px-1.5",
                                for (seg, node) in &root.children {
                                    {tag_node(seg, node, String::new(), 0, collapsed, selected.clone())}
                                }
                                if !untagged.is_empty() {
                                    div { class: "px-2 pb-0.5 pt-2 text-[0.65rem] font-semibold uppercase tracking-wider text-muted-foreground/70",
                                        "Untagged"
                                    }
                                    for page in &untagged {
                                        {page_row(page, 0, selected.clone())}
                                    }
                                }
                            }
                        }
                    }
                    Some(Ok(_)) => {
                        let t = tree().expect("tree follows files");
                        let nodes = Rc::new(t.0.clone());
                        let roots = t.1.clone();
                        rsx! {
                            nav { class: "flex flex-col gap-px px-1.5",
                                for &root in roots.iter() {
                                    {explorer_node(nodes.clone(), root, 0, collapsed, selected.clone())}
                                }
                            }
                        }
                    }
                    Some(Err(e)) => rsx! {
                        div { class: "px-3 py-2 text-xs text-destructive", "Vault unreachable: {e}" }
                    },
                    None => rsx! {
                        div { class: "flex items-center gap-2 px-3 py-2 text-xs text-muted-foreground",
                            Spinner { size: SpinnerSize::Small }
                            "Loading vault…"
                        }
                    },
                }
            }
            // The sidebar footer keeps its old jobs: presence, account,
            // org — the explorer is the main sidebar now.
            div { class: "max-h-48 overflow-y-auto border-t border-border/60",
                crate::presence::PresenceRoster {}
            }
            div { class: "border-t border-border/60 p-1.5",
                crate::auth::AccountSwitcher {}
            }
            div { class: "px-1.5 pb-2",
                crate::shell::org_switcher::OrgSwitcher { compact: true }
            }
        }
    }
}

fn explorer_node(
    nodes: Rc<Vec<TreeNode>>,
    idx: usize,
    depth: usize,
    mut collapsed: Signal<std::collections::HashSet<String>>,
    selected: String,
) -> Element {
    let node = nodes[idx].clone();
    let nav = use_navigator();
    let is_folder = node.is_folder;
    let is_base = std::path::Path::new(&node.meta.path)
        .extension()
        .is_some_and(|e| e.eq_ignore_ascii_case("base"));
    let key = node.meta.basename.to_lowercase();
    let is_collapsed = collapsed.read().contains(&key);
    let is_selected = !selected.is_empty() && node.meta.path == selected;
    let indent = depth * 12;

    let row_cls = if is_selected {
        "flex w-full items-center gap-1.5 rounded-md bg-accent px-1.5 py-1 text-left text-[13px] text-foreground"
    } else {
        "flex w-full items-center gap-1.5 rounded-md px-1.5 py-1 text-left text-[13px] text-muted-foreground hover:bg-accent/40 hover:text-foreground"
    };
    let toggle_key = key.clone();
    let path = node.meta.path.clone();
    let title = node.meta.title.clone();
    let chevron = if is_collapsed { "" } else { "rotate-90" };

    rsx! {
        div { key: "{node.meta.path}",
            button {
                r#type: "button",
                class: "{row_cls}",
                style: "padding-left: {indent + 6}px",
                onclick: move |_| {
                    if is_folder {
                        let mut set = collapsed.write();
                        if !set.remove(&toggle_key) {
                            set.insert(toggle_key.clone());
                        }
                    } else {
                        nav.push(Route::VaultRoute { path: path.clone() });
                    }
                },
                if is_folder {
                    span { class: "flex h-3.5 w-3.5 shrink-0 items-center justify-center transition-transform {chevron}",
                        ChevronRight { size: 12 }
                    }
                } else if is_base {
                    // Base views get a board glyph — they open as live
                    // views, not text.
                    span { class: "flex h-3.5 w-3.5 shrink-0 items-center justify-center text-primary",
                        SquareKanban { size: 12 }
                    }
                } else {
                    span { class: "flex h-3.5 w-3.5 shrink-0 items-center justify-center",
                        FileText { size: 12 }
                    }
                }
                span { class: "truncate", "{title}" }
            }
            if is_folder && !is_collapsed {
                for &child in node.children.iter() {
                    {explorer_node(nodes.clone(), child, depth + 1, collapsed, selected.clone())}
                }
            }
        }
    }
}

/// One tag virtual folder row + its children (pages, then subtags).
fn tag_node(
    seg: &str,
    node: &TagNode,
    prefix: String,
    depth: usize,
    mut collapsed: Signal<std::collections::HashSet<String>>,
    selected: String,
) -> Element {
    let tag_path = if prefix.is_empty() {
        seg.to_string()
    } else {
        format!("{prefix}/{seg}")
    };
    let key = format!("tag:{tag_path}");
    let is_collapsed = collapsed.read().contains(&key);
    let indent = depth * 12;
    let count = node.pages.len();
    let chevron = if is_collapsed { "" } else { "rotate-90" };
    let toggle_key = key.clone();

    rsx! {
        div { key: "{tag_path}",
            button {
                r#type: "button",
                class: "flex w-full items-center gap-1.5 rounded-md px-1.5 py-1 text-left text-[13px] text-muted-foreground hover:bg-accent/40 hover:text-foreground",
                style: "padding-left: {indent + 6}px",
                onclick: move |_| {
                    let mut set = collapsed.write();
                    if !set.remove(&toggle_key) {
                        set.insert(toggle_key.clone());
                    }
                },
                span { class: "flex h-3.5 w-3.5 shrink-0 items-center justify-center transition-transform {chevron}",
                    ChevronRight { size: 12 }
                }
                span { class: "truncate", "{seg}" }
                if count > 0 {
                    span { class: "ml-auto text-[0.65rem] tabular-nums text-muted-foreground/60", "{count}" }
                }
            }
            if !is_collapsed {
                for page in &node.pages {
                    {page_row(page, depth + 1, selected.clone())}
                }
                for (child_seg, child) in &node.children {
                    {tag_node(child_seg, child, tag_path.clone(), depth + 1, collapsed, selected.clone())}
                }
            }
        }
    }
}

/// A single note row (tag mode) — same look as the folder tree's
/// file rows; clicking navigates.
fn page_row(page: &vault_proto::PageMeta, depth: usize, selected: String) -> Element {
    let nav = use_navigator();
    let is_base = std::path::Path::new(&page.path)
        .extension()
        .is_some_and(|e| e.eq_ignore_ascii_case("base"));
    let is_selected = !selected.is_empty() && page.path == selected;
    let indent = depth * 12;
    let row_cls = if is_selected {
        "flex w-full items-center gap-1.5 rounded-md bg-accent px-1.5 py-1 text-left text-[13px] text-foreground"
    } else {
        "flex w-full items-center gap-1.5 rounded-md px-1.5 py-1 text-left text-[13px] text-muted-foreground hover:bg-accent/40 hover:text-foreground"
    };
    let path = page.path.clone();
    let title = page.title.clone();

    rsx! {
        button {
            key: "{page.path}",
            r#type: "button",
            class: "{row_cls}",
            style: "padding-left: {indent + 6}px",
            onclick: move |_| {
                nav.push(Route::VaultRoute { path: path.clone() });
            },
            if is_base {
                span { class: "flex h-3.5 w-3.5 shrink-0 items-center justify-center text-primary",
                    SquareKanban { size: 12 }
                }
            } else {
                span { class: "flex h-3.5 w-3.5 shrink-0 items-center justify-center",
                    FileText { size: 12 }
                }
            }
            span { class: "truncate", "{title}" }
        }
    }
}
