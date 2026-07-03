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

    // Collapsed folder basenames. Starts empty (all expanded) — the
    // vault is the home screen now; hiding it by default hides the
    // system.
    let collapsed = use_signal(std::collections::HashSet::<String>::new);

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
            }
            div { class: "min-h-0 flex-1 overflow-y-auto pb-2",
                match &*files.read_unchecked() {
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
