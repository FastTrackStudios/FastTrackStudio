//! `/wiki` — knowledge-graph view of the vault's `[[wikilink]]` web.
//!
//! Fetches the selected org's vault markdown via `VaultSyncClient`,
//! builds a force-directed graph client-side with
//! [`view_knowledge_graph::build_wiki_graph`], and renders it through
//! the dumb [`KnowledgeGraphView`]. This is the foundation the wiki
//! feature will grow on (swap the data source from vault → wiki tree).

use dioxus::prelude::*;
use fts_ui::prelude::*;
use view_knowledge_graph::{KnowledgeGraphView, WikiGraph, build_wiki_graph};

use crate::orgs::{OrgMeta, OrgSelection, selected_slugs};

#[component]
pub fn WikiView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // Fetch + build once per (org) change. The graph is scoped to one
    // org's vault (the selected org, or the home org when viewing All).
    let graph = use_resource(move || async move {
        let slugs = selected_slugs(&selection.read(), &org_list.read());
        let slug = slugs
            .first()
            .cloned()
            .ok_or_else(|| "no organization selected".to_string())?;
        let files = crate::feeds::fetch_wiki_files(&slug).await?;
        Ok::<WikiGraph, String>(build_wiki_graph(&files))
    });

    let discovering = org_list.read().is_empty();
    let body = if discovering {
        render_loading()
    } else {
        match &*graph.read() {
            Some(Ok(g)) if g.nodes.is_empty() => render_empty(),
            Some(Ok(g)) => {
                let g = g.clone();
                rsx! {
                    div { class: "min-h-0 flex-1 overflow-hidden rounded-xl border border-border/70 bg-card/30",
                        KnowledgeGraphView {
                            graph: g,
                            spacing: 1.6,
                            node_scale: 0.85,
                            on_node_click: move |_id: String| {},
                        }
                    }
                }
            }
            Some(Err(e)) => rsx! {
                div { class: "rounded-xl border border-destructive/40 bg-destructive/10 px-4 py-3 text-sm",
                    "Couldn't build the knowledge graph: {e}"
                }
            },
            None => render_loading(),
        }
    };

    rsx! {
        div { class: "mx-auto flex h-full w-full max-w-6xl flex-col gap-4 p-4 sm:p-6 lg:p-8",
            header { class: "flex flex-col gap-1",
                span { class: "text-[0.7rem] font-semibold uppercase tracking-[0.18em] text-muted-foreground",
                    "Workspace"
                }
                Heading { level: HeadingLevel::H1, class: "tracking-tight", "Knowledge graph" }
                Text {
                    variant: TextVariant::Muted,
                    "The wikilink web of your vault — pages are nodes, `[[links]]` are edges."
                }
            }
            {body}
        }
    }
}

fn render_loading() -> Element {
    rsx! {
        div { class: "flex min-h-0 flex-1 items-center justify-center rounded-xl border border-border/70 bg-card/30",
            Text { variant: TextVariant::Muted, "Building the graph…" }
        }
    }
}

fn render_empty() -> Element {
    rsx! {
        div { class: "flex min-h-0 flex-1 flex-col items-center justify-center gap-2 rounded-2xl border border-dashed border-border/70 bg-card/40 py-16 text-center",
            Heading { level: HeadingLevel::H3, "Nothing to graph yet" }
            Text { variant: TextVariant::Muted, "Add some `[[wikilinks]]` between vault notes and they'll appear here." }
        }
    }
}
