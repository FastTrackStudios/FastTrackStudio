//! Render Dioxus components to HTML strings via `dioxus-ssr`.
//!
//! Each per-page document = one `VirtualDom`. The components
//! themselves live in [`crate::components`]; this module just
//! drives them and wraps the output in the static `<html>` shell.

use crate::components::{BacklinksPanel, DocBody, PageContent, Sidebar, WikiResolver};
use crate::graph::{BacklinkEntry, GraphView};
use crate::tags::TagPageEntry;
use dioxus::prelude::*;
use knowledge_proto::{Block, Page};

/// Render a single page to a complete HTML document string.
pub fn render_page(
    site_title: &str,
    page: &Page,
    blocks: &[Block],
    resolver: &WikiResolver,
    all_pages: &[Page],
    backlinks: &[BacklinkEntry],
) -> String {
    let site_title = site_title.to_string();
    let page_title = page.basename.clone();
    let blocks = blocks.to_vec();
    let pages = all_pages.to_vec();
    let current_id = page.id;
    let journal_day = page.journal_day.clone();
    let resolver_for_root = resolver.clone();
    let backlinks = backlinks.to_vec();

    let mut vdom = VirtualDom::new_with_props(
        Root,
        RootProps {
            site_title,
            page_title,
            blocks,
            pages,
            current_id,
            journal_day,
            resolver: resolver_for_root,
            backlinks,
        },
    );
    vdom.rebuild_in_place();
    let body = dioxus_ssr::render(&vdom);
    shell(&page.basename, body, "")
}

/// Render a single tag aggregation page — list of pages tagged with `tag`.
pub fn render_tag_page(
    site_title: &str,
    tag: &str,
    entries: &[TagPageEntry],
    pages: &[Page],
) -> String {
    let mut vdom = VirtualDom::new_with_props(
        TagPageRoot,
        TagPageRootProps {
            site_title: site_title.to_string(),
            tag: tag.to_string(),
            entries: entries.to_vec(),
            pages: pages.to_vec(),
        },
    );
    vdom.rebuild_in_place();
    let body = dioxus_ssr::render(&vdom);
    shell(&format!("#{tag}"), body, "")
}

#[component]
fn TagPageRoot(
    site_title: String,
    tag: String,
    entries: Vec<TagPageEntry>,
    pages: Vec<Page>,
) -> Element {
    rsx! {
        DocBody {
            site_title: site_title,
            page_title: format!("#{tag}"),
            sidebar: rsx! {
                Sidebar { pages: pages, current_id: None }
            },
            body: rsx! {
                article { class: "content",
                    p { class: "page-meta", "Pages tagged with #{tag}" }
                    ul { class: "page-index",
                        for e in entries {
                            {
                                let href = format!("/{}/", e.slug);
                                rsx! {
                                    li { key: "{e.slug}",
                                        a { href: "{href}", "{e.label}" }
                                    }
                                }
                            }
                        }
                    }
                }
            },
        }
    }
}

/// Render `/tags/` — index of every tag with page counts.
pub fn render_tags_index(site_title: &str, tags: &[(String, usize)], pages: &[Page]) -> String {
    let mut vdom = VirtualDom::new_with_props(
        TagsIndexRoot,
        TagsIndexRootProps {
            site_title: site_title.to_string(),
            tags: tags.to_vec(),
            pages: pages.to_vec(),
        },
    );
    vdom.rebuild_in_place();
    let body = dioxus_ssr::render(&vdom);
    shell("Tags", body, "")
}

#[component]
fn TagsIndexRoot(site_title: String, tags: Vec<(String, usize)>, pages: Vec<Page>) -> Element {
    rsx! {
        DocBody {
            site_title: site_title,
            page_title: "Tags".to_string(),
            sidebar: rsx! {
                Sidebar { pages: pages, current_id: None }
            },
            body: rsx! {
                article { class: "content",
                    ul { class: "tag-index",
                        for (tag, n) in tags {
                            {
                                let href = format!("/tags/{}/", tag);
                                rsx! {
                                    li { key: "{tag}",
                                        a { href: "{href}", "#{tag}" }
                                        span { class: "tag-count", " {n}" }
                                    }
                                }
                            }
                        }
                    }
                }
            },
        }
    }
}

/// Render the graph view page — sidebar + a single canvas
/// hosting the wikilink force-directed graph.
pub fn render_graph_page(site_title: &str, pages: &[Page]) -> String {
    let mut vdom = VirtualDom::new_with_props(
        GraphPageRoot,
        GraphPageRootProps {
            site_title: site_title.to_string(),
            pages: pages.to_vec(),
        },
    );
    vdom.rebuild_in_place();
    let body = dioxus_ssr::render(&vdom);
    shell("Graph", body, "")
}

#[component]
fn GraphPageRoot(site_title: String, pages: Vec<Page>) -> Element {
    rsx! {
        DocBody {
            site_title: site_title,
            page_title: "Graph".to_string(),
            sidebar: rsx! {
                Sidebar { pages: pages, current_id: None }
            },
            body: rsx! {
                article { class: "content",
                    p { class: "page-meta",
                        "Wikilink graph — hover a node to highlight its neighborhood, click to open the page."
                    }
                    GraphView { height_px: 600 }
                }
            },
        }
    }
}

/// Render the landing page (page list).
pub fn render_index(site_title: &str, pages: &[Page]) -> String {
    let mut vdom = VirtualDom::new_with_props(
        IndexRoot,
        IndexRootProps {
            site_title: site_title.to_string(),
            pages: pages.to_vec(),
        },
    );
    vdom.rebuild_in_place();
    let body = dioxus_ssr::render(&vdom);
    shell(site_title, body, "")
}

/// Per-page root: provides WikiResolver context, renders body.
#[component]
fn Root(
    site_title: String,
    page_title: String,
    blocks: Vec<Block>,
    pages: Vec<Page>,
    current_id: uuid::Uuid,
    journal_day: Option<String>,
    resolver: WikiResolver,
    backlinks: Vec<BacklinkEntry>,
) -> Element {
    use_context_provider(|| resolver);
    rsx! {
        DocBody {
            site_title: site_title,
            page_title: page_title.clone(),
            sidebar: rsx! {
                Sidebar { pages: pages, current_id: Some(current_id) }
            },
            body: rsx! {
                PageContent { blocks: blocks, journal_day: journal_day }
                BacklinksPanel { entries: backlinks }
            },
        }
    }
}

/// Index root — minimal sidebar (none today), page list as body.
#[component]
fn IndexRoot(site_title: String, pages: Vec<Page>) -> Element {
    let mut sorted = pages.clone();
    sorted.sort_by(|a, b| a.basename.to_lowercase().cmp(&b.basename.to_lowercase()));
    rsx! {
        DocBody {
            site_title: site_title.clone(),
            page_title: site_title,
            sidebar: rsx! {},
            body: rsx! {
                article { class: "content",
                    ul { class: "page-index",
                        for p in sorted {
                            {
                                let slug = crate::site::slugify(&p.basename);
                                rsx! {
                                    li { key: "{p.id}",
                                        a { href: "/{slug}/", "{p.basename}" }
                                    }
                                }
                            }
                        }
                    }
                }
            },
        }
    }
}

/// Static HTML shell. Wraps Dioxus-rendered body markup in a
/// proper `<html>`/`<head>`/`<body>` document. `extra_head`
/// slots in per-page tags (OG cards, canonical, etc.) when
/// later phases need them.
fn shell(page_title: &str, body: String, extra_head: &str) -> String {
    format!(
        r##"<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>{page_title}</title>
<link rel="stylesheet" href="/assets/style.css">
{extra_head}
</head>
<body>
{body}
<script src="/assets/katex_loader.js" defer></script>
</body>
</html>
"##,
        page_title = html_escape(page_title),
    )
}

fn html_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '&' => out.push_str("&amp;"),
            '"' => out.push_str("&quot;"),
            '\'' => out.push_str("&#39;"),
            _ => out.push(c),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;
    use std::sync::Arc;

    fn book() -> WikiResolver {
        let mut m = HashMap::new();
        m.insert("foo".into(), "foo".into());
        WikiResolver(Arc::new(m))
    }

    fn block(content: &str) -> Block {
        Block {
            id: uuid::Uuid::new_v4(),
            vault_id: uuid::Uuid::nil(),
            page_id: uuid::Uuid::nil(),
            parent_block_id: None,
            sort_key: "a".into(),
            content: content.into(),
            kind: "paragraph".into(),
            heading_level: None,
            list_ordered: false,
            list_task: None,
            code_lang: None,
            callout_kind: None,
            callout_foldable: false,
            properties_json: "{}".into(),
            obsidian_block_id: None,
            refs_json: "[]".into(),
            canvas_node_json: None,
            collapsed: false,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
        }
    }

    fn page(name: &str) -> Page {
        Page {
            id: uuid::Uuid::new_v4(),
            vault_id: uuid::Uuid::nil(),
            folder_id: None,
            path: format!("{}.md", name),
            basename: name.into(),
            ext: "md".into(),
            aliases: Vec::new(),
            frontmatter_json: "{}".into(),
            stat_ctime: chrono::Utc::now(),
            stat_mtime: chrono::Utc::now(),
            stat_size: 0,
            is_journal: false,
            journal_day: None,
            shadow_for_kind: None,
            shadow_for_id: None,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
        }
    }

    #[test]
    fn renders_basic_page() {
        let p = page("Test");
        let blocks = vec![block("Hello [[Foo]]")];
        let pages = vec![p.clone()];
        let html = render_page("Site", &p, &blocks, &book(), &pages, &[]);
        assert!(html.contains("<!DOCTYPE html>"));
        assert!(html.contains("Test"));
        assert!(html.contains(r#"href="/foo/""#), "{html}");
    }

    #[test]
    fn renders_index() {
        let pages = vec![page("Alpha"), page("Beta")];
        let html = render_index("Site", &pages);
        assert!(html.contains("Alpha"));
        assert!(html.contains("Beta"));
        // Alphabetical order.
        let alpha = html.find("Alpha").unwrap();
        let beta = html.find("Beta").unwrap();
        assert!(alpha < beta);
    }
}
