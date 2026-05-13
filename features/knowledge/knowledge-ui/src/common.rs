//! Common types reused across the `editor`, `canvas`, and surrounding
//! views (page / quickswitcher / backlinks / embeds).
//!
//! `PageBrief` / `BlockBrief` exist to avoid dragging the full proto
//! types through picker call sites; the editor scaffold and the
//! surrounding UI agree on these shapes.

use chrono::{DateTime, Utc};
use dioxus::prelude::*;
use knowledge_proto::{Block, Page};
use uuid::Uuid;

// ── PageBrief ──────────────────────────────────────────────────────────

/// Page kind, for icon / badge selection in pickers and previews.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PageKind {
    Markdown,
    Canvas,
    Base,
    Journal,
}

impl PageKind {
    pub fn label(self) -> &'static str {
        match self {
            PageKind::Markdown => "md",
            PageKind::Canvas => "canvas",
            PageKind::Base => "base",
            PageKind::Journal => "journal",
        }
    }
}

/// Lightweight projection of a `Page` row.
///
/// `basename` and `title` are the same value, kept under both names to
/// match the editor pickers' existing field access (`basename`) and the
/// surrounding UI's preferred wording (`title`).
#[derive(Clone, Debug, PartialEq)]
pub struct PageBrief {
    pub id: Uuid,
    pub basename: String,
    pub path: String,
    pub aliases: Vec<String>,
    pub kind: PageKind,
}

impl PageBrief {
    /// Display title — same value as `basename`. Surrounding UI prefers
    /// this name; the editor pickers use `basename` directly.
    pub fn title(&self) -> &str {
        &self.basename
    }
}

pub fn page_brief_from(p: &Page) -> PageBrief {
    let kind = if p.is_journal {
        PageKind::Journal
    } else {
        match p.ext.as_str() {
            "canvas" => PageKind::Canvas,
            "base" => PageKind::Base,
            _ => PageKind::Markdown,
        }
    };
    PageBrief {
        id: p.id,
        basename: p.basename.clone(),
        path: p.path.clone(),
        aliases: p.aliases.clone(),
        kind,
    }
}

// ── BlockBrief ─────────────────────────────────────────────────────────

/// Lightweight projection of a `Block` row.
///
/// `page_basename` and `page_title` carry the same value (the source
/// page's display title) — the editor pickers use `page_basename`, the
/// surrounding UI prefers `page_title`.
#[derive(Clone, Debug, PartialEq)]
pub struct BlockBrief {
    pub id: Uuid,
    pub page_id: Uuid,
    pub page_basename: String,
    /// First 80 chars of content, single-line.
    pub preview: String,
    pub kind: String,
}

impl BlockBrief {
    pub fn page_title(&self) -> &str {
        &self.page_basename
    }
}

pub fn block_brief_from(b: &Block, page_title: &str) -> BlockBrief {
    let mut preview = b.content.lines().next().unwrap_or("").trim().to_string();
    if preview.chars().count() > 80 {
        preview = preview.chars().take(80).collect::<String>() + "…";
    }
    BlockBrief {
        id: b.id,
        page_id: b.page_id,
        page_basename: page_title.to_string(),
        preview,
        kind: b.kind.clone(),
    }
}

// ── relative_time ──────────────────────────────────────────────────────

/// Coarse Obsidian-style relative time: `just now`, `5m ago`, `3h ago`,
/// `2d ago`, then the full date.
pub fn relative_time(t: DateTime<Utc>, now: DateTime<Utc>) -> String {
    let secs = (now - t).num_seconds();
    if secs < 30 {
        return "just now".to_string();
    }
    if secs < 60 {
        return format!("{secs}s ago");
    }
    let mins = secs / 60;
    if mins < 60 {
        return format!("{mins}m ago");
    }
    let hrs = mins / 60;
    if hrs < 24 {
        return format!("{hrs}h ago");
    }
    let days = hrs / 24;
    if days < 14 {
        return format!("{days}d ago");
    }
    t.format("%Y-%m-%d").to_string()
}

// ── render_block_readonly ──────────────────────────────────────────────

/// Render a single block's markdown content read-only.
///
/// Shared rendering surface used by `PagePreview`, `BacklinksPanel`,
/// and `BlockEmbed`. Reuses the inline parser from the live-preview
/// editor so wikilinks / tags / code spans display consistently with
/// the active editor view.
///
/// FUTURE: extend with proper rendering for ordered/unordered list
/// nesting + tables once the editor's decoration layer stabilizes.
pub fn render_block_readonly(block: &Block) -> Element {
    use crate::editor::inline_parser::{InlineSpan, SpanKind, parse_inline};

    let spans = parse_inline(&block.content);

    fn render_span(span: &InlineSpan) -> Element {
        match &span.kind {
            SpanKind::Plain(t) => rsx! { span { "{t}" } },
            SpanKind::Bold(t) => rsx! { strong { "{t}" } },
            SpanKind::Italic(t) => rsx! { em { "{t}" } },
            SpanKind::BoldItalic(t) => rsx! { strong { em { "{t}" } } },
            SpanKind::Strike(t) => rsx! { s { "{t}" } },
            SpanKind::Highlight(t) => rsx! { mark { "{t}" } },
            SpanKind::InlineCode(t) => rsx! {
                code { class: "rounded bg-muted px-1 font-mono text-[0.85em]", "{t}" }
            },
            SpanKind::Wikilink { target, alias, .. } => {
                let label = alias.clone().unwrap_or_else(|| target.clone());
                rsx! {
                    span { class: "rounded px-1 font-medium text-primary underline-offset-2 hover:underline",
                        "{label}"
                    }
                }
            }
            SpanKind::Embed { target, alias, .. } => {
                let label = alias.clone().unwrap_or_else(|| target.clone());
                rsx! {
                    span { class: "rounded bg-primary/10 px-1 font-medium text-primary",
                        "{label}"
                    }
                }
            }
            SpanKind::BlockRef { target_id, .. } => {
                let short: String = target_id.to_string().chars().take(8).collect();
                rsx! { span { class: "rounded bg-muted px-1 font-mono text-xs", "(({short}))" } }
            }
            SpanKind::Tag { path, .. } => {
                let label = format!("#{}", path.join("/"));
                rsx! {
                    span { class: "rounded bg-secondary/30 px-1 text-secondary-foreground",
                        "{label}"
                    }
                }
            }
            SpanKind::EntityLink { kind, alias, .. } => {
                let label = alias.clone().unwrap_or_else(|| kind.clone());
                rsx! {
                    span { class: "rounded bg-accent/30 px-1 text-accent-foreground", "{label}" }
                }
            }
            SpanKind::MdLink { text, url, .. } => rsx! {
                a {
                    class: "text-primary underline-offset-2 hover:underline",
                    href: "{url}",
                    "{text}"
                }
            },
            SpanKind::Footnote { id, .. } => rsx! {
                sup { class: "text-xs text-muted-foreground", "[^{id}]" }
            },
            SpanKind::Math { raw } => rsx! {
                span { class: "rounded bg-muted px-1 font-mono text-[0.85em]", "{raw}" }
            },
        }
    }

    let inner: Element = if spans.is_empty() {
        let c = block.content.clone();
        rsx! { "{c}" }
    } else {
        rsx! {
            for span in spans.iter() {
                {render_span(span)}
            }
        }
    };

    match block.kind.as_str() {
        "heading" => {
            let level = block.heading_level.unwrap_or(2).clamp(1, 6);
            let cls = match level {
                1 => "text-2xl font-semibold",
                2 => "text-xl font-semibold",
                3 => "text-lg font-semibold",
                4 => "text-base font-semibold",
                _ => "text-sm font-semibold",
            };
            rsx! { div { class: "{cls}", {inner} } }
        }
        "code" => {
            let content = block.content.clone();
            rsx! {
                pre { class: "overflow-x-auto rounded bg-muted p-2 font-mono text-xs",
                    code { "{content}" }
                }
            }
        }
        "callout" => {
            let kind = block.callout_kind.clone().unwrap_or_else(|| "note".into());
            rsx! {
                div { class: "rounded border-l-4 border-primary bg-muted/40 p-2 text-sm",
                    div { class: "mb-1 text-xs font-semibold uppercase text-muted-foreground",
                        "{kind}"
                    }
                    {inner}
                }
            }
        }
        "blockquote" => rsx! {
            blockquote { class: "border-l-2 border-border pl-3 italic text-muted-foreground",
                {inner}
            }
        },
        "thematic_break" => rsx! { hr { class: "my-2 border-border" } },
        "list_item" => {
            let mark = block.list_task.clone();
            rsx! {
                div { class: "flex items-start gap-2 text-sm",
                    if let Some(m) = mark {
                        span { class: "mt-0.5 font-mono text-xs text-muted-foreground", "[{m}]" }
                    } else {
                        span { class: "mt-1 text-muted-foreground", "•" }
                    }
                    div { class: "flex-1", {inner} }
                }
            }
        }
        _ => rsx! { div { class: "text-sm leading-relaxed", {inner} } },
    }
}
