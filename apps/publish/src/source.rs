//! Trait abstraction for "what counts as a publishable page".
//!
//! v1 of the publisher hard-codes Task-architect's
//! `knowledge_proto::{Page, Block}` as the input shape. This
//! module sketches the v2 generalization: any data source that
//! can produce `(PageMeta, Vec<BlockKind>)` per page can drive
//! the same pipeline. Useful for:
//!
//! - Doc sites authored as raw Markdown trees on disk
//! - Sites authored in another tool (Notion export, Roam export)
//! - Sites where the user wants to embed arbitrary Dioxus
//!   components per page (e.g. `<DemoChart data="…" />` in the
//!   middle of a doc)
//!
//! Today the publisher consumes the concrete types directly;
//! this trait is documented + exported for downstream callers
//! to use as the integration seam. When we ship a non-vault
//! input source, this is the API it'll plug into.
//!
//! ## Status
//!
//! Defined but not yet wired through `site::build` — that
//! refactor follows when the second input source lands. Marker
//! traits + types are public so external crates can start
//! implementing against them.

use std::sync::Arc;

/// Small subset of metadata the publisher needs to render a
/// page. Anything more domain-specific (folder, tags, journal
/// day) lives on extensions provided via [`PublishablePage::extras`].
#[derive(Clone, Debug)]
pub struct PageMeta {
    /// Human-readable title (used as `<h1>` and in nav).
    pub title: String,
    /// URL-safe slug. The publisher will use this verbatim as
    /// the directory under `out/`.
    pub slug: String,
    /// Optional ISO date (`YYYY-MM-DD`) shown under the title.
    /// For journals/blogs.
    pub date: Option<String>,
    /// Tags this page should appear under in `/tags/`.
    pub tags: Vec<String>,
}

/// What renders inside one page's `<article>`. Markdown-ish
/// blocks today; future variants will let users plug arbitrary
/// Dioxus components.
#[derive(Clone, Debug)]
pub enum PageBlock {
    /// Heading. `level` is 1..=6.
    Heading { level: u8, text: String },
    /// Plain paragraph text — markdown-flavored. Inline parser
    /// runs over it (wikilinks, emphasis, etc.).
    Paragraph(String),
    /// Code block. `lang` drives syntax highlighting.
    Code { lang: String, text: String },
    /// List item, optionally with a task-state marker.
    ListItem {
        text: String,
        task: Option<String>, // " ", "x", "/", etc.
    },
    /// Raw HTML — escape hatch for content sources that have
    /// already rendered some markup. Use with care.
    Html(String),
}

/// The trait every input source implements.
pub trait PublishableSource: Send + Sync {
    /// Iterate every publishable page in the source.
    fn pages(&self) -> Vec<Arc<dyn PublishablePage>>;

    /// Wikilink resolution table. Keys are lowercased basenames,
    /// values are the slugs they point to. The publisher uses
    /// this so `[[wikilinks]]` resolve cross-page during render.
    fn wiki_resolver(&self) -> std::collections::HashMap<String, String>;
}

/// One page's rendered shape.
pub trait PublishablePage: Send + Sync {
    fn meta(&self) -> PageMeta;
    fn blocks(&self) -> Vec<PageBlock>;
}
