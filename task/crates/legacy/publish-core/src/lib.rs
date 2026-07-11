//! Shared vault-content engine for `apps/publish` and
//! `features/knowledge/knowledge-ui`.
//!
//! ## What lives here
//!
//! - [`parser`] — Logseq-flavored inline parser: `[[wikilinks]]`,
//!   `((block-refs))`, `![[Page embeds]]`, `{{query …}}`,
//!   `{{namespace …}}`, plus the GFM core (bold/italic/strike/
//!   highlight/code/external links). Resolution happens at parse
//!   time against caller-supplied resolvers so the rendered tree
//!   carries everything the renderer needs.
//! - [`components`] — Dioxus components that render the parsed
//!   tree: `BlockNode`, `Inlines`, `InlineNode`, plus the
//!   resolver newtypes (`WikiResolver`, `BlockRefResolver`,
//!   `PageEmbedResolver`, `QueryResolver`, `NamespaceResolver`).
//! - [`syntax`] — server-side code-block highlighting via syntect.
//!
//! Callers provide resolvers via Dioxus `use_context_provider`;
//! every render arm reads them via `try_use_context`, so each
//! surface (static-site, live editor) plugs in its own data
//! without changing the contract.

pub mod components;
pub mod edit_html;
pub mod parser;
pub mod syntax;

pub use edit_html::{escape_html, render_edit_html};

pub use components::{
    AssetBaseResolver, BacklinkEntry, BacklinksPanel, BlockNode, BlockRefNavigator,
    BlockRefResolver, BlockRefTarget, DocBody, Drawer, InlineNode, Inlines, NamespaceResolver,
    PageContent, PageEmbedResolver, PagePropertyResolver, PageTreeNode, PlanningTimestamps,
    QueryHit, QueryResolver, Sidebar, TagNavigator, TaskMarker, TemplateResolver, WikiNavigator,
    WikiPreviewResolver, WikiResolver, build_page_tree, extract_youtube_id, parse_property_line,
    parse_props, peel_block_properties, peel_drawers, peel_planning, peel_task_marker, slugify,
};
pub use parser::{Node, parse};
