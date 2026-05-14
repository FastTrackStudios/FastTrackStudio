//! `knowledge-proto` — wire contract for the `knowledge` feature.
//!
//! Obsidian-compatible PKM/outliner. Seven top-level entities:
//!
//! - `Vault`         — a workspace; `root_path = Some(_)` for mounted
//!                     Obsidian dirs, `None` for app-only ephemeral
//! - `Folder`        — first-class so empty folders persist + match
//!                     Obsidian's `getAllFolders` semantics
//! - `Page`          — a single markdown / canvas / base file
//! - `Block`         — one paragraph / list-item / heading / etc.
//!                     under a Page; UUID is stable across moves
//! - `KnowledgeTag`  — a globally-known `#tag` (color, description);
//!                     prefixed `Knowledge` to avoid collisions
//! - `Base`          — an Obsidian `.base` definition (filter + views)
//!
//! Plus an `OutlinerEmbed` shadow-page UUIDv5 helper so other
//! features can embed a knowledge outliner against their own entity
//! ids without bloating their proto with a `notes_page_id` field.
//!
//! The on-disk-round-trippable Obsidian helpers (parse_frontmatter,
//! parse_page, serialize_page, extract_refs, resolve_linkpath, …)
//! live in the `obsidian` module — pure functions, no Loro/Repo
//! dependency.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

pub mod bases;
pub mod canvas;
pub mod export;
pub mod lexorank;
pub mod obsidian;
pub mod property_schema;
pub mod refs;
pub mod shadow;

pub use canvas::{CanvasNode, CanvasShape};
pub use refs::{BlockRef, EmbedRef, EntityRef, LinkRef, Ref, TagRef};
pub use shadow::shadow_page_id;

// ── Const enums for non-DB validation ────────────────────────────────

pub const NEW_LINK_FORMATS: &[&str] = &["shortest", "relative", "absolute"];
pub const VIEW_MODES: &[&str] = &["source", "live-preview", "preview"];

pub const BLOCK_KINDS: &[&str] = &[
    "paragraph",
    "heading",
    "list_item",
    "code",
    "callout",
    "blockquote",
    "table",
    "thematic_break",
    "html",
    "yaml",
    "embed",
    "canvas",
];

/// Obsidian task marker characters (the `x` inside `- [x]`). The
/// minimal `" "` / `"x"` set covers the on-disk Obsidian standard;
/// the additional marks come from the Tasks community plugin and are
/// preserved on round-trip but not interpreted in v1.
pub const TASK_MARKS: &[&str] = &[" ", "x", "/", "-", ">", "<", "?", "!"];

pub const BASE_VIEW_KINDS: &[&str] = &["table", "board", "gallery", "calendar", "list"];

// ── Vault ─────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "vaults", repo)]
pub struct Vault {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    /// `None` for ephemeral/app-only vaults, `Some` for a mounted
    /// Obsidian directory. The on-disk sync crate (Phase C) is the
    /// only consumer that cares about this — Phase A treats every
    /// vault as ephemeral.
    pub root_path: Option<String>,

    /// `.obsidian/app.json` → `useMarkdownLinks`.
    pub use_markdown_links: bool,

    /// `.obsidian/app.json` → `newLinkFormat`. One of
    /// `NEW_LINK_FORMATS`.
    pub new_link_format: String,

    /// `""` means vault root.
    pub attachment_folder_path: String,

    /// One of `VIEW_MODES`.
    pub default_view_mode: String,

    /// Unparsed `.obsidian/app.json` blob so unknown keys survive a
    /// round-trip. Sync crate replaces this whenever it writes the
    /// config back to disk.
    pub config_json: String,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Folder ────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "folders", repo)]
pub struct Folder {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub vault_id: Uuid,

    /// Vault-relative; no leading slash. Identifies the folder
    /// uniquely within its vault.
    #[architect(filterable, sortable, fulltext)]
    pub path: String,

    #[architect(filterable)]
    pub parent_id: Option<Uuid>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Page ──────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "pages", repo)]
pub struct Page {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub vault_id: Uuid,

    #[architect(filterable)]
    pub folder_id: Option<Uuid>,

    /// Vault-relative path including extension, e.g. `notes/Hello.md`.
    #[architect(filterable, fulltext)]
    pub path: String,

    /// Filename without extension — what Obsidian uses as the link
    /// target when `newLinkFormat = "shortest"`.
    #[architect(filterable, sortable, fulltext)]
    pub basename: String,

    /// `md` / `canvas` / `base` / other.
    #[architect(filterable)]
    pub ext: String,

    /// Frontmatter `aliases:` field, materialized for lookups.
    #[architect(json)]
    pub aliases: Vec<String>,

    /// Order-preserving frontmatter (via `IndexMap`-backed serde).
    /// Stored as a JSON blob keyed by string; consumers should
    /// deserialize through `serde_json::from_str` into an
    /// `IndexMap<String, serde_json::Value>` to retain key order.
    pub frontmatter_json: String,

    /// Round-tripped from the filesystem when mounted from disk.
    pub stat_ctime: DateTime<Utc>,
    pub stat_mtime: DateTime<Utc>,
    pub stat_size: i64,

    /// True when the page lives under the journal folder + matches
    /// the journal-naming convention. Sync crate sets this.
    #[architect(filterable)]
    pub is_journal: bool,

    /// ISO `YYYY-MM-DD` string. NOTE: spec asked for
    /// `chrono::NaiveDate` but the Loro codec layer only handles
    /// `DateTime<Utc>`; we keep this as a string to avoid forcing a
    /// crdt-layer expansion in Phase A. Sync crate parses it back
    /// when needed.
    pub journal_day: Option<String>,

    /// When this page is a shadow page for another feature's entity
    /// (e.g. Task notes), this is the kind (`"task"`, `"project"`,
    /// …). Together with `shadow_for_id` it can be reverse-resolved
    /// from `shadow_page_id`.
    #[architect(filterable)]
    pub shadow_for_kind: Option<String>,

    #[architect(filterable)]
    pub shadow_for_id: Option<Uuid>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Block ─────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "blocks", repo)]
pub struct Block {
    /// Stable across moves (rename / parent change / page move).
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Denormalized for fast vault-scoped queries.
    #[architect(filterable)]
    pub vault_id: Uuid,

    #[architect(filterable)]
    pub page_id: Uuid,

    #[architect(filterable)]
    pub parent_block_id: Option<Uuid>,

    /// Fractional-index string. Compare lexicographically to get
    /// in-order traversal; insert between two siblings by picking a
    /// key that sorts strictly between them.
    #[architect(sortable)]
    pub sort_key: String,

    /// One of `BLOCK_KINDS`.
    #[architect(filterable)]
    pub kind: String,

    /// Raw markdown for this block. One block ≈ one paragraph /
    /// list-item / heading / etc.
    ///
    /// FUTURE: upgrade `Block.content` to `LoroText` for
    /// character-level CRDT merges. Currently full-string LWW per
    /// write, which is fine for the editor patterns in Phase A but
    /// will need revisiting once two clients can edit the same
    /// block concurrently.
    pub content: String,

    /// `1..=6` when `kind == "heading"`.
    pub heading_level: Option<i32>,

    /// `true` for `1.`-style ordered lists.
    pub list_ordered: bool,

    /// `None` for non-task list items, `Some(" ")` for unchecked,
    /// `Some("x")` for checked, `Some("/")` for in-progress, etc.
    /// See `TASK_MARKS`.
    pub list_task: Option<String>,

    /// Set when `kind == "code"`.
    pub code_lang: Option<String>,

    /// Callout kind — e.g. `"note"`, `"warning"`, `"danger"`. Only
    /// meaningful when `kind == "callout"`.
    pub callout_kind: Option<String>,

    pub callout_foldable: bool,

    /// Logseq-style `prop:: value` lines parsed out of the block
    /// content. Stored as a JSON blob; key order is preserved by
    /// IndexMap on the serde round-trip.
    pub properties_json: String,

    /// The textual `^id` trailing anchor that round-trips back into
    /// the markdown. Distinct from `Block.id` (which is a Uuid).
    pub obsidian_block_id: Option<String>,

    pub collapsed: bool,

    /// `Vec<Ref>` serialized as JSON; computed from `content` on
    /// save. Reads can use this without re-parsing the markdown.
    pub refs_json: String,

    /// Canvas placement when this block lives on a whiteboard Page
    /// (`Page.ext == "canvas"`). `None` for normal outliner blocks.
    /// Carries `CanvasNode` (see `knowledge_proto::canvas`) serialized
    /// as JSON: x/y/w/h/z/rotation + `CanvasShape` variant + color +
    /// locked. Stored as a JSON blob so the canvas schema can grow
    /// without proto migrations.
    pub canvas_node_json: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── KnowledgeTag ──────────────────────────────────────────────────────

/// Renamed from spec's `Tag` to avoid collision with any other
/// feature's `Tag`. Internally we never reference it by short name.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "knowledge_tags", repo)]
pub struct KnowledgeTag {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub vault_id: Uuid,

    /// Full path, e.g. `"projects/alpha"`.
    #[architect(filterable, fulltext)]
    pub tag: String,

    pub color: Option<String>,

    pub description: Option<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Base (.base file from Obsidian Bases) ─────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "bases", repo)]
pub struct Base {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub vault_id: Uuid,

    /// `Some` when the `.base` file lives on disk as a page, `None`
    /// for app-authored (in-memory) bases.
    pub page_id: Option<Uuid>,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    /// Raw YAML; round-trippable.
    pub definition_yaml: String,

    /// `FilterNode` tree as JSON. The exact shape lives in a future
    /// `bases::filter` module; for Phase A this is opaque.
    pub parsed_filter_json: String,

    /// `Vec<ViewSpec>` as JSON. View kinds come from
    /// `BASE_VIEW_KINDS`.
    pub parsed_views_json: String,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── KnowledgeService ──────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum KnowledgeServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

/// Server-only outliner-verb surface. The Loro repos already give
/// every consumer raw CRUD; this trait packages the higher-level
/// edits the live-preview editor calls into (insert / split / merge
/// / indent / outdent / move / page-as-markdown).
#[cfg_attr(feature = "vox", vox::service)]
pub trait KnowledgeService {
    /// Find-or-create the shadow page for an embedding entity.
    async fn ensure_shadow_page(
        &self,
        entity_kind: String,
        entity_id: Uuid,
        vault_id: Uuid,
    ) -> Result<Uuid, KnowledgeServiceError>;

    /// Serialize a page back to canonical markdown.
    async fn page_as_markdown(&self, page_id: Uuid) -> Result<String, KnowledgeServiceError>;

    /// Re-parse a page from a new markdown blob, reconciling blocks
    /// by `obsidian_block_id` first then by content similarity.
    async fn edit_page_markdown(
        &self,
        page_id: Uuid,
        new_markdown: String,
    ) -> Result<(), KnowledgeServiceError>;

    // ── Outliner verbs ────────────────────────────────────────────

    async fn insert_block_after(
        &self,
        sibling: Uuid,
        content: String,
    ) -> Result<Uuid, KnowledgeServiceError>;
    async fn split_block(
        &self,
        block_id: Uuid,
        byte_offset: u32,
    ) -> Result<Uuid, KnowledgeServiceError>;
    async fn merge_blocks(&self, first: Uuid, second: Uuid) -> Result<(), KnowledgeServiceError>;
    async fn indent(&self, block: Uuid) -> Result<(), KnowledgeServiceError>;
    async fn outdent(&self, block: Uuid) -> Result<(), KnowledgeServiceError>;
    async fn move_up(&self, block: Uuid) -> Result<(), KnowledgeServiceError>;
    async fn move_down(&self, block: Uuid) -> Result<(), KnowledgeServiceError>;
}
