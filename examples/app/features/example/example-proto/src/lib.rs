//! `example-proto` — the canonical `architect` usage example. **Start
//! here:** this one file is the contract the whole stack is generated
//! from. End-to-end walkthrough: `docs/content/getting-started/walkthrough.md`.
//!
//! One struct definition. One source of truth. The
//! [`architect::Entity`] derive does the rest:
//!
//! - Wasm builds get the wire struct `Example` with `facet::Facet`
//!   derived, the `ExampleCreate`/`ExampleUpdate`/`ExampleList`
//!   payload types, the `ExampleRepo` `#[vox::service]` trait (→ the
//!   auto-generated `ExampleRepoClient` you call from the browser), and
//!   an `ExampleRepoLayer` token so the repo composes through the layer
//!   system (`layers![…]` / `.into_router()`).
//! - Server builds (`--features server-seaorm`) additionally get the
//!   SeaORM `Model` + `Entity` + `Column` + `Relation` + `ActiveModel`,
//!   plus `ExampleRepoStorage<C>` that implements the repo trait against
//!   a SeaORM connection (with its `Services` bundle emitted too).
//!
//! No `cfg_attr` in the struct, no parallel definitions, no manual
//! `From` impls — architect emits the storage<->wire bridge for you.
//!
//! Backends (`example-memory`, `example-db`, `example-crdt`,
//! `examples/external-stub`) each `impl ExampleRepo` + declare a
//! `Services` bundle; the server composes them via
//! `app_server::service_router`; clients consume them over a remote or
//! in-process `Transport` (`examples/app/ui/src/client.rs`).

// Re-exported so downstream test/client crates can reach Page/Sort/Filter
// without taking a direct architect dep.
pub use architect;

use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(architect::Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(
    // SeaORM table this entity persists to. Architect emits the
    // `#[sea_orm(table_name = "examples")]` decoration internally.
    table_name = "examples",
    // Emit the `ExampleRepo` vox trait + the server-side
    // `ExampleRepoStorage<C>` impl. Most entities want this; opt out
    // by removing this flag if you're using a custom storage layer.
    repo,
)]
pub struct Example {
    /// Stable identifier. `on_create = Uuid::new_v4()` runs the
    /// expression server-side when inserting a new row.
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Free-text label. `filterable` adds query-param support on the
    /// repo's `list` method; `fulltext` joins it into FTS5 search.
    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    /// Longer description. Searchable via fulltext as well.
    #[architect(filterable, fulltext)]
    pub description: String,

    /// Audit timestamp. Excluded from create/update payloads (clients
    /// can't set it); populated by the macro's storage layer on insert.
    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Service trait ──────────────────────────────────────────────────────
//
// Plain `#[vox::service]` trait. Architect does not touch this. Standard
// CRUD (`list_examples`, `get_example`, `create_example`, etc.) lives on
// the auto-generated `ExampleRepo` — the server mounts both Repo + Service
// dispatchers, so clients call whichever surface fits the call site.
//
// The Service is reserved for domain operations that don't map cleanly
// onto CRUD: full-text search, multi-row mutations, workflow steps, etc.

#[derive(Debug, Clone, PartialEq, Eq, facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum ExampleServiceError {
    #[error("example not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

// Gated on the `vox` feature exactly like the architect-emitted
// `ExampleRepo` trait — drop the vox feature and the trait is plain
// async-in-trait without an RPC surface.
#[cfg_attr(feature = "vox", vox::service)]
pub trait ExampleService {
    /// Full-text search across `name` + `description`. Returns at most
    /// `limit` matches (server clamps to a sane max).
    async fn search(&self, query: String, limit: u32) -> Result<Vec<Example>, ExampleServiceError>;

    /// Duplicate an existing example, optionally overriding the name on
    /// the new row. Returns the freshly inserted record.
    async fn duplicate(
        &self,
        id: Uuid,
        new_name: Option<String>,
    ) -> Result<Example, ExampleServiceError>;
}
