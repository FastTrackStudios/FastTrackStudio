//! `example-proto` — the canonical `architect` usage example.
//!
//! One struct definition. One source of truth. The
//! [`architect::Entity`] derive does the rest:
//!
//! - Wasm builds get the wire struct `Example` with `facet::Facet`
//!   derived, the `ExampleCreate`/`ExampleUpdate`/`ExampleList`
//!   payload types, the `ExampleRepo` `#[vox::service]` trait, and
//!   the auto-generated `ExampleClient` you call from the browser.
//! - Server builds (`--features server`) additionally get the SeaORM
//!   `Model` + `Entity` + `Column` + `Relation` + `ActiveModel`, plus
//!   `ExampleRepoStorage<C>` that implements the repo trait against
//!   a SeaORM connection.
//!
//! No `cfg_attr` in the struct, no parallel definitions, no manual
//! `From` impls — architect emits the storage<->wire bridge for you.

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[derive(Entity)]
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

    /// Audit timestamps managed by architect. Excluded from create
    /// + update payloads (clients can't set them); populated by the
    /// macro's storage layer on insert / update.
    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}
