//! `example-proto` — the schema for the `Example` entity.
//!
//! This crate is **wasm-clean** by default: `cargo build --target
//! wasm32-unknown-unknown` works without enabling any features. The
//! `server` feature pulls in SeaORM derives + crudcrate's storage glue
//! so the same struct definition powers the wire DTO *and* the
//! database row.
//!
//! ## What's in here
//!
//! - [`Example`] — the canonical record. Hand-written below; the
//!   crudcrate `EntityToModels` macro decorates it server-side.
//! - [`ExampleRepo`] (server feature) — auto-generated vox trait with
//!   `get_example` / `list_examples` / `create_example` /
//!   `update_example` / `delete_example`. Pure CRUD; no business logic.
//! - [`ExampleService`] — hand-written vox trait. The domain surface
//!   our clients call. It composes one or more repos plus any
//!   validation / authorization / event emission logic.
//!
//! ## Why this split
//!
//! Repos answer the question "give me/save the row". Services answer
//! "do this user-visible thing", which usually involves more than one
//! row, plus business rules. Keeping them separate keeps the generated
//! Repo trait dumb (no auth, no validation — those belong in the
//! service or the storage backend) and keeps the Service trait small
//! and reviewable.

use chrono::{DateTime, Utc};
use facet::Facet;
use uuid::Uuid;

// ── The Example record ─────────────────────────────────────────────────
//
// One struct, two derive sets. With `--features server` we also derive
// SeaORM's `DeriveEntityModel` and crudcrate's `EntityToModels` (which
// emits the `Example` API struct, `ExampleCreate`, `ExampleUpdate`,
// `ExampleList`, the `CRUDResource` impl, plus the `ExampleRepo` vox
// trait + `ExampleRepoStorage` dispatcher). Wasm clients see only the
// hand-written struct below.

#[cfg(not(feature = "server"))]
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
pub struct Example {
    pub id: Uuid,
    pub name: String,
    pub description: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

// Server-side: crudcrate generates `Example` + the rest from the
// SeaORM Model below. That `Example` API struct carries the same
// fields, so wasm/server code can both `use example_proto::Example`.
#[cfg(feature = "server")]
pub use server::*;

#[cfg(feature = "server")]
mod server {
    use chrono::{DateTime, Utc};
    use crudcrate::EntityToModels;
    use sea_orm::entity::prelude::*;
    use uuid::Uuid;

    #[derive(Clone, Debug, PartialEq, DeriveEntityModel, EntityToModels)]
    #[sea_orm(table_name = "examples")]
    #[crudcrate(api_struct = "Example", generate_vox_service)]
    pub struct Model {
        #[sea_orm(primary_key, auto_increment = false)]
        #[crudcrate(primary_key, exclude(create, update), on_create = Uuid::new_v4())]
        pub id: Uuid,

        #[crudcrate(filterable, sortable, fulltext)]
        pub name: String,

        #[crudcrate(filterable, fulltext)]
        pub description: String,

        #[crudcrate(exclude(create, update), on_create = Utc::now())]
        pub created_at: DateTime<Utc>,

        #[crudcrate(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
        pub updated_at: DateTime<Utc>,
    }

    #[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
    pub enum Relation {}

    impl ActiveModelBehavior for ActiveModel {}
}

// ── ExampleService — hand-written domain surface ───────────────────────

/// Error envelope returned across the wire by [`ExampleService`]. Keep
/// this enum tight; new variants are a wire-format change.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum ExampleServiceError {
    NotFound,
    InvalidInput(String),
    Internal(String),
}

/// User-facing domain operations on [`Example`].
///
/// Clients only see this trait — the server may compose it from the
/// auto-generated `ExampleRepo` and any other lower-level repos. New
/// behavior (e.g. "rename and bump updated_at and audit-log it")
/// becomes a new method here, not a new field on `Example`.
#[vox::service]
pub trait ExampleService {
    /// List every example the caller can read. The frontend uses this
    /// for the home screen; pagination + filters will land here when
    /// we want them.
    async fn list_examples(&self) -> Result<Vec<Example>, ExampleServiceError>;

    /// Look up one example by ID.
    async fn get_example(&self, id: Uuid) -> Result<Example, ExampleServiceError>;

    /// Create a new example with the given name + description.
    /// Returns the persisted row.
    async fn create_example(
        &self,
        name: String,
        description: String,
    ) -> Result<Example, ExampleServiceError>;

    /// Rename an existing example. Demonstrates a domain operation
    /// that maps to an Update against the repo plus an audit hook
    /// (left as a TODO in the service impl).
    async fn rename_example(
        &self,
        id: Uuid,
        new_name: String,
    ) -> Result<Example, ExampleServiceError>;

    /// Remove an example. Returns Ok regardless of whether the row
    /// existed — this is "delete if present" semantics.
    async fn delete_example(&self, id: Uuid) -> Result<(), ExampleServiceError>;
}
