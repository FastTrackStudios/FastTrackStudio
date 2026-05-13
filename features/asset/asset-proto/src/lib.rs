//! `asset-proto` — wire contract for physical/digital assets.
//!
//! Cross-cutting feature: gear, hardware, sample packs, license seats.
//! Other features (project, location, person) reference assets by uuid.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "assets", repo)]
pub struct Asset {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Human-readable label, e.g. `"SSL Bus Compressor"`.
    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    /// Lifecycle state — free-form for now. Common values: `"active"`,
    /// `"in-repair"`, `"retired"`, `"sold"`, `"lost"`.
    #[architect(filterable, sortable)]
    pub status: String,

    #[architect(filterable)]
    pub manufacturer: Option<String>,

    #[architect(filterable)]
    pub model: Option<String>,

    #[architect(filterable)]
    pub serial_number: Option<String>,

    /// FK to `person` feature once it's populated.
    #[architect(filterable)]
    pub owner_id: Option<Uuid>,

    /// FK to `location` feature.
    #[architect(filterable)]
    pub location_id: Option<Uuid>,

    pub notes: Option<String>,

    pub tags: Vec<String>,

    #[architect(filterable, sortable)]
    pub acquired_at: Option<DateTime<Utc>>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum AssetServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait AssetService {
    async fn transfer(&self, asset_id: Uuid, new_owner_id: Uuid) -> Result<(), AssetServiceError>;
}
