//! `location-proto` — wire contract for physical places.
//!
//! Studios, venues, offices, client sites. Cross-cutting: assets and
//! people live somewhere; events happen somewhere.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "locations", repo)]
pub struct Location {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    /// Free-form category: `"studio"`, `"venue"`, `"office"`,
    /// `"client-site"`, etc.
    #[architect(filterable)]
    pub kind: Option<String>,

    pub address1: Option<String>,
    pub address2: Option<String>,
    pub city: Option<String>,
    pub state: Option<String>,
    pub postal_code: Option<String>,

    /// ISO 3166-1 alpha-2.
    #[architect(filterable)]
    pub country_code: Option<String>,

    pub contact_name: Option<String>,
    pub contact_email: Option<String>,

    /// Self-FK for sub-locations (Studio A → Live Room).
    #[architect(filterable)]
    pub parent_id: Option<Uuid>,

    pub notes: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum LocationServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait LocationService {
    /// Re-parent a location under a different parent.
    async fn reparent(
        &self,
        location_id: Uuid,
        new_parent_id: Option<Uuid>,
    ) -> Result<(), LocationServiceError>;
}
