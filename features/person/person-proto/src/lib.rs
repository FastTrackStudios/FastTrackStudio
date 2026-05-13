//! `person-proto` — wire contract for the `person` feature.
//!
//! Three top-level entities:
//!
//! - `Person` — an individual (engineer, producer, client, collaborator)
//! - `Client` — a company / individual / label that contracts work
//! - `Team`   — a grouping of people
//!
//! Each is a separate `architect::Entity` with its own Repo trait.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Person ────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "people", repo)]
pub struct Person {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(filterable, fulltext)]
    pub email: Option<String>,

    #[architect(filterable)]
    pub phone: Option<String>,

    #[architect(filterable)]
    pub role: Option<String>,

    #[architect(filterable)]
    pub client_id: Option<Uuid>,

    #[architect(filterable)]
    pub team_id: Option<Uuid>,

    #[architect(fulltext)]
    pub notes: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Client ────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "clients", repo)]
pub struct Client {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(filterable)]
    pub kind: String,

    pub website: Option<String>,

    #[architect(filterable)]
    pub contact_email: Option<String>,

    pub address: Option<String>,

    #[architect(filterable)]
    pub country_code: Option<String>,

    pub default_billable_rate_cents: Option<u32>,

    pub notes: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Team ──────────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "teams", repo)]
pub struct Team {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(fulltext)]
    pub description: Option<String>,

    #[architect(filterable)]
    pub owner: Option<String>,

    #[architect(filterable)]
    pub archived: bool,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── PersonService ─────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum PersonServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait PersonService {
    /// Attach a person to a client (set `client_id`).
    async fn attach_to_client(
        &self,
        person_id: Uuid,
        client_id: Uuid,
    ) -> Result<(), PersonServiceError>;
}
