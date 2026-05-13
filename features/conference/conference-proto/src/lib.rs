pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "meetings", repo)]
pub struct Meeting {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(filterable)]
    pub host_user: Option<String>,

    #[architect(filterable, sortable)]
    pub scheduled_at: DateTime<Utc>,

    #[architect(filterable, sortable)]
    pub started_at: Option<DateTime<Utc>>,

    #[architect(filterable, sortable)]
    pub ended_at: Option<DateTime<Utc>>,

    #[architect(filterable, sortable)]
    pub status: String,

    pub recording_url: Option<String>,

    #[architect(fulltext)]
    pub notes: Option<String>,

    pub participants: Vec<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum ConferenceServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait ConferenceService {
    async fn end_meeting(&self, meeting_id: Uuid) -> Result<(), ConferenceServiceError>;
}
