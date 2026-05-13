pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "comments", repo)]
pub struct Comment {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub entity_id: Uuid,

    #[architect(filterable)]
    pub entity_type: String,

    #[architect(filterable, sortable, fulltext)]
    pub author: String,

    #[architect(fulltext)]
    pub body: String,

    pub time_start_ms: Option<i64>,

    pub time_end_ms: Option<i64>,

    #[architect(filterable)]
    pub reply_to: Option<Uuid>,

    #[architect(filterable)]
    pub resolved: bool,

    pub resolved_by: Option<String>,

    pub mentions: Vec<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now(), sortable)]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "reactions", repo)]
pub struct Reaction {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub entity_id: Uuid,

    #[architect(filterable)]
    pub entity_type: String,

    #[architect(filterable, sortable)]
    pub emoji: String,

    #[architect(filterable)]
    pub user: String,

    #[architect(exclude(create, update), on_create = Utc::now(), sortable)]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "attachments", repo)]
pub struct Attachment {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub owner_id: Uuid,

    #[architect(filterable)]
    pub owner_type: String,

    #[architect(filterable)]
    pub source: String,

    #[architect(fulltext, sortable)]
    pub path: String,

    pub label: Option<String>,

    #[architect(filterable)]
    pub mime: Option<String>,

    pub size_bytes: Option<i64>,

    pub checksum: Option<String>,

    #[architect(filterable)]
    pub uploader: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now(), sortable)]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum ThreadsServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait ThreadsService {
    async fn resolve_thread(&self, comment_id: Uuid) -> Result<(), ThreadsServiceError>;
}
