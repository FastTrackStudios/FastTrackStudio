pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "emails", repo)]
pub struct Email {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub message_id: String,

    #[architect(filterable, sortable, fulltext)]
    pub subject: String,

    #[architect(filterable, fulltext)]
    pub from_addr: String,

    pub to_addrs: Vec<String>,

    pub cc_addrs: Vec<String>,

    pub bcc_addrs: Vec<String>,

    #[architect(fulltext)]
    pub body: Option<String>,

    #[architect(filterable, sortable)]
    pub received_at: DateTime<Utc>,

    #[architect(filterable)]
    pub read: bool,

    #[architect(filterable)]
    pub starred: bool,

    #[architect(filterable)]
    pub folder: Option<String>,

    #[architect(filterable)]
    pub thread_id: Option<String>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum EmailServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait EmailService {
    async fn mark_read(&self, email_id: Uuid) -> Result<(), EmailServiceError>;
}
