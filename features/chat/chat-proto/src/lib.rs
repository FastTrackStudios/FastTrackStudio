//! `chat-proto` — wire contract for the chat feature.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

// ── Channel ───────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "channels", repo)]
pub struct Channel {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(filterable)]
    pub kind: String,

    #[architect(fulltext)]
    pub topic: Option<String>,

    #[architect(filterable)]
    pub project_id: Option<Uuid>,

    #[architect(filterable)]
    pub archived: bool,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── Message ───────────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "messages", repo)]
pub struct Message {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable)]
    pub channel_id: Uuid,

    #[architect(filterable, fulltext)]
    pub author: String,

    #[architect(fulltext)]
    pub body: String,

    #[architect(filterable)]
    pub reply_to: Option<Uuid>,

    #[architect(filterable)]
    pub edited_at: Option<DateTime<Utc>>,

    #[architect(filterable)]
    pub deleted: bool,

    pub mentions: Vec<String>,

    pub attachment_ids: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── ChannelMember ─────────────────────────────────────────────────────

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "channel_members", repo)]
pub struct ChannelMember {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable)]
    pub channel_id: Uuid,

    #[architect(filterable)]
    pub user: String,

    #[architect(filterable)]
    pub role: String,

    #[architect(filterable, sortable)]
    pub joined_at: DateTime<Utc>,

    pub last_read_message_id: Option<Uuid>,

    #[architect(filterable)]
    pub muted: bool,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

// ── ChatService ───────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum ChatServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait ChatService {
    async fn mark_read(&self, member_id: Uuid, message_id: Uuid) -> Result<(), ChatServiceError>;
}
