pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "agent_runs", repo)]
pub struct AgentRun {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    #[architect(filterable)]
    pub kind: String,

    #[architect(fulltext)]
    pub prompt: String,

    #[architect(filterable, sortable)]
    pub status: String,

    #[architect(filterable)]
    pub task_id: Option<Uuid>,

    #[architect(filterable, sortable)]
    pub started_at: Option<DateTime<Utc>>,

    #[architect(filterable, sortable)]
    pub completed_at: Option<DateTime<Utc>>,

    pub result: Option<String>,

    pub error_message: Option<String>,

    pub tokens_used: Option<u32>,

    pub cost_cents: Option<u32>,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum AgentServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait AgentService {
    async fn cancel(&self, run_id: Uuid) -> Result<(), AgentServiceError>;
}
