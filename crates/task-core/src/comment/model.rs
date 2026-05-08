//! Comment entity — threaded comments with timecode ranges.

use crudcrate::EntityToModels;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

use crate::task::JsonProperties;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, EntityToModels, Serialize, Deserialize)]
#[sea_orm(table_name = "comments")]
#[crudcrate(
    api_struct = "CommentApi",
    generate_vox_service,
    name_singular = "comment",
    name_plural = "comments"
)]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Entity this comment is attached to (task ID, output ID, etc.).
    #[crudcrate(filterable)]
    pub entity_id: Uuid,

    /// Entity type: "task", "output", "session", "project".
    #[crudcrate(filterable)]
    pub entity_type: String,

    pub author: String,
    pub body: String,

    /// Time reference start (seconds) for audio/video comments.
    pub time_start: Option<f64>,
    /// Time reference end (seconds) for ranged comments.
    pub time_end: Option<f64>,

    /// Parent comment ID for threading.
    #[crudcrate(filterable)]
    pub reply_to: Option<Uuid>,

    pub resolved: bool,
    pub resolved_by: Option<String>,

    /// Mentions extracted from body (JSON array).
    pub mentions: Json,

    /// Nextcloud Deck comment ID for sync.
    pub external_id: Option<String>,

    /// Workflow-defined frontmatter keys. Free-form `key → value` map
    /// stored as JSON. Excluded from list responses to keep them lean;
    /// fetched via single-entity get or PropertyService.
    #[crudcrate(exclude(list))]
    #[serde(default)]
    #[sea_orm(column_type = "Json", default_value = "{}")]
    pub properties: JsonProperties,

    pub deleted_at: Option<chrono::DateTime<chrono::Utc>>,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,

    #[crudcrate(exclude(create, update), on_create = chrono::Utc::now(), on_update = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "crate::task::Entity",
        from = "Column::EntityId",
        to = "crate::task::Column::Id"
    )]
    Task,
}

impl ActiveModelBehavior for ActiveModel {}

pub type Comment = Model;
