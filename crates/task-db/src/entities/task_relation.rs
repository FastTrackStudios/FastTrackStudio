//! Task relation entity — typed relationships between tasks.

use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};
use crudcrate::EntityToModels;
use crudcrate::CRUDResource;

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, EntityToModels, Serialize, Deserialize)]
#[sea_orm(table_name = "task_relations")]
#[crudcrate(
    api_struct = "TaskRelationApi",
    generate_vox_service,
    name_singular = "task_relation",
    name_plural = "task_relations",
)]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Source task.
    #[crudcrate(filterable)]
    pub issue_id: Uuid,

    /// Target task.
    #[crudcrate(filterable)]
    pub related_issue_id: Uuid,

    /// Relation type: blocked_by, blocking, relates_to, duplicate_of,
    /// implemented_by, implements, start_before, start_after, finish_before, finish_after.
    #[crudcrate(filterable)]
    pub relation_type: String,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {
    #[sea_orm(
        belongs_to = "super::task::Entity",
        from = "Column::IssueId",
        to = "super::task::Column::Id"
    )]
    SourceTask,
    #[sea_orm(
        belongs_to = "super::task::Entity",
        from = "Column::RelatedIssueId",
        to = "super::task::Column::Id"
    )]
    RelatedTask,
}

impl ActiveModelBehavior for ActiveModel {}
