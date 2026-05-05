//! Project entity — database index for project metadata.

use crudcrate::EntityToModels;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, EntityToModels, Serialize, Deserialize)]
#[sea_orm(table_name = "projects")]
#[crudcrate(
    api_struct = "ProjectApi",
    generate_vox_service,
    name_singular = "project",
    name_plural = "projects"
)]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable, sortable, fulltext)]
    pub title: String,

    #[crudcrate(filterable)]
    pub slug: String,

    #[crudcrate(filterable)]
    pub status: String,

    #[crudcrate(filterable)]
    pub project_type: Option<String>,

    pub description: Option<String>,

    #[crudcrate(filterable)]
    pub area: Option<String>,

    /// Short identifier for task IDs (e.g. "PRJ").
    pub identifier: Option<String>,

    /// Next sequence number for tasks.
    pub next_sequence: Option<i32>,

    /// Parent project slug (for nesting).
    #[crudcrate(filterable)]
    pub parent_slug: Option<String>,

    pub lead: Option<String>,
    pub default_assignee: Option<String>,
    pub emoji: Option<String>,

    /// Organization slug this project belongs to.
    #[crudcrate(filterable)]
    pub organization: Option<String>,

    /// Team members (JSON array).
    pub team: Json,

    /// Referenced project slugs (JSON array).
    pub references: Json,

    pub due: Option<chrono::NaiveDate>,
    pub start: Option<chrono::NaiveDate>,

    /// Path to the project.md file.
    pub file_path: Option<String>,

    pub deleted_at: Option<chrono::DateTime<chrono::Utc>>,
    pub archived_at: Option<chrono::DateTime<chrono::Utc>>,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,

    #[crudcrate(exclude(create, update), on_create = chrono::Utc::now(), on_update = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}
