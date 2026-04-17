//! Task entity — SeaORM model with crudcrate CRUD + Vox RPC generation.
//!
//! This is the database representation of a task. The markdown `.md` file
//! remains the source of truth for body content (notes, subtasks, descriptions).
//! This table indexes the structured metadata for fast queries.

use sea_orm::entity::prelude::*;
use crudcrate::EntityToModels;
use crudcrate::CRUDResource;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, DeriveEntityModel, EntityToModels, Serialize, Deserialize)]
#[sea_orm(table_name = "tasks")]
#[crudcrate(
    api_struct = "TaskApi",
    
    generate_vox_service,
    name_singular = "task",
    name_plural = "tasks",
)]
pub struct Model {
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// Human-readable sequence number within the project (e.g. 42 → "PRJ-42").
    #[crudcrate(filterable, sortable)]
    pub sequence_id: Option<i32>,

    #[crudcrate(filterable, sortable, fulltext)]
    pub title: String,

    /// Status: Open, InProgress, Done, OnHold, Planned, Cancelled, Archived.
    #[crudcrate(filterable, sortable)]
    pub status: String,

    /// Priority: None, Low, Normal, High, Urgent.
    #[crudcrate(filterable, sortable)]
    pub priority: String,

    /// Issue type: task, bug, story, epic, feature, chore.
    #[crudcrate(filterable)]
    pub issue_type: Option<String>,

    /// Project this task belongs to (by slug or title).
    #[crudcrate(filterable)]
    pub project: Option<String>,

    /// Primary assignee.
    #[crudcrate(filterable)]
    pub assignee: Option<String>,

    /// Additional assignees (JSON array).
    
    pub assignees: Json,

    /// Who created this task.
    #[crudcrate(filterable)]
    pub created_by: Option<String>,

    pub due: Option<chrono::NaiveDate>,
    pub scheduled: Option<chrono::NaiveDate>,
    pub start: Option<chrono::NaiveDate>,
    pub completed_date: Option<chrono::NaiveDate>,

    /// Tags (JSON array).
    
    pub tags: Json,

    /// Time estimate in minutes.
    pub time_estimate: Option<i32>,

    /// Float for custom ordering.
    pub sort_order: Option<f64>,

    /// Task body content (markdown).
    pub body: Option<String>,

    /// Path to the .md file (relative to vault root).
    pub file_path: Option<String>,

    /// Is this a draft (not visible to team)?
    #[crudcrate(filterable)]
    pub is_draft: bool,

    /// Soft deletion.
    pub deleted_at: Option<chrono::DateTime<chrono::Utc>>,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,

    #[crudcrate(exclude(create, update), on_create = chrono::Utc::now(), on_update = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}
