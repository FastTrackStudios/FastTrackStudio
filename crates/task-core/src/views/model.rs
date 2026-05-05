//! Saved views — persistent filter + sort + display configurations.
//!
//! A view is a named set of filters, grouping, sorting, and column visibility
//! that can be saved, shared, and reused. Like Plane's IssueView.
//!
//! Stored as `.md` files in `views/` directory:
//! ```yaml
//! title: "My Urgent Tasks"
//! filters:
//!   priority: [Urgent, High]
//!   status: [Open, InProgress]
//!   assignee: [cody]
//! display:
//!   group_by: status
//!   order_by: priority
//!   layout: list
//! ```

use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use sea_orm::sea_query::{ArrayType, ColumnType, Nullable, Value, ValueType, ValueTypeErr};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;
use uuid::Uuid;

macro_rules! json_object_type {
    ($name:ident) => {
        impl From<$name> for Value {
            fn from(value: $name) -> Self {
                Value::Json(Some(Box::new(
                    serde_json::to_value(value)
                        .unwrap_or(serde_json::Value::Object(Default::default())),
                )))
            }
        }

        impl Nullable for $name {
            fn null() -> Value {
                Value::Json(None)
            }
        }

        impl TryGetable for $name {
            fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
                let value: serde_json::Value = res.try_get_by(idx)?;
                serde_json::from_value(value).map_err(|err| {
                    TryGetError::DbErr(sea_orm::DbErr::Type(format!(
                        "failed to deserialize JSON object: {err}"
                    )))
                })
            }
        }

        impl ValueType for $name {
            fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
                match value {
                    Value::Json(Some(value)) => {
                        serde_json::from_value(*value).map_err(|_| ValueTypeErr)
                    }
                    _ => Err(ValueTypeErr),
                }
            }

            fn type_name() -> String {
                stringify!($name).to_string()
            }

            fn array_type() -> ArrayType {
                ArrayType::Json
            }

            fn column_type() -> ColumnType {
                ColumnType::Json
            }
        }
    };
}

/// A saved view configuration.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Default,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
    ToSchema,
)]
#[sea_orm(table_name = "saved_views")]
#[crudcrate(
    api_struct = "SavedViewApi",
    generate_vox_service,
    name_singular = "saved view",
    name_plural = "saved views"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(
        primary_key,
        exclude(create),
        on_create = uuid::Uuid::new_v4()
    )]
    pub id: Uuid,

    #[crudcrate(filterable, sortable, fulltext)]
    pub title: String,
    #[crudcrate(fulltext)]
    pub description: Option<String>,

    /// Optional project scope (null = workspace-level view).
    #[crudcrate(filterable, sortable)]
    pub project: Option<String>,

    /// Filter criteria.
    #[facet(default)]
    pub filters: ViewFilters,

    /// Display settings.
    #[facet(default)]
    pub display: ViewDisplay,

    /// Who created this view.
    #[crudcrate(filterable, sortable)]
    pub created_by: Option<String>,

    /// Whether this view is shared with the team or personal.
    #[crudcrate(filterable)]
    pub is_shared: bool,

    /// Sort order for the views list.
    #[crudcrate(sortable)]
    pub sort_order: Option<f64>,

    #[facet(default)]
    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,

    #[facet(default)]
    #[crudcrate(
        exclude(create, update),
        on_create = chrono::Utc::now(),
        on_update = chrono::Utc::now()
    )]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

pub type SavedView = Model;

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

/// Filter criteria for a view.
#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct ViewFilters {
    #[facet(default)]
    pub status: Vec<String>,
    #[facet(default)]
    pub priority: Vec<String>,
    #[facet(default)]
    pub assignee: Vec<String>,
    #[facet(default)]
    pub tags: Vec<String>,
    #[facet(default)]
    pub project: Vec<String>,
    #[facet(default)]
    pub issue_type: Vec<String>,
    pub due_before: Option<String>,
    pub due_after: Option<String>,
    pub created_before: Option<String>,
    pub created_after: Option<String>,
    /// Whether to include sub-tasks.
    pub include_subtasks: Option<bool>,
    /// Full-text search query.
    pub search: Option<String>,
}

json_object_type!(ViewFilters);

/// Display/presentation settings for a view.
#[derive(Debug, Clone, PartialEq, Default, Facet, Serialize, Deserialize, ToSchema)]
pub struct ViewDisplay {
    /// Layout: list, kanban, calendar, gantt, table.
    pub layout: Option<String>,
    /// Group by field: status, priority, assignee, project, none.
    pub group_by: Option<String>,
    /// Sort by field: priority, due, created, title, updated.
    pub order_by: Option<String>,
    /// Sort direction: asc, desc.
    pub order_direction: Option<String>,
    /// Show empty groups in kanban/grouped views.
    pub show_empty_groups: Option<bool>,
    /// Visible properties/columns.
    #[facet(default)]
    pub visible_properties: Vec<String>,
}

json_object_type!(ViewDisplay);
