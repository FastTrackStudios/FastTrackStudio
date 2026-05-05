//! Cycles (sprints) — time-boxed containers for tasks.
//!
//! A cycle has a start and end date and contains a subset of project tasks.
//! Tasks can belong to at most one active cycle.
//!
//! Stored as `cycles/<name>.md` in the project folder.

use chrono::NaiveDate;
use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use sea_orm::sea_query::{ArrayType, ColumnType, Nullable, Value, ValueType, ValueTypeErr};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use utoipa::ToSchema;
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
#[facet(transparent)]
#[serde(transparent)]
pub struct CycleTaskList(pub Vec<String>);

impl Default for CycleTaskList {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl Deref for CycleTaskList {
    type Target = Vec<String>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for CycleTaskList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<String>> for CycleTaskList {
    fn from(value: Vec<String>) -> Self {
        Self(value)
    }
}

impl From<CycleTaskList> for Value {
    fn from(value: CycleTaskList) -> Self {
        Value::Json(Some(Box::new(
            serde_json::to_value(value.0).unwrap_or(serde_json::Value::Array(Vec::new())),
        )))
    }
}

impl Nullable for CycleTaskList {
    fn null() -> Value {
        Value::Json(None)
    }
}

impl TryGetable for CycleTaskList {
    fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
        let value: serde_json::Value = res.try_get_by(idx)?;
        let items = serde_json::from_value(value).map_err(|err| {
            TryGetError::DbErr(sea_orm::DbErr::Type(format!(
                "failed to deserialize JSON array: {err}"
            )))
        })?;
        Ok(Self(items))
    }
}

impl ValueType for CycleTaskList {
    fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
        match value {
            Value::Json(Some(value)) => serde_json::from_value(*value)
                .map(Self)
                .map_err(|_| ValueTypeErr),
            _ => Err(ValueTypeErr),
        }
    }

    fn type_name() -> String {
        stringify!(CycleTaskList).to_string()
    }

    fn array_type() -> ArrayType {
        ArrayType::Json
    }

    fn column_type() -> ColumnType {
        ColumnType::Json
    }
}

/// A time-boxed work cycle (sprint).
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
#[sea_orm(table_name = "cycles")]
#[crudcrate(
    api_struct = "CycleApi",
    generate_vox_service,
    name_singular = "cycle",
    name_plural = "cycles"
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
    #[crudcrate(filterable, sortable)]
    pub start_date: Option<NaiveDate>,
    #[crudcrate(filterable, sortable)]
    pub end_date: Option<NaiveDate>,

    /// Who owns/manages this cycle.
    #[crudcrate(filterable, sortable)]
    pub owned_by: Option<String>,

    /// Task titles or IDs included in this cycle.
    #[facet(default)]
    pub tasks: CycleTaskList,

    /// Status: planned, active, completed, cancelled.
    #[crudcrate(filterable, sortable)]
    pub status: CycleStatus,

    /// Progress snapshot for reporting.
    pub total_tasks: Option<u32>,
    pub completed_tasks: Option<u32>,

    pub sort_order: Option<f64>,
}

pub type Cycle = Model;

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

#[derive(
    Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize, ToSchema, EnumIter, DeriveActiveEnum,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(32))")]
#[repr(u8)]
pub enum CycleStatus {
    #[sea_orm(string_value = "planned")]
    Planned,
    #[sea_orm(string_value = "active")]
    Active,
    #[sea_orm(string_value = "completed")]
    Completed,
    #[sea_orm(string_value = "cancelled")]
    Cancelled,
}

impl Default for CycleStatus {
    fn default() -> Self {
        CycleStatus::Planned
    }
}

impl Cycle {
    pub fn is_active(&self) -> bool {
        self.status == CycleStatus::Active
    }

    pub fn progress(&self) -> f64 {
        match (self.completed_tasks, self.total_tasks) {
            (Some(done), Some(total)) if total > 0 => done as f64 / total as f64,
            _ => 0.0,
        }
    }

    pub fn days_remaining(&self) -> Option<i64> {
        let today = chrono::Local::now().date_naive();
        self.end_date.map(|end| (end - today).num_days())
    }
}
