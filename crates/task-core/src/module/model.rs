//! Modules (epics/features) — logical groupings that span cycles.
//!
//! A module groups related tasks across time. Unlike cycles which are
//! time-boxed, modules are feature-scoped.
//!
//! Stored as `modules/<name>.md` in the project folder.

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
pub struct ModuleStringList(pub Vec<String>);

impl Default for ModuleStringList {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl Deref for ModuleStringList {
    type Target = Vec<String>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ModuleStringList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<String>> for ModuleStringList {
    fn from(value: Vec<String>) -> Self {
        Self(value)
    }
}

impl From<ModuleStringList> for Value {
    fn from(value: ModuleStringList) -> Self {
        Value::Json(Some(Box::new(
            serde_json::to_value(value.0).unwrap_or(serde_json::Value::Array(Vec::new())),
        )))
    }
}

impl Nullable for ModuleStringList {
    fn null() -> Value {
        Value::Json(None)
    }
}

impl TryGetable for ModuleStringList {
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

impl ValueType for ModuleStringList {
    fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
        match value {
            Value::Json(Some(value)) => serde_json::from_value(*value)
                .map(Self)
                .map_err(|_| ValueTypeErr),
            _ => Err(ValueTypeErr),
        }
    }

    fn type_name() -> String {
        stringify!(ModuleStringList).to_string()
    }

    fn array_type() -> ArrayType {
        ArrayType::Json
    }

    fn column_type() -> ColumnType {
        ColumnType::Json
    }
}

/// A feature module / epic.
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
#[sea_orm(table_name = "modules")]
#[crudcrate(
    api_struct = "ModuleApi",
    generate_vox_service,
    name_singular = "module",
    name_plural = "modules"
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
    pub target_date: Option<NaiveDate>,

    /// Module lead.
    #[crudcrate(filterable, sortable)]
    pub lead: Option<String>,

    /// Team members working on this module.
    #[facet(default)]
    pub members: ModuleStringList,

    /// Task titles or IDs in this module.
    #[facet(default)]
    pub tasks: ModuleStringList,

    /// Status.
    #[crudcrate(filterable, sortable)]
    pub status: ModuleStatus,

    pub sort_order: Option<f64>,
}

pub type Module = Model;

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}

#[derive(
    Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize, ToSchema, EnumIter, DeriveActiveEnum,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(32))")]
#[repr(u8)]
pub enum ModuleStatus {
    #[sea_orm(string_value = "backlog")]
    Backlog,
    #[sea_orm(string_value = "planned")]
    Planned,
    #[sea_orm(string_value = "in_progress")]
    InProgress,
    #[sea_orm(string_value = "paused")]
    Paused,
    #[sea_orm(string_value = "completed")]
    Completed,
    #[sea_orm(string_value = "cancelled")]
    Cancelled,
}

impl Default for ModuleStatus {
    fn default() -> Self {
        ModuleStatus::Backlog
    }
}
