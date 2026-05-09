//! `Routine` entity — workout template (PPL day, 5k run, etc.).

use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use sea_orm::sea_query::{ArrayType, ColumnType, Nullable, Value, ValueType, ValueTypeErr};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use utoipa::ToSchema;

/// JSON-array-of-strings stored on `routines.tags`.
#[derive(Debug, Clone, Default, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
#[serde(transparent)]
pub struct RoutineTagList(pub Vec<String>);

impl RoutineTagList {
    #[must_use]
    pub fn into_inner(self) -> Vec<String> {
        self.0
    }
}

impl Deref for RoutineTagList {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for RoutineTagList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<String>> for RoutineTagList {
    fn from(value: Vec<String>) -> Self {
        Self(value)
    }
}

impl FromIterator<String> for RoutineTagList {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl IntoIterator for RoutineTagList {
    type Item = String;
    type IntoIter = std::vec::IntoIter<String>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<RoutineTagList> for Value {
    fn from(value: RoutineTagList) -> Self {
        Value::Json(Some(Box::new(
            serde_json::to_value(value.0).unwrap_or(serde_json::Value::Array(Vec::new())),
        )))
    }
}

impl Nullable for RoutineTagList {
    fn null() -> Value {
        Value::Json(None)
    }
}

impl TryGetable for RoutineTagList {
    fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
        let value: serde_json::Value = res.try_get_by(idx)?;
        let items = serde_json::from_value(value).map_err(|err| {
            TryGetError::DbErr(sea_orm::DbErr::Type(format!(
                "failed to deserialize RoutineTagList: {err}"
            )))
        })?;
        Ok(Self(items))
    }
}

impl ValueType for RoutineTagList {
    fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
        match value {
            Value::Json(Some(value)) => serde_json::from_value(*value)
                .map(Self)
                .map_err(|_| ValueTypeErr),
            _ => Err(ValueTypeErr),
        }
    }

    fn type_name() -> String {
        "RoutineTagList".to_string()
    }

    fn array_type() -> ArrayType {
        ArrayType::Json
    }

    fn column_type() -> ColumnType {
        ColumnType::Json
    }
}

#[derive(
    Clone,
    Debug,
    Default,
    PartialEq,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
)]
#[sea_orm(table_name = "routines")]
#[crudcrate(
    api_struct = "RoutineApi",
    generate_vox_service,
    name_singular = "routine",
    name_plural = "routines"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable, fulltext)]
    pub name: String,

    #[crudcrate(filterable)]
    pub slug: String,

    pub description: Option<String>,

    /// Markdown body for warm-up notes, programming context, etc.
    pub body_markdown: String,

    /// Tag for the broad style of the routine: "ppl", "full-body",
    /// "running", "kettlebell", "mobility", "circuit". Free-form.
    #[crudcrate(filterable)]
    pub category: Option<String>,

    /// Estimated total duration in minutes. Optional — used for UI hints.
    pub estimated_duration_minutes: Option<u32>,

    /// Difficulty hint, free-form: "beginner", "intermediate", "advanced".
    pub difficulty: Option<String>,

    /// Tags (Vec<String>) — squat-day, hypertrophy, deload, etc.
    #[sea_orm(column_type = "Json")]
    pub tags: RoutineTagList,

    #[crudcrate(filterable)]
    pub organization: Option<String>,

    pub created_by: Option<String>,

    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub properties: crate::property::JsonObject,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {
    fn new() -> Self {
        Self {
            tags: sea_orm::ActiveValue::Set(RoutineTagList::default()),
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

pub type Routine = Model;
