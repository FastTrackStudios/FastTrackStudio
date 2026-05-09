//! `Exercise` entity — canonical fitness movement catalog.

use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use sea_orm::sea_query::{ArrayType, ColumnType, Nullable, Value, ValueType, ValueTypeErr};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use utoipa::ToSchema;

/// JSON-array-of-strings stored on `exercises.aliases`.
#[derive(Debug, Clone, Default, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
#[serde(transparent)]
pub struct ExerciseAliasList(pub Vec<String>);

impl ExerciseAliasList {
    #[must_use]
    pub fn into_inner(self) -> Vec<String> {
        self.0
    }
}

impl Deref for ExerciseAliasList {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ExerciseAliasList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<String>> for ExerciseAliasList {
    fn from(value: Vec<String>) -> Self {
        Self(value)
    }
}

impl FromIterator<String> for ExerciseAliasList {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl IntoIterator for ExerciseAliasList {
    type Item = String;
    type IntoIter = std::vec::IntoIter<String>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<ExerciseAliasList> for Value {
    fn from(value: ExerciseAliasList) -> Self {
        Value::Json(Some(Box::new(
            serde_json::to_value(value.0).unwrap_or(serde_json::Value::Array(Vec::new())),
        )))
    }
}

impl Nullable for ExerciseAliasList {
    fn null() -> Value {
        Value::Json(None)
    }
}

impl TryGetable for ExerciseAliasList {
    fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
        let value: serde_json::Value = res.try_get_by(idx)?;
        let items = serde_json::from_value(value).map_err(|err| {
            TryGetError::DbErr(sea_orm::DbErr::Type(format!(
                "failed to deserialize ExerciseAliasList: {err}"
            )))
        })?;
        Ok(Self(items))
    }
}

impl ValueType for ExerciseAliasList {
    fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
        match value {
            Value::Json(Some(value)) => serde_json::from_value(*value)
                .map(Self)
                .map_err(|_| ValueTypeErr),
            _ => Err(ValueTypeErr),
        }
    }

    fn type_name() -> String {
        "ExerciseAliasList".to_string()
    }

    fn array_type() -> ArrayType {
        ArrayType::Json
    }

    fn column_type() -> ColumnType {
        ColumnType::Json
    }
}

/// Secondary muscle list (Vec<String>) for `exercises.secondary_muscles`.
#[derive(Debug, Clone, Default, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
#[serde(transparent)]
pub struct ExerciseMuscleList(pub Vec<String>);

impl ExerciseMuscleList {
    #[must_use]
    pub fn into_inner(self) -> Vec<String> {
        self.0
    }
}

impl Deref for ExerciseMuscleList {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ExerciseMuscleList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<String>> for ExerciseMuscleList {
    fn from(value: Vec<String>) -> Self {
        Self(value)
    }
}

impl FromIterator<String> for ExerciseMuscleList {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl IntoIterator for ExerciseMuscleList {
    type Item = String;
    type IntoIter = std::vec::IntoIter<String>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<ExerciseMuscleList> for Value {
    fn from(value: ExerciseMuscleList) -> Self {
        Value::Json(Some(Box::new(
            serde_json::to_value(value.0).unwrap_or(serde_json::Value::Array(Vec::new())),
        )))
    }
}

impl Nullable for ExerciseMuscleList {
    fn null() -> Value {
        Value::Json(None)
    }
}

impl TryGetable for ExerciseMuscleList {
    fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
        let value: serde_json::Value = res.try_get_by(idx)?;
        let items = serde_json::from_value(value).map_err(|err| {
            TryGetError::DbErr(sea_orm::DbErr::Type(format!(
                "failed to deserialize ExerciseMuscleList: {err}"
            )))
        })?;
        Ok(Self(items))
    }
}

impl ValueType for ExerciseMuscleList {
    fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
        match value {
            Value::Json(Some(value)) => serde_json::from_value(*value)
                .map(Self)
                .map_err(|_| ValueTypeErr),
            _ => Err(ValueTypeErr),
        }
    }

    fn type_name() -> String {
        "ExerciseMuscleList".to_string()
    }

    fn array_type() -> ArrayType {
        ArrayType::Json
    }

    fn column_type() -> ColumnType {
        ColumnType::Json
    }
}

/// Movement modality — drives the SetLog field set downstream.
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    DeriveActiveEnum,
    EnumIter,
    utoipa::ToSchema,
    facet::Facet,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(16))")]
#[repr(u8)]
pub enum ExerciseModality {
    #[default]
    #[sea_orm(string_value = "strength")]
    Strength,
    #[sea_orm(string_value = "cardio")]
    Cardio,
    #[sea_orm(string_value = "mobility")]
    Mobility,
}

impl ExerciseModality {
    #[must_use]
    pub fn as_str(&self) -> &'static str {
        match self {
            ExerciseModality::Strength => "strength",
            ExerciseModality::Cardio => "cardio",
            ExerciseModality::Mobility => "mobility",
        }
    }

    #[must_use]
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_ascii_lowercase().as_str() {
            "strength" | "lift" | "weights" => Some(ExerciseModality::Strength),
            "cardio" | "endurance" | "aerobic" => Some(ExerciseModality::Cardio),
            "mobility" | "stretch" | "stretching" | "flexibility" => {
                Some(ExerciseModality::Mobility)
            }
            _ => None,
        }
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
#[sea_orm(table_name = "exercises")]
#[crudcrate(
    api_struct = "ExerciseApi",
    generate_vox_service,
    name_singular = "exercise",
    name_plural = "exercises"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable, fulltext)]
    pub name: String,

    /// Lower-cased URL-friendly slug, unique within (organization, slug).
    /// Auto-derived from name when not provided.
    #[crudcrate(filterable)]
    pub slug: String,

    /// Lower-cased alternative names ("ohp" -> "overhead-press").
    #[sea_orm(column_type = "Json")]
    pub aliases: ExerciseAliasList,

    /// Modality enum stored as text. Drives the SetLog field set
    /// downstream (strength = reps+weight; cardio = duration+distance;
    /// mobility = duration only).
    #[crudcrate(filterable)]
    pub modality: ExerciseModality,

    /// Primary muscle group / discipline tag. Free-form, indexed for
    /// filtering. e.g. "chest", "quads", "back", "running", "cycling",
    /// "full-body".
    #[crudcrate(filterable)]
    pub primary_muscle: Option<String>,

    /// Secondary muscle groups (Vec<String>, json column).
    #[sea_orm(column_type = "Json")]
    pub secondary_muscles: ExerciseMuscleList,

    /// Equipment required ("barbell", "dumbbell", "kettlebell",
    /// "bodyweight", "machine", "cable", "treadmill", "none").
    #[crudcrate(filterable)]
    pub equipment: Option<String>,

    /// Markdown body — long-form description, cues, common mistakes.
    /// Doubles as the place to drop [[glossary]] wikilinks.
    pub body_markdown: String,

    /// Optional video / image URL (a tutorial link).
    pub media_url: Option<String>,

    /// Owning organization. None = global. Same convention as Food/Glossary.
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
            aliases: sea_orm::ActiveValue::Set(ExerciseAliasList::default()),
            secondary_muscles: sea_orm::ActiveValue::Set(ExerciseMuscleList::default()),
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

pub type Exercise = Model;
