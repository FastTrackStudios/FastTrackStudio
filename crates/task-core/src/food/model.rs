//! `Food` entity — canonical ingredient catalog.

use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use sea_orm::sea_query::{ArrayType, ColumnType, Nullable, Value, ValueType, ValueTypeErr};
use sea_orm::{ColIdx, QueryResult, TryGetError, TryGetable};
use serde::{Deserialize, Serialize};
use std::ops::{Deref, DerefMut};
use utoipa::ToSchema;

/// JSON-array-of-strings stored on `foods.aliases`. Mirror of the
/// `json_vec_type!`-generated `crate::task::StringList` shape, kept
/// distinct so the type name matches its column for clarity.
#[derive(Debug, Clone, Default, PartialEq, Facet, Serialize, Deserialize, ToSchema)]
#[serde(transparent)]
pub struct FoodAliasList(pub Vec<String>);

impl FoodAliasList {
    #[must_use]
    pub fn into_inner(self) -> Vec<String> {
        self.0
    }
}

impl Deref for FoodAliasList {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for FoodAliasList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl From<Vec<String>> for FoodAliasList {
    fn from(value: Vec<String>) -> Self {
        Self(value)
    }
}

impl FromIterator<String> for FoodAliasList {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl IntoIterator for FoodAliasList {
    type Item = String;
    type IntoIter = std::vec::IntoIter<String>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl From<FoodAliasList> for Value {
    fn from(value: FoodAliasList) -> Self {
        Value::Json(Some(Box::new(
            serde_json::to_value(value.0).unwrap_or(serde_json::Value::Array(Vec::new())),
        )))
    }
}

impl Nullable for FoodAliasList {
    fn null() -> Value {
        Value::Json(None)
    }
}

impl TryGetable for FoodAliasList {
    fn try_get_by<I: ColIdx>(res: &QueryResult, idx: I) -> Result<Self, TryGetError> {
        let value: serde_json::Value = res.try_get_by(idx)?;
        let items = serde_json::from_value(value).map_err(|err| {
            TryGetError::DbErr(sea_orm::DbErr::Type(format!(
                "failed to deserialize FoodAliasList: {err}"
            )))
        })?;
        Ok(Self(items))
    }
}

impl ValueType for FoodAliasList {
    fn try_from(value: Value) -> Result<Self, ValueTypeErr> {
        match value {
            Value::Json(Some(value)) => serde_json::from_value(*value)
                .map(Self)
                .map_err(|_| ValueTypeErr),
            _ => Err(ValueTypeErr),
        }
    }

    fn type_name() -> String {
        "FoodAliasList".to_string()
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
#[sea_orm(table_name = "foods")]
#[crudcrate(
    api_struct = "FoodApi",
    generate_vox_service,
    name_singular = "food",
    name_plural = "foods"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable, fulltext)]
    pub name: String,

    /// Lower-cased alternative names that resolve to this Food during
    /// ingredient matching ("evoo" -> "olive oil"). Stored as a JSON
    /// array; see [`FoodAliasList`].
    #[sea_orm(column_type = "Json")]
    pub aliases: FoodAliasList,

    /// Free-form category tag ("produce", "dairy", "pantry-staple", …).
    pub category: Option<String>,
    /// Informational hint ("g" | "ml" | "cup" | "piece") used by UIs
    /// when the user needs to enter a quantity.
    pub default_unit: Option<String>,

    /// Owning organization (matches auth seed slugs). `None` = global
    /// library shared across orgs.
    #[crudcrate(filterable)]
    pub organization: Option<String>,

    /// Default nutrition per 100g for this generic ingredient. JSON
    /// shape mirrors [`crate::nutrition::NutritionFacts`]. Each
    /// [`crate::food_product::FoodProduct`] can override with
    /// brand-specific data.
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub nutrition_per_100g: crate::property::JsonObject,

    /// Free-form notes (allergens, sourcing, etc.).
    pub notes: Option<String>,

    /// Polymorphic Obsidian-style properties (mirrors task/recipe).
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub properties: crate::property::JsonObject,

    pub created_by: Option<String>,

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
            aliases: sea_orm::ActiveValue::Set(FoodAliasList::default()),
            nutrition_per_100g: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

pub type Food = Model;
