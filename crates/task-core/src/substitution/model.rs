//! `Substitution` entity — one swappable-food rule (from → to).

use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

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
#[sea_orm(table_name = "substitutions")]
#[crudcrate(
    api_struct = "SubstitutionApi",
    generate_vox_service,
    name_singular = "substitution",
    name_plural = "substitutions"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable)]
    pub from_food_id: Uuid,
    #[crudcrate(filterable)]
    pub to_food_id: Uuid,

    /// Multiplier on the original quantity. 1.0 means swap 1:1.
    /// 0.75 for "use 3/4 cup honey instead of 1 cup sugar".
    pub ratio: f64,

    /// Free-form note explaining the swap mechanics
    /// ("plus 1 tbsp lemon juice", "reduce other liquids by 25%").
    pub conversion_note: Option<String>,

    /// JSON object describing when this swap applies. Recognized keys:
    /// `dietary` (Vec<String>), `context` (Vec<String>), `season`
    /// (Vec<String>). Empty / missing = always applies.
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub applies_when: crate::property::JsonObject,

    /// Quality of the swap, 0.0..=1.0. Higher = better match.
    pub confidence: f32,

    /// When true, the inverse swap (`to → from` at `1/ratio`) is also
    /// implicitly valid. The query helper synthesizes inverse hits on
    /// the fly.
    pub bidirectional: bool,

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
            applies_when: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

pub type Substitution = Model;
