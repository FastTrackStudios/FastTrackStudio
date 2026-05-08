//! `CookbookRecipe` join entity.

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
#[sea_orm(table_name = "cookbook_recipes")]
#[crudcrate(
    api_struct = "CookbookRecipeApi",
    generate_vox_service,
    name_singular = "cookbook_recipe",
    name_plural = "cookbook_recipes"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable)]
    pub cookbook_id: Uuid,
    #[crudcrate(filterable)]
    pub recipe_id: Uuid,

    /// Ordering within the cookbook (1-based).
    pub sequence: u32,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub added_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {}
