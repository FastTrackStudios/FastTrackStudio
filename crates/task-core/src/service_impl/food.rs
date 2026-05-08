//! SeaORM-backed [`FoodService`] implementation.
//!
//! Catalog ops (foods, products) plus the `link_recipe_ingredient`
//! escape hatch for manually wiring an ingredient row to a Food after
//! the auto name-match on recipe insert missed it.

use chrono::Utc;
use sea_orm::{
    ActiveModelTrait, ColumnTrait, DatabaseConnection, EntityTrait, QueryFilter, QueryOrder, Set,
};
use uuid::Uuid;

use crate::food::{self, FoodAliasList, FoodApi};
use crate::food_product::{self, FoodProductApi};
use crate::property::JsonObject;
use crate::recipe_ingredient;
use crate::service::{
    CreateFoodProductRequest, CreateFoodRequest, FoodPatch, FoodProductPatch, FoodService,
    VaultError,
};

use super::helpers::convert_model;

/// Typed dependencies for [`FoodServiceImpl`].
pub struct FoodServiceDeps {
    pub db: DatabaseConnection,
}

#[derive(Clone)]
pub struct FoodServiceImpl {
    db: DatabaseConnection,
}

impl FoodServiceImpl {
    pub fn new(deps: FoodServiceDeps) -> Self {
        Self { db: deps.db }
    }
}

fn io(err: impl std::fmt::Display, ctx: &str) -> VaultError {
    VaultError::IoError(format!("{ctx}: {err}"))
}

fn parse_json_object(payload: Option<&str>, ctx: &str) -> Result<JsonObject, VaultError> {
    match payload {
        Some(s) if !s.trim().is_empty() => {
            let value: serde_json::Value = serde_json::from_str(s)
                .map_err(|e| VaultError::ParseError(format!("{ctx}: {e}")))?;
            Ok(JsonObject::from_value(value))
        }
        _ => Ok(JsonObject::default()),
    }
}

fn food_to_api(model: food::Model) -> Result<FoodApi, VaultError> {
    convert_model::<food::Model, FoodApi>(model)
}

fn product_to_api(model: food_product::Model) -> Result<FoodProductApi, VaultError> {
    convert_model::<food_product::Model, FoodProductApi>(model)
}

/// Lower-case + dedupe an alias list while preserving first-seen
/// order. Used both on create and `add_food_alias` so callers don't
/// have to pre-clean their input.
fn normalize_aliases(input: Vec<String>) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for raw in input {
        let alias = raw.trim().to_lowercase();
        if alias.is_empty() {
            continue;
        }
        if !out.iter().any(|existing| existing == &alias) {
            out.push(alias);
        }
    }
    out
}

impl FoodService for FoodServiceImpl {
    // ── Foods ───────────────────────────────────────────────────────

    async fn list_foods(&self, organization: Option<String>) -> Result<Vec<FoodApi>, VaultError> {
        let mut q = food::Entity::find().order_by_asc(food::Column::Name);
        if let Some(org) = organization {
            q = q.filter(food::Column::Organization.eq(org));
        }
        let rows = q.all(&self.db).await.map_err(|e| io(e, "list_foods"))?;
        rows.into_iter().map(food_to_api).collect()
    }

    async fn get_food(&self, id: Uuid) -> Result<Option<FoodApi>, VaultError> {
        let row = food::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_food"))?;
        row.map(food_to_api).transpose()
    }

    async fn find_food_by_name(
        &self,
        organization: Option<String>,
        name_or_alias: String,
    ) -> Result<Option<FoodApi>, VaultError> {
        let hit = food::find_food_by_name(&self.db, organization.as_deref(), &name_or_alias)
            .await
            .map_err(|e| io(e, "find_food_by_name"))?;
        hit.map(food_to_api).transpose()
    }

    async fn create_food(&self, request: CreateFoodRequest) -> Result<FoodApi, VaultError> {
        if request.name.trim().is_empty() {
            return Err(VaultError::ParseError("food name is empty".to_string()));
        }
        let aliases = FoodAliasList::from(normalize_aliases(request.aliases));
        let nutrition = parse_json_object(request.nutrition_json.as_deref(), "nutrition_json")?;
        let now = Utc::now();
        let active = food::ActiveModel {
            id: Set(Uuid::new_v4()),
            name: Set(request.name),
            aliases: Set(aliases),
            category: Set(request.category),
            default_unit: Set(request.default_unit),
            organization: Set(request.organization),
            nutrition_per_100g: Set(nutrition),
            notes: Set(request.notes),
            properties: Set(JsonObject::default()),
            created_by: Set(request.created_by),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert food"))?;
        food_to_api(saved)
    }

    async fn update_food(&self, id: Uuid, patch: FoodPatch) -> Result<FoodApi, VaultError> {
        let model = food::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load food"))?
            .ok_or_else(|| VaultError::NotFound(format!("food:{id}")))?;
        let mut active: food::ActiveModel = model.into();
        if let Some(name) = patch.name {
            active.name = Set(name);
        }
        if let Some(cat) = patch.category {
            active.category = Set(Some(cat));
        }
        if let Some(unit) = patch.default_unit {
            active.default_unit = Set(Some(unit));
        }
        if let Some(aliases) = patch.aliases {
            active.aliases = Set(FoodAliasList::from(normalize_aliases(aliases)));
        }
        if let Some(nutrition) = patch.nutrition_json {
            active.nutrition_per_100g = Set(parse_json_object(
                Some(nutrition.as_str()),
                "nutrition_json",
            )?);
        }
        if let Some(notes) = patch.notes {
            active.notes = Set(Some(notes));
        }
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "update food"))?;
        food_to_api(saved)
    }

    async fn delete_food(&self, id: Uuid) -> Result<(), VaultError> {
        food::Entity::delete_by_id(id)
            .exec(&self.db)
            .await
            .map_err(|e| io(e, "delete food"))?;
        Ok(())
    }

    async fn add_food_alias(&self, food_id: Uuid, alias: String) -> Result<FoodApi, VaultError> {
        let model = food::Entity::find_by_id(food_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load food"))?
            .ok_or_else(|| VaultError::NotFound(format!("food:{food_id}")))?;
        let mut current: Vec<String> = model.aliases.clone().into_inner();
        current.push(alias);
        let normalized = normalize_aliases(current);
        let mut active: food::ActiveModel = model.into();
        active.aliases = Set(FoodAliasList::from(normalized));
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "add_food_alias"))?;
        food_to_api(saved)
    }

    // ── Food products ───────────────────────────────────────────────

    async fn list_food_products(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<FoodProductApi>, VaultError> {
        let mut q = food_product::Entity::find().order_by_asc(food_product::Column::Name);
        if let Some(org) = organization {
            q = q.filter(food_product::Column::Organization.eq(org));
        }
        let rows = q
            .all(&self.db)
            .await
            .map_err(|e| io(e, "list_food_products"))?;
        rows.into_iter().map(product_to_api).collect()
    }

    async fn get_food_product(&self, id: Uuid) -> Result<Option<FoodProductApi>, VaultError> {
        let row = food_product::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_food_product"))?;
        row.map(product_to_api).transpose()
    }

    async fn get_food_product_by_barcode(
        &self,
        organization: Option<String>,
        barcode: String,
    ) -> Result<Option<FoodProductApi>, VaultError> {
        let mut q = food_product::Entity::find().filter(food_product::Column::Barcode.eq(barcode));
        q = match organization {
            Some(org) => q.filter(food_product::Column::Organization.eq(org)),
            None => q.filter(food_product::Column::Organization.is_null()),
        };
        let row = q
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_food_product_by_barcode"))?;
        row.map(product_to_api).transpose()
    }

    async fn create_food_product(
        &self,
        request: CreateFoodProductRequest,
    ) -> Result<FoodProductApi, VaultError> {
        if request.name.trim().is_empty() {
            return Err(VaultError::ParseError(
                "food product name is empty".to_string(),
            ));
        }
        if request.source.trim().is_empty() {
            return Err(VaultError::ParseError(
                "food product source is empty".to_string(),
            ));
        }
        // Soft FK: ensure the parent Food exists.
        if food::Entity::find_by_id(request.food_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "verify food parent"))?
            .is_none()
        {
            return Err(VaultError::NotFound(format!("food:{}", request.food_id)));
        }
        let nutrition = parse_json_object(request.nutrition_json.as_deref(), "nutrition_json")?;
        let now = Utc::now();
        let active = food_product::ActiveModel {
            id: Set(Uuid::new_v4()),
            food_id: Set(request.food_id),
            barcode: Set(request.barcode),
            brand: Set(request.brand),
            name: Set(request.name),
            package_size_g: Set(request.package_size_g),
            package_size_label: Set(request.package_size_label),
            source: Set(request.source),
            external_id: Set(request.external_id),
            nutrition_per_100g: Set(nutrition),
            image_url: Set(request.image_url),
            last_synced_at: Set(None),
            organization: Set(request.organization),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert food_product"))?;
        product_to_api(saved)
    }

    async fn update_food_product(
        &self,
        id: Uuid,
        patch: FoodProductPatch,
    ) -> Result<FoodProductApi, VaultError> {
        let model = food_product::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load food_product"))?
            .ok_or_else(|| VaultError::NotFound(format!("food_product:{id}")))?;
        let mut active: food_product::ActiveModel = model.into();
        if let Some(brand) = patch.brand {
            active.brand = Set(Some(brand));
        }
        if let Some(name) = patch.name {
            active.name = Set(name);
        }
        if let Some(g) = patch.package_size_g {
            active.package_size_g = Set(Some(g));
        }
        if let Some(label) = patch.package_size_label {
            active.package_size_label = Set(Some(label));
        }
        if let Some(nutrition) = patch.nutrition_json {
            active.nutrition_per_100g = Set(parse_json_object(
                Some(nutrition.as_str()),
                "nutrition_json",
            )?);
        }
        if let Some(url) = patch.image_url {
            active.image_url = Set(Some(url));
        }
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "update food_product"))?;
        product_to_api(saved)
    }

    async fn link_recipe_ingredient(
        &self,
        recipe_ingredient_id: Uuid,
        food_id: Uuid,
    ) -> Result<(), VaultError> {
        // Soft FK on both sides.
        if food::Entity::find_by_id(food_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "verify food"))?
            .is_none()
        {
            return Err(VaultError::NotFound(format!("food:{food_id}")));
        }
        let model = recipe_ingredient::Entity::find_by_id(recipe_ingredient_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load recipe_ingredient"))?
            .ok_or_else(|| {
                VaultError::NotFound(format!("recipe_ingredient:{recipe_ingredient_id}"))
            })?;
        let mut active: recipe_ingredient::ActiveModel = model.into();
        active.food_id = Set(Some(food_id));
        active.updated_at = Set(Utc::now());
        active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "link_recipe_ingredient"))?;
        Ok(())
    }
}
