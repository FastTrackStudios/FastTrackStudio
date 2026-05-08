//! SeaORM-backed [`PantryService`] implementation.
//!
//! Holds a raw `DatabaseConnection` plus an optional Open Food Facts
//! client. Resolution helpers reuse `crate::food::find_food_by_name`
//! (also used by `CookingService`) to keep barcode/name-resolve paths
//! aligned without hard-coupling to `FoodServiceImpl`.

use std::collections::HashMap;
use std::sync::Arc;

use chrono::{Duration, NaiveDate, Utc};
use sea_orm::{
    ActiveModelTrait, ColumnTrait, DatabaseConnection, EntityTrait, QueryFilter, QueryOrder, Set,
};
use uuid::Uuid;

use crate::food::{self, FoodAliasList};
use crate::food_product;
use crate::meal_plan;
use crate::pantry::{self, PantryItemApi};
use crate::property::JsonObject;
use crate::provider::OpenFoodFactsClient;
use crate::recipe;
use crate::recipe_ingredient;
use crate::service::{
    AddToPantryRequest, ConsumeFromPantryRequest, GenerateShoppingListFromMissingRequest,
    PantryItemPatch, PantryListRequest, PantryService, RecipeMatchView, VaultError,
};
use crate::shopping_list;

use super::helpers::convert_model;

/// Typed dependencies for [`PantryServiceImpl`].
pub struct PantryServiceDeps {
    pub db: DatabaseConnection,
    /// Optional Open Food Facts client. `None` means barcode-driven
    /// `add_to_pantry` falls back to local cache only.
    pub openfoodfacts: Option<Arc<OpenFoodFactsClient>>,
}

#[derive(Clone)]
pub struct PantryServiceImpl {
    db: DatabaseConnection,
    openfoodfacts: Option<Arc<OpenFoodFactsClient>>,
}

impl PantryServiceImpl {
    pub fn new(deps: PantryServiceDeps) -> Self {
        Self {
            db: deps.db,
            openfoodfacts: deps.openfoodfacts,
        }
    }
}

fn io(err: impl std::fmt::Display, ctx: &str) -> VaultError {
    VaultError::IoError(format!("{ctx}: {err}"))
}

fn item_to_api(model: pantry::Model) -> Result<PantryItemApi, VaultError> {
    convert_model::<pantry::Model, PantryItemApi>(model)
}

/// Resolve the (food_id, product_id) pair for a pantry insert. Any path
/// that yields neither errors out — pantry rows must reference at least
/// one of the two so queries can match against them.
async fn resolve_food_and_product(
    db: &DatabaseConnection,
    openfoodfacts: Option<&Arc<OpenFoodFactsClient>>,
    request: &AddToPantryRequest,
) -> Result<(Option<Uuid>, Option<Uuid>), VaultError> {
    // Direct ids take precedence.
    if request.food_id.is_some() || request.product_id.is_some() {
        let mut food_id = request.food_id;
        if let Some(pid) = request.product_id {
            // When product_id is set, copy food_id from the product row when not already given.
            if let Some(prod) = food_product::Entity::find_by_id(pid)
                .one(db)
                .await
                .map_err(|e| io(e, "load food_product"))?
            {
                food_id = food_id.or(Some(prod.food_id));
            } else {
                return Err(VaultError::NotFound(format!("food_product:{pid}")));
            }
        }
        return Ok((food_id, request.product_id));
    }

    // Barcode lookup — cache first, then OFF if available.
    if let Some(barcode) = request.barcode.as_deref() {
        let trimmed = barcode.trim();
        if !trimmed.is_empty() {
            let mut q = food_product::Entity::find()
                .filter(food_product::Column::Barcode.eq(trimmed.to_string()));
            q = match request.organization.as_deref() {
                Some(org) => q.filter(food_product::Column::Organization.eq(org.to_string())),
                None => q.filter(food_product::Column::Organization.is_null()),
            };
            if let Some(cached) = q.one(db).await.map_err(|e| io(e, "barcode cache lookup"))? {
                return Ok((Some(cached.food_id), Some(cached.id)));
            }

            // Cache miss — try OFF.
            if let Some(client) = openfoodfacts {
                let fetched = client
                    .lookup(trimmed)
                    .await
                    .map_err(|err| VaultError::ParseError(format!("openfoodfacts: {err}")))?;
                if let Some(product) = fetched {
                    // Resolve / auto-create a Food row to anchor the product.
                    let food_id = match product.product_name.as_deref() {
                        Some(name) => {
                            match food::find_food_by_name(db, request.organization.as_deref(), name)
                                .await
                                .map_err(|e| io(e, "resolve food by product name"))?
                            {
                                Some(hit) => hit.id,
                                None => {
                                    create_food_shell(db, request.organization.as_deref(), name)
                                        .await?
                                }
                            }
                        }
                        None => {
                            create_food_shell(db, request.organization.as_deref(), &product.barcode)
                                .await?
                        }
                    };
                    let now = Utc::now();
                    let active = food_product::ActiveModel {
                        id: Set(Uuid::new_v4()),
                        food_id: Set(food_id),
                        barcode: Set(Some(trimmed.to_string())),
                        brand: Set(product.brands.clone()),
                        name: Set(product
                            .product_name
                            .clone()
                            .filter(|s| !s.trim().is_empty())
                            .unwrap_or_else(|| product.barcode.clone())),
                        package_size_g: Set(product.package_size_g),
                        package_size_label: Set(product.quantity_label.clone()),
                        source: Set("openfoodfacts".to_string()),
                        external_id: Set(Some(product.barcode.clone())),
                        nutrition_per_100g: Set(product.nutrition.to_json_object()),
                        image_url: Set(product.image_url.clone()),
                        last_synced_at: Set(Some(now)),
                        organization: Set(request.organization.clone()),
                        properties: Set(JsonObject::default()),
                        created_at: Set(now),
                        updated_at: Set(now),
                    };
                    let saved = active
                        .insert(db)
                        .await
                        .map_err(|e| io(e, "insert OFF food_product"))?;
                    return Ok((Some(saved.food_id), Some(saved.id)));
                }
            }

            // Provider missing or returned None.
            if request.allow_manual_product {
                let food_id =
                    create_food_shell(db, request.organization.as_deref(), trimmed).await?;
                let now = Utc::now();
                let active = food_product::ActiveModel {
                    id: Set(Uuid::new_v4()),
                    food_id: Set(food_id),
                    barcode: Set(Some(trimmed.to_string())),
                    brand: Set(None),
                    name: Set(trimmed.to_string()),
                    package_size_g: Set(None),
                    package_size_label: Set(None),
                    source: Set("manual".to_string()),
                    external_id: Set(None),
                    nutrition_per_100g: Set(JsonObject::default()),
                    image_url: Set(None),
                    last_synced_at: Set(None),
                    organization: Set(request.organization.clone()),
                    properties: Set(JsonObject::default()),
                    created_at: Set(now),
                    updated_at: Set(now),
                };
                let saved = active
                    .insert(db)
                    .await
                    .map_err(|e| io(e, "insert manual food_product shell"))?;
                return Ok((Some(saved.food_id), Some(saved.id)));
            }
            return Err(VaultError::NotFound(format!(
                "barcode {trimmed} not found in cache or Open Food Facts"
            )));
        }
    }

    // Name match.
    if let Some(name) = request.food_name.as_deref() {
        if !name.trim().is_empty() {
            if let Some(hit) = food::find_food_by_name(db, request.organization.as_deref(), name)
                .await
                .map_err(|e| io(e, "find_food_by_name"))?
            {
                return Ok((Some(hit.id), None));
            }
            return Err(VaultError::NotFound(format!("food:{name}")));
        }
    }

    Err(VaultError::ParseError(
        "add_to_pantry needs one of: barcode, food_name, food_id, product_id".to_string(),
    ))
}

async fn create_food_shell(
    db: &DatabaseConnection,
    organization: Option<&str>,
    name: &str,
) -> Result<Uuid, VaultError> {
    let now = Utc::now();
    let active = food::ActiveModel {
        id: Set(Uuid::new_v4()),
        name: Set(name.to_string()),
        aliases: Set(FoodAliasList::default()),
        category: Set(None),
        default_unit: Set(None),
        organization: Set(organization.map(str::to_string)),
        nutrition_per_100g: Set(JsonObject::default()),
        notes: Set(Some("auto-created by pantry resolver".to_string())),
        properties: Set(JsonObject::default()),
        created_by: Set(Some("pantry-resolver".to_string())),
        created_at: Set(now),
        updated_at: Set(now),
    };
    let saved = active
        .insert(db)
        .await
        .map_err(|e| io(e, "create food shell"))?;
    Ok(saved.id)
}

impl PantryService for PantryServiceImpl {
    async fn list_pantry_items(
        &self,
        request: PantryListRequest,
    ) -> Result<Vec<PantryItemApi>, VaultError> {
        let mut q = pantry::Entity::find().order_by_asc(pantry::Column::ExpirationDate);
        q = match request.organization.as_deref() {
            Some(org) => q.filter(pantry::Column::Organization.eq(org.to_string())),
            None => q.filter(pantry::Column::Organization.is_null()),
        };
        if let Some(loc) = request.location_id {
            q = q.filter(pantry::Column::LocationId.eq(loc));
        }
        let mut rows = q
            .all(&self.db)
            .await
            .map_err(|e| io(e, "list_pantry_items"))?;

        if request.low_stock_only {
            rows.retain(|r| {
                r.min_stock
                    .map(|threshold| r.quantity <= threshold)
                    .unwrap_or(false)
            });
        }
        if let Some(within) = request.expiring_within_days {
            let today = Utc::now().date_naive();
            let cutoff = today + Duration::days(i64::from(within));
            rows.retain(|r| {
                r.expiration_date
                    .map(|d| d >= today && d <= cutoff)
                    .unwrap_or(false)
            });
        }

        rows.into_iter().map(item_to_api).collect()
    }

    async fn get_pantry_item(&self, id: Uuid) -> Result<Option<PantryItemApi>, VaultError> {
        let row = pantry::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_pantry_item"))?;
        row.map(item_to_api).transpose()
    }

    async fn add_to_pantry(
        &self,
        request: AddToPantryRequest,
    ) -> Result<PantryItemApi, VaultError> {
        if request.unit.trim().is_empty() {
            return Err(VaultError::ParseError("unit is empty".to_string()));
        }
        let (food_id, product_id) =
            resolve_food_and_product(&self.db, self.openfoodfacts.as_ref(), &request).await?;
        if food_id.is_none() && product_id.is_none() {
            return Err(VaultError::ParseError(
                "could not resolve food_id or product_id".to_string(),
            ));
        }
        let now = Utc::now();
        let active = pantry::ActiveModel {
            id: Set(Uuid::new_v4()),
            food_id: Set(food_id),
            product_id: Set(product_id),
            location_id: Set(request.location_id),
            quantity: Set(request.quantity),
            unit: Set(request.unit),
            expiration_date: Set(request.expiration_date),
            opened_at: Set(None),
            min_stock: Set(request.min_stock),
            purchased_at: Set(request.purchased_at),
            notes: Set(request.notes),
            organization: Set(request.organization),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert pantry_item"))?;
        item_to_api(saved)
    }

    async fn consume_from_pantry(
        &self,
        request: ConsumeFromPantryRequest,
    ) -> Result<Option<PantryItemApi>, VaultError> {
        // Resolve target row.
        let row = if let Some(id) = request.pantry_item_id {
            pantry::Entity::find_by_id(id)
                .one(&self.db)
                .await
                .map_err(|e| io(e, "load pantry_item"))?
                .ok_or_else(|| VaultError::NotFound(format!("pantry_item:{id}")))?
        } else {
            let mut q = pantry::Entity::find();
            q = match request.organization.as_deref() {
                Some(org) => q.filter(pantry::Column::Organization.eq(org.to_string())),
                None => q.filter(pantry::Column::Organization.is_null()),
            };
            if let Some(food_id) = request.food_id {
                q = q.filter(pantry::Column::FoodId.eq(food_id));
            } else if let Some(product_id) = request.product_id {
                q = q.filter(pantry::Column::ProductId.eq(product_id));
            } else {
                return Err(VaultError::ParseError(
                    "consume needs one of pantry_item_id, food_id, product_id".to_string(),
                ));
            }
            // Closest-to-expiration first; rows without expiration_date sort last.
            let mut rows = q
                .all(&self.db)
                .await
                .map_err(|e| io(e, "load candidate pantry rows"))?;
            rows.sort_by(|a, b| match (a.expiration_date, b.expiration_date) {
                (Some(x), Some(y)) => x.cmp(&y),
                (Some(_), None) => std::cmp::Ordering::Less,
                (None, Some(_)) => std::cmp::Ordering::Greater,
                (None, None) => std::cmp::Ordering::Equal,
            });
            rows.into_iter()
                .next()
                .ok_or_else(|| VaultError::NotFound("no matching pantry row".to_string()))?
        };

        let new_qty = row.quantity - request.amount;
        if new_qty <= 0.0 {
            let id = row.id;
            pantry::Entity::delete_by_id(id)
                .exec(&self.db)
                .await
                .map_err(|e| io(e, "delete depleted pantry_item"))?;
            return Ok(None);
        }
        let mut active: pantry::ActiveModel = row.into();
        active.quantity = Set(new_qty);
        if !request.unit.trim().is_empty() {
            active.unit = Set(request.unit);
        }
        if let Some(notes) = request.notes {
            active.notes = Set(Some(notes));
        }
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "update pantry_item"))?;
        Ok(Some(item_to_api(saved)?))
    }

    async fn update_pantry_item(
        &self,
        id: Uuid,
        patch: PantryItemPatch,
    ) -> Result<PantryItemApi, VaultError> {
        let model = pantry::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load pantry_item"))?
            .ok_or_else(|| VaultError::NotFound(format!("pantry_item:{id}")))?;
        let mut active: pantry::ActiveModel = model.into();
        if let Some(q) = patch.quantity {
            active.quantity = Set(q);
        }
        if let Some(u) = patch.unit {
            active.unit = Set(u);
        }
        if let Some(loc) = patch.location_id {
            active.location_id = Set(Some(loc));
        }
        if let Some(date) = patch.expiration_date {
            active.expiration_date = Set(Some(date));
        }
        if let Some(opened) = patch.opened_at {
            active.opened_at = Set(Some(opened));
        }
        if let Some(min) = patch.min_stock {
            active.min_stock = Set(Some(min));
        }
        if let Some(notes) = patch.notes {
            active.notes = Set(Some(notes));
        }
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "update pantry_item"))?;
        item_to_api(saved)
    }

    async fn delete_pantry_item(&self, id: Uuid) -> Result<(), VaultError> {
        pantry::Entity::delete_by_id(id)
            .exec(&self.db)
            .await
            .map_err(|e| io(e, "delete pantry_item"))?;
        Ok(())
    }

    async fn expiring_soon(
        &self,
        organization: Option<String>,
        within_days: u32,
    ) -> Result<Vec<PantryItemApi>, VaultError> {
        let today = Utc::now().date_naive();
        let cutoff = today + Duration::days(i64::from(within_days));
        let mut q = pantry::Entity::find()
            .filter(pantry::Column::ExpirationDate.is_not_null())
            .filter(pantry::Column::ExpirationDate.gte(today))
            .filter(pantry::Column::ExpirationDate.lte(cutoff))
            .order_by_asc(pantry::Column::ExpirationDate);
        q = match organization.as_deref() {
            Some(org) => q.filter(pantry::Column::Organization.eq(org.to_string())),
            None => q.filter(pantry::Column::Organization.is_null()),
        };
        let rows = q.all(&self.db).await.map_err(|e| io(e, "expiring_soon"))?;
        rows.into_iter().map(item_to_api).collect()
    }

    async fn low_stock(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<PantryItemApi>, VaultError> {
        let mut q = pantry::Entity::find().filter(pantry::Column::MinStock.is_not_null());
        q = match organization.as_deref() {
            Some(org) => q.filter(pantry::Column::Organization.eq(org.to_string())),
            None => q.filter(pantry::Column::Organization.is_null()),
        };
        let mut rows = q.all(&self.db).await.map_err(|e| io(e, "low_stock"))?;
        rows.retain(|r| {
            r.min_stock
                .map(|threshold| r.quantity <= threshold)
                .unwrap_or(false)
        });
        rows.into_iter().map(item_to_api).collect()
    }

    async fn recipes_i_can_cook(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<RecipeMatchView>, VaultError> {
        // Load recipes scoped to org.
        let mut rq = recipe::Entity::find().order_by_asc(recipe::Column::Name);
        rq = match organization.as_deref() {
            Some(org) => rq.filter(recipe::Column::Organization.eq(org.to_string())),
            None => rq.filter(recipe::Column::Organization.is_null()),
        };
        let recipes = rq.all(&self.db).await.map_err(|e| io(e, "load recipes"))?;

        // Snapshot pantry once and build a food_id index.
        let mut pq = pantry::Entity::find();
        pq = match organization.as_deref() {
            Some(org) => pq.filter(pantry::Column::Organization.eq(org.to_string())),
            None => pq.filter(pantry::Column::Organization.is_null()),
        };
        let pantry_rows = pq
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load pantry snapshot"))?;
        let mut by_food: HashMap<Uuid, Vec<pantry::Model>> = HashMap::new();
        for row in pantry_rows {
            if let Some(food_id) = row.food_id {
                by_food.entry(food_id).or_default().push(row);
            }
        }

        let mut out = Vec::new();
        for r in recipes {
            let ingredients = recipe_ingredient::Entity::find()
                .filter(recipe_ingredient::Column::RecipeId.eq(r.id))
                .filter(recipe_ingredient::Column::IsSection.eq(false))
                .all(&self.db)
                .await
                .map_err(|e| io(e, "load recipe ingredients"))?;
            let with_food: Vec<_> = ingredients.iter().filter(|i| i.food_id.is_some()).collect();
            if with_food.is_empty() {
                // Skip recipes whose ingredients have no food_id at all.
                continue;
            }
            let total = with_food.len() as u32;
            let mut matched = 0u32;
            let mut unmatched = Vec::new();
            let mut warnings = Vec::new();
            for ing in &with_food {
                let food_id = ing.food_id.expect("filtered above");
                let pantry_rows = by_food.get(&food_id);
                if let Some(rows) = pantry_rows {
                    matched += 1;
                    // Unit comparison — emit a warning when units don't
                    // line up. Conservative: the matcher doesn't attempt
                    // arithmetic across units (g <-> ml depends on
                    // density, etc.).
                    if let Some(req_unit) = ing.unit.as_deref() {
                        let req_norm = req_unit.trim().to_ascii_lowercase();
                        if !req_norm.is_empty()
                            && !rows
                                .iter()
                                .any(|p| p.unit.trim().to_ascii_lowercase() == req_norm)
                        {
                            warnings.push(format!(
                                "{}: recipe asks for unit '{}', pantry rows use {:?}",
                                ing.food,
                                req_unit,
                                rows.iter().map(|p| p.unit.clone()).collect::<Vec<_>>()
                            ));
                        }
                    }
                } else {
                    unmatched.push(ing.food.clone());
                }
            }
            out.push(RecipeMatchView {
                recipe_id: r.id,
                recipe_name: r.name.clone(),
                total_ingredients: total,
                matched_ingredients: matched,
                unmatched_food_lines: unmatched,
                warnings,
            });
        }
        Ok(out)
    }

    async fn generate_shopping_list_from_missing(
        &self,
        request: GenerateShoppingListFromMissingRequest,
    ) -> Result<Uuid, VaultError> {
        // Snapshot pantry food_ids.
        let mut pq = pantry::Entity::find();
        pq = match request.organization.as_deref() {
            Some(org) => pq.filter(pantry::Column::Organization.eq(org.to_string())),
            None => pq.filter(pantry::Column::Organization.is_null()),
        };
        let pantry_rows = pq
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load pantry snapshot"))?;
        let mut covered: HashMap<Uuid, Vec<String>> = HashMap::new();
        for row in pantry_rows {
            if let Some(fid) = row.food_id {
                covered.entry(fid).or_default().push(row.unit);
            }
        }

        // Same query shape as CookingService::generate_from_meal_plan.
        let mut q = meal_plan::Entity::find()
            .filter(meal_plan::Column::Date.gte(request.from_date))
            .filter(meal_plan::Column::Date.lte(request.to_date))
            .filter(meal_plan::Column::RecipeId.is_not_null());
        if let Some(org) = request.organization.as_deref() {
            q = q.filter(meal_plan::Column::Organization.eq(org));
        }
        let entries = q
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load meal_plan range"))?;
        let now = Utc::now();

        let mut next_seq = shopping_list::ItemEntity::find()
            .filter(shopping_list::ItemColumn::ListId.eq(request.list_id))
            .order_by_desc(shopping_list::ItemColumn::Sequence)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "next item seq"))?
            .map(|r| r.sequence + 1)
            .unwrap_or(1);

        // TODO: dedupe / quantity-sum across recipes — current behavior
        // matches CookingService::generate_from_meal_plan: two recipes
        // calling for "1 cup flour" produce two list rows.
        for entry in entries {
            let recipe_id = match entry.recipe_id {
                Some(r) => r,
                None => continue,
            };
            let ingredients = recipe_ingredient::Entity::find()
                .filter(recipe_ingredient::Column::RecipeId.eq(recipe_id))
                .order_by_asc(recipe_ingredient::Column::Sequence)
                .all(&self.db)
                .await
                .map_err(|e| io(e, "load recipe ingredients"))?;
            for ing in ingredients {
                if ing.is_section {
                    continue;
                }
                if let Some(fid) = ing.food_id {
                    if let Some(units) = covered.get(&fid) {
                        // Same-unit match → drop. Cross-unit: still drop
                        // (we have *some* of that food). The unit warning
                        // surfaces in `recipes_i_can_cook` instead.
                        let req_unit = ing
                            .unit
                            .as_deref()
                            .unwrap_or("")
                            .trim()
                            .to_ascii_lowercase();
                        if req_unit.is_empty()
                            || units
                                .iter()
                                .any(|u| u.trim().to_ascii_lowercase() == req_unit)
                            || !units.is_empty()
                        {
                            continue;
                        }
                    }
                }
                let active = shopping_list::ItemActiveModel {
                    id: Set(Uuid::new_v4()),
                    list_id: Set(request.list_id),
                    sequence: Set(next_seq),
                    quantity: Set(ing.quantity),
                    unit: Set(ing.unit.clone()),
                    food: Set(ing.food.clone()),
                    note: Set(ing.note.clone()),
                    recipe_id: Set(Some(recipe_id)),
                    meal_plan_id: Set(Some(entry.id)),
                    checked: Set(false),
                    label: Set(None),
                    created_at: Set(now),
                    updated_at: Set(now),
                };
                active
                    .insert(&self.db)
                    .await
                    .map_err(|e| io(e, "insert generated item"))?;
                next_seq += 1;
            }
        }

        // Avoid unused: NaiveDate referenced via request fields only.
        let _ = NaiveDate::from_ymd_opt(2000, 1, 1);

        Ok(request.list_id)
    }
}
