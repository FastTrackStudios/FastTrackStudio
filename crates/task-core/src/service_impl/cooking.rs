//! SeaORM-backed [`CookingService`] implementation.
//!
//! Holds a raw `DatabaseConnection` because most operations span more
//! than one table:
//! - `create_recipe` / `update_recipe` insert/replace child ingredients
//!   and steps in one transaction.
//! - `generate_from_meal_plan` joins `meal_plan_entries` against
//!   `recipe_ingredients`.
//! - `add_recipe_to_cookbook` enforces the unique `(cookbook_id,
//!   recipe_id)` pair.
//!
//! Slug uniqueness rule: when a `Recipe` row would conflict with an
//! existing `(organization, slug)`, we append `-2`, `-3`, … until we
//! find a free slot.

use chrono::{NaiveDate, Utc};
use sea_orm::{
    ActiveModelTrait, ColumnTrait, DatabaseConnection, EntityTrait, QueryFilter, QueryOrder, Set,
    TransactionTrait,
};
use uuid::Uuid;

use crate::cookbook::{self, CookbookApi};
use crate::cookbook_recipe;
use crate::meal_plan::{self, MealPlanEntryApi, MealType};
use crate::property::JsonObject;
use crate::recipe::{self, RecipeApi, RecipeIngredientSpec, RecipeStepSpec};
use crate::recipe_ingredient::{self, RecipeIngredientApi};
use crate::recipe_step::{self, RecipeStepApi};
use crate::service::{
    AddShoppingItemRequest, CookbookWithRecipes, CookingService, CreateRecipeRequest,
    GenerateShoppingListRequest, MealPlanRangeRequest, RecipePatch, RecipeWithDetails,
    SetMealPlanEntryRequest, ShoppingListWithItems, VaultError,
};
use crate::shopping_list::{self, ShoppingListApi, ShoppingListItemApi};

use super::helpers::convert_model;

/// Typed dependencies for [`CookingServiceImpl`].
pub struct CookingServiceDeps {
    pub db: DatabaseConnection,
}

#[derive(Clone)]
pub struct CookingServiceImpl {
    db: DatabaseConnection,
}

impl CookingServiceImpl {
    pub fn new(deps: CookingServiceDeps) -> Self {
        Self { db: deps.db }
    }
}

fn io(err: impl std::fmt::Display, ctx: &str) -> VaultError {
    VaultError::IoError(format!("{ctx}: {err}"))
}

fn parse(err: impl std::fmt::Display, ctx: &str) -> VaultError {
    VaultError::ParseError(format!("{ctx}: {err}"))
}

fn to_slug(name: &str) -> String {
    let s = slug::slugify(name);
    if s.is_empty() {
        "recipe".to_string()
    } else {
        s
    }
}

async fn unique_slug(
    db: &DatabaseConnection,
    base: &str,
    organization: Option<&str>,
) -> Result<String, VaultError> {
    let mut candidate = base.to_string();
    let mut suffix = 2u32;
    loop {
        let mut q = recipe::Entity::find().filter(recipe::Column::Slug.eq(candidate.clone()));
        q = match organization {
            Some(org) => q.filter(recipe::Column::Organization.eq(org)),
            None => q.filter(recipe::Column::Organization.is_null()),
        };
        let collision = q
            .one(db)
            .await
            .map_err(|e| io(e, "slug uniqueness check"))?
            .is_some();
        if !collision {
            return Ok(candidate);
        }
        candidate = format!("{base}-{suffix}");
        suffix += 1;
        if suffix > 1000 {
            return Err(VaultError::IoError(format!(
                "exhausted slug suffixes for base {base}"
            )));
        }
    }
}

fn ingredient_specs_from_json(payload: &str) -> Result<Vec<RecipeIngredientSpec>, VaultError> {
    if payload.trim().is_empty() {
        return Ok(Vec::new());
    }
    serde_json::from_str(payload).map_err(|e| parse(e, "ingredients_json"))
}

fn step_specs_from_json(payload: &str) -> Result<Vec<RecipeStepSpec>, VaultError> {
    if payload.trim().is_empty() {
        return Ok(Vec::new());
    }
    serde_json::from_str(payload).map_err(|e| parse(e, "steps_json"))
}

fn recipe_to_api(model: recipe::Model) -> Result<RecipeApi, VaultError> {
    convert_model::<recipe::Model, RecipeApi>(model)
}

fn cookbook_to_api(model: cookbook::Model) -> Result<CookbookApi, VaultError> {
    convert_model::<cookbook::Model, CookbookApi>(model)
}

fn meal_plan_to_api(model: meal_plan::Model) -> Result<MealPlanEntryApi, VaultError> {
    convert_model::<meal_plan::Model, MealPlanEntryApi>(model)
}

fn shopping_list_to_api(model: shopping_list::Model) -> Result<ShoppingListApi, VaultError> {
    convert_model::<shopping_list::Model, ShoppingListApi>(model)
}

fn item_to_api(model: shopping_list::Item) -> Result<ShoppingListItemApi, VaultError> {
    convert_model::<shopping_list::Item, ShoppingListItemApi>(model)
}

fn ingredient_to_api(model: recipe_ingredient::Model) -> Result<RecipeIngredientApi, VaultError> {
    convert_model::<recipe_ingredient::Model, RecipeIngredientApi>(model)
}

fn step_to_api(model: recipe_step::Model) -> Result<RecipeStepApi, VaultError> {
    convert_model::<recipe_step::Model, RecipeStepApi>(model)
}

fn ingredient_active_for(
    recipe_id: Uuid,
    spec: &RecipeIngredientSpec,
    sequence: u32,
    now: chrono::DateTime<Utc>,
) -> recipe_ingredient::ActiveModel {
    recipe_ingredient::ActiveModel {
        id: Set(Uuid::new_v4()),
        recipe_id: Set(recipe_id),
        sequence: Set(spec.sequence.unwrap_or(sequence)),
        quantity: Set(spec.quantity),
        unit: Set(spec.unit.clone()),
        food: Set(spec.food.clone()),
        note: Set(spec.note.clone()),
        is_section: Set(spec.is_section.unwrap_or(false)),
        created_at: Set(now),
        updated_at: Set(now),
    }
}

fn step_active_for(
    recipe_id: Uuid,
    spec: &RecipeStepSpec,
    sequence: u32,
    now: chrono::DateTime<Utc>,
) -> recipe_step::ActiveModel {
    recipe_step::ActiveModel {
        id: Set(Uuid::new_v4()),
        recipe_id: Set(recipe_id),
        sequence: Set(spec.sequence.unwrap_or(sequence)),
        text: Set(spec.text.clone()),
        duration_minutes: Set(spec.duration_minutes),
        created_at: Set(now),
        updated_at: Set(now),
    }
}

impl CookingService for CookingServiceImpl {
    // ── Recipes ─────────────────────────────────────────────────────

    async fn list_recipes(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<RecipeApi>, VaultError> {
        let mut q = recipe::Entity::find().order_by_asc(recipe::Column::Name);
        if let Some(org) = organization {
            q = q.filter(recipe::Column::Organization.eq(org));
        }
        let rows = q.all(&self.db).await.map_err(|e| io(e, "list_recipes"))?;
        rows.into_iter().map(recipe_to_api).collect()
    }

    async fn get_recipe(&self, id: Uuid) -> Result<Option<RecipeWithDetails>, VaultError> {
        let Some(model) = recipe::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_recipe"))?
        else {
            return Ok(None);
        };
        let ingredients = recipe_ingredient::Entity::find()
            .filter(recipe_ingredient::Column::RecipeId.eq(id))
            .order_by_asc(recipe_ingredient::Column::Sequence)
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load ingredients"))?;
        let steps = recipe_step::Entity::find()
            .filter(recipe_step::Column::RecipeId.eq(id))
            .order_by_asc(recipe_step::Column::Sequence)
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load steps"))?;
        let ingredient_apis = ingredients
            .into_iter()
            .map(ingredient_to_api)
            .collect::<Result<Vec<_>, _>>()?;
        let step_apis = steps
            .into_iter()
            .map(step_to_api)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Some(RecipeWithDetails {
            recipe: recipe_to_api(model)?,
            ingredients_json: serde_json::to_string(&ingredient_apis)
                .map_err(|e| io(e, "serialize ingredients"))?,
            steps_json: serde_json::to_string(&step_apis).map_err(|e| io(e, "serialize steps"))?,
        }))
    }

    async fn create_recipe(
        &self,
        request: CreateRecipeRequest,
    ) -> Result<RecipeWithDetails, VaultError> {
        if request.name.trim().is_empty() {
            return Err(VaultError::ParseError("recipe name is empty".to_string()));
        }
        let ingredients = ingredient_specs_from_json(&request.ingredients_json)?;
        let steps = step_specs_from_json(&request.steps_json)?;

        let base_slug = to_slug(&request.name);
        let slug = unique_slug(&self.db, &base_slug, request.organization.as_deref()).await?;

        let now = Utc::now();
        let recipe_id = Uuid::new_v4();

        let total = match (request.prep_time_minutes, request.cook_time_minutes) {
            (Some(p), Some(c)) => Some(p + c),
            _ => None,
        };

        let active = recipe::ActiveModel {
            id: Set(recipe_id),
            name: Set(request.name.clone()),
            slug: Set(slug),
            description: Set(request.description),
            organization: Set(request.organization),
            prep_time_minutes: Set(request.prep_time_minutes),
            cook_time_minutes: Set(request.cook_time_minutes),
            total_time_minutes: Set(total),
            servings: Set(request.servings),
            yield_label: Set(None),
            source_url: Set(request.source_url),
            rating: Set(None),
            last_made: Set(None),
            notes: Set(None),
            created_by: Set(request.created_by),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };

        let txn = self
            .db
            .begin()
            .await
            .map_err(|e| io(e, "begin create_recipe txn"))?;
        active
            .insert(&txn)
            .await
            .map_err(|e| io(e, "insert recipe"))?;
        for (idx, spec) in ingredients.iter().enumerate() {
            ingredient_active_for(recipe_id, spec, (idx + 1) as u32, now)
                .insert(&txn)
                .await
                .map_err(|e| io(e, "insert ingredient"))?;
        }
        for (idx, spec) in steps.iter().enumerate() {
            step_active_for(recipe_id, spec, (idx + 1) as u32, now)
                .insert(&txn)
                .await
                .map_err(|e| io(e, "insert step"))?;
        }
        txn.commit()
            .await
            .map_err(|e| io(e, "commit create_recipe txn"))?;

        // Hand off to get_recipe so the response shape stays consistent.
        self.get_recipe(recipe_id)
            .await?
            .ok_or_else(|| VaultError::IoError("recipe disappeared post-insert".to_string()))
    }

    async fn update_recipe(&self, id: Uuid, patch: RecipePatch) -> Result<RecipeApi, VaultError> {
        let model = recipe::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load recipe"))?
            .ok_or_else(|| VaultError::NotFound(format!("recipe:{id}")))?;
        let new_ingredients = match patch.ingredients_json.as_deref() {
            Some(payload) => Some(ingredient_specs_from_json(payload)?),
            None => None,
        };
        let new_steps = match patch.steps_json.as_deref() {
            Some(payload) => Some(step_specs_from_json(payload)?),
            None => None,
        };

        let now = Utc::now();
        let txn = self
            .db
            .begin()
            .await
            .map_err(|e| io(e, "begin update_recipe txn"))?;

        let mut active: recipe::ActiveModel = model.into();
        if let Some(name) = patch.name {
            active.name = Set(name);
        }
        if let Some(desc) = patch.description {
            active.description = Set(Some(desc));
        }
        if let Some(prep) = patch.prep_time_minutes {
            active.prep_time_minutes = Set(Some(prep));
        }
        if let Some(cook) = patch.cook_time_minutes {
            active.cook_time_minutes = Set(Some(cook));
        }
        if let Some(servings) = patch.servings {
            active.servings = Set(Some(servings));
        }
        if let Some(url) = patch.source_url {
            active.source_url = Set(Some(url));
        }
        if let Some(yield_label) = patch.yield_label {
            active.yield_label = Set(Some(yield_label));
        }
        if let Some(notes) = patch.notes {
            active.notes = Set(Some(notes));
        }
        active.updated_at = Set(now);
        let saved = active
            .update(&txn)
            .await
            .map_err(|e| io(e, "update recipe"))?;

        if let Some(specs) = new_ingredients {
            recipe_ingredient::Entity::delete_many()
                .filter(recipe_ingredient::Column::RecipeId.eq(id))
                .exec(&txn)
                .await
                .map_err(|e| io(e, "clear ingredients"))?;
            for (idx, spec) in specs.iter().enumerate() {
                ingredient_active_for(id, spec, (idx + 1) as u32, now)
                    .insert(&txn)
                    .await
                    .map_err(|e| io(e, "insert ingredient"))?;
            }
        }
        if let Some(specs) = new_steps {
            recipe_step::Entity::delete_many()
                .filter(recipe_step::Column::RecipeId.eq(id))
                .exec(&txn)
                .await
                .map_err(|e| io(e, "clear steps"))?;
            for (idx, spec) in specs.iter().enumerate() {
                step_active_for(id, spec, (idx + 1) as u32, now)
                    .insert(&txn)
                    .await
                    .map_err(|e| io(e, "insert step"))?;
            }
        }

        txn.commit()
            .await
            .map_err(|e| io(e, "commit update_recipe txn"))?;
        recipe_to_api(saved)
    }

    async fn delete_recipe(&self, id: Uuid) -> Result<(), VaultError> {
        let txn = self
            .db
            .begin()
            .await
            .map_err(|e| io(e, "begin delete_recipe txn"))?;
        recipe_ingredient::Entity::delete_many()
            .filter(recipe_ingredient::Column::RecipeId.eq(id))
            .exec(&txn)
            .await
            .map_err(|e| io(e, "delete ingredients"))?;
        recipe_step::Entity::delete_many()
            .filter(recipe_step::Column::RecipeId.eq(id))
            .exec(&txn)
            .await
            .map_err(|e| io(e, "delete steps"))?;
        cookbook_recipe::Entity::delete_many()
            .filter(cookbook_recipe::Column::RecipeId.eq(id))
            .exec(&txn)
            .await
            .map_err(|e| io(e, "delete cookbook joins"))?;
        recipe::Entity::delete_by_id(id)
            .exec(&txn)
            .await
            .map_err(|e| io(e, "delete recipe"))?;
        txn.commit()
            .await
            .map_err(|e| io(e, "commit delete_recipe txn"))?;
        Ok(())
    }

    async fn rate_recipe(&self, id: Uuid, rating: f32) -> Result<RecipeApi, VaultError> {
        if !(0.0..=5.0).contains(&rating) {
            return Err(VaultError::ParseError(format!(
                "rating {rating} outside 0.0..=5.0"
            )));
        }
        let model = recipe::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load recipe"))?
            .ok_or_else(|| VaultError::NotFound(format!("recipe:{id}")))?;
        let mut active: recipe::ActiveModel = model.into();
        active.rating = Set(Some(rating));
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "rate recipe"))?;
        recipe_to_api(saved)
    }

    async fn mark_made(
        &self,
        id: Uuid,
        on_date: Option<NaiveDate>,
    ) -> Result<RecipeApi, VaultError> {
        let model = recipe::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load recipe"))?
            .ok_or_else(|| VaultError::NotFound(format!("recipe:{id}")))?;
        let when = on_date.unwrap_or_else(|| chrono::Local::now().date_naive());
        let mut active: recipe::ActiveModel = model.into();
        active.last_made = Set(Some(when));
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "mark_made"))?;
        recipe_to_api(saved)
    }

    // ── Cookbooks ───────────────────────────────────────────────────

    async fn list_cookbooks(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<CookbookApi>, VaultError> {
        let mut q = cookbook::Entity::find().order_by_asc(cookbook::Column::Name);
        if let Some(org) = organization {
            q = q.filter(cookbook::Column::Organization.eq(org));
        }
        let rows = q.all(&self.db).await.map_err(|e| io(e, "list_cookbooks"))?;
        rows.into_iter().map(cookbook_to_api).collect()
    }

    async fn get_cookbook(&self, id: Uuid) -> Result<Option<CookbookWithRecipes>, VaultError> {
        let Some(model) = cookbook::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_cookbook"))?
        else {
            return Ok(None);
        };
        let joins = cookbook_recipe::Entity::find()
            .filter(cookbook_recipe::Column::CookbookId.eq(id))
            .order_by_asc(cookbook_recipe::Column::Sequence)
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load cookbook joins"))?;
        let mut recipes: Vec<RecipeApi> = Vec::with_capacity(joins.len());
        for join in joins {
            if let Some(r) = recipe::Entity::find_by_id(join.recipe_id)
                .one(&self.db)
                .await
                .map_err(|e| io(e, "load joined recipe"))?
            {
                recipes.push(recipe_to_api(r)?);
            }
        }
        Ok(Some(CookbookWithRecipes {
            cookbook: cookbook_to_api(model)?,
            recipes_json: serde_json::to_string(&recipes)
                .map_err(|e| io(e, "serialize cookbook recipes"))?,
        }))
    }

    async fn create_cookbook(
        &self,
        name: String,
        description: Option<String>,
        organization: Option<String>,
    ) -> Result<CookbookApi, VaultError> {
        if name.trim().is_empty() {
            return Err(VaultError::ParseError("cookbook name is empty".to_string()));
        }
        let now = Utc::now();
        let active = cookbook::ActiveModel {
            id: Set(Uuid::new_v4()),
            name: Set(name),
            description: Set(description),
            organization: Set(organization),
            created_by: Set(None),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert cookbook"))?;
        cookbook_to_api(saved)
    }

    async fn add_recipe_to_cookbook(
        &self,
        cookbook_id: Uuid,
        recipe_id: Uuid,
    ) -> Result<(), VaultError> {
        // Idempotent — skip if the pair already exists.
        let existing = cookbook_recipe::Entity::find()
            .filter(cookbook_recipe::Column::CookbookId.eq(cookbook_id))
            .filter(cookbook_recipe::Column::RecipeId.eq(recipe_id))
            .one(&self.db)
            .await
            .map_err(|e| io(e, "check cookbook join"))?;
        if existing.is_some() {
            return Ok(());
        }
        let next_seq = cookbook_recipe::Entity::find()
            .filter(cookbook_recipe::Column::CookbookId.eq(cookbook_id))
            .order_by_desc(cookbook_recipe::Column::Sequence)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "next sequence"))?
            .map(|r| r.sequence + 1)
            .unwrap_or(1);
        let now = Utc::now();
        let active = cookbook_recipe::ActiveModel {
            id: Set(Uuid::new_v4()),
            cookbook_id: Set(cookbook_id),
            recipe_id: Set(recipe_id),
            sequence: Set(next_seq),
            added_at: Set(now),
        };
        active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert cookbook join"))?;
        Ok(())
    }

    async fn remove_recipe_from_cookbook(
        &self,
        cookbook_id: Uuid,
        recipe_id: Uuid,
    ) -> Result<(), VaultError> {
        cookbook_recipe::Entity::delete_many()
            .filter(cookbook_recipe::Column::CookbookId.eq(cookbook_id))
            .filter(cookbook_recipe::Column::RecipeId.eq(recipe_id))
            .exec(&self.db)
            .await
            .map_err(|e| io(e, "remove cookbook join"))?;
        Ok(())
    }

    // ── Meal plan ───────────────────────────────────────────────────

    async fn list_meal_plan(
        &self,
        request: MealPlanRangeRequest,
    ) -> Result<Vec<MealPlanEntryApi>, VaultError> {
        let mut q = meal_plan::Entity::find()
            .filter(meal_plan::Column::Date.gte(request.from_date))
            .filter(meal_plan::Column::Date.lte(request.to_date))
            .order_by_asc(meal_plan::Column::Date)
            .order_by_asc(meal_plan::Column::MealType);
        if let Some(org) = request.organization.as_deref() {
            q = q.filter(meal_plan::Column::Organization.eq(org));
        }
        let rows = q.all(&self.db).await.map_err(|e| io(e, "list_meal_plan"))?;
        rows.into_iter().map(meal_plan_to_api).collect()
    }

    async fn set_meal_plan_entry(
        &self,
        request: SetMealPlanEntryRequest,
    ) -> Result<MealPlanEntryApi, VaultError> {
        let meal = MealType::parse(&request.meal_type)
            .ok_or_else(|| parse(&request.meal_type, "unknown meal_type"))?;
        // Slot uniqueness: upsert on (organization, date, meal_type).
        let mut q = meal_plan::Entity::find()
            .filter(meal_plan::Column::Date.eq(request.date))
            .filter(meal_plan::Column::MealType.eq(meal));
        q = match request.organization.as_deref() {
            Some(org) => q.filter(meal_plan::Column::Organization.eq(org)),
            None => q.filter(meal_plan::Column::Organization.is_null()),
        };
        let existing = q.one(&self.db).await.map_err(|e| io(e, "lookup slot"))?;
        let now = Utc::now();
        let saved = if let Some(model) = existing {
            let mut active: meal_plan::ActiveModel = model.into();
            active.recipe_id = Set(request.recipe_id);
            active.title = Set(request.title);
            active.servings_planned = Set(request.servings_planned);
            active.notes = Set(request.notes);
            if let Some(by) = request.created_by {
                active.created_by = Set(Some(by));
            }
            active.updated_at = Set(now);
            active
                .update(&self.db)
                .await
                .map_err(|e| io(e, "update meal_plan"))?
        } else {
            let active = meal_plan::ActiveModel {
                id: Set(Uuid::new_v4()),
                date: Set(request.date),
                meal_type: Set(meal),
                organization: Set(request.organization),
                recipe_id: Set(request.recipe_id),
                title: Set(request.title),
                servings_planned: Set(request.servings_planned),
                notes: Set(request.notes),
                created_by: Set(request.created_by),
                created_at: Set(now),
                updated_at: Set(now),
            };
            active
                .insert(&self.db)
                .await
                .map_err(|e| io(e, "insert meal_plan"))?
        };
        meal_plan_to_api(saved)
    }

    async fn delete_meal_plan_entry(&self, id: Uuid) -> Result<(), VaultError> {
        meal_plan::Entity::delete_by_id(id)
            .exec(&self.db)
            .await
            .map_err(|e| io(e, "delete meal_plan"))?;
        Ok(())
    }

    // ── Shopping lists ──────────────────────────────────────────────

    async fn list_shopping_lists(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<ShoppingListApi>, VaultError> {
        let mut q = shopping_list::Entity::find().order_by_asc(shopping_list::Column::Name);
        if let Some(org) = organization {
            q = q.filter(shopping_list::Column::Organization.eq(org));
        }
        let rows = q
            .all(&self.db)
            .await
            .map_err(|e| io(e, "list_shopping_lists"))?;
        rows.into_iter().map(shopping_list_to_api).collect()
    }

    async fn get_shopping_list(
        &self,
        id: Uuid,
    ) -> Result<Option<ShoppingListWithItems>, VaultError> {
        let Some(model) = shopping_list::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_shopping_list"))?
        else {
            return Ok(None);
        };
        let items = shopping_list::ItemEntity::find()
            .filter(shopping_list::ItemColumn::ListId.eq(id))
            .order_by_asc(shopping_list::ItemColumn::Sequence)
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load items"))?;
        let item_apis = items
            .into_iter()
            .map(item_to_api)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Some(ShoppingListWithItems {
            list: shopping_list_to_api(model)?,
            items_json: serde_json::to_string(&item_apis).map_err(|e| io(e, "serialize items"))?,
        }))
    }

    async fn create_shopping_list(
        &self,
        name: String,
        organization: Option<String>,
    ) -> Result<ShoppingListApi, VaultError> {
        if name.trim().is_empty() {
            return Err(VaultError::ParseError(
                "shopping list name is empty".to_string(),
            ));
        }
        let now = Utc::now();
        let active = shopping_list::ActiveModel {
            id: Set(Uuid::new_v4()),
            name: Set(name),
            organization: Set(organization),
            completed_at: Set(None),
            created_by: Set(None),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert shopping_list"))?;
        shopping_list_to_api(saved)
    }

    async fn generate_from_meal_plan(
        &self,
        request: GenerateShoppingListRequest,
    ) -> Result<ShoppingListWithItems, VaultError> {
        // Fetch meal_plan rows in range that have a recipe_id.
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

        // Sequence continues past existing items.
        let mut next_seq = shopping_list::ItemEntity::find()
            .filter(shopping_list::ItemColumn::ListId.eq(request.list_id))
            .order_by_desc(shopping_list::ItemColumn::Sequence)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "next item seq"))?
            .map(|r| r.sequence + 1)
            .unwrap_or(1);

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

        self.get_shopping_list(request.list_id)
            .await?
            .ok_or_else(|| VaultError::NotFound(format!("shopping_list:{}", request.list_id)))
    }

    async fn check_item(&self, item_id: Uuid, checked: bool) -> Result<(), VaultError> {
        let model = shopping_list::ItemEntity::find_by_id(item_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load item"))?
            .ok_or_else(|| VaultError::NotFound(format!("shopping_list_item:{item_id}")))?;
        let mut active: shopping_list::ItemActiveModel = model.into();
        active.checked = Set(checked);
        active.updated_at = Set(Utc::now());
        active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "check item"))?;
        Ok(())
    }

    async fn add_shopping_list_item(
        &self,
        request: AddShoppingItemRequest,
    ) -> Result<(), VaultError> {
        if request.food.trim().is_empty() {
            return Err(VaultError::ParseError("food is empty".to_string()));
        }
        let next_seq = shopping_list::ItemEntity::find()
            .filter(shopping_list::ItemColumn::ListId.eq(request.list_id))
            .order_by_desc(shopping_list::ItemColumn::Sequence)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "next item seq"))?
            .map(|r| r.sequence + 1)
            .unwrap_or(1);
        let now = Utc::now();
        let active = shopping_list::ItemActiveModel {
            id: Set(Uuid::new_v4()),
            list_id: Set(request.list_id),
            sequence: Set(next_seq),
            quantity: Set(request.quantity),
            unit: Set(request.unit),
            food: Set(request.food),
            note: Set(request.note),
            recipe_id: Set(None),
            meal_plan_id: Set(None),
            checked: Set(false),
            label: Set(request.label),
            created_at: Set(now),
            updated_at: Set(now),
        };
        active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert shopping_list_item"))?;
        Ok(())
    }
}
