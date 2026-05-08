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
use crate::cooking_session::{self, CookingSessionApi, CookingSessionStatus, StepState};
use crate::food_log;
use crate::meal_plan::{self, MealPlanEntryApi, MealType};
use crate::nutrition::{
    AggregatedNutrition, IngredientNutritionInput, NutritionFacts, aggregate_recipe_nutrition,
};
use crate::property::JsonObject;
use crate::recipe::{self, RecipeApi, RecipeIngredientSpec, RecipeStepSpec};
use crate::recipe_ingredient::{self, RecipeIngredientApi};
use crate::recipe_step::{self, RecipeStepApi};
use crate::service::{
    AddShoppingItemRequest, AggregatedNutritionView, CompleteCookingSessionRequest,
    CookbookWithRecipes, CookingService, CookingSessionView, CreateRecipeRequest,
    CreateSubstitutionRequest, GenerateShoppingListRequest, ImportRecipeRequest,
    IngredientSuggestion, MarkIngredientGatheredRequest, MarkMealPlanCookedRequest,
    MealPlanRangeRequest, NavigateStepRequest, RankedSubstitution, RecipeImportPreview,
    RecipePatch, RecipeWithDetails, ScaledRecipeView, SetMealPlanEntryRequest,
    ShoppingListWithItems, StartCookingSessionRequest, StepTimerActionRequest,
    SubstitutionSuggestionsView, SuggestSubstitutionsRequest, VaultError,
};
use crate::shopping_list::{self, ShoppingListApi, ShoppingListItemApi};
use crate::substitution::{self, SubstitutionApi};

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

fn session_to_api(model: cooking_session::Model) -> Result<CookingSessionApi, VaultError> {
    convert_model::<cooking_session::Model, CookingSessionApi>(model)
}

/// Materialize a [`CookingSessionView`] from a session row, by reading
/// the snapshotted JSON state plus the recipe's ingredients/steps and
/// scaling ingredients to the session's `scaled_servings`.
async fn build_session_view(
    db: &DatabaseConnection,
    session: cooking_session::Model,
) -> Result<CookingSessionView, VaultError> {
    let recipe = recipe::Entity::find_by_id(session.recipe_id)
        .one(db)
        .await
        .map_err(|e| io(e, "load recipe for session view"))?;
    let ingredients = recipe_ingredient::Entity::find()
        .filter(recipe_ingredient::Column::RecipeId.eq(session.recipe_id))
        .order_by_asc(recipe_ingredient::Column::Sequence)
        .all(db)
        .await
        .map_err(|e| io(e, "load ingredients for session view"))?;
    let steps = recipe_step::Entity::find()
        .filter(recipe_step::Column::RecipeId.eq(session.recipe_id))
        .order_by_asc(recipe_step::Column::Sequence)
        .all(db)
        .await
        .map_err(|e| io(e, "load steps for session view"))?;

    let ingredient_apis: Vec<RecipeIngredientApi> = ingredients
        .into_iter()
        .map(ingredient_to_api)
        .collect::<Result<Vec<_>, _>>()?;
    let step_apis: Vec<RecipeStepApi> = steps
        .into_iter()
        .map(step_to_api)
        .collect::<Result<Vec<_>, _>>()?;

    let source_servings = recipe.as_ref().and_then(|r| r.servings);
    let target_servings = session.scaled_servings.or(source_servings).unwrap_or(1);
    let scaled =
        crate::recipe::scale_ingredients(&ingredient_apis, source_servings, target_servings);

    let mise_state = cooking_session::mise_en_place_from_json(&session.mise_en_place_state);
    let step_states = cooking_session::step_states_from_json(&session.step_states);
    let ungathered_count: u32 = mise_state.iter().filter(|b| !**b).count() as u32;

    let scaled_ingredients_json = serde_json::to_string(&scaled.ingredients)
        .map_err(|e| io(e, "serialize scaled ingredients"))?;
    let steps_json = serde_json::to_string(&step_apis).map_err(|e| io(e, "serialize steps"))?;
    let step_states_json =
        serde_json::to_string(&step_states).map_err(|e| io(e, "serialize step_states"))?;
    let mise_en_place_json =
        serde_json::to_string(&mise_state).map_err(|e| io(e, "serialize mise"))?;

    Ok(CookingSessionView {
        session: session_to_api(session)?,
        scaled_ingredients_json,
        steps_json,
        step_states_json,
        mise_en_place_json,
        ungathered_count,
    })
}

/// Persist updated mise/step JSON columns + `current_step_index` and
/// return the refreshed model.
async fn save_session_state(
    db: &DatabaseConnection,
    mut session: cooking_session::Model,
    mise: Option<&[bool]>,
    steps: Option<&[StepState]>,
    new_index: Option<i32>,
) -> Result<cooking_session::Model, VaultError> {
    if let Some(m) = mise {
        session.mise_en_place_state = cooking_session::mise_en_place_to_json(m);
    }
    if let Some(s) = steps {
        session.step_states = cooking_session::step_states_to_json(s);
    }
    if let Some(idx) = new_index {
        session.current_step_index = idx;
    }
    let mut active: cooking_session::ActiveModel = session.clone().into();
    active.mise_en_place_state = Set(session.mise_en_place_state.clone());
    active.step_states = Set(session.step_states.clone());
    active.current_step_index = Set(session.current_step_index);
    active.updated_at = Set(Utc::now());
    let updated = active
        .update(db)
        .await
        .map_err(|e| io(e, "update cooking session state"))?;
    Ok(updated)
}

fn ingredient_active_for(
    recipe_id: Uuid,
    spec: &RecipeIngredientSpec,
    sequence: u32,
    food_id: Option<Uuid>,
    now: chrono::DateTime<Utc>,
) -> recipe_ingredient::ActiveModel {
    recipe_ingredient::ActiveModel {
        id: Set(Uuid::new_v4()),
        recipe_id: Set(recipe_id),
        sequence: Set(spec.sequence.unwrap_or(sequence)),
        quantity: Set(spec.quantity),
        unit: Set(spec.unit.clone()),
        food: Set(spec.food.clone()),
        food_id: Set(food_id),
        note: Set(spec.note.clone()),
        is_section: Set(spec.is_section.unwrap_or(false)),
        created_at: Set(now),
        updated_at: Set(now),
    }
}

/// Serialize-friendly mirror of [`AggregatedNutrition`] for the
/// `nutrition_summary` JSON column. Mirrors the fields callers care
/// about without leaking the entire facet/serde graph.
#[derive(serde::Serialize)]
struct SerializedAggregated<'a> {
    total: &'a NutritionFacts,
    per_serving: Option<&'a NutritionFacts>,
    warnings: &'a [String],
}

impl<'a> From<&'a AggregatedNutrition> for SerializedAggregated<'a> {
    fn from(value: &'a AggregatedNutrition) -> Self {
        Self {
            total: &value.total,
            per_serving: value.per_serving.as_ref(),
            warnings: &value.warnings,
        }
    }
}

/// Compute nutrition aggregate for a recipe by walking its ingredients
/// and resolving each `food_id` → `Food.nutrition_per_100g`.
pub(crate) async fn compute_recipe_nutrition<C: sea_orm::ConnectionTrait>(
    db: &C,
    recipe_id: Uuid,
) -> Result<AggregatedNutrition, VaultError> {
    let recipe = recipe::Entity::find_by_id(recipe_id)
        .one(db)
        .await
        .map_err(|e| io(e, "load recipe for aggregation"))?
        .ok_or_else(|| VaultError::NotFound(format!("recipe:{recipe_id}")))?;
    let ingredients = recipe_ingredient::Entity::find()
        .filter(recipe_ingredient::Column::RecipeId.eq(recipe_id))
        .order_by_asc(recipe_ingredient::Column::Sequence)
        .all(db)
        .await
        .map_err(|e| io(e, "load ingredients for aggregation"))?;
    let mut inputs: Vec<IngredientNutritionInput> = Vec::with_capacity(ingredients.len());
    for ing in ingredients {
        if ing.is_section {
            continue;
        }
        let nutrition = match ing.food_id {
            Some(fid) => crate::food::Entity::find_by_id(fid)
                .one(db)
                .await
                .map_err(|e| io(e, "load food for aggregation"))?
                .map(|f| NutritionFacts::from_json_object(&f.nutrition_per_100g)),
            None => None,
        };
        inputs.push(IngredientNutritionInput {
            food_id: ing.food_id,
            food_name: ing.food.clone(),
            quantity: ing.quantity,
            unit: ing.unit.clone(),
            nutrition_per_100g: nutrition,
        });
    }
    Ok(aggregate_recipe_nutrition(recipe.servings, &inputs))
}

/// Resolve `food_id` for an ingredient via name-match against the
/// canonical `Food` catalog. Returns `None` on miss — recipe data
/// stays valid even when no matching Food row exists yet.
async fn resolve_food_id<C: sea_orm::ConnectionTrait>(
    db: &C,
    organization: Option<&str>,
    spec: &RecipeIngredientSpec,
) -> Result<Option<Uuid>, VaultError> {
    if spec.is_section.unwrap_or(false) {
        return Ok(None);
    }
    let needle = spec.food.trim();
    if needle.is_empty() {
        return Ok(None);
    }
    let hit = crate::food::find_food_by_name(db, organization, needle)
        .await
        .map_err(|e| io(e, "find_food_by_name"))?;
    Ok(hit.map(|f| f.id))
}

/// Serde-friendly mirror of [`CreateRecipeRequest`] used to expose the
/// importer's draft as JSON in [`RecipeImportPreview`]. The on-the-wire
/// type itself is `facet::Facet` only (Vox transport), so we project it
/// through this struct when we need a stable JSON shape.
#[derive(serde::Serialize)]
struct DraftView<'a> {
    name: &'a str,
    description: Option<&'a str>,
    organization: Option<&'a str>,
    prep_time_minutes: Option<u32>,
    cook_time_minutes: Option<u32>,
    servings: Option<u32>,
    source_url: Option<&'a str>,
    created_by: Option<&'a str>,
    image_url: Option<&'a str>,
    yield_label: Option<&'a str>,
    properties_json: Option<&'a str>,
    ingredients_json: &'a str,
    steps_json: &'a str,
}

fn serialize_draft(req: &CreateRecipeRequest) -> DraftView<'_> {
    DraftView {
        name: req.name.as_str(),
        description: req.description.as_deref(),
        organization: req.organization.as_deref(),
        prep_time_minutes: req.prep_time_minutes,
        cook_time_minutes: req.cook_time_minutes,
        servings: req.servings,
        source_url: req.source_url.as_deref(),
        created_by: req.created_by.as_deref(),
        image_url: req.image_url.as_deref(),
        yield_label: req.yield_label.as_deref(),
        properties_json: req.properties_json.as_deref(),
        ingredients_json: req.ingredients_json.as_str(),
        steps_json: req.steps_json.as_str(),
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
        let organization = request.organization.clone();
        let slug = unique_slug(&self.db, &base_slug, organization.as_deref()).await?;

        let now = Utc::now();
        let recipe_id = Uuid::new_v4();

        let total = match (request.prep_time_minutes, request.cook_time_minutes) {
            (Some(p), Some(c)) => Some(p + c),
            _ => None,
        };

        let properties = match request.properties_json.as_deref() {
            Some(s) if !s.trim().is_empty() => {
                let value: serde_json::Value =
                    serde_json::from_str(s).map_err(|e| parse(e, "properties_json"))?;
                if !value.is_object() {
                    return Err(parse(
                        "expected JSON object",
                        "properties_json must encode an object",
                    ));
                }
                JsonObject::from_value(value)
            }
            _ => JsonObject::default(),
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
            yield_label: Set(request.yield_label),
            source_url: Set(request.source_url),
            image_url: Set(request.image_url),
            rating: Set(None),
            last_made: Set(None),
            notes: Set(None),
            created_by: Set(request.created_by),
            properties: Set(properties),
            nutrition_summary: Set(JsonObject::default()),
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
            let food_id = resolve_food_id(&txn, organization.as_deref(), spec).await?;
            ingredient_active_for(recipe_id, spec, (idx + 1) as u32, food_id, now)
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
        let organization = model.organization.clone();
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
                let food_id = resolve_food_id(&txn, organization.as_deref(), spec).await?;
                ingredient_active_for(id, spec, (idx + 1) as u32, food_id, now)
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

    async fn import_recipe(
        &self,
        request: ImportRecipeRequest,
    ) -> Result<RecipeWithDetails, VaultError> {
        let importer = crate::recipe_import::RecipeImporter::new();
        let result = importer
            .import(&request.url)
            .await
            .map_err(|e| parse(e, "recipe import"))?;
        let mut draft = result.draft;
        draft.organization = request.organization;
        if draft.created_by.is_none() {
            draft.created_by = request.created_by;
        }
        if draft.source_url.is_none() {
            draft.source_url = Some(result.source_url);
        }
        self.create_recipe(draft).await
    }

    async fn preview_recipe_import(&self, url: String) -> Result<RecipeImportPreview, VaultError> {
        let importer = crate::recipe_import::RecipeImporter::new();
        let result = importer
            .import(&url)
            .await
            .map_err(|e| parse(e, "recipe import"))?;
        let draft_json = serde_json::to_string(&serialize_draft(&result.draft))
            .map_err(|e| io(e, "serialize draft"))?;
        let warnings_json =
            serde_json::to_string(&result.warnings).map_err(|e| io(e, "serialize warnings"))?;
        Ok(RecipeImportPreview {
            draft_json,
            source_url: result.source_url,
            strategy: result.strategy.as_str().to_string(),
            warnings_json,
        })
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

    async fn recompute_recipe_nutrition(
        &self,
        recipe_id: Uuid,
    ) -> Result<AggregatedNutritionView, VaultError> {
        let aggregated = compute_recipe_nutrition(&self.db, recipe_id).await?;
        let recipe = recipe::Entity::find_by_id(recipe_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load recipe (recompute)"))?
            .ok_or_else(|| VaultError::NotFound(format!("recipe:{recipe_id}")))?;
        let recipe_name = recipe.name.clone();

        let summary_value =
            serde_json::to_value(SerializedAggregated::from(&aggregated)).unwrap_or_default();
        let mut active: recipe::ActiveModel = recipe.into();
        active.nutrition_summary = Set(JsonObject::from_value(summary_value));
        active.updated_at = Set(Utc::now());
        active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "persist nutrition_summary"))?;

        let total_json = serde_json::to_string(&aggregated.total)
            .map_err(|e| io(e, "serialize nutrition total"))?;
        let per_serving_json = match aggregated.per_serving.as_ref() {
            Some(p) => Some(serde_json::to_string(p).map_err(|e| io(e, "serialize per_serving"))?),
            None => None,
        };
        Ok(AggregatedNutritionView {
            recipe_id,
            recipe_name,
            total_json,
            per_serving_json,
            warnings: aggregated.warnings,
        })
    }

    async fn mark_meal_plan_cooked(
        &self,
        request: MarkMealPlanCookedRequest,
    ) -> Result<Vec<Uuid>, VaultError> {
        let entry = meal_plan::Entity::find_by_id(request.meal_plan_entry_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load meal_plan_entry"))?
            .ok_or_else(|| {
                VaultError::NotFound(format!("meal_plan_entry:{}", request.meal_plan_entry_id))
            })?;
        let servings_consumed = request
            .servings_consumed
            .or(entry.servings_planned)
            .unwrap_or(1)
            .max(1);
        let now = Utc::now();
        let log_id = Uuid::new_v4();
        let mut active = <food_log::ActiveModel as sea_orm::ActiveModelTrait>::default();
        active.properties = Set(JsonObject::default());
        active.id = Set(log_id);
        active.date = Set(entry.date);
        active.meal_type = Set(entry.meal_type);
        active.organization = Set(entry.organization.clone());
        active.meal_plan_entry_id = Set(Some(entry.id));
        active.created_by = Set(request.created_by.or(entry.created_by.clone()));
        active.created_at = Set(now);
        active.updated_at = Set(now);
        if let Some(recipe_id) = entry.recipe_id {
            // Always recompute first so the snapshot is fresh.
            let aggregated = compute_recipe_nutrition(&self.db, recipe_id).await?;
            let recipe = recipe::Entity::find_by_id(recipe_id)
                .one(&self.db)
                .await
                .map_err(|e| io(e, "load recipe (mark cooked)"))?
                .ok_or_else(|| VaultError::NotFound(format!("recipe:{recipe_id}")))?;
            let per_serving_facts = aggregated
                .per_serving
                .clone()
                .unwrap_or_else(|| aggregated.total.clone());
            let factor = f64::from(servings_consumed);
            active.recipe_id = Set(Some(recipe_id));
            active.food_name = Set(recipe.name.clone());
            active.kcal = Set(per_serving_facts.kcal_per_100g.map(|v| v * factor));
            active.protein_g = Set(per_serving_facts.protein_g.map(|v| v * factor));
            active.carbs_g = Set(per_serving_facts.carbs_g.map(|v| v * factor));
            active.sugars_g = Set(per_serving_facts.sugars_g.map(|v| v * factor));
            active.fiber_g = Set(per_serving_facts.fiber_g.map(|v| v * factor));
            active.fat_g = Set(per_serving_facts.fat_g.map(|v| v * factor));
            active.saturated_fat_g = Set(per_serving_facts.saturated_fat_g.map(|v| v * factor));
            active.sodium_mg = Set(per_serving_facts.sodium_mg.map(|v| v * factor));
            // quantity_grams = 100g sentinel × servings (the recipe
            // doesn't carry a per-serving gram weight; we record
            // servings_consumed in notes for traceability).
            active.quantity_grams = Set(0.0);
            active.notes = Set(Some(format!("servings_consumed={servings_consumed}")));
        } else {
            active.recipe_id = Set(None);
            active.food_name = Set(entry
                .title
                .clone()
                .unwrap_or_else(|| "untitled meal".to_string()));
            active.quantity_grams = Set(0.0);
            // All macros stay None.
        }
        active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert food_log"))?;
        Ok(vec![log_id])
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

    // ── Cooking sessions ────────────────────────────────────────────

    async fn start_cooking_session(
        &self,
        request: StartCookingSessionRequest,
    ) -> Result<CookingSessionView, VaultError> {
        let recipe = recipe::Entity::find_by_id(request.recipe_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load recipe for start_cooking_session"))?
            .ok_or_else(|| VaultError::NotFound(format!("recipe:{}", request.recipe_id)))?;
        let ingredients = recipe_ingredient::Entity::find()
            .filter(recipe_ingredient::Column::RecipeId.eq(recipe.id))
            .order_by_asc(recipe_ingredient::Column::Sequence)
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load ingredients for start_cooking_session"))?;
        let steps = recipe_step::Entity::find()
            .filter(recipe_step::Column::RecipeId.eq(recipe.id))
            .order_by_asc(recipe_step::Column::Sequence)
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load steps for start_cooking_session"))?;

        let mise = vec![false; ingredients.len()];
        let step_states = vec![StepState::default(); steps.len()];
        let now = Utc::now();
        let session_id = Uuid::new_v4();
        let active = cooking_session::ActiveModel {
            id: Set(session_id),
            recipe_id: Set(recipe.id),
            recipe_name_snapshot: Set(recipe.name.clone()),
            status: Set(CookingSessionStatus::Active),
            current_step_index: Set(-1),
            scaled_servings: Set(request.scaled_servings.or(recipe.servings)),
            mise_en_place_state: Set(cooking_session::mise_en_place_to_json(&mise)),
            step_states: Set(cooking_session::step_states_to_json(&step_states)),
            notes: Set(None),
            started_at: Set(now),
            completed_at: Set(None),
            organization: Set(request.organization.or(recipe.organization.clone())),
            created_by: Set(request.created_by),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let _ = ingredients; // (counts already used)
        let model = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert cooking_session"))?;
        build_session_view(&self.db, model).await
    }

    async fn get_cooking_session(
        &self,
        id: Uuid,
    ) -> Result<Option<CookingSessionView>, VaultError> {
        let Some(model) = cooking_session::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_cooking_session"))?
        else {
            return Ok(None);
        };
        Ok(Some(build_session_view(&self.db, model).await?))
    }

    async fn list_active_cooking_sessions(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<CookingSessionApi>, VaultError> {
        let mut q = cooking_session::Entity::find()
            .filter(cooking_session::Column::Status.eq(CookingSessionStatus::Active))
            .order_by_desc(cooking_session::Column::StartedAt);
        if let Some(org) = organization {
            q = q.filter(cooking_session::Column::Organization.eq(org));
        }
        let rows = q
            .all(&self.db)
            .await
            .map_err(|e| io(e, "list_active_cooking_sessions"))?;
        rows.into_iter().map(session_to_api).collect()
    }

    async fn mark_ingredient_gathered(
        &self,
        request: MarkIngredientGatheredRequest,
    ) -> Result<CookingSessionView, VaultError> {
        let session = cooking_session::Entity::find_by_id(request.session_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load cooking_session"))?
            .ok_or_else(|| {
                VaultError::NotFound(format!("cooking_session:{}", request.session_id))
            })?;
        let mut mise = cooking_session::mise_en_place_from_json(&session.mise_en_place_state);
        let idx = request.ingredient_index as usize;
        if idx >= mise.len() {
            return Err(VaultError::ParseError(format!(
                "ingredient_index {idx} out of range (len={})",
                mise.len()
            )));
        }
        mise[idx] = request.gathered;
        let updated = save_session_state(&self.db, session, Some(&mise), None, None).await?;
        build_session_view(&self.db, updated).await
    }

    async fn navigate_step(
        &self,
        request: NavigateStepRequest,
    ) -> Result<CookingSessionView, VaultError> {
        let session = cooking_session::Entity::find_by_id(request.session_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load cooking_session"))?
            .ok_or_else(|| {
                VaultError::NotFound(format!("cooking_session:{}", request.session_id))
            })?;
        let step_states = cooking_session::step_states_from_json(&session.step_states);
        let max_index = if step_states.is_empty() {
            -1
        } else {
            (step_states.len() - 1) as i32
        };
        let current = session.current_step_index;
        let new_index = match request.direction.as_str() {
            "next" => (current + 1).min(max_index),
            "previous" | "prev" => (current - 1).max(-1),
            "jump" => {
                let target = request.jump_to.ok_or_else(|| {
                    VaultError::ParseError("jump direction requires jump_to".to_string())
                })?;
                if target < -1 || target > max_index {
                    return Err(VaultError::ParseError(format!(
                        "jump_to {target} out of range [-1, {max_index}]"
                    )));
                }
                target
            }
            other => {
                return Err(VaultError::ParseError(format!(
                    "unknown navigate direction: {other}"
                )));
            }
        };
        // Don't auto-start timers — explicit user action only.
        let updated = save_session_state(&self.db, session, None, None, Some(new_index)).await?;
        build_session_view(&self.db, updated).await
    }

    async fn step_timer_action(
        &self,
        request: StepTimerActionRequest,
    ) -> Result<CookingSessionView, VaultError> {
        let session = cooking_session::Entity::find_by_id(request.session_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load cooking_session"))?
            .ok_or_else(|| {
                VaultError::NotFound(format!("cooking_session:{}", request.session_id))
            })?;
        let mut step_states = cooking_session::step_states_from_json(&session.step_states);
        let step_index = request.step_index.unwrap_or(session.current_step_index);
        if step_index < 0 || (step_index as usize) >= step_states.len() {
            return Err(VaultError::ParseError(format!(
                "step_index {step_index} out of range (len={})",
                step_states.len()
            )));
        }
        let idx = step_index as usize;
        let now = Utc::now();
        let state = &mut step_states[idx];
        match request.action.as_str() {
            "start" => {
                state.started_at = Some(now);
                state.paused_at = None;
                state.completed_at = None;
            }
            "pause" => {
                if state.started_at.is_none() {
                    return Err(VaultError::ParseError(
                        "cannot pause a step that hasn't started".to_string(),
                    ));
                }
                if state.completed_at.is_some() {
                    return Err(VaultError::ParseError(
                        "cannot pause a completed step".to_string(),
                    ));
                }
                if state.paused_at.is_some() {
                    return Err(VaultError::ParseError("step is already paused".to_string()));
                }
                state.paused_at = Some(now);
            }
            "resume" => {
                let paused = state
                    .paused_at
                    .ok_or_else(|| VaultError::ParseError("step is not paused".to_string()))?;
                let elapsed_paused = (now - paused).num_seconds().max(0) as u32;
                state.pause_offset_seconds =
                    state.pause_offset_seconds.saturating_add(elapsed_paused);
                state.paused_at = None;
            }
            "complete" => {
                if state.started_at.is_none() {
                    state.started_at = Some(now);
                }
                // If currently paused, fold the paused interval into
                // pause_offset before stamping completion.
                if let Some(paused) = state.paused_at.take() {
                    let elapsed_paused = (now - paused).num_seconds().max(0) as u32;
                    state.pause_offset_seconds =
                        state.pause_offset_seconds.saturating_add(elapsed_paused);
                }
                state.completed_at = Some(now);
            }
            "reset" => {
                *state = StepState::default();
            }
            other => {
                return Err(VaultError::ParseError(format!(
                    "unknown timer action: {other}"
                )));
            }
        }
        let updated = save_session_state(&self.db, session, None, Some(&step_states), None).await?;
        build_session_view(&self.db, updated).await
    }

    async fn complete_cooking_session(
        &self,
        request: CompleteCookingSessionRequest,
    ) -> Result<CookingSessionView, VaultError> {
        let session = cooking_session::Entity::find_by_id(request.session_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load cooking_session"))?
            .ok_or_else(|| {
                VaultError::NotFound(format!("cooking_session:{}", request.session_id))
            })?;
        let now = Utc::now();
        let log_date = request
            .log_date
            .unwrap_or_else(|| chrono::Local::now().date_naive());

        // Update Recipe.last_made.
        if let Some(recipe) = recipe::Entity::find_by_id(session.recipe_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load recipe for complete_cooking_session"))?
        {
            let mut active: recipe::ActiveModel = recipe.into();
            active.last_made = Set(Some(log_date));
            active.updated_at = Set(now);
            let _ = active
                .update(&self.db)
                .await
                .map_err(|e| io(e, "update recipe.last_made"))?;
        }

        // Optional auto-log: insert a FoodLog row using the recipe's
        // cached nutrition_summary × servings_eaten.
        if request.log_meal {
            let aggregated = compute_recipe_nutrition(&self.db, session.recipe_id).await?;
            let per_serving = aggregated
                .per_serving
                .clone()
                .unwrap_or_else(|| aggregated.total.clone());
            let servings = request
                .servings_eaten
                .or(session.scaled_servings)
                .unwrap_or(1)
                .max(1);
            let factor = f64::from(servings);
            let meal_type = request
                .meal_type
                .as_deref()
                .and_then(MealType::parse)
                .unwrap_or(MealType::Other);
            let mut active = <food_log::ActiveModel as sea_orm::ActiveModelTrait>::default();
            active.id = Set(Uuid::new_v4());
            active.date = Set(log_date);
            active.meal_type = Set(meal_type);
            active.organization = Set(session.organization.clone());
            active.recipe_id = Set(Some(session.recipe_id));
            active.food_name = Set(session.recipe_name_snapshot.clone());
            active.quantity_grams = Set(0.0);
            active.kcal = Set(per_serving.kcal_per_100g.map(|v| v * factor));
            active.protein_g = Set(per_serving.protein_g.map(|v| v * factor));
            active.carbs_g = Set(per_serving.carbs_g.map(|v| v * factor));
            active.sugars_g = Set(per_serving.sugars_g.map(|v| v * factor));
            active.fiber_g = Set(per_serving.fiber_g.map(|v| v * factor));
            active.fat_g = Set(per_serving.fat_g.map(|v| v * factor));
            active.saturated_fat_g = Set(per_serving.saturated_fat_g.map(|v| v * factor));
            active.sodium_mg = Set(per_serving.sodium_mg.map(|v| v * factor));
            active.notes = Set(Some(format!(
                "cooking_session={}; servings_eaten={servings}",
                session.id
            )));
            active.created_by = Set(request.actor.or(session.created_by.clone()));
            active.properties = Set(JsonObject::default());
            active.created_at = Set(now);
            active.updated_at = Set(now);
            active
                .insert(&self.db)
                .await
                .map_err(|e| io(e, "insert food_log for cooking session"))?;
        }

        // Stamp the session as completed.
        let mut active: cooking_session::ActiveModel = session.clone().into();
        active.status = Set(CookingSessionStatus::Completed);
        active.completed_at = Set(Some(now));
        active.updated_at = Set(now);
        let updated = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "complete cooking_session"))?;
        build_session_view(&self.db, updated).await
    }

    async fn abandon_cooking_session(&self, id: Uuid) -> Result<CookingSessionView, VaultError> {
        let session = cooking_session::Entity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load cooking_session"))?
            .ok_or_else(|| VaultError::NotFound(format!("cooking_session:{id}")))?;
        let now = Utc::now();
        let mut active: cooking_session::ActiveModel = session.into();
        active.status = Set(CookingSessionStatus::Abandoned);
        active.completed_at = Set(Some(now));
        active.updated_at = Set(now);
        let updated = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "abandon cooking_session"))?;
        build_session_view(&self.db, updated).await
    }

    async fn scale_recipe(
        &self,
        recipe_id: Uuid,
        target_servings: u32,
    ) -> Result<ScaledRecipeView, VaultError> {
        let recipe = recipe::Entity::find_by_id(recipe_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load recipe for scale"))?
            .ok_or_else(|| VaultError::NotFound(format!("recipe:{recipe_id}")))?;
        let ingredients = recipe_ingredient::Entity::find()
            .filter(recipe_ingredient::Column::RecipeId.eq(recipe_id))
            .order_by_asc(recipe_ingredient::Column::Sequence)
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load ingredients for scale"))?;
        let ingredient_apis: Vec<RecipeIngredientApi> = ingredients
            .into_iter()
            .map(ingredient_to_api)
            .collect::<Result<Vec<_>, _>>()?;
        let result =
            crate::recipe::scale_ingredients(&ingredient_apis, recipe.servings, target_servings);
        let scaled_ingredients_json = serde_json::to_string(&result.ingredients)
            .map_err(|e| io(e, "serialize scaled ingredients"))?;
        Ok(ScaledRecipeView {
            recipe_id,
            recipe_name: recipe.name.clone(),
            source_servings: result.source_servings,
            target_servings: result.target_servings,
            multiplier: result.multiplier,
            scaled_ingredients_json,
            warnings: result.warnings,
        })
    }

    // ── Substitutions ───────────────────────────────────────────────

    async fn suggest_substitutions(
        &self,
        request: SuggestSubstitutionsRequest,
    ) -> Result<SubstitutionSuggestionsView, VaultError> {
        let recipe = recipe::Entity::find_by_id(request.recipe_id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load recipe for suggest_substitutions"))?
            .ok_or_else(|| VaultError::NotFound(format!("recipe:{}", request.recipe_id)))?;
        let ingredients = recipe_ingredient::Entity::find()
            .filter(recipe_ingredient::Column::RecipeId.eq(recipe.id))
            .order_by_asc(recipe_ingredient::Column::Sequence)
            .all(&self.db)
            .await
            .map_err(|e| io(e, "load ingredients for suggest_substitutions"))?;

        let limit = request.limit_per_ingredient.unwrap_or(5).max(1) as usize;
        let mut warnings: Vec<String> = Vec::new();
        let mut suggestions: Vec<IngredientSuggestion> = Vec::new();
        let dietary_filter: Vec<String> = request
            .dietary_filter
            .iter()
            .map(|s| s.trim().to_ascii_lowercase())
            .filter(|s| !s.is_empty())
            .collect();
        let has_dietary = !dietary_filter.is_empty();
        let has_missing = !request.missing_food_ids.is_empty();
        if !has_dietary && !has_missing {
            return Ok(SubstitutionSuggestionsView {
                recipe_id: recipe.id,
                recipe_name: recipe.name,
                suggestions_json: "[]".to_string(),
                warnings,
            });
        }

        for ing in &ingredients {
            if ing.is_section {
                continue;
            }
            let mut reasons: Vec<String> = Vec::new();
            let food_row = match ing.food_id {
                Some(fid) => crate::food::Entity::find_by_id(fid)
                    .one(&self.db)
                    .await
                    .map_err(|e| io(e, "load food for ingredient"))?,
                None => None,
            };

            if has_missing
                && ing
                    .food_id
                    .is_some_and(|f| request.missing_food_ids.contains(&f))
            {
                reasons.push("missing".to_string());
            }
            if has_dietary {
                if let Some(food) = &food_row {
                    let tags = dietary_tags_from_food(food);
                    for filter_tag in &dietary_filter {
                        if !tags.iter().any(|t| t == filter_tag) {
                            reasons.push(format!("dietary:{filter_tag}"));
                        }
                    }
                } else if ing.food_id.is_none() {
                    warnings.push(format!(
                        "can't check dietary tags for unlinked ingredient '{}'",
                        ing.food
                    ));
                    reasons.push("unlinked".to_string());
                } else {
                    // food_id set but row not found — treat similar to unlinked.
                    warnings.push(format!(
                        "linked food row missing for ingredient '{}'",
                        ing.food
                    ));
                    reasons.push("unlinked".to_string());
                }
            }

            if reasons.is_empty() {
                continue;
            }

            let mut ranked: Vec<RankedSubstitution> = Vec::new();
            if let Some(fid) = ing.food_id {
                // Direct hits.
                let direct =
                    load_substitutions_by_from(&self.db, fid, request.organization.as_deref())
                        .await?;
                for sub in direct {
                    if let Some(rs) = build_ranked(&self.db, &sub, false, &dietary_filter).await? {
                        ranked.push(rs);
                    }
                }
                // Inverse hits (only bidirectional rules apply).
                let inverse =
                    load_substitutions_by_to(&self.db, fid, request.organization.as_deref())
                        .await?;
                for sub in inverse {
                    if !sub.bidirectional {
                        continue;
                    }
                    if let Some(rs) = build_ranked(&self.db, &sub, true, &dietary_filter).await? {
                        ranked.push(rs);
                    }
                }
            }

            // Re-score with original quantity scaling.
            for rs in &mut ranked {
                rs.suggested_quantity = ing.quantity.map(|q| q * rs.ratio);
                rs.suggested_unit = ing.unit.clone();
            }

            // Boost if applies_when.dietary exactly matches the filter.
            if has_dietary {
                for rs in &mut ranked {
                    let summary_tags = applies_when_dietary_tags(&rs.applies_when_summary);
                    if !summary_tags.is_empty()
                        && summary_tags.iter().all(|t| dietary_filter.contains(t))
                        && summary_tags.len() == dietary_filter.len()
                    {
                        rs.score = (rs.score + 0.2).min(1.5);
                    }
                }
            }

            // Sort & truncate.
            ranked.sort_by(|a, b| {
                b.score
                    .partial_cmp(&a.score)
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            ranked.truncate(limit);

            suggestions.push(IngredientSuggestion {
                ingredient_food_id: ing.food_id,
                ingredient_food_name: ing.food.clone(),
                original_quantity: ing.quantity,
                original_unit: ing.unit.clone(),
                suggestions: ranked,
                reasons,
            });
        }

        let suggestions_json =
            serde_json::to_string(&suggestions).map_err(|e| io(e, "serialize suggestions"))?;
        Ok(SubstitutionSuggestionsView {
            recipe_id: recipe.id,
            recipe_name: recipe.name,
            suggestions_json,
            warnings,
        })
    }

    async fn create_substitution(
        &self,
        request: CreateSubstitutionRequest,
    ) -> Result<SubstitutionApi, VaultError> {
        if !request.ratio.is_finite() || request.ratio <= 0.0 {
            return Err(VaultError::ParseError(format!(
                "ratio must be a positive finite number (got {})",
                request.ratio
            )));
        }
        let confidence = request.confidence.clamp(0.0, 1.0);
        let applies_when = match request.applies_when_json.as_deref() {
            Some(s) if !s.trim().is_empty() => {
                let value: serde_json::Value =
                    serde_json::from_str(s).map_err(|e| parse(e, "applies_when_json"))?;
                if !value.is_object() {
                    return Err(parse(
                        "expected JSON object",
                        "applies_when_json must encode an object",
                    ));
                }
                JsonObject::from_value(value)
            }
            _ => JsonObject::default(),
        };
        let now = Utc::now();
        let active = substitution::ActiveModel {
            id: Set(Uuid::new_v4()),
            from_food_id: Set(request.from_food_id),
            to_food_id: Set(request.to_food_id),
            ratio: Set(request.ratio),
            conversion_note: Set(request.conversion_note),
            applies_when: Set(applies_when),
            confidence: Set(confidence),
            bidirectional: Set(request.bidirectional),
            organization: Set(request.organization),
            created_by: Set(request.created_by),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let model = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert substitution"))?;
        substitution_to_api(model)
    }

    async fn list_substitutions(
        &self,
        organization: Option<String>,
    ) -> Result<Vec<SubstitutionApi>, VaultError> {
        let mut q = substitution::Entity::find().order_by_asc(substitution::Column::CreatedAt);
        if let Some(org) = organization {
            q = q.filter(substitution::Column::Organization.eq(org));
        }
        let rows = q
            .all(&self.db)
            .await
            .map_err(|e| io(e, "list_substitutions"))?;
        rows.into_iter().map(substitution_to_api).collect()
    }

    async fn delete_substitution(&self, id: Uuid) -> Result<(), VaultError> {
        substitution::Entity::delete_by_id(id)
            .exec(&self.db)
            .await
            .map_err(|e| io(e, "delete_substitution"))?;
        Ok(())
    }
}

fn substitution_to_api(model: substitution::Model) -> Result<SubstitutionApi, VaultError> {
    convert_model::<substitution::Model, SubstitutionApi>(model)
}

/// Pull `dietary_tags` from a Food's `properties.dietary_tags` JSON
/// array, lower-cased. Missing/non-array → empty.
fn dietary_tags_from_food(food: &crate::food::Model) -> Vec<String> {
    food.properties
        .as_value()
        .get("dietary_tags")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.trim().to_ascii_lowercase()))
                .filter(|s| !s.is_empty())
                .collect()
        })
        .unwrap_or_default()
}

/// Pull `dietary` array from an `applies_when` JSON object.
fn applies_when_dietary(value: &serde_json::Value) -> Vec<String> {
    value
        .get("dietary")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(|s| s.trim().to_ascii_lowercase()))
                .filter(|s| !s.is_empty())
                .collect()
        })
        .unwrap_or_default()
}

/// Decode the `applies_when_summary` ("vegan, gluten_free") back to a
/// tag list. Used internally for the dietary-perfect-fit boost.
fn applies_when_dietary_tags(summary: &str) -> Vec<String> {
    summary
        .split(',')
        .map(|s| s.trim().to_ascii_lowercase())
        .filter(|s| !s.is_empty())
        .collect()
}

async fn load_substitutions_by_from<C: sea_orm::ConnectionTrait>(
    db: &C,
    from_food_id: Uuid,
    organization: Option<&str>,
) -> Result<Vec<substitution::Model>, VaultError> {
    let mut q =
        substitution::Entity::find().filter(substitution::Column::FromFoodId.eq(from_food_id));
    q = match organization {
        Some(org) => q.filter(substitution::Column::Organization.eq(org)),
        None => q.filter(substitution::Column::Organization.is_null()),
    };
    q.all(db)
        .await
        .map_err(|e| io(e, "load substitutions by from_food_id"))
}

async fn load_substitutions_by_to<C: sea_orm::ConnectionTrait>(
    db: &C,
    to_food_id: Uuid,
    organization: Option<&str>,
) -> Result<Vec<substitution::Model>, VaultError> {
    let mut q = substitution::Entity::find().filter(substitution::Column::ToFoodId.eq(to_food_id));
    q = match organization {
        Some(org) => q.filter(substitution::Column::Organization.eq(org)),
        None => q.filter(substitution::Column::Organization.is_null()),
    };
    q.all(db)
        .await
        .map_err(|e| io(e, "load substitutions by to_food_id"))
}

/// Apply the dietary-applies_when filter and compute the ranking score.
/// Returns `None` when the substitution is filtered out.
async fn build_ranked<C: sea_orm::ConnectionTrait>(
    db: &C,
    sub: &substitution::Model,
    is_inverse: bool,
    dietary_filter: &[String],
) -> Result<Option<RankedSubstitution>, VaultError> {
    let aw_dietary = applies_when_dietary(sub.applies_when.as_value());
    if !dietary_filter.is_empty() && !aw_dietary.is_empty() {
        // applies_when.dietary must be a subset of the filter.
        let all_in = aw_dietary.iter().all(|t| dietary_filter.contains(t));
        if !all_in {
            return Ok(None);
        }
    }

    let (from_id, to_id, ratio) = if is_inverse {
        let inv = if sub.ratio.abs() < f64::EPSILON {
            1.0
        } else {
            1.0 / sub.ratio
        };
        (sub.to_food_id, sub.from_food_id, inv)
    } else {
        (sub.from_food_id, sub.to_food_id, sub.ratio)
    };

    let to_food_name = crate::food::Entity::find_by_id(to_id)
        .one(db)
        .await
        .map_err(|e| io(e, "load to_food for substitution"))?
        .map(|f| f.name)
        .unwrap_or_else(|| to_id.to_string());

    let mut score = sub.confidence as f64;
    if is_inverse {
        score -= 0.1;
    }
    let score = score.clamp(0.0, 1.5);

    Ok(Some(RankedSubstitution {
        substitution_id: sub.id,
        from_food_id: from_id,
        to_food_id: to_id,
        to_food_name,
        suggested_quantity: None,
        suggested_unit: None,
        ratio,
        conversion_note: sub.conversion_note.clone(),
        confidence: sub.confidence,
        score,
        applies_when_summary: aw_dietary.join(", "),
        is_inverse,
    }))
}
