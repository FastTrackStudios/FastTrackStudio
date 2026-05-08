//! `task cook` — cooking / meal-prep workflow commands.
//!
//! Mealie-style surface: recipes (CRUD + rate + made), cookbooks
//! (collections + add/remove), meal plans (per-day per-meal-type slot),
//! shopping lists (manual or generated from a meal-plan range).

use chrono::NaiveDate;
use clap::{Args, Subcommand};
use task_core::food::FoodApi;
use task_core::food_product::FoodProductApi;
use task_core::recipe::{RecipeApi, RecipeIngredientSpec, RecipeStepSpec};
use task_core::service::{
    AddShoppingItemRequest, AddToPantryRequest, BarcodeLookupRequest,
    CompleteCookingSessionRequest, ConsumeFromPantryRequest, CookbookWithRecipes,
    CookingServiceClient, CookingSessionView, CreateFoodProductRequest, CreateFoodRequest,
    CreateRecipeRequest, CreateSubstitutionRequest, FoodServiceClient,
    GenerateShoppingListFromMissingRequest, GenerateShoppingListRequest, ImportRecipeRequest,
    IngredientSuggestion, MarkIngredientGatheredRequest, MealPlanRangeRequest, NavigateStepRequest,
    PantryItemPatch, PantryListRequest, PantryServiceClient, RecipeWithDetails,
    SetMealPlanEntryRequest, ShoppingListWithItems, StartCookingSessionRequest,
    StepTimerActionRequest, SuggestSubstitutionsRequest,
};
use uuid::Uuid;

use crate::shared::RemoteVoxConfig;

#[derive(Subcommand)]
pub(crate) enum CookCommands {
    /// Recipe CRUD + rate + mark-made.
    Recipe {
        #[command(subcommand)]
        command: RecipeCommands,
    },
    /// Cookbook collections.
    Cookbook {
        #[command(subcommand)]
        command: CookbookCommands,
    },
    /// Meal-plan slots.
    Plan {
        #[command(subcommand)]
        command: PlanCommands,
    },
    /// Shopping lists.
    Shop {
        #[command(subcommand)]
        command: ShopCommands,
    },
    /// Canonical ingredient catalog (Food).
    Food {
        #[command(subcommand)]
        command: FoodCommands,
    },
    /// Branded products (FoodProduct) — barcode-keyed.
    Product {
        #[command(subcommand)]
        command: ProductCommands,
    },
    /// Pantry stock + low-stock + expiring queries.
    Pantry {
        #[command(subcommand)]
        command: PantryCommands,
    },
    /// Daily food log (nutrition diary).
    Log {
        #[command(subcommand)]
        command: LogCommands,
    },
    /// Recipe + daily / weekly nutrition aggregates.
    Nutrition {
        #[command(subcommand)]
        command: NutritionCommands,
    },
    /// Interactive cooking sessions — mise en place, step navigation,
    /// timers, recipe scaling.
    Session {
        #[command(subcommand)]
        command: SessionCommands,
    },
    /// Substitution catalog management (CRUD over swap rules).
    Substitution {
        #[command(subcommand)]
        command: SubstitutionCommands,
    },
}

// ── Substitutions ───────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum SubstitutionCommands {
    List {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Create {
        #[arg(long)]
        from: String,
        #[arg(long)]
        to: String,
        #[arg(long)]
        ratio: f64,
        #[arg(long)]
        note: Option<String>,
        #[arg(long, default_value_t = 0.8)]
        confidence: f32,
        #[arg(long)]
        bidirectional: bool,
        #[arg(long = "diet", value_name = "TAG")]
        diet: Vec<String>,
        #[arg(long = "context", value_name = "CTX")]
        context: Vec<String>,
        #[arg(long)]
        organization: Option<String>,
    },
    Delete {
        id: String,
    },
}

// ── Session ─────────────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum SessionCommands {
    /// Begin an interactive cooking session for a recipe.
    Start {
        recipe: String,
        #[arg(long)]
        servings: Option<u32>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long = "created-by")]
        created_by: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Show a session view by id.
    Show {
        session: String,
        #[arg(long)]
        json: bool,
    },
    /// List all currently-active sessions.
    ListActive {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Move the current step cursor (next / previous / jump-to).
    Step {
        session: String,
        #[arg(long, conflicts_with_all = ["previous", "jump_to"])]
        next: bool,
        #[arg(long, conflicts_with_all = ["next", "jump_to"])]
        previous: bool,
        #[arg(long = "jump-to")]
        jump_to: Option<i32>,
        #[arg(long)]
        json: bool,
    },
    /// Per-step timer action: start / pause / resume / complete / reset.
    Timer {
        session: String,
        #[arg(long)]
        action: String,
        #[arg(long = "step")]
        step: Option<i32>,
        #[arg(long)]
        json: bool,
    },
    /// Toggle a mise-en-place ingredient checkbox.
    Ingredient {
        session: String,
        #[arg(long)]
        index: u32,
        #[arg(long, conflicts_with = "uncheck")]
        check: bool,
        #[arg(long, conflicts_with = "check")]
        uncheck: bool,
        #[arg(long)]
        json: bool,
    },
    /// Mark the session complete; optionally auto-log the meal.
    Complete {
        session: String,
        #[arg(long = "log-meal")]
        log_meal: bool,
        #[arg(long)]
        servings: Option<u32>,
        #[arg(long = "meal-type")]
        meal_type: Option<String>,
        #[arg(long)]
        date: Option<String>,
        #[arg(long)]
        actor: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Mark the session abandoned (no log, no last_made update).
    Abandon { session: String },
}

// ── Log ─────────────────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum LogCommands {
    /// Add a food log entry. Provide quantity + unit + food, or use
    /// `--barcode` / `--food-id` / `--product-id` for catalog lookups.
    Add {
        /// Quantity (e.g. `2`).
        quantity: f64,
        /// Unit (e.g. `g`, `cup`, `oz`).
        unit: String,
        /// Free-form food name (`scrambled eggs`).
        food: Option<String>,
        #[arg(long)]
        date: Option<String>,
        #[arg(long, default_value = "other")]
        meal: String,
        #[arg(long)]
        barcode: Option<String>,
        #[arg(long = "food-id")]
        food_id: Option<String>,
        #[arg(long = "product-id")]
        product_id: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        notes: Option<String>,
    },
    /// List log rows.
    Show {
        #[arg(long)]
        date: Option<String>,
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Edit a log row.
    Update {
        log_id: String,
        #[arg(long = "quantity-grams")]
        quantity_grams: Option<f64>,
        #[arg(long)]
        kcal: Option<f64>,
        #[arg(long)]
        protein: Option<f64>,
        #[arg(long)]
        carbs: Option<f64>,
        #[arg(long)]
        fat: Option<f64>,
        #[arg(long)]
        notes: Option<String>,
    },
    Delete {
        log_id: String,
    },
}

// ── Nutrition ───────────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum NutritionCommands {
    /// Sum every log row for today.
    Today {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Per-day breakdown for a 7-day window.
    Week {
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Print a recipe's cached nutrition_summary; with `--recompute` it
    /// is re-aggregated first.
    Recipe {
        recipe: String,
        #[arg(long)]
        recompute: bool,
        #[arg(long)]
        json: bool,
    },
}

// ── Food ────────────────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum FoodCommands {
    List {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Show {
        food: String,
        #[arg(long)]
        json: bool,
    },
    Create {
        #[arg(long)]
        name: String,
        #[arg(long = "alias", value_name = "ALIAS")]
        aliases: Vec<String>,
        #[arg(long)]
        category: Option<String>,
        #[arg(long = "default-unit")]
        default_unit: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        /// JSON-encoded `NutritionFacts` payload.
        #[arg(long)]
        nutrition: Option<String>,
        #[arg(long)]
        notes: Option<String>,
    },
    Alias {
        food: String,
        alias: String,
    },
    /// Manually link a recipe-ingredient row to a Food.
    Link {
        recipe_ingredient_id: String,
        food: String,
    },
}

// ── Product ─────────────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum ProductCommands {
    List {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Show {
        product: String,
        #[arg(long)]
        json: bool,
    },
    Create {
        #[arg(long)]
        food: String,
        #[arg(long)]
        barcode: Option<String>,
        #[arg(long)]
        brand: Option<String>,
        #[arg(long)]
        name: String,
        #[arg(long = "package-size-g")]
        package_size_g: Option<f64>,
        #[arg(long = "package-size-label")]
        package_size_label: Option<String>,
        #[arg(long, default_value = "manual")]
        source: String,
        #[arg(long)]
        organization: Option<String>,
    },
    /// Look up a product on Open Food Facts (cache-first). Default
    /// cache TTL is 7 days; pass `--max-age-hours 0` to force a fresh
    /// fetch.
    Lookup {
        barcode: String,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long = "max-age-hours", default_value = "168")]
        max_age_hours: u32,
        #[arg(long = "auto-create-food")]
        auto_create_food: bool,
        #[arg(long)]
        json: bool,
    },
}

// ── Pantry ──────────────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum PantryCommands {
    List {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        location: Option<String>,
        #[arg(long = "expiring-within-days")]
        expiring_within_days: Option<u32>,
        #[arg(long = "low-stock")]
        low_stock: bool,
        #[arg(long)]
        json: bool,
    },
    Show {
        pantry_item: String,
        #[arg(long)]
        json: bool,
    },
    Add {
        #[arg(long)]
        barcode: Option<String>,
        #[arg(long)]
        food: Option<String>,
        #[arg(long = "food-id")]
        food_id: Option<String>,
        #[arg(long = "product-id")]
        product_id: Option<String>,
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        quantity: f64,
        #[arg(long)]
        unit: String,
        #[arg(long)]
        expiration: Option<String>,
        #[arg(long = "min-stock")]
        min_stock: Option<f64>,
        #[arg(long)]
        purchased: Option<String>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        notes: Option<String>,
        #[arg(long = "allow-manual-product")]
        allow_manual_product: bool,
    },
    Consume {
        #[arg(long)]
        food: Option<String>,
        #[arg(long = "food-id")]
        food_id: Option<String>,
        #[arg(long = "product-id")]
        product_id: Option<String>,
        #[arg(long = "pantry-item-id")]
        pantry_item_id: Option<String>,
        #[arg(long)]
        amount: f64,
        #[arg(long)]
        unit: String,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        notes: Option<String>,
    },
    Update {
        pantry_item: String,
        #[arg(long)]
        quantity: Option<f64>,
        #[arg(long)]
        unit: Option<String>,
        #[arg(long)]
        location: Option<String>,
        #[arg(long)]
        expiration: Option<String>,
        #[arg(long = "opened-at")]
        opened_at: Option<String>,
        #[arg(long = "min-stock")]
        min_stock: Option<f64>,
        #[arg(long)]
        notes: Option<String>,
    },
    Delete {
        pantry_item: String,
    },
    Expiring {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long = "within-days", default_value_t = 7)]
        within_days: u32,
        #[arg(long)]
        json: bool,
    },
    LowStock {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

// ── Recipe ──────────────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum RecipeCommands {
    List {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Show {
        recipe: String,
        #[arg(long)]
        json: bool,
    },
    Create(CreateRecipeArgs),
    Rate {
        recipe: String,
        rating: f32,
    },
    Made {
        recipe: String,
        #[arg(long)]
        date: Option<String>,
    },
    /// Mealie-style import from a URL (schema.org JSON-LD + OpenGraph
    /// fallback). Use `--dry-run` to preview without persisting.
    Import {
        url: String,
        #[arg(long)]
        dry_run: bool,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long = "created-by")]
        created_by: Option<String>,
        #[arg(long)]
        json: bool,
    },
    /// Match recipes against current pantry stock. Use
    /// `--include-partial` to also list recipes missing some
    /// ingredients.
    WhatCanICook {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long = "include-partial")]
        include_partial: bool,
        #[arg(long)]
        json: bool,
    },
    /// Linearly scale a recipe's ingredients to a target serving count.
    Scale {
        recipe: String,
        #[arg(long)]
        servings: u32,
        #[arg(long)]
        json: bool,
    },
    /// Suggest substitutions for missing ingredients or a dietary
    /// restriction.
    Substitutions {
        recipe: String,
        /// Repeatable: ingredient food name or UUID that's missing.
        #[arg(long = "missing", value_name = "FOOD")]
        missing: Vec<String>,
        /// Repeatable: dietary tag (vegan, gluten_free, dairy_free, …).
        #[arg(long = "diet", value_name = "TAG")]
        diet: Vec<String>,
        #[arg(long)]
        limit: Option<u32>,
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        json: bool,
    },
}

#[derive(Args)]
pub(crate) struct CreateRecipeArgs {
    #[arg(long)]
    name: String,
    #[arg(long)]
    description: Option<String>,
    #[arg(long)]
    prep: Option<u32>,
    #[arg(long)]
    cook: Option<u32>,
    #[arg(long)]
    servings: Option<u32>,
    #[arg(long)]
    organization: Option<String>,
    /// One ingredient spec per `--ingredients` flag. Format:
    /// `"<qty?> <unit?> <food>"` (e.g. `"1.5 cup olive oil"` or
    /// `"2 large eggs"` or just `"salt"`).
    #[arg(long = "ingredient", value_name = "INGREDIENT")]
    ingredients: Vec<String>,
    /// One step text per `--step` flag.
    #[arg(long = "step", value_name = "STEP")]
    steps: Vec<String>,
}

// ── Cookbook ────────────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum CookbookCommands {
    List {
        #[arg(long)]
        json: bool,
    },
    Show {
        cookbook: String,
        #[arg(long)]
        json: bool,
    },
    Create {
        #[arg(long)]
        name: String,
        #[arg(long)]
        description: Option<String>,
        #[arg(long)]
        organization: Option<String>,
    },
    Add {
        cookbook: String,
        recipe: String,
    },
    Remove {
        cookbook: String,
        recipe: String,
    },
}

// ── Plan ────────────────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum PlanCommands {
    List {
        #[arg(long)]
        organization: Option<String>,
        #[arg(long)]
        from: Option<String>,
        #[arg(long)]
        to: Option<String>,
        #[arg(long)]
        json: bool,
    },
    Set {
        #[arg(long)]
        date: String,
        #[arg(long = "meal-type")]
        meal_type: String,
        #[arg(long)]
        recipe: Option<String>,
        #[arg(long)]
        title: Option<String>,
        #[arg(long)]
        servings: Option<u32>,
        #[arg(long)]
        organization: Option<String>,
    },
    Delete {
        entry_id: String,
    },
    /// Mark a meal-plan entry as cooked: snapshots nutrition into a
    /// FoodLog row.
    MarkCooked {
        entry_id: String,
        #[arg(long)]
        servings: Option<u32>,
        #[arg(long)]
        actor: Option<String>,
    },
}

// ── Shop ────────────────────────────────────────────────────────────

#[derive(Subcommand)]
pub(crate) enum ShopCommands {
    List {
        #[arg(long)]
        json: bool,
    },
    Show {
        list: String,
        #[arg(long)]
        json: bool,
    },
    Create {
        #[arg(long)]
        name: String,
        #[arg(long)]
        organization: Option<String>,
    },
    Generate {
        list: String,
        #[arg(long)]
        from: String,
        #[arg(long)]
        to: String,
        #[arg(long)]
        organization: Option<String>,
    },
    /// Generate a list with only the ingredients NOT already in the
    /// pantry across the given meal-plan date range.
    GenerateFromMissing {
        list: String,
        #[arg(long)]
        from: String,
        #[arg(long)]
        to: String,
        #[arg(long)]
        organization: Option<String>,
    },
    Add {
        list: String,
        #[arg(long)]
        food: String,
        #[arg(long)]
        quantity: Option<f64>,
        #[arg(long)]
        unit: Option<String>,
        #[arg(long)]
        label: Option<String>,
    },
    Check {
        item_id: String,
    },
    Uncheck {
        item_id: String,
    },
}

// ── Dispatcher ──────────────────────────────────────────────────────

pub(crate) async fn run_remote_cook_command(
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: CookCommands,
) -> eyre::Result<()> {
    match command {
        CookCommands::Recipe { command } => {
            let client = remote.cooking().await?;
            run_recipe(&client, remote, actor, command).await
        }
        CookCommands::Cookbook { command } => {
            let client = remote.cooking().await?;
            run_cookbook(&client, command).await
        }
        CookCommands::Plan { command } => {
            let client = remote.cooking().await?;
            run_plan(&client, actor, command).await
        }
        CookCommands::Shop { command } => {
            let client = remote.cooking().await?;
            run_shop(&client, remote, command).await
        }
        CookCommands::Food { command } => {
            let client = remote.food().await?;
            run_food(&client, actor, command).await
        }
        CookCommands::Product { command } => {
            let client = remote.food().await?;
            run_product(&client, command).await
        }
        CookCommands::Pantry { command } => {
            let pantry_client = remote.pantry().await?;
            let cooking_client = remote.cooking().await?;
            run_pantry(&pantry_client, &cooking_client, command).await
        }
        CookCommands::Log { command } => {
            let client = remote.nutrition().await?;
            run_log(&client, actor, command).await
        }
        CookCommands::Nutrition { command } => run_nutrition(remote, command).await,
        CookCommands::Session { command } => {
            let client = remote.cooking().await?;
            run_session(&client, remote, command).await
        }
        CookCommands::Substitution { command } => {
            let client = remote.cooking().await?;
            let food_client = remote.food().await?;
            run_substitution(&client, &food_client, command).await
        }
    }
}

// ── Log handlers ────────────────────────────────────────────────────

fn parse_date_or_today(value: Option<String>) -> eyre::Result<NaiveDate> {
    match value {
        Some(s) => NaiveDate::parse_from_str(&s, "%Y-%m-%d")
            .map_err(|e| eyre::eyre!("invalid date '{s}': {e}")),
        None => Ok(chrono::Local::now().date_naive()),
    }
}

async fn run_log(
    client: &task_core::service::NutritionServiceClient,
    actor: Option<&str>,
    command: LogCommands,
) -> eyre::Result<()> {
    use task_core::service::{FoodLogPatch, LogFoodRequest, LogListRequest};
    match command {
        LogCommands::Add {
            quantity,
            unit,
            food,
            date,
            meal,
            barcode,
            food_id,
            product_id,
            organization,
            notes,
        } => {
            let date = parse_date_or_today(date)?;
            let req = LogFoodRequest {
                date,
                meal_type: meal,
                organization,
                food_id: food_id.as_deref().map(Uuid::parse_str).transpose()?,
                food_name: food,
                product_id: product_id.as_deref().map(Uuid::parse_str).transpose()?,
                barcode,
                quantity,
                unit,
                notes,
                created_by: actor.map(str::to_string),
            };
            let row = client
                .log_food(req)
                .await
                .map_err(|e| eyre::eyre!("log_food: {e}"))?;
            println!("logged: {} ({:?} kcal)", row.food_name, row.kcal);
        }
        LogCommands::Show {
            date,
            from,
            to,
            organization,
            json,
        } => {
            let from_date = match (date.clone(), from) {
                (Some(d), _) | (None, Some(d)) => NaiveDate::parse_from_str(&d, "%Y-%m-%d")?,
                _ => chrono::Local::now().date_naive(),
            };
            let to_date = match (date, to) {
                (Some(d), _) => NaiveDate::parse_from_str(&d, "%Y-%m-%d")?,
                (_, Some(d)) => NaiveDate::parse_from_str(&d, "%Y-%m-%d")?,
                _ => from_date,
            };
            let rows = client
                .list_log(LogListRequest {
                    organization,
                    from_date,
                    to_date,
                })
                .await
                .map_err(|e| eyre::eyre!("list_log: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&rows)?);
            } else {
                for r in &rows {
                    println!(
                        "{}  {:?}  {}  {:.1}g  kcal={:?}",
                        r.date, r.meal_type, r.food_name, r.quantity_grams, r.kcal
                    );
                }
                println!("({} rows)", rows.len());
            }
        }
        LogCommands::Update {
            log_id,
            quantity_grams,
            kcal,
            protein,
            carbs,
            fat,
            notes,
        } => {
            let id = Uuid::parse_str(&log_id)?;
            let row = client
                .update_log(
                    id,
                    FoodLogPatch {
                        quantity_grams,
                        kcal,
                        protein_g: protein,
                        carbs_g: carbs,
                        fat_g: fat,
                        notes,
                    },
                )
                .await
                .map_err(|e| eyre::eyre!("update_log: {e}"))?;
            println!("updated: {}", row.id);
        }
        LogCommands::Delete { log_id } => {
            let id = Uuid::parse_str(&log_id)?;
            client
                .delete_log(id)
                .await
                .map_err(|e| eyre::eyre!("delete_log: {e}"))?;
            println!("deleted {id}");
        }
    }
    Ok(())
}

async fn run_nutrition(remote: &RemoteVoxConfig, command: NutritionCommands) -> eyre::Result<()> {
    match command {
        NutritionCommands::Today { organization, json } => {
            let client = remote.nutrition().await?;
            let date = chrono::Local::now().date_naive();
            let totals = client
                .daily_totals(organization, date)
                .await
                .map_err(|e| eyre::eyre!("daily_totals: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&totals)?);
            } else {
                println!(
                    "{}: kcal={:.0} P={:.1}g C={:.1}g F={:.1}g  ({} rows)",
                    totals.date,
                    totals.kcal,
                    totals.protein_g,
                    totals.carbs_g,
                    totals.fat_g,
                    totals.log_count,
                );
            }
        }
        NutritionCommands::Week {
            from,
            organization,
            json,
        } => {
            let client = remote.nutrition().await?;
            let from_date = parse_date_or_today(from)?;
            let summary = client
                .weekly_summary(organization, from_date)
                .await
                .map_err(|e| eyre::eyre!("weekly_summary: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&summary)?);
            } else {
                for d in &summary.days {
                    println!(
                        "{}: kcal={:.0} P={:.1}g C={:.1}g F={:.1}g",
                        d.date, d.kcal, d.protein_g, d.carbs_g, d.fat_g
                    );
                }
                let a = &summary.averages;
                println!(
                    "avg/day: kcal={:.0} P={:.1}g C={:.1}g F={:.1}g",
                    a.kcal, a.protein_g, a.carbs_g, a.fat_g
                );
            }
        }
        NutritionCommands::Recipe {
            recipe,
            recompute,
            json,
        } => {
            let client = remote.cooking().await?;
            let id = if let Ok(uuid) = Uuid::parse_str(&recipe) {
                uuid
            } else {
                resolve_recipe_id(&client, &recipe).await?
            };
            if recompute {
                let view = client
                    .recompute_recipe_nutrition(id)
                    .await
                    .map_err(|e| eyre::eyre!("recompute_recipe_nutrition: {e}"))?;
                if json {
                    println!("{}", serde_json::to_string_pretty(&view)?);
                } else {
                    println!("recipe: {} ({})", view.recipe_name, view.recipe_id);
                    println!("total: {}", view.total_json);
                    if let Some(p) = &view.per_serving_json {
                        println!("per serving: {p}");
                    }
                    for w in &view.warnings {
                        println!("warning: {w}");
                    }
                }
                return Ok(());
            }
            let recipe_with = client
                .get_recipe(id)
                .await
                .map_err(|e| eyre::eyre!("get_recipe: {e}"))?
                .ok_or_else(|| eyre::eyre!("recipe not found: {id}"))?;
            // RecipeApi doesn't carry nutrition_summary in list; we just
            // print the recipe id + name and ask the user to recompute.
            if json {
                println!("{}", serde_json::to_string_pretty(&recipe_with.recipe)?);
            } else {
                println!(
                    "recipe: {} ({})",
                    recipe_with.recipe.name, recipe_with.recipe.id
                );
                println!("(use --recompute to refresh and print the nutrition summary)");
            }
        }
    }
    Ok(())
}

// ── Food handlers ───────────────────────────────────────────────────

async fn resolve_food_id(client: &FoodServiceClient, reference: &str) -> eyre::Result<Uuid> {
    if let Ok(id) = Uuid::parse_str(reference) {
        return Ok(id);
    }
    // Try organization-less first, then it's up to the caller to use a
    // UUID for org-scoped lookups (the lookup helpers walk the global
    // catalog; for org-scoped the user can pass the UUID directly).
    let hit = client
        .find_food_by_name(None, reference.to_string())
        .await
        .map_err(|e| eyre::eyre!("find_food_by_name: {e}"))?;
    if let Some(food) = hit {
        return Ok(food.id);
    }
    // Fall back to a personal-org lookup (the seeded catalog lives there).
    let hit = client
        .find_food_by_name(Some("personal".to_string()), reference.to_string())
        .await
        .map_err(|e| eyre::eyre!("find_food_by_name: {e}"))?;
    hit.map(|f| f.id)
        .ok_or_else(|| eyre::eyre!("food not found: {reference}"))
}

async fn run_food(
    client: &FoodServiceClient,
    actor: Option<&str>,
    command: FoodCommands,
) -> eyre::Result<()> {
    match command {
        FoodCommands::List { organization, json } => {
            let foods = client
                .list_foods(organization)
                .await
                .map_err(|e| eyre::eyre!("list_foods: {e}"))?;
            print_foods(&foods, json)?;
        }
        FoodCommands::Show { food, json } => {
            let id = resolve_food_id(client, &food).await?;
            let item = client
                .get_food(id)
                .await
                .map_err(|e| eyre::eyre!("get_food: {e}"))?
                .ok_or_else(|| eyre::eyre!("food not found: {food}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&item)?);
            } else {
                println!("{}  {}", item.id, item.name);
                if let Some(cat) = &item.category {
                    println!("  category:     {cat}");
                }
                if let Some(unit) = &item.default_unit {
                    println!("  default unit: {unit}");
                }
                if !item.aliases.is_empty() {
                    let list: Vec<&String> = item.aliases.iter().collect();
                    println!("  aliases:      {list:?}");
                }
                if let Some(org) = &item.organization {
                    println!("  organization: {org}");
                }
            }
        }
        FoodCommands::Create {
            name,
            aliases,
            category,
            default_unit,
            organization,
            nutrition,
            notes,
        } => {
            let saved = client
                .create_food(CreateFoodRequest {
                    name,
                    aliases,
                    category,
                    default_unit,
                    organization,
                    nutrition_json: nutrition,
                    notes,
                    created_by: actor.map(str::to_string),
                })
                .await
                .map_err(|e| eyre::eyre!("create_food: {e}"))?;
            println!("Created food '{}' (id={})", saved.name, saved.id);
        }
        FoodCommands::Alias { food, alias } => {
            let id = resolve_food_id(client, &food).await?;
            let updated = client
                .add_food_alias(id, alias)
                .await
                .map_err(|e| eyre::eyre!("add_food_alias: {e}"))?;
            let list: Vec<&String> = updated.aliases.iter().collect();
            println!("'{}' aliases: {list:?}", updated.name);
        }
        FoodCommands::Link {
            recipe_ingredient_id,
            food,
        } => {
            let ing_id = Uuid::parse_str(&recipe_ingredient_id)
                .map_err(|e| eyre::eyre!("invalid recipe-ingredient UUID: {e}"))?;
            let food_id = resolve_food_id(client, &food).await?;
            client
                .link_recipe_ingredient(ing_id, food_id)
                .await
                .map_err(|e| eyre::eyre!("link_recipe_ingredient: {e}"))?;
            println!("Linked recipe-ingredient {ing_id} -> food {food_id}");
        }
    }
    Ok(())
}

async fn run_product(client: &FoodServiceClient, command: ProductCommands) -> eyre::Result<()> {
    match command {
        ProductCommands::List { organization, json } => {
            let rows = client
                .list_food_products(organization)
                .await
                .map_err(|e| eyre::eyre!("list_food_products: {e}"))?;
            print_products(&rows, json)?;
        }
        ProductCommands::Show { product, json } => {
            // UUID short-circuit; otherwise treat as barcode.
            let item = if let Ok(id) = Uuid::parse_str(&product) {
                client
                    .get_food_product(id)
                    .await
                    .map_err(|e| eyre::eyre!("get_food_product: {e}"))?
            } else {
                // Try personal org by default for barcode lookup.
                client
                    .get_food_product_by_barcode(Some("personal".to_string()), product.clone())
                    .await
                    .map_err(|e| eyre::eyre!("get_food_product_by_barcode: {e}"))?
            }
            .ok_or_else(|| eyre::eyre!("food product not found: {product}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&item)?);
            } else {
                println!("{}  {}", item.id, item.name);
                if let Some(brand) = &item.brand {
                    println!("  brand:    {brand}");
                }
                if let Some(bar) = &item.barcode {
                    println!("  barcode:  {bar}");
                }
                println!("  food_id:  {}", item.food_id);
                println!("  source:   {}", item.source);
            }
        }
        ProductCommands::Lookup {
            barcode,
            organization,
            max_age_hours,
            auto_create_food,
            json,
        } => {
            // Track whether the product was already cached so we can
            // tell the user "freshly fetched" vs "served from cache".
            let cached_before = client
                .get_food_product_by_barcode(organization.clone(), barcode.clone())
                .await
                .map_err(|e| eyre::eyre!("get_food_product_by_barcode: {e}"))?;
            let request = BarcodeLookupRequest {
                barcode: barcode.clone(),
                organization,
                max_age_hours,
                auto_create_food,
                user_agent_override: None,
            };
            let outcome = client
                .lookup_food_product_by_barcode(request)
                .await
                .map_err(|e| eyre::eyre!("lookup_food_product_by_barcode: {e}"))?;
            match outcome {
                None => {
                    if json {
                        println!("null");
                    } else {
                        println!("(no product found for barcode {barcode})");
                    }
                }
                Some(product) => {
                    let was_cached = match (cached_before.as_ref(), product.last_synced_at) {
                        (Some(prev), Some(now_synced)) => prev
                            .last_synced_at
                            .map(|prev_synced| prev_synced == now_synced)
                            .unwrap_or(false),
                        _ => false,
                    };
                    if json {
                        println!("{}", serde_json::to_string_pretty(&product)?);
                    } else {
                        println!(
                            "{}  {}  ({})",
                            if was_cached { "[cached]" } else { "[fresh] " },
                            product.name,
                            product.barcode.clone().unwrap_or_else(|| "-".to_string()),
                        );
                        if let Some(brand) = &product.brand {
                            println!("  brand:        {brand}");
                        }
                        if let Some(label) = &product.package_size_label {
                            println!("  package:      {label}");
                        }
                        if let Some(grams) = product.package_size_g {
                            println!("  package (g):  {grams}");
                        }
                        if let Some(url) = &product.image_url {
                            println!("  image:        {url}");
                        }
                        // Pull kcal from the JSON nutrition payload.
                        let nutrition_value = serde_json::to_value(&product.nutrition_per_100g)
                            .unwrap_or(serde_json::Value::Null);
                        if let Some(kcal) = nutrition_value
                            .get("kcal_per_100g")
                            .and_then(|v| v.as_f64())
                        {
                            println!("  kcal/100g:    {kcal:.0}");
                        }
                        println!("  food_id:      {}", product.food_id);
                    }
                }
            }
        }
        ProductCommands::Create {
            food,
            barcode,
            brand,
            name,
            package_size_g,
            package_size_label,
            source,
            organization,
        } => {
            let food_id = resolve_food_id(client, &food).await?;
            let saved = client
                .create_food_product(CreateFoodProductRequest {
                    food_id,
                    barcode,
                    brand,
                    name,
                    package_size_g,
                    package_size_label,
                    source,
                    external_id: None,
                    nutrition_json: None,
                    image_url: None,
                    organization,
                })
                .await
                .map_err(|e| eyre::eyre!("create_food_product: {e}"))?;
            println!("Created product '{}' (id={})", saved.name, saved.id);
        }
    }
    Ok(())
}

fn print_foods(foods: &[FoodApi], json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(foods)?);
        return Ok(());
    }
    if foods.is_empty() {
        println!("(no foods)");
        return Ok(());
    }
    for f in foods {
        println!(
            "{}  {}  ({})",
            f.id,
            f.name,
            f.category.clone().unwrap_or_else(|| "-".to_string())
        );
    }
    Ok(())
}

fn print_products(products: &[FoodProductApi], json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(products)?);
        return Ok(());
    }
    if products.is_empty() {
        println!("(no products)");
        return Ok(());
    }
    for p in products {
        println!(
            "{}  {}  [{}]  {}",
            p.id,
            p.name,
            p.barcode.clone().unwrap_or_else(|| "-".to_string()),
            p.brand.clone().unwrap_or_else(|| "-".to_string()),
        );
    }
    Ok(())
}

// ── Recipe handlers ─────────────────────────────────────────────────

async fn run_recipe(
    client: &CookingServiceClient,
    remote: &RemoteVoxConfig,
    actor: Option<&str>,
    command: RecipeCommands,
) -> eyre::Result<()> {
    match command {
        RecipeCommands::List { organization, json } => {
            let recipes = client
                .list_recipes(organization)
                .await
                .map_err(|e| eyre::eyre!("list_recipes: {e}"))?;
            print_recipes(&recipes, json)?;
        }
        RecipeCommands::Show { recipe, json } => {
            let id = resolve_recipe_id(client, &recipe).await?;
            let detail = client
                .get_recipe(id)
                .await
                .map_err(|e| eyre::eyre!("get_recipe: {e}"))?
                .ok_or_else(|| eyre::eyre!("recipe not found: {recipe}"))?;
            print_recipe_detail_with_glossary(&detail, json, Some(remote)).await?;
        }
        RecipeCommands::Create(args) => {
            let ingredients: Vec<RecipeIngredientSpec> = args
                .ingredients
                .iter()
                .map(|s| parse_ingredient(s))
                .collect();
            let steps: Vec<RecipeStepSpec> = args
                .steps
                .into_iter()
                .map(|text| RecipeStepSpec {
                    text,
                    ..Default::default()
                })
                .collect();
            let request = CreateRecipeRequest {
                name: args.name,
                description: args.description,
                organization: args.organization,
                prep_time_minutes: args.prep,
                cook_time_minutes: args.cook,
                servings: args.servings,
                source_url: None,
                created_by: actor.map(str::to_string),
                image_url: None,
                yield_label: None,
                properties_json: None,
                ingredients_json: serde_json::to_string(&ingredients)?,
                steps_json: serde_json::to_string(&steps)?,
            };
            let detail = client
                .create_recipe(request)
                .await
                .map_err(|e| eyre::eyre!("create_recipe: {e}"))?;
            print_recipe_detail(&detail, false)?;
        }
        RecipeCommands::Rate { recipe, rating } => {
            let id = resolve_recipe_id(client, &recipe).await?;
            let updated = client
                .rate_recipe(id, rating)
                .await
                .map_err(|e| eyre::eyre!("rate_recipe: {e}"))?;
            println!(
                "Rated '{}' {:.1} / 5",
                updated.name,
                updated.rating.unwrap_or(0.0)
            );
        }
        RecipeCommands::Import {
            url,
            dry_run,
            organization,
            created_by,
            json,
        } => {
            if dry_run {
                let preview = client
                    .preview_recipe_import(url)
                    .await
                    .map_err(|e| eyre::eyre!("preview_recipe_import: {e}"))?;
                if json {
                    println!("{}", preview.draft_json);
                } else {
                    println!("Strategy: {}", preview.strategy);
                    println!("Source:   {}", preview.source_url);
                    let warnings: Vec<String> =
                        serde_json::from_str(&preview.warnings_json).unwrap_or_default();
                    if !warnings.is_empty() {
                        println!("Warnings:");
                        for w in &warnings {
                            println!("  - {w}");
                        }
                    }
                    let pretty: serde_json::Value = serde_json::from_str(&preview.draft_json)
                        .unwrap_or(serde_json::Value::Null);
                    println!(
                        "Draft:\n{}",
                        serde_json::to_string_pretty(&pretty).unwrap_or(preview.draft_json)
                    );
                }
            } else {
                let detail = client
                    .import_recipe(ImportRecipeRequest {
                        url,
                        organization,
                        created_by: created_by.or_else(|| actor.map(str::to_string)),
                    })
                    .await
                    .map_err(|e| eyre::eyre!("import_recipe: {e}"))?;
                if json {
                    print_recipe_detail(&detail, true)?;
                } else {
                    println!(
                        "Imported '{}' (id={}, slug={})",
                        detail.recipe.name, detail.recipe.id, detail.recipe.slug
                    );
                }
            }
        }
        RecipeCommands::Made { recipe, date } => {
            let id = resolve_recipe_id(client, &recipe).await?;
            let on = match date {
                Some(s) => Some(parse_date(&s)?),
                None => None,
            };
            let updated = client
                .mark_made(id, on)
                .await
                .map_err(|e| eyre::eyre!("mark_made: {e}"))?;
            println!(
                "Marked '{}' made on {}",
                updated.name,
                updated
                    .last_made
                    .map(|d| d.to_string())
                    .unwrap_or_else(|| "—".to_string())
            );
        }
        RecipeCommands::WhatCanICook {
            organization,
            include_partial,
            json,
        } => {
            let pantry_client = remote.pantry().await?;
            let matches = pantry_client
                .recipes_i_can_cook(organization)
                .await
                .map_err(|e| eyre::eyre!("recipes_i_can_cook: {e}"))?;
            let filtered: Vec<_> = matches
                .into_iter()
                .filter(|m| include_partial || m.matched_ingredients == m.total_ingredients)
                .collect();
            if json {
                println!("{}", serde_json::to_string_pretty(&filtered)?);
            } else if filtered.is_empty() {
                println!("(no recipes match the current pantry)");
            } else {
                for m in &filtered {
                    let tag = if m.matched_ingredients == m.total_ingredients {
                        "FULL"
                    } else {
                        "partial"
                    };
                    println!(
                        "[{tag}] {}  ({}/{})  {}",
                        m.recipe_name, m.matched_ingredients, m.total_ingredients, m.recipe_id
                    );
                    if !m.unmatched_food_lines.is_empty() {
                        println!("   missing: {}", m.unmatched_food_lines.join(", "));
                    }
                    for w in &m.warnings {
                        println!("   warn: {w}");
                    }
                }
            }
        }
        RecipeCommands::Substitutions {
            recipe,
            missing,
            diet,
            limit,
            organization,
            json,
        } => {
            let recipe_id = resolve_recipe_id(client, &recipe).await?;
            let food_client = remote.food().await?;
            let mut missing_ids: Vec<Uuid> = Vec::new();
            for m in &missing {
                missing_ids.push(resolve_food_id(&food_client, m).await?);
            }
            let view = client
                .suggest_substitutions(SuggestSubstitutionsRequest {
                    recipe_id,
                    missing_food_ids: missing_ids,
                    dietary_filter: diet,
                    organization,
                    limit_per_ingredient: limit,
                })
                .await
                .map_err(|e| eyre::eyre!("suggest_substitutions: {e}"))?;
            print_substitution_suggestions(&view, json)?;
        }
        RecipeCommands::Scale {
            recipe,
            servings,
            json,
        } => {
            let id = resolve_recipe_id(client, &recipe).await?;
            let view = client
                .scale_recipe(id, servings)
                .await
                .map_err(|e| eyre::eyre!("scale_recipe: {e}"))?;
            if json {
                println!(
                    "{}",
                    serde_json::to_string_pretty(&view.scaled_ingredients_json)?
                );
            } else {
                println!(
                    "{}: {} -> {} servings (x{:.3})",
                    view.recipe_name,
                    view.source_servings
                        .map(|n| n.to_string())
                        .unwrap_or_else(|| "?".to_string()),
                    view.target_servings,
                    view.multiplier
                );
                for w in &view.warnings {
                    println!("  warn: {w}");
                }
                let pretty: serde_json::Value =
                    serde_json::from_str(&view.scaled_ingredients_json).unwrap_or_default();
                println!(
                    "{}",
                    serde_json::to_string_pretty(&pretty).unwrap_or(view.scaled_ingredients_json)
                );
            }
        }
    }
    let _ = actor;
    Ok(())
}

// ── Session handlers ────────────────────────────────────────────────

async fn run_session(
    client: &CookingServiceClient,
    remote: &RemoteVoxConfig,
    command: SessionCommands,
) -> eyre::Result<()> {
    match command {
        SessionCommands::Start {
            recipe,
            servings,
            organization,
            created_by,
            json,
        } => {
            let recipe_id = resolve_recipe_id(client, &recipe).await?;
            let view = client
                .start_cooking_session(StartCookingSessionRequest {
                    recipe_id,
                    scaled_servings: servings,
                    organization,
                    created_by,
                })
                .await
                .map_err(|e| eyre::eyre!("start_cooking_session: {e}"))?;
            print_session(&view, json)?;
        }
        SessionCommands::Show { session, json } => {
            let id = Uuid::parse_str(&session)?;
            let view = client
                .get_cooking_session(id)
                .await
                .map_err(|e| eyre::eyre!("get_cooking_session: {e}"))?
                .ok_or_else(|| eyre::eyre!("cooking session not found: {session}"))?;
            print_session_with_glossary(&view, json, Some(remote)).await?;
        }
        SessionCommands::ListActive { organization, json } => {
            let rows = client
                .list_active_cooking_sessions(organization)
                .await
                .map_err(|e| eyre::eyre!("list_active_cooking_sessions: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&rows)?);
            } else if rows.is_empty() {
                println!("(no active sessions)");
            } else {
                for s in rows {
                    println!(
                        "{}  {}  step {} started {}",
                        s.id, s.recipe_name_snapshot, s.current_step_index, s.started_at
                    );
                }
            }
        }
        SessionCommands::Step {
            session,
            next,
            previous,
            jump_to,
            json,
        } => {
            let session_id = Uuid::parse_str(&session)?;
            let direction = if next {
                "next"
            } else if previous {
                "previous"
            } else if jump_to.is_some() {
                "jump"
            } else {
                return Err(eyre::eyre!(
                    "must pass --next, --previous, or --jump-to <i32>"
                ));
            };
            let view = client
                .navigate_step(NavigateStepRequest {
                    session_id,
                    direction: direction.to_string(),
                    jump_to,
                })
                .await
                .map_err(|e| eyre::eyre!("navigate_step: {e}"))?;
            print_session(&view, json)?;
        }
        SessionCommands::Timer {
            session,
            action,
            step,
            json,
        } => {
            let session_id = Uuid::parse_str(&session)?;
            let view = client
                .step_timer_action(StepTimerActionRequest {
                    session_id,
                    step_index: step,
                    action,
                })
                .await
                .map_err(|e| eyre::eyre!("step_timer_action: {e}"))?;
            print_session(&view, json)?;
        }
        SessionCommands::Ingredient {
            session,
            index,
            check,
            uncheck,
            json,
        } => {
            let session_id = Uuid::parse_str(&session)?;
            let gathered = if check {
                true
            } else if uncheck {
                false
            } else {
                return Err(eyre::eyre!("pass --check or --uncheck"));
            };
            let view = client
                .mark_ingredient_gathered(MarkIngredientGatheredRequest {
                    session_id,
                    ingredient_index: index,
                    gathered,
                })
                .await
                .map_err(|e| eyre::eyre!("mark_ingredient_gathered: {e}"))?;
            print_session(&view, json)?;
        }
        SessionCommands::Complete {
            session,
            log_meal,
            servings,
            meal_type,
            date,
            actor,
            json,
        } => {
            let session_id = Uuid::parse_str(&session)?;
            let log_date = match date {
                Some(s) => Some(parse_date(&s)?),
                None => None,
            };
            let view = client
                .complete_cooking_session(CompleteCookingSessionRequest {
                    session_id,
                    log_meal,
                    servings_eaten: servings,
                    meal_type,
                    log_date,
                    actor,
                })
                .await
                .map_err(|e| eyre::eyre!("complete_cooking_session: {e}"))?;
            print_session(&view, json)?;
        }
        SessionCommands::Abandon { session } => {
            let id = Uuid::parse_str(&session)?;
            let view = client
                .abandon_cooking_session(id)
                .await
                .map_err(|e| eyre::eyre!("abandon_cooking_session: {e}"))?;
            println!(
                "Session {} abandoned (recipe: {})",
                view.session.id, view.session.recipe_name_snapshot
            );
        }
    }
    Ok(())
}

fn print_session(view: &CookingSessionView, json: bool) -> eyre::Result<()> {
    if json {
        // CookingSessionView is facet::Facet but not serde::Serialize;
        // assemble a JSON shape from its fields by hand so callers can
        // pipe / consume it.
        let payload = serde_json::json!({
            "session": view.session,
            "scaled_ingredients": serde_json::from_str::<serde_json::Value>(&view.scaled_ingredients_json).unwrap_or(serde_json::Value::Null),
            "steps": serde_json::from_str::<serde_json::Value>(&view.steps_json).unwrap_or(serde_json::Value::Null),
            "step_states": serde_json::from_str::<serde_json::Value>(&view.step_states_json).unwrap_or(serde_json::Value::Null),
            "mise_en_place": serde_json::from_str::<serde_json::Value>(&view.mise_en_place_json).unwrap_or(serde_json::Value::Null),
            "ungathered_count": view.ungathered_count,
        });
        println!("{}", serde_json::to_string_pretty(&payload)?);
        return Ok(());
    }
    let s = &view.session;
    println!(
        "Session {}  recipe: {}  status: {}",
        s.id,
        s.recipe_name_snapshot,
        s.status.as_str()
    );
    if let Some(srv) = s.scaled_servings {
        println!("  servings: {srv}");
    }
    let total_ings = view.mise_en_place_json.matches("true").count()
        + view.mise_en_place_json.matches("false").count();
    let gathered = total_ings - view.ungathered_count as usize;
    println!("  mise en place: {} of {} gathered", gathered, total_ings);
    let steps: serde_json::Value =
        serde_json::from_str(&view.steps_json).unwrap_or(serde_json::Value::Array(vec![]));
    let total_steps = steps.as_array().map(|a| a.len()).unwrap_or(0);
    if s.current_step_index < 0 {
        println!("  Phase: mise en place (before step 1 of {total_steps})");
    } else {
        let idx = s.current_step_index as usize;
        let step_text = steps
            .get(idx)
            .and_then(|s| s.get("text"))
            .and_then(|t| t.as_str())
            .unwrap_or("");
        let duration = steps
            .get(idx)
            .and_then(|s| s.get("duration_minutes"))
            .and_then(|d| d.as_u64());
        println!(
            "  Step {} of {}: {}{}",
            idx + 1,
            total_steps,
            step_text,
            duration
                .map(|d| format!(" ({d} min timer)"))
                .unwrap_or_default()
        );
    }
    Ok(())
}

// ── Cookbook handlers ───────────────────────────────────────────────

async fn run_cookbook(
    client: &CookingServiceClient,
    command: CookbookCommands,
) -> eyre::Result<()> {
    match command {
        CookbookCommands::List { json } => {
            let cookbooks = client
                .list_cookbooks(None)
                .await
                .map_err(|e| eyre::eyre!("list_cookbooks: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&cookbooks)?);
            } else if cookbooks.is_empty() {
                println!("(no cookbooks)");
            } else {
                for c in cookbooks {
                    println!(
                        "{}  {}  ({})",
                        c.id,
                        c.name,
                        c.organization.unwrap_or_else(|| "—".to_string())
                    );
                }
            }
        }
        CookbookCommands::Show { cookbook, json } => {
            let id = resolve_cookbook_id(client, &cookbook).await?;
            let detail = client
                .get_cookbook(id)
                .await
                .map_err(|e| eyre::eyre!("get_cookbook: {e}"))?
                .ok_or_else(|| eyre::eyre!("cookbook not found: {cookbook}"))?;
            print_cookbook_detail(&detail, json)?;
        }
        CookbookCommands::Create {
            name,
            description,
            organization,
        } => {
            let cb = client
                .create_cookbook(name, description, organization)
                .await
                .map_err(|e| eyre::eyre!("create_cookbook: {e}"))?;
            println!("Created cookbook {} ({})", cb.name, cb.id);
        }
        CookbookCommands::Add { cookbook, recipe } => {
            let cb_id = resolve_cookbook_id(client, &cookbook).await?;
            let r_id = resolve_recipe_id(client, &recipe).await?;
            client
                .add_recipe_to_cookbook(cb_id, r_id)
                .await
                .map_err(|e| eyre::eyre!("add_recipe_to_cookbook: {e}"))?;
            println!("Added.");
        }
        CookbookCommands::Remove { cookbook, recipe } => {
            let cb_id = resolve_cookbook_id(client, &cookbook).await?;
            let r_id = resolve_recipe_id(client, &recipe).await?;
            client
                .remove_recipe_from_cookbook(cb_id, r_id)
                .await
                .map_err(|e| eyre::eyre!("remove_recipe_from_cookbook: {e}"))?;
            println!("Removed.");
        }
    }
    Ok(())
}

// ── Plan handlers ───────────────────────────────────────────────────

async fn run_plan(
    client: &CookingServiceClient,
    actor: Option<&str>,
    command: PlanCommands,
) -> eyre::Result<()> {
    match command {
        PlanCommands::List {
            organization,
            from,
            to,
            json,
        } => {
            let today = chrono::Local::now().date_naive();
            let from_date = from.map(|s| parse_date(&s)).transpose()?.unwrap_or(today);
            let to_date = to
                .map(|s| parse_date(&s))
                .transpose()?
                .unwrap_or(today + chrono::Duration::days(7));
            let request = MealPlanRangeRequest {
                organization,
                from_date,
                to_date,
            };
            let entries = client
                .list_meal_plan(request)
                .await
                .map_err(|e| eyre::eyre!("list_meal_plan: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&entries)?);
            } else if entries.is_empty() {
                println!("(no plan entries)");
            } else {
                for e in entries {
                    let label = e
                        .title
                        .clone()
                        .or_else(|| e.recipe_id.map(|id| id.to_string()))
                        .unwrap_or_else(|| "—".to_string());
                    println!("{}  {:<10} {}", e.date, e.meal_type.as_str(), label);
                }
            }
        }
        PlanCommands::Set {
            date,
            meal_type,
            recipe,
            title,
            servings,
            organization,
        } => {
            let date = parse_date(&date)?;
            let recipe_id = match recipe {
                Some(r) => Some(resolve_recipe_id(client, &r).await?),
                None => None,
            };
            let request = SetMealPlanEntryRequest {
                date,
                meal_type,
                organization,
                recipe_id,
                title,
                servings_planned: servings,
                notes: None,
                created_by: actor.map(str::to_string),
            };
            let entry = client
                .set_meal_plan_entry(request)
                .await
                .map_err(|e| eyre::eyre!("set_meal_plan_entry: {e}"))?;
            println!(
                "Set slot {} {}: {}",
                entry.date,
                entry.meal_type.as_str(),
                entry.id
            );
        }
        PlanCommands::Delete { entry_id } => {
            let id = Uuid::parse_str(&entry_id)?;
            client
                .delete_meal_plan_entry(id)
                .await
                .map_err(|e| eyre::eyre!("delete_meal_plan_entry: {e}"))?;
            println!("Deleted.");
        }
        PlanCommands::MarkCooked {
            entry_id,
            servings,
            actor: explicit_actor,
        } => {
            let id = Uuid::parse_str(&entry_id)?;
            let req = task_core::service::MarkMealPlanCookedRequest {
                meal_plan_entry_id: id,
                servings_consumed: servings,
                created_by: explicit_actor.or_else(|| actor.map(str::to_string)),
            };
            let ids = client
                .mark_meal_plan_cooked(req)
                .await
                .map_err(|e| eyre::eyre!("mark_meal_plan_cooked: {e}"))?;
            for log_id in ids {
                println!("food_log: {log_id}");
            }
        }
    }
    Ok(())
}

// ── Shop handlers ───────────────────────────────────────────────────

async fn run_shop(
    client: &CookingServiceClient,
    remote: &RemoteVoxConfig,
    command: ShopCommands,
) -> eyre::Result<()> {
    match command {
        ShopCommands::List { json } => {
            let lists = client
                .list_shopping_lists(None)
                .await
                .map_err(|e| eyre::eyre!("list_shopping_lists: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&lists)?);
            } else if lists.is_empty() {
                println!("(no shopping lists)");
            } else {
                for l in lists {
                    println!(
                        "{}  {}  ({})",
                        l.id,
                        l.name,
                        l.organization.unwrap_or_else(|| "—".to_string())
                    );
                }
            }
        }
        ShopCommands::Show { list, json } => {
            let id = resolve_shopping_list_id(client, &list).await?;
            let detail = client
                .get_shopping_list(id)
                .await
                .map_err(|e| eyre::eyre!("get_shopping_list: {e}"))?
                .ok_or_else(|| eyre::eyre!("shopping list not found: {list}"))?;
            print_shopping_list_detail(&detail, json)?;
        }
        ShopCommands::Create { name, organization } => {
            let list = client
                .create_shopping_list(name, organization)
                .await
                .map_err(|e| eyre::eyre!("create_shopping_list: {e}"))?;
            println!("Created list {} ({})", list.name, list.id);
        }
        ShopCommands::Generate {
            list,
            from,
            to,
            organization,
        } => {
            let id = resolve_shopping_list_id(client, &list).await?;
            let request = GenerateShoppingListRequest {
                list_id: id,
                organization,
                from_date: parse_date(&from)?,
                to_date: parse_date(&to)?,
            };
            let detail = client
                .generate_from_meal_plan(request)
                .await
                .map_err(|e| eyre::eyre!("generate_from_meal_plan: {e}"))?;
            print_shopping_list_detail(&detail, false)?;
        }
        ShopCommands::GenerateFromMissing {
            list,
            from,
            to,
            organization,
        } => {
            let id = resolve_shopping_list_id(client, &list).await?;
            let pantry_client = remote.pantry().await?;
            let returned = pantry_client
                .generate_shopping_list_from_missing(GenerateShoppingListFromMissingRequest {
                    list_id: id,
                    organization,
                    from_date: parse_date(&from)?,
                    to_date: parse_date(&to)?,
                })
                .await
                .map_err(|e| eyre::eyre!("generate_shopping_list_from_missing: {e}"))?;
            let detail = client
                .get_shopping_list(returned)
                .await
                .map_err(|e| eyre::eyre!("get_shopping_list: {e}"))?
                .ok_or_else(|| eyre::eyre!("shopping list not found: {returned}"))?;
            print_shopping_list_detail(&detail, false)?;
        }
        ShopCommands::Add {
            list,
            food,
            quantity,
            unit,
            label,
        } => {
            let id = resolve_shopping_list_id(client, &list).await?;
            client
                .add_shopping_list_item(AddShoppingItemRequest {
                    list_id: id,
                    food,
                    quantity,
                    unit,
                    note: None,
                    label,
                })
                .await
                .map_err(|e| eyre::eyre!("add_shopping_list_item: {e}"))?;
            println!("Added.");
        }
        ShopCommands::Check { item_id } => {
            let id = Uuid::parse_str(&item_id)?;
            client
                .check_item(id, true)
                .await
                .map_err(|e| eyre::eyre!("check_item: {e}"))?;
            println!("Checked.");
        }
        ShopCommands::Uncheck { item_id } => {
            let id = Uuid::parse_str(&item_id)?;
            client
                .check_item(id, false)
                .await
                .map_err(|e| eyre::eyre!("check_item: {e}"))?;
            println!("Unchecked.");
        }
    }
    Ok(())
}

// ── Helpers ─────────────────────────────────────────────────────────

fn parse_date(s: &str) -> eyre::Result<NaiveDate> {
    NaiveDate::parse_from_str(s.trim(), "%Y-%m-%d")
        .map_err(|e| eyre::eyre!("invalid date '{s}' (want YYYY-MM-DD): {e}"))
}

/// Thin wrapper around [`task_core::recipe_ingredient::parse_ingredient_line`]
/// kept so the call sites read naturally (`parse_ingredient(line)` rather
/// than the longer free-function path).
fn parse_ingredient(spec: &str) -> RecipeIngredientSpec {
    task_core::recipe_ingredient::parse_ingredient_line(spec)
}

async fn resolve_recipe_id(client: &CookingServiceClient, reference: &str) -> eyre::Result<Uuid> {
    if let Ok(id) = Uuid::parse_str(reference) {
        return Ok(id);
    }
    let recipes = client
        .list_recipes(None)
        .await
        .map_err(|e| eyre::eyre!("list_recipes: {e}"))?;
    recipes
        .into_iter()
        .find(|r| r.name.eq_ignore_ascii_case(reference) || r.slug.eq_ignore_ascii_case(reference))
        .map(|r| r.id)
        .ok_or_else(|| eyre::eyre!("recipe not found: {reference}"))
}

async fn resolve_cookbook_id(client: &CookingServiceClient, reference: &str) -> eyre::Result<Uuid> {
    if let Ok(id) = Uuid::parse_str(reference) {
        return Ok(id);
    }
    let cookbooks = client
        .list_cookbooks(None)
        .await
        .map_err(|e| eyre::eyre!("list_cookbooks: {e}"))?;
    cookbooks
        .into_iter()
        .find(|c| c.name.eq_ignore_ascii_case(reference))
        .map(|c| c.id)
        .ok_or_else(|| eyre::eyre!("cookbook not found: {reference}"))
}

async fn resolve_shopping_list_id(
    client: &CookingServiceClient,
    reference: &str,
) -> eyre::Result<Uuid> {
    if let Ok(id) = Uuid::parse_str(reference) {
        return Ok(id);
    }
    let lists = client
        .list_shopping_lists(None)
        .await
        .map_err(|e| eyre::eyre!("list_shopping_lists: {e}"))?;
    lists
        .into_iter()
        .find(|l| l.name.eq_ignore_ascii_case(reference))
        .map(|l| l.id)
        .ok_or_else(|| eyre::eyre!("shopping list not found: {reference}"))
}

fn print_recipes(recipes: &[RecipeApi], json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(recipes)?);
        return Ok(());
    }
    if recipes.is_empty() {
        println!("(no recipes)");
        return Ok(());
    }
    for r in recipes {
        println!(
            "{}  {:<32} prep={:<4} cook={:<4} serves={:<3} rating={}",
            r.id,
            r.name,
            r.prep_time_minutes
                .map(|n| n.to_string())
                .unwrap_or_else(|| "—".to_string()),
            r.cook_time_minutes
                .map(|n| n.to_string())
                .unwrap_or_else(|| "—".to_string()),
            r.servings
                .map(|n| n.to_string())
                .unwrap_or_else(|| "—".to_string()),
            r.rating
                .map(|n| format!("{n:.1}"))
                .unwrap_or_else(|| "—".to_string()),
        );
    }
    Ok(())
}

fn print_recipe_detail(detail: &RecipeWithDetails, json: bool) -> eyre::Result<()> {
    if json {
        let payload = serde_json::json!({
            "recipe": detail.recipe,
            "ingredients": serde_json::from_str::<serde_json::Value>(&detail.ingredients_json)
                .unwrap_or(serde_json::Value::Array(Vec::new())),
            "steps": serde_json::from_str::<serde_json::Value>(&detail.steps_json)
                .unwrap_or(serde_json::Value::Array(Vec::new())),
        });
        println!("{}", serde_json::to_string_pretty(&payload)?);
        return Ok(());
    }
    let r = &detail.recipe;
    println!("Recipe {}", r.id);
    println!("  name:    {}", r.name);
    println!("  slug:    {}", r.slug);
    if let Some(d) = &r.description {
        println!("  desc:    {d}");
    }
    if let Some(p) = r.prep_time_minutes {
        println!("  prep:    {p} min");
    }
    if let Some(c) = r.cook_time_minutes {
        println!("  cook:    {c} min");
    }
    if let Some(s) = r.servings {
        println!("  serves:  {s}");
    }
    if let Some(r) = r.rating {
        println!("  rating:  {r:.1}");
    }
    let ingredients: Vec<serde_json::Value> =
        serde_json::from_str(&detail.ingredients_json).unwrap_or_default();
    println!("  ingredients:");
    for ing in ingredients {
        let qty = ing
            .get("quantity")
            .and_then(|v| v.as_f64())
            .map(|n| format!("{n} "))
            .unwrap_or_default();
        let unit = ing
            .get("unit")
            .and_then(|v| v.as_str())
            .map(|s| format!("{s} "))
            .unwrap_or_default();
        let food = ing.get("food").and_then(|v| v.as_str()).unwrap_or("");
        println!("    - {qty}{unit}{food}");
    }
    let steps: Vec<serde_json::Value> =
        serde_json::from_str(&detail.steps_json).unwrap_or_default();
    println!("  steps:");
    for (i, step) in steps.into_iter().enumerate() {
        let text = step.get("text").and_then(|v| v.as_str()).unwrap_or("");
        println!("    {}. {}", i + 1, text);
    }
    Ok(())
}

fn print_cookbook_detail(detail: &CookbookWithRecipes, json: bool) -> eyre::Result<()> {
    if json {
        let payload = serde_json::json!({
            "cookbook": detail.cookbook,
            "recipes": serde_json::from_str::<serde_json::Value>(&detail.recipes_json)
                .unwrap_or(serde_json::Value::Array(Vec::new())),
        });
        println!("{}", serde_json::to_string_pretty(&payload)?);
        return Ok(());
    }
    let c = &detail.cookbook;
    println!("Cookbook {}", c.id);
    println!("  name: {}", c.name);
    if let Some(d) = &c.description {
        println!("  desc: {d}");
    }
    let recipes: Vec<serde_json::Value> =
        serde_json::from_str(&detail.recipes_json).unwrap_or_default();
    println!("  recipes ({}):", recipes.len());
    for r in recipes {
        let name = r.get("name").and_then(|v| v.as_str()).unwrap_or("?");
        println!("    - {name}");
    }
    Ok(())
}

fn print_shopping_list_detail(detail: &ShoppingListWithItems, json: bool) -> eyre::Result<()> {
    if json {
        let payload = serde_json::json!({
            "list": detail.list,
            "items": serde_json::from_str::<serde_json::Value>(&detail.items_json)
                .unwrap_or(serde_json::Value::Array(Vec::new())),
        });
        println!("{}", serde_json::to_string_pretty(&payload)?);
        return Ok(());
    }
    let l = &detail.list;
    println!("Shopping list {}", l.id);
    println!("  name: {}", l.name);
    let items: Vec<serde_json::Value> =
        serde_json::from_str(&detail.items_json).unwrap_or_default();
    println!("  items ({}):", items.len());
    for item in items {
        let checked = item
            .get("checked")
            .and_then(|v| v.as_bool())
            .unwrap_or(false);
        let mark = if checked { "[x]" } else { "[ ]" };
        let qty = item
            .get("quantity")
            .and_then(|v| v.as_f64())
            .map(|n| format!("{n} "))
            .unwrap_or_default();
        let unit = item
            .get("unit")
            .and_then(|v| v.as_str())
            .map(|s| format!("{s} "))
            .unwrap_or_default();
        let food = item.get("food").and_then(|v| v.as_str()).unwrap_or("?");
        let id = item.get("id").and_then(|v| v.as_str()).unwrap_or("");
        println!("    {mark} {qty}{unit}{food}  ({id})");
    }
    Ok(())
}

// ── Pantry handlers ─────────────────────────────────────────────────

fn parse_optional_date(s: Option<&str>) -> eyre::Result<Option<NaiveDate>> {
    match s {
        Some(v) => Ok(Some(parse_date(v)?)),
        None => Ok(None),
    }
}

async fn run_pantry(
    pantry: &PantryServiceClient,
    cooking: &CookingServiceClient,
    command: PantryCommands,
) -> eyre::Result<()> {
    let _ = cooking;
    match command {
        PantryCommands::List {
            organization,
            location,
            expiring_within_days,
            low_stock,
            json,
        } => {
            let location_id = match location {
                Some(l) if !l.is_empty() => Some(Uuid::parse_str(&l).map_err(|_| {
                    eyre::eyre!("--location must be a UUID at the CLI; resolve via task cook pantry list output")
                })?),
                _ => None,
            };
            let rows = pantry
                .list_pantry_items(PantryListRequest {
                    organization,
                    location_id,
                    low_stock_only: low_stock,
                    expiring_within_days,
                })
                .await
                .map_err(|e| eyre::eyre!("list_pantry_items: {e}"))?;
            print_pantry_rows(&rows, json)?;
        }
        PantryCommands::Show { pantry_item, json } => {
            let id = Uuid::parse_str(&pantry_item)
                .map_err(|_| eyre::eyre!("pantry-item must be a UUID"))?;
            let row = pantry
                .get_pantry_item(id)
                .await
                .map_err(|e| eyre::eyre!("get_pantry_item: {e}"))?
                .ok_or_else(|| eyre::eyre!("pantry item not found: {id}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&row)?);
            } else {
                print_pantry_row(&row);
            }
        }
        PantryCommands::Add {
            barcode,
            food,
            food_id,
            product_id,
            location,
            quantity,
            unit,
            expiration,
            min_stock,
            purchased,
            organization,
            notes,
            allow_manual_product,
        } => {
            let request = AddToPantryRequest {
                organization,
                barcode,
                food_name: food,
                food_id: food_id
                    .map(|s| Uuid::parse_str(&s))
                    .transpose()
                    .map_err(|e| eyre::eyre!("--food-id: {e}"))?,
                product_id: product_id
                    .map(|s| Uuid::parse_str(&s))
                    .transpose()
                    .map_err(|e| eyre::eyre!("--product-id: {e}"))?,
                location_id: match location {
                    Some(l) => Some(
                        Uuid::parse_str(&l)
                            .map_err(|_| eyre::eyre!("--location must be a UUID at the CLI"))?,
                    ),
                    None => None,
                },
                quantity,
                unit,
                expiration_date: parse_optional_date(expiration.as_deref())?,
                min_stock,
                purchased_at: parse_optional_date(purchased.as_deref())?,
                notes,
                allow_manual_product,
            };
            let row = pantry
                .add_to_pantry(request)
                .await
                .map_err(|e| eyre::eyre!("add_to_pantry: {e}"))?;
            println!("Added pantry item {} ({})", row.id, row.unit);
        }
        PantryCommands::Consume {
            food,
            food_id,
            product_id,
            pantry_item_id,
            amount,
            unit,
            organization,
            notes,
        } => {
            let mut food_id_uuid = food_id
                .map(|s| Uuid::parse_str(&s))
                .transpose()
                .map_err(|e| eyre::eyre!("--food-id: {e}"))?;
            // Name → food_id is a luxury here; prompt callers to use
            // --food-id when they have multiple candidates.
            if food_id_uuid.is_none() {
                if let Some(name) = food.as_deref() {
                    food_id_uuid = Some(Uuid::parse_str(name).map_err(|_| {
                        eyre::eyre!("--food expects a UUID at the CLI; pass --food-id")
                    })?);
                }
            }
            let request = ConsumeFromPantryRequest {
                organization,
                food_id: food_id_uuid,
                product_id: product_id
                    .map(|s| Uuid::parse_str(&s))
                    .transpose()
                    .map_err(|e| eyre::eyre!("--product-id: {e}"))?,
                pantry_item_id: pantry_item_id
                    .map(|s| Uuid::parse_str(&s))
                    .transpose()
                    .map_err(|e| eyre::eyre!("--pantry-item-id: {e}"))?,
                amount,
                unit,
                notes,
            };
            let result = pantry
                .consume_from_pantry(request)
                .await
                .map_err(|e| eyre::eyre!("consume_from_pantry: {e}"))?;
            match result {
                Some(row) => println!("Remaining: {} {}  ({})", row.quantity, row.unit, row.id),
                None => println!("Depleted (row removed)."),
            }
        }
        PantryCommands::Update {
            pantry_item,
            quantity,
            unit,
            location,
            expiration,
            opened_at,
            min_stock,
            notes,
        } => {
            let id = Uuid::parse_str(&pantry_item)
                .map_err(|_| eyre::eyre!("pantry-item must be a UUID"))?;
            let location_id = match location {
                Some(l) => Some(
                    Uuid::parse_str(&l)
                        .map_err(|_| eyre::eyre!("--location must be a UUID at the CLI"))?,
                ),
                None => None,
            };
            let opened = match opened_at {
                Some(s) => Some(
                    chrono::DateTime::parse_from_rfc3339(&s)
                        .map_err(|e| eyre::eyre!("--opened-at: {e}"))?
                        .with_timezone(&chrono::Utc),
                ),
                None => None,
            };
            let patch = PantryItemPatch {
                quantity,
                unit,
                location_id,
                expiration_date: parse_optional_date(expiration.as_deref())?,
                opened_at: opened,
                min_stock,
                notes,
            };
            let row = pantry
                .update_pantry_item(id, patch)
                .await
                .map_err(|e| eyre::eyre!("update_pantry_item: {e}"))?;
            println!(
                "Updated {} → quantity {} {}",
                row.id, row.quantity, row.unit
            );
        }
        PantryCommands::Delete { pantry_item } => {
            let id = Uuid::parse_str(&pantry_item)
                .map_err(|_| eyre::eyre!("pantry-item must be a UUID"))?;
            pantry
                .delete_pantry_item(id)
                .await
                .map_err(|e| eyre::eyre!("delete_pantry_item: {e}"))?;
            println!("Deleted {id}");
        }
        PantryCommands::Expiring {
            organization,
            within_days,
            json,
        } => {
            let rows = pantry
                .expiring_soon(organization, within_days)
                .await
                .map_err(|e| eyre::eyre!("expiring_soon: {e}"))?;
            print_pantry_rows(&rows, json)?;
        }
        PantryCommands::LowStock { organization, json } => {
            let rows = pantry
                .low_stock(organization)
                .await
                .map_err(|e| eyre::eyre!("low_stock: {e}"))?;
            print_pantry_rows(&rows, json)?;
        }
    }
    Ok(())
}

fn print_pantry_rows(rows: &[task_core::pantry::PantryItemApi], json: bool) -> eyre::Result<()> {
    if json {
        println!("{}", serde_json::to_string_pretty(rows)?);
        return Ok(());
    }
    if rows.is_empty() {
        println!("(no pantry items)");
        return Ok(());
    }
    for r in rows {
        print_pantry_row(r);
    }
    Ok(())
}

fn print_pantry_row(r: &task_core::pantry::PantryItemApi) {
    let exp = r
        .expiration_date
        .map(|d| d.to_string())
        .unwrap_or_else(|| "—".to_string());
    let min = r
        .min_stock
        .map(|m| format!("min={m}"))
        .unwrap_or_else(|| "—".to_string());
    println!(
        "{}  {} {}  exp={}  {}  ({})",
        r.id, r.quantity, r.unit, exp, min, r.id
    );
}

// ── Substitutions ───────────────────────────────────────────────────

async fn run_substitution(
    client: &CookingServiceClient,
    food_client: &FoodServiceClient,
    command: SubstitutionCommands,
) -> eyre::Result<()> {
    match command {
        SubstitutionCommands::List { organization, json } => {
            let rows = client
                .list_substitutions(organization)
                .await
                .map_err(|e| eyre::eyre!("list_substitutions: {e}"))?;
            if json {
                println!("{}", serde_json::to_string_pretty(&rows)?);
            } else if rows.is_empty() {
                println!("(no substitutions)");
            } else {
                for s in &rows {
                    let bidir = if s.bidirectional { "↔" } else { "→" };
                    let note = s.conversion_note.as_deref().unwrap_or("");
                    println!(
                        "{}  {} {} {}  ratio={:.2}  conf={:.2}  {}",
                        s.id, s.from_food_id, bidir, s.to_food_id, s.ratio, s.confidence, note
                    );
                }
            }
        }
        SubstitutionCommands::Create {
            from,
            to,
            ratio,
            note,
            confidence,
            bidirectional,
            diet,
            context,
            organization,
        } => {
            let from_id = resolve_food_id(food_client, &from).await?;
            let to_id = resolve_food_id(food_client, &to).await?;
            let applies_when = build_applies_when(&diet, &context);
            let applies_when_json = if applies_when.as_object().is_some_and(|m| m.is_empty()) {
                None
            } else {
                Some(serde_json::to_string(&applies_when)?)
            };
            let api = client
                .create_substitution(CreateSubstitutionRequest {
                    from_food_id: from_id,
                    to_food_id: to_id,
                    ratio,
                    conversion_note: note,
                    applies_when_json,
                    confidence,
                    bidirectional,
                    organization,
                    created_by: None,
                })
                .await
                .map_err(|e| eyre::eyre!("create_substitution: {e}"))?;
            println!(
                "Created substitution {} ({} → {}, ratio {:.2}, conf {:.2})",
                api.id, api.from_food_id, api.to_food_id, api.ratio, api.confidence
            );
        }
        SubstitutionCommands::Delete { id } => {
            let parsed = Uuid::parse_str(&id)
                .map_err(|e| eyre::eyre!("invalid substitution id '{id}': {e}"))?;
            client
                .delete_substitution(parsed)
                .await
                .map_err(|e| eyre::eyre!("delete_substitution: {e}"))?;
            println!("Deleted substitution {parsed}");
        }
    }
    Ok(())
}

fn build_applies_when(diet: &[String], context: &[String]) -> serde_json::Value {
    let mut obj = serde_json::Map::new();
    if !diet.is_empty() {
        obj.insert(
            "dietary".to_string(),
            serde_json::Value::Array(
                diet.iter()
                    .map(|s| serde_json::Value::String(s.clone()))
                    .collect(),
            ),
        );
    }
    if !context.is_empty() {
        obj.insert(
            "context".to_string(),
            serde_json::Value::Array(
                context
                    .iter()
                    .map(|s| serde_json::Value::String(s.clone()))
                    .collect(),
            ),
        );
    }
    serde_json::Value::Object(obj)
}

fn print_substitution_suggestions(
    view: &task_core::service::SubstitutionSuggestionsView,
    json: bool,
) -> eyre::Result<()> {
    if json {
        let payload = serde_json::json!({
            "recipe_id": view.recipe_id,
            "recipe_name": view.recipe_name,
            "suggestions": serde_json::from_str::<serde_json::Value>(&view.suggestions_json)
                .unwrap_or(serde_json::Value::Array(Vec::new())),
            "warnings": view.warnings,
        });
        println!("{}", serde_json::to_string_pretty(&payload)?);
        return Ok(());
    }
    let suggestions: Vec<IngredientSuggestion> =
        serde_json::from_str(&view.suggestions_json).unwrap_or_default();
    println!("Substitutions for '{}':", view.recipe_name);
    for w in &view.warnings {
        println!("  warn: {w}");
    }
    if suggestions.is_empty() {
        println!("  (no flagged ingredients)");
        return Ok(());
    }
    for s in &suggestions {
        let qty = s
            .original_quantity
            .map(|q| format!("{q}"))
            .unwrap_or_else(|| "?".to_string());
        let unit = s.original_unit.as_deref().unwrap_or("");
        let reasons = if s.reasons.is_empty() {
            String::new()
        } else {
            format!(" [{}]", s.reasons.join(", "))
        };
        println!(
            "\nFor \"{}\" ({} {}){}:",
            s.ingredient_food_name,
            qty,
            unit,
            reasons.trim_end()
        );
        if s.suggestions.is_empty() {
            println!("  (no available swaps)");
            continue;
        }
        for r in &s.suggestions {
            let qty_str = r
                .suggested_quantity
                .map(|q| format!("{q}"))
                .unwrap_or_else(|| format!("{:.2}×", r.ratio));
            let note = r
                .conversion_note
                .as_deref()
                .map(|n| format!(" — {n}"))
                .unwrap_or_default();
            let inv = if r.is_inverse { " (inverse)" } else { "" };
            println!(
                "  → {} ({} {}){}{}  — confidence {:.2}, score {:.2}",
                r.to_food_name,
                qty_str,
                r.suggested_unit.as_deref().unwrap_or(""),
                note,
                inv,
                r.confidence,
                r.score
            );
        }
    }
    Ok(())
}

// ── Glossary integration ────────────────────────────────────────────
//
// `task cook recipe show` and `task cook session show` resolve
// `[[wikilink]]` references in step text against the cross-cutting
// `Glossary` catalog (scoped to `category = "cooking"` so audio terms
// don't bleed into recipe rendering). The wikilink resolution is a
// presentation concern only — when --json is set we leave the
// underlying step text as plain markdown but include a side-band
// `wikilinks` payload so consumers can render their own UIs.

const COOKING_GLOSSARY_CATEGORY: &str = "cooking";

/// Resolve `[[wikilink]]`s in the supplied step texts against the
/// glossary service. Returns one parsed JSON spans-payload + the
/// per-text rendered output. Any service error is logged as a warning
/// and the rendering falls back to the original text (rendering is
/// not load-bearing).
async fn resolve_step_wikilinks(
    remote: &RemoteVoxConfig,
    texts: &[String],
) -> Vec<(String, Vec<(String, String, String)>)> {
    let client = match remote.glossary().await {
        Ok(c) => c,
        Err(_) => return texts.iter().map(|t| (t.clone(), Vec::new())).collect(),
    };
    let mut out = Vec::with_capacity(texts.len());
    for text in texts {
        let view = match client
            .resolve_in_text(task_core::service::ResolveInTextRequest {
                text: text.clone(),
                organization: None,
                category: Some(COOKING_GLOSSARY_CATEGORY.to_string()),
            })
            .await
        {
            Ok(v) => v,
            Err(_) => {
                out.push((text.clone(), Vec::new()));
                continue;
            }
        };
        let parsed: serde_json::Value =
            serde_json::from_str(&view.spans_json).unwrap_or(serde_json::Value::Null);
        let mut concepts: Vec<(String, String, String)> = Vec::new();
        let mut spans: Vec<task_core::ResolvedWikilink> = Vec::new();
        if let Some(arr) = parsed.as_array() {
            for entry in arr {
                let span_obj = match entry.get("span") {
                    Some(s) => s,
                    None => continue,
                };
                let start = span_obj
                    .get("start")
                    .and_then(serde_json::Value::as_u64)
                    .unwrap_or(0) as usize;
                let end = span_obj
                    .get("end")
                    .and_then(serde_json::Value::as_u64)
                    .unwrap_or(0) as usize;
                let raw = span_obj
                    .get("raw")
                    .and_then(serde_json::Value::as_str)
                    .unwrap_or("")
                    .to_string();
                let slug = span_obj
                    .get("slug")
                    .and_then(serde_json::Value::as_str)
                    .unwrap_or("")
                    .to_string();
                let display = span_obj
                    .get("display")
                    .and_then(serde_json::Value::as_str)
                    .map(str::to_string);
                let target_id = entry
                    .get("target_id")
                    .and_then(serde_json::Value::as_str)
                    .and_then(|s| uuid::Uuid::parse_str(s).ok());
                if let Some(summary) = entry.get("term_summary") {
                    if let (Some(name), Some(slug_s)) = (
                        summary.get("name").and_then(serde_json::Value::as_str),
                        summary.get("slug").and_then(serde_json::Value::as_str),
                    ) {
                        let body = summary
                            .get("body_excerpt")
                            .and_then(serde_json::Value::as_str)
                            .unwrap_or("")
                            .to_string();
                        if !concepts.iter().any(|(_, s, _)| s == slug_s) {
                            concepts.push((name.to_string(), slug_s.to_string(), body));
                        }
                    }
                }
                spans.push(task_core::ResolvedWikilink {
                    span: task_core::WikilinkSpan {
                        start,
                        end,
                        raw,
                        slug,
                        display,
                    },
                    target_id,
                });
            }
        }
        let (rendered, _) = task_core::render_wikilinks_for_terminal(text, &spans);
        out.push((rendered, concepts));
    }
    out
}

async fn print_recipe_detail_with_glossary(
    detail: &RecipeWithDetails,
    json: bool,
    remote: Option<&RemoteVoxConfig>,
) -> eyre::Result<()> {
    if json {
        // JSON path is unchanged for backward compat.
        return print_recipe_detail(detail, json);
    }
    let r = &detail.recipe;
    println!("Recipe {}", r.id);
    println!("  name:    {}", r.name);
    println!("  slug:    {}", r.slug);
    if let Some(d) = &r.description {
        println!("  desc:    {d}");
    }
    if let Some(p) = r.prep_time_minutes {
        println!("  prep:    {p} min");
    }
    if let Some(c) = r.cook_time_minutes {
        println!("  cook:    {c} min");
    }
    if let Some(s) = r.servings {
        println!("  serves:  {s}");
    }
    if let Some(rt) = r.rating {
        println!("  rating:  {rt:.1}");
    }
    let ingredients: Vec<serde_json::Value> =
        serde_json::from_str(&detail.ingredients_json).unwrap_or_default();
    println!("  ingredients:");
    for ing in ingredients {
        let qty = ing
            .get("quantity")
            .and_then(|v| v.as_f64())
            .map(|n| format!("{n} "))
            .unwrap_or_default();
        let unit = ing
            .get("unit")
            .and_then(|v| v.as_str())
            .map(|s| format!("{s} "))
            .unwrap_or_default();
        let food = ing.get("food").and_then(|v| v.as_str()).unwrap_or("");
        println!("    - {qty}{unit}{food}");
    }
    let steps: Vec<serde_json::Value> =
        serde_json::from_str(&detail.steps_json).unwrap_or_default();
    let texts: Vec<String> = steps
        .iter()
        .map(|s| {
            s.get("text")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string()
        })
        .collect();
    let resolved = match remote {
        Some(r) => resolve_step_wikilinks(r, &texts).await,
        None => texts.iter().map(|t| (t.clone(), Vec::new())).collect(),
    };
    println!("  steps:");
    let mut all_concepts: Vec<(String, String, String)> = Vec::new();
    for (i, (rendered, concepts)) in resolved.iter().enumerate() {
        println!("    {}. {}", i + 1, rendered);
        for c in concepts {
            if !all_concepts.iter().any(|(_, s, _)| s == &c.1) {
                all_concepts.push(c.clone());
            }
        }
    }
    if !all_concepts.is_empty() {
        println!("\n  Concepts referenced:");
        for (name, _slug, body) in &all_concepts {
            let one_line = body.split(['.', '\n']).next().unwrap_or("").trim();
            if one_line.is_empty() {
                println!("    - {name}");
            } else {
                println!("    - {name}: {one_line}.");
            }
        }
    }
    Ok(())
}

async fn print_session_with_glossary(
    view: &CookingSessionView,
    json: bool,
    remote: Option<&RemoteVoxConfig>,
) -> eyre::Result<()> {
    if json {
        return print_session(view, json);
    }
    let s = &view.session;
    println!(
        "Session {}  recipe: {}  status: {}",
        s.id,
        s.recipe_name_snapshot,
        s.status.as_str()
    );
    if let Some(srv) = s.scaled_servings {
        println!("  servings: {srv}");
    }
    let total_ings = view.mise_en_place_json.matches("true").count()
        + view.mise_en_place_json.matches("false").count();
    let gathered = total_ings - view.ungathered_count as usize;
    println!("  mise en place: {} of {} gathered", gathered, total_ings);
    let steps: serde_json::Value =
        serde_json::from_str(&view.steps_json).unwrap_or(serde_json::Value::Array(vec![]));
    let total_steps = steps.as_array().map(|a| a.len()).unwrap_or(0);
    if s.current_step_index < 0 {
        println!("  Phase: mise en place (before step 1 of {total_steps})");
        return Ok(());
    }
    let idx = s.current_step_index as usize;
    let step_text = steps
        .get(idx)
        .and_then(|s| s.get("text"))
        .and_then(|t| t.as_str())
        .unwrap_or("")
        .to_string();
    let duration = steps
        .get(idx)
        .and_then(|s| s.get("duration_minutes"))
        .and_then(|d| d.as_u64());
    let resolved = match remote {
        Some(r) => resolve_step_wikilinks(r, std::slice::from_ref(&step_text)).await,
        None => vec![(step_text.clone(), Vec::new())],
    };
    let (rendered, concepts) = resolved
        .into_iter()
        .next()
        .unwrap_or((step_text.clone(), Vec::new()));
    println!(
        "  Step {} of {}: {}{}",
        idx + 1,
        total_steps,
        rendered,
        duration
            .map(|d| format!(" ({d} min timer)"))
            .unwrap_or_default()
    );
    if !concepts.is_empty() {
        println!("\n  Concepts referenced:");
        for (name, _slug, body) in &concepts {
            let one_line = body.split(['.', '\n']).next().unwrap_or("").trim();
            if one_line.is_empty() {
                println!("    - {name}");
            } else {
                println!("    - {name}: {one_line}.");
            }
        }
    }
    Ok(())
}
