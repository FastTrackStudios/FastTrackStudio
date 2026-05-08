//! `task cook` — cooking / meal-prep workflow commands.
//!
//! Mealie-style surface: recipes (CRUD + rate + made), cookbooks
//! (collections + add/remove), meal plans (per-day per-meal-type slot),
//! shopping lists (manual or generated from a meal-plan range).

use chrono::NaiveDate;
use clap::{Args, Subcommand};
use task_core::recipe::{RecipeApi, RecipeIngredientSpec, RecipeStepSpec};
use task_core::service::{
    AddShoppingItemRequest, CookbookWithRecipes, CookingServiceClient, CreateRecipeRequest,
    GenerateShoppingListRequest, ImportRecipeRequest, MealPlanRangeRequest, RecipeWithDetails,
    SetMealPlanEntryRequest, ShoppingListWithItems,
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
    let client = remote.cooking().await?;
    match command {
        CookCommands::Recipe { command } => run_recipe(&client, actor, command).await,
        CookCommands::Cookbook { command } => run_cookbook(&client, command).await,
        CookCommands::Plan { command } => run_plan(&client, actor, command).await,
        CookCommands::Shop { command } => run_shop(&client, command).await,
    }
}

// ── Recipe handlers ─────────────────────────────────────────────────

async fn run_recipe(
    client: &CookingServiceClient,
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
            print_recipe_detail(&detail, json)?;
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
    }
    Ok(())
}

// ── Shop handlers ───────────────────────────────────────────────────

async fn run_shop(client: &CookingServiceClient, command: ShopCommands) -> eyre::Result<()> {
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
