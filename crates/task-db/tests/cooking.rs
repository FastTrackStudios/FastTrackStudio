//! Integration tests for `CookingService`.

use chrono::Duration;
use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
use task_core::recipe::{RecipeIngredientSpec, RecipeStepSpec};
use task_core::service::{
    AddShoppingItemRequest, CookingService, CreateRecipeRequest, GenerateShoppingListRequest,
    MealPlanRangeRequest, SetMealPlanEntryRequest,
};
use task_core::service_impl::{CookingServiceDeps, CookingServiceImpl};
use task_core::{recipe_ingredient, shopping_list};
use task_db::seed::{DEMO_NAMESPACE, seed_demo_data};
use uuid::Uuid;

fn demo_id(key: &str) -> Uuid {
    Uuid::new_v5(&DEMO_NAMESPACE, key.as_bytes())
}

#[tokio::test]
async fn seed_populates_cooking_fixtures() {
    let db = task_db::init_memory().await.expect("init db");
    let s = seed_demo_data(&db).await.expect("seed");
    assert!(s.recipes_created >= 6, "recipes={}", s.recipes_created);
    assert!(
        s.cookbooks_created >= 1,
        "cookbooks={}",
        s.cookbooks_created
    );
    assert!(
        s.meal_plan_entries_created >= 7,
        "meal_plan={}",
        s.meal_plan_entries_created
    );
    assert!(
        s.shopping_list_items_created > 0,
        "items={}",
        s.shopping_list_items_created
    );
}

#[tokio::test]
async fn list_recipes_returns_seeded_recipes_ordered_by_name() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });

    let recipes = svc.list_recipes(None).await.expect("list");
    assert!(recipes.len() >= 6, "got {} recipes", recipes.len());
    for window in recipes.windows(2) {
        assert!(
            window[0].name <= window[1].name,
            "not sorted: {:?}",
            recipes.iter().map(|r| &r.name).collect::<Vec<_>>()
        );
    }
}

#[tokio::test]
async fn get_recipe_returns_ingredients_and_steps_in_sequence_order() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });

    let id = demo_id("recipe:weeknight-carbonara");
    let detail = svc
        .get_recipe(id)
        .await
        .expect("get_recipe")
        .expect("present");

    let ingredients: Vec<serde_json::Value> =
        serde_json::from_str(&detail.ingredients_json).expect("ingredients json");
    assert!(
        ingredients.len() >= 4,
        "want >=4 ingredients, got {}",
        ingredients.len()
    );
    let mut prev = 0u64;
    for ing in &ingredients {
        let seq = ing.get("sequence").and_then(|v| v.as_u64()).unwrap();
        assert!(seq >= prev, "ingredient sequence not monotonic");
        prev = seq;
    }

    let steps: Vec<serde_json::Value> =
        serde_json::from_str(&detail.steps_json).expect("steps json");
    assert!(steps.len() >= 3, "want >=3 steps, got {}", steps.len());
}

#[tokio::test]
async fn create_recipe_round_trips_children() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });

    let ingredients = vec![
        RecipeIngredientSpec {
            food: "flour".into(),
            quantity: Some(2.0),
            unit: Some("cup".into()),
            ..Default::default()
        },
        RecipeIngredientSpec {
            food: "milk".into(),
            quantity: Some(1.0),
            unit: Some("cup".into()),
            ..Default::default()
        },
        RecipeIngredientSpec {
            food: "egg".into(),
            quantity: Some(1.0),
            ..Default::default()
        },
    ];
    let steps = vec![
        RecipeStepSpec {
            text: "Mix dry".into(),
            ..Default::default()
        },
        RecipeStepSpec {
            text: "Add wet, whisk".into(),
            ..Default::default()
        },
    ];

    let detail = svc
        .create_recipe(CreateRecipeRequest {
            name: "Test Pancakes".into(),
            description: None,
            organization: Some("personal".into()),
            prep_time_minutes: Some(5),
            cook_time_minutes: Some(10),
            servings: Some(2),
            source_url: None,
            created_by: Some("cody".into()),
            ingredients_json: serde_json::to_string(&ingredients).unwrap(),
            steps_json: serde_json::to_string(&steps).unwrap(),
        })
        .await
        .expect("create");

    assert_eq!(detail.recipe.name, "Test Pancakes");
    let id = detail.recipe.id;
    let reloaded = svc.get_recipe(id).await.expect("reload").expect("present");
    let ing_back: Vec<serde_json::Value> =
        serde_json::from_str(&reloaded.ingredients_json).unwrap();
    let step_back: Vec<serde_json::Value> = serde_json::from_str(&reloaded.steps_json).unwrap();
    assert_eq!(ing_back.len(), 3);
    assert_eq!(step_back.len(), 2);
}

#[tokio::test]
async fn rate_recipe_validates_range() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let id = demo_id("recipe:greek-salad");

    assert!(svc.rate_recipe(id, 6.0).await.is_err(), "6.0 should fail");
    let ok = svc.rate_recipe(id, 4.5).await.expect("4.5");
    assert_eq!(ok.rating, Some(4.5));
}

#[tokio::test]
async fn set_meal_plan_entry_upserts_on_slot_uniqueness() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });

    let date = chrono::Local::now().date_naive() + Duration::days(30);
    let recipe_a = demo_id("recipe:greek-salad");
    let recipe_b = demo_id("recipe:weeknight-carbonara");

    let first = svc
        .set_meal_plan_entry(SetMealPlanEntryRequest {
            date,
            meal_type: "dinner".into(),
            organization: Some("personal".into()),
            recipe_id: Some(recipe_a),
            title: None,
            servings_planned: None,
            notes: None,
            created_by: None,
        })
        .await
        .expect("first");

    let second = svc
        .set_meal_plan_entry(SetMealPlanEntryRequest {
            date,
            meal_type: "dinner".into(),
            organization: Some("personal".into()),
            recipe_id: Some(recipe_b),
            title: None,
            servings_planned: None,
            notes: None,
            created_by: None,
        })
        .await
        .expect("second");

    assert_eq!(first.id, second.id, "should upsert into same row");
    assert_eq!(second.recipe_id, Some(recipe_b));
}

#[tokio::test]
async fn generate_from_meal_plan_yields_one_item_per_ingredient() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });

    // Seed plants its own meal-plan entries spanning today..+6. Plant a
    // dedicated week starting in the future so we control the range and
    // know exactly which recipes are included.
    let base_date = chrono::Local::now().date_naive() + Duration::days(60);
    let recipe_a = demo_id("recipe:greek-salad");
    let recipe_b = demo_id("recipe:smashburger");

    svc.set_meal_plan_entry(SetMealPlanEntryRequest {
        date: base_date,
        meal_type: "lunch".into(),
        organization: Some("personal-test".into()),
        recipe_id: Some(recipe_a),
        title: None,
        servings_planned: None,
        notes: None,
        created_by: None,
    })
    .await
    .expect("plant lunch");
    svc.set_meal_plan_entry(SetMealPlanEntryRequest {
        date: base_date,
        meal_type: "dinner".into(),
        organization: Some("personal-test".into()),
        recipe_id: Some(recipe_b),
        title: None,
        servings_planned: None,
        notes: None,
        created_by: None,
    })
    .await
    .expect("plant dinner");

    // Count expected ingredients (non-section) for the two recipes.
    let mut expected = 0usize;
    for rid in [recipe_a, recipe_b] {
        let ings = recipe_ingredient::Entity::find()
            .filter(recipe_ingredient::Column::RecipeId.eq(rid))
            .all(&db)
            .await
            .expect("ings");
        expected += ings.iter().filter(|i| !i.is_section).count();
    }
    assert!(expected > 0, "expected > 0 ingredients across recipes");

    let list = svc
        .create_shopping_list("Test Generate".into(), Some("personal-test".into()))
        .await
        .expect("create list");

    svc.generate_from_meal_plan(GenerateShoppingListRequest {
        list_id: list.id,
        organization: Some("personal-test".into()),
        from_date: base_date,
        to_date: base_date,
    })
    .await
    .expect("generate");

    let detail = svc
        .get_shopping_list(list.id)
        .await
        .expect("get list")
        .expect("present");
    let items: Vec<serde_json::Value> =
        serde_json::from_str(&detail.items_json).expect("items json");
    assert_eq!(
        items.len(),
        expected,
        "items={}, expected={}",
        items.len(),
        expected
    );
}

#[tokio::test]
async fn check_item_flips_bool() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });

    let list = svc
        .create_shopping_list("Test Check".into(), Some("personal-test".into()))
        .await
        .expect("create list");
    svc.add_shopping_list_item(AddShoppingItemRequest {
        list_id: list.id,
        food: "milk".into(),
        quantity: Some(1.0),
        unit: Some("gal".into()),
        note: None,
        label: None,
    })
    .await
    .expect("add");

    let items = shopping_list::ItemEntity::find()
        .filter(shopping_list::ItemColumn::ListId.eq(list.id))
        .all(&db)
        .await
        .expect("load items");
    assert_eq!(items.len(), 1);
    let item = &items[0];
    assert!(!item.checked);

    svc.check_item(item.id, true).await.expect("check");
    let after = shopping_list::ItemEntity::find_by_id(item.id)
        .one(&db)
        .await
        .expect("reload")
        .expect("present");
    assert!(after.checked);

    svc.check_item(item.id, false).await.expect("uncheck");
    let after = shopping_list::ItemEntity::find_by_id(item.id)
        .one(&db)
        .await
        .expect("reload")
        .expect("present");
    assert!(!after.checked);
}

#[tokio::test]
async fn list_meal_plan_filters_by_range() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });

    let today = chrono::Local::now().date_naive();
    let entries = svc
        .list_meal_plan(MealPlanRangeRequest {
            organization: Some("personal".into()),
            from_date: today,
            to_date: today + Duration::days(6),
        })
        .await
        .expect("list");
    assert!(
        entries.len() >= 7,
        "want >= 7 entries, got {}",
        entries.len()
    );
}
