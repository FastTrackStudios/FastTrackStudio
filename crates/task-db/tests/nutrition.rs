//! Integration tests for the NutritionService + nutrition aggregation.

use chrono::Duration;
use task_core::nutrition::{IngredientNutritionInput, NutritionFacts, aggregate_recipe_nutrition};
use task_core::service::{
    CookingService, LogFoodRequest, LogListRequest, MarkMealPlanCookedRequest, NutritionService,
    SetMealPlanEntryRequest,
};
use task_core::service_impl::{
    CookingServiceDeps, CookingServiceImpl, NutritionServiceDeps, NutritionServiceImpl,
};
use task_db::seed::seed_demo_data;
use uuid::Uuid;

const ORG_PERSONAL: &str = "personal";

fn nutrition_svc(db: &sea_orm::DatabaseConnection) -> NutritionServiceImpl {
    NutritionServiceImpl::new(NutritionServiceDeps {
        db: db.clone(),
        openfoodfacts: None,
    })
}

fn cooking_svc(db: &sea_orm::DatabaseConnection) -> CookingServiceImpl {
    CookingServiceImpl::new(CookingServiceDeps { db: db.clone() })
}

#[tokio::test]
async fn seed_populates_food_logs() {
    let db = task_db::init_memory().await.expect("init db");
    let s = seed_demo_data(&db).await.expect("seed");
    assert!(
        s.food_logs_created >= 12,
        "food_logs_created={}",
        s.food_logs_created
    );
}

#[test]
fn aggregator_three_ingredient_sum_with_per_serving() {
    fn nf(kcal: f64, protein: f64, carbs: f64, fat: f64) -> NutritionFacts {
        NutritionFacts {
            kcal_per_100g: Some(kcal),
            protein_g: Some(protein),
            carbs_g: Some(carbs),
            fat_g: Some(fat),
            ..Default::default()
        }
    }
    let ings = vec![
        IngredientNutritionInput {
            food_id: Some(Uuid::nil()),
            food_name: "chicken".into(),
            quantity: Some(200.0),
            unit: Some("g".into()),
            nutrition_per_100g: Some(nf(165.0, 31.0, 0.0, 3.6)),
        },
        IngredientNutritionInput {
            food_id: Some(Uuid::nil()),
            food_name: "rice".into(),
            quantity: Some(150.0),
            unit: Some("g".into()),
            nutrition_per_100g: Some(nf(130.0, 2.7, 28.0, 0.3)),
        },
        IngredientNutritionInput {
            food_id: Some(Uuid::nil()),
            food_name: "olive oil".into(),
            quantity: Some(15.0),
            unit: Some("g".into()),
            nutrition_per_100g: Some(nf(884.0, 0.0, 0.0, 100.0)),
        },
    ];
    let agg = aggregate_recipe_nutrition(Some(2), &ings);
    let expected = 165.0 * 2.0 + 130.0 * 1.5 + 884.0 * 0.15;
    let got = agg.total.kcal_per_100g.unwrap();
    assert!((got - expected).abs() < 1e-3);
    let per_serv = agg.per_serving.unwrap().kcal_per_100g.unwrap();
    assert!((per_serv * 2.0 - got).abs() < 1e-3);
    assert!(agg.warnings.is_empty());
}

#[tokio::test]
async fn recompute_recipe_nutrition_persists_summary_idempotent() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = cooking_svc(&db);

    let recipes = svc
        .list_recipes(Some(ORG_PERSONAL.to_string()))
        .await
        .expect("list recipes");
    let recipe = recipes.first().expect("at least one demo recipe");

    let view1 = svc
        .recompute_recipe_nutrition(recipe.id)
        .await
        .expect("recompute 1");
    assert_eq!(view1.recipe_id, recipe.id);
    assert!(!view1.total_json.is_empty());

    let view2 = svc
        .recompute_recipe_nutrition(recipe.id)
        .await
        .expect("recompute 2");
    assert_eq!(view1.total_json, view2.total_json);
}

#[tokio::test]
async fn log_food_resolves_food_by_name_and_snapshots_nutrition() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = nutrition_svc(&db);
    let today = chrono::Local::now().date_naive();

    let row = svc
        .log_food(LogFoodRequest {
            date: today,
            meal_type: "breakfast".to_string(),
            organization: Some(ORG_PERSONAL.to_string()),
            food_name: Some("eggs".to_string()),
            quantity: 100.0,
            unit: "g".to_string(),
            ..Default::default()
        })
        .await
        .expect("log_food");
    assert!(row.food_id.is_some(), "should resolve food_id");
    assert!((row.quantity_grams - 100.0).abs() < 1e-6);
}

#[tokio::test]
async fn daily_totals_sums_rows_for_a_date() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = nutrition_svc(&db);
    let today = chrono::Local::now().date_naive();
    let totals = svc
        .daily_totals(Some(ORG_PERSONAL.to_string()), today)
        .await
        .expect("daily_totals");
    // Demo seeds at least two log rows on day 0.
    assert!(totals.log_count >= 2, "log_count={}", totals.log_count);
    assert!(totals.kcal > 0.0, "kcal={}", totals.kcal);
}

#[tokio::test]
async fn weekly_summary_returns_seven_days() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = nutrition_svc(&db);
    let today = chrono::Local::now().date_naive();
    let from = today - Duration::days(6);
    let summary = svc
        .weekly_summary(Some(ORG_PERSONAL.to_string()), from)
        .await
        .expect("weekly_summary");
    assert_eq!(summary.days.len(), 7);
}

#[tokio::test]
async fn list_log_returns_seeded_rows() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = nutrition_svc(&db);
    let today = chrono::Local::now().date_naive();
    let from = today - Duration::days(6);
    let rows = svc
        .list_log(LogListRequest {
            organization: Some(ORG_PERSONAL.to_string()),
            from_date: from,
            to_date: today,
        })
        .await
        .expect("list_log");
    assert!(rows.len() >= 12, "log row count = {}", rows.len());
}

#[tokio::test]
async fn mark_meal_plan_cooked_for_recipe_creates_food_log() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let cooking = cooking_svc(&db);

    // Pick any recipe and assign it to a meal-plan slot far in the
    // future so we don't collide with the seeded slots.
    let recipes = cooking
        .list_recipes(Some(ORG_PERSONAL.to_string()))
        .await
        .expect("list recipes");
    let recipe_id = recipes.first().unwrap().id;
    let date = chrono::Local::now().date_naive() + Duration::days(30);

    let entry = cooking
        .set_meal_plan_entry(SetMealPlanEntryRequest {
            date,
            meal_type: "lunch".to_string(),
            organization: Some(ORG_PERSONAL.to_string()),
            recipe_id: Some(recipe_id),
            servings_planned: Some(2),
            ..Default::default()
        })
        .await
        .expect("set_meal_plan_entry");

    let log_ids = cooking
        .mark_meal_plan_cooked(MarkMealPlanCookedRequest {
            meal_plan_entry_id: entry.id,
            servings_consumed: Some(1),
            created_by: Some("cody".into()),
        })
        .await
        .expect("mark_meal_plan_cooked");
    assert_eq!(log_ids.len(), 1);

    let nut = nutrition_svc(&db);
    let rows = nut
        .list_log(LogListRequest {
            organization: Some(ORG_PERSONAL.to_string()),
            from_date: date,
            to_date: date,
        })
        .await
        .expect("list_log");
    assert!(rows.iter().any(|r| Some(recipe_id) == r.recipe_id));
}

#[tokio::test]
async fn mark_meal_plan_cooked_for_freeform_creates_macro_less_log() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let cooking = cooking_svc(&db);
    let date = chrono::Local::now().date_naive() + Duration::days(31);

    let entry = cooking
        .set_meal_plan_entry(SetMealPlanEntryRequest {
            date,
            meal_type: "dinner".to_string(),
            organization: Some(ORG_PERSONAL.to_string()),
            recipe_id: None,
            title: Some("Takeout sushi".to_string()),
            ..Default::default()
        })
        .await
        .expect("set_meal_plan_entry");

    let ids = cooking
        .mark_meal_plan_cooked(MarkMealPlanCookedRequest {
            meal_plan_entry_id: entry.id,
            ..Default::default()
        })
        .await
        .expect("mark_meal_plan_cooked");
    assert_eq!(ids.len(), 1);

    let nut = nutrition_svc(&db);
    let rows = nut
        .list_log(LogListRequest {
            organization: Some(ORG_PERSONAL.to_string()),
            from_date: date,
            to_date: date,
        })
        .await
        .expect("list_log");
    let row = rows
        .iter()
        .find(|r| r.meal_plan_entry_id == Some(entry.id))
        .expect("created freeform log row");
    assert!(row.kcal.is_none());
    assert!(row.recipe_id.is_none());
    assert_eq!(row.food_name, "Takeout sushi");
}
