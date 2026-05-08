//! Integration tests for the PantryService surface.

use chrono::{Duration, NaiveDate};
use sea_orm::EntityTrait;
use task_core::service::{
    AddToPantryRequest, ConsumeFromPantryRequest, CookingService,
    GenerateShoppingListFromMissingRequest, PantryListRequest, PantryService,
    SetMealPlanEntryRequest,
};
use task_core::service_impl::{
    CookingServiceDeps, CookingServiceImpl, PantryServiceDeps, PantryServiceImpl,
};
use task_db::seed::{demo_id, seed_demo_data};

const ORG_PERSONAL: &str = "personal";

fn pantry(db: &sea_orm::DatabaseConnection) -> PantryServiceImpl {
    PantryServiceImpl::new(PantryServiceDeps {
        db: db.clone(),
        openfoodfacts: None,
    })
}

#[tokio::test]
async fn seed_populates_pantry_items() {
    let db = task_db::init_memory().await.expect("init db");
    let s = seed_demo_data(&db).await.expect("seed");
    assert!(
        s.pantry_items_created >= 12,
        "pantry_items_created={}",
        s.pantry_items_created
    );
}

#[tokio::test]
async fn add_to_pantry_resolves_food_by_name() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = pantry(&db);

    let added = svc
        .add_to_pantry(AddToPantryRequest {
            organization: Some(ORG_PERSONAL.to_string()),
            food_name: Some("brown sugar".to_string()),
            quantity: 500.0,
            unit: "g".to_string(),
            ..Default::default()
        })
        .await
        .expect("add");
    assert!(added.food_id.is_some(), "food_id should be resolved");
    assert!(added.product_id.is_none());
}

#[tokio::test]
async fn add_to_pantry_resolves_barcode_via_cache() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = pantry(&db);

    // Bertolli EVOO barcode is cached by the seed.
    let added = svc
        .add_to_pantry(AddToPantryRequest {
            organization: Some(ORG_PERSONAL.to_string()),
            barcode: Some("0048500201497".to_string()),
            quantity: 500.0,
            unit: "ml".to_string(),
            ..Default::default()
        })
        .await
        .expect("add by barcode");
    assert!(added.product_id.is_some(), "product_id resolved from cache");
    assert!(added.food_id.is_some(), "food_id copied from product");
}

#[tokio::test]
async fn consume_from_pantry_decrements_then_deletes() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = pantry(&db);

    let pantry_item_id = demo_id("pantry:butter-fridge");
    let before = svc
        .get_pantry_item(pantry_item_id)
        .await
        .expect("get")
        .expect("present");
    assert!(before.quantity > 0.0);

    // Decrement partway.
    let after = svc
        .consume_from_pantry(ConsumeFromPantryRequest {
            organization: Some(ORG_PERSONAL.to_string()),
            pantry_item_id: Some(pantry_item_id),
            amount: 100.0,
            unit: "g".to_string(),
            ..Default::default()
        })
        .await
        .expect("consume")
        .expect("still present");
    assert!((after.quantity - (before.quantity - 100.0)).abs() < 1e-6);

    // Drain the rest.
    let drained = svc
        .consume_from_pantry(ConsumeFromPantryRequest {
            organization: Some(ORG_PERSONAL.to_string()),
            pantry_item_id: Some(pantry_item_id),
            amount: after.quantity + 5.0,
            unit: "g".to_string(),
            ..Default::default()
        })
        .await
        .expect("drain");
    assert!(drained.is_none(), "row should be deleted at zero");
    assert!(
        svc.get_pantry_item(pantry_item_id)
            .await
            .expect("get")
            .is_none()
    );
}

#[tokio::test]
async fn expiring_soon_returns_within_window_sorted() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = pantry(&db);

    let rows = svc
        .expiring_soon(Some(ORG_PERSONAL.to_string()), 7)
        .await
        .expect("expiring");
    assert!(!rows.is_empty(), "seeded eggs row expires in 5 days");
    let mut prev: Option<NaiveDate> = None;
    for r in &rows {
        let date = r.expiration_date.expect("filter sets non-null");
        if let Some(p) = prev {
            assert!(date >= p, "expiring_soon must be sorted ascending");
        }
        prev = Some(date);
    }
}

#[tokio::test]
async fn low_stock_returns_below_threshold_only() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = pantry(&db);

    let rows = svc
        .low_stock(Some(ORG_PERSONAL.to_string()))
        .await
        .expect("low_stock");
    assert!(!rows.is_empty());
    for r in &rows {
        let threshold = r.min_stock.expect("non-None for low_stock results");
        assert!(
            r.quantity <= threshold,
            "row {:?} above threshold ({} > {})",
            r.id,
            r.quantity,
            threshold
        );
    }
}

#[tokio::test]
async fn list_pantry_filters_low_stock() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = pantry(&db);

    let all = svc
        .list_pantry_items(PantryListRequest {
            organization: Some(ORG_PERSONAL.to_string()),
            ..Default::default()
        })
        .await
        .expect("all");
    let low = svc
        .list_pantry_items(PantryListRequest {
            organization: Some(ORG_PERSONAL.to_string()),
            low_stock_only: true,
            ..Default::default()
        })
        .await
        .expect("low");
    assert!(low.len() < all.len());
    assert!(!low.is_empty());
}

#[tokio::test]
async fn recipes_i_can_cook_distinguishes_full_and_partial() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = pantry(&db);
    let matches = svc
        .recipes_i_can_cook(Some(ORG_PERSONAL.to_string()))
        .await
        .expect("matches");
    assert!(!matches.is_empty(), "should consider some recipes");

    // Every match has at least one food_id-linked ingredient (we filter).
    for m in &matches {
        assert!(m.total_ingredients > 0);
        assert!(m.matched_ingredients <= m.total_ingredients);
    }

    // At least one recipe with a missing-ingredient (partial) entry.
    let partial = matches
        .iter()
        .any(|m| m.matched_ingredients < m.total_ingredients);
    assert!(partial, "want at least one partial match");
}

#[tokio::test]
async fn generate_shopping_list_from_missing_drops_pantry_covered() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let cook = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let svc = pantry(&db);

    // Schedule a meal plan that uses the smashburger recipe, then
    // generate two lists: one with the standard generator and one with
    // the missing-from-pantry filter. Pantry should drop coverage rows.
    let recipes = cook
        .list_recipes(Some(ORG_PERSONAL.to_string()))
        .await
        .expect("recipes");
    let target = recipes
        .iter()
        .find(|r| r.name.to_lowercase().contains("smashburger"))
        .expect("smashburger recipe");

    let date = chrono::Local::now().date_naive() + Duration::days(30);
    cook.set_meal_plan_entry(SetMealPlanEntryRequest {
        date,
        meal_type: "Dinner".to_string(),
        recipe_id: Some(target.id),
        organization: Some(ORG_PERSONAL.to_string()),
        ..Default::default()
    })
    .await
    .expect("set entry");

    let list = cook
        .create_shopping_list(
            "pantry-missing-test".to_string(),
            Some(ORG_PERSONAL.to_string()),
        )
        .await
        .expect("create list");
    let list_id = list.id;
    use task_core::shopping_list;

    let returned = svc
        .generate_shopping_list_from_missing(GenerateShoppingListFromMissingRequest {
            list_id,
            organization: Some(ORG_PERSONAL.to_string()),
            from_date: date,
            to_date: date,
        })
        .await
        .expect("generate");
    assert_eq!(returned, list_id);

    // The list should have at least one item (smashburger has
    // ingredients not in the pantry, e.g. brioche bun, pickles).
    let items = shopping_list::ItemEntity::find()
        .all(&db)
        .await
        .expect("items")
        .into_iter()
        .filter(|i| i.list_id == list_id)
        .collect::<Vec<_>>();
    assert!(!items.is_empty(), "expected some not-in-pantry items");

    // None of the items should be eggs / olive oil / kosher salt /
    // ground beef — those are in the pantry.
    for i in &items {
        let lc = i.food.to_lowercase();
        assert!(
            !lc.contains("kosher salt") && !lc.contains("olive oil"),
            "pantry should have covered {lc}"
        );
    }
}
