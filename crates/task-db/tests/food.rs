//! Integration tests for the Food + FoodProduct foundation.

use sea_orm::{ColumnTrait, EntityTrait, QueryFilter};
use task_core::nutrition::NutritionFacts;
use task_core::recipe_ingredient;
use task_core::service::{
    CookingService, CreateFoodProductRequest, CreateFoodRequest, CreateRecipeRequest, FoodService,
};
use task_core::service_impl::{
    CookingServiceDeps, CookingServiceImpl, FoodServiceDeps, FoodServiceImpl,
};
use task_db::seed::seed_demo_data;
use uuid::Uuid;

const ORG_PERSONAL: &str = "personal";

#[tokio::test]
async fn seed_populates_food_catalog() {
    let db = task_db::init_memory().await.expect("init db");
    let s = seed_demo_data(&db).await.expect("seed");
    assert!(s.foods_created >= 30, "foods_created={}", s.foods_created);
    assert!(
        s.food_products_created >= 3,
        "food_products_created={}",
        s.food_products_created
    );
}

#[tokio::test]
async fn find_food_by_name_hits_canonical_and_alias_in_org() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FoodServiceImpl::new(FoodServiceDeps {
        db: db.clone(),
        openfoodfacts: None,
    });

    // Canonical name.
    let hit = svc
        .find_food_by_name(Some(ORG_PERSONAL.to_string()), "olive oil".into())
        .await
        .expect("by name");
    assert!(hit.is_some());
    assert_eq!(hit.unwrap().name, "olive oil");

    // Alias on the seeded olive-oil row ("evoo").
    let hit = svc
        .find_food_by_name(Some(ORG_PERSONAL.to_string()), "EVOO".into())
        .await
        .expect("alias hit");
    assert!(hit.is_some(), "alias EVOO should resolve to olive oil");

    // Wrong org scope — no match.
    let miss = svc
        .find_food_by_name(Some("nope".to_string()), "olive oil".into())
        .await
        .expect("scope");
    assert!(miss.is_none());
}

#[tokio::test]
async fn create_food_dedups_aliases_case_insensitively() {
    let db = task_db::init_memory().await.expect("init db");
    let svc = FoodServiceImpl::new(FoodServiceDeps {
        db: db.clone(),
        openfoodfacts: None,
    });
    let created = svc
        .create_food(CreateFoodRequest {
            name: "Tahini".to_string(),
            aliases: vec![
                "Sesame Paste".to_string(),
                "sesame paste".to_string(),
                "SESAME PASTE".to_string(),
                "tahina".to_string(),
            ],
            category: Some("pantry-staple".to_string()),
            default_unit: Some("tbsp".to_string()),
            organization: Some(ORG_PERSONAL.to_string()),
            nutrition_json: None,
            notes: None,
            created_by: Some("cody".to_string()),
        })
        .await
        .expect("create");
    assert_eq!(created.aliases.len(), 2);
    assert!(created.aliases.iter().any(|a| a == "sesame paste"));
    assert!(created.aliases.iter().any(|a| a == "tahina"));
}

#[tokio::test]
async fn add_food_alias_is_idempotent() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FoodServiceImpl::new(FoodServiceDeps {
        db: db.clone(),
        openfoodfacts: None,
    });
    // The seeded olive-oil already has alias "evoo".
    let oil = svc
        .find_food_by_name(Some(ORG_PERSONAL.to_string()), "olive oil".into())
        .await
        .expect("find")
        .expect("present");
    let before = oil.aliases.len();

    let after_dup = svc
        .add_food_alias(oil.id, "EVOO".to_string())
        .await
        .expect("dup alias");
    assert_eq!(
        after_dup.aliases.len(),
        before,
        "duplicate alias should be a no-op"
    );

    let after_new = svc
        .add_food_alias(oil.id, "Liquid Gold".to_string())
        .await
        .expect("new alias");
    assert_eq!(after_new.aliases.len(), before + 1);
    assert!(after_new.aliases.iter().any(|a| a == "liquid gold"));
}

#[tokio::test]
async fn auto_match_links_recipe_ingredients_on_create() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let cook = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });

    let ingredients_json = serde_json::to_string(&serde_json::json!([
        {"food": "olive oil", "quantity": 2.0, "unit": "tbsp"},
        {"food": "spaghetti", "quantity": 1.0, "unit": "lb"},
        {"food": "completely unknown blursed chimera", "quantity": 1.0},
    ]))
    .unwrap();
    let detail = cook
        .create_recipe(CreateRecipeRequest {
            name: "Auto-link Probe".to_string(),
            organization: Some(ORG_PERSONAL.to_string()),
            ingredients_json,
            steps_json: "[]".to_string(),
            ..Default::default()
        })
        .await
        .expect("create");

    let rows = recipe_ingredient::Entity::find()
        .filter(recipe_ingredient::Column::RecipeId.eq(detail.recipe.id))
        .all(&db)
        .await
        .expect("ingredients");
    let mut by_food = std::collections::HashMap::new();
    for row in &rows {
        by_food.insert(row.food.clone(), row.food_id);
    }
    assert!(by_food.get("olive oil").unwrap().is_some());
    assert!(by_food.get("spaghetti").unwrap().is_some());
    assert!(
        by_food
            .get("completely unknown blursed chimera")
            .unwrap()
            .is_none()
    );
}

#[tokio::test]
async fn link_recipe_ingredient_populates_food_id() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let cook = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let svc = FoodServiceImpl::new(FoodServiceDeps {
        db: db.clone(),
        openfoodfacts: None,
    });

    let detail = cook
        .create_recipe(CreateRecipeRequest {
            name: "Link Probe".to_string(),
            organization: Some(ORG_PERSONAL.to_string()),
            ingredients_json: r#"[{"food":"frobnitz","quantity":1.0}]"#.to_string(),
            steps_json: "[]".to_string(),
            ..Default::default()
        })
        .await
        .expect("create");
    let row = recipe_ingredient::Entity::find()
        .filter(recipe_ingredient::Column::RecipeId.eq(detail.recipe.id))
        .one(&db)
        .await
        .expect("ing")
        .expect("present");
    assert!(row.food_id.is_none(), "no canonical match expected");

    // Create a Food and link it manually.
    let food = svc
        .create_food(CreateFoodRequest {
            name: "frobnitz".to_string(),
            organization: Some(ORG_PERSONAL.to_string()),
            ..Default::default()
        })
        .await
        .expect("food");
    svc.link_recipe_ingredient(row.id, food.id)
        .await
        .expect("link");
    let after = recipe_ingredient::Entity::find_by_id(row.id)
        .one(&db)
        .await
        .expect("reload")
        .expect("present");
    assert_eq!(after.food_id, Some(food.id));
}

#[tokio::test]
async fn seed_auto_links_majority_of_recipe_ingredients() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let rows = recipe_ingredient::Entity::find()
        .all(&db)
        .await
        .expect("rows");
    let total = rows.len();
    let linked = rows.iter().filter(|r| r.food_id.is_some()).count();
    assert!(total > 0);
    assert!(
        linked * 2 > total,
        ">50% should auto-link: {linked}/{total}"
    );
}

#[tokio::test]
async fn nutrition_facts_scale_and_sum() {
    let oil = NutritionFacts {
        kcal_per_100g: Some(884.0),
        fat_g: Some(100.0),
        ..Default::default()
    };
    let scaled = oil.scale_to_grams(10.0);
    assert!((scaled.kcal_per_100g.unwrap() - 88.4).abs() < 1e-6);
    assert!((scaled.fat_g.unwrap() - 10.0).abs() < 1e-6);

    let egg = NutritionFacts {
        kcal_per_100g: Some(155.0),
        protein_g: Some(13.0),
        ..Default::default()
    };
    let combined = scaled.sum(&egg);
    assert!((combined.kcal_per_100g.unwrap() - (88.4 + 155.0)).abs() < 1e-6);
    assert_eq!(combined.protein_g, Some(13.0));
    assert_eq!(combined.source.as_deref(), Some("calculated"));
}

#[tokio::test]
async fn product_lookup_by_barcode_is_org_scoped() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = FoodServiceImpl::new(FoodServiceDeps {
        db: db.clone(),
        openfoodfacts: None,
    });

    // Create a product with a barcode then look it up.
    let foods = svc
        .list_foods(Some(ORG_PERSONAL.to_string()))
        .await
        .expect("list");
    let oil = foods.iter().find(|f| f.name == "olive oil").expect("oil");
    let _ = svc
        .create_food_product(CreateFoodProductRequest {
            food_id: oil.id,
            barcode: Some("0123456789012".to_string()),
            brand: Some("Kirkland".to_string()),
            name: "Kirkland Organic EVOO 1L".to_string(),
            source: "manual".to_string(),
            organization: Some(ORG_PERSONAL.to_string()),
            ..Default::default()
        })
        .await
        .expect("product");

    let hit = svc
        .get_food_product_by_barcode(Some(ORG_PERSONAL.to_string()), "0123456789012".to_string())
        .await
        .expect("lookup");
    assert!(hit.is_some());
    assert_eq!(hit.unwrap().brand.as_deref(), Some("Kirkland"));

    // Different org → no hit.
    let miss = svc
        .get_food_product_by_barcode(Some("other-org".to_string()), "0123456789012".to_string())
        .await
        .expect("scope");
    assert!(miss.is_none());

    // Reference Uuid imported to silence unused warnings.
    let _ = Uuid::nil();
}
