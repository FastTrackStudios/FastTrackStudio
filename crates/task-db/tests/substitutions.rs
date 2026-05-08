//! Integration tests for the substitution catalog and the
//! `suggest_substitutions` ranking algorithm.

use sea_orm::ActiveValue::Set;
use sea_orm::{ActiveModelTrait, EntityTrait};
use task_core::property::JsonObject;
use task_core::service::{
    CookingService, CreateSubstitutionRequest, IngredientSuggestion, SuggestSubstitutionsRequest,
};
use task_core::service_impl::{CookingServiceDeps, CookingServiceImpl};
use task_core::substitution;
use task_db::seed::{DEMO_NAMESPACE, seed_demo_data};
use uuid::Uuid;

fn demo_id(key: &str) -> Uuid {
    Uuid::new_v5(&DEMO_NAMESPACE, key.as_bytes())
}

#[tokio::test]
async fn seed_populates_substitutions() {
    let db = task_db::init_memory().await.expect("init db");
    let s = seed_demo_data(&db).await.expect("seed");
    assert!(
        s.substitutions_created >= 15,
        "substitutions_created={}",
        s.substitutions_created
    );
}

#[tokio::test]
async fn create_substitution_round_trips() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let from_id = demo_id("food:butter");
    let to_id = demo_id("food:olive-oil");
    let api = svc
        .create_substitution(CreateSubstitutionRequest {
            from_food_id: from_id,
            to_food_id: to_id,
            ratio: 0.75,
            conversion_note: Some("for sautéing".to_string()),
            applies_when_json: Some(r#"{"dietary":["vegan"]}"#.to_string()),
            confidence: 0.9,
            bidirectional: false,
            organization: Some("personal".to_string()),
            created_by: Some("test".to_string()),
        })
        .await
        .expect("create");
    assert_eq!(api.from_food_id, from_id);
    assert_eq!(api.to_food_id, to_id);
    assert!((api.ratio - 0.75).abs() < f64::EPSILON);
    let list = svc
        .list_substitutions(Some("personal".to_string()))
        .await
        .expect("list");
    assert!(list.iter().any(|r| r.id == api.id));
    svc.delete_substitution(api.id).await.expect("delete");
    let after = svc
        .list_substitutions(Some("personal".to_string()))
        .await
        .expect("list");
    assert!(!after.iter().any(|r| r.id == api.id));
}

#[tokio::test]
async fn suggest_vegan_filter_returns_flax_egg_for_eggs() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    // banana-pancakes contains eggs; pick that recipe.
    let recipe_id = demo_id("recipe:banana-pancakes");
    let view = svc
        .suggest_substitutions(SuggestSubstitutionsRequest {
            recipe_id,
            missing_food_ids: Vec::new(),
            dietary_filter: vec!["vegan".to_string()],
            organization: Some("personal".to_string()),
            limit_per_ingredient: Some(5),
        })
        .await
        .expect("suggest");
    let suggestions: Vec<IngredientSuggestion> =
        serde_json::from_str(&view.suggestions_json).expect("decode");
    let eggs = suggestions
        .iter()
        .find(|s| s.ingredient_food_id == Some(demo_id("food:eggs")))
        .expect("eggs flagged");
    assert!(eggs.reasons.iter().any(|r| r == "dietary:vegan"));
    let top = eggs.suggestions.first().expect("at least one suggestion");
    assert_eq!(top.to_food_id, demo_id("food:flax-egg"));
}

#[tokio::test]
async fn bidirectional_inverse_is_synthesized() {
    // Use a fresh DB so we can construct exactly one bidirectional
    // entry going `to → from` and check the inverse is returned.
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    // The seeded `cow_milk → almond_milk` entry is bidirectional.
    // Querying suggestions for almond milk (the `to` side) when it's
    // missing should synthesize the inverse direction (almond → cow).
    // Insert a "buttermilk uses almond milk" pseudo-row by patching the
    // recipe's first ingredient food link to almond milk so the query
    // walks the inverse path. Simplest way: drive the helper directly
    // by invoking suggest_substitutions on a recipe that has milk and
    // marking almond-milk as missing — but the recipe doesn't have
    // almond milk. Instead, construct a fresh substitution that's
    // `bidirectional` only and verify the inverse direction surfaces
    // when the recipe ingredient matches `to_food_id`.
    let now = chrono::Utc::now();
    // Create a fresh recipe with almond_milk as an ingredient.
    let fresh_recipe_id = Uuid::new_v4();
    let fresh_recipe = task_core::recipe::ActiveModel {
        id: Set(fresh_recipe_id),
        name: Set("Almond milk smoothie".to_string()),
        slug: Set("almond-milk-smoothie".to_string()),
        description: Set(None),
        organization: Set(Some("personal".to_string())),
        prep_time_minutes: Set(None),
        cook_time_minutes: Set(None),
        total_time_minutes: Set(None),
        servings: Set(Some(1)),
        yield_label: Set(None),
        source_url: Set(None),
        image_url: Set(None),
        rating: Set(None),
        last_made: Set(None),
        notes: Set(None),
        nutrition_summary: Set(JsonObject::default()),
        properties: Set(JsonObject::default()),
        created_by: Set(None),
        created_at: Set(now),
        updated_at: Set(now),
    };
    fresh_recipe.insert(&db).await.expect("insert recipe");
    let ing = task_core::recipe_ingredient::ActiveModel {
        id: Set(Uuid::new_v4()),
        recipe_id: Set(fresh_recipe_id),
        sequence: Set(1),
        quantity: Set(Some(1.0)),
        unit: Set(Some("cup".to_string())),
        food: Set("almond milk".to_string()),
        food_id: Set(Some(demo_id("food:almond-milk"))),
        note: Set(None),
        is_section: Set(false),
        created_at: Set(now),
        updated_at: Set(now),
    };
    ing.insert(&db).await.expect("insert ingredient");

    // Mark almond milk missing — this drives the missing-path filter
    // (no dietary filter required to surface inverses).
    let view = svc
        .suggest_substitutions(SuggestSubstitutionsRequest {
            recipe_id: fresh_recipe_id,
            missing_food_ids: vec![demo_id("food:almond-milk")],
            dietary_filter: Vec::new(),
            organization: Some("personal".to_string()),
            limit_per_ingredient: Some(10),
        })
        .await
        .expect("suggest");
    let suggestions: Vec<IngredientSuggestion> =
        serde_json::from_str(&view.suggestions_json).expect("decode");
    let almond = suggestions
        .iter()
        .find(|s| s.ingredient_food_id == Some(demo_id("food:almond-milk")))
        .expect("almond milk flagged");
    let inverse = almond
        .suggestions
        .iter()
        .find(|r| r.is_inverse)
        .expect("at least one inverse");
    // The seeded cow→almond bidirectional sub uses ratio 1.0; inverse
    // is also 1.0. Just verify the synthesized direction makes sense.
    assert_eq!(inverse.to_food_id, demo_id("food:whole-milk"));
    assert!((inverse.ratio - 1.0).abs() < f64::EPSILON);
}

#[tokio::test]
async fn unlinked_ingredient_emits_warning() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    let now = chrono::Utc::now();
    let recipe_id = Uuid::new_v4();
    let recipe = task_core::recipe::ActiveModel {
        id: Set(recipe_id),
        name: Set("Mystery dish".to_string()),
        slug: Set("mystery-dish".to_string()),
        description: Set(None),
        organization: Set(Some("personal".to_string())),
        prep_time_minutes: Set(None),
        cook_time_minutes: Set(None),
        total_time_minutes: Set(None),
        servings: Set(Some(1)),
        yield_label: Set(None),
        source_url: Set(None),
        image_url: Set(None),
        rating: Set(None),
        last_made: Set(None),
        notes: Set(None),
        nutrition_summary: Set(JsonObject::default()),
        properties: Set(JsonObject::default()),
        created_by: Set(None),
        created_at: Set(now),
        updated_at: Set(now),
    };
    recipe.insert(&db).await.expect("insert recipe");
    let ing = task_core::recipe_ingredient::ActiveModel {
        id: Set(Uuid::new_v4()),
        recipe_id: Set(recipe_id),
        sequence: Set(1),
        quantity: Set(Some(1.0)),
        unit: Set(Some("cup".to_string())),
        food: Set("unobtainium".to_string()),
        food_id: Set(None),
        note: Set(None),
        is_section: Set(false),
        created_at: Set(now),
        updated_at: Set(now),
    };
    ing.insert(&db).await.expect("insert ingredient");

    let view = svc
        .suggest_substitutions(SuggestSubstitutionsRequest {
            recipe_id,
            missing_food_ids: Vec::new(),
            dietary_filter: vec!["vegan".to_string()],
            organization: Some("personal".to_string()),
            limit_per_ingredient: None,
        })
        .await
        .expect("suggest");
    assert!(
        view.warnings.iter().any(|w| w.contains("unobtainium")),
        "expected an unlinked-ingredient warning, got: {:?}",
        view.warnings
    );
    let suggestions: Vec<IngredientSuggestion> =
        serde_json::from_str(&view.suggestions_json).expect("decode");
    let entry = suggestions
        .iter()
        .find(|s| s.ingredient_food_name == "unobtainium")
        .expect("unlinked ingredient flagged");
    assert!(entry.suggestions.is_empty());
}

#[tokio::test]
async fn limit_per_ingredient_caps_results() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = CookingServiceImpl::new(CookingServiceDeps { db: db.clone() });
    // whole milk has 3 vegan-direction subs (almond/oat/soy). Cap at 1.
    let recipe_id = demo_id("recipe:banana-pancakes");
    let view = svc
        .suggest_substitutions(SuggestSubstitutionsRequest {
            recipe_id,
            missing_food_ids: Vec::new(),
            dietary_filter: vec!["vegan".to_string()],
            organization: Some("personal".to_string()),
            limit_per_ingredient: Some(1),
        })
        .await
        .expect("suggest");
    let suggestions: Vec<IngredientSuggestion> =
        serde_json::from_str(&view.suggestions_json).expect("decode");
    for s in &suggestions {
        assert!(
            s.suggestions.len() <= 1,
            "ingredient {:?} returned {} suggestions",
            s.ingredient_food_name,
            s.suggestions.len()
        );
    }
}

#[tokio::test]
async fn substitution_table_is_creatable_from_migration() {
    let db = task_db::init_memory().await.expect("init db");
    // Just verify the table is queryable post-migration.
    let _rows: Vec<substitution::Model> = substitution::Entity::find()
        .all(&db)
        .await
        .expect("query empty substitutions");
}
