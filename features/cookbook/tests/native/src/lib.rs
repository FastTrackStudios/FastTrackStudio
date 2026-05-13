//! Native integration tests for the `cookbook` feature. One ephemeral
//! `CrdtDoc` hosts every entity type — proves they coexist under one
//! root and survive cross-entity sync round-trips.

#![cfg(test)]

use architect::Page;
use chrono::{Duration, Utc};
use cookbook_crdt::{
    CookbookRepoLoro, CrdtDoc, MealPlanRepoLoro, RecipeIngredientRepoLoro, RecipeRepoLoro,
    RecipeStepRepoLoro,
};
use cookbook_proto::{
    CookbookCreate, CookbookRepo, MealPlanCreate, MealPlanRepo, RecipeCreate,
    RecipeIngredientCreate, RecipeIngredientRepo, RecipeRepo, RecipeStepCreate, RecipeStepRepo,
};
use loro::ExportMode;

fn page() -> Page {
    Page {
        index: 0,
        size: 100,
    }
}

#[tokio::test]
async fn cookbook_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = CookbookRepoLoro::new(&doc);
    let c = repo
        .create(CookbookCreate {
            name: "Weeknight Dinners".into(),
            author: Some("cody".into()),
            description: Some("Quick stuff".into()),
            cover_image_url: None,
            tags: vec!["fast".into(), "easy".into()],
        })
        .await
        .unwrap();
    let got = repo.get(c.id).await.unwrap();
    assert_eq!(got.name, "Weeknight Dinners");
    assert_eq!(got.author.as_deref(), Some("cody"));
    assert_eq!(got.tags, vec!["fast".to_string(), "easy".to_string()]);
}

#[tokio::test]
async fn recipe_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let cookbooks = CookbookRepoLoro::new(&doc);
    let recipes = RecipeRepoLoro::new(&doc);

    let cb = cookbooks
        .create(CookbookCreate {
            name: "Italian".into(),
            author: None,
            description: None,
            cover_image_url: None,
            tags: vec![],
        })
        .await
        .unwrap();

    let r = recipes
        .create(RecipeCreate {
            cookbook_id: Some(cb.id),
            name: "Carbonara".into(),
            summary: Some("Classic".into()),
            servings: Some(2),
            prep_time_minutes: Some(10),
            cook_time_minutes: Some(15),
            total_time_minutes: Some(25),
            cuisine: Some("italian".into()),
            source_url: None,
            image_url: None,
            tags: vec!["pasta".into()],
        })
        .await
        .unwrap();

    let got = recipes.get(r.id).await.unwrap();
    assert_eq!(got.name, "Carbonara");
    assert_eq!(got.cookbook_id, Some(cb.id));
    assert_eq!(got.servings, Some(2));
    assert_eq!(got.total_time_minutes, Some(25));
    assert_eq!(got.cuisine.as_deref(), Some("italian"));
}

#[tokio::test]
async fn ingredient_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let recipes = RecipeRepoLoro::new(&doc);
    let ingredients = RecipeIngredientRepoLoro::new(&doc);

    let r = recipes
        .create(RecipeCreate {
            cookbook_id: None,
            name: "Test".into(),
            summary: None,
            servings: None,
            prep_time_minutes: None,
            cook_time_minutes: None,
            total_time_minutes: None,
            cuisine: None,
            source_url: None,
            image_url: None,
            tags: vec![],
        })
        .await
        .unwrap();

    let ing = ingredients
        .create(RecipeIngredientCreate {
            recipe_id: r.id,
            name: "olive oil".into(),
            qty_thousandths: 30_000,
            unit: Some("ml".into()),
            notes: Some("extra virgin".into()),
            sort_index: 0,
            food_product_id: None,
        })
        .await
        .unwrap();

    let got = ingredients.get(ing.id).await.unwrap();
    assert_eq!(got.name, "olive oil");
    assert_eq!(got.recipe_id, r.id);
    assert_eq!(got.qty_thousandths, 30_000);
    assert_eq!(got.unit.as_deref(), Some("ml"));
}

#[tokio::test]
async fn step_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let recipes = RecipeRepoLoro::new(&doc);
    let steps = RecipeStepRepoLoro::new(&doc);

    let r = recipes
        .create(RecipeCreate {
            cookbook_id: None,
            name: "Test".into(),
            summary: None,
            servings: None,
            prep_time_minutes: None,
            cook_time_minutes: None,
            total_time_minutes: None,
            cuisine: None,
            source_url: None,
            image_url: None,
            tags: vec![],
        })
        .await
        .unwrap();

    let s = steps
        .create(RecipeStepCreate {
            recipe_id: r.id,
            step_number: 1,
            instruction: "Boil water".into(),
            duration_minutes: Some(5),
            image_url: None,
        })
        .await
        .unwrap();

    let got = steps.get(s.id).await.unwrap();
    assert_eq!(got.instruction, "Boil water");
    assert_eq!(got.step_number, 1);
    assert_eq!(got.duration_minutes, Some(5));
}

#[tokio::test]
async fn meal_plan_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let plans = MealPlanRepoLoro::new(&doc);

    let when = Utc::now() + Duration::days(1);
    let mp = plans
        .create(MealPlanCreate {
            recipe_id: None,
            name: "Tomorrow's dinner".into(),
            planned_for: when,
            meal_type: "dinner".into(),
            servings: Some(4),
            notes: Some("with salad".into()),
        })
        .await
        .unwrap();

    let got = plans.get(mp.id).await.unwrap();
    assert_eq!(got.name, "Tomorrow's dinner");
    assert_eq!(got.meal_type, "dinner");
    assert_eq!(got.servings, Some(4));
}

#[tokio::test]
async fn all_five_coexist_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let cookbooks = CookbookRepoLoro::new(&doc);
    let recipes = RecipeRepoLoro::new(&doc);
    let ingredients = RecipeIngredientRepoLoro::new(&doc);
    let steps = RecipeStepRepoLoro::new(&doc);
    let plans = MealPlanRepoLoro::new(&doc);

    let cb = cookbooks
        .create(CookbookCreate {
            name: "CB".into(),
            author: None,
            description: None,
            cover_image_url: None,
            tags: vec![],
        })
        .await
        .unwrap();

    let r = recipes
        .create(RecipeCreate {
            cookbook_id: Some(cb.id),
            name: "R1".into(),
            summary: None,
            servings: None,
            prep_time_minutes: None,
            cook_time_minutes: None,
            total_time_minutes: None,
            cuisine: None,
            source_url: None,
            image_url: None,
            tags: vec![],
        })
        .await
        .unwrap();

    ingredients
        .create(RecipeIngredientCreate {
            recipe_id: r.id,
            name: "salt".into(),
            qty_thousandths: 5_000,
            unit: Some("g".into()),
            notes: None,
            sort_index: 0,
            food_product_id: None,
        })
        .await
        .unwrap();

    steps
        .create(RecipeStepCreate {
            recipe_id: r.id,
            step_number: 1,
            instruction: "Do thing".into(),
            duration_minutes: None,
            image_url: None,
        })
        .await
        .unwrap();

    plans
        .create(MealPlanCreate {
            recipe_id: Some(r.id),
            name: "Plan 1".into(),
            planned_for: Utc::now(),
            meal_type: "lunch".into(),
            servings: None,
            notes: None,
        })
        .await
        .unwrap();

    assert_eq!(cookbooks.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(recipes.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(ingredients.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(steps.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(plans.list(page(), None, None).await.unwrap().total, 1);
}

#[tokio::test]
async fn replicas_converge_across_recipe_and_ingredient() {
    let a = CrdtDoc::ephemeral();
    let b = CrdtDoc::ephemeral();
    let ra = RecipeRepoLoro::new(&a);
    let rb = RecipeRepoLoro::new(&b);
    let ia = RecipeIngredientRepoLoro::new(&a);
    let ib = RecipeIngredientRepoLoro::new(&b);

    let r_a = ra
        .create(RecipeCreate {
            cookbook_id: None,
            name: "A-recipe".into(),
            summary: None,
            servings: None,
            prep_time_minutes: None,
            cook_time_minutes: None,
            total_time_minutes: None,
            cuisine: None,
            source_url: None,
            image_url: None,
            tags: vec![],
        })
        .await
        .unwrap();
    let r_b = rb
        .create(RecipeCreate {
            cookbook_id: None,
            name: "B-recipe".into(),
            summary: None,
            servings: None,
            prep_time_minutes: None,
            cook_time_minutes: None,
            total_time_minutes: None,
            cuisine: None,
            source_url: None,
            image_url: None,
            tags: vec![],
        })
        .await
        .unwrap();

    ia.create(RecipeIngredientCreate {
        recipe_id: r_a.id,
        name: "from-a".into(),
        qty_thousandths: 1_000,
        unit: None,
        notes: None,
        sort_index: 0,
        food_product_id: None,
    })
    .await
    .unwrap();
    ib.create(RecipeIngredientCreate {
        recipe_id: r_b.id,
        name: "from-b".into(),
        qty_thousandths: 2_000,
        unit: None,
        notes: None,
        sort_index: 0,
        food_product_id: None,
    })
    .await
    .unwrap();

    let ab = ra.doc().export(ExportMode::all_updates()).unwrap();
    let bb = rb.doc().export(ExportMode::all_updates()).unwrap();
    rb.doc().import(&ab).unwrap();
    ra.doc().import(&bb).unwrap();

    assert_eq!(ra.list(page(), None, None).await.unwrap().total, 2);
    assert_eq!(rb.list(page(), None, None).await.unwrap().total, 2);
    assert_eq!(ia.list(page(), None, None).await.unwrap().total, 2);
    assert_eq!(ib.list(page(), None, None).await.unwrap().total, 2);
}

// ── pantry / food-product / shopping-list (lifted from inventory) ────

use cookbook_crdt::{FoodProductRepoLoro, PantryItemRepoLoro, ShoppingListItemRepoLoro};
use cookbook_proto::{
    FoodProductCreate, FoodProductRepo, PantryItemCreate, PantryItemRepo, ShoppingListItemCreate,
    ShoppingListItemRepo,
};

#[tokio::test]
async fn food_product_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = FoodProductRepoLoro::new(&doc);

    let p = repo
        .create(FoodProductCreate {
            name: "Whole Milk".into(),
            brand: Some("Organic Valley".into()),
            category: Some("dairy".into()),
            barcode: Some("0123456789012".into()),
            default_unit: Some("ml".into()),
            default_qty_thousandths: Some(1_000_000),
            notes: None,
            tags: vec!["fridge".into(), "dairy".into()],
        })
        .await
        .unwrap();

    let got = repo.get(p.id).await.unwrap();
    assert_eq!(got.name, "Whole Milk");
    assert_eq!(got.brand.as_deref(), Some("Organic Valley"));
    assert_eq!(got.category.as_deref(), Some("dairy"));
    assert_eq!(got.barcode.as_deref(), Some("0123456789012"));
    assert_eq!(got.default_unit.as_deref(), Some("ml"));
    assert_eq!(got.default_qty_thousandths, Some(1_000_000));
    assert_eq!(got.tags, vec!["fridge".to_string(), "dairy".to_string()]);
}

#[tokio::test]
async fn pantry_item_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = PantryItemRepoLoro::new(&doc);

    let expires = Utc::now() + Duration::days(14);
    let item = repo
        .create(PantryItemCreate {
            product_id: None,
            name: "Eggs".into(),
            qty_thousandths: 12_000,
            unit: "ea".into(),
            location: Some("fridge".into()),
            expires_at: Some(expires),
            opened_at: None,
            notes: Some("free range".into()),
            tags: vec!["protein".into()],
        })
        .await
        .unwrap();

    let got = repo.get(item.id).await.unwrap();
    assert_eq!(got.name, "Eggs");
    assert_eq!(got.qty_thousandths, 12_000);
    assert_eq!(got.unit, "ea");
    assert_eq!(got.location.as_deref(), Some("fridge"));
    assert!(got.expires_at.is_some());
    assert_eq!(got.tags, vec!["protein".to_string()]);
}

#[tokio::test]
async fn shopping_list_item_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = ShoppingListItemRepoLoro::new(&doc);

    let item = repo
        .create(ShoppingListItemCreate {
            product_id: None,
            name: "Bread".into(),
            qty_thousandths: 1_000,
            unit: "ea".into(),
            purchased: false,
            purchased_at: None,
            sort_index: 5,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    let got = repo.get(item.id).await.unwrap();
    assert_eq!(got.name, "Bread");
    assert_eq!(got.qty_thousandths, 1_000);
    assert_eq!(got.unit, "ea");
    assert!(!got.purchased);
    assert_eq!(got.sort_index, 5);
}

#[tokio::test]
async fn pantry_food_shopping_coexist_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let products = FoodProductRepoLoro::new(&doc);
    let pantry = PantryItemRepoLoro::new(&doc);
    let shopping = ShoppingListItemRepoLoro::new(&doc);

    products
        .create(FoodProductCreate {
            name: "Salt".into(),
            brand: None,
            category: Some("spice".into()),
            barcode: None,
            default_unit: Some("g".into()),
            default_qty_thousandths: Some(500_000),
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    pantry
        .create(PantryItemCreate {
            product_id: None,
            name: "Pasta".into(),
            qty_thousandths: 500_000,
            unit: "g".into(),
            location: Some("pantry".into()),
            expires_at: None,
            opened_at: None,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    shopping
        .create(ShoppingListItemCreate {
            product_id: None,
            name: "Olive Oil".into(),
            qty_thousandths: 750_000,
            unit: "ml".into(),
            purchased: false,
            purchased_at: None,
            sort_index: 0,
            notes: None,
            tags: vec![],
        })
        .await
        .unwrap();

    assert_eq!(products.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(pantry.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(shopping.list(page(), None, None).await.unwrap().total, 1);
}
