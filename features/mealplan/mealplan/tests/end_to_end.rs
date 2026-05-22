//! End-to-end smoke test for the full mealplan slice.
//!
//! Walks the loop:
//!
//! 1. Open an empty tempdir vault.
//! 2. Create two pantry items (pasta + olive oil) — both
//!    barcoded, both with stock entries.
//! 3. Create a recipe that uses pasta + oil + a missing
//!    ingredient (truffles) + a substitutable ingredient
//!    (butter, with a registry rule pointing at olive oil).
//! 4. Create a meal referencing the recipe + cook it (debits
//!    pasta + oil from pantry).
//! 5. Check fulfillment for a *second* meal of the same
//!    recipe — confirms shortage detection + substitution
//!    suggestions surface.
//! 6. Auto-populate a shopping list from the missing items.
//! 7. Confirm meal nutrition aggregates correctly.
//!
//! This guards against silent regressions when any single
//! phase's model changes — the whole chain has to keep
//! composing.

use chrono::NaiveDate;
use cookbook::{CookbookService, Ingredient, Nutrition, Recipe};
use mealplan::{
    MealplanService, ShoppingService, ShoppingStore, ShortageReason, Slot, SubstitutionService,
    SubstitutionStore, fulfillment,
};
use pantry::{PantryService, StockEntry, SubReason};
use std::path::Path;
use tempfile::tempdir;
use uuid::Uuid;
use vault::Vault;

fn fresh_vault(root: &Path) -> Vault {
    std::fs::create_dir_all(root.join("Operations/Inventory/Pantry")).unwrap();
    std::fs::create_dir_all(root.join("Wiki/Cookbook")).unwrap();
    std::fs::create_dir_all(root.join("Projects/Mealplan/meals")).unwrap();
    std::fs::create_dir_all(root.join("shopping")).unwrap();
    std::fs::create_dir_all(root.join("substitutions")).unwrap();
    Vault::open(root).expect("open vault")
}

#[test]
fn full_grocery_to_meal_loop() {
    let dir = tempdir().unwrap();
    let vault = fresh_vault(dir.path());

    let store = mealplan::Store::new(vault);
    let pantry = store.pantry().clone();
    let cookbook = store.cookbook().clone();
    let shopping = ShoppingStore::from_shared(store.shared());
    let subs = SubstitutionStore::from_shared(store.shared());

    // ── 1. Create pantry items with barcodes + stock ──
    let mut pasta = pantry::PantryItem::from_item(inventory::Item {
        path: String::new(),
        id: Uuid::nil(),
        name: "Pasta".into(),
        category: "food".into(),
        location_id: None,
        condition: "good".into(),
        status: "stored".into(),
        manufacturer: None,
        model: None,
        serial: None,
        purchase_date: None,
        value: None,
        tasks: Vec::new(),
        tags: vec!["item".into(), "pantry".into()],
        date_created: None,
        date_modified: None,
        details: String::new(),
    });
    pasta.unit = "g".into();
    pasta.barcodes = vec!["8001234567890".into()];
    let pasta = pantry.create(pasta).unwrap();
    pantry
        .add_stock(
            &pasta.id.to_string(),
            StockEntry {
                id: Uuid::nil(),
                qty: 500.0,
                purchased_date: NaiveDate::from_ymd_opt(2026, 5, 1).unwrap(),
                best_before: None,
                opened: false,
                opened_date: None,
                price: Some(2.49),
                location_id: None,
                note: None,
            },
        )
        .unwrap();

    let mut olive_oil = pantry::PantryItem::from_item(inventory::Item {
        path: String::new(),
        id: Uuid::nil(),
        name: "Olive Oil".into(),
        category: "food".into(),
        location_id: None,
        condition: "good".into(),
        status: "stored".into(),
        manufacturer: None,
        model: None,
        serial: None,
        purchase_date: None,
        value: None,
        tasks: Vec::new(),
        tags: vec!["item".into(), "pantry".into()],
        date_created: None,
        date_modified: None,
        details: String::new(),
    });
    olive_oil.unit = "ml".into();
    let olive_oil = pantry.create(olive_oil).unwrap();
    pantry
        .add_stock(
            &olive_oil.id.to_string(),
            StockEntry {
                id: Uuid::nil(),
                qty: 1000.0,
                purchased_date: NaiveDate::from_ymd_opt(2026, 5, 1).unwrap(),
                best_before: None,
                opened: false,
                opened_date: None,
                price: Some(8.99),
                location_id: None,
                note: None,
            },
        )
        .unwrap();

    // Empty "Butter" item so the substitution registry
    // rule has somewhere to point at.
    let mut butter = pantry::PantryItem::from_item(inventory::Item {
        path: String::new(),
        id: Uuid::nil(),
        name: "Butter".into(),
        category: "food".into(),
        location_id: None,
        condition: "good".into(),
        status: "stored".into(),
        manufacturer: None,
        model: None,
        serial: None,
        purchase_date: None,
        value: None,
        tasks: Vec::new(),
        tags: vec!["item".into(), "pantry".into()],
        date_created: None,
        date_modified: None,
        details: String::new(),
    });
    butter.unit = "g".into();
    let butter = pantry.create(butter).unwrap();

    // ── 2. Substitution rule: butter → olive oil ──
    subs.create(mealplan::SubstitutionRule {
        path: String::new(),
        id: Uuid::nil(),
        name: "Butter → Olive Oil".into(),
        from_item_id: butter.id,
        to_item_id: olive_oil.id,
        ratio: 0.75,
        reasons: vec![SubReason::Vegan, SubReason::OutOfStock],
        tags: Vec::new(),
        note: None,
        date_created: None,
        date_modified: None,
        details: String::new(),
    })
    .unwrap();

    // ── 3. Recipe: pasta + oil + butter + truffles ──
    let recipe = Recipe {
        path: String::new(),
        id: Uuid::nil(),
        name: "Truffle Pasta".into(),
        description: None,
        course: "dinner".into(),
        cuisine: None,
        prep_minutes: Some(5),
        cook_minutes: Some(15),
        servings: Some(2),
        ingredients: vec![
            Ingredient {
                name: "Pasta".into(),
                qty: Some(200.0),
                unit: "g".into(),
                pantry_item_id: Some(pasta.id),
                substitutes: Vec::new(),
                note: None,
                optional: false,
            },
            Ingredient {
                name: "Olive Oil".into(),
                qty: Some(30.0),
                unit: "ml".into(),
                pantry_item_id: Some(olive_oil.id),
                substitutes: Vec::new(),
                note: None,
                optional: false,
            },
            Ingredient {
                name: "Butter".into(),
                qty: Some(20.0),
                unit: "g".into(),
                pantry_item_id: Some(butter.id),
                substitutes: Vec::new(),
                note: None,
                optional: false,
            },
            Ingredient {
                name: "Truffles".into(),
                qty: Some(5.0),
                unit: "g".into(),
                pantry_item_id: None,
                substitutes: Vec::new(),
                note: None,
                optional: false,
            },
        ],
        steps: vec!["Boil water.".into(), "Cook pasta.".into(), "Toss.".into()],
        nutrition: Some(Nutrition {
            calories: Some(520.0),
            protein_g: Some(16.0),
            carbs_g: Some(78.0),
            fat_g: Some(16.0),
            fiber_g: None,
            sugar_g: None,
        }),
        tags: vec!["weeknight".into()],
        nested_recipes: Vec::new(),
        source: None,
        date_created: None,
        date_modified: None,
        details: String::new(),
    };
    let recipe = cookbook.create(recipe).unwrap();

    // ── 4. Schedule + cook a meal — debits stock ──
    let meal = mealplan::Meal {
        path: String::new(),
        id: Uuid::nil(),
        name: "Friday Pasta Night".into(),
        scheduled_for: NaiveDate::from_ymd_opt(2026, 5, 22).unwrap(),
        slot: Slot::Dinner.as_str().to_string(),
        servings: 2,
        recipe_ids: vec![recipe.id],
        status: mealplan::Status::Planned.as_str().to_string(),
        pantry_deductions: Vec::new(),
        tags: Vec::new(),
        date_created: None,
        date_modified: None,
        details: String::new(),
    };
    let meal = store.create(meal).unwrap();
    let cooked = store
        .cook(
            &meal.id.to_string(),
            vec![
                mealplan::PantryDeduction {
                    item_id: pasta.id,
                    qty: 200.0,
                    unit: "g".into(),
                },
                mealplan::PantryDeduction {
                    item_id: olive_oil.id,
                    qty: 30.0,
                    unit: "ml".into(),
                },
            ],
        )
        .unwrap();
    assert_eq!(cooked.status, "cooked");

    // Pasta stock should be 300g; oil 970ml.
    let pasta_now = pantry.get(&pasta.id.to_string()).unwrap();
    assert!((pasta_now.stock_total().unwrap() - 300.0).abs() < 1e-6);
    let oil_now = pantry.get(&olive_oil.id.to_string()).unwrap();
    assert!((oil_now.stock_total().unwrap() - 970.0).abs() < 1e-6);

    // ── 5. Fulfillment for a follow-up meal — surfaces
    //       the truffles shortage + butter sub suggestion ──
    let rules = subs.list().unwrap();
    let pantry_now = pantry.list().unwrap();
    let f = fulfillment::check_with_subs(&recipe, &pantry_now, &rules, &[]);
    assert!(!f.can_cook);
    let by_name: std::collections::HashMap<&str, &mealplan::Shortage> =
        f.missing.iter().map(|s| (s.name.as_str(), s)).collect();

    let truffles = by_name.get("Truffles").expect("truffles shortage");
    assert!(matches!(truffles.reason, ShortageReason::NotInPantry));
    let butter_short = by_name.get("Butter").expect("butter shortage");
    assert!(
        !butter_short.suggestions.is_empty(),
        "butter should have subs"
    );
    assert!(
        butter_short
            .suggestions
            .iter()
            .any(|s| s.name == "Olive Oil"),
        "registry rule butter→olive-oil should surface"
    );

    // ── 6. Shopping list auto-populate ──
    let list = shopping
        .create(mealplan::ShoppingList {
            path: String::new(),
            id: Uuid::nil(),
            name: "Weekly Grocery Run".into(),
            store_location_id: None,
            entries: Vec::new(),
            date_created: None,
            date_modified: None,
            details: String::new(),
        })
        .unwrap();
    let list = shopping
        .add_missing_for_recipe(&list.id.to_string(), &recipe.id.to_string(), 2)
        .unwrap();
    assert!(
        list.entries.iter().any(|e| e.name == "Truffles"),
        "shopping list should pick up truffles"
    );
    assert!(
        list.entries.iter().any(|e| e.name == "Butter"),
        "shopping list should pick up butter"
    );

    // ── 7. Meal nutrition aggregation ──
    let total = cooked
        .nutrition_total(std::slice::from_ref(&recipe))
        .expect("nutrition");
    // 2 servings of a 520-cal recipe = 1040 cal.
    assert!((total.calories.unwrap() - 1040.0).abs() < 1e-6);
    assert!((total.protein_g.unwrap() - 32.0).abs() < 1e-6);
}
