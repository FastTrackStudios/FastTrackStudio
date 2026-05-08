//! Recipe-import integration tests.
//!
//! Drives the [`task_core::recipe_import::RecipeImporter`] parse path
//! against captured HTML fixtures (no live network). Covers JSON-LD
//! single-Recipe, JSON-LD `@graph` wrapper with `HowToSection`
//! flattening, and the OpenGraph fallback.

#![cfg(feature = "server")]

use serde_json::Value;
use task_core::recipe::{RecipeIngredientSpec, RecipeStepSpec};
use task_core::recipe_import::{ImportStrategy, RecipeImporter, parse_iso_duration};
use task_core::recipe_ingredient::parse_ingredient_line;

const NYT_FIXTURE: &str = include_str!("recipe_import_fixtures/nyt-cooking-jsonld.html");
const SE_FIXTURE: &str = include_str!("recipe_import_fixtures/seriouseats-graph.html");
const OG_FIXTURE: &str = include_str!("recipe_import_fixtures/opengraph-only.html");

#[test]
fn jsonld_single_recipe_populates_all_known_fields() {
    let result = RecipeImporter::parse(NYT_FIXTURE, "https://cooking.example.com/r/1234").unwrap();
    assert_eq!(result.strategy, ImportStrategy::JsonLd);

    let draft = &result.draft;
    assert_eq!(draft.name, "Buttermilk Pancakes");
    assert_eq!(
        draft.description.as_deref(),
        Some("A reliable, fluffy stack of buttermilk pancakes.")
    );
    assert_eq!(draft.prep_time_minutes, Some(10));
    assert_eq!(draft.cook_time_minutes, Some(15));
    // "4 servings" — leading integer is lifted into servings, full
    // string is preserved as the yield label.
    assert_eq!(draft.servings, Some(4));
    assert_eq!(draft.yield_label.as_deref(), Some("4 servings"));
    assert_eq!(
        draft.source_url.as_deref(),
        Some("https://cooking.example.com/recipes/1234-buttermilk-pancakes")
    );
    assert_eq!(draft.created_by.as_deref(), Some("Sam Sifton"));
    assert_eq!(
        draft.image_url.as_deref(),
        Some("https://static01.example.com/img/pancakes.jpg")
    );
    assert_eq!(
        result.image_url.as_deref(),
        Some("https://static01.example.com/img/pancakes.jpg")
    );

    let ingredients: Vec<RecipeIngredientSpec> =
        serde_json::from_str(&draft.ingredients_json).unwrap();
    assert_eq!(ingredients.len(), 6);
    assert_eq!(ingredients[0].quantity, Some(2.0));
    assert_eq!(ingredients[0].unit.as_deref(), Some("cups"));
    assert_eq!(ingredients[0].food, "all-purpose flour");

    let steps: Vec<RecipeStepSpec> = serde_json::from_str(&draft.steps_json).unwrap();
    assert_eq!(steps.len(), 4);
    assert!(steps[0].text.starts_with("Whisk dry"));

    let props: Value = serde_json::from_str(draft.properties_json.as_deref().unwrap()).unwrap();
    assert_eq!(props["cuisine"], "American");
    assert_eq!(props["category"], "Breakfast");
    let keywords = props["keywords"].as_array().unwrap();
    assert!(
        keywords
            .iter()
            .any(|v| v == &Value::String("pancakes".to_string()))
    );
    let rating = props["rating"].as_f64().unwrap();
    assert!((rating - 4.7).abs() < 1e-3);
}

#[test]
fn jsonld_graph_wrapper_finds_recipe_and_flattens_sections() {
    let result =
        RecipeImporter::parse(SE_FIXTURE, "https://seriouseats.example.com/cookies").unwrap();
    assert_eq!(result.strategy, ImportStrategy::JsonLd);

    let draft = &result.draft;
    assert_eq!(draft.name, "The Best Chocolate Chip Cookies");
    assert_eq!(draft.prep_time_minutes, Some(20));
    assert_eq!(draft.cook_time_minutes, Some(15));
    // Numeric recipeYield → servings, no yield_label.
    assert_eq!(draft.servings, Some(24));
    assert_eq!(draft.yield_label, None);

    let ingredients: Vec<RecipeIngredientSpec> =
        serde_json::from_str(&draft.ingredients_json).unwrap();
    assert_eq!(ingredients.len(), 9);
    // "1.5 cups granulated sugar"
    assert_eq!(ingredients[1].quantity, Some(1.5));

    let steps: Vec<RecipeStepSpec> = serde_json::from_str(&draft.steps_json).unwrap();
    // 2 + 4 = 6 steps total across the two sections.
    assert_eq!(steps.len(), 6);
    // First step under each section gets the section name prefixed.
    assert!(steps[0].text.starts_with("[Brown the butter]"));
    assert!(steps[2].text.starts_with("[Mix and bake]"));
    // Subsequent steps in the same section don't repeat the prefix.
    assert!(!steps[1].text.starts_with('['));

    let props: Value = serde_json::from_str(draft.properties_json.as_deref().unwrap()).unwrap();
    assert_eq!(props["cuisine"], "American");
    // recipeCategory was an array — first element wins.
    assert_eq!(props["category"], "Dessert");

    // Image came from an `image: { @type: ImageObject, url: ... }`.
    assert_eq!(
        draft.image_url.as_deref(),
        Some("https://seriouseats.example.com/cookies.jpg")
    );

    // ratingValue was a string — coerced into f32.
    let rating = props["rating"].as_f64().unwrap();
    assert!((rating - 4.8).abs() < 1e-3);
}

#[test]
fn opengraph_fallback_when_no_jsonld() {
    let result =
        RecipeImporter::parse(OG_FIXTURE, "https://example.com/recipes/roast-chicken").unwrap();
    assert_eq!(result.strategy, ImportStrategy::OpenGraph);

    let draft = &result.draft;
    assert_eq!(draft.name, "Simple Roast Chicken");
    assert_eq!(
        draft.description.as_deref(),
        Some("A whole roast chicken with crispy skin and juicy meat.")
    );
    assert_eq!(
        draft.image_url.as_deref(),
        Some("https://example.com/img/roast-chicken.jpg")
    );
    let ingredients: Vec<RecipeIngredientSpec> =
        serde_json::from_str(&draft.ingredients_json).unwrap();
    let steps: Vec<RecipeStepSpec> = serde_json::from_str(&draft.steps_json).unwrap();
    assert!(ingredients.is_empty());
    assert!(steps.is_empty());

    assert!(!result.warnings.is_empty());
    assert!(
        result
            .warnings
            .iter()
            .any(|w| w.contains("manual entry required"))
    );
}

#[test]
fn iso_duration_table() {
    let cases = [
        ("PT15M", Some(15)),
        ("PT1H", Some(60)),
        ("PT1H30M", Some(90)),
        ("PT0M", Some(0)),
        ("P0DT15M", Some(15)),
        ("P1D", Some(24 * 60)),
        ("PT2H45M30S", Some(165)),
        ("", None),
        ("garbage", None),
        ("P1Y", None),
        ("P1W", None),
    ];
    for (input, expected) in cases {
        assert_eq!(
            parse_iso_duration(input),
            expected,
            "duration parser disagreed on {input:?}"
        );
    }
}

#[test]
fn ingredient_line_parser_smoke() {
    let cases = [
        ("1.5 cup olive oil", Some(1.5_f64), Some("cup"), "olive oil"),
        ("2 eggs", Some(2.0), None, "eggs"),
        ("salt", None, None, "salt"),
        ("", None, None, ""),
        (
            "16 tablespoons unsalted butter",
            Some(16.0),
            Some("tablespoons"),
            "unsalted butter",
        ),
    ];
    for (line, qty, unit, food) in cases {
        let spec = parse_ingredient_line(line);
        assert_eq!(spec.quantity, qty, "quantity for {line:?}");
        assert_eq!(spec.unit.as_deref(), unit, "unit for {line:?}");
        assert_eq!(spec.food, food, "food for {line:?}");
    }
}
