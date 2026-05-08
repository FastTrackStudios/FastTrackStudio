//! Schema.org Recipe extraction from `<script type="application/ld+json">`
//! blocks.
//!
//! The walker is intentionally permissive: a Recipe can appear top-level,
//! inside an array, inside `@graph`, or with `@type` as either a string
//! or array of strings. The first match wins.

use scraper::{Html, Selector};
use serde_json::Value;

use crate::recipe::{RecipeIngredientSpec, RecipeStepSpec};
use crate::service::CreateRecipeRequest;

use super::duration::parse_iso_duration;

#[derive(Debug, Clone, Default)]
pub(super) struct ExtractedRecipe {
    pub draft: CreateRecipeRequest,
    pub image_url: Option<String>,
    pub warnings: Vec<String>,
}

/// Walk all `<script type="application/ld+json">` blocks; return the
/// first one that yields a usable `Recipe`. `None` means no JSON-LD
/// `Recipe` was found — caller can fall back to OpenGraph.
pub(super) fn extract(html: &str) -> Option<ExtractedRecipe> {
    let document = Html::parse_document(html);
    // `Selector::parse` is fallible only on programmer error here.
    let selector = Selector::parse(r#"script[type="application/ld+json"]"#).ok()?;
    for script in document.select(&selector) {
        let raw: String = script.text().collect();
        let trimmed = raw.trim();
        if trimmed.is_empty() {
            continue;
        }
        let Ok(value) = serde_json::from_str::<Value>(trimmed) else {
            continue;
        };
        if let Some(recipe) = find_recipe(&value) {
            if let Some(extracted) = map_recipe(recipe) {
                return Some(extracted);
            }
        }
    }
    None
}

/// Recursively walk `value` looking for the first object whose `@type`
/// includes "Recipe" (case-insensitive).
fn find_recipe(value: &Value) -> Option<&Value> {
    match value {
        Value::Object(map) => {
            if type_includes_recipe(map.get("@type")) {
                return Some(value);
            }
            if let Some(graph) = map.get("@graph") {
                if let Some(found) = find_recipe(graph) {
                    return Some(found);
                }
            }
            for (_, child) in map {
                if let Some(found) = find_recipe(child) {
                    return Some(found);
                }
            }
            None
        }
        Value::Array(items) => items.iter().find_map(find_recipe),
        _ => None,
    }
}

fn type_includes_recipe(value: Option<&Value>) -> bool {
    match value {
        Some(Value::String(s)) => s.eq_ignore_ascii_case("recipe"),
        Some(Value::Array(items)) => items.iter().any(|v| match v {
            Value::String(s) => s.eq_ignore_ascii_case("recipe"),
            _ => false,
        }),
        _ => false,
    }
}

fn map_recipe(value: &Value) -> Option<ExtractedRecipe> {
    let map = value.as_object()?;
    let mut warnings = Vec::new();

    let name = pick_string(map.get("name"))?.trim().to_string();
    if name.is_empty() {
        return None;
    }

    let description = pick_string(map.get("description")).map(|s| s.trim().to_string());

    let prep_time_minutes = map
        .get("prepTime")
        .and_then(pick_string_ref)
        .and_then(|s| parse_iso_duration(&s));
    let cook_time_minutes = map
        .get("cookTime")
        .and_then(pick_string_ref)
        .and_then(|s| parse_iso_duration(&s));

    // recipeYield → servings or yield_label.
    let (servings, yield_label) = match map.get("recipeYield") {
        Some(Value::Number(n)) => (n.as_u64().and_then(|v| u32::try_from(v).ok()), None),
        Some(Value::String(s)) => {
            let trimmed = s.trim();
            if let Ok(n) = trimmed.parse::<u32>() {
                (Some(n), None)
            } else {
                // Try to lift a leading integer out of strings like
                // "12 cookies" while still keeping the full label.
                let leading: String = trimmed.chars().take_while(|c| c.is_ascii_digit()).collect();
                let leading_n: Option<u32> = if leading.is_empty() {
                    None
                } else {
                    leading.parse().ok()
                };
                (leading_n, Some(trimmed.to_string()))
            }
        }
        Some(Value::Array(items)) => {
            // First element wins.
            let first = items.first().and_then(pick_string_ref);
            if let Some(s) = first {
                let trimmed = s.trim();
                if let Ok(n) = trimmed.parse::<u32>() {
                    (Some(n), None)
                } else {
                    (None, Some(trimmed.to_string()))
                }
            } else {
                (None, None)
            }
        }
        _ => (None, None),
    };

    // Ingredients
    let ingredients: Vec<RecipeIngredientSpec> = match map.get("recipeIngredient") {
        Some(Value::Array(items)) => items
            .iter()
            .filter_map(pick_string_ref)
            .map(|s| crate::recipe_ingredient::parse_ingredient_line(&s))
            .collect(),
        _ => Vec::new(),
    };

    // Instructions
    let steps = parse_instructions(map.get("recipeInstructions"));

    // Image
    let image_url = pick_image(map.get("image"));

    // Rating
    let rating = match map.get("aggregateRating") {
        Some(Value::Object(rating_map)) => {
            extract_rating(rating_map.get("ratingValue"), &mut warnings)
        }
        _ => None,
    };

    // properties: cuisine / category / keywords
    let mut properties = serde_json::Map::new();
    if let Some(cuisine) = pick_first_string(map.get("recipeCuisine")) {
        properties.insert("cuisine".to_string(), Value::String(cuisine));
    }
    if let Some(category) = pick_first_string(map.get("recipeCategory")) {
        properties.insert("category".to_string(), Value::String(category));
    }
    if let Some(keywords) = pick_keywords(map.get("keywords")) {
        properties.insert("keywords".to_string(), Value::Array(keywords));
    }
    if let Some(rating_value) = rating {
        properties.insert(
            "rating".to_string(),
            serde_json::Number::from_f64(rating_value.into())
                .map(Value::Number)
                .unwrap_or(Value::Null),
        );
    }

    let properties_json = if properties.is_empty() {
        None
    } else {
        Some(Value::Object(properties).to_string())
    };

    let created_by = map
        .get("author")
        .and_then(extract_author_name)
        .map(|s| s.trim().to_string())
        .filter(|s| !s.is_empty());

    let source_url = pick_string(map.get("url"));

    let ingredients_json = serde_json::to_string(&ingredients).ok()?;
    let steps_json = serde_json::to_string(&steps).ok()?;

    let draft = CreateRecipeRequest {
        name,
        description,
        organization: None,
        prep_time_minutes,
        cook_time_minutes,
        servings,
        source_url,
        created_by,
        image_url: image_url.clone(),
        yield_label,
        properties_json,
        ingredients_json,
        steps_json,
    };
    Some(ExtractedRecipe {
        draft,
        image_url,
        warnings,
    })
}

fn parse_instructions(value: Option<&Value>) -> Vec<RecipeStepSpec> {
    let mut steps: Vec<RecipeStepSpec> = Vec::new();
    let Some(value) = value else {
        return steps;
    };
    walk_instructions(value, None, &mut steps);
    steps
}

/// Walk the (possibly nested) `recipeInstructions` value, flattening
/// HowToStep / HowToSection into a single `Vec<RecipeStepSpec>`. The
/// section name (when present) is prepended to the first step under
/// that section to keep `RecipeStepSpec` schema-flat.
fn walk_instructions(
    value: &Value,
    mut section_prefix: Option<String>,
    out: &mut Vec<RecipeStepSpec>,
) {
    match value {
        Value::String(text) => {
            push_step(text, &mut section_prefix, out);
        }
        Value::Array(items) => {
            for item in items {
                walk_instructions(item, section_prefix.clone(), out);
                // Only the first step gets the prefix; clear it after
                // any push that consumed it.
                if section_prefix.is_some() && !out.is_empty() {
                    section_prefix = None;
                }
            }
        }
        Value::Object(map) => {
            let ty = map.get("@type").and_then(|v| v.as_str()).unwrap_or("");
            if ty.eq_ignore_ascii_case("HowToSection") {
                let name = map
                    .get("name")
                    .and_then(|v| v.as_str())
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty());
                if let Some(items) = map.get("itemListElement") {
                    walk_instructions(items, name, out);
                }
            } else if let Some(text) = map.get("text").and_then(|v| v.as_str()) {
                push_step(text, &mut section_prefix, out);
            } else if let Some(name) = map.get("name").and_then(|v| v.as_str()) {
                push_step(name, &mut section_prefix, out);
            }
        }
        _ => {}
    }
}

fn push_step(text: &str, section_prefix: &mut Option<String>, out: &mut Vec<RecipeStepSpec>) {
    let trimmed = text.trim();
    if trimmed.is_empty() {
        return;
    }
    let final_text = match section_prefix.take() {
        Some(prefix) => format!("[{prefix}] {trimmed}"),
        None => trimmed.to_string(),
    };
    out.push(RecipeStepSpec {
        text: final_text,
        ..Default::default()
    });
}

fn pick_string(value: Option<&Value>) -> Option<String> {
    value.and_then(pick_string_ref)
}

fn pick_string_ref(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.clone()),
        _ => None,
    }
}

fn pick_first_string(value: Option<&Value>) -> Option<String> {
    match value {
        Some(Value::String(s)) => Some(s.trim().to_string()).filter(|s| !s.is_empty()),
        Some(Value::Array(items)) => items
            .iter()
            .find_map(pick_string_ref)
            .map(|s| s.trim().to_string())
            .filter(|s| !s.is_empty()),
        _ => None,
    }
}

fn pick_keywords(value: Option<&Value>) -> Option<Vec<Value>> {
    match value {
        Some(Value::String(s)) => {
            let parts: Vec<Value> = s
                .split(',')
                .map(|p| p.trim())
                .filter(|p| !p.is_empty())
                .map(|p| Value::String(p.to_string()))
                .collect();
            if parts.is_empty() { None } else { Some(parts) }
        }
        Some(Value::Array(items)) => {
            let parts: Vec<Value> = items
                .iter()
                .filter_map(pick_string_ref)
                .map(|s| Value::String(s.trim().to_string()))
                .filter(|v| matches!(v, Value::String(s) if !s.is_empty()))
                .collect();
            if parts.is_empty() { None } else { Some(parts) }
        }
        _ => None,
    }
}

fn pick_image(value: Option<&Value>) -> Option<String> {
    match value {
        Some(Value::String(s)) => Some(s.clone()),
        Some(Value::Object(map)) => map.get("url").and_then(|v| v.as_str()).map(str::to_string),
        Some(Value::Array(items)) => items.iter().find_map(|item| pick_image(Some(item))),
        _ => None,
    }
}

fn extract_rating(value: Option<&Value>, warnings: &mut Vec<String>) -> Option<f32> {
    let raw = match value {
        Some(Value::Number(n)) => n.as_f64(),
        Some(Value::String(s)) => s.parse::<f64>().ok(),
        _ => None,
    }?;
    let raw = raw as f32;
    if !(0.0..=5.0).contains(&raw) {
        warnings.push(format!("ratingValue {raw} out of 0..=5, clamped"));
        Some(raw.clamp(0.0, 5.0))
    } else {
        Some(raw)
    }
}

fn extract_author_name(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.clone()),
        Value::Object(map) => map.get("name").and_then(|v| v.as_str()).map(str::to_string),
        Value::Array(items) => items.iter().find_map(extract_author_name),
        _ => None,
    }
}
