//! `vault::VaultPage` → `Recipe`.
//!
//! Discriminator: `type: recipe` in the frontmatter (or
//! `recipe` in `tags:`). Missing optional fields fall back
//! to defaults.

use thiserror::Error;
use uuid::Uuid;
use vault::VaultPage;

use crate::model::{Ingredient, Nutrition, Recipe};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("page has no frontmatter")]
    NoFrontmatter,
    #[error("frontmatter is not a YAML mapping")]
    NotAMapping,
    #[error("frontmatter parse: {0}")]
    Yaml(String),
}

pub fn looks_like_recipe(page: &VaultPage) -> bool {
    let Some((fm, _)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let Ok(map) = serde_yaml::from_str::<serde_yaml::Mapping>(fm) else {
        return false;
    };
    if map.get("type").and_then(|v| v.as_str()) == Some("recipe") {
        return true;
    }
    if let Some(seq) = map.get("tags").and_then(|v| v.as_sequence()) {
        return seq.iter().any(|v| v.as_str() == Some("recipe"));
    }
    false
}

pub fn parse_page(page: &VaultPage) -> Result<Recipe, ParseError> {
    let (fm, body) = split_frontmatter(&page.raw).ok_or(ParseError::NoFrontmatter)?;
    let map: serde_yaml::Mapping =
        serde_yaml::from_str(fm).map_err(|e| ParseError::Yaml(e.to_string()))?;

    let id = take_str(&map, "id")
        .and_then(|s| Uuid::parse_str(&s).ok())
        .unwrap_or_else(|| Uuid::new_v5(&Uuid::NAMESPACE_URL, page.rel_path.as_bytes()));
    let name = take_str(&map, "name").unwrap_or_else(|| page.basename.clone());
    let description = take_str(&map, "description");
    let course = take_str(&map, "course").unwrap_or_else(|| "main".into());
    let cuisine = take_str(&map, "cuisine");
    let prep_minutes = map
        .get("prepMinutes")
        .and_then(|v| v.as_u64())
        .and_then(|n| u32::try_from(n).ok());
    let cook_minutes = map
        .get("cookMinutes")
        .and_then(|v| v.as_u64())
        .and_then(|n| u32::try_from(n).ok());
    let servings = map
        .get("servings")
        .and_then(|v| v.as_u64())
        .and_then(|n| u32::try_from(n).ok());
    let ingredients = parse_ingredients(&map);
    let steps = take_string_list(&map, "steps");
    let nutrition = map
        .get("nutrition")
        .and_then(|v| serde_yaml::from_value::<Nutrition>(v.clone()).ok());
    let tags = take_string_list(&map, "tags")
        .into_iter()
        .filter(|t| t != "recipe")
        .collect();
    let source = take_str(&map, "source");
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    Ok(Recipe {
        path: page.rel_path.clone(),
        id,
        name,
        description,
        course,
        cuisine,
        prep_minutes,
        cook_minutes,
        servings,
        ingredients,
        steps,
        nutrition,
        tags,
        source,
        date_created,
        date_modified,
        details: body.to_string(),
    })
}

fn parse_ingredients(map: &serde_yaml::Mapping) -> Vec<Ingredient> {
    let Some(seq) = map.get("ingredients").and_then(|v| v.as_sequence()) else {
        return Vec::new();
    };
    seq.iter()
        .filter_map(|row| {
            // Tolerate string-only shorthand:
            //   ingredients: ["1 tbsp olive oil"]
            // (parsed best-effort; agents that need structure
            // should round-trip through the typed shape).
            if let Some(s) = row.as_str() {
                return Some(Ingredient {
                    name: s.to_string(),
                    qty: None,
                    unit: String::new(),
                    note: None,
                    optional: false,
                });
            }
            let m = row.as_mapping()?;
            let name = m.get("name").and_then(|v| v.as_str())?.to_string();
            let qty = m.get("qty").and_then(|v| v.as_f64());
            let unit = m
                .get("unit")
                .and_then(|v| v.as_str())
                .unwrap_or_default()
                .to_string();
            let note = m
                .get("note")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            let optional = m.get("optional").and_then(|v| v.as_bool()).unwrap_or(false);
            Some(Ingredient {
                name,
                qty,
                unit,
                note,
                optional,
            })
        })
        .collect()
}

pub(crate) fn split_frontmatter(src: &str) -> Option<(&str, &str)> {
    let rest = src.strip_prefix("---\n")?;
    let end = rest.find("\n---\n")?;
    Some((&rest[..end], &rest[end + 5..]))
}

fn take_str(map: &serde_yaml::Mapping, key: &str) -> Option<String> {
    map.get(key).and_then(|v| match v {
        serde_yaml::Value::String(s) => Some(s.clone()),
        serde_yaml::Value::Number(n) => Some(n.to_string()),
        serde_yaml::Value::Bool(b) => Some(b.to_string()),
        _ => None,
    })
}

fn take_string_list(map: &serde_yaml::Mapping, key: &str) -> Vec<String> {
    let Some(v) = map.get(key) else {
        return Vec::new();
    };
    match v {
        serde_yaml::Value::Sequence(seq) => seq
            .iter()
            .filter_map(|item| item.as_str().map(|s| s.to_string()))
            .collect(),
        serde_yaml::Value::String(s) => vec![s.clone()],
        _ => Vec::new(),
    }
}
