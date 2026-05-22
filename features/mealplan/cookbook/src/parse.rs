//! Cooklang source → [`Recipe`].
//!
//! Wraps `cooklang::CooklangParser` (all extensions enabled,
//! bundled units). Projects the parsed AST into our flat wire
//! shape: a list of ingredients with numeric quantities (when
//! possible), a list of rendered step strings, and metadata
//! lifted from the `>> key: value` block.

use chrono::{DateTime, Utc};
use cooklang::{Converter, CooklangParser, Extensions, Value};
use thiserror::Error;

use crate::model::{Ingredient, Recipe};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("cooklang parse failed: {0}")]
    Cooklang(String),
}

/// Parse a `.cook` source string into a [`Recipe`].
pub fn parse_cook(path: &str, source: &str) -> Result<Recipe, ParseError> {
    parse_cook_at(path, source, None)
}

/// Like [`parse_cook`] but stamps `date_modified` from the
/// caller (typically the file's mtime).
pub fn parse_cook_at(
    path: &str,
    source: &str,
    date_modified: Option<DateTime<Utc>>,
) -> Result<Recipe, ParseError> {
    let parser = parser();
    let (parsed, _report) = parser
        .parse(source)
        .into_result()
        .map_err(|e| ParseError::Cooklang(format!("{e:?}")))?;

    let name = parsed
        .metadata
        .title()
        .map(str::to_string)
        .unwrap_or_else(|| basename_of(path));
    let description = parsed.metadata.description().map(str::to_string);
    let course = take_meta_str(&parsed.metadata, "course");
    let cuisine = take_meta_str(&parsed.metadata, "cuisine");
    let tags = parsed
        .metadata
        .tags()
        .map(|ts| ts.into_iter().map(|s| s.into_owned()).collect())
        .unwrap_or_default();
    let source_url = parsed.metadata.source().and_then(|s| {
        s.url()
            .map(str::to_string)
            .or_else(|| s.name().map(str::to_string))
    });

    let (prep_minutes, cook_minutes) = match parsed.metadata.time(parser.converter()) {
        Some(cooklang::metadata::RecipeTime::Total(t)) => (None, Some(t)),
        Some(cooklang::metadata::RecipeTime::Composed {
            prep_time,
            cook_time,
        }) => (prep_time, cook_time),
        None => (None, None),
    };

    let servings = parsed.metadata.servings().and_then(|s| s.as_number());

    let ingredients = parsed
        .ingredients
        .iter()
        .filter(|i| i.modifiers().should_be_listed())
        .map(project_ingredient)
        .collect();

    let cookware = parsed.cookware.iter().map(|c| c.name.clone()).collect();

    let nested_recipes = parsed
        .ingredients
        .iter()
        .filter_map(|i| i.reference.as_ref().map(|r| r.path("/")))
        .collect();

    let steps = parsed
        .sections
        .iter()
        .flat_map(|s| s.content.iter())
        .filter_map(render_content)
        .collect();

    Ok(Recipe {
        path: path.to_string(),
        name,
        description,
        course,
        cuisine,
        prep_minutes,
        cook_minutes,
        servings,
        ingredients,
        steps,
        cookware,
        nested_recipes,
        tags,
        source_url,
        date_modified,
        source: source.to_string(),
    })
}

fn project_ingredient(i: &cooklang::Ingredient) -> Ingredient {
    let (qty, unit, qty_display) = match &i.quantity {
        Some(q) => {
            let unit = q.unit().unwrap_or_default().to_string();
            let qty = number_value(q.value());
            let display = Some(format!("{}", q.value()));
            (qty, unit, display)
        }
        None => (None, String::new(), None),
    };
    Ingredient {
        name: i.name.clone(),
        alias: i.alias.clone(),
        qty,
        unit,
        qty_display,
        note: i.note.clone(),
        optional: i.modifiers().contains(cooklang::Modifiers::OPT),
        is_recipe_ref: i.modifiers().contains(cooklang::Modifiers::RECIPE),
    }
}

fn number_value(v: &Value) -> Option<f64> {
    match v {
        Value::Number(n) => Some(n.value()),
        Value::Range { start, end } => Some((f64::from(*start) + f64::from(*end)) / 2.0),
        Value::Text(_) => None,
    }
}

fn render_content(c: &cooklang::Content) -> Option<String> {
    match c {
        cooklang::Content::Step(step) => Some(render_step(step)),
        cooklang::Content::Text(t) => {
            let trimmed = t.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(trimmed.to_string())
            }
        }
    }
}

fn render_step(step: &cooklang::Step) -> String {
    // Items reference the recipe-level vecs by index. We don't
    // have those vecs in scope; render plain text and drop a
    // bullet for non-text items. Editors / UI re-parse via
    // cooklang directly for the rich form. `Recipe::steps` is
    // for index views + grep.
    let mut out = String::new();
    for item in &step.items {
        match item {
            cooklang::Item::Text { value } => out.push_str(value),
            cooklang::Item::Ingredient { .. }
            | cooklang::Item::Cookware { .. }
            | cooklang::Item::Timer { .. }
            | cooklang::Item::InlineQuantity { .. } => out.push('·'),
        }
    }
    out.trim().to_string()
}

fn take_meta_str(m: &cooklang::Metadata, key: &str) -> Option<String> {
    m.get(key).and_then(|v| v.as_str()).map(str::to_string)
}

fn basename_of(path: &str) -> String {
    std::path::Path::new(path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or(path)
        .to_string()
}

fn parser() -> &'static CooklangParser {
    use std::sync::OnceLock;
    static P: OnceLock<CooklangParser> = OnceLock::new();
    P.get_or_init(|| CooklangParser::new(Extensions::all(), Converter::bundled()))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_minimal_recipe() {
        let src = ">> title: Pasta\n>> servings: 2\n\nBoil @pasta{200%g} in salted water.";
        let r = parse_cook("Cookbook/Pasta.cook", src).expect("parse");
        assert_eq!(r.name, "Pasta");
        assert_eq!(r.servings, Some(2));
        assert_eq!(r.ingredients.len(), 1);
        assert_eq!(r.ingredients[0].name, "pasta");
        assert_eq!(r.ingredients[0].qty, Some(200.0));
        assert_eq!(r.ingredients[0].unit, "g");
        assert_eq!(r.steps.len(), 1);
    }

    #[test]
    fn falls_back_to_filename_for_title() {
        let r = parse_cook("Cookbook/Truffle Pasta.cook", "Just cook it.").unwrap();
        assert_eq!(r.name, "Truffle Pasta");
    }

    #[test]
    fn parses_metadata_block() {
        let src = "\
>> title: Carbonara
>> description: Roman classic
>> course: dinner
>> cuisine: italian
>> servings: 4
>> prep time: 5 min
>> cook time: 15 min
>> tags: weeknight, pasta

Cook the @pasta{400%g}.
";
        let r = parse_cook("Cookbook/Carbonara.cook", src).unwrap();
        assert_eq!(r.name, "Carbonara");
        assert_eq!(r.description.as_deref(), Some("Roman classic"));
        assert_eq!(r.course.as_deref(), Some("dinner"));
        assert_eq!(r.cuisine.as_deref(), Some("italian"));
        assert_eq!(r.servings, Some(4));
        assert_eq!(r.prep_minutes, Some(5));
        assert_eq!(r.cook_minutes, Some(15));
        assert_eq!(r.tags, vec!["weeknight", "pasta"]);
    }

    #[test]
    fn optional_ingredient_modifier() {
        let r = parse_cook("Cookbook/X.cook", "Top with @?parmesan{}.").unwrap();
        assert_eq!(r.ingredients.len(), 1);
        assert!(r.ingredients[0].optional);
    }

    #[test]
    fn recipe_reference_is_collected() {
        let r = parse_cook(
            "Cookbook/Pizza.cook",
            "Make @@./Shared/Pizza Dough{}, then top.",
        )
        .unwrap();
        assert!(!r.nested_recipes.is_empty());
    }
}

#[cfg(test)]
mod migration_check {
    use super::*;
    #[test]
    fn parses_multiword_ingredients_from_migration() {
        // Output shape produced by migrate-md-to-cook.
        let src = "\
>> title: Truffle Pasta
>> servings: 2

Cook @Pasta{200%g} for 8 minutes.
Drain and toss with @Olive Oil{30%ml} and @Truffles{5%g}.
";
        let r = parse_cook("Cookbook/Truffle Pasta.cook", src).unwrap();
        assert_eq!(
            r.ingredients.len(),
            3,
            "got {:?}",
            r.ingredients.iter().map(|i| &i.name).collect::<Vec<_>>()
        );
        assert!(
            r.ingredients
                .iter()
                .any(|i| i.name.eq_ignore_ascii_case("olive oil")),
            "olive oil missing"
        );
    }
}
