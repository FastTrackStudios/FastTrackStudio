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

use crate::model::{CookStep, CookSteps, Ingredient, Recipe, RecipeTimer, StringList};

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
        .map_or_else(|| basename_of(path), str::to_string);
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

    let cookware = parsed.cookware.iter().map(|c| c.name.clone()).collect();

    // Every `@@` reference, whether or not it carries a path. Cooklang
    // only builds a `reference` for a path-ish form like `@@./sauce`;
    // a bare `@@sauce` is still a recipe reference, just without one.
    // Collecting the name in that case lets the resolver find it by
    // stem instead of dropping the link on the floor.
    let mut nested_recipes: Vec<String> = parsed
        .ingredients
        .iter()
        .filter(|i| i.modifiers().contains(cooklang::Modifiers::RECIPE))
        .map(|i| {
            i.reference
                .as_ref()
                .map_or_else(|| i.name.clone(), |r| r.path("/"))
        })
        .collect();

    // …plus the vault's own link form, `[[Hot Honey]]{6}`. This is the
    // spelling to prefer: it is the same syntax every other note in the
    // vault uses to point at a page, so the cookbook participates in
    // the wiki graph instead of carrying a private path convention.
    // Cooklang passes `[[…]]` through as plain text, so each one also
    // becomes a synthetic recipe-ref ingredient — that is the shape a
    // `@@ref` already produces, and it lets fulfillment treat both
    // identically (see `mealplan::fulfillment::flatten`).
    let mut ingredients: Vec<Ingredient> = parsed
        .ingredients
        .iter()
        .filter(|i| i.modifiers().should_be_listed())
        .map(project_ingredient)
        .collect();
    for link in crate::wiki::scan_recipe_links(source) {
        if nested_recipes
            .iter()
            .any(|n| n.eq_ignore_ascii_case(&link.target))
        {
            continue;
        }
        ingredients.push(Ingredient {
            name: link.target.clone(),
            alias: None,
            qty: link.servings,
            unit: String::new(),
            qty_display: link.servings.map(|q| format!("{q}")),
            note: None,
            optional: false,
            is_recipe_ref: true,
        });
        nested_recipes.push(link.target);
    }
    let nested_recipes: StringList = nested_recipes.into_iter().collect();

    // Structured steps: ingredient / cookware / timer names kept inline
    // (no more `·` placeholders) and timers extracted. `steps` is the
    // same text, kept for the existing index/grep/wiki consumers.
    // Each step carries the name of the `= Section` it sits under, so
    // cook mode can walk "Prep" and "Cook" as separate phases. An
    // unnamed section (or a recipe with no `=` headings at all) leaves
    // `section: None` — one anonymous run of steps, exactly as before.
    let cook_steps: Vec<CookStep> = parsed
        .sections
        .iter()
        .flat_map(|s| {
            let name = s.name.clone().filter(|n| !n.trim().is_empty());
            s.content.iter().map(move |c| (name.clone(), c))
        })
        .filter_map(|(section, c)| {
            project_content(c, &parsed).map(|step| CookStep { section, ..step })
        })
        .collect();
    let steps: StringList = cook_steps.iter().map(|s| s.text.clone()).collect();

    Ok(Recipe {
        path: path.to_string(),
        name,
        description,
        course,
        cuisine,
        prep_minutes,
        cook_minutes,
        servings,
        ingredients: ingredients.into_iter().collect(),
        steps,
        cook_steps: CookSteps::from(cook_steps),
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
        Value::Range { start, end } => Some(f64::midpoint(f64::from(*start), f64::from(*end))),
        Value::Text(_) => None,
    }
}

fn project_content(c: &cooklang::Content, recipe: &cooklang::Recipe) -> Option<CookStep> {
    match c {
        cooklang::Content::Step(step) => Some(project_step(step, recipe)),
        cooklang::Content::Text(t) => {
            let trimmed = t.trim();
            if trimmed.is_empty() {
                None
            } else {
                Some(CookStep {
                    text: trimmed.to_string(),
                    timers: Vec::new(),
                    section: None,
                })
            }
        }
    }
}

/// Render one step to readable text, resolving each `Item`'s index into
/// the recipe-level component vecs so ingredient / cookware / timer
/// names land inline, and collecting the step's timers as structured
/// [`RecipeTimer`]s for one-tap countdowns.
fn project_step(step: &cooklang::Step, recipe: &cooklang::Recipe) -> CookStep {
    let mut text = String::new();
    let mut timers = Vec::new();
    for item in &step.items {
        match item {
            cooklang::Item::Text { value } => text.push_str(value),
            cooklang::Item::Ingredient { index } => {
                if let Some(ing) = recipe.ingredients.get(*index) {
                    text.push_str(ing.alias.as_deref().unwrap_or(&ing.name));
                }
            }
            cooklang::Item::Cookware { index } => {
                if let Some(cw) = recipe.cookware.get(*index) {
                    text.push_str(&cw.name);
                }
            }
            cooklang::Item::Timer { index } => {
                if let Some(timer) = recipe.timers.get(*index) {
                    let projected = project_timer(timer);
                    text.push_str(&projected.display);
                    timers.push(projected);
                }
            }
            cooklang::Item::InlineQuantity { index } => {
                if let Some(q) = recipe.inline_quantities.get(*index) {
                    text.push_str(&q.to_string());
                }
            }
        }
    }
    CookStep {
        text: text.trim().to_string(),
        timers,
        // Filled in by the caller, which knows the enclosing section.
        section: None,
    }
}

/// A cooklang `~name{qty%unit}` timer → our [`RecipeTimer`]. Converts
/// the quantity to whole seconds (a bare/unknown unit is read as
/// minutes — the cooking default).
fn project_timer(t: &cooklang::Timer) -> RecipeTimer {
    let (seconds, display) = match &t.quantity {
        Some(q) => (timer_seconds(q), q.to_string()),
        None => (0, t.name.clone().unwrap_or_default()),
    };
    RecipeTimer {
        name: t.name.clone(),
        seconds,
        display,
    }
}

fn timer_seconds(q: &cooklang::Quantity) -> u32 {
    let Some(val) = number_value(q.value()) else {
        return 0;
    };
    let mult = match q.unit().map(|u| u.trim().to_ascii_lowercase()).as_deref() {
        Some("s" | "sec" | "secs" | "second" | "seconds") => 1.0,
        Some("h" | "hr" | "hrs" | "hour" | "hours") => 3600.0,
        // minutes, plus the unitless default
        _ => 60.0,
    };
    (val * mult).round().clamp(0.0, f64::from(u32::MAX)) as u32
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
        // Ingredient names are inlined into the step text now (no `·`).
        assert_eq!(r.steps[0], "Boil pasta in salted water.");
        assert!(!r.steps[0].contains('·'));
    }

    #[test]
    fn extracts_timers_and_inlines_names() {
        let src = "\
>> title: Steeped Tea

Boil @water{500%ml} in a #kettle, then steep the @tea bag{1} for ~steep{4%minutes}.

Rest the cup for ~{30%seconds} before sipping.";
        let r = parse_cook("Cookbook/Tea.cook", src).unwrap();
        assert_eq!(r.cook_steps.len(), 2);

        // Step 1: names inlined, one named timer = 4 minutes = 240s.
        let s1 = &r.cook_steps[0];
        assert!(s1.text.contains("Boil water"), "{}", s1.text);
        assert!(s1.text.contains("kettle"), "{}", s1.text);
        assert_eq!(s1.timers.len(), 1);
        assert_eq!(s1.timers[0].name.as_deref(), Some("steep"));
        assert_eq!(s1.timers[0].seconds, 240);

        // Step 2: a bare timer in seconds.
        let s2 = &r.cook_steps[1];
        assert_eq!(s2.timers.len(), 1);
        assert_eq!(s2.timers[0].name, None);
        assert_eq!(s2.timers[0].seconds, 30);

        // `steps` mirrors `cook_steps` text.
        assert_eq!(r.steps.len(), r.cook_steps.len());
        assert_eq!(r.steps[0], r.cook_steps[0].text);
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
        assert_eq!(r.tags.0, vec!["weeknight", "pasta"]);
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

    #[test]
    fn steps_carry_their_section_name() {
        let src = "\
>> title: Sectioned

= Prep

Chop @onion{1}.

= Cook

Fry the onion for ~{5%min}.

Season and serve.
";
        let r = parse_cook("Cookbook/Sectioned.cook", src).unwrap();
        let sections: Vec<_> = r.cook_steps.iter().map(|s| s.section.as_deref()).collect();
        assert_eq!(
            sections,
            vec![Some("Prep"), Some("Cook"), Some("Cook")],
            "each step should report the `= heading` it sits under"
        );
    }

    #[test]
    fn unsectioned_recipe_leaves_section_none() {
        let r = parse_cook("Cookbook/Flat.cook", "Boil @pasta{200%g}.\nDrain it.").unwrap();
        assert!(
            r.cook_steps.iter().all(|s| s.section.is_none()),
            "a recipe with no `=` headings stays one anonymous run of steps"
        );
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
