//! Recipe fulfillment — "can I make this from what's in
//! the pantry?". Pure functions over a parsed cooklang
//! recipe + the pantry snapshot.
//!
//! The match flow: each recipe ingredient (cooklang
//! `@name{qty%unit}`) is paired to a `pantry::PantryItem`
//! by **name** (case-insensitive, substring fallback). The
//! recipe carries no pantry IDs — cooklang files are pure
//! and portable, so the join is at the name layer. The
//! ingredient's qty + unit is compared to the pantry item's
//! `stock_total` + `unit` with unit conversion via
//! [`pantry::convert_str`].
//!
//! Output: [`Fulfillment { can_cook, missing }`] where
//! `missing` carries the per-ingredient shortfall so the
//! shopping-list builder can populate against it.
//!
//! Substitutions: see [`check_with_subs`] — pulls from the
//! pantry-item sub layer and the registry rule layer.
//! Recipe-level subs no longer exist (cooklang has no
//! native concept); encode them as pantry-side substitutes
//! or registry rules.

use cookbook::Recipe;
use facet::Facet;
use pantry::PantryItem;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Fulfillment {
    pub can_cook: bool,
    pub missing: Vec<Shortage>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Shortage {
    /// Recipe ingredient name (cooklang `@name`).
    pub name: String,

    #[serde(rename = "ingredientIdx")]
    pub ingredient_idx: u32,

    pub need: f64,
    pub have: f64,
    pub unit: String,
    pub reason: ShortageReason,

    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub suggestions: Vec<SubstitutionSuggestion>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct SubstitutionSuggestion {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "toItemId")]
    pub to_item_id: Option<uuid::Uuid>,
    pub ratio: f64,
    pub need: f64,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub have: Option<f64>,
    #[serde(default)]
    pub reasons: Vec<pantry::SubReason>,
    pub source: SubstitutionSource,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub note: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum SubstitutionSource {
    PantryItem,
    Registry,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum ShortageReason {
    NotInPantry,
    InsufficientQty,
    UnitMismatch,
    OptionalNoQty,
}

const MAX_NEST_DEPTH: u32 = 8;

#[must_use]
pub fn check(recipe: &Recipe, pantry: &[PantryItem], servings: u32) -> Fulfillment {
    let scale = scale_factor(recipe, servings);
    let mut missing = Vec::new();
    for (idx, ing) in recipe.ingredients.iter().enumerate() {
        check_one(ing, idx, scale, pantry, &mut missing);
    }
    Fulfillment {
        can_cook: missing.iter().all(|s| {
            !matches!(
                s.reason,
                ShortageReason::NotInPantry | ShortageReason::InsufficientQty
            )
        }),
        missing,
    }
}

fn scale_factor(recipe: &Recipe, servings: u32) -> f64 {
    let base = recipe.servings.filter(|s| *s > 0).unwrap_or(1);
    f64::from(servings) / f64::from(base)
}

fn check_one(
    ing: &cookbook::Ingredient,
    idx: usize,
    scale: f64,
    pantry: &[PantryItem],
    missing: &mut Vec<Shortage>,
) {
    let need = match ing.qty {
        Some(q) => q * scale,
        None if ing.optional => return,
        None => {
            missing.push(Shortage {
                name: ing.name.clone(),
                ingredient_idx: idx as u32,
                need: 0.0,
                have: 0.0,
                unit: ing.unit.clone(),
                reason: ShortageReason::OptionalNoQty,
                suggestions: Vec::new(),
            });
            return;
        }
    };

    match match_pantry(ing, pantry) {
        Some(item) => {
            let have_total = item.stock_total().unwrap_or(0.0);
            let have = pantry::convert_str(have_total, &item.unit, &ing.unit);
            match have {
                Some(h) if h + 1e-9 >= need => {}
                Some(h) => missing.push(Shortage {
                    name: ing.name.clone(),
                    ingredient_idx: idx as u32,
                    need,
                    have: h,
                    unit: ing.unit.clone(),
                    reason: ShortageReason::InsufficientQty,
                    suggestions: Vec::new(),
                }),
                None => missing.push(Shortage {
                    name: ing.name.clone(),
                    ingredient_idx: idx as u32,
                    need,
                    have: have_total,
                    unit: ing.unit.clone(),
                    reason: ShortageReason::UnitMismatch,
                    suggestions: Vec::new(),
                }),
            }
        }
        None if ing.optional => {}
        None => missing.push(Shortage {
            name: ing.name.clone(),
            ingredient_idx: idx as u32,
            need,
            have: 0.0,
            unit: ing.unit.clone(),
            reason: ShortageReason::NotInPantry,
            suggestions: Vec::new(),
        }),
    }
}

/// Check fulfillment with nested-recipe support. Recurses
/// through `Recipe::nested_recipes` (cooklang `@@./path{}`)
/// up to [`MAX_NEST_DEPTH`] levels. `all_recipes` must
/// include every nested recipe by `path`; missing paths are
/// silently skipped.
#[must_use]
pub fn check_nested(
    recipe: &Recipe,
    all_recipes: &[Recipe],
    pantry: &[PantryItem],
    servings: u32,
) -> Fulfillment {
    use std::collections::HashMap;
    let index: HashMap<&str, &Recipe> = all_recipes.iter().map(|r| (r.path.as_str(), r)).collect();

    let scale = scale_factor(recipe, servings);
    let mut visited = std::collections::HashSet::new();
    let mut flat = flatten(recipe, &index, scale, &mut visited, 0);
    fold_same_ingredient(&mut flat);

    let synthetic = Recipe {
        ingredients: cookbook::Ingredients(flat),
        servings: Some(1),
        ..recipe.clone()
    };
    check(&synthetic, pantry, 1)
}

fn flatten(
    recipe: &Recipe,
    index: &std::collections::HashMap<&str, &Recipe>,
    scale: f64,
    visited: &mut std::collections::HashSet<String>,
    depth: u32,
) -> Vec<cookbook::Ingredient> {
    if depth > MAX_NEST_DEPTH || visited.contains(&recipe.path) {
        return Vec::new();
    }
    visited.insert(recipe.path.clone());

    let mut out: Vec<cookbook::Ingredient> = recipe
        .ingredients
        .iter()
        .map(|ing| cookbook::Ingredient {
            qty: ing.qty.map(|q| q * scale),
            ..ing.clone()
        })
        .collect();

    for nested_path in recipe.nested_recipes.iter() {
        if let Some(child) = index.get(nested_path.as_str()) {
            let base = f64::from(child.servings.unwrap_or(1).max(1));
            let child_scale = scale / base;
            out.extend(flatten(child, index, child_scale, visited, depth + 1));
        }
    }

    visited.remove(&recipe.path);
    out
}

fn fold_same_ingredient(rows: &mut Vec<cookbook::Ingredient>) {
    let mut i = 0;
    while i < rows.len() {
        let mut j = i + 1;
        while j < rows.len() {
            let merge_ok = rows[i].name.eq_ignore_ascii_case(&rows[j].name)
                && rows[i].unit.eq_ignore_ascii_case(&rows[j].unit);
            if merge_ok {
                let add = rows[j].qty.unwrap_or(0.0);
                let base = rows[i].qty.unwrap_or(0.0);
                rows[i].qty = Some(base + add);
                rows.remove(j);
            } else {
                j += 1;
            }
        }
        i += 1;
    }
}

#[must_use]
pub fn check_with_subs(
    recipe: &Recipe,
    pantry: &[PantryItem],
    rules: &[crate::substitutions::SubstitutionRule],
    goals: &[pantry::SubReason],
) -> Fulfillment {
    let mut base = check(recipe, pantry, recipe.servings.unwrap_or(1));
    let pantry_by_id: std::collections::HashMap<uuid::Uuid, &PantryItem> =
        pantry.iter().map(|p| (p.id, p)).collect();

    for short in &mut base.missing {
        let idx = short.ingredient_idx as usize;
        let Some(ing) = recipe.ingredients.get(idx) else {
            continue;
        };
        let Some(primary) = match_pantry(ing, pantry) else {
            continue;
        };

        for sub in &primary.substitutes {
            if let Some(target) = pantry_by_id.get(&sub.item_id) {
                let need = short.need * sub.ratio;
                let have = pantry::convert_str(
                    target.stock_total().unwrap_or(0.0),
                    &target.unit,
                    &short.unit,
                );
                short.suggestions.push(SubstitutionSuggestion {
                    name: target.name.clone(),
                    to_item_id: Some(target.id),
                    ratio: sub.ratio,
                    need,
                    have,
                    reasons: sub.reasons.clone(),
                    source: SubstitutionSource::PantryItem,
                    note: sub.note.clone(),
                });
            }
        }

        for rule in rules.iter().filter(|r| r.from_item_id == primary.id) {
            if let Some(target) = pantry_by_id.get(&rule.to_item_id) {
                let need = short.need * rule.ratio;
                let have = pantry::convert_str(
                    target.stock_total().unwrap_or(0.0),
                    &target.unit,
                    &short.unit,
                );
                short.suggestions.push(SubstitutionSuggestion {
                    name: target.name.clone(),
                    to_item_id: Some(target.id),
                    ratio: rule.ratio,
                    need,
                    have,
                    reasons: rule.reasons.clone(),
                    source: SubstitutionSource::Registry,
                    note: rule.note.clone(),
                });
            }
        }

        if goals.is_empty() {
            short.suggestions.sort_by_key(|s| {
                let oos = !s.reasons.contains(&pantry::SubReason::OutOfStock);
                let layer = match s.source {
                    SubstitutionSource::PantryItem => 0,
                    SubstitutionSource::Registry => 1,
                };
                (oos, layer)
            });
        } else {
            short
                .suggestions
                .retain(|s| s.reasons.iter().any(|r| goals.contains(r)));
            short.suggestions.sort_by(|a, b| {
                let a_score = a.reasons.iter().filter(|r| goals.contains(r)).count();
                let b_score = b.reasons.iter().filter(|r| goals.contains(r)).count();
                b_score.cmp(&a_score)
            });
        }
    }

    base
}

fn match_pantry<'p>(
    ing: &cookbook::Ingredient,
    pantry: &'p [PantryItem],
) -> Option<&'p PantryItem> {
    let needle = ing
        .name
        .trim()
        .trim_start_matches("[[")
        .trim_end_matches("]]")
        .to_ascii_lowercase();
    if needle.is_empty() {
        return None;
    }
    pantry
        .iter()
        .find(|p| p.name.eq_ignore_ascii_case(&needle))
        .or_else(|| {
            pantry
                .iter()
                .find(|p| p.name.to_ascii_lowercase().contains(&needle))
        })
        .or_else(|| {
            pantry
                .iter()
                .find(|p| needle.contains(&p.name.to_ascii_lowercase()))
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::NaiveDate;
    use cookbook::Ingredient;
    use uuid::Uuid;

    fn pantry_row(name: &str, qty: f64, unit: &str) -> PantryItem {
        let mut row = pantry::PantryItem::from_item(inventory::Item {
            path: String::new(),
            id: Uuid::new_v4(),
            name: name.to_string(),
            category: "food".into(),
            location_id: None,
            condition: "good".into(),
            status: "stored".into(),
            manufacturer: None,
            model: None,
            serial: None,
            purchase_date: None,
            value: None,
            tasks: inventory::model::StringList::default(),
            tags: inventory::model::StringList(vec!["item".into(), "pantry".into()]),
            date_created: None,
            date_modified: None,
            details: String::new(),
        });
        row.qty = Some(qty);
        row.unit = unit.to_string();
        row.stock_entries.push(pantry::StockEntry {
            id: Uuid::new_v4(),
            qty,
            purchased_date: NaiveDate::from_ymd_opt(2026, 1, 1).unwrap(),
            best_before: None,
            opened: false,
            opened_date: None,
            price: None,
            location_id: None,
            note: None,
        });
        row
    }

    fn recipe_with(path: &str, ings: Vec<Ingredient>, servings: u32) -> Recipe {
        Recipe {
            path: path.into(),
            name: "test".into(),
            description: None,
            course: None,
            cuisine: None,
            prep_minutes: None,
            cook_minutes: None,
            servings: Some(servings),
            ingredients: cookbook::Ingredients(ings),
            steps: cookbook::StringList::default(),
            cookware: cookbook::StringList::default(),
            nested_recipes: cookbook::StringList::default(),
            tags: cookbook::StringList::default(),
            source_url: None,
            date_modified: None,
            source: String::new(),
        }
    }

    fn ing(name: &str, qty: f64, unit: &str) -> Ingredient {
        Ingredient {
            name: name.into(),
            alias: None,
            qty: Some(qty),
            unit: unit.into(),
            qty_display: None,
            note: None,
            optional: false,
            is_recipe_ref: false,
        }
    }

    #[test]
    fn can_cook_when_stock_sufficient() {
        let r = recipe_with("Cookbook/X.cook", vec![ing("Pasta", 200.0, "g")], 2);
        let s = vec![pantry_row("Pasta", 500.0, "g")];
        assert!(check(&r, &s, 2).can_cook);
    }

    #[test]
    fn scales_with_servings() {
        let r = recipe_with("Cookbook/X.cook", vec![ing("Pasta", 200.0, "g")], 2);
        let s = vec![pantry_row("Pasta", 300.0, "g")];
        let f = check(&r, &s, 4);
        assert!(!f.can_cook);
        assert!(matches!(
            f.missing[0].reason,
            ShortageReason::InsufficientQty
        ));
    }

    #[test]
    fn cross_unit_with_conversion() {
        let r = recipe_with("Cookbook/X.cook", vec![ing("Olive Oil", 30.0, "ml")], 1);
        let s = vec![pantry_row("Olive Oil", 1.0, "l")];
        assert!(check(&r, &s, 1).can_cook);
    }

    #[test]
    fn missing_item_surfaces() {
        let r = recipe_with("Cookbook/X.cook", vec![ing("Truffles", 5.0, "g")], 1);
        let s = vec![pantry_row("Pasta", 500.0, "g")];
        let f = check(&r, &s, 1);
        assert!(matches!(f.missing[0].reason, ShortageReason::NotInPantry));
    }

    #[test]
    fn optional_ingredient_doesnt_block() {
        let mut opt = ing("Truffles", 5.0, "g");
        opt.optional = true;
        let r = recipe_with("Cookbook/X.cook", vec![ing("Pasta", 100.0, "g"), opt], 1);
        let s = vec![pantry_row("Pasta", 500.0, "g")];
        assert!(check(&r, &s, 1).can_cook);
    }

    #[test]
    fn nested_recipe_aggregates_ingredients() {
        let mut dough = recipe_with("Cookbook/Dough.cook", vec![ing("Flour", 200.0, "g")], 1);
        dough.name = "Pizza Dough".into();
        let mut pizza = recipe_with("Cookbook/Pizza.cook", vec![ing("Flour", 100.0, "g")], 1);
        pizza.name = "Pizza".into();
        pizza.nested_recipes = cookbook::StringList(vec!["Cookbook/Dough.cook".into()]);
        let s = vec![pantry_row("Flour", 250.0, "g")];
        let f = check_nested(&pizza, &[pizza.clone(), dough], &s, 1);
        assert!(!f.can_cook);
        assert!((f.missing[0].need - 300.0).abs() < 1e-6);
    }

    #[test]
    fn cycle_guard() {
        let mut a = recipe_with("Cookbook/A.cook", vec![ing("X", 1.0, "g")], 1);
        let mut b = recipe_with("Cookbook/B.cook", vec![ing("Y", 1.0, "g")], 1);
        a.nested_recipes = cookbook::StringList(vec!["Cookbook/B.cook".into()]);
        b.nested_recipes = cookbook::StringList(vec!["Cookbook/A.cook".into()]);
        let s = vec![pantry_row("X", 10.0, "g"), pantry_row("Y", 10.0, "g")];
        assert!(check_nested(&a, &[a.clone(), b.clone()], &s, 1).can_cook);
    }
}
