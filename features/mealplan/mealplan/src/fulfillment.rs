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
use pantry::PantryItem;

// The fulfillment **wire types** now live in the wasm-clean
// `mealplan-proto` crate (so `MealplanService::can_cook` can
// return `Fulfillment` from a wasm client). The pure matching
// logic below stays here — it needs the native `pantry`
// unit-conversion layer. Re-exported so existing
// `mealplan::fulfillment::*` / `mealplan::Fulfillment` paths keep
// working.
pub use mealplan_proto::fulfillment::{
    CookReceipt, DeductionLine, Fulfillment, Shortage, ShortageReason, SkipReason,
    SkippedIngredient, SubstitutionSource, SubstitutionSuggestion,
};

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

/// A full cook receipt for `recipe` at `servings`: for each ingredient
/// matched to a pantry item with a convertible unit, the amount to
/// consume **in the pantry item's unit**, capped at what's in stock.
/// Cooking never invents a deduction it can't compute safely — but
/// instead of silently dropping those ingredients it records each in
/// [`CookReceipt::skipped`] with the reason (no quantity, no pantry
/// match, inconvertible unit, or out of stock) so the cook can see what
/// to top up by hand. The `deducted` rows feed `PantryService::consume`.
#[must_use]
pub fn plan_cook(recipe: &Recipe, pantry: &[PantryItem], servings: u32) -> CookReceipt {
    let scale = scale_factor(recipe, servings);
    let mut receipt = CookReceipt::default();
    for ing in recipe.ingredients.iter() {
        // Resolve the deduction (pushed inline), or the reason it
        // can't be made. `None` ⇒ deducted; `Some(reason)` ⇒ skipped.
        let skip = 'plan: {
            let Some(need) = ing.qty.map(|q| q * scale) else {
                break 'plan Some(SkipReason::NoQuantity);
            };
            let Some(item) = match_pantry(ing, pantry) else {
                break 'plan Some(SkipReason::NoPantryMatch);
            };
            let Some(in_item_unit) = pantry::convert_str(need, &ing.unit, &item.unit) else {
                break 'plan Some(SkipReason::InconvertibleUnit);
            };
            let available = item.stock_total().unwrap_or(0.0);
            let qty = in_item_unit.min(available);
            if qty <= 1e-9 {
                break 'plan Some(SkipReason::OutOfStock);
            }
            receipt.deducted.push(DeductionLine {
                item_id: item.id,
                ingredient: ing.name.clone(),
                qty,
                unit: item.unit.clone(),
            });
            None
        };
        if let Some(reason) = skip {
            receipt.skipped.push(SkippedIngredient {
                ingredient: ing.name.clone(),
                reason,
            });
        }
    }
    receipt
}

/// The pantry deductions for cooking `recipe` at `servings`, as the
/// plain `PantryDeduction` rows the meal-`cook` path stamps onto a
/// meal. A thin projection of [`plan_cook`] — see it for the matching
/// rules and the skipped-ingredient accounting.
#[must_use]
pub fn plan_deductions(
    recipe: &Recipe,
    pantry: &[PantryItem],
    servings: u32,
) -> Vec<crate::model::PantryDeduction> {
    plan_cook(recipe, pantry, servings)
        .deducted
        .into_iter()
        .map(|line| crate::model::PantryDeduction {
            item_id: line.item_id,
            qty: line.qty,
            unit: line.unit,
        })
        .collect()
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

        for sub in primary.substitutes.iter() {
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
                    reasons: rule.reasons.0.clone(),
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
            cook_steps: Default::default(),
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
    fn plan_deductions_scales_caps_and_skips() {
        let r = recipe_with(
            "Cookbook/X.cook",
            vec![
                ing("Pasta", 200.0, "g"), // matched, in stock
                ing("Salt", 5.0, "g"),    // matched but only 2g in stock → capped
                ing("Saffron", 1.0, "g"), // not in pantry → skipped
            ],
            2,
        );
        let pantry = vec![
            pantry_row("Pasta", 500.0, "g"),
            pantry_row("Salt", 2.0, "g"),
        ];

        // Doubling the servings (base 2 → 4) doubles the needs.
        let plan = plan_deductions(&r, &pantry, 4);
        assert_eq!(plan.len(), 2, "saffron has no pantry match → no deduction");

        let pasta = plan.iter().find(|d| (d.qty - 400.0).abs() < 1e-6);
        assert!(pasta.is_some(), "200g × 2 servings = 400g, 500g in stock");

        // 5g × 2 = 10g needed, but only 2g on hand → capped at 2g.
        let salt = plan.iter().find(|d| (d.qty - 2.0).abs() < 1e-6);
        assert!(salt.is_some(), "deduction is capped at available stock");
    }

    #[test]
    fn plan_cook_surfaces_skipped_ingredients() {
        let mut to_taste = ing("Pepper", 0.0, "g");
        to_taste.qty = None; // "@pepper" with no amount
        let r = recipe_with(
            "Cookbook/X.cook",
            vec![
                ing("Pasta", 200.0, "g"),     // matched, in stock → deducted
                ing("Salt", 5.0, "g"),        // matched but 0g on hand → out of stock
                ing("Garlic", 2.0, "cloves"), // matched but unit won't convert
                ing("Saffron", 1.0, "g"),     // no pantry match
                to_taste,                     // no quantity
            ],
            2,
        );
        let pantry = vec![
            pantry_row("Pasta", 500.0, "g"),
            pantry_row("Salt", 0.0, "g"),
            pantry_row("Garlic", 100.0, "g"),
        ];

        let receipt = plan_cook(&r, &pantry, 2);

        assert_eq!(receipt.deducted.len(), 1, "only pasta is deductible");
        assert_eq!(receipt.deducted[0].ingredient, "Pasta");
        assert!((receipt.deducted[0].qty - 200.0).abs() < 1e-6);

        let reason = |name: &str| {
            receipt
                .skipped
                .iter()
                .find(|s| s.ingredient == name)
                .map(|s| s.reason)
        };
        assert_eq!(reason("Salt"), Some(SkipReason::OutOfStock));
        assert_eq!(reason("Garlic"), Some(SkipReason::InconvertibleUnit));
        assert_eq!(reason("Saffron"), Some(SkipReason::NoPantryMatch));
        assert_eq!(reason("Pepper"), Some(SkipReason::NoQuantity));
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
