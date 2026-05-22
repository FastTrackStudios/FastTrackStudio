//! Recipe fulfillment — "can I make this from what's in
//! the pantry?". Modeled on grocy's `recipes_fulfillment`
//! SQL view, but pure Rust + pure functions so the same
//! code runs server-side, in tests, and (later) in the
//! Dioxus UI.
//!
//! The match flow: each recipe ingredient is paired to a
//! `pantry::PantryItem` via (1) an explicit
//! `Ingredient.pantry_item_id` link, falling back to (2) a
//! lower-case substring match on `name`. Once paired, the
//! ingredient's qty + unit is compared to the pantry item's
//! `stock_total` + `unit` with unit conversion via
//! [`pantry::convert_str`] when the bases align.
//!
//! Output: [`Fulfillment { can_cook, missing }`] where
//! `missing` carries the per-ingredient shortfall so a
//! shopping-list builder (phase 7) can populate against it.

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
    /// Recipe ingredient name (echoed for display).
    pub name: String,

    /// Index into `Recipe.ingredients` — lets the caller
    /// look the original row up for additional context
    /// (note, optional flag).
    #[serde(rename = "ingredientIdx")]
    pub ingredient_idx: u32,

    /// How much is needed *in the ingredient's unit*.
    pub need: f64,

    /// How much we have, converted to the same unit.
    /// `0.0` when no matching pantry item was found.
    pub have: f64,

    pub unit: String,

    /// Why we're short: no match, qty short, or
    /// unit-incompatible (mass needed vs volume in pantry,
    /// no density known).
    pub reason: ShortageReason,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum ShortageReason {
    /// No pantry item matched this ingredient.
    NotInPantry,
    /// Matched but quantity is insufficient.
    InsufficientQty,
    /// Matched but recipe + pantry units live in
    /// incompatible bases — caller can't auto-decide.
    UnitMismatch,
    /// Optional ingredient with no qty/unit; we don't
    /// block on these.
    OptionalNoQty,
}

/// Max depth for nested-recipe resolution. Guards against
/// accidental cycles (recipe A → recipe B → recipe A) and
/// pathologically deep trees. Eight levels is way more than
/// any real cookbook needs.
const MAX_NEST_DEPTH: u32 = 8;

/// Check whether `recipe` can be cooked from `pantry` at
/// `servings`. Pure; no I/O. No nested-recipe resolution —
/// use [`check_nested`] when the recipe has
/// [`Recipe::nested_recipes`].
pub fn check(recipe: &Recipe, pantry: &[PantryItem], servings: u32) -> Fulfillment {
    let scale = if let Some(base) = recipe.servings.filter(|s| *s > 0) {
        servings as f64 / base as f64
    } else {
        // Recipe didn't specify a base — assume 1:1.
        1.0
    };

    let mut missing = Vec::new();
    for (idx, ing) in recipe.ingredients.iter().enumerate() {
        // Ingredients with no qty (e.g. `"salt: to taste"`)
        // can't be checked — skip when optional, surface as
        // a soft shortage otherwise so the UI knows to ask.
        let need = match ing.qty {
            Some(q) => q * scale,
            None if ing.optional => continue,
            None => {
                missing.push(Shortage {
                    name: ing.name.clone(),
                    ingredient_idx: idx as u32,
                    need: 0.0,
                    have: 0.0,
                    unit: ing.unit.clone(),
                    reason: ShortageReason::OptionalNoQty,
                });
                continue;
            }
        };

        match match_pantry(ing, pantry) {
            Some(item) => {
                let have_total = item.stock_total().unwrap_or(0.0);
                // Convert pantry total into the recipe's
                // unit. When units match exactly (case-
                // insensitive), no conversion needed.
                let have = pantry::convert_str(have_total, &item.unit, &ing.unit);
                match have {
                    Some(h) if h + 1e-9 >= need => { /* sufficient */ }
                    Some(h) => missing.push(Shortage {
                        name: ing.name.clone(),
                        ingredient_idx: idx as u32,
                        need,
                        have: h,
                        unit: ing.unit.clone(),
                        reason: ShortageReason::InsufficientQty,
                    }),
                    None => missing.push(Shortage {
                        name: ing.name.clone(),
                        ingredient_idx: idx as u32,
                        need,
                        have: have_total,
                        unit: ing.unit.clone(),
                        reason: ShortageReason::UnitMismatch,
                    }),
                }
            }
            None if ing.optional => continue,
            None => missing.push(Shortage {
                name: ing.name.clone(),
                ingredient_idx: idx as u32,
                need,
                have: 0.0,
                unit: ing.unit.clone(),
                reason: ShortageReason::NotInPantry,
            }),
        }
    }

    Fulfillment {
        can_cook: missing.iter().all(|s| {
            matches!(
                s.reason,
                ShortageReason::OptionalNoQty | ShortageReason::UnitMismatch
            )
        }) && missing.iter().all(|s| {
            !matches!(
                s.reason,
                ShortageReason::NotInPantry | ShortageReason::InsufficientQty
            )
        }),
        missing,
    }
}

/// Check fulfillment with nested-recipe support. Recurses
/// through [`Recipe::nested_recipes`] up to
/// [`MAX_NEST_DEPTH`] levels and aggregates ingredient
/// quantities before matching against the pantry. Pass
/// `all_recipes` so nested lookups can resolve; missing ids
/// are silently skipped (they surface as missing
/// ingredients through whatever the parent recipe
/// references directly).
///
/// Same-ingredient rows across nestings are summed before
/// matching so a "tomato sauce uses garlic" + "garlic
/// bread uses garlic" combo asks the pantry for the sum,
/// not twice the smaller need.
pub fn check_nested(
    recipe: &Recipe,
    all_recipes: &[Recipe],
    pantry: &[PantryItem],
    servings: u32,
) -> Fulfillment {
    use std::collections::HashMap;

    let index: HashMap<uuid::Uuid, &Recipe> = all_recipes.iter().map(|r| (r.id, r)).collect();

    let scale = if let Some(base) = recipe.servings.filter(|s| *s > 0) {
        servings as f64 / base as f64
    } else {
        1.0
    };

    let mut visited: HashMap<uuid::Uuid, ()> = HashMap::new();
    let mut flat = flatten(recipe, &index, scale, &mut visited, 0);
    fold_same_ingredient(&mut flat);

    // Wrap the flattened rows into a synthetic recipe so we
    // can reuse `check`. servings = 1 because `flat`'s
    // quantities are already scaled.
    let synthetic = Recipe {
        ingredients: flat,
        servings: Some(1),
        ..recipe.clone()
    };
    check(&synthetic, pantry, 1)
}

fn flatten(
    recipe: &Recipe,
    index: &std::collections::HashMap<uuid::Uuid, &Recipe>,
    scale: f64,
    visited: &mut std::collections::HashMap<uuid::Uuid, ()>,
    depth: u32,
) -> Vec<cookbook::Ingredient> {
    if depth > MAX_NEST_DEPTH || visited.contains_key(&recipe.id) {
        return Vec::new();
    }
    visited.insert(recipe.id, ());

    let mut out: Vec<cookbook::Ingredient> = recipe
        .ingredients
        .iter()
        .map(|ing| cookbook::Ingredient {
            qty: ing.qty.map(|q| q * scale),
            ..ing.clone()
        })
        .collect();

    for nested in &recipe.nested_recipes {
        if let Some(child) = index.get(&nested.recipe_id) {
            let base = child.servings.unwrap_or(1).max(1) as f64;
            let child_scale = scale * (nested.servings as f64 / base);
            out.extend(flatten(child, index, child_scale, visited, depth + 1));
        }
    }

    visited.remove(&recipe.id);
    out
}

/// Sum quantities across same-named, same-unit ingredients.
/// Different units stay separate (they'll get unit-matched
/// independently in `check`). Case-insensitive name + unit
/// comparison.
fn fold_same_ingredient(rows: &mut Vec<cookbook::Ingredient>) {
    let mut i = 0;
    while i < rows.len() {
        let mut j = i + 1;
        while j < rows.len() {
            let merge_ok = rows[i].name.eq_ignore_ascii_case(&rows[j].name)
                && rows[i].unit.eq_ignore_ascii_case(&rows[j].unit)
                && rows[i].pantry_item_id == rows[j].pantry_item_id;
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

fn match_pantry<'p>(
    ing: &cookbook::Ingredient,
    pantry: &'p [PantryItem],
) -> Option<&'p PantryItem> {
    if let Some(id) = ing.pantry_item_id {
        if let Some(item) = pantry.iter().find(|p| p.id == id) {
            return Some(item);
        }
    }
    // Fallback: lower-case substring match on the
    // ingredient name against pantry name. Strip
    // wikilink syntax (`[[Olive Oil]]`) before matching.
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
        .find(|p| p.name.to_ascii_lowercase().contains(&needle))
        .or_else(|| {
            // Reverse match — pantry name shorter than the
            // ingredient (e.g. ingredient
            // `"Extra Virgin Olive Oil"` vs pantry
            // `"Olive Oil"`).
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
            tasks: Vec::new(),
            tags: vec!["item".into(), "pantry".into()],
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

    fn recipe_with(ings: Vec<Ingredient>, servings: u32) -> Recipe {
        Recipe {
            path: String::new(),
            id: Uuid::new_v4(),
            name: "test".into(),
            description: None,
            course: "main".into(),
            cuisine: None,
            prep_minutes: None,
            cook_minutes: None,
            servings: Some(servings),
            ingredients: ings,
            steps: Vec::new(),
            nutrition: None,
            tags: Vec::new(),
            nested_recipes: Vec::new(),
            source: None,
            date_created: None,
            date_modified: None,
            details: String::new(),
        }
    }

    fn ing(name: &str, qty: f64, unit: &str) -> Ingredient {
        Ingredient {
            name: name.into(),
            qty: Some(qty),
            unit: unit.into(),
            pantry_item_id: None,
            note: None,
            optional: false,
        }
    }

    #[test]
    fn can_cook_when_stock_sufficient() {
        let recipe = recipe_with(vec![ing("Pasta", 200.0, "g")], 2);
        let stock = vec![pantry_row("Pasta", 500.0, "g")];
        let f = check(&recipe, &stock, 2);
        assert!(f.can_cook);
        assert!(f.missing.is_empty());
    }

    #[test]
    fn scales_with_servings() {
        let recipe = recipe_with(vec![ing("Pasta", 200.0, "g")], 2);
        let stock = vec![pantry_row("Pasta", 300.0, "g")];
        // 4 servings = 400 g needed, only 300 g on hand.
        let f = check(&recipe, &stock, 4);
        assert!(!f.can_cook);
        assert_eq!(f.missing.len(), 1);
        assert!(matches!(
            f.missing[0].reason,
            ShortageReason::InsufficientQty
        ));
    }

    #[test]
    fn cross_unit_with_conversion() {
        let recipe = recipe_with(vec![ing("Olive Oil", 30.0, "ml")], 1);
        let stock = vec![pantry_row("Olive Oil", 1.0, "l")];
        let f = check(&recipe, &stock, 1);
        assert!(f.can_cook, "{f:?}");
    }

    #[test]
    fn missing_item_surfaces() {
        let recipe = recipe_with(vec![ing("Truffles", 5.0, "g")], 1);
        let stock = vec![pantry_row("Pasta", 500.0, "g")];
        let f = check(&recipe, &stock, 1);
        assert!(!f.can_cook);
        assert!(matches!(f.missing[0].reason, ShortageReason::NotInPantry));
    }

    #[test]
    fn optional_ingredient_doesnt_block() {
        let mut opt = ing("Truffles", 5.0, "g");
        opt.optional = true;
        let recipe = recipe_with(vec![ing("Pasta", 100.0, "g"), opt], 1);
        let stock = vec![pantry_row("Pasta", 500.0, "g")];
        assert!(check(&recipe, &stock, 1).can_cook);
    }

    #[test]
    fn nested_recipe_aggregates_ingredients() {
        // pizza_dough: 200g flour
        let mut dough = recipe_with(vec![ing("Flour", 200.0, "g")], 1);
        dough.name = "Pizza Dough".into();

        // pizza: 100g flour (for dusting) + nested dough
        let mut pizza = recipe_with(vec![ing("Flour", 100.0, "g")], 1);
        pizza.name = "Pizza".into();
        pizza.nested_recipes = vec![cookbook::NestedRecipe {
            recipe_id: dough.id,
            servings: 1,
        }];

        let stock = vec![pantry_row("Flour", 250.0, "g")];
        let f = check_nested(&pizza, &[pizza.clone(), dough], &stock, 1);
        // Need 300g total (100 + 200), have 250 → short.
        assert!(!f.can_cook);
        let short = &f.missing[0];
        assert_eq!(short.name.to_ascii_lowercase(), "flour");
        assert!((short.need - 300.0).abs() < 1e-6);
        assert!((short.have - 250.0).abs() < 1e-6);
    }

    #[test]
    fn cycle_guard_doesnt_infinite_loop() {
        // a → b → a
        let mut a = recipe_with(vec![ing("X", 1.0, "g")], 1);
        let mut b = recipe_with(vec![ing("Y", 1.0, "g")], 1);
        a.nested_recipes = vec![cookbook::NestedRecipe {
            recipe_id: b.id,
            servings: 1,
        }];
        b.nested_recipes = vec![cookbook::NestedRecipe {
            recipe_id: a.id,
            servings: 1,
        }];
        let stock = vec![pantry_row("X", 10.0, "g"), pantry_row("Y", 10.0, "g")];
        let f = check_nested(&a, &[a.clone(), b.clone()], &stock, 1);
        assert!(f.can_cook);
    }
}
