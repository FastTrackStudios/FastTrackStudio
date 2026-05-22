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

/// Check whether `recipe` can be cooked from `pantry` at
/// `servings`. Pure; no I/O.
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
}
