//! Recipe-level nutrition aggregation.
//!
//! Pure function: take a list of [`IngredientNutritionInput`] (each
//! carrying the ingredient's optional `nutrition_per_100g` row from the
//! linked `Food` plus its quantity / unit) and produce a per-recipe
//! [`NutritionFacts`] sum + an optional per-serving slice.
//!
//! Ingredients without a resolved Food link, with unknown units, or
//! with no nutrition data are skipped — each skip emits a warning so
//! callers can surface gaps. Volume → mass without density is also a
//! warning (we never silently assume water).

use serde::{Deserialize, Serialize};
use uuid::Uuid;

use super::model::NutritionFacts;
use super::units::to_grams;

/// One row of input to [`aggregate_recipe_nutrition`].
#[derive(Debug, Clone, Default)]
pub struct IngredientNutritionInput {
    pub food_id: Option<Uuid>,
    /// Free-form ingredient text, used only in warnings.
    pub food_name: String,
    pub quantity: Option<f64>,
    pub unit: Option<String>,
    /// Resolved by the caller (Food.nutrition_per_100g, or product
    /// override when relevant). When `None`, the ingredient is skipped.
    pub nutrition_per_100g: Option<NutritionFacts>,
}

/// Per-aggregator notes blob. Embedded inside the resulting
/// `NutritionFacts.notes` JSON field.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
struct AggregatorNotes {
    servings: Option<u32>,
    ingredients_with_nutrition: u32,
    ingredients_skipped: u32,
}

/// Output of [`aggregate_recipe_nutrition`].
#[derive(Debug, Clone, Default)]
pub struct AggregatedNutrition {
    /// Whole-batch total. `source = "calculated"`, `notes` JSON-encodes
    /// [`AggregatorNotes`].
    pub total: NutritionFacts,
    /// Per-serving slice when `recipe_servings` is `Some(n)` with `n >= 1`.
    pub per_serving: Option<NutritionFacts>,
    /// One warning per skipped ingredient. Categories:
    ///   - `"<food>: missing food link"`
    ///   - `"<food>: missing nutrition data"`
    ///   - `"<food>: missing quantity"`
    ///   - `"<food>: missing unit"`
    ///   - `"<food>: unknown unit '<u>'"`
    ///   - `"<food>: volume unit '<u>' has no density"`
    pub warnings: Vec<String>,
}

fn slice_per_serving(total: &NutritionFacts, servings: u32) -> NutritionFacts {
    let factor = 1.0 / f64::from(servings);
    fn s(v: Option<f64>, factor: f64) -> Option<f64> {
        v.map(|x| x * factor)
    }
    NutritionFacts {
        kcal_per_100g: s(total.kcal_per_100g, factor),
        protein_g: s(total.protein_g, factor),
        carbs_g: s(total.carbs_g, factor),
        sugars_g: s(total.sugars_g, factor),
        fiber_g: s(total.fiber_g, factor),
        fat_g: s(total.fat_g, factor),
        saturated_fat_g: s(total.saturated_fat_g, factor),
        sodium_mg: s(total.sodium_mg, factor),
        source: Some("calculated".to_string()),
        notes: total.notes.clone(),
    }
}

/// Sum nutrition contributions across `ingredients`. Each ingredient's
/// `nutrition_per_100g` is multiplied by `grams / 100`, then folded into
/// a running total.
///
/// Returns the total (per recipe) plus an optional per-serving slice
/// when `recipe_servings` is provided. Skipped ingredients append a
/// warning rather than failing.
#[must_use]
pub fn aggregate_recipe_nutrition(
    recipe_servings: Option<u32>,
    ingredients: &[IngredientNutritionInput],
) -> AggregatedNutrition {
    let mut total = NutritionFacts::default();
    let mut warnings = Vec::new();
    let mut counted: u32 = 0;
    let mut skipped: u32 = 0;

    for ing in ingredients {
        let label = if ing.food_name.trim().is_empty() {
            "<unnamed>".to_string()
        } else {
            ing.food_name.trim().to_string()
        };

        if ing.food_id.is_none() {
            warnings.push(format!("{label}: missing food link"));
            skipped += 1;
            continue;
        }
        let Some(facts) = ing.nutrition_per_100g.as_ref() else {
            warnings.push(format!("{label}: missing nutrition data"));
            skipped += 1;
            continue;
        };
        let Some(quantity) = ing.quantity else {
            warnings.push(format!("{label}: missing quantity"));
            skipped += 1;
            continue;
        };
        let Some(unit) = ing.unit.as_deref() else {
            warnings.push(format!("{label}: missing unit"));
            skipped += 1;
            continue;
        };
        let Some(grams) = to_grams(quantity, unit) else {
            // Either unknown or a volume-without-density. We don't have
            // densities yet at this layer so we surface a single
            // warning category.
            warnings.push(format!("{label}: unknown or volume unit '{unit}'"));
            skipped += 1;
            continue;
        };
        let scaled = facts.scale_to_grams(grams);
        total = total.sum(&scaled);
        counted += 1;
    }

    total.source = Some("calculated".to_string());
    let notes = AggregatorNotes {
        servings: recipe_servings,
        ingredients_with_nutrition: counted,
        ingredients_skipped: skipped,
    };
    total.notes = serde_json::to_string(&notes).ok();

    let per_serving = match recipe_servings {
        Some(n) if n >= 1 => Some(slice_per_serving(&total, n)),
        _ => None,
    };

    AggregatedNutrition {
        total,
        per_serving,
        warnings,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn nf(kcal: f64, protein: f64, carbs: f64, fat: f64) -> NutritionFacts {
        NutritionFacts {
            kcal_per_100g: Some(kcal),
            protein_g: Some(protein),
            carbs_g: Some(carbs),
            fat_g: Some(fat),
            ..Default::default()
        }
    }

    #[test]
    fn three_ingredient_sum() {
        let ings = vec![
            IngredientNutritionInput {
                food_id: Some(Uuid::nil()),
                food_name: "chicken".into(),
                quantity: Some(200.0),
                unit: Some("g".into()),
                nutrition_per_100g: Some(nf(165.0, 31.0, 0.0, 3.6)),
            },
            IngredientNutritionInput {
                food_id: Some(Uuid::nil()),
                food_name: "rice".into(),
                quantity: Some(150.0),
                unit: Some("g".into()),
                nutrition_per_100g: Some(nf(130.0, 2.7, 28.0, 0.3)),
            },
            IngredientNutritionInput {
                food_id: Some(Uuid::nil()),
                food_name: "olive oil".into(),
                quantity: Some(15.0),
                unit: Some("g".into()),
                nutrition_per_100g: Some(nf(884.0, 0.0, 0.0, 100.0)),
            },
        ];
        let agg = aggregate_recipe_nutrition(Some(2), &ings);
        let kcal = agg.total.kcal_per_100g.unwrap();
        let expected = 165.0 * 2.0 + 130.0 * 1.5 + 884.0 * 0.15;
        assert!((kcal - expected).abs() < 1e-3);
        let per_serv = agg.per_serving.unwrap();
        assert!((per_serv.kcal_per_100g.unwrap() - expected / 2.0).abs() < 1e-3);
        assert!(agg.warnings.is_empty());
    }

    #[test]
    fn missing_food_id_skipped_with_warning() {
        let ings = vec![
            IngredientNutritionInput {
                food_id: Some(Uuid::nil()),
                food_name: "rice".into(),
                quantity: Some(100.0),
                unit: Some("g".into()),
                nutrition_per_100g: Some(nf(130.0, 2.7, 28.0, 0.3)),
            },
            IngredientNutritionInput {
                food_id: None,
                food_name: "salt".into(),
                quantity: Some(2.0),
                unit: Some("g".into()),
                nutrition_per_100g: None,
            },
        ];
        let agg = aggregate_recipe_nutrition(None, &ings);
        assert_eq!(agg.warnings.len(), 1);
        assert!(agg.warnings[0].contains("missing food link"));
        assert!(agg.per_serving.is_none());
        assert!((agg.total.kcal_per_100g.unwrap() - 130.0).abs() < 1e-3);
    }

    #[test]
    fn unknown_unit_skipped() {
        let ings = vec![IngredientNutritionInput {
            food_id: Some(Uuid::nil()),
            food_name: "pepper".into(),
            quantity: Some(1.0),
            unit: Some("pinch".into()),
            nutrition_per_100g: Some(nf(100.0, 1.0, 1.0, 1.0)),
        }];
        let agg = aggregate_recipe_nutrition(Some(4), &ings);
        assert_eq!(agg.warnings.len(), 1);
        assert!(agg.warnings[0].contains("unknown or volume unit"));
        // No counted ingredients — total is empty.
        assert!(agg.total.kcal_per_100g.is_none());
    }

    #[test]
    fn per_serving_division() {
        let ings = vec![IngredientNutritionInput {
            food_id: Some(Uuid::nil()),
            food_name: "rice".into(),
            quantity: Some(400.0),
            unit: Some("g".into()),
            nutrition_per_100g: Some(nf(130.0, 2.7, 28.0, 0.3)),
        }];
        let agg = aggregate_recipe_nutrition(Some(4), &ings);
        let total_kcal = agg.total.kcal_per_100g.unwrap();
        let per_serv = agg.per_serving.unwrap().kcal_per_100g.unwrap();
        assert!((per_serv * 4.0 - total_kcal).abs() < 1e-3);
    }
}
