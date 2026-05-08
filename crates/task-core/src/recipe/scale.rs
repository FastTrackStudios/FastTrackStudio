//! Linear recipe scaling — multiply every ingredient quantity by
//! `target_servings / source_servings` and return a new
//! `Vec<RecipeIngredientApi>`.
//!
//! Step durations and step text are intentionally NOT touched. Most
//! cook times are roughly invariant under modest scaling (a 25-minute
//! bake is still ~25 minutes whether you make a half batch or a double
//! batch), and rewriting step text at scale ("crack 3 eggs" → "crack 6
//! eggs") would require true NLP. That's a separate problem.
//!
//! When `source_servings` is `None` or `0` we return the input
//! unchanged with a warning so the caller can surface "this recipe
//! doesn't declare servings, so it can't be scaled" to the UI.

use crate::recipe_ingredient::RecipeIngredientApi;

/// Output of [`scale_ingredients`]: the multiplied ingredient list plus
/// metadata for caller introspection.
#[derive(Debug, Clone, Default)]
pub struct ScaleResult {
    pub source_servings: Option<u32>,
    pub target_servings: u32,
    pub multiplier: f64,
    pub ingredients: Vec<RecipeIngredientApi>,
    pub warnings: Vec<String>,
}

/// Linearly scale `ingredients` by `target_servings / source_servings`.
///
/// - When `source_servings` is `None` or `0`, returns the input
///   unchanged with `multiplier = 1.0` and a warning describing why.
/// - `unit`, `food`, `note`, `food_id`, and `is_section` are preserved.
/// - Section-header rows (`is_section = true`) are passed through with
///   `quantity` set to `None` (sections never carry a meaningful
///   quantity).
#[must_use]
pub fn scale_ingredients(
    ingredients: &[RecipeIngredientApi],
    source_servings: Option<u32>,
    target_servings: u32,
) -> ScaleResult {
    let mut warnings: Vec<String> = Vec::new();
    let target = if target_servings == 0 {
        warnings.push("target_servings was 0, treating as 1".to_string());
        1
    } else {
        target_servings
    };

    let multiplier = match source_servings {
        Some(src) if src > 0 => f64::from(target) / f64::from(src),
        Some(0) => {
            warnings.push(
                "recipe.servings was 0; cannot scale, returning original quantities".to_string(),
            );
            1.0
        }
        Some(_) => 1.0,
        None => {
            warnings.push(
                "recipe.servings is unknown; cannot scale, returning original quantities"
                    .to_string(),
            );
            1.0
        }
    };

    let scaled: Vec<RecipeIngredientApi> = ingredients
        .iter()
        .cloned()
        .map(|mut row| {
            if !row.is_section {
                if let Some(q) = row.quantity {
                    row.quantity = Some(q * multiplier);
                }
            } else {
                row.quantity = None;
            }
            row
        })
        .collect();

    ScaleResult {
        source_servings,
        target_servings: target,
        multiplier,
        ingredients: scaled,
        warnings,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use uuid::Uuid;

    fn ing(quantity: Option<f64>, food: &str, is_section: bool) -> RecipeIngredientApi {
        RecipeIngredientApi {
            id: Uuid::nil(),
            recipe_id: Uuid::nil(),
            sequence: 0,
            quantity,
            unit: Some("g".to_string()),
            food: food.to_string(),
            food_id: None,
            note: None,
            is_section,
            created_at: chrono::Utc::now(),
            updated_at: chrono::Utc::now(),
        }
    }

    #[test]
    fn identity_when_source_equals_target() {
        let ings = vec![
            ing(Some(100.0), "flour", false),
            ing(Some(2.0), "eggs", false),
        ];
        let r = scale_ingredients(&ings, Some(4), 4);
        assert!((r.multiplier - 1.0).abs() < f64::EPSILON);
        assert_eq!(r.ingredients[0].quantity, Some(100.0));
        assert_eq!(r.ingredients[1].quantity, Some(2.0));
        assert!(r.warnings.is_empty());
    }

    #[test]
    fn doubles_quantities_at_2x() {
        let ings = vec![
            ing(Some(100.0), "flour", false),
            ing(Some(2.5), "milk", false),
        ];
        let r = scale_ingredients(&ings, Some(4), 8);
        assert!((r.multiplier - 2.0).abs() < f64::EPSILON);
        assert_eq!(r.ingredients[0].quantity, Some(200.0));
        assert_eq!(r.ingredients[1].quantity, Some(5.0));
    }

    #[test]
    fn halves_quantities_at_half_x() {
        let ings = vec![ing(Some(100.0), "flour", false)];
        let r = scale_ingredients(&ings, Some(4), 2);
        assert!((r.multiplier - 0.5).abs() < f64::EPSILON);
        assert_eq!(r.ingredients[0].quantity, Some(50.0));
    }

    #[test]
    fn none_source_returns_input_with_warning() {
        let ings = vec![ing(Some(100.0), "flour", false)];
        let r = scale_ingredients(&ings, None, 4);
        assert!((r.multiplier - 1.0).abs() < f64::EPSILON);
        assert_eq!(r.ingredients[0].quantity, Some(100.0));
        assert_eq!(r.warnings.len(), 1);
        assert!(r.warnings[0].contains("unknown"));
    }

    #[test]
    fn zero_target_treated_as_one_with_warning() {
        let ings = vec![ing(Some(4.0), "x", false)];
        let r = scale_ingredients(&ings, Some(4), 0);
        assert_eq!(r.target_servings, 1);
        assert!(!r.warnings.is_empty());
        assert_eq!(r.ingredients[0].quantity, Some(1.0));
    }

    #[test]
    fn section_rows_preserved_quantity_cleared() {
        let ings = vec![
            ing(None, "for the sauce", true),
            ing(Some(10.0), "olive oil", false),
        ];
        let r = scale_ingredients(&ings, Some(2), 4);
        assert!(r.ingredients[0].is_section);
        assert_eq!(r.ingredients[0].quantity, None);
        assert_eq!(r.ingredients[1].quantity, Some(20.0));
    }
}
