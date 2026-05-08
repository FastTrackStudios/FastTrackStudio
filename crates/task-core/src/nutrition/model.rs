//! `NutritionFacts` — kcal + macros per 100g, lives in JSON columns.

use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

/// Nutritional information for 100g of an ingredient or product.
///
/// Stored as JSON inside the `nutrition_per_100g` column on
/// [`crate::food::Food`] and [`crate::food_product::FoodProduct`]. Each
/// `Option` allows partial population — Open Food Facts often omits
/// fields and we propagate that honestly rather than zeroing.
#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize, ToSchema, facet::Facet)]
pub struct NutritionFacts {
    pub kcal_per_100g: Option<f64>,
    pub protein_g: Option<f64>,
    pub carbs_g: Option<f64>,
    pub sugars_g: Option<f64>,
    pub fiber_g: Option<f64>,
    pub fat_g: Option<f64>,
    pub saturated_fat_g: Option<f64>,
    pub sodium_mg: Option<f64>,
    /// `"openfoodfacts" | "manual" | "calculated"`.
    pub source: Option<String>,
    /// When `source = "calculated"`, the recipe nutrition aggregator
    /// records its assumptions here. Free-form JSON-encoded blob.
    pub notes: Option<String>,
}

fn scale_opt(value: Option<f64>, factor: f64) -> Option<f64> {
    value.map(|v| v * factor)
}

fn sum_opt(a: Option<f64>, b: Option<f64>) -> Option<f64> {
    match (a, b) {
        (Some(a), Some(b)) => Some(a + b),
        (Some(a), None) => Some(a),
        (None, Some(b)) => Some(b),
        (None, None) => None,
    }
}

impl NutritionFacts {
    /// Scale facts proportionally from "per 100g" to a `grams` weight.
    /// Returns absolute values for that mass (no longer per-100g).
    #[must_use]
    pub fn scale_to_grams(&self, grams: f64) -> Self {
        let factor = grams / 100.0;
        Self {
            kcal_per_100g: scale_opt(self.kcal_per_100g, factor),
            protein_g: scale_opt(self.protein_g, factor),
            carbs_g: scale_opt(self.carbs_g, factor),
            sugars_g: scale_opt(self.sugars_g, factor),
            fiber_g: scale_opt(self.fiber_g, factor),
            fat_g: scale_opt(self.fat_g, factor),
            saturated_fat_g: scale_opt(self.saturated_fat_g, factor),
            sodium_mg: scale_opt(self.sodium_mg, factor),
            source: Some("calculated".to_string()),
            notes: self.notes.clone(),
        }
    }

    /// Sum two nutrition rows (None + Some = Some, Some + Some = sum).
    /// The returned `source` is `"calculated"`.
    #[must_use]
    pub fn sum(&self, other: &Self) -> Self {
        Self {
            kcal_per_100g: sum_opt(self.kcal_per_100g, other.kcal_per_100g),
            protein_g: sum_opt(self.protein_g, other.protein_g),
            carbs_g: sum_opt(self.carbs_g, other.carbs_g),
            sugars_g: sum_opt(self.sugars_g, other.sugars_g),
            fiber_g: sum_opt(self.fiber_g, other.fiber_g),
            fat_g: sum_opt(self.fat_g, other.fat_g),
            saturated_fat_g: sum_opt(self.saturated_fat_g, other.saturated_fat_g),
            sodium_mg: sum_opt(self.sodium_mg, other.sodium_mg),
            source: Some("calculated".to_string()),
            notes: None,
        }
    }

    /// Decode from a [`crate::property::JsonObject`]. Empty / missing
    /// keys produce `Default::default()`.
    #[must_use]
    pub fn from_json_object(obj: &crate::property::JsonObject) -> Self {
        serde_json::from_value(obj.as_value().clone()).unwrap_or_default()
    }

    /// Encode into a [`crate::property::JsonObject`] for storage.
    pub fn to_json_object(&self) -> crate::property::JsonObject {
        crate::property::JsonObject::from_value(
            serde_json::to_value(self).unwrap_or(serde_json::Value::Null),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scale_to_grams_proportional() {
        let nf = NutritionFacts {
            kcal_per_100g: Some(884.0),
            fat_g: Some(100.0),
            ..Default::default()
        };
        let scaled = nf.scale_to_grams(50.0);
        assert!((scaled.kcal_per_100g.unwrap() - 442.0).abs() < 1e-6);
        assert!((scaled.fat_g.unwrap() - 50.0).abs() < 1e-6);
        assert_eq!(scaled.source.as_deref(), Some("calculated"));
    }

    #[test]
    fn sum_combines_with_none_passthrough() {
        let a = NutritionFacts {
            kcal_per_100g: Some(100.0),
            protein_g: Some(5.0),
            ..Default::default()
        };
        let b = NutritionFacts {
            kcal_per_100g: Some(50.0),
            fat_g: Some(2.0),
            ..Default::default()
        };
        let combined = a.sum(&b);
        assert_eq!(combined.kcal_per_100g, Some(150.0));
        assert_eq!(combined.protein_g, Some(5.0));
        assert_eq!(combined.fat_g, Some(2.0));
        assert_eq!(combined.source.as_deref(), Some("calculated"));
    }
}
