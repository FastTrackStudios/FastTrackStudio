//! Unit conversion helpers for nutrition aggregation.
//!
//! Pure functions, no DB. Two layers:
//!   1. [`to_grams`] — direct mass-unit conversion. Returns `None` for
//!      unknown or volume units.
//!   2. [`volume_to_grams`] — volume → grams given an explicit density.
//!      Returns `None` for non-volume units.
//!   3. [`to_grams_best_effort`] — try mass first, then volume. When
//!      `density_g_per_ml` is `None` and `assume_water = true`, falls
//!      back to a density of `1.0` (water-equivalent).
//!
//! All inputs are case-insensitive and tolerate plurals + hyphens. The
//! intentional gap is volume → mass without density: callers either
//! provide a density (looked up from a Food row in the future) or
//! accept the `None` and surface a warning.

/// Normalize a free-form unit string for matching: lowercase, trim,
/// strip trailing periods, replace hyphens / underscores with spaces.
fn normalize(unit: &str) -> String {
    unit.trim()
        .trim_end_matches('.')
        .replace(['-', '_'], " ")
        .to_ascii_lowercase()
}

/// Convert `(quantity, unit)` to grams using mass units only.
///
/// Recognised units:
///   - `g | gram | grams` → identity
///   - `kg | kilogram | kilograms` → ×1000
///   - `oz | ounce | ounces` → ×28.3495
///   - `lb | lbs | pound | pounds` → ×453.592
///   - `mg | milligram | milligrams` → ÷1000
///
/// Volume units intentionally return `None`; use [`volume_to_grams`].
#[must_use]
pub fn to_grams(quantity: f64, unit: &str) -> Option<f64> {
    let u = normalize(unit);
    let factor = match u.as_str() {
        "g" | "gram" | "grams" => 1.0,
        "kg" | "kilogram" | "kilograms" => 1000.0,
        "oz" | "ounce" | "ounces" => 28.3495,
        "lb" | "lbs" | "pound" | "pounds" => 453.592,
        "mg" | "milligram" | "milligrams" => 0.001,
        _ => return None,
    };
    Some(quantity * factor)
}

/// Convert a volume to grams given a density in g/ml.
///
/// Recognised volume units (all routed through ml first):
///   - `ml | milliliter | milliliters` → identity ml
///   - `l | liter | liters | litre | litres` → ×1000 ml
///   - `cup | cups` → ×236.588 ml
///   - `tbsp | tablespoon | tablespoons` → ×14.787 ml
///   - `tsp | teaspoon | teaspoons` → ×4.929 ml
///   - `fl oz | fl_oz | fluid ounce | fluid ounces` → ×29.574 ml
#[must_use]
pub fn volume_to_grams(quantity: f64, unit: &str, density_g_per_ml: f64) -> Option<f64> {
    let ml_factor = volume_to_ml_factor(unit)?;
    Some(quantity * ml_factor * density_g_per_ml)
}

/// Internal: factor from `unit` → ml. `None` when not a volume.
fn volume_to_ml_factor(unit: &str) -> Option<f64> {
    let u = normalize(unit);
    let factor = match u.as_str() {
        "ml" | "milliliter" | "milliliters" => 1.0,
        "l" | "liter" | "liters" | "litre" | "litres" => 1000.0,
        "cup" | "cups" => 236.588,
        "tbsp" | "tablespoon" | "tablespoons" => 14.787,
        "tsp" | "teaspoon" | "teaspoons" => 4.929,
        "fl oz" | "fluid ounce" | "fluid ounces" => 29.574,
        _ => return None,
    };
    Some(factor)
}

/// Best-effort conversion: try mass first, then volume.
///
/// When `density_g_per_ml` is provided, volumes are converted via that
/// density. When it is `None` and `assume_water = true`, water-density
/// (`1.0`) is used as a fallback. Otherwise volumes return `None`.
#[must_use]
pub fn to_grams_best_effort(
    quantity: f64,
    unit: &str,
    density_g_per_ml: Option<f64>,
    assume_water: bool,
) -> Option<f64> {
    if let Some(g) = to_grams(quantity, unit) {
        return Some(g);
    }
    volume_to_ml_factor(unit)?;
    let density = density_g_per_ml.or(if assume_water { Some(1.0) } else { None })?;
    volume_to_grams(quantity, unit, density)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn approx(a: f64, b: f64) -> bool {
        (a - b).abs() < 1e-3
    }

    #[test]
    fn grams_identity() {
        assert!(approx(to_grams(50.0, "g").unwrap(), 50.0));
        assert!(approx(to_grams(50.0, "GRAMS").unwrap(), 50.0));
        assert!(approx(to_grams(50.0, " gram ").unwrap(), 50.0));
    }

    #[test]
    fn kilograms_scale_up() {
        assert!(approx(to_grams(1.5, "kg").unwrap(), 1500.0));
        assert!(approx(to_grams(1.5, "Kilograms").unwrap(), 1500.0));
    }

    #[test]
    fn milligrams_scale_down() {
        assert!(approx(to_grams(2500.0, "mg").unwrap(), 2.5));
        assert!(approx(to_grams(2500.0, "MILLIGRAM").unwrap(), 2.5));
    }

    #[test]
    fn ounces_imperial() {
        assert!(approx(to_grams(1.0, "oz").unwrap(), 28.3495));
        assert!(approx(to_grams(1.0, "Ounce").unwrap(), 28.3495));
        assert!(approx(to_grams(2.0, "ounces").unwrap(), 56.699));
    }

    #[test]
    fn pounds_imperial() {
        assert!(approx(to_grams(1.0, "lb").unwrap(), 453.592));
        assert!(approx(to_grams(2.0, "lbs").unwrap(), 907.184));
        assert!(approx(to_grams(1.0, "Pound").unwrap(), 453.592));
        assert!(approx(to_grams(1.0, "POUNDS").unwrap(), 453.592));
    }

    #[test]
    fn unknown_unit_returns_none() {
        assert!(to_grams(1.0, "pinch").is_none());
        assert!(to_grams(1.0, "").is_none());
    }

    #[test]
    fn volume_units_not_handled_by_to_grams() {
        assert!(to_grams(1.0, "cup").is_none());
        assert!(to_grams(1.0, "ml").is_none());
        assert!(to_grams(1.0, "tbsp").is_none());
    }

    #[test]
    fn ml_identity_with_density() {
        assert!(approx(volume_to_grams(100.0, "ml", 1.0).unwrap(), 100.0));
        assert!(approx(volume_to_grams(100.0, "ml", 0.92).unwrap(), 92.0));
    }

    #[test]
    fn liter_scale() {
        assert!(approx(volume_to_grams(1.0, "l", 1.0).unwrap(), 1000.0));
        assert!(approx(volume_to_grams(1.0, "Liter", 1.0).unwrap(), 1000.0));
        assert!(approx(volume_to_grams(1.0, "litres", 1.0).unwrap(), 1000.0));
    }

    #[test]
    fn cup_to_grams_water() {
        assert!(approx(volume_to_grams(1.0, "cup", 1.0).unwrap(), 236.588));
        assert!(approx(volume_to_grams(1.0, "Cups", 1.0).unwrap(), 236.588));
    }

    #[test]
    fn tablespoon_teaspoon() {
        assert!(approx(volume_to_grams(1.0, "tbsp", 1.0).unwrap(), 14.787));
        assert!(approx(
            volume_to_grams(1.0, "tablespoon", 1.0).unwrap(),
            14.787
        ));
        assert!(approx(volume_to_grams(1.0, "tsp", 1.0).unwrap(), 4.929));
        assert!(approx(
            volume_to_grams(1.0, "teaspoons", 1.0).unwrap(),
            4.929
        ));
    }

    #[test]
    fn fluid_ounce_variants() {
        assert!(approx(volume_to_grams(1.0, "fl oz", 1.0).unwrap(), 29.574));
        assert!(approx(volume_to_grams(1.0, "fl_oz", 1.0).unwrap(), 29.574));
        assert!(approx(volume_to_grams(1.0, "fl-oz", 1.0).unwrap(), 29.574));
        assert!(approx(
            volume_to_grams(1.0, "Fluid Ounce", 1.0).unwrap(),
            29.574
        ));
        assert!(approx(
            volume_to_grams(2.0, "fluid ounces", 1.0).unwrap(),
            59.148
        ));
    }

    #[test]
    fn unknown_volume_returns_none() {
        assert!(volume_to_grams(1.0, "splash", 1.0).is_none());
        assert!(volume_to_grams(1.0, "g", 1.0).is_none());
    }

    #[test]
    fn density_factor_applied() {
        // Olive oil, density ≈ 0.92 g/ml.
        assert!(approx(volume_to_grams(1.0, "cup", 0.92).unwrap(), 217.66));
    }

    #[test]
    fn best_effort_prefers_mass() {
        // Even with a density supplied, mass units stay mass.
        assert!(approx(
            to_grams_best_effort(50.0, "g", Some(0.5), false).unwrap(),
            50.0
        ));
    }

    #[test]
    fn best_effort_volume_with_density() {
        assert!(approx(
            to_grams_best_effort(1.0, "cup", Some(0.92), false).unwrap(),
            217.66
        ));
    }

    #[test]
    fn best_effort_volume_without_density_returns_none() {
        assert!(to_grams_best_effort(1.0, "cup", None, false).is_none());
    }

    #[test]
    fn best_effort_volume_water_assumption() {
        let g = to_grams_best_effort(1.0, "cup", None, true).unwrap();
        assert!(approx(g, 236.588));
    }

    #[test]
    fn best_effort_unknown_unit() {
        assert!(to_grams_best_effort(1.0, "pinch", None, true).is_none());
        assert!(to_grams_best_effort(1.0, "pinch", Some(1.0), true).is_none());
    }

    #[test]
    fn case_and_punctuation_tolerance() {
        assert!(approx(to_grams(50.0, "G.").unwrap(), 50.0));
        assert!(approx(to_grams(1.0, "OZ").unwrap(), 28.3495));
        assert!(approx(volume_to_grams(1.0, "TBSP", 1.0).unwrap(), 14.787));
    }
}
