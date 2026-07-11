//! Walk a `vault::Vault` and collect every page that
//! looks like an exercise.

use vault::Vault;

use crate::model::Exercise;
use crate::parse::{looks_like_exercise, parse_page};

pub fn scan_vault(vault: &Vault) -> Vec<Exercise> {
    vault
        .pages
        .iter()
        .filter(|p| looks_like_exercise(p))
        .filter_map(|p| match parse_page(p) {
            Ok(e) => Some(e),
            Err(e) => {
                tracing::warn!(path = %p.rel_path, ?e, "exercise parse failed");
                None
            }
        })
        .collect()
}

/// Convenience: every exercise in a given category.
pub fn by_category(vault: &Vault, category: &str) -> Vec<Exercise> {
    let needle = category.to_ascii_lowercase();
    scan_vault(vault)
        .into_iter()
        .filter(|e| e.category.eq_ignore_ascii_case(&needle))
        .collect()
}

/// Convenience: every exercise that uses any of `equipment`.
/// Empty `equipment` returns the full list (no filter).
pub fn by_equipment(vault: &Vault, equipment: &[String]) -> Vec<Exercise> {
    if equipment.is_empty() {
        return scan_vault(vault);
    }
    let needles: Vec<String> = equipment.iter().map(|e| e.to_ascii_lowercase()).collect();
    scan_vault(vault)
        .into_iter()
        .filter(|ex| {
            ex.equipment.iter().any(|have| {
                let have = have.to_ascii_lowercase();
                needles.iter().any(|n| have.contains(n))
            })
        })
        .collect()
}
