//! Walk a `vault::Vault` and collect every page that looks
//! like a pantry item.

use chrono::NaiveDate;
use vault::Vault;

use crate::model::PantryItem;
use crate::parse::{looks_like_pantry_item, parse_page};

pub fn scan_vault(vault: &Vault) -> Vec<PantryItem> {
    vault
        .pages
        .iter()
        .filter(|p| looks_like_pantry_item(p))
        .filter_map(|p| match parse_page(p) {
            Ok(i) => Some(i),
            Err(e) => {
                tracing::warn!(path = %p.rel_path, ?e, "pantry parse failed");
                None
            }
        })
        .collect()
}

/// Convenience: every pantry item past its printed expiry as
/// of `today`.
pub fn expired(vault: &Vault, today: NaiveDate) -> Vec<PantryItem> {
    scan_vault(vault)
        .into_iter()
        .filter(|i| i.is_expired(today))
        .collect()
}

/// Convenience: every pantry item at or below its
/// `minimum` reorder threshold.
pub fn low_stock(vault: &Vault) -> Vec<PantryItem> {
    scan_vault(vault)
        .into_iter()
        .filter(|i| i.is_low())
        .collect()
}
