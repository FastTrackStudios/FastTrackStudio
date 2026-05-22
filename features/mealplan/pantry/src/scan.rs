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

/// Stock entries (paired with their parent item) whose
/// `best_before` falls within `[today, today + days)`.
/// Drives the "expiring this week" surface — wires into
/// shopping-list auto-populate in phase 7.
pub fn expiring_within(
    vault: &Vault,
    today: NaiveDate,
    days: u32,
) -> Vec<(PantryItem, crate::model::StockEntry)> {
    let horizon = today
        .checked_add_days(chrono::Days::new(days as u64))
        .unwrap_or(NaiveDate::MAX);
    let mut out = Vec::new();
    for item in scan_vault(vault) {
        for entry in &item.stock_entries {
            if let Some(bb) = entry.best_before {
                if bb >= today && bb < horizon {
                    out.push((item.clone(), entry.clone()));
                }
            }
        }
    }
    out
}
