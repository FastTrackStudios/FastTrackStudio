//! Walk a `vault::Vault` and collect every page that looks
//! like an inventory item.

use vault::Vault;

use crate::model::Item;
use crate::parse::{looks_like_item, parse_page};

pub fn scan_vault(vault: &Vault) -> Vec<Item> {
    vault
        .pages
        .iter()
        .filter(|p| looks_like_item(p))
        .filter_map(|p| match parse_page(p) {
            Ok(i) => Some(i),
            Err(e) => {
                tracing::warn!(path = %p.rel_path, ?e, "item parse failed");
                None
            }
        })
        .collect()
}

/// Convenience: every item whose `location_id` matches.
pub fn items_at(vault: &Vault, location_id: uuid::Uuid) -> Vec<Item> {
    scan_vault(vault)
        .into_iter()
        .filter(|i| i.location_id == Some(location_id))
        .collect()
}

/// Convenience: every item flagged for attention (poor /
/// broken / missing / in-repair).
pub fn items_needing_attention(vault: &Vault) -> Vec<Item> {
    scan_vault(vault)
        .into_iter()
        .filter(|i| {
            let cond = crate::model::Condition::from_str(&i.condition);
            let stat = crate::model::Status::from_str(&i.status);
            cond.map(|c| c.needs_attention()).unwrap_or(false)
                || matches!(
                    stat,
                    Some(crate::model::Status::Missing | crate::model::Status::InRepair)
                )
        })
        .collect()
}
