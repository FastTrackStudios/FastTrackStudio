//! Walk a `vault::Vault` and collect every page that looks
//! like a location. Pages without the discriminator are
//! skipped silently; parse failures are logged + skipped.

use vault::Vault;

use crate::model::Location;
use crate::parse::{looks_like_location, parse_page};

pub fn scan_vault(vault: &Vault) -> Vec<Location> {
    vault
        .pages
        .iter()
        .filter(|p| looks_like_location(p))
        .filter_map(|p| match parse_page(p) {
            Ok(loc) => Some(loc),
            Err(e) => {
                tracing::warn!(path = %p.rel_path, ?e, "location parse failed");
                None
            }
        })
        .collect()
}
