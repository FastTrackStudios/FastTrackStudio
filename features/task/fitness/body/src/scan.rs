//! Walk the vault for body metrics.

use vault::Vault;

use crate::model::BodyMetric;
use crate::parse::{looks_like_body_metric, parse_page};

pub fn scan_vault(vault: &Vault) -> Vec<BodyMetric> {
    vault
        .pages
        .iter()
        .filter(|p| looks_like_body_metric(p))
        .filter_map(|p| match parse_page(p) {
            Ok(m) => Some(m),
            Err(e) => {
                tracing::warn!(path = %p.rel_path, ?e, "body-metric parse failed");
                None
            }
        })
        .collect()
}

/// Convenience: metric whose `kind` matches (case-insensitive).
/// First match wins; typically there's one page per kind.
pub fn by_kind(vault: &Vault, kind: &str) -> Option<BodyMetric> {
    let needle = kind.to_ascii_lowercase();
    scan_vault(vault)
        .into_iter()
        .find(|m| m.kind.eq_ignore_ascii_case(&needle))
}
