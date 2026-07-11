//! Walk the vault for intake logs.

use chrono::NaiveDate;
use vault::Vault;

use crate::model::IntakeLog;
use crate::parse::{looks_like_intake, parse_page};

pub fn scan_vault(vault: &Vault) -> Vec<IntakeLog> {
    vault
        .pages
        .iter()
        .filter(|p| looks_like_intake(p))
        .filter_map(|p| match parse_page(p) {
            Ok(l) => Some(l),
            Err(e) => {
                tracing::warn!(path = %p.rel_path, ?e, "intake parse failed");
                None
            }
        })
        .collect()
}

/// Convenience: log on a specific day. First match wins
/// (there's typically one log per day; multi-logs merge
/// at the next write).
pub fn for_day(vault: &Vault, day: NaiveDate) -> Option<IntakeLog> {
    scan_vault(vault).into_iter().find(|l| l.date == day)
}

/// Logs in `[start, end)`. Used by weekly + monthly
/// summary views and by (future) fitness goal tracking.
pub fn between(vault: &Vault, start: NaiveDate, end: NaiveDate) -> Vec<IntakeLog> {
    scan_vault(vault)
        .into_iter()
        .filter(|l| l.date >= start && l.date < end)
        .collect()
}
