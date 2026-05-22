//! Walk a `vault::Vault` and collect every page that looks like
//! a task. Returns the parsed `TaskInfo` rows; pages that don't
//! match the task discriminator are skipped silently.

use vault::Vault;

use crate::model::TaskInfo;
use crate::parse::{looks_like_task, parse_page};

pub fn scan_vault(vault: &Vault) -> Vec<TaskInfo> {
    vault
        .pages
        .iter()
        .filter(|p| looks_like_task(p))
        .filter_map(|p| match parse_page(p) {
            Ok(t) => Some(t),
            Err(e) => {
                tracing::warn!(path = %p.rel_path, ?e, "task parse failed");
                None
            }
        })
        .collect()
}
