//! Vault-wide scanner.

use thiserror::Error;
use uuid::Uuid;
use vault::Vault;

use crate::model::ProjectInfo;
use crate::parse::{ParseError, looks_like_project, parse_page};

#[derive(Debug, Error)]
pub enum ScanError {
    #[error("vault: {0}")]
    Vault(String),
}

/// Collect every project page in the vault. Logs + skips
/// files whose frontmatter fails to parse — one malformed
/// project shouldn't nuke the whole list.
///
/// Pages with `id: <nil>` (e.g. just-created by hand without
/// running `write_project` first) get a fresh UUID in the
/// returned `ProjectInfo`. The on-disk file is **not**
/// rewritten here — callers that want the id persisted must
/// follow up with `write_project(..., overwrite=true)`.
pub fn scan_vault(vault: &Vault) -> Result<Vec<ProjectInfo>, ScanError> {
    let mut out = Vec::new();
    for page in &vault.pages {
        let proto = page.to_proto();
        if !looks_like_project(&proto) {
            continue;
        }
        match parse_page(&proto) {
            Ok(mut p) => {
                if p.id.is_nil() {
                    p.id = Uuid::new_v4();
                    tracing::debug!(
                        path = %p.path,
                        new_id = %p.id,
                        "project: backfilled missing id (not persisted)",
                    );
                }
                out.push(p);
            }
            Err(ParseError::NoFrontmatter) => {
                // Project discriminator without a complete
                // frontmatter — skip silently. Could happen
                // mid-edit.
            }
            Err(e) => {
                tracing::warn!(path = %page.rel_path, error = %e, "project: parse failed");
            }
        }
    }
    Ok(out)
}
