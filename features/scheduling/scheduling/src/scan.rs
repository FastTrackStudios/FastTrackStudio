//! Vault scanner — walks a `vault::Vault` and yields scheduling
//! entities by frontmatter `type:` discriminator.
//!
//! v1 only emits day templates; event-type / schedule / booking
//! scanning lands with the `VaultScheduler` impl in a follow-up.
//! The shape here is the same surface the `task` crate's
//! `scan_vault` exposes, so a future generic scanner can absorb
//! both.

use thiserror::Error;

use scheduling_proto::DayTemplate;

#[derive(Debug, Error)]
pub enum ScanError {
    #[error("parse error in {path}: {source}")]
    Parse {
        path: String,
        #[source]
        source: crate::parse::ParseError,
    },
    #[error("vault io: {0}")]
    Vault(String),
}

/// Empty stub. Real implementation walks `vault.pages()`, filters
/// by `frontmatter.get("type")`, dispatches to the right parser.
/// Lives here so consumers can already wire a call site even
/// though the body is no-op.
pub fn scan_day_templates(_vault: &vault::Vault) -> Result<Vec<DayTemplate>, ScanError> {
    Ok(Vec::new())
}
