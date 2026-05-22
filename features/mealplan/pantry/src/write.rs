//! `PantryItem` → markdown bytes + path helpers.
//!
//! Frontmatter carries `type: item` and ensures `pantry` is
//! in the tag list, so the page round-trips through both the
//! inventory scanner and ours. Empty optional fields are
//! dropped to keep new files terse.

use std::path::{Path, PathBuf};

use chrono::Utc;
use thiserror::Error;

use crate::model::PantryItem;

#[derive(Debug, Error)]
pub enum WriteError {
    #[error("yaml: {0}")]
    Yaml(String),
    #[error("io: {0}")]
    Io(String),
    #[error("file exists at {0}; refusing to overwrite (pass overwrite=true)")]
    Exists(String),
    #[error("bad path: {0}")]
    BadPath(String),
}

pub fn serialize_pantry_item(item: &PantryItem) -> Result<String, WriteError> {
    // Make sure the `pantry` tag is present before we hand
    // the row to YAML — pantry's discriminator depends on it.
    let mut owned = item.clone();
    if !owned.tags.iter().any(|t| t == "pantry") {
        owned.tags.push("pantry".to_string());
    }

    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "item".into());
    let body_yaml = serde_yaml::to_value(&owned).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(m) = body_yaml {
        for (k, v) in m {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| WriteError::Yaml(e.to_string()))?;
    let body = if item.details.is_empty() {
        String::new()
    } else if item.details.starts_with('\n') {
        item.details.clone()
    } else {
        format!("\n{}", item.details)
    };
    Ok(format!("---\n{yaml}---\n{body}"))
}

pub fn write_pantry_item(
    vault_root: &Path,
    item: &mut PantryItem,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    if item.path.is_empty() {
        return Err(WriteError::BadPath("pantry item.path is empty".into()));
    }
    let abs = vault_root.join(&item.path);
    if !overwrite && abs.exists() {
        return Err(WriteError::Exists(abs.display().to_string()));
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| WriteError::Io(e.to_string()))?;
    }
    let now = Utc::now();
    if item.date_created.is_none() {
        item.date_created = Some(now);
    }
    item.date_modified = Some(now);
    let body = serialize_pantry_item(item)?;
    std::fs::write(&abs, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Default layout: `pantry/<slug>.md`. Lives alongside (not
/// inside) `inventory/` so casual filesystem inspection
/// surfaces food vs. gear without reading frontmatter.
#[must_use]
pub fn default_pantry_path(name: &str, folder: Option<&str>) -> String {
    let slug = slugify(name);
    match folder {
        Some(f) => format!("{}/{slug}.md", f.trim_end_matches('/')),
        None => format!("pantry/{slug}.md"),
    }
}

pub(crate) fn slugify(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut prev_dash = false;
    for ch in s.chars() {
        if ch.is_alphanumeric() {
            for lc in ch.to_lowercase() {
                out.push(lc);
            }
            prev_dash = false;
        } else if !prev_dash && !out.is_empty() {
            out.push('-');
            prev_dash = true;
        }
    }
    while out.ends_with('-') {
        out.pop();
    }
    if out.is_empty() {
        out.push_str("pantry-item");
    }
    out
}
