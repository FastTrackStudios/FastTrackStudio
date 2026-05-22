//! `Recipe` → markdown bytes + path helpers.
//!
//! Default folder is `Wiki/Cookbook/` so recipes ride on the
//! wiki feature for graph + linking. Empty optional fields are
//! skipped to keep new files terse.

use std::path::{Path, PathBuf};

use chrono::Utc;
use thiserror::Error;

use crate::model::Recipe;

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

pub fn serialize_recipe(recipe: &Recipe) -> Result<String, WriteError> {
    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "recipe".into());
    let body_yaml = serde_yaml::to_value(recipe).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(m) = body_yaml {
        for (k, v) in m {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| WriteError::Yaml(e.to_string()))?;
    let body = if recipe.details.is_empty() {
        String::new()
    } else if recipe.details.starts_with('\n') {
        recipe.details.clone()
    } else {
        format!("\n{}", recipe.details)
    };
    Ok(format!("---\n{yaml}---\n{body}"))
}

pub fn write_recipe(
    vault_root: &Path,
    recipe: &mut Recipe,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    if recipe.path.is_empty() {
        return Err(WriteError::BadPath("recipe.path is empty".into()));
    }
    let abs = vault_root.join(&recipe.path);
    if !overwrite && abs.exists() {
        return Err(WriteError::Exists(abs.display().to_string()));
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| WriteError::Io(e.to_string()))?;
    }
    let now = Utc::now();
    if recipe.date_created.is_none() {
        recipe.date_created = Some(now);
    }
    recipe.date_modified = Some(now);
    let body = serialize_recipe(recipe)?;
    std::fs::write(&abs, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Default layout: `Wiki/Cookbook/<slug>.md`. Override
/// `folder` only when you want a different sub-folder under
/// the wiki (e.g. `"Wiki/Cookbook/Drafts"`).
#[must_use]
pub fn default_recipe_path(name: &str, folder: Option<&str>) -> String {
    let slug = slugify(name);
    match folder {
        Some(f) => format!("{}/{slug}.md", f.trim_end_matches('/')),
        None => format!("Wiki/Cookbook/{slug}.md"),
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
        out.push_str("recipe");
    }
    out
}
