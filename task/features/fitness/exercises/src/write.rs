//! `Exercise` → markdown bytes + path helpers. Default
//! path lives under `Wiki/Exercises/` so the wiki feature
//! picks the page up like any other curated entry.

use std::path::{Path, PathBuf};

use chrono::Utc;
use thiserror::Error;

use crate::model::Exercise;

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

pub fn serialize_exercise(ex: &Exercise) -> Result<String, WriteError> {
    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "exercise".into());
    let body = serde_yaml::to_value(ex).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(m) = body {
        for (k, v) in m {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| WriteError::Yaml(e.to_string()))?;
    let body = if ex.details.is_empty() {
        String::new()
    } else if ex.details.starts_with('\n') {
        ex.details.clone()
    } else {
        format!("\n{}", ex.details)
    };
    Ok(format!("---\n{yaml}---\n{body}"))
}

pub fn write_exercise(
    vault_root: &Path,
    ex: &mut Exercise,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    if ex.path.is_empty() {
        return Err(WriteError::BadPath("exercise.path is empty".into()));
    }
    let abs = vault_root.join(&ex.path);
    if !overwrite && abs.exists() {
        return Err(WriteError::Exists(abs.display().to_string()));
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| WriteError::Io(e.to_string()))?;
    }
    let now = Utc::now();
    if ex.date_created.is_none() {
        ex.date_created = Some(now);
    }
    ex.date_modified = Some(now);
    let body = serialize_exercise(ex)?;
    std::fs::write(&abs, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Default layout: `Wiki/Exercises/<slug>.md`.
pub fn default_exercise_path(name: &str, folder: Option<&str>) -> String {
    let slug = slugify(name);
    match folder {
        Some(f) => format!("{}/{slug}.md", f.trim_end_matches('/')),
        None => format!("Wiki/Exercises/{slug}.md"),
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
        out.push_str("exercise");
    }
    out
}
