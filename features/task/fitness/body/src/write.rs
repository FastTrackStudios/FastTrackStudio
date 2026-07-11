//! `BodyMetric` → markdown bytes + path helpers.

use std::path::{Path, PathBuf};

use chrono::Utc;
use thiserror::Error;

use crate::model::BodyMetric;

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

pub fn serialize_metric(m: &BodyMetric) -> Result<String, WriteError> {
    // Sort entries ascending by date on write so the page
    // reads chronologically in a text editor + chart-builders
    // don't need to re-sort.
    let mut owned = m.clone();
    owned.entries.sort_by_key(|e| e.date);

    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "body-metric".into());
    let body = serde_yaml::to_value(&owned).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(map) = body {
        for (k, v) in map {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| WriteError::Yaml(e.to_string()))?;
    let body = if m.details.is_empty() {
        String::new()
    } else if m.details.starts_with('\n') {
        m.details.clone()
    } else {
        format!("\n{}", m.details)
    };
    Ok(format!("---\n{yaml}---\n{body}"))
}

pub fn write_metric(
    vault_root: &Path,
    m: &mut BodyMetric,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    if m.path.is_empty() {
        return Err(WriteError::BadPath("metric.path is empty".into()));
    }
    let abs = vault_root.join(&m.path);
    if !overwrite && abs.exists() {
        return Err(WriteError::Exists(abs.display().to_string()));
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| WriteError::Io(e.to_string()))?;
    }
    let now = Utc::now();
    if m.date_created.is_none() {
        m.date_created = Some(now);
    }
    m.date_modified = Some(now);
    let body = serialize_metric(m)?;
    std::fs::write(&abs, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Default layout: `Projects/Fitness/body/<slug>.md` (e.g.
/// `Projects/Fitness/body/weight.md`, `.../bodyfat.md`).
pub fn default_metric_path(name: &str, folder: Option<&str>) -> String {
    let slug = slugify(name);
    match folder {
        Some(f) => format!("{}/{slug}.md", f.trim_end_matches('/')),
        None => format!("Projects/Fitness/body/{slug}.md"),
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
        out.push_str("body-metric");
    }
    out
}
