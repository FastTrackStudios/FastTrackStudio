//! `IntakeLog` → markdown bytes + path helpers.

use std::path::{Path, PathBuf};

use chrono::Utc;
use thiserror::Error;

use crate::model::IntakeLog;

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

pub fn serialize_intake(log: &IntakeLog) -> Result<String, WriteError> {
    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "intake-log".into());
    let body = serde_yaml::to_value(log).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(m) = body {
        for (k, v) in m {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| WriteError::Yaml(e.to_string()))?;
    let body = if log.details.is_empty() {
        String::new()
    } else if log.details.starts_with('\n') {
        log.details.clone()
    } else {
        format!("\n{}", log.details)
    };
    Ok(format!("---\n{yaml}---\n{body}"))
}

pub fn write_intake(
    vault_root: &Path,
    log: &mut IntakeLog,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    if log.path.is_empty() {
        return Err(WriteError::BadPath("intake.path is empty".into()));
    }
    let abs = vault_root.join(&log.path);
    if !overwrite && abs.exists() {
        return Err(WriteError::Exists(abs.display().to_string()));
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| WriteError::Io(e.to_string()))?;
    }
    let now = Utc::now();
    if log.date_created.is_none() {
        log.date_created = Some(now);
    }
    log.date_modified = Some(now);
    let body = serialize_intake(log)?;
    std::fs::write(&abs, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Default layout: `intake/<YYYY-MM-DD>.md`. One page per
/// day; the date is the filename so directory listings
/// sort chronologically.
pub fn default_intake_path(date: chrono::NaiveDate, folder: Option<&str>) -> String {
    let date_str = date.format("%Y-%m-%d");
    match folder {
        Some(f) => format!("{}/{date_str}.md", f.trim_end_matches('/')),
        None => format!("intake/{date_str}.md"),
    }
}
