//! Serializers + path helpers for routines + sessions.

use std::path::{Path, PathBuf};

use chrono::Utc;
use thiserror::Error;

use crate::model::{Routine, WorkoutSession};

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

pub fn serialize_routine(r: &Routine) -> Result<String, WriteError> {
    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "routine".into());
    let body = serde_yaml::to_value(r).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(m) = body {
        for (k, v) in m {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| WriteError::Yaml(e.to_string()))?;
    let body = if r.details.is_empty() {
        String::new()
    } else if r.details.starts_with('\n') {
        r.details.clone()
    } else {
        format!("\n{}", r.details)
    };
    Ok(format!("---\n{yaml}---\n{body}"))
}

pub fn serialize_session(s: &WorkoutSession) -> Result<String, WriteError> {
    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "workout".into());
    let body = serde_yaml::to_value(s).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(m) = body {
        for (k, v) in m {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| WriteError::Yaml(e.to_string()))?;
    let body = if s.details.is_empty() {
        String::new()
    } else if s.details.starts_with('\n') {
        s.details.clone()
    } else {
        format!("\n{}", s.details)
    };
    Ok(format!("---\n{yaml}---\n{body}"))
}

pub fn write_routine(
    vault_root: &Path,
    r: &mut Routine,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    let body = serialize_routine(r)?;
    write_page(
        vault_root,
        &mut r.path,
        &mut r.date_created,
        &mut r.date_modified,
        overwrite,
        &body,
    )
}

pub fn write_session(
    vault_root: &Path,
    s: &mut WorkoutSession,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    let body = serialize_session(s)?;
    write_page(
        vault_root,
        &mut s.path,
        &mut s.date_created,
        &mut s.date_modified,
        overwrite,
        &body,
    )
}

fn write_page(
    vault_root: &Path,
    path: &mut String,
    created: &mut Option<chrono::DateTime<chrono::Utc>>,
    modified: &mut Option<chrono::DateTime<chrono::Utc>>,
    overwrite: bool,
    body: &str,
) -> Result<PathBuf, WriteError> {
    if path.is_empty() {
        return Err(WriteError::BadPath("path is empty".into()));
    }
    let abs = vault_root.join(&*path);
    if !overwrite && abs.exists() {
        return Err(WriteError::Exists(abs.display().to_string()));
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| WriteError::Io(e.to_string()))?;
    }
    let now = Utc::now();
    if created.is_none() {
        *created = Some(now);
    }
    *modified = Some(now);
    std::fs::write(&abs, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Default layout: `routines/<slug>.md`.
pub fn default_routine_path(name: &str, folder: Option<&str>) -> String {
    let slug = slugify(name);
    match folder {
        Some(f) => format!("{}/{slug}.md", f.trim_end_matches('/')),
        None => format!("routines/{slug}.md"),
    }
}

/// Default layout: `workouts/<YYYY-MM-DD>-<slug>.md`. Date
/// goes first so directory listings sort chronologically.
pub fn default_session_path(date: chrono::NaiveDate, name: &str, folder: Option<&str>) -> String {
    let slug = slugify(name);
    let date_str = date.format("%Y-%m-%d");
    match folder {
        Some(f) => format!("{}/{date_str}-{slug}.md", f.trim_end_matches('/')),
        None => format!("workouts/{date_str}-{slug}.md"),
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
        out.push_str("workout");
    }
    out
}
