//! `Milestone` → markdown bytes. Frontmatter carries
//! `type: milestone` for the parser discriminator.

use std::path::{Path, PathBuf};

use chrono::Utc;
use thiserror::Error;

use crate::model::Milestone;

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

pub fn serialize_milestone(m: &Milestone) -> Result<String, WriteError> {
    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "milestone".into());
    let body_yaml = serde_yaml::to_value(m).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(map) = body_yaml {
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

pub fn write_milestone(
    vault_root: &Path,
    m: &mut Milestone,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    if m.path.is_empty() {
        return Err(WriteError::BadPath("milestone.path is empty".into()));
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
    let body = serialize_milestone(m)?;
    std::fs::write(&abs, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Default layout: a `milestones/` subdir inside the project's
/// own folder. Given the project's vault-relative path:
///
/// - `Projects/Health/Health.md` → `Projects/Health/milestones/<ms-slug>.md`
/// - `Projects/Mealplan.md`      → `Projects/Mealplan/milestones/<ms-slug>.md`
///
/// In the flat case the folder is created on first write; the
/// project file stays as a sibling of the new folder. Honors
/// the project's on-disk casing so existing `Projects/Health/`
/// trees stay one folder, not two.
#[must_use]
pub fn default_milestone_path(project_rel_path: &str, title: &str) -> String {
    let ms = slugify(title);
    // Derive the project's folder:
    // - if the project file is `X/X.md` or `X/something.md`, use `X/`
    // - if it's a flat `X.md`, use the file stem
    let p = std::path::Path::new(project_rel_path);
    let parent = p.parent().and_then(|x| x.to_str()).unwrap_or("");
    let stem = p.file_stem().and_then(|x| x.to_str()).unwrap_or("");
    if parent == "Projects" || parent.is_empty() {
        // Flat project file. Create a sibling folder named
        // after the stem (preserving its on-disk casing).
        if parent.is_empty() {
            format!("{stem}/milestones/{ms}.md")
        } else {
            format!("{parent}/{stem}/milestones/{ms}.md")
        }
    } else {
        // Nested: project lives inside its own folder already.
        // Just append `milestones/`.
        format!("{parent}/milestones/{ms}.md")
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
        out.push_str("milestone");
    }
    out
}
