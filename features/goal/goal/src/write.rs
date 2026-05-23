//! `Goal` → markdown bytes + path helpers.
//!
//! Frontmatter always carries `type: goal` so the parser has
//! a single discriminator. Empty optional fields are skipped
//! to keep new files terse.

use std::path::{Path, PathBuf};

use chrono::Utc;
use thiserror::Error;

use crate::model::Goal;

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

pub fn serialize_goal(goal: &Goal) -> Result<String, WriteError> {
    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "goal".into());
    let body_yaml = serde_yaml::to_value(goal).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(m) = body_yaml {
        for (k, v) in m {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| WriteError::Yaml(e.to_string()))?;
    let body = if goal.details.is_empty() {
        String::new()
    } else if goal.details.starts_with('\n') {
        goal.details.clone()
    } else {
        format!("\n{}", goal.details)
    };
    Ok(format!("---\n{yaml}---\n{body}"))
}

pub fn write_goal(
    vault_root: &Path,
    goal: &mut Goal,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    if goal.path.is_empty() {
        return Err(WriteError::BadPath("goal.path is empty".into()));
    }
    let abs = vault_root.join(&goal.path);
    if !overwrite && abs.exists() {
        return Err(WriteError::Exists(abs.display().to_string()));
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| WriteError::Io(e.to_string()))?;
    }
    let now = Utc::now();
    if goal.date_created.is_none() {
        goal.date_created = Some(now);
    }
    goal.date_modified = Some(now);
    let body = serialize_goal(goal)?;
    std::fs::write(&abs, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Default layout: `Goals/<slug>.md` for top-level; nested
/// decompositions can live at `Goals/<parent-slug>/<slug>.md`
/// — pass `folder` to override.
#[must_use]
pub fn default_goal_path(title: &str, folder: Option<&str>) -> String {
    let slug = slugify(title);
    match folder {
        Some(f) => format!("{}/{slug}.md", f.trim_end_matches('/')),
        None => format!("Goals/{slug}.md"),
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
        out.push_str("goal");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{Goal, Tags};
    use uuid::Uuid;

    #[test]
    fn round_trip_minimal() {
        let mut g = Goal {
            path: "Goals/buy-a-house.md".into(),
            id: Uuid::new_v4(),
            title: "Buy a House".into(),
            kind: "lifetime".into(),
            status: "aspiration".into(),
            parent_id: None,
            target_date: None,
            cycle_id: None,
            tags: Tags(vec!["housing".into()]),
            date_created: None,
            date_modified: None,
            details: "Vision: own a place by 35.".into(),
        };
        let tmp = tempfile::tempdir().unwrap();
        let path = write_goal(tmp.path(), &mut g, false).unwrap();
        let raw = std::fs::read_to_string(&path).unwrap();
        assert!(raw.contains("type: goal"));
        assert!(raw.contains("title: Buy a House"));
        assert!(raw.contains("Vision"));

        let parsed = crate::parse::parse_goal(&g.path, "buy-a-house", &raw).unwrap();
        assert_eq!(parsed.title, g.title);
        assert_eq!(parsed.kind, g.kind);
        assert_eq!(parsed.id, g.id);
    }
}
