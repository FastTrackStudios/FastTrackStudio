//! `ProjectInfo` → markdown bytes + write-to-disk.
//!
//! `id: <uuid>` is always emitted (nil ids are backfilled
//! here), so the file on disk always carries a stable identity
//! downstream features can FK against. Pages that were never
//! written through this path parse with a deterministic
//! uuid-v5-of-path fallback (see `parse_page`), which this
//! writer persists on the next save.

use std::path::{Path, PathBuf};

use thiserror::Error;
use uuid::Uuid;

use crate::model::ProjectInfo;

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

/// Serialize a `ProjectInfo` into the full markdown source:
/// `---` fenced frontmatter + body. Round-trip-clean for the
/// fields the model tracks.
pub fn serialize_project(project: &ProjectInfo) -> Result<String, WriteError> {
    // Make sure `id` is non-nil before serializing so the
    // file is downstream-FK-safe on first write.
    let mut p = project.clone();
    if p.id.is_nil() {
        p.id = Uuid::new_v4();
    }
    // `type: project` is the scanner's discriminator. Emit it
    // first so a fresh `task project create` round-trips
    // through `looks_like_project` without needing a `project`
    // tag. Mirrors `goal::serialize_goal`.
    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "project".into());
    let body_yaml = serde_yaml::to_value(&p).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(m) = body_yaml {
        for (k, v) in m {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| WriteError::Yaml(e.to_string()))?;
    let body = if p.details.is_empty() {
        String::new()
    } else if p.details.starts_with('\n') {
        p.details.clone()
    } else {
        format!("\n{}", p.details)
    };
    Ok(format!("---\n{yaml}---\n{body}"))
}

/// Write a project to `<vault_root>/<project.path>`. Creates
/// parent directories. Refuses to overwrite an existing file
/// unless `overwrite` is true. Backfills `id` if nil.
pub fn write_project(
    vault_root: &Path,
    project: &mut ProjectInfo,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    if project.path.is_empty() {
        return Err(WriteError::BadPath("project.path is empty".into()));
    }
    if project.id.is_nil() {
        project.id = Uuid::new_v4();
    }
    let abs = vault_root.join(&project.path);
    if !overwrite && abs.exists() {
        return Err(WriteError::Exists(abs.display().to_string()));
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| WriteError::Io(e.to_string()))?;
    }
    let serialized = serialize_project(project)?;
    std::fs::write(&abs, serialized).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Conventional path for a freshly captured project — slug
/// from the title, dropped under `Projects/`.
#[must_use]
pub fn default_project_path(title: &str) -> String {
    let slug = title
        .to_ascii_lowercase()
        .chars()
        .map(|c| if c.is_alphanumeric() { c } else { '-' })
        .collect::<String>();
    let cleaned = slug
        .split('-')
        .filter(|s| !s.is_empty())
        .collect::<Vec<_>>()
        .join("-");
    if cleaned.is_empty() {
        "untitled-project.md".to_string()
    } else {
        format!("Projects/{cleaned}.md")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::states::{StateDef, StateGroup, StatesConfig};

    /// `states:` config survives a serialize → parse round-trip,
    /// and absent config round-trips as `None` (so vaults
    /// without registries stay byte-stable).
    #[test]
    fn states_config_round_trips_through_frontmatter() {
        let mut p = crate::parse_str(
            "Projects/demo.md",
            "demo",
            "---\ntype: project\ntitle: Demo\n---\n",
        )
        .expect("minimal project parses");
        assert_eq!(p.states, None);
        p.id = Uuid::new_v4();

        // No config: serialized page carries no `states:` key.
        let plain = serialize_project(&p).expect("serialize");
        assert!(!plain.contains("states:"));

        p.states = Some(StatesConfig(vec![
            StateDef {
                name: "specced".into(),
                group: StateGroup::Unstarted,
                color: "#88ccff".into(),
                default: true,
                order: 1,
            },
            StateDef {
                name: "building".into(),
                group: StateGroup::Started,
                color: String::new(),
                default: false,
                order: 2,
            },
            StateDef {
                name: "shipped".into(),
                group: StateGroup::Completed,
                color: String::new(),
                default: false,
                order: 3,
            },
        ]));
        let raw = serialize_project(&p).expect("serialize");
        let back = crate::parse_str("Projects/demo.md", "demo", &raw).expect("re-parse");
        assert_eq!(back.states, p.states);
        assert_eq!(back.id, p.id);
    }
}
