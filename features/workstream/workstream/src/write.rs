//! `Workstream` → markdown bytes. Frontmatter carries
//! `type: workstream` for the parser discriminator.

use std::path::{Path, PathBuf};

use chrono::Utc;
use thiserror::Error;

use crate::model::Workstream;

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

pub fn serialize_workstream(w: &Workstream) -> Result<String, WriteError> {
    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "workstream".into());
    let body_yaml = serde_yaml::to_value(w).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(map) = body_yaml {
        for (k, v) in map {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| WriteError::Yaml(e.to_string()))?;
    let body = if w.details.is_empty() {
        String::new()
    } else if w.details.starts_with('\n') {
        w.details.clone()
    } else {
        format!("\n{}", w.details)
    };
    Ok(format!("---\n{yaml}---\n{body}"))
}

pub fn write_workstream(
    vault_root: &Path,
    w: &mut Workstream,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    if w.path.is_empty() {
        return Err(WriteError::BadPath("workstream.path is empty".into()));
    }
    let abs = vault_root.join(&w.path);
    if !overwrite && abs.exists() {
        return Err(WriteError::Exists(abs.display().to_string()));
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| WriteError::Io(e.to_string()))?;
    }
    let now = Utc::now();
    if w.date_created.is_none() {
        w.date_created = Some(now);
    }
    w.date_modified = Some(now);
    let body = serialize_workstream(w)?;
    std::fs::write(&abs, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Default layout: a `workstreams/` subdir inside the project's
/// own folder — the exact sibling of `milestones/`. Given the
/// project's vault-relative path:
///
/// - `Projects/Health/Health.md` → `Projects/Health/workstreams/<ws-slug>.md`
/// - `Projects/Mealplan.md`      → `Projects/Mealplan/workstreams/<ws-slug>.md`
///
/// In the flat case the folder is created on first write; the
/// project file stays as a sibling of the new folder. Honors
/// the project's on-disk casing so existing `Projects/Health/`
/// trees stay one folder, not two.
#[must_use]
pub fn default_workstream_path(project_rel_path: &str, title: &str) -> String {
    let ws = slugify(title);
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
            format!("{stem}/workstreams/{ws}.md")
        } else {
            format!("{parent}/{stem}/workstreams/{ws}.md")
        }
    } else {
        // Nested: project lives inside its own folder already.
        // Just append `workstreams/`.
        format!("{parent}/workstreams/{ws}.md")
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
        out.push_str("workstream");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{AgentRefList, Links, Workstream};
    use workflows_proto::AgentRef;

    fn sample() -> Workstream {
        Workstream {
            path: "Projects/Demo/workstreams/acp-adapter.md".into(),
            id: uuid::Uuid::new_v4(),
            title: "ACP adapter".into(),
            project_id: uuid::Uuid::new_v4(),
            status: "in-progress".into(),
            lead: Some(AgentRef::human("cody")),
            members: AgentRefList(vec![
                AgentRef::agent_versioned("hermes", "h4"),
                AgentRef::agent("claude"),
            ]),
            start_date: Some(chrono::NaiveDate::from_ymd_opt(2026, 6, 1).unwrap()),
            target_date: Some(chrono::NaiveDate::from_ymd_opt(2026, 7, 15).unwrap()),
            links: Links(vec!["https://example.com/prd".into()]),
            date_created: None,
            date_modified: None,
            details: "The charter.".into(),
        }
    }

    #[test]
    fn workstream_round_trips_through_markdown() {
        let ws = sample();
        let md = serialize_workstream(&ws).expect("serialize");
        assert!(md.contains("type: workstream"), "missing type:\n{md}");
        let parsed =
            crate::parse::parse_workstream(&ws.path, "acp-adapter", &md).expect("parse back");
        assert_eq!(parsed.id, ws.id);
        assert_eq!(parsed.title, ws.title);
        assert_eq!(parsed.project_id, ws.project_id);
        assert_eq!(parsed.status, "in-progress");
        assert_eq!(parsed.lead, ws.lead);
        assert_eq!(parsed.members, ws.members);
        assert_eq!(parsed.start_date, ws.start_date);
        assert_eq!(parsed.target_date, ws.target_date);
        assert_eq!(parsed.links, ws.links);
        assert_eq!(parsed.details.trim(), "The charter.");
    }

    #[test]
    fn default_path_mirrors_milestone_layout() {
        assert_eq!(
            default_workstream_path("Projects/Health/Health.md", "Mobile Push"),
            "Projects/Health/workstreams/mobile-push.md"
        );
        assert_eq!(
            default_workstream_path("Projects/Mealplan.md", "Mobile Push"),
            "Projects/Mealplan/workstreams/mobile-push.md"
        );
    }
}
