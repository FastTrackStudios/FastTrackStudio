//! `TaskInfo` → markdown bytes, and write-to-disk helpers.
//!
//! The frontmatter serializer uses `serde_yaml` and *only*
//! emits keys whose value is non-default — empty `tags: []`,
//! `contexts: []`, missing `due`, etc. are skipped. That keeps
//! freshly-created task files terse and the diff-noise on
//! `done`-flips minimal.

use std::path::{Path, PathBuf};

use chrono::Utc;
use thiserror::Error;

use crate::model::TaskInfo;

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

/// Serialize a `TaskInfo` into the full markdown source:
/// frontmatter block + body. Round-trip-clean for the fields
/// the model tracks; anything the source file had that doesn't
/// map to a `TaskInfo` field is *not* preserved (use
/// `vault_obsidian::set_property` for surgical edits).
pub fn serialize_task(task: &TaskInfo) -> Result<String, WriteError> {
    // `type: task` is the scanner's discriminator. Emit it
    // first so freshly written tasks round-trip through
    // `looks_like_task` without depending on a `task` tag.
    // Mirrors `project::serialize_project` / `goal::serialize_goal`.
    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "task".into());
    let body_yaml = serde_yaml::to_value(task).map_err(|e| WriteError::Yaml(e.to_string()))?;
    if let serde_yaml::Value::Mapping(map) = body_yaml {
        for (k, v) in map {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| WriteError::Yaml(e.to_string()))?;
    // Always end the close fence with `\n` so consumers that
    // detect frontmatter via the `\n---\n` pattern (vault-obsidian,
    // task::parse, anything else) see the boundary cleanly. For
    // empty bodies the file ends with `---\n`; non-empty bodies
    // get their leading newline normalized.
    let body = if task.details.is_empty() {
        String::new()
    } else if task.details.starts_with('\n') {
        task.details.clone()
    } else {
        format!("\n{}", task.details)
    };
    Ok(format!("---\n{yaml}---\n{body}"))
}

/// Write a `TaskInfo` to `<vault_root>/<task.path>`. Creates
/// parent directories. Refuses to overwrite an existing file
/// unless `overwrite` is true (so `task capture` doesn't
/// silently clobber).
pub fn write_task(
    vault_root: &Path,
    task: &mut TaskInfo,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    if task.path.is_empty() {
        return Err(WriteError::BadPath("task.path is empty".into()));
    }
    let abs = vault_root.join(&task.path);
    if !overwrite && abs.exists() {
        return Err(WriteError::Exists(abs.display().to_string()));
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| WriteError::Io(e.to_string()))?;
    }
    let now = Utc::now();
    if task.date_created.is_none() {
        task.date_created = Some(now);
    }
    task.date_modified = Some(now);

    let body = serialize_task(task)?;
    std::fs::write(&abs, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Compute a vault-relative path for a new task from its title.
/// Default layout: `Task/<slug>.md`. Slug is lowercase, spaces
/// → `-`, non-alphanumerics dropped. Pass `folder` to override
/// the default `Task/` (e.g. `Some("Projects/website")`).
#[must_use]
pub fn default_task_path(title: &str, folder: Option<&str>) -> String {
    let slug = slugify(title);
    match folder {
        Some(f) => format!("{}/{slug}.md", f.trim_end_matches('/')),
        None => format!("Task/{slug}.md"),
    }
}

fn slugify(s: &str) -> String {
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
        out.push_str("task");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slugify_lowercases_and_dashes() {
        assert_eq!(slugify("Buy Milk Today!"), "buy-milk-today");
        assert_eq!(slugify("  multiple   spaces  "), "multiple-spaces");
        assert_eq!(slugify("Fix Bug #123"), "fix-bug-123");
        assert_eq!(slugify("!@#$%"), "task");
    }

    #[test]
    fn default_path_uses_tasks_folder() {
        assert_eq!(default_task_path("Buy milk", None), "Task/buy-milk.md");
        assert_eq!(
            default_task_path("Buy milk", Some("Projects/groceries")),
            "Projects/groceries/buy-milk.md"
        );
    }

    #[test]
    fn workflow_attrs_round_trip_through_yaml() {
        use crate::model::{AgentRefList, Estimate, UuidList, WorkflowAttrs};
        use workflows_proto::AgentRef;

        let cycle = uuid::Uuid::new_v4();
        let blocker = uuid::Uuid::new_v4();

        let attrs = WorkflowAttrs {
            cycle: Some(cycle),
            estimate: Some(Estimate::M),
            assignees: AgentRefList(vec![
                AgentRef::human("cody"),
                AgentRef::agent_versioned("claude", "opus-4-7"),
            ]),
            blockers: UuidList(vec![blocker]),
            relates_to: UuidList::default(),
            session: None,
        };

        let task = TaskInfo {
            id: uuid::Uuid::new_v4(),
            path: "tasks/example.md".into(),
            title: "example".into(),
            status: "open".into(),
            priority: "normal".into(),
            due: None,
            scheduled: None,
            tags: crate::model::StringList(vec!["task".into()]),
            contexts: crate::model::StringList::default(),
            projects: crate::model::StringList::default(),
            project_id: None,
            milestone_id: None,
            time_estimate: None,
            time_entries: crate::model::TimeEntries::default(),
            recurrence: None,
            recurrence_anchor: None,
            complete_instances: crate::model::StringList::default(),
            completed_date: None,
            agent_profile: String::new(),
            dispatched_agent_tasks: crate::model::StringList::default(),
            date_created: None,
            date_modified: None,
            details: String::new(),
            workflow: Some(attrs.clone()),
        };

        let yaml = serialize_task(&task).expect("serialize");
        assert!(
            yaml.contains("workflow:"),
            "yaml missing workflow key:\n{yaml}"
        );
        assert!(yaml.contains("cycle:"), "yaml missing cycle key");
        assert!(
            yaml.contains(&format!("{cycle}")),
            "yaml missing cycle uuid"
        );

        // Round-trip back through the parser. Build a fake VaultPage.
        let parsed = crate::parse::parse_str("tasks/example.md", "example", &yaml).expect("parse");
        let parsed_attrs = parsed.workflow.expect("workflow attrs present");
        assert_eq!(parsed_attrs.cycle, Some(cycle));
        assert_eq!(parsed_attrs.assignees.len(), 2);
        assert_eq!(parsed_attrs.blockers.0, vec![blocker]);
    }

    #[test]
    fn workflow_omitted_when_none() {
        let task = TaskInfo {
            id: uuid::Uuid::new_v4(),
            path: "tasks/plain.md".into(),
            title: "plain".into(),
            status: "open".into(),
            priority: "normal".into(),
            due: None,
            scheduled: None,
            tags: crate::model::StringList(vec!["task".into()]),
            contexts: crate::model::StringList::default(),
            projects: crate::model::StringList::default(),
            project_id: None,
            milestone_id: None,
            time_estimate: None,
            time_entries: crate::model::TimeEntries::default(),
            recurrence: None,
            recurrence_anchor: None,
            complete_instances: crate::model::StringList::default(),
            completed_date: None,
            agent_profile: String::new(),
            dispatched_agent_tasks: crate::model::StringList::default(),
            date_created: None,
            date_modified: None,
            details: String::new(),
            workflow: None,
        };
        let yaml = serialize_task(&task).expect("serialize");
        assert!(
            !yaml.contains("workflow:"),
            "expected 'workflow' key to be omitted when None:\n{yaml}"
        );
    }
}
