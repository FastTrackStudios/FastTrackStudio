//! `vault::VaultPage` → `TaskInfo`.
//!
//! Reads the parsed frontmatter, picks out the TaskNotes-shape
//! fields, and falls back to defaults for everything missing.
//! Unknown frontmatter keys are *not* preserved on round-trip
//! through `TaskInfo` — they live on the source `VaultPage`
//! instead. That keeps the typed surface narrow and lets
//! callers reach for the raw frontmatter when they need it.

use serde_json::Value;
use thiserror::Error;
use vault::VaultPage;

use crate::model::{TaskInfo, TimeEntry};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("page has no frontmatter")]
    NoFrontmatter,
    #[error("frontmatter is not a YAML mapping")]
    NotAMapping,
    #[error("frontmatter parse: {0}")]
    Yaml(String),
}

/// Whether the page looks like a task. Conventional discriminators:
/// `type: task` field, OR `task` present in the `tags` array.
pub fn looks_like_task(page: &VaultPage) -> bool {
    let Some((fm, _body)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let Ok(map) = serde_yaml::from_str::<serde_yaml::Mapping>(fm) else {
        return false;
    };
    if let Some(t) = map.get("type") {
        if t.as_str() == Some("task") {
            return true;
        }
    }
    if let Some(tags) = map.get("tags") {
        if let Some(seq) = tags.as_sequence() {
            if seq.iter().any(|v| v.as_str() == Some("task")) {
                return true;
            }
        }
    }
    false
}

/// Parse a `VaultPage` into a `TaskInfo`. The page must have
/// YAML frontmatter; missing optional fields default to their
/// `Default::default()` equivalent.
pub fn parse_page(page: &VaultPage) -> Result<TaskInfo, ParseError> {
    let (fm, body) = split_frontmatter(&page.raw).ok_or(ParseError::NoFrontmatter)?;
    let map: serde_yaml::Mapping =
        serde_yaml::from_str(fm).map_err(|e| ParseError::Yaml(e.to_string()))?;

    let title = take_str(&map, "title").unwrap_or_else(|| page.basename.clone());
    let status = take_str(&map, "status").unwrap_or_else(|| "open".into());
    let priority = take_str(&map, "priority").unwrap_or_else(|| "normal".into());
    let due = take_str(&map, "due");
    let scheduled = take_str(&map, "scheduled");

    let tags = take_string_list(&map, "tags");
    let contexts = take_string_list(&map, "contexts");
    let projects = take_string_list(&map, "projects");

    let time_estimate = map
        .get("timeEstimate")
        .and_then(|v| v.as_u64())
        .and_then(|n| u32::try_from(n).ok());

    let time_entries = map
        .get("timeEntries")
        .and_then(|v| v.as_sequence())
        .map(|seq| {
            seq.iter()
                .filter_map(|row| {
                    let row_map = row.as_mapping()?;
                    let start = row_map
                        .get("startTime")?
                        .as_str()
                        .and_then(|s| s.parse().ok())?;
                    let end = row_map
                        .get("endTime")
                        .and_then(|v| v.as_str())
                        .and_then(|s| s.parse().ok());
                    Some(TimeEntry {
                        start_time: start,
                        end_time: end,
                    })
                })
                .collect()
        })
        .unwrap_or_default();

    let recurrence = take_str(&map, "recurrence");
    let recurrence_anchor = take_str(&map, "recurrence_anchor");
    let complete_instances = take_string_list(&map, "complete_instances");
    let completed_date = take_str(&map, "completedDate").and_then(|s| s.parse().ok());
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    Ok(TaskInfo {
        path: page.rel_path.clone(),
        title,
        status,
        priority,
        due,
        scheduled,
        tags,
        contexts,
        projects,
        time_estimate,
        time_entries,
        recurrence,
        recurrence_anchor,
        complete_instances,
        completed_date,
        date_created,
        date_modified,
        details: body.to_string(),
    })
}

/// Split a markdown source into `(frontmatter_yaml, body)`. The
/// frontmatter must be fenced by `---` on a line by itself
/// immediately at the start of file. Returns `None` if no
/// frontmatter is present.
fn split_frontmatter(src: &str) -> Option<(&str, &str)> {
    let rest = src.strip_prefix("---\n")?;
    let end = rest.find("\n---\n")?;
    Some((&rest[..end], &rest[end + 5..]))
}

fn take_str(map: &serde_yaml::Mapping, key: &str) -> Option<String> {
    map.get(key).and_then(|v| match v {
        serde_yaml::Value::String(s) => Some(s.clone()),
        serde_yaml::Value::Number(n) => Some(n.to_string()),
        serde_yaml::Value::Bool(b) => Some(b.to_string()),
        _ => None,
    })
}

fn take_string_list(map: &serde_yaml::Mapping, key: &str) -> Vec<String> {
    let Some(v) = map.get(key) else {
        return Vec::new();
    };
    match v {
        serde_yaml::Value::Sequence(seq) => seq
            .iter()
            .filter_map(|item| match item {
                serde_yaml::Value::String(s) => Some(s.clone()),
                _ => None,
            })
            .collect(),
        // Tolerate `tags: foo` shorthand (single string).
        serde_yaml::Value::String(s) => vec![s.clone()],
        _ => Vec::new(),
    }
}

// Reserved for future use — JSON variant of `take_str` for
// callers reading the page's raw frontmatter_json blob.
#[allow(dead_code)]
fn json_to_string(v: &Value) -> Option<String> {
    match v {
        Value::String(s) => Some(s.clone()),
        Value::Number(n) => Some(n.to_string()),
        Value::Bool(b) => Some(b.to_string()),
        _ => None,
    }
}
