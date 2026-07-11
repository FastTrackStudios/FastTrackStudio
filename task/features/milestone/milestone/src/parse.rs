//! `vault::VaultPage` → `Milestone`. Discriminator:
//! `type: milestone` in frontmatter (or `milestone` tag).

use thiserror::Error;
use uuid::Uuid;
use vault_proto::VaultPage;

use crate::model::Milestone;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("page has no frontmatter")]
    NoFrontmatter,
    #[error("frontmatter is not a YAML mapping")]
    NotAMapping,
    #[error("frontmatter parse: {0}")]
    Yaml(String),
    #[error("milestone is missing required `projectId`")]
    MissingProject,
}

#[must_use]
pub fn looks_like_milestone(page: &VaultPage) -> bool {
    let Some((fm, _)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let Ok(map) = serde_yaml::from_str::<serde_yaml::Mapping>(fm) else {
        return false;
    };
    if map.get("type").and_then(|v| v.as_str()) == Some("milestone") {
        return true;
    }
    if let Some(seq) = map.get("tags").and_then(|v| v.as_sequence()) {
        return seq.iter().any(|v| v.as_str() == Some("milestone"));
    }
    false
}

pub fn parse_page(page: &VaultPage) -> Result<Milestone, ParseError> {
    parse_milestone(&page.rel_path, &page.basename, &page.raw)
}

pub fn parse_milestone(rel_path: &str, basename: &str, raw: &str) -> Result<Milestone, ParseError> {
    let (fm, body) = split_frontmatter(raw).ok_or(ParseError::NoFrontmatter)?;
    let map: serde_yaml::Mapping =
        serde_yaml::from_str(fm).map_err(|e| ParseError::Yaml(e.to_string()))?;

    let id = take_str(&map, "id")
        .and_then(|s| Uuid::parse_str(&s).ok())
        .unwrap_or_else(|| Uuid::new_v5(&Uuid::NAMESPACE_URL, rel_path.as_bytes()));
    let title = take_str(&map, "title").unwrap_or_else(|| basename.to_string());
    let project_id = take_str(&map, "projectId")
        .or_else(|| take_str(&map, "project_id"))
        .and_then(|s| Uuid::parse_str(&s).ok())
        .ok_or(ParseError::MissingProject)?;
    let goal_id = take_str(&map, "goalId")
        .or_else(|| take_str(&map, "goal_id"))
        .and_then(|s| Uuid::parse_str(&s).ok());
    let status = take_str(&map, "status").unwrap_or_else(|| "open".into());
    let due_date = take_str(&map, "dueDate")
        .or_else(|| take_str(&map, "due_date"))
        .and_then(|s| s.parse().ok());
    let forge_ref = take_str(&map, "forgeRef").or_else(|| take_str(&map, "forge_ref"));
    let tags = take_string_list(&map, "tags")
        .into_iter()
        .filter(|t| t != "milestone")
        .collect();
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    Ok(Milestone {
        path: rel_path.to_string(),
        id,
        title,
        project_id,
        goal_id,
        status,
        due_date,
        tags,
        forge_ref,
        date_created,
        date_modified,
        details: body.to_string(),
    })
}

pub(crate) fn split_frontmatter(src: &str) -> Option<(&str, &str)> {
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
    match map.get(key) {
        Some(serde_yaml::Value::Sequence(seq)) => seq
            .iter()
            .filter_map(|item| item.as_str().map(ToString::to_string))
            .collect(),
        Some(serde_yaml::Value::String(s)) => vec![s.clone()],
        _ => Vec::new(),
    }
}
