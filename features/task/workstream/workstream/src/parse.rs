//! `vault::VaultPage` → `Workstream`. Discriminator:
//! `type: workstream` in frontmatter (or `workstream` tag).

use thiserror::Error;
use uuid::Uuid;
use vault_proto::VaultPage;
use workflows_proto::AgentRef;

use crate::model::{AgentRefList, Links, Workstream};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("page has no frontmatter")]
    NoFrontmatter,
    #[error("frontmatter is not a YAML mapping")]
    NotAMapping,
    #[error("frontmatter parse: {0}")]
    Yaml(String),
    #[error("workstream is missing required `projectId`")]
    MissingProject,
}

#[must_use]
pub fn looks_like_workstream(page: &VaultPage) -> bool {
    let Some((fm, _)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let Ok(map) = serde_yaml::from_str::<serde_yaml::Mapping>(fm) else {
        return false;
    };
    if map.get("type").and_then(|v| v.as_str()) == Some("workstream") {
        return true;
    }
    if let Some(seq) = map.get("tags").and_then(|v| v.as_sequence()) {
        return seq.iter().any(|v| v.as_str() == Some("workstream"));
    }
    false
}

pub fn parse_page(page: &VaultPage) -> Result<Workstream, ParseError> {
    parse_workstream(&page.rel_path, &page.basename, &page.raw)
}

pub fn parse_workstream(
    rel_path: &str,
    basename: &str,
    raw: &str,
) -> Result<Workstream, ParseError> {
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
    let status = take_str(&map, "status").unwrap_or_else(|| "backlog".into());
    // `lead` / `members` are serde-tagged `AgentRef` mappings —
    // deserialize through serde_yaml rather than field-picking.
    let lead: Option<AgentRef> = map
        .get("lead")
        .and_then(|v| serde_yaml::from_value(v.clone()).ok());
    let members: Vec<AgentRef> = map
        .get("members")
        .and_then(|v| serde_yaml::from_value(v.clone()).ok())
        .unwrap_or_default();
    let start_date = take_str(&map, "startDate")
        .or_else(|| take_str(&map, "start_date"))
        .and_then(|s| s.parse().ok());
    let target_date = take_str(&map, "targetDate")
        .or_else(|| take_str(&map, "target_date"))
        .and_then(|s| s.parse().ok());
    let links = take_string_list(&map, "links");
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    Ok(Workstream {
        path: rel_path.to_string(),
        id,
        title,
        project_id,
        status,
        lead,
        members: AgentRefList(members),
        start_date,
        target_date,
        links: Links(links),
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
