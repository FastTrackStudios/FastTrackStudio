//! `vault::VaultPage` → `BodyMetric`. Discriminator
//! `type: body-metric` in frontmatter or `body-metric` in
//! tags.

use thiserror::Error;
use uuid::Uuid;
use vault::VaultPage;

use crate::model::{BodyEntry, BodyMetric};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("page has no frontmatter")]
    NoFrontmatter,
    #[error("frontmatter is not a YAML mapping")]
    NotAMapping,
    #[error("frontmatter parse: {0}")]
    Yaml(String),
}

pub fn looks_like_body_metric(page: &VaultPage) -> bool {
    let Some((fm, _)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let Ok(map) = serde_yaml::from_str::<serde_yaml::Mapping>(fm) else {
        return false;
    };
    if map.get("type").and_then(|v| v.as_str()) == Some("body-metric") {
        return true;
    }
    if let Some(seq) = map.get("tags").and_then(|v| v.as_sequence()) {
        return seq.iter().any(|v| v.as_str() == Some("body-metric"));
    }
    false
}

pub fn parse_page(page: &VaultPage) -> Result<BodyMetric, ParseError> {
    let (fm, body) = split_frontmatter(&page.raw).ok_or(ParseError::NoFrontmatter)?;
    let map: serde_yaml::Mapping =
        serde_yaml::from_str(fm).map_err(|e| ParseError::Yaml(e.to_string()))?;

    let id = take_str(&map, "id")
        .and_then(|s| Uuid::parse_str(&s).ok())
        .unwrap_or_else(|| Uuid::new_v5(&Uuid::NAMESPACE_URL, page.rel_path.as_bytes()));
    let name = take_str(&map, "name").unwrap_or_else(|| page.basename.clone());
    let kind = take_str(&map, "kind").unwrap_or_else(|| "other".into());
    let unit = take_str(&map, "unit").unwrap_or_default();
    let goal = map.get("goal").and_then(|v| v.as_f64());
    let tags = take_string_list(&map, "tags")
        .into_iter()
        .filter(|t| t != "body-metric")
        .collect();
    let entries = crate::model::Entries(parse_entries(&map));
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    Ok(BodyMetric {
        path: page.rel_path.clone(),
        id,
        name,
        kind,
        unit,
        goal,
        tags,
        entries,
        date_created,
        date_modified,
        details: body.to_string(),
    })
}

fn parse_entries(map: &serde_yaml::Mapping) -> Vec<BodyEntry> {
    let Some(seq) = map.get("entries").and_then(|v| v.as_sequence()) else {
        return Vec::new();
    };
    seq.iter()
        .filter_map(|row| {
            let m = row.as_mapping()?;
            let id = m
                .get("id")
                .and_then(|v| v.as_str())
                .and_then(|s| Uuid::parse_str(s).ok())
                .unwrap_or_else(Uuid::new_v4);
            let date = m
                .get("date")
                .and_then(|v| v.as_str())
                .and_then(|s| s.parse().ok())?;
            let value = m.get("value").and_then(|v| v.as_f64())?;
            let unit = m
                .get("unit")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            let note = m
                .get("note")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            Some(BodyEntry {
                id,
                date,
                value,
                unit,
                note,
            })
        })
        .collect()
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
    let Some(v) = map.get(key) else {
        return Vec::new();
    };
    match v {
        serde_yaml::Value::Sequence(seq) => seq
            .iter()
            .filter_map(|item| item.as_str().map(|s| s.to_string()))
            .collect(),
        serde_yaml::Value::String(s) => vec![s.clone()],
        _ => Vec::new(),
    }
}
