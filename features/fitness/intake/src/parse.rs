//! `vault::VaultPage` → `IntakeLog`. Discriminator
//! `type: intake-log`.

use thiserror::Error;
use uuid::Uuid;
use vault::VaultPage;

use crate::model::{IntakeEntry, IntakeLog, IntakeSource};
use mealplan::cookbook::Nutrition;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("page has no frontmatter")]
    NoFrontmatter,
    #[error("frontmatter is not a YAML mapping")]
    NotAMapping,
    #[error("frontmatter parse: {0}")]
    Yaml(String),
    #[error("missing required field: {0}")]
    MissingField(&'static str),
}

pub fn looks_like_intake(page: &VaultPage) -> bool {
    let Some((fm, _)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let Ok(map) = serde_yaml::from_str::<serde_yaml::Mapping>(fm) else {
        return false;
    };
    if map.get("type").and_then(|v| v.as_str()) == Some("intake-log") {
        return true;
    }
    if let Some(seq) = map.get("tags").and_then(|v| v.as_sequence()) {
        return seq.iter().any(|v| v.as_str() == Some("intake-log"));
    }
    false
}

pub fn parse_page(page: &VaultPage) -> Result<IntakeLog, ParseError> {
    let (fm, body) = split_frontmatter(&page.raw).ok_or(ParseError::NoFrontmatter)?;
    let map: serde_yaml::Mapping =
        serde_yaml::from_str(fm).map_err(|e| ParseError::Yaml(e.to_string()))?;

    let id = take_str(&map, "id")
        .and_then(|s| Uuid::parse_str(&s).ok())
        .unwrap_or_else(|| Uuid::new_v5(&Uuid::NAMESPACE_URL, page.rel_path.as_bytes()));
    let name = take_str(&map, "name").unwrap_or_else(|| page.basename.clone());
    let date = take_str(&map, "date")
        .and_then(|s| s.parse().ok())
        .ok_or(ParseError::MissingField("date"))?;
    let entries = parse_entries(&map);
    let target = map
        .get("target")
        .and_then(|v| serde_yaml::from_value::<Nutrition>(v.clone()).ok());
    let tags = take_string_list(&map, "tags")
        .into_iter()
        .filter(|t| t != "intake-log")
        .collect();
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    Ok(IntakeLog {
        path: page.rel_path.clone(),
        id,
        name,
        date,
        entries,
        target,
        tags,
        date_created,
        date_modified,
        details: body.to_string(),
    })
}

fn parse_entries(map: &serde_yaml::Mapping) -> Vec<IntakeEntry> {
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
            let source = parse_source(m.get("source")?)?;
            let name = m.get("name").and_then(|v| v.as_str())?.to_string();
            let qty = m.get("qty").and_then(|v| v.as_f64())?;
            let unit = m
                .get("unit")
                .and_then(|v| v.as_str())
                .unwrap_or_default()
                .to_string();
            let time = m
                .get("time")
                .and_then(|v| v.as_str())
                .and_then(|s| s.parse().ok());
            let slot = m
                .get("slot")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            let nutrition = m
                .get("nutrition")
                .and_then(|v| serde_yaml::from_value::<Nutrition>(v.clone()).ok());
            let note = m
                .get("note")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            Some(IntakeEntry {
                id,
                source,
                name,
                qty,
                unit,
                time,
                slot,
                nutrition,
                note,
            })
        })
        .collect()
}

fn parse_source(v: &serde_yaml::Value) -> Option<IntakeSource> {
    let m = v.as_mapping()?;
    let kind = m.get("kind").and_then(|v| v.as_str())?;
    match kind {
        "recipe" => {
            let id = m
                .get("id")
                .and_then(|v| v.as_str())
                .and_then(|s| Uuid::parse_str(s).ok())?;
            Some(IntakeSource::Recipe { id })
        }
        "pantry" => {
            let id = m
                .get("id")
                .and_then(|v| v.as_str())
                .and_then(|s| Uuid::parse_str(s).ok())?;
            Some(IntakeSource::Pantry { id })
        }
        "freeform" => Some(IntakeSource::Freeform),
        _ => None,
    }
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
