//! `vault::VaultPage` → `Item`.
//!
//! Discriminator: `type: item` in the frontmatter, or
//! `item` in `tags:`. Missing optional fields fall back to
//! defaults; missing `id` is synthesized so legacy pages
//! still load — callers should `write_item` to persist a
//! real uuid.

use thiserror::Error;
use uuid::Uuid;
use vault::VaultPage;

use crate::model::Item;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("page has no frontmatter")]
    NoFrontmatter,
    #[error("frontmatter is not a YAML mapping")]
    NotAMapping,
    #[error("frontmatter parse: {0}")]
    Yaml(String),
}

#[must_use]
pub fn looks_like_item(page: &VaultPage) -> bool {
    let Some((fm, _)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let Ok(map) = serde_yaml::from_str::<serde_yaml::Mapping>(fm) else {
        return false;
    };
    if map.get("type").and_then(|v| v.as_str()) == Some("item") {
        return true;
    }
    if let Some(seq) = map.get("tags").and_then(|v| v.as_sequence()) {
        return seq.iter().any(|v| v.as_str() == Some("item"));
    }
    false
}

pub fn parse_page(page: &VaultPage) -> Result<Item, ParseError> {
    let (fm, body) = split_frontmatter(&page.raw).ok_or(ParseError::NoFrontmatter)?;
    let map: serde_yaml::Mapping =
        serde_yaml::from_str(fm).map_err(|e| ParseError::Yaml(e.to_string()))?;

    let id = take_str(&map, "id")
        .and_then(|s| Uuid::parse_str(&s).ok())
        .unwrap_or_else(|| Uuid::new_v5(&Uuid::NAMESPACE_URL, page.rel_path.as_bytes()));
    let name = take_str(&map, "name").unwrap_or_else(|| page.basename.clone());
    let category = take_str(&map, "category").unwrap_or_default();
    let location_id = take_str(&map, "location_id").and_then(|s| Uuid::parse_str(&s).ok());
    let condition = take_str(&map, "condition").unwrap_or_else(|| "good".into());
    let status = take_str(&map, "status").unwrap_or_else(|| "stored".into());
    let manufacturer = take_str(&map, "manufacturer");
    let model = take_str(&map, "model");
    let serial = take_str(&map, "serial");
    let purchase_date = take_str(&map, "purchaseDate").and_then(|s| s.parse().ok());
    let value = map.get("value").and_then(serde_yaml::Value::as_f64);
    let tasks = take_string_list(&map, "tasks");
    let tags = take_string_list(&map, "tags")
        .into_iter()
        .filter(|t| t != "item")
        .collect();
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    Ok(Item {
        path: page.rel_path.clone(),
        id,
        name,
        category,
        location_id,
        condition,
        status,
        manufacturer,
        model,
        serial,
        purchase_date,
        value,
        tasks: crate::model::StringList(tasks),
        tags: crate::model::StringList(tags),
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
    let Some(v) = map.get(key) else {
        return Vec::new();
    };
    match v {
        serde_yaml::Value::Sequence(seq) => seq
            .iter()
            .filter_map(|item| item.as_str().map(std::string::ToString::to_string))
            .collect(),
        serde_yaml::Value::String(s) => vec![s.clone()],
        _ => Vec::new(),
    }
}
