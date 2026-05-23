//! `vault::VaultPage` → `Goal`.
//!
//! Discriminator: `type: goal` in the frontmatter (or `goal`
//! in `tags:`). Missing optional fields fall back to
//! defaults; missing `id` is synthesized from the path so
//! legacy pages still load — callers should `write_goal` to
//! persist.

use thiserror::Error;
use uuid::Uuid;
use vault::VaultPage;

use crate::model::Goal;

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
pub fn looks_like_goal(page: &VaultPage) -> bool {
    let Some((fm, _)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let Ok(map) = serde_yaml::from_str::<serde_yaml::Mapping>(fm) else {
        return false;
    };
    if map.get("type").and_then(|v| v.as_str()) == Some("goal") {
        return true;
    }
    if let Some(seq) = map.get("tags").and_then(|v| v.as_sequence()) {
        return seq.iter().any(|v| v.as_str() == Some("goal"));
    }
    false
}

pub fn parse_page(page: &VaultPage) -> Result<Goal, ParseError> {
    parse_goal(&page.rel_path, &page.basename, &page.raw)
}

/// Parse goal frontmatter + body. Lower-level surface for
/// callers that don't have a `VaultPage` handy (e.g. CLI
/// importers, migration scripts).
pub fn parse_goal(rel_path: &str, basename: &str, raw: &str) -> Result<Goal, ParseError> {
    let (fm, body) = split_frontmatter(raw).ok_or(ParseError::NoFrontmatter)?;
    let map: serde_yaml::Mapping =
        serde_yaml::from_str(fm).map_err(|e| ParseError::Yaml(e.to_string()))?;

    let id = take_str(&map, "id")
        .and_then(|s| Uuid::parse_str(&s).ok())
        .unwrap_or_else(|| Uuid::new_v5(&Uuid::NAMESPACE_URL, rel_path.as_bytes()));
    let title = take_str(&map, "title").unwrap_or_else(|| basename.to_string());
    let kind = take_str(&map, "kind").unwrap_or_else(|| "lifetime".into());
    let status = take_str(&map, "status").unwrap_or_else(|| "aspiration".into());
    let parent_id = take_str(&map, "parentId")
        .or_else(|| take_str(&map, "parent_id"))
        .and_then(|s| Uuid::parse_str(&s).ok());
    let target_date = take_str(&map, "targetDate")
        .or_else(|| take_str(&map, "target_date"))
        .and_then(|s| s.parse().ok());
    let cycle_id = take_str(&map, "cycleId")
        .or_else(|| take_str(&map, "cycle_id"))
        .and_then(|s| Uuid::parse_str(&s).ok());
    let tags = take_string_list(&map, "tags")
        .into_iter()
        .filter(|t| t != "goal")
        .collect();
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    Ok(Goal {
        path: rel_path.to_string(),
        id,
        title,
        kind,
        status,
        parent_id,
        target_date,
        cycle_id,
        tags,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Tags;

    #[test]
    fn round_trip_minimal_goal() {
        let raw = "---\ntype: goal\nid: 550e8400-e29b-41d4-a716-446655440000\ntitle: Buy a House\nkind: lifetime\nstatus: aspiration\ntags:\n- housing\n- long-term\n---\nVision: own a place by 35.\n";
        let g = parse_goal("Goals/buy-a-house.md", "buy-a-house", raw).unwrap();
        assert_eq!(g.title, "Buy a House");
        assert_eq!(g.kind, "lifetime");
        assert_eq!(g.status, "aspiration");
        assert_eq!(g.tags, Tags(vec!["housing".into(), "long-term".into()]));
        assert!(g.details.contains("Vision"));
    }

    #[test]
    fn accepts_snake_case_parent_id() {
        let raw = "---\ntype: goal\nid: 550e8400-e29b-41d4-a716-446655440000\nparent_id: 660e8400-e29b-41d4-a716-446655440000\n---\nbody\n";
        let g = parse_goal("Goals/x.md", "x", raw).unwrap();
        assert!(g.parent_id.is_some());
    }
}
