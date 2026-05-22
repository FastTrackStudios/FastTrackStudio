//! `VaultPage` (or raw markdown) → [`ProjectInfo`].

use std::str::FromStr;

use thiserror::Error;
use uuid::Uuid;
use vault::VaultPage;

use crate::model::ProjectInfo;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("missing frontmatter")]
    NoFrontmatter,
    #[error("yaml: {0}")]
    Yaml(String),
    #[error("not a project (no `type: project` or `tags: [project]`)")]
    NotProject,
}

/// `true` if the page declares itself as a project. Two
/// shapes accepted:
///
/// - `type: project` in the frontmatter, or
/// - `tags: [..., project]` (case-insensitive on `project`).
#[must_use]
pub fn looks_like_project(page: &VaultPage) -> bool {
    let Some((fm, _)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let map: serde_yaml::Mapping = match serde_yaml::from_str(fm) {
        Ok(m) => m,
        Err(_) => return false,
    };
    if let Some(ty) = map.get("type").and_then(serde_yaml::Value::as_str) {
        if ty.eq_ignore_ascii_case("project") {
            return true;
        }
    }
    if let Some(tags) = map.get("tags").and_then(serde_yaml::Value::as_sequence) {
        if tags
            .iter()
            .filter_map(serde_yaml::Value::as_str)
            .any(|t| t.eq_ignore_ascii_case("project"))
        {
            return true;
        }
    }
    false
}

/// Parse a `VaultPage` into a `ProjectInfo`. The page must
/// carry frontmatter; missing optional fields default.
pub fn parse_page(page: &VaultPage) -> Result<ProjectInfo, ParseError> {
    parse_page_inner(&page.rel_path, &page.basename, &page.raw)
}

/// Parse raw markdown into a `ProjectInfo`. `rel_path` and
/// `basename` only feed defaults (basename fills `title`
/// when frontmatter omits it; `rel_path` becomes
/// `ProjectInfo::path`).
pub fn parse_str(rel_path: &str, basename: &str, raw: &str) -> Result<ProjectInfo, ParseError> {
    parse_page_inner(rel_path, basename, raw)
}

fn parse_page_inner(rel_path: &str, basename: &str, raw: &str) -> Result<ProjectInfo, ParseError> {
    let (fm, body) = split_frontmatter(raw).ok_or(ParseError::NoFrontmatter)?;
    let map: serde_yaml::Mapping =
        serde_yaml::from_str(fm).map_err(|e| ParseError::Yaml(e.to_string()))?;

    let title = take_str(&map, "title").unwrap_or_else(|| basename.to_string());
    let status = take_str(&map, "status").unwrap_or_else(|| "active".into());
    let priority = take_str(&map, "priority").unwrap_or_else(|| "normal".into());
    let lead = take_str(&map, "lead").unwrap_or_default();
    let tags = take_string_list(&map, "tags");
    let client_id = take_str(&map, "clientId").and_then(|s| Uuid::from_str(&s).ok());
    let billable_default = take_bool(&map, "billableDefault").unwrap_or(false);
    let currency = take_str(&map, "currency").unwrap_or_default();
    let default_rate_cents = take_i64(&map, "defaultRateCents").unwrap_or(0);
    let estimated_seconds = take_i64(&map, "estimatedSeconds").unwrap_or(0);
    let agent_profile = take_str(&map, "agentProfile").unwrap_or_default();
    let color = take_str(&map, "color").unwrap_or_default();
    let archived = take_bool(&map, "archived").unwrap_or(false);
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    // `id` is required for stable cross-feature references.
    // First-write callers (`scan_vault::ensure_id`) backfill
    // a new UUID when missing.
    let id = take_str(&map, "id")
        .and_then(|s| Uuid::from_str(&s).ok())
        .unwrap_or_else(Uuid::nil);

    Ok(ProjectInfo {
        path: rel_path.to_string(),
        id,
        title,
        status,
        priority,
        lead,
        tags,
        details: body.to_string(),
        client_id,
        billable_default,
        currency,
        default_rate_cents,
        estimated_seconds,
        agent_profile,
        color,
        archived,
        date_created,
        date_modified,
    })
}

// ── helpers (mirror task::parse) ─────────────────────────────

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
    map.get(key)
        .and_then(serde_yaml::Value::as_sequence)
        .map(|seq| {
            seq.iter()
                .filter_map(|v| v.as_str().map(str::to_string))
                .collect()
        })
        .unwrap_or_default()
}

fn take_bool(map: &serde_yaml::Mapping, key: &str) -> Option<bool> {
    map.get(key).and_then(serde_yaml::Value::as_bool)
}

fn take_i64(map: &serde_yaml::Mapping, key: &str) -> Option<i64> {
    map.get(key).and_then(serde_yaml::Value::as_i64)
}
