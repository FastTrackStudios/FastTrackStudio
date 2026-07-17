//! Markdown → [`Contact`].
//!
//! The structured fields live in the leading YAML frontmatter (emails /
//! phones / groups as sequences or a single scalar); the body holds the
//! free-form notes. Tolerant by design — a missing field falls back to
//! a sensible default so one malformed file never nukes the whole
//! directory (the scanner logs + skips truly unreadable files).

use contacts_proto::{Contact, ContactSource};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("invalid frontmatter: {0}")]
    Frontmatter(String),
}

/// Split a markdown file's leading YAML frontmatter from the body.
/// Returns `(frontmatter, body)`. Mirrors `recall::parse`.
#[must_use]
pub fn frontmatter_split(src: &str) -> Option<(&str, &str)> {
    let rest = src.strip_prefix("---\n")?;
    let end = rest.find("\n---\n")?;
    Some((&rest[..end], &rest[end + 5..]))
}

fn take_str(map: &serde_yaml::Mapping, key: &str) -> Option<String> {
    map.get(serde_yaml::Value::from(key))
        .and_then(serde_yaml::Value::as_str)
        .map(str::to_string)
        .filter(|s| !s.is_empty())
}

fn take_bool(map: &serde_yaml::Mapping, key: &str) -> bool {
    map.get(serde_yaml::Value::from(key))
        .and_then(serde_yaml::Value::as_bool)
        .unwrap_or(false)
}

/// Read a multi-value field that may be a YAML sequence *or* a single
/// scalar (or newline-joined scalar), returning it newline-joined —
/// the shape the [`Contact`] entity stores.
fn take_multi(map: &serde_yaml::Mapping, key: &str) -> String {
    match map.get(serde_yaml::Value::from(key)) {
        Some(serde_yaml::Value::Sequence(items)) => items
            .iter()
            .filter_map(serde_yaml::Value::as_str)
            .map(str::trim)
            .filter(|s| !s.is_empty())
            .collect::<Vec<_>>()
            .join("\n"),
        Some(serde_yaml::Value::String(s)) => s
            .lines()
            .map(str::trim)
            .filter(|l| !l.is_empty())
            .collect::<Vec<_>>()
            .join("\n"),
        _ => String::new(),
    }
}

/// Parse a contact. `path` supplies the fallback id (the file stem)
/// when frontmatter has none; `body` is the raw markdown after the
/// frontmatter fence (the free-form notes).
pub fn parse_contact(
    path: &str,
    frontmatter_yaml: &str,
    body: &str,
) -> Result<Contact, ParseError> {
    let map: serde_yaml::Mapping = serde_yaml::from_str(frontmatter_yaml)
        .map_err(|e| ParseError::Frontmatter(e.to_string()))?;

    let id = take_str(&map, "id").unwrap_or_else(|| {
        std::path::Path::new(path)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or(path)
            .to_string()
    });

    let notes = {
        let trimmed = body.trim();
        if trimmed.is_empty() {
            None
        } else {
            Some(trimmed.to_string())
        }
    };

    Ok(Contact {
        id,
        uid: take_str(&map, "uid"),
        full_name: take_str(&map, "full_name").unwrap_or_default(),
        given_name: take_str(&map, "given_name"),
        family_name: take_str(&map, "family_name"),
        organization: take_str(&map, "organization"),
        title: take_str(&map, "title"),
        emails: take_multi(&map, "emails"),
        phones: take_multi(&map, "phones"),
        address: take_str(&map, "address"),
        birthday: take_str(&map, "birthday"),
        photo_url: take_str(&map, "photo_url"),
        groups: take_multi(&map, "groups"),
        notes,
        source: take_str(&map, "source").unwrap_or_else(|| ContactSource::MANUAL.to_string()),
        account: take_str(&map, "account"),
        etag: take_str(&map, "etag"),
        linked_party_id: take_str(&map, "linked_party_id"),
        linked_user_id: take_str(&map, "linked_user_id"),
        archived: take_bool(&map, "archived"),
        created: take_str(&map, "created").unwrap_or_default(),
        updated: take_str(&map, "updated"),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_scalar_email_as_single_value() {
        let fm = "type: contact\nid: x\nfull_name: Ada\nemails: ada@example.com\n";
        let c = parse_contact("x.md", fm, "").unwrap();
        assert_eq!(c.email_list(), vec!["ada@example.com"]);
    }

    #[test]
    fn falls_back_to_file_stem_for_id() {
        let fm = "full_name: Ada\n";
        let c = parse_contact("Records/contacts/from-stem.md", fm, "").unwrap();
        assert_eq!(c.id, "from-stem");
    }

    #[test]
    fn body_becomes_notes() {
        let fm = "id: x\nfull_name: Ada\n";
        let c = parse_contact("x.md", fm, "\nSome notes here.\n").unwrap();
        assert_eq!(c.notes.as_deref(), Some("Some notes here."));
    }
}
