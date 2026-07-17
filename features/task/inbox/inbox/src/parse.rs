//! Markdown → [`InboxItem`].
//!
//! The structured fields live in the leading YAML frontmatter; the
//! markdown body *is* the verbatim capture. Tolerant by design — a
//! missing field falls back to a sensible default so one malformed
//! file never nukes the whole inbox list (the scanner logs + skips
//! truly unreadable files).

use inbox_proto::InboxItem;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("invalid frontmatter: {0}")]
    Frontmatter(String),
}

/// Split a markdown file's leading YAML frontmatter from the body.
/// Returns `(frontmatter, body)`. Mirrors `scheduling::scan`.
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
}

fn take_i64(map: &serde_yaml::Mapping, key: &str) -> Option<i64> {
    map.get(serde_yaml::Value::from(key))
        .and_then(serde_yaml::Value::as_i64)
}

/// Parse a captured inbox item. `path` supplies the fallback id
/// (the file stem) when frontmatter has none; `body` is the raw
/// markdown after the frontmatter fence.
pub fn parse_inbox_item(
    path: &str,
    frontmatter_yaml: &str,
    body: &str,
) -> Result<InboxItem, ParseError> {
    let map: serde_yaml::Mapping = serde_yaml::from_str(frontmatter_yaml)
        .map_err(|e| ParseError::Frontmatter(e.to_string()))?;

    let id = take_str(&map, "id").unwrap_or_else(|| {
        std::path::Path::new(path)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or(path)
            .to_string()
    });

    Ok(InboxItem {
        id,
        body: body.trim().to_string(),
        kind: take_str(&map, "kind").unwrap_or_else(|| InboxItem::KIND_FLEETING.to_string()),
        status: take_str(&map, "status").unwrap_or_else(|| InboxItem::STATUS_OPEN.to_string()),
        source: take_str(&map, "source").unwrap_or_default(),
        created: take_str(&map, "created").unwrap_or_default(),
        resurface_on: take_str(&map, "resurface_on"),
        processed_into: take_str(&map, "processed_into"),
        // Spaced-repetition state (obsidian-spaced-repetition SM-2).
        // Absent on notes captured before SR landed → sensible defaults:
        // BASE_EASE, never-reviewed interval 0, zero reviews.
        ease: take_i64(&map, "sr-ease").unwrap_or(inbox_proto::schedule::BASE_EASE),
        interval: take_i64(&map, "sr-interval").unwrap_or(0),
        reviews: take_i64(&map, "sr-reviews").unwrap_or(0),
    })
}
