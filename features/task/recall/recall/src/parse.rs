//! Markdown → [`RecallCard`].
//!
//! The structured fields live in the leading YAML frontmatter (FSRS
//! state under `sr-*` keys); the body holds the front + back prompt,
//! split by a `<!-- back -->` marker. Tolerant by design — a missing
//! field falls back to a sensible default so one malformed file never
//! nukes the whole deck (the scanner logs + skips truly unreadable
//! files).

use recall_proto::{CardType, RecallCard};
use thiserror::Error;

/// The line that separates the front from the back in the body.
pub const BACK_MARKER: &str = "<!-- back -->";

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("invalid frontmatter: {0}")]
    Frontmatter(String),
}

/// Split a markdown file's leading YAML frontmatter from the body.
/// Returns `(frontmatter, body)`. Mirrors `inbox::parse`.
#[must_use]
pub fn frontmatter_split(src: &str) -> Option<(&str, &str)> {
    let rest = src.strip_prefix("---\n")?;
    let end = rest.find("\n---\n")?;
    Some((&rest[..end], &rest[end + 5..]))
}

/// Split a card body into `(front, back)` around [`BACK_MARKER`]. A
/// body without the marker is treated as a front-only card.
#[must_use]
pub fn split_body(body: &str) -> (String, String) {
    let needle = format!("\n{BACK_MARKER}\n");
    match body.split_once(&needle) {
        Some((front, back)) => (front.trim().to_string(), back.trim().to_string()),
        None => (body.trim().to_string(), String::new()),
    }
}

fn take_str(map: &serde_yaml::Mapping, key: &str) -> Option<String> {
    map.get(serde_yaml::Value::from(key))
        .and_then(serde_yaml::Value::as_str)
        .map(str::to_string)
}

fn take_f64(map: &serde_yaml::Mapping, key: &str) -> f64 {
    map.get(serde_yaml::Value::from(key))
        .and_then(serde_yaml::Value::as_f64)
        .unwrap_or(0.0)
}

fn take_i64(map: &serde_yaml::Mapping, key: &str) -> i64 {
    map.get(serde_yaml::Value::from(key))
        .and_then(serde_yaml::Value::as_i64)
        .unwrap_or(0)
}

fn take_bool(map: &serde_yaml::Mapping, key: &str) -> bool {
    map.get(serde_yaml::Value::from(key))
        .and_then(serde_yaml::Value::as_bool)
        .unwrap_or(false)
}

/// Parse a learning card. `path` supplies the fallback id (the file
/// stem) when frontmatter has none; `body` is the raw markdown after
/// the frontmatter fence.
pub fn parse_recall_card(
    path: &str,
    frontmatter_yaml: &str,
    body: &str,
) -> Result<RecallCard, ParseError> {
    let map: serde_yaml::Mapping = serde_yaml::from_str(frontmatter_yaml)
        .map_err(|e| ParseError::Frontmatter(e.to_string()))?;

    let id = take_str(&map, "id").unwrap_or_else(|| {
        std::path::Path::new(path)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or(path)
            .to_string()
    });

    let (front, back) = split_body(body);

    Ok(RecallCard {
        id,
        project: take_str(&map, "project").unwrap_or_default(),
        card_type: take_str(&map, "card_type").unwrap_or_else(|| CardType::FREE.to_string()),
        front,
        back,
        source_note: take_str(&map, "source_note"),
        stability: take_f64(&map, "sr-stability"),
        difficulty: take_f64(&map, "sr-difficulty"),
        reps: take_i64(&map, "sr-reps"),
        lapses: take_i64(&map, "sr-lapses"),
        due: take_str(&map, "sr-due"),
        last_review: take_str(&map, "sr-last-review"),
        archived: take_bool(&map, "archived"),
        created: take_str(&map, "created").unwrap_or_default(),
    })
}
