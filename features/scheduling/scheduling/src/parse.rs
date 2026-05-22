//! Markdown ↔ proto parse. v1 surface covers day-templates only;
//! event-type / schedule / booking parsers land alongside the
//! `VaultScheduler` impl in a follow-up.

use thiserror::Error;

use scheduling_proto::{
    BlockCategory, DayTemplate, DayTemplateId, TimeBlock, TimeBlockId, TimeOfDay,
};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("invalid frontmatter: {0}")]
    Frontmatter(String),
    #[error("invalid time string '{value}' (expected HH:MM)")]
    Time { value: String },
    #[error("missing required field: {field}")]
    MissingField { field: &'static str },
}

/// Parse the frontmatter side of a day-template markdown page.
/// The body is not consumed — callers can keep it as a free-form
/// notes field if they want.
pub fn parse_day_template(path: &str, frontmatter_yaml: &str) -> Result<DayTemplate, ParseError> {
    let value: serde_yaml::Value = serde_yaml::from_str(frontmatter_yaml)
        .map_err(|e| ParseError::Frontmatter(e.to_string()))?;
    let map = value
        .as_mapping()
        .ok_or_else(|| ParseError::Frontmatter("expected mapping".into()))?;

    let id = map
        .get(serde_yaml::Value::String("id".into()))
        .and_then(serde_yaml::Value::as_str)
        .map(|s| s.to_string())
        .unwrap_or_else(|| path.to_string());
    let name = map
        .get(serde_yaml::Value::String("name".into()))
        .and_then(serde_yaml::Value::as_str)
        .ok_or(ParseError::MissingField { field: "name" })?
        .to_string();
    let description = map
        .get(serde_yaml::Value::String("description".into()))
        .and_then(serde_yaml::Value::as_str)
        .map(str::to_string);

    let blocks_raw = map
        .get(serde_yaml::Value::String("blocks".into()))
        .and_then(serde_yaml::Value::as_sequence)
        .ok_or(ParseError::MissingField { field: "blocks" })?;

    let mut blocks = Vec::with_capacity(blocks_raw.len());
    for raw in blocks_raw {
        blocks.push(parse_block(raw)?);
    }

    Ok(DayTemplate {
        id: DayTemplateId(id),
        name,
        description,
        blocks,
    })
}

fn parse_block(raw: &serde_yaml::Value) -> Result<TimeBlock, ParseError> {
    let m = raw
        .as_mapping()
        .ok_or_else(|| ParseError::Frontmatter("block must be mapping".into()))?;
    let id = m
        .get(serde_yaml::Value::String("id".into()))
        .and_then(serde_yaml::Value::as_str)
        .map(str::to_string)
        .unwrap_or_else(|| uuid::Uuid::new_v4().to_string());
    let label = m
        .get(serde_yaml::Value::String("label".into()))
        .and_then(serde_yaml::Value::as_str)
        .ok_or(ParseError::MissingField { field: "label" })?
        .to_string();
    let start = m
        .get(serde_yaml::Value::String("start".into()))
        .and_then(serde_yaml::Value::as_str)
        .ok_or(ParseError::MissingField { field: "start" })?;
    let end = m
        .get(serde_yaml::Value::String("end".into()))
        .and_then(serde_yaml::Value::as_str)
        .ok_or(ParseError::MissingField { field: "end" })?;
    let category = m
        .get(serde_yaml::Value::String("category".into()))
        .and_then(serde_yaml::Value::as_str)
        .map(parse_category)
        .unwrap_or(BlockCategory::Other);
    let note = m
        .get(serde_yaml::Value::String("note".into()))
        .and_then(serde_yaml::Value::as_str)
        .map(str::to_string);
    Ok(TimeBlock {
        id: TimeBlockId(id),
        start: parse_time(start)?,
        end: parse_time(end)?,
        label,
        category,
        note,
    })
}

fn parse_time(value: &str) -> Result<TimeOfDay, ParseError> {
    let (h, m) = value.split_once(':').ok_or_else(|| ParseError::Time {
        value: value.into(),
    })?;
    let h: u8 = h.parse().map_err(|_| ParseError::Time {
        value: value.into(),
    })?;
    let m: u8 = m.parse().map_err(|_| ParseError::Time {
        value: value.into(),
    })?;
    if h >= 24 || m >= 60 {
        return Err(ParseError::Time {
            value: value.into(),
        });
    }
    Ok(TimeOfDay::new(h, m))
}

fn parse_category(value: &str) -> BlockCategory {
    match value.to_ascii_lowercase().as_str() {
        "reset" => BlockCategory::Reset,
        "spiritual" => BlockCategory::Spiritual,
        "meal" => BlockCategory::Meal,
        "exercise" | "gym" => BlockCategory::Exercise,
        "hygiene" => BlockCategory::Hygiene,
        "allocatable" | "block" => BlockCategory::Allocatable,
        "maintenance" => BlockCategory::Maintenance,
        "winddown" | "wind-down" => BlockCategory::WindDown,
        "sleep" => BlockCategory::Sleep,
        _ => BlockCategory::Other,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip_basic_day_template() {
        let yaml = r#"
type: scheduling-day-template
id: weekday
name: Weekday routine
description: Default Mon–Fri shape
blocks:
  - id: morning-reset
    label: Morning Reset
    start: "06:00"
    end: "06:30"
    category: reset
  - id: block-1
    label: "Block 1: Work / Event / Free Time"
    start: "09:30"
    end: "12:30"
    category: allocatable
    note: Deep work
"#;
        let dt = parse_day_template("templates/weekday.md", yaml).unwrap();
        assert_eq!(dt.name, "Weekday routine");
        assert_eq!(dt.blocks.len(), 2);
        assert_eq!(dt.blocks[0].start.minutes_since_midnight, 6 * 60);
        assert!(matches!(dt.blocks[1].category, BlockCategory::Allocatable));
    }

    #[test]
    fn rejects_bad_time() {
        let yaml = r#"
type: scheduling-day-template
name: bad
blocks:
  - label: x
    start: "25:00"
    end: "26:00"
"#;
        assert!(matches!(
            parse_day_template("p", yaml),
            Err(ParseError::Time { .. })
        ));
    }
}
