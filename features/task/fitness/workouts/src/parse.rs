//! `vault::VaultPage` → `Routine` / `WorkoutSession`.
//! Routines: `type: routine`. Sessions: `type: workout`.

use thiserror::Error;
use uuid::Uuid;
use vault::VaultPage;

use crate::model::{LoggedSet, Routine, RoutineDay, RoutineSlot, WorkoutSession};

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

pub fn looks_like_routine(page: &VaultPage) -> bool {
    discriminator_is(page, "routine")
}

pub fn looks_like_session(page: &VaultPage) -> bool {
    discriminator_is(page, "workout")
}

fn discriminator_is(page: &VaultPage, kind: &str) -> bool {
    let Some((fm, _)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let Ok(map) = serde_yaml::from_str::<serde_yaml::Mapping>(fm) else {
        return false;
    };
    if map.get("type").and_then(|v| v.as_str()) == Some(kind) {
        return true;
    }
    if let Some(seq) = map.get("tags").and_then(|v| v.as_sequence()) {
        return seq.iter().any(|v| v.as_str() == Some(kind));
    }
    false
}

pub fn parse_routine(page: &VaultPage) -> Result<Routine, ParseError> {
    let (fm, body) = split_frontmatter(&page.raw).ok_or(ParseError::NoFrontmatter)?;
    let map: serde_yaml::Mapping =
        serde_yaml::from_str(fm).map_err(|e| ParseError::Yaml(e.to_string()))?;

    let id = take_str(&map, "id")
        .and_then(|s| Uuid::parse_str(&s).ok())
        .unwrap_or_else(|| Uuid::new_v5(&Uuid::NAMESPACE_URL, page.rel_path.as_bytes()));
    let name = take_str(&map, "name").unwrap_or_else(|| page.basename.clone());
    let description = take_str(&map, "description");
    let days = parse_days(&map);
    let tags = take_string_list(&map, "tags")
        .into_iter()
        .filter(|t| t != "routine")
        .collect();
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    Ok(Routine {
        path: page.rel_path.clone(),
        id,
        name,
        description,
        days: crate::model::RoutineDays(days),
        tags: crate::model::Tags(tags),
        date_created,
        date_modified,
        details: body.to_string(),
    })
}

pub fn parse_session(page: &VaultPage) -> Result<WorkoutSession, ParseError> {
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
    let routine_id = take_str(&map, "routineId").and_then(|s| Uuid::parse_str(&s).ok());
    let day_name = take_str(&map, "dayName");
    let logged_sets = parse_logged_sets(&map);
    let status = take_str(&map, "status").unwrap_or_else(|| "completed".into());
    let duration_minutes = map
        .get("durationMinutes")
        .and_then(|v| v.as_u64())
        .and_then(|n| u32::try_from(n).ok());
    let tags = take_string_list(&map, "tags")
        .into_iter()
        .filter(|t| t != "workout")
        .collect();
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    Ok(WorkoutSession {
        path: page.rel_path.clone(),
        id,
        name,
        date,
        routine_id,
        day_name,
        logged_sets: crate::model::LoggedSets(logged_sets),
        status,
        duration_minutes,
        tags: crate::model::Tags(tags),
        date_created,
        date_modified,
        details: body.to_string(),
    })
}

fn parse_days(map: &serde_yaml::Mapping) -> Vec<RoutineDay> {
    let Some(seq) = map.get("days").and_then(|v| v.as_sequence()) else {
        return Vec::new();
    };
    seq.iter()
        .filter_map(|row| {
            let m = row.as_mapping()?;
            let name = m.get("name").and_then(|v| v.as_str())?.to_string();
            let slots = m
                .get("slots")
                .and_then(|v| v.as_sequence())
                .map(|s| s.iter().filter_map(parse_slot).collect())
                .unwrap_or_default();
            let note = m
                .get("note")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            Some(RoutineDay { name, slots, note })
        })
        .collect()
}

fn parse_slot(v: &serde_yaml::Value) -> Option<RoutineSlot> {
    let m = v.as_mapping()?;
    let exercise_id = m
        .get("exerciseId")
        .and_then(|v| v.as_str())
        .and_then(|s| Uuid::parse_str(s).ok())?;
    let exercise_name = m
        .get("exerciseName")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    let sets = m
        .get("sets")
        .and_then(|v| v.as_u64())
        .and_then(|n| u32::try_from(n).ok());
    let reps = m.get("reps").and_then(|v| match v {
        serde_yaml::Value::String(s) => Some(s.clone()),
        serde_yaml::Value::Number(n) => Some(n.to_string()),
        _ => None,
    });
    let weight_kg = m.get("weightKg").and_then(|v| v.as_f64());
    let rir = m
        .get("rir")
        .and_then(|v| v.as_u64())
        .and_then(|n| u32::try_from(n).ok());
    let rest_seconds = m
        .get("restSeconds")
        .and_then(|v| v.as_u64())
        .and_then(|n| u32::try_from(n).ok());
    let note = m
        .get("note")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string());
    Some(RoutineSlot {
        exercise_id,
        exercise_name,
        sets,
        reps,
        weight_kg,
        rir,
        rest_seconds,
        note,
    })
}

fn parse_logged_sets(map: &serde_yaml::Mapping) -> Vec<LoggedSet> {
    let Some(seq) = map.get("loggedSets").and_then(|v| v.as_sequence()) else {
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
            let exercise_id = m
                .get("exerciseId")
                .and_then(|v| v.as_str())
                .and_then(|s| Uuid::parse_str(s).ok())?;
            let exercise_name = m
                .get("exerciseName")
                .and_then(|v| v.as_str())
                .unwrap_or("")
                .to_string();
            let order = m
                .get("order")
                .and_then(|v| v.as_u64())
                .and_then(|n| u32::try_from(n).ok())
                .unwrap_or(0);
            let reps = m
                .get("reps")
                .and_then(|v| v.as_u64())
                .and_then(|n| u32::try_from(n).ok())?;
            let weight_kg = m.get("weightKg").and_then(|v| v.as_f64()).unwrap_or(0.0);
            let rir = m
                .get("rir")
                .and_then(|v| v.as_u64())
                .and_then(|n| u32::try_from(n).ok());
            let rpe = m.get("rpe").and_then(|v| v.as_f64());
            let completed = m.get("completed").and_then(|v| v.as_bool()).unwrap_or(true);
            let note = m
                .get("note")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string());
            Some(LoggedSet {
                id,
                exercise_id,
                exercise_name,
                order,
                reps,
                weight_kg,
                rir,
                rpe,
                completed,
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
