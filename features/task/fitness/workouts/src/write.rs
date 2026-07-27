//! Serializers + path helpers for routines + sessions.
//!
//! Serialization lives in [`crate::entity`]; this module keeps the
//! historical `workouts::write::*` paths working and adds the one
//! thing the shared store doesn't cover — writing a page straight to a
//! vault root on disk, without an in-memory `Vault`.

use std::path::{Path, PathBuf};

use chrono::Utc;
use vault_entity::store::VaultEntity;

pub use vault_entity::WriteError;

use crate::entity::{Routines, Sessions};
use crate::model::{Routine, WorkoutSession};

/// Render a routine as a full markdown page.
pub fn serialize_routine(r: &Routine) -> Result<String, WriteError> {
    Routines::to_markdown(r)
}

/// Render a session as a full markdown page.
pub fn serialize_session(s: &WorkoutSession) -> Result<String, WriteError> {
    Sessions::to_markdown(s)
}

pub fn write_routine(
    vault_root: &Path,
    r: &mut Routine,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    let body = serialize_routine(r)?;
    write_page(
        vault_root,
        &mut r.path,
        &mut r.date_created,
        &mut r.date_modified,
        overwrite,
        &body,
    )
}

pub fn write_session(
    vault_root: &Path,
    s: &mut WorkoutSession,
    overwrite: bool,
) -> Result<PathBuf, WriteError> {
    let body = serialize_session(s)?;
    write_page(
        vault_root,
        &mut s.path,
        &mut s.date_created,
        &mut s.date_modified,
        overwrite,
        &body,
    )
}

fn write_page(
    vault_root: &Path,
    path: &mut String,
    created: &mut Option<chrono::DateTime<chrono::Utc>>,
    modified: &mut Option<chrono::DateTime<chrono::Utc>>,
    overwrite: bool,
    body: &str,
) -> Result<PathBuf, WriteError> {
    if path.is_empty() {
        return Err(WriteError::BadPath("path is empty".into()));
    }
    let abs = vault_root.join(&*path);
    if !overwrite && abs.exists() {
        return Err(WriteError::Exists(abs.display().to_string()));
    }
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).map_err(|e| WriteError::Io(e.to_string()))?;
    }
    let now = Utc::now();
    if created.is_none() {
        *created = Some(now);
    }
    *modified = Some(now);
    std::fs::write(&abs, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(abs)
}

/// Default layout: `routines/<slug>.md`.
pub fn default_routine_path(name: &str, folder: Option<&str>) -> String {
    Routines::default_path(name, folder)
}

/// Default layout: `workouts/<YYYY-MM-DD>-<slug>.md`. Date
/// goes first so directory listings sort chronologically.
///
/// The date prefix means this can't go through
/// [`VaultEntity::default_path`] — [`crate::store::Store`] applies it
/// before handing the session to the shared store.
pub fn default_session_path(date: chrono::NaiveDate, name: &str, folder: Option<&str>) -> String {
    let slug = vault_entity::slugify(name, Sessions::SLUG_FALLBACK);
    let date_str = date.format("%Y-%m-%d");
    let dir = folder
        .unwrap_or(Sessions::DEFAULT_FOLDER)
        .trim_end_matches('/');
    format!("{dir}/{date_str}-{slug}.md")
}
