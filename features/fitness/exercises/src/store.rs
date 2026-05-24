//! File-backed [`ExercisesService`] implementation. Same
//! shape as the cookbook / pantry / locations stores.

use std::sync::{Arc, Mutex};

use uuid::Uuid;
use vault::Vault;

use crate::model::Exercise;
use crate::parse::{looks_like_exercise, parse_page};
use crate::scan::scan_vault;
use crate::service::{ExercisesError, ExercisesService};
use crate::write::{default_exercise_path, serialize_exercise};
use architect::HasDispatcher;
use architect::dispatch::TokioBlockingDispatcher;

#[derive(Clone)]
pub struct Store {
    inner: Arc<Mutex<Vault>>,
}

impl Store {
    pub fn new(vault: Vault) -> Self {
        Self {
            inner: Arc::new(Mutex::new(vault)),
        }
    }

    pub fn from_shared(inner: Arc<Mutex<Vault>>) -> Self {
        Self { inner }
    }

    pub fn shared(&self) -> Arc<Mutex<Vault>> {
        self.inner.clone()
    }
}

fn map_io(e: impl std::fmt::Display) -> ExercisesError {
    ExercisesError::Io(e.to_string())
}

fn find_idx(vault: &Vault, id: Uuid) -> Option<usize> {
    vault
        .pages
        .iter()
        .position(|p| looks_like_exercise(p) && parse_page(p).map(|e| e.id == id).unwrap_or(false))
}

impl HasDispatcher for Store {
    type Dispatcher = TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        TokioBlockingDispatcher
    }
}

impl ExercisesService for Store {
    fn list(&self) -> Result<Vec<Exercise>, ExercisesError> {
        let guard = self.inner.lock().expect("exercises store poisoned");
        Ok(scan_vault(&guard))
    }

    fn get(&self, id: &str) -> Result<Exercise, ExercisesError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| ExercisesError::BadRequest(format!("id: {e}")))?;
        let guard = self.inner.lock().expect("exercises store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_exercise(p)) {
            if let Ok(e) = parse_page(page) {
                if e.id == uuid {
                    return Ok(e);
                }
            }
        }
        Err(ExercisesError::NotFound(id.to_string()))
    }

    fn find_by_name(&self, name: &str) -> Result<Exercise, ExercisesError> {
        let needle = name.trim().to_ascii_lowercase();
        if needle.is_empty() {
            return Err(ExercisesError::BadRequest("empty name".into()));
        }
        let guard = self.inner.lock().expect("exercises store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_exercise(p)) {
            if let Ok(e) = parse_page(page) {
                if e.name.eq_ignore_ascii_case(&needle)
                    || e.aliases.iter().any(|a| a.eq_ignore_ascii_case(&needle))
                {
                    return Ok(e);
                }
            }
        }
        Err(ExercisesError::NotFound(format!("name: {name}")))
    }

    fn create(&self, mut ex: Exercise) -> Result<Exercise, ExercisesError> {
        if ex.id.is_nil() {
            ex.id = Uuid::new_v4();
        }
        if ex.path.is_empty() {
            ex.path = default_exercise_path(&ex.name, None);
        }
        let now = chrono::Utc::now();
        ex.date_created.get_or_insert(now);
        ex.date_modified = Some(now);
        let body = serialize_exercise(&ex).map_err(map_io)?;
        let mut guard = self.inner.lock().expect("exercises store poisoned");
        if guard.pages.iter().any(|p| p.rel_path == ex.path) {
            return Err(ExercisesError::AlreadyExists(ex.path));
        }
        vault::create_page(&mut guard, &ex.path, body).map_err(map_io)?;
        Ok(ex)
    }

    fn update(&self, mut ex: Exercise) -> Result<Exercise, ExercisesError> {
        let mut guard = self.inner.lock().expect("exercises store poisoned");
        let idx =
            find_idx(&guard, ex.id).ok_or_else(|| ExercisesError::NotFound(ex.id.to_string()))?;
        ex.path = guard.pages[idx].rel_path.clone();
        ex.date_modified = Some(chrono::Utc::now());
        let body = serialize_exercise(&ex).map_err(map_io)?;
        guard.pages[idx].raw = body;
        let path = ex.path.clone();
        vault::save_page(&mut guard, &path).map_err(map_io)?;
        Ok(ex)
    }

    fn rename(&self, id: &str, new_path: &str) -> Result<Exercise, ExercisesError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| ExercisesError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("exercises store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| ExercisesError::NotFound(id.to_string()))?;
        if guard.pages.iter().any(|p| p.rel_path == new_path) {
            return Err(ExercisesError::AlreadyExists(new_path.to_string()));
        }
        let old_path = guard.pages[idx].rel_path.clone();
        let raw = guard.pages[idx].raw.clone();
        vault::delete_page(&mut guard, &old_path).map_err(map_io)?;
        vault::create_page(&mut guard, new_path, raw).map_err(map_io)?;
        let new_page = guard
            .pages
            .iter()
            .find(|p| p.rel_path == new_path)
            .ok_or_else(|| ExercisesError::Io("rename: page missing post-write".into()))?;
        parse_page(new_page).map_err(|e| ExercisesError::Io(e.to_string()))
    }

    fn delete(&self, id: &str) -> Result<(), ExercisesError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| ExercisesError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("exercises store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| ExercisesError::NotFound(id.to_string()))?;
        let path = guard.pages[idx].rel_path.clone();
        vault::delete_page(&mut guard, &path).map_err(map_io)?;
        Ok(())
    }
}
