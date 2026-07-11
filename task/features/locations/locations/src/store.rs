//! File-backed [`LocationsService`] implementation. Wraps a
//! `vault::Vault` (the in-memory snapshot of pages on disk)
//! behind a mutex so concurrent `&self` calls serialize.
//!
//! Same shape as `vault::sync::Backend` — cheap to `Clone`,
//! internals are `Arc`'d. Each method re-scans the cached
//! vault snapshot (cheap) rather than maintaining a parallel
//! `HashMap<Uuid, Location>` index — the source of truth is
//! the markdown on disk.

use std::sync::{Arc, Mutex};

use uuid::Uuid;
use vault::Vault;

use crate::model::Location;
use crate::parse::{looks_like_location, parse_page};
use crate::scan::scan_vault;
use crate::service::{LocationsError, LocationsService};
use crate::write::{default_location_path, serialize_location};

/// File-backed locations store. One per vault.
#[derive(Clone, architect::HasDispatcher)]
pub struct Store {
    inner: Arc<Mutex<Vault>>,
}

impl Store {
    #[must_use]
    pub fn new(vault: Vault) -> Self {
        Self {
            inner: Arc::new(Mutex::new(vault)),
        }
    }

    /// Hand the inner `Vault` to a closure for direct
    /// inspection (read-only). Useful when other features
    /// (e.g. `inventory`) want to share the same snapshot.
    pub fn with_vault<R>(&self, f: impl FnOnce(&Vault) -> R) -> R {
        let guard = self.inner.lock().expect("locations store poisoned");
        f(&guard)
    }
}

fn map_io(e: impl std::fmt::Display) -> LocationsError {
    LocationsError::Io(e.to_string())
}

impl LocationsService for Store {
    fn list(&self) -> Result<Vec<Location>, LocationsError> {
        let guard = self.inner.lock().expect("locations store poisoned");
        Ok(scan_vault(&guard))
    }

    fn get(&self, id: &str) -> Result<Location, LocationsError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| LocationsError::BadRequest(format!("id: {e}")))?;
        let guard = self.inner.lock().expect("locations store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_location(p)) {
            if let Ok(loc) = parse_page(page) {
                if loc.id == uuid {
                    return Ok(loc);
                }
            }
        }
        Err(LocationsError::NotFound(id.to_string()))
    }

    fn create(&self, mut loc: Location) -> Result<Location, LocationsError> {
        if loc.id.is_nil() {
            loc.id = Uuid::new_v4();
        }
        if loc.path.is_empty() {
            loc.path = default_location_path(&loc.name, None);
        }
        let now = chrono::Utc::now();
        loc.date_created.get_or_insert(now);
        loc.date_modified = Some(now);
        let body = serialize_location(&loc).map_err(map_io)?;
        let mut guard = self.inner.lock().expect("locations store poisoned");
        if guard.pages.iter().any(|p| p.rel_path == loc.path) {
            return Err(LocationsError::AlreadyExists(loc.path));
        }
        vault::create_page(&mut guard, &loc.path, body).map_err(map_io)?;
        Ok(loc)
    }

    fn update(&self, mut loc: Location) -> Result<Location, LocationsError> {
        let mut guard = self.inner.lock().expect("locations store poisoned");
        let idx = guard
            .pages
            .iter()
            .position(|p| {
                looks_like_location(p) && parse_page(p).map(|l| l.id == loc.id).unwrap_or(false)
            })
            .ok_or_else(|| LocationsError::NotFound(loc.id.to_string()))?;
        loc.path = guard.pages[idx].rel_path.clone();
        loc.date_modified = Some(chrono::Utc::now());
        let body = serialize_location(&loc).map_err(map_io)?;
        guard.pages[idx].raw = body;
        let path = loc.path.clone();
        vault::save_page(&mut guard, &path).map_err(map_io)?;
        Ok(loc)
    }

    fn rename(&self, id: &str, new_path: &str) -> Result<Location, LocationsError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| LocationsError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("locations store poisoned");
        let idx = guard
            .pages
            .iter()
            .position(|p| {
                looks_like_location(p) && parse_page(p).map(|l| l.id == uuid).unwrap_or(false)
            })
            .ok_or_else(|| LocationsError::NotFound(id.to_string()))?;
        if guard.pages.iter().any(|p| p.rel_path == new_path) {
            return Err(LocationsError::AlreadyExists(new_path.to_string()));
        }
        let old_path = guard.pages[idx].rel_path.clone();
        let raw = guard.pages[idx].raw.clone();
        vault::delete_page(&mut guard, &old_path).map_err(map_io)?;
        vault::create_page(&mut guard, new_path, raw).map_err(map_io)?;
        let new_page = guard
            .pages
            .iter()
            .find(|p| p.rel_path == new_path)
            .ok_or_else(|| LocationsError::Io("rename: page missing post-write".into()))?;
        parse_page(new_page).map_err(|e| LocationsError::Io(e.to_string()))
    }

    fn delete(&self, id: &str) -> Result<(), LocationsError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| LocationsError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("locations store poisoned");
        let idx = guard
            .pages
            .iter()
            .position(|p| {
                looks_like_location(p) && parse_page(p).map(|l| l.id == uuid).unwrap_or(false)
            })
            .ok_or_else(|| LocationsError::NotFound(id.to_string()))?;
        let path = guard.pages[idx].rel_path.clone();
        vault::delete_page(&mut guard, &path).map_err(map_io)?;
        Ok(())
    }
}
