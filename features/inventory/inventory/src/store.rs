//! File-backed [`InventoryService`] implementation.
//!
//! Same shape as the locations store: wraps a `vault::Vault`
//! behind a mutex; cheap to `Clone`. Each call re-scans the
//! cached vault rather than maintaining a parallel index —
//! the markdown on disk is the source of truth.

use std::sync::{Arc, Mutex};

use uuid::Uuid;
use vault::Vault;

use crate::model::Item;
use crate::parse::{looks_like_item, parse_page};
use crate::scan::scan_vault;
use crate::service::{InventoryError, InventoryService};
use crate::write::{default_item_path, serialize_item};

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

    /// Reuse a `vault::Vault` mutex already owned by another
    /// feature (e.g. the locations store) so both surfaces
    /// share one consistent snapshot.
    pub fn from_shared(inner: Arc<Mutex<Vault>>) -> Self {
        Self { inner }
    }

    /// Expose the inner mutex for callers that want to share
    /// it with another feature's store.
    pub fn shared(&self) -> Arc<Mutex<Vault>> {
        self.inner.clone()
    }
}

fn map_io(e: impl std::fmt::Display) -> InventoryError {
    InventoryError::Io(e.to_string())
}

fn find_idx(vault: &Vault, id: Uuid) -> Option<usize> {
    vault
        .pages
        .iter()
        .position(|p| looks_like_item(p) && parse_page(p).map(|i| i.id == id).unwrap_or(false))
}

impl InventoryService for Store {
    fn list(&self) -> Result<Vec<Item>, InventoryError> {
        let guard = self.inner.lock().expect("inventory store poisoned");
        Ok(scan_vault(&guard))
    }

    fn list_at(&self, location_id: &str) -> Result<Vec<Item>, InventoryError> {
        let uuid = Uuid::parse_str(location_id)
            .map_err(|e| InventoryError::BadRequest(format!("location_id: {e}")))?;
        let guard = self.inner.lock().expect("inventory store poisoned");
        Ok(crate::scan::items_at(&guard, uuid))
    }

    fn get(&self, id: &str) -> Result<Item, InventoryError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| InventoryError::BadRequest(format!("id: {e}")))?;
        let guard = self.inner.lock().expect("inventory store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_item(p)) {
            if let Ok(i) = parse_page(page) {
                if i.id == uuid {
                    return Ok(i);
                }
            }
        }
        Err(InventoryError::NotFound(id.to_string()))
    }

    fn create(&self, mut item: Item) -> Result<Item, InventoryError> {
        if item.id.is_nil() {
            item.id = Uuid::new_v4();
        }
        if item.path.is_empty() {
            item.path = default_item_path(&item.name, None);
        }
        let now = chrono::Utc::now();
        item.date_created.get_or_insert(now);
        item.date_modified = Some(now);
        let body = serialize_item(&item).map_err(map_io)?;
        let mut guard = self.inner.lock().expect("inventory store poisoned");
        if guard.pages.iter().any(|p| p.rel_path == item.path) {
            return Err(InventoryError::AlreadyExists(item.path));
        }
        vault::create_page(&mut guard, &item.path, body).map_err(map_io)?;
        Ok(item)
    }

    fn update(&self, mut item: Item) -> Result<Item, InventoryError> {
        let mut guard = self.inner.lock().expect("inventory store poisoned");
        let idx = find_idx(&guard, item.id)
            .ok_or_else(|| InventoryError::NotFound(item.id.to_string()))?;
        item.path = guard.pages[idx].rel_path.clone();
        item.date_modified = Some(chrono::Utc::now());
        let body = serialize_item(&item).map_err(map_io)?;
        guard.pages[idx].raw = body;
        let path = item.path.clone();
        vault::save_page(&mut guard, &path).map_err(map_io)?;
        Ok(item)
    }

    fn rename(&self, id: &str, new_path: &str) -> Result<Item, InventoryError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| InventoryError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("inventory store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| InventoryError::NotFound(id.to_string()))?;
        if guard.pages.iter().any(|p| p.rel_path == new_path) {
            return Err(InventoryError::AlreadyExists(new_path.to_string()));
        }
        let old_path = guard.pages[idx].rel_path.clone();
        let raw = guard.pages[idx].raw.clone();
        vault::delete_page(&mut guard, &old_path).map_err(map_io)?;
        vault::create_page(&mut guard, new_path, raw).map_err(map_io)?;
        let new_page = guard
            .pages
            .iter()
            .find(|p| p.rel_path == new_path)
            .ok_or_else(|| InventoryError::Io("rename: page missing post-write".into()))?;
        parse_page(new_page).map_err(|e| InventoryError::Io(e.to_string()))
    }

    fn delete(&self, id: &str) -> Result<(), InventoryError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| InventoryError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("inventory store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| InventoryError::NotFound(id.to_string()))?;
        let path = guard.pages[idx].rel_path.clone();
        vault::delete_page(&mut guard, &path).map_err(map_io)?;
        Ok(())
    }

    fn set_status(&self, id: &str, status: &str) -> Result<Item, InventoryError> {
        let mut item = self.get(id)?;
        item.status = status.to_string();
        self.update(item)
    }

    fn set_condition(&self, id: &str, condition: &str) -> Result<Item, InventoryError> {
        let mut item = self.get(id)?;
        item.condition = condition.to_string();
        self.update(item)
    }

    fn set_location(&self, id: &str, location_id: &str) -> Result<Item, InventoryError> {
        let mut item = self.get(id)?;
        item.location_id = if location_id.is_empty() {
            None
        } else {
            Some(
                Uuid::parse_str(location_id)
                    .map_err(|e| InventoryError::BadRequest(format!("location_id: {e}")))?,
            )
        };
        self.update(item)
    }
}
