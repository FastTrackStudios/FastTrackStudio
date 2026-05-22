//! File-backed [`PantryService`] impl.

use std::sync::{Arc, Mutex};

use uuid::Uuid;
use vault::Vault;

use crate::model::PantryItem;
use crate::parse::{looks_like_pantry_item, parse_page};
use crate::scan::scan_vault;
use crate::service::{BarcodeResolution, PantryError, PantryService};
use crate::write::{default_pantry_path, serialize_pantry_item};

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

fn map_io(e: impl std::fmt::Display) -> PantryError {
    PantryError::Io(e.to_string())
}

fn find_idx(vault: &Vault, id: Uuid) -> Option<usize> {
    vault.pages.iter().position(|p| {
        looks_like_pantry_item(p) && parse_page(p).map(|i| i.id == id).unwrap_or(false)
    })
}

impl PantryService for Store {
    fn list(&self) -> Result<Vec<PantryItem>, PantryError> {
        let guard = self.inner.lock().expect("pantry store poisoned");
        Ok(scan_vault(&guard))
    }

    fn get(&self, id: &str) -> Result<PantryItem, PantryError> {
        let uuid = Uuid::parse_str(id).map_err(|e| PantryError::BadRequest(format!("id: {e}")))?;
        let guard = self.inner.lock().expect("pantry store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_pantry_item(p)) {
            if let Ok(i) = parse_page(page) {
                if i.id == uuid {
                    return Ok(i);
                }
            }
        }
        Err(PantryError::NotFound(id.to_string()))
    }

    fn create(&self, mut item: PantryItem) -> Result<PantryItem, PantryError> {
        if item.id.is_nil() {
            item.id = Uuid::new_v4();
        }
        if item.path.is_empty() {
            item.path = default_pantry_path(&item.name, None);
        }
        if !item.tags.iter().any(|t| t == "pantry") {
            item.tags.push("pantry".into());
        }
        let now = chrono::Utc::now();
        item.date_created.get_or_insert(now);
        item.date_modified = Some(now);
        let body = serialize_pantry_item(&item).map_err(map_io)?;
        let mut guard = self.inner.lock().expect("pantry store poisoned");
        if guard.pages.iter().any(|p| p.rel_path == item.path) {
            return Err(PantryError::AlreadyExists(item.path));
        }
        vault::create_page(&mut guard, &item.path, body).map_err(map_io)?;
        Ok(item)
    }

    fn update(&self, mut item: PantryItem) -> Result<PantryItem, PantryError> {
        let mut guard = self.inner.lock().expect("pantry store poisoned");
        let idx =
            find_idx(&guard, item.id).ok_or_else(|| PantryError::NotFound(item.id.to_string()))?;
        item.path = guard.pages[idx].rel_path.clone();
        item.date_modified = Some(chrono::Utc::now());
        let body = serialize_pantry_item(&item).map_err(map_io)?;
        guard.pages[idx].raw = body;
        let path = item.path.clone();
        vault::save_page(&mut guard, &path).map_err(map_io)?;
        Ok(item)
    }

    fn rename(&self, id: &str, new_path: &str) -> Result<PantryItem, PantryError> {
        let uuid = Uuid::parse_str(id).map_err(|e| PantryError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("pantry store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| PantryError::NotFound(id.to_string()))?;
        if guard.pages.iter().any(|p| p.rel_path == new_path) {
            return Err(PantryError::AlreadyExists(new_path.to_string()));
        }
        let old_path = guard.pages[idx].rel_path.clone();
        let raw = guard.pages[idx].raw.clone();
        vault::delete_page(&mut guard, &old_path).map_err(map_io)?;
        vault::create_page(&mut guard, new_path, raw).map_err(map_io)?;
        let new_page = guard
            .pages
            .iter()
            .find(|p| p.rel_path == new_path)
            .ok_or_else(|| PantryError::Io("rename: page missing post-write".into()))?;
        parse_page(new_page).map_err(|e| PantryError::Io(e.to_string()))
    }

    fn delete(&self, id: &str) -> Result<(), PantryError> {
        let uuid = Uuid::parse_str(id).map_err(|e| PantryError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("pantry store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| PantryError::NotFound(id.to_string()))?;
        let path = guard.pages[idx].rel_path.clone();
        vault::delete_page(&mut guard, &path).map_err(map_io)?;
        Ok(())
    }

    fn consume(&self, id: &str, amount: f64) -> Result<PantryItem, PantryError> {
        if amount < 0.0 {
            return Err(PantryError::BadRequest(
                "consume amount must be non-negative".into(),
            ));
        }
        let mut item = self.get(id)?;
        let have = item.qty.unwrap_or(0.0);
        if amount > have {
            return Err(PantryError::InsufficientStock {
                have,
                need: amount,
                unit: item.unit.clone(),
            });
        }
        item.qty = Some(have - amount);
        // Auto-mark as opened on first consume — most pantry
        // items are packaged, so the first scoop is also the
        // moment the shelf-life clock starts ticking.
        if !item.opened {
            item.opened = true;
            item.opened_date
                .get_or_insert_with(|| chrono::Utc::now().date_naive());
        }
        self.update(item)
    }

    fn restock(&self, id: &str, amount: f64) -> Result<PantryItem, PantryError> {
        if amount < 0.0 {
            return Err(PantryError::BadRequest(
                "restock amount must be non-negative".into(),
            ));
        }
        let mut item = self.get(id)?;
        item.qty = Some(item.qty.unwrap_or(0.0) + amount);
        self.update(item)
    }

    fn find_by_barcode(&self, barcode: &str) -> Result<PantryItem, PantryError> {
        let needle = barcode.trim();
        if needle.is_empty() {
            return Err(PantryError::BadRequest("empty barcode".into()));
        }
        let guard = self.inner.lock().expect("pantry store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_pantry_item(p)) {
            if let Ok(i) = parse_page(page) {
                if i.barcodes.iter().any(|b| b == needle) {
                    return Ok(i);
                }
            }
        }
        Err(PantryError::NotFound(format!("barcode: {needle}")))
    }

    fn resolve_barcode(&self, barcode: &str) -> Result<BarcodeResolution, PantryError> {
        match self.find_by_barcode(barcode) {
            Ok(item) => Ok(BarcodeResolution::Local(item)),
            Err(PantryError::NotFound(_)) => match crate::lookup::lookup_external(barcode) {
                Ok(Some(draft)) => Ok(BarcodeResolution::Draft(draft)),
                Ok(None) => Ok(BarcodeResolution::NotFound),
                Err(e) => Err(PantryError::Lookup(e.to_string())),
            },
            Err(e) => Err(e),
        }
    }

    fn open(&self, id: &str) -> Result<PantryItem, PantryError> {
        let mut item = self.get(id)?;
        if item.opened {
            return Ok(item);
        }
        item.opened = true;
        item.opened_date = Some(chrono::Utc::now().date_naive());
        self.update(item)
    }
}
