//! File-backed [`CookbookService`] impl. Same shape as the
//! locations / inventory stores: `Arc<Mutex<vault::Vault>>`,
//! cheap to `Clone`, source of truth is markdown on disk.

use std::sync::{Arc, Mutex};

use uuid::Uuid;
use vault::Vault;

use crate::model::Recipe;
use crate::parse::{looks_like_recipe, parse_page};
use crate::scan::scan_vault;
use crate::service::{CookbookError, CookbookService};
use crate::write::{default_recipe_path, serialize_recipe};

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
    /// feature (locations / inventory / pantry / mealplan) so
    /// every surface sees one consistent snapshot.
    pub fn from_shared(inner: Arc<Mutex<Vault>>) -> Self {
        Self { inner }
    }

    pub fn shared(&self) -> Arc<Mutex<Vault>> {
        self.inner.clone()
    }
}

fn map_io(e: impl std::fmt::Display) -> CookbookError {
    CookbookError::Io(e.to_string())
}

fn find_idx(vault: &Vault, id: Uuid) -> Option<usize> {
    vault
        .pages
        .iter()
        .position(|p| looks_like_recipe(p) && parse_page(p).map(|r| r.id == id).unwrap_or(false))
}

impl CookbookService for Store {
    fn list(&self) -> Result<Vec<Recipe>, CookbookError> {
        let guard = self.inner.lock().expect("cookbook store poisoned");
        Ok(scan_vault(&guard))
    }

    fn get(&self, id: &str) -> Result<Recipe, CookbookError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| CookbookError::BadRequest(format!("id: {e}")))?;
        let guard = self.inner.lock().expect("cookbook store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_recipe(p)) {
            if let Ok(r) = parse_page(page) {
                if r.id == uuid {
                    return Ok(r);
                }
            }
        }
        Err(CookbookError::NotFound(id.to_string()))
    }

    fn create(&self, mut recipe: Recipe) -> Result<Recipe, CookbookError> {
        if recipe.id.is_nil() {
            recipe.id = Uuid::new_v4();
        }
        if recipe.path.is_empty() {
            recipe.path = default_recipe_path(&recipe.name, None);
        }
        let now = chrono::Utc::now();
        recipe.date_created.get_or_insert(now);
        recipe.date_modified = Some(now);
        let body = serialize_recipe(&recipe).map_err(map_io)?;
        let mut guard = self.inner.lock().expect("cookbook store poisoned");
        if guard.pages.iter().any(|p| p.rel_path == recipe.path) {
            return Err(CookbookError::AlreadyExists(recipe.path));
        }
        vault::create_page(&mut guard, &recipe.path, body).map_err(map_io)?;
        Ok(recipe)
    }

    fn update(&self, mut recipe: Recipe) -> Result<Recipe, CookbookError> {
        let mut guard = self.inner.lock().expect("cookbook store poisoned");
        let idx = find_idx(&guard, recipe.id)
            .ok_or_else(|| CookbookError::NotFound(recipe.id.to_string()))?;
        recipe.path = guard.pages[idx].rel_path.clone();
        recipe.date_modified = Some(chrono::Utc::now());
        let body = serialize_recipe(&recipe).map_err(map_io)?;
        guard.pages[idx].raw = body;
        let path = recipe.path.clone();
        vault::save_page(&mut guard, &path).map_err(map_io)?;
        Ok(recipe)
    }

    fn rename(&self, id: &str, new_path: &str) -> Result<Recipe, CookbookError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| CookbookError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("cookbook store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| CookbookError::NotFound(id.to_string()))?;
        if guard.pages.iter().any(|p| p.rel_path == new_path) {
            return Err(CookbookError::AlreadyExists(new_path.to_string()));
        }
        let old_path = guard.pages[idx].rel_path.clone();
        let raw = guard.pages[idx].raw.clone();
        vault::delete_page(&mut guard, &old_path).map_err(map_io)?;
        vault::create_page(&mut guard, new_path, raw).map_err(map_io)?;
        let new_page = guard
            .pages
            .iter()
            .find(|p| p.rel_path == new_path)
            .ok_or_else(|| CookbookError::Io("rename: page missing post-write".into()))?;
        parse_page(new_page).map_err(|e| CookbookError::Io(e.to_string()))
    }

    fn delete(&self, id: &str) -> Result<(), CookbookError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| CookbookError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("cookbook store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| CookbookError::NotFound(id.to_string()))?;
        let path = guard.pages[idx].rel_path.clone();
        vault::delete_page(&mut guard, &path).map_err(map_io)?;
        Ok(())
    }
}
