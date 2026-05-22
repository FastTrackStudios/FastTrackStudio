//! File-backed [`MealplanService`] impl. Holds a shared
//! `vault::Vault` mutex and a [`pantry::Store`] so
//! [`MealplanService::cook`] can deduct stock atomically
//! against the same vault snapshot.

use std::sync::{Arc, Mutex};

use cookbook::Store as CookbookStore;
use cookbook::{self, CookbookService};
use pantry::{PantryService, Store as PantryStore};
use uuid::Uuid;
use vault::Vault;

use crate::fulfillment::{self, Fulfillment};
use crate::model::{Meal, PantryDeduction};
use crate::parse::{looks_like_meal, parse_page};
use crate::scan::scan_vault;
use crate::service::{MealplanError, MealplanService};
use crate::write::{default_meal_path, serialize_meal};

/// File-backed mealplan store.
///
/// Must share its `vault::Vault` mutex with the pantry store
/// it holds — otherwise the deduct-on-cook flow would race
/// against pantry edits made through the other surface.
/// Construct via [`Store::new`] from a single `Vault` so the
/// shared mutex is set up correctly.
#[derive(Clone)]
pub struct Store {
    inner: Arc<Mutex<Vault>>,
    pantry: PantryStore,
    cookbook: CookbookStore,
}

impl Store {
    /// Build mealplan + pantry + cookbook stores around one
    /// shared vault mutex.
    pub fn new(vault: Vault) -> Self {
        let pantry = PantryStore::new(vault);
        let inner = pantry.shared();
        let cookbook = CookbookStore::from_shared(inner.clone());
        Self {
            inner,
            pantry,
            cookbook,
        }
    }

    /// Reuse a vault mutex already in use by another feature
    /// (cookbook / locations / inventory). Pairs with that
    /// feature's `Store::shared`.
    pub fn from_shared(inner: Arc<Mutex<Vault>>) -> Self {
        let pantry = PantryStore::from_shared(inner.clone());
        let cookbook = CookbookStore::from_shared(inner.clone());
        Self {
            inner,
            pantry,
            cookbook,
        }
    }

    pub fn shared(&self) -> Arc<Mutex<Vault>> {
        self.inner.clone()
    }

    /// Borrow the underlying pantry store — useful when a
    /// caller wants to drive both surfaces (e.g. show "what
    /// did we eat this week and what's left in the fridge").
    pub fn pantry(&self) -> &PantryStore {
        &self.pantry
    }

    pub fn cookbook(&self) -> &CookbookStore {
        &self.cookbook
    }
}

fn map_io(e: impl std::fmt::Display) -> MealplanError {
    MealplanError::Io(e.to_string())
}

fn find_idx(vault: &Vault, id: Uuid) -> Option<usize> {
    vault
        .pages
        .iter()
        .position(|p| looks_like_meal(p) && parse_page(p).map(|m| m.id == id).unwrap_or(false))
}

impl MealplanService for Store {
    fn list(&self) -> Result<Vec<Meal>, MealplanError> {
        let guard = self.inner.lock().expect("mealplan store poisoned");
        Ok(scan_vault(&guard))
    }

    fn get(&self, id: &str) -> Result<Meal, MealplanError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| MealplanError::BadRequest(format!("id: {e}")))?;
        let guard = self.inner.lock().expect("mealplan store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_meal(p)) {
            if let Ok(m) = parse_page(page) {
                if m.id == uuid {
                    return Ok(m);
                }
            }
        }
        Err(MealplanError::NotFound(id.to_string()))
    }

    fn create(&self, mut meal: Meal) -> Result<Meal, MealplanError> {
        if meal.id.is_nil() {
            meal.id = Uuid::new_v4();
        }
        if meal.path.is_empty() {
            meal.path = default_meal_path(meal.scheduled_for, &meal.slot, Some(&meal.name), None);
        }
        let now = chrono::Utc::now();
        meal.date_created.get_or_insert(now);
        meal.date_modified = Some(now);
        let body = serialize_meal(&meal).map_err(map_io)?;
        let mut guard = self.inner.lock().expect("mealplan store poisoned");
        if guard.pages.iter().any(|p| p.rel_path == meal.path) {
            return Err(MealplanError::AlreadyExists(meal.path));
        }
        vault::create_page(&mut guard, &meal.path, body).map_err(map_io)?;
        Ok(meal)
    }

    fn update(&self, mut meal: Meal) -> Result<Meal, MealplanError> {
        let mut guard = self.inner.lock().expect("mealplan store poisoned");
        let idx = find_idx(&guard, meal.id)
            .ok_or_else(|| MealplanError::NotFound(meal.id.to_string()))?;
        meal.path = guard.pages[idx].rel_path.clone();
        meal.date_modified = Some(chrono::Utc::now());
        let body = serialize_meal(&meal).map_err(map_io)?;
        guard.pages[idx].raw = body;
        let path = meal.path.clone();
        vault::save_page(&mut guard, &path).map_err(map_io)?;
        Ok(meal)
    }

    fn rename(&self, id: &str, new_path: &str) -> Result<Meal, MealplanError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| MealplanError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("mealplan store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| MealplanError::NotFound(id.to_string()))?;
        if guard.pages.iter().any(|p| p.rel_path == new_path) {
            return Err(MealplanError::AlreadyExists(new_path.to_string()));
        }
        let old_path = guard.pages[idx].rel_path.clone();
        let raw = guard.pages[idx].raw.clone();
        vault::delete_page(&mut guard, &old_path).map_err(map_io)?;
        vault::create_page(&mut guard, new_path, raw).map_err(map_io)?;
        let new_page = guard
            .pages
            .iter()
            .find(|p| p.rel_path == new_path)
            .ok_or_else(|| MealplanError::Io("rename: page missing post-write".into()))?;
        parse_page(new_page).map_err(|e| MealplanError::Io(e.to_string()))
    }

    fn delete(&self, id: &str) -> Result<(), MealplanError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| MealplanError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("mealplan store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| MealplanError::NotFound(id.to_string()))?;
        let path = guard.pages[idx].rel_path.clone();
        vault::delete_page(&mut guard, &path).map_err(map_io)?;
        Ok(())
    }

    fn cook(&self, id: &str, deductions: Vec<PantryDeduction>) -> Result<Meal, MealplanError> {
        // Deduct first, then stamp the meal. If a deduct
        // fails we surface the pantry error and leave the
        // meal unchanged — caller retries with a corrected
        // ingredient list.
        for row in &deductions {
            self.pantry
                .consume(&row.item_id.to_string(), row.qty)
                .map_err(|e| MealplanError::Pantry(e.to_string()))?;
        }
        let mut meal = self.get(id)?;
        meal.status = crate::model::Status::Cooked.as_str().to_string();
        meal.pantry_deductions = deductions;
        self.update(meal)
    }

    fn skip(&self, id: &str) -> Result<Meal, MealplanError> {
        let mut meal = self.get(id)?;
        meal.status = crate::model::Status::Skipped.as_str().to_string();
        self.update(meal)
    }

    fn can_cook(&self, recipe_id: &str, servings: u32) -> Result<Fulfillment, MealplanError> {
        let recipe = self
            .cookbook
            .get(recipe_id)
            .map_err(|e| MealplanError::NotFound(format!("recipe {recipe_id}: {e}")))?;
        let pantry_items = self
            .pantry
            .list()
            .map_err(|e| MealplanError::Pantry(e.to_string()))?;
        // Pull every recipe — nested-recipe resolution
        // needs them. Recipes are cheap to list (small N
        // in any realistic vault); revisit if this gets hot.
        let all_recipes = if recipe.nested_recipes.is_empty() {
            Vec::new()
        } else {
            self.cookbook
                .list()
                .map_err(|e| MealplanError::Pantry(format!("cookbook list: {e}")))?
        };
        Ok(if recipe.nested_recipes.is_empty() {
            fulfillment::check(&recipe, &pantry_items, servings)
        } else {
            fulfillment::check_nested(&recipe, &all_recipes, &pantry_items, servings)
        })
    }
}
