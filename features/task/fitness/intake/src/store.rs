//! File-backed [`IntakeService`] impl. Holds shared
//! `mealplan` stores so recipe / pantry nutrition can
//! resolve.

use std::sync::{Arc, Mutex};

use chrono::{NaiveDate, Utc};
use mealplan::MealNutrition;
use mealplan::cookbook::{CookbookService, Nutrition};
use mealplan::pantry::PantryService;
use uuid::Uuid;
use vault::Vault;

use crate::model::{IntakeEntry, IntakeLog, IntakeSource, scale_nutrition};
use crate::parse::{looks_like_intake, parse_page};
use crate::scan::scan_vault;
use crate::service::{IntakeError, IntakeService};
use crate::write::{default_intake_path, serialize_intake};

#[derive(Clone, architect::HasDispatcher)]
pub struct Store {
    inner: Arc<Mutex<Vault>>,
    cookbook: mealplan::cookbook::Store,
    pantry: mealplan::pantry::Store,
}

impl Store {
    pub fn new(vault: Vault) -> Self {
        let root = vault.root.clone();
        let pantry = mealplan::pantry::Store::new(vault);
        let inner = pantry.shared();
        let cookbook = mealplan::cookbook::Store::new(root);
        Self {
            inner,
            cookbook,
            pantry,
        }
    }

    pub fn from_shared(inner: Arc<Mutex<Vault>>) -> Self {
        let root = inner.lock().expect("shared vault poisoned").root.clone();
        let cookbook = mealplan::cookbook::Store::new(root);
        let pantry = mealplan::pantry::Store::from_shared(inner.clone());
        Self {
            inner,
            cookbook,
            pantry,
        }
    }

    pub fn shared(&self) -> Arc<Mutex<Vault>> {
        self.inner.clone()
    }
}

fn map_io(e: impl std::fmt::Display) -> IntakeError {
    IntakeError::Io(e.to_string())
}

fn find_idx(vault: &Vault, id: Uuid) -> Option<usize> {
    vault
        .pages
        .iter()
        .position(|p| looks_like_intake(p) && parse_page(p).map(|l| l.id == id).unwrap_or(false))
}

fn parse_date(s: &str) -> Result<NaiveDate, IntakeError> {
    s.parse()
        .map_err(|e| IntakeError::BadRequest(format!("date: {e}")))
}

fn slot_to_opt(slot: &str) -> Option<String> {
    if slot.trim().is_empty() {
        None
    } else {
        Some(slot.to_string())
    }
}

impl IntakeService for Store {
    fn list(&self) -> Result<Vec<IntakeLog>, IntakeError> {
        let guard = self.inner.lock().expect("intake store poisoned");
        Ok(scan_vault(&guard))
    }

    fn get(&self, id: &str) -> Result<IntakeLog, IntakeError> {
        let uuid = Uuid::parse_str(id).map_err(|e| IntakeError::BadRequest(format!("id: {e}")))?;
        let guard = self.inner.lock().expect("intake store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_intake(p)) {
            if let Ok(l) = parse_page(page) {
                if l.id == uuid {
                    return Ok(l);
                }
            }
        }
        Err(IntakeError::NotFound(id.to_string()))
    }

    fn for_day(&self, date: &str) -> Result<IntakeLog, IntakeError> {
        let day = parse_date(date)?;
        {
            let guard = self.inner.lock().expect("intake store poisoned");
            for page in guard.pages.iter().filter(|p| looks_like_intake(p)) {
                if let Ok(l) = parse_page(page) {
                    if l.date == day {
                        return Ok(l);
                    }
                }
            }
        }
        // No log for `date` — create one.
        let log = IntakeLog {
            path: String::new(),
            id: Uuid::nil(),
            name: format!("Intake {date}"),
            date: day,
            entries: crate::model::Entries::default(),
            target: crate::model::DailyTarget::default(),
            tags: crate::model::Tags::default(),
            date_created: None,
            date_modified: None,
            details: String::new(),
        };
        self.create(log)
    }

    fn create(&self, mut log: IntakeLog) -> Result<IntakeLog, IntakeError> {
        if log.id.is_nil() {
            log.id = Uuid::new_v4();
        }
        if log.path.is_empty() {
            log.path = default_intake_path(log.date, None);
        }
        let now = Utc::now();
        log.date_created.get_or_insert(now);
        log.date_modified = Some(now);
        let body = serialize_intake(&log).map_err(map_io)?;
        let mut guard = self.inner.lock().expect("intake store poisoned");
        if guard.pages.iter().any(|p| p.rel_path == log.path) {
            return Err(IntakeError::AlreadyExists(log.path));
        }
        vault::create_page(&mut guard, &log.path, body).map_err(map_io)?;
        Ok(log)
    }

    fn update(&self, mut log: IntakeLog) -> Result<IntakeLog, IntakeError> {
        let mut guard = self.inner.lock().expect("intake store poisoned");
        let idx =
            find_idx(&guard, log.id).ok_or_else(|| IntakeError::NotFound(log.id.to_string()))?;
        log.path = guard.pages[idx].rel_path.clone();
        log.date_modified = Some(Utc::now());
        let body = serialize_intake(&log).map_err(map_io)?;
        guard.pages[idx].raw = body;
        let path = log.path.clone();
        vault::save_page(&mut guard, &path).map_err(map_io)?;
        Ok(log)
    }

    fn delete(&self, id: &str) -> Result<(), IntakeError> {
        let uuid = Uuid::parse_str(id).map_err(|e| IntakeError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("intake store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| IntakeError::NotFound(id.to_string()))?;
        let path = guard.pages[idx].rel_path.clone();
        vault::delete_page(&mut guard, &path).map_err(map_io)?;
        Ok(())
    }

    fn log_recipe(
        &self,
        date: &str,
        recipe_path: &str,
        servings: f64,
        slot: &str,
    ) -> Result<IntakeLog, IntakeError> {
        if servings < 0.0 {
            return Err(IntakeError::BadRequest(
                "servings must be non-negative".into(),
            ));
        }
        let recipe = self
            .cookbook
            .get(recipe_path)
            .map_err(|e| IntakeError::Mealplan(format!("recipe lookup: {e}")))?;
        // Recipe nutrition is computed at intake time from
        // pantry per-unit data (the cooklang file carries no
        // nutrition). Build a synthetic 1-serving meal and
        // delegate; multiply by the caller's `servings`.
        let items = self
            .pantry
            .list()
            .map_err(|e| IntakeError::Mealplan(e.to_string()))?;
        let synthetic_meal = mealplan::Meal {
            path: String::new(),
            id: Uuid::nil(),
            name: recipe.name.clone(),
            scheduled_for: chrono::NaiveDate::from_ymd_opt(1970, 1, 1).unwrap(),
            slot: "snack".into(),
            servings: 1,
            recipe_paths: mealplan::model::StringList(vec![recipe_path.into()]),
            status: "planned".into(),
            pantry_deductions: mealplan::model::PantryDeductions::default(),
            tags: mealplan::model::StringList::default(),
            date_created: None,
            date_modified: None,
            details: String::new(),
        };
        let per_serving = synthetic_meal.nutrition_total(std::slice::from_ref(&recipe), &items);
        let nutrition = per_serving.as_ref().map(|n| scale_nutrition(n, servings));
        let entry = IntakeEntry {
            id: Uuid::new_v4(),
            source: IntakeSource::Recipe {
                path: recipe_path.into(),
            },
            name: recipe.name.clone(),
            qty: servings,
            unit: "serving".into(),
            time: None,
            slot: slot_to_opt(slot),
            nutrition,
            note: None,
        };
        self.log_entry(date, entry)
    }

    fn log_pantry(
        &self,
        date: &str,
        item_id: &str,
        qty: f64,
        slot: &str,
    ) -> Result<IntakeLog, IntakeError> {
        if qty < 0.0 {
            return Err(IntakeError::BadRequest("qty must be non-negative".into()));
        }
        let item = self
            .pantry
            .get(item_id)
            .map_err(|e| IntakeError::Mealplan(format!("pantry lookup: {e}")))?;
        let id = Uuid::parse_str(item_id)
            .map_err(|e| IntakeError::BadRequest(format!("item_id: {e}")))?;
        let nutrition = compute_pantry_nutrition(&item, qty);
        let unit = item.unit.clone();
        let entry = IntakeEntry {
            id: Uuid::new_v4(),
            source: IntakeSource::Pantry { id },
            name: item.name.clone(),
            qty,
            unit,
            time: None,
            slot: slot_to_opt(slot),
            nutrition,
            note: None,
        };
        self.log_entry(date, entry)
    }

    fn log_freeform(
        &self,
        date: &str,
        name: &str,
        nutrition: Nutrition,
        slot: &str,
    ) -> Result<IntakeLog, IntakeError> {
        let entry = IntakeEntry {
            id: Uuid::new_v4(),
            source: IntakeSource::Freeform,
            name: name.to_string(),
            qty: 1.0,
            unit: String::new(),
            time: None,
            slot: slot_to_opt(slot),
            nutrition: Some(nutrition),
            note: None,
        };
        self.log_entry(date, entry)
    }

    fn log_entry(&self, date: &str, mut entry: IntakeEntry) -> Result<IntakeLog, IntakeError> {
        if entry.id.is_nil() {
            entry.id = Uuid::new_v4();
        }
        let mut log = self.for_day(date)?;
        log.entries.push(entry);
        self.update(log)
    }
}

/// Pantry nutrition is per `nutrition_unit_qty` of
/// `nutrition_unit` (typically per 100g). Scaling needs:
///
///   scale = qty / nutrition_unit_qty
///
/// We don't know `nutrition_unit_qty` numerically — OFF
/// returns it as a string `"100g"`. Parse the leading
/// digits; default to `100.0` so per-100g labels (the
/// common case) Just Work.
fn compute_pantry_nutrition(item: &mealplan::pantry::PantryItem, qty: f64) -> Option<Nutrition> {
    let n = item.nutrition_per_unit.as_ref()?;
    let unit_qty = item
        .nutrition_unit
        .as_deref()
        .and_then(|s| {
            s.chars()
                .take_while(|c| c.is_ascii_digit() || *c == '.')
                .collect::<String>()
                .parse::<f64>()
                .ok()
        })
        .unwrap_or(100.0);
    let scale = if unit_qty > 0.0 { qty / unit_qty } else { qty };
    Some(scale_nutrition(n, scale))
}
