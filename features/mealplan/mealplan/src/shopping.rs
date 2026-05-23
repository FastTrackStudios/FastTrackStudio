//! Shopping lists — modeled on grocy's `shopping_lists`
//! + `shopping_list` (multi-list with per-list entries).
//!
//! Each list is a markdown page (`type: shopping-list`)
//! under `<vault>/shopping/`. Entries reference pantry
//! items by `item_id` when known so "mark purchased" can
//! call `pantry::add_stock` automatically; free-text rows
//! (one-off purchases) are supported too.
//!
//! The auto-populate methods (`add_missing_for_meal`,
//! `add_low_stock`, `add_expired_or_overdue`) mirror
//! grocy's `/stock/shoppinglist/add-*` API endpoints.

use std::sync::{Arc, Mutex};

use chrono::{DateTime, NaiveDate, Utc};
use cookbook::CookbookService;
use facet::Facet;
use pantry::{PantryService, Store as PantryStore};
use serde::{Deserialize, Serialize};
use thiserror::Error;
use uuid::Uuid;
use vault::{Vault, VaultPage};

use crate::fulfillment::{Fulfillment, Shortage, ShortageReason};

// ── Model ────────────────────────────────────────────────────

/// `Vec<ShoppingEntry>` newtype — JSON column.
#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(architect::JsonField, Debug, Clone, Default, PartialEq, Facet, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ShoppingEntries(pub Vec<ShoppingEntry>);

impl ShoppingEntries {
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl From<Vec<ShoppingEntry>> for ShoppingEntries {
    fn from(v: Vec<ShoppingEntry>) -> Self {
        Self(v)
    }
}

impl std::ops::Deref for ShoppingEntries {
    type Target = Vec<ShoppingEntry>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for ShoppingEntries {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(architect::Entity, Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[architect(table_name = "shopping_lists", repo)]
pub struct ShoppingList {
    #[serde(skip)]
    #[architect(filterable, sortable)]
    pub path: String,

    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub name: String,

    /// Optional default store (a `locations::Location` of
    /// `kind: venue`). Lets the UI group lists by where
    /// you'll shop.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "storeLocationId"
    )]
    #[architect(filterable)]
    pub store_location_id: Option<Uuid>,

    #[serde(default)]
    #[architect(json)]
    pub entries: ShoppingEntries,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateCreated"
    )]
    pub date_created: Option<DateTime<Utc>>,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateModified"
    )]
    pub date_modified: Option<DateTime<Utc>>,

    #[serde(skip)]
    pub details: String,
}

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct ShoppingEntry {
    pub id: Uuid,

    /// Optional `pantry::PantryItem` id. When set,
    /// `mark_purchased` will call
    /// `pantry::PantryService::add_stock` against this id
    /// so the row lands in stock immediately.
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "itemId")]
    pub item_id: Option<Uuid>,

    /// Display name. Required even when `item_id` is set —
    /// the list reads independently of the pantry catalog.
    pub name: String,

    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub qty: Option<f64>,

    #[serde(default)]
    pub unit: String,

    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub note: Option<String>,

    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub purchased: bool,
}

// ── Errors + service ─────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet, Error)]
#[repr(u8)]
pub enum ShoppingError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("already exists: {0}")]
    AlreadyExists(String),
    #[error("bad request: {0}")]
    BadRequest(String),
    #[error("pantry: {0}")]
    Pantry(String),
    #[error("io: {0}")]
    Io(String),
}

#[architect::rpc]
pub trait ShoppingService {
    fn list(&self) -> Result<Vec<ShoppingList>, ShoppingError>;

    fn get(&self, id: &str) -> Result<ShoppingList, ShoppingError>;

    fn create(&self, list: ShoppingList) -> Result<ShoppingList, ShoppingError>;

    fn update(&self, list: ShoppingList) -> Result<ShoppingList, ShoppingError>;

    fn delete(&self, id: &str) -> Result<(), ShoppingError>;

    /// Add every shortage from `recipe`'s fulfillment to
    /// `list_id`. `recipe_path` is the vault-relative
    /// `.cook` file path.
    fn add_missing_for_recipe(
        &self,
        list_id: &str,
        recipe_path: &str,
        servings: u32,
    ) -> Result<ShoppingList, ShoppingError>;

    /// Add every pantry item at or below its `minimum`
    /// reorder threshold to `list_id`.
    fn add_low_stock(&self, list_id: &str) -> Result<ShoppingList, ShoppingError>;

    /// Add every pantry item with a stock entry already
    /// past its `best_before` as of `today`.
    fn add_expired_or_overdue(
        &self,
        list_id: &str,
        today: NaiveDate,
    ) -> Result<ShoppingList, ShoppingError>;

    /// Drop all entries from the list (keeps the list
    /// itself). Useful after a grocery run.
    fn clear(&self, id: &str) -> Result<ShoppingList, ShoppingError>;

    /// Mark `entry_id` as purchased. When the entry has an
    /// `item_id`, also calls
    /// `pantry::PantryService::add_stock` against that id
    /// (creates a single batch row using `entry.qty` and
    /// today's date as `purchased_date`).
    fn mark_purchased(&self, list_id: &str, entry_id: &str) -> Result<ShoppingList, ShoppingError>;
}

// ── Parse / write ────────────────────────────────────────────

fn split_frontmatter(src: &str) -> Option<(&str, &str)> {
    let rest = src.strip_prefix("---\n")?;
    let end = rest.find("\n---\n")?;
    Some((&rest[..end], &rest[end + 5..]))
}

#[must_use]
pub fn looks_like_shopping_list(page: &VaultPage) -> bool {
    let Some((fm, _)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let Ok(map) = serde_yaml::from_str::<serde_yaml::Mapping>(fm) else {
        return false;
    };
    map.get("type").and_then(|v| v.as_str()) == Some("shopping-list")
}

fn parse_page(page: &VaultPage) -> Option<ShoppingList> {
    let (fm, body) = split_frontmatter(&page.raw)?;
    let map: serde_yaml::Mapping = serde_yaml::from_str(fm).ok()?;
    let take_str = |k: &str| {
        map.get(k).and_then(|v| match v {
            serde_yaml::Value::String(s) => Some(s.clone()),
            serde_yaml::Value::Number(n) => Some(n.to_string()),
            _ => None,
        })
    };
    let id = take_str("id")
        .and_then(|s| Uuid::parse_str(&s).ok())
        .unwrap_or_else(|| Uuid::new_v5(&Uuid::NAMESPACE_URL, page.rel_path.as_bytes()));
    let name = take_str("name").unwrap_or_else(|| page.basename.clone());
    let store_location_id = take_str("storeLocationId").and_then(|s| Uuid::parse_str(&s).ok());
    let entries: Vec<ShoppingEntry> = map
        .get("entries")
        .and_then(|v| v.as_sequence())
        .map(|seq| {
            seq.iter()
                .filter_map(|row| {
                    let m = row.as_mapping()?;
                    let entry_id = m
                        .get("id")
                        .and_then(|v| v.as_str())
                        .and_then(|s| Uuid::parse_str(s).ok())
                        .unwrap_or_else(Uuid::new_v4);
                    let item_id = m
                        .get("itemId")
                        .and_then(|v| v.as_str())
                        .and_then(|s| Uuid::parse_str(s).ok());
                    let name = m.get("name").and_then(|v| v.as_str())?.to_string();
                    let qty = m.get("qty").and_then(serde_yaml::Value::as_f64);
                    let unit = m
                        .get("unit")
                        .and_then(|v| v.as_str())
                        .unwrap_or_default()
                        .to_string();
                    let note = m
                        .get("note")
                        .and_then(|v| v.as_str())
                        .map(std::string::ToString::to_string);
                    let purchased = m
                        .get("purchased")
                        .and_then(serde_yaml::Value::as_bool)
                        .unwrap_or(false);
                    Some(ShoppingEntry {
                        id: entry_id,
                        item_id,
                        name,
                        qty,
                        unit,
                        note,
                        purchased,
                    })
                })
                .collect()
        })
        .unwrap_or_default();
    let date_created = take_str("dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str("dateModified").and_then(|s| s.parse().ok());

    Some(ShoppingList {
        path: page.rel_path.clone(),
        id,
        name,
        store_location_id,
        entries: ShoppingEntries(entries),
        date_created,
        date_modified,
        details: body.to_string(),
    })
}

fn serialize(list: &ShoppingList) -> Result<String, ShoppingError> {
    let mut wrapper = serde_yaml::Mapping::new();
    wrapper.insert("type".into(), "shopping-list".into());
    let body = serde_yaml::to_value(list).map_err(|e| ShoppingError::Io(e.to_string()))?;
    if let serde_yaml::Value::Mapping(m) = body {
        for (k, v) in m {
            wrapper.insert(k, v);
        }
    }
    let yaml = serde_yaml::to_string(&serde_yaml::Value::Mapping(wrapper))
        .map_err(|e| ShoppingError::Io(e.to_string()))?;
    let details = if list.details.is_empty() {
        String::new()
    } else if list.details.starts_with('\n') {
        list.details.clone()
    } else {
        format!("\n{}", list.details)
    };
    Ok(format!("---\n{yaml}---\n{details}"))
}

fn default_path(name: &str) -> String {
    let mut slug = String::new();
    let mut prev_dash = false;
    for ch in name.chars() {
        if ch.is_alphanumeric() {
            for lc in ch.to_lowercase() {
                slug.push(lc);
            }
            prev_dash = false;
        } else if !prev_dash && !slug.is_empty() {
            slug.push('-');
            prev_dash = true;
        }
    }
    while slug.ends_with('-') {
        slug.pop();
    }
    if slug.is_empty() {
        slug.push_str("shopping-list");
    }
    format!("shopping/{slug}.md")
}

// ── Store ────────────────────────────────────────────────────

/// File-backed [`ShoppingService`] impl. Carries a
/// `pantry::Store` so auto-populate (low-stock,
/// expired/overdue) and `mark_purchased` can drive the
/// pantry directly. The mealplan store is *not* held —
/// `add_missing_for_recipe` accepts a recipe id + servings
/// and runs fulfillment itself.
#[derive(Clone)]
pub struct Store {
    inner: Arc<Mutex<Vault>>,
    pantry: PantryStore,
    cookbook: cookbook::Store,
}

impl Store {
    #[must_use]
    pub fn new(vault: Vault) -> Self {
        let root = vault.root.clone();
        let pantry = PantryStore::new(vault);
        let inner = pantry.shared();
        let cookbook = cookbook::Store::new(root);
        Self {
            inner,
            pantry,
            cookbook,
        }
    }

    pub fn from_shared(inner: Arc<Mutex<Vault>>) -> Self {
        let root = inner.lock().expect("shared vault poisoned").root.clone();
        let pantry = PantryStore::from_shared(inner.clone());
        let cookbook = cookbook::Store::new(root);
        Self {
            inner,
            pantry,
            cookbook,
        }
    }

    #[must_use]
    pub fn shared(&self) -> Arc<Mutex<Vault>> {
        self.inner.clone()
    }

    #[must_use]
    pub fn pantry(&self) -> &PantryStore {
        &self.pantry
    }
}

fn map_io(e: impl std::fmt::Display) -> ShoppingError {
    ShoppingError::Io(e.to_string())
}

fn find_idx(vault: &Vault, id: Uuid) -> Option<usize> {
    vault
        .pages
        .iter()
        .position(|p| looks_like_shopping_list(p) && parse_page(p).is_some_and(|l| l.id == id))
}

fn push_or_merge(list: &mut ShoppingList, entry: ShoppingEntry) {
    // Merge same (item_id, unit) rows by summing qty —
    // mirrors grocy's "add to existing if present".
    if let Some(existing) = list.entries.iter_mut().find(|e| {
        !e.purchased
            && e.unit.eq_ignore_ascii_case(&entry.unit)
            && match (e.item_id, entry.item_id) {
                (Some(a), Some(b)) => a == b,
                (None, None) => e.name.eq_ignore_ascii_case(&entry.name),
                _ => false,
            }
    }) {
        let base = existing.qty.unwrap_or(0.0);
        let add = entry.qty.unwrap_or(0.0);
        if base > 0.0 || add > 0.0 {
            existing.qty = Some(base + add);
        }
        return;
    }
    list.entries.push(entry);
}

impl ShoppingService for Store {
    fn list(&self) -> Result<Vec<ShoppingList>, ShoppingError> {
        let guard = self.inner.lock().expect("shopping store poisoned");
        Ok(guard
            .pages
            .iter()
            .filter(|p| looks_like_shopping_list(p))
            .filter_map(parse_page)
            .collect())
    }

    fn get(&self, id: &str) -> Result<ShoppingList, ShoppingError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| ShoppingError::BadRequest(format!("id: {e}")))?;
        let guard = self.inner.lock().expect("shopping store poisoned");
        for page in guard.pages.iter().filter(|p| looks_like_shopping_list(p)) {
            if let Some(l) = parse_page(page) {
                if l.id == uuid {
                    return Ok(l);
                }
            }
        }
        Err(ShoppingError::NotFound(id.to_string()))
    }

    fn create(&self, mut list: ShoppingList) -> Result<ShoppingList, ShoppingError> {
        if list.id.is_nil() {
            list.id = Uuid::new_v4();
        }
        if list.path.is_empty() {
            list.path = default_path(&list.name);
        }
        let now = Utc::now();
        list.date_created.get_or_insert(now);
        list.date_modified = Some(now);
        let body = serialize(&list)?;
        let mut guard = self.inner.lock().expect("shopping store poisoned");
        if guard.pages.iter().any(|p| p.rel_path == list.path) {
            return Err(ShoppingError::AlreadyExists(list.path));
        }
        vault::create_page(&mut guard, &list.path, body).map_err(map_io)?;
        Ok(list)
    }

    fn update(&self, mut list: ShoppingList) -> Result<ShoppingList, ShoppingError> {
        let mut guard = self.inner.lock().expect("shopping store poisoned");
        let idx = find_idx(&guard, list.id)
            .ok_or_else(|| ShoppingError::NotFound(list.id.to_string()))?;
        list.path = guard.pages[idx].rel_path.clone();
        list.date_modified = Some(Utc::now());
        let body = serialize(&list)?;
        guard.pages[idx].raw = body;
        let path = list.path.clone();
        vault::save_page(&mut guard, &path).map_err(map_io)?;
        Ok(list)
    }

    fn delete(&self, id: &str) -> Result<(), ShoppingError> {
        let uuid =
            Uuid::parse_str(id).map_err(|e| ShoppingError::BadRequest(format!("id: {e}")))?;
        let mut guard = self.inner.lock().expect("shopping store poisoned");
        let idx = find_idx(&guard, uuid).ok_or_else(|| ShoppingError::NotFound(id.to_string()))?;
        let path = guard.pages[idx].rel_path.clone();
        vault::delete_page(&mut guard, &path).map_err(map_io)?;
        Ok(())
    }

    fn add_missing_for_recipe(
        &self,
        list_id: &str,
        recipe_path: &str,
        servings: u32,
    ) -> Result<ShoppingList, ShoppingError> {
        let recipe = self
            .cookbook
            .get(recipe_path)
            .map_err(|e| ShoppingError::NotFound(format!("recipe {recipe_path}: {e}")))?;
        let pantry_items = self
            .pantry
            .list()
            .map_err(|e| ShoppingError::Pantry(e.to_string()))?;
        let f: Fulfillment = if recipe.nested_recipes.is_empty() {
            crate::fulfillment::check(&recipe, &pantry_items, servings)
        } else {
            let all = self
                .cookbook
                .list()
                .map_err(|e| ShoppingError::Pantry(format!("cookbook list: {e}")))?;
            crate::fulfillment::check_nested(&recipe, &all, &pantry_items, servings)
        };

        let mut list = self.get(list_id)?;
        for short in f.missing.into_iter().filter(|s| {
            matches!(
                s.reason,
                ShortageReason::NotInPantry | ShortageReason::InsufficientQty
            )
        }) {
            push_or_merge(&mut list, shortage_to_entry(short));
        }
        self.update(list)
    }

    fn add_low_stock(&self, list_id: &str) -> Result<ShoppingList, ShoppingError> {
        let mut list = self.get(list_id)?;
        let items = self
            .pantry
            .list()
            .map_err(|e| ShoppingError::Pantry(e.to_string()))?;
        for item in items.into_iter().filter(pantry::PantryItem::is_low) {
            let need = item
                .minimum
                .zip(item.stock_total())
                .map_or(0.0, |(min, have)| (min - have).max(0.0));
            push_or_merge(
                &mut list,
                ShoppingEntry {
                    id: Uuid::new_v4(),
                    item_id: Some(item.id),
                    name: item.name.clone(),
                    qty: if need > 0.0 { Some(need) } else { None },
                    unit: item.unit.clone(),
                    note: Some("low stock".into()),
                    purchased: false,
                },
            );
        }
        self.update(list)
    }

    fn add_expired_or_overdue(
        &self,
        list_id: &str,
        today: NaiveDate,
    ) -> Result<ShoppingList, ShoppingError> {
        let mut list = self.get(list_id)?;
        let items = self
            .pantry
            .list()
            .map_err(|e| ShoppingError::Pantry(e.to_string()))?;
        for item in items {
            if item.stock_entries.iter().any(|e| e.is_expired(today)) || item.is_expired(today) {
                push_or_merge(
                    &mut list,
                    ShoppingEntry {
                        id: Uuid::new_v4(),
                        item_id: Some(item.id),
                        name: item.name.clone(),
                        qty: None,
                        unit: item.unit.clone(),
                        note: Some("replace — expired".into()),
                        purchased: false,
                    },
                );
            }
        }
        self.update(list)
    }

    fn clear(&self, id: &str) -> Result<ShoppingList, ShoppingError> {
        let mut list = self.get(id)?;
        list.entries.clear();
        self.update(list)
    }

    fn mark_purchased(&self, list_id: &str, entry_id: &str) -> Result<ShoppingList, ShoppingError> {
        let entry_uuid = Uuid::parse_str(entry_id)
            .map_err(|e| ShoppingError::BadRequest(format!("entry_id: {e}")))?;
        let mut list = self.get(list_id)?;
        let entry = list
            .entries
            .iter_mut()
            .find(|e| e.id == entry_uuid)
            .ok_or_else(|| ShoppingError::NotFound(format!("entry: {entry_id}")))?;
        entry.purchased = true;
        let pantry_item_id = entry.item_id;
        let qty = entry.qty;
        let unit = entry.unit.clone();

        // Optional pantry-add. Only fire when the entry was
        // linked to a known pantry item and the qty is
        // explicit — free-text rows without qty just flip
        // purchased + leave pantry alone.
        if let (Some(item_id), Some(q)) = (pantry_item_id, qty) {
            self.pantry
                .add_stock(
                    &item_id.to_string(),
                    pantry::StockEntry {
                        id: Uuid::new_v4(),
                        qty: q,
                        purchased_date: Utc::now().date_naive(),
                        best_before: None,
                        opened: false,
                        opened_date: None,
                        price: None,
                        location_id: None,
                        note: if unit.is_empty() {
                            None
                        } else {
                            Some(format!("shopping list ({unit})"))
                        },
                    },
                )
                .map_err(|e| ShoppingError::Pantry(e.to_string()))?;
        }
        self.update(list)
    }
}

fn shortage_to_entry(short: Shortage) -> ShoppingEntry {
    ShoppingEntry {
        id: Uuid::new_v4(),
        item_id: None,
        name: short.name,
        qty: Some((short.need - short.have).max(0.0)),
        unit: short.unit,
        note: Some(match short.reason {
            ShortageReason::NotInPantry => "missing — not in pantry".into(),
            ShortageReason::InsufficientQty => "low — top up".into(),
            ShortageReason::UnitMismatch => "unit mismatch — check recipe".into(),
            ShortageReason::OptionalNoQty => "optional — qty TBD".into(),
        }),
        purchased: false,
    }
}
