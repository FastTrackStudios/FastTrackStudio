//! `PantryItem` — typed view of one food-on-hand page.
//!
//! A pantry page IS an inventory page: same `type: item`
//! discriminator + same `id` so `inventory::looks_like_item`
//! returns true, and `pantry` in `tags:` so this crate can
//! pick the food rows out of the broader inventory list. One
//! physical thing = one markdown file visible in both lists;
//! no shadow rows, no parallel uuid.
//!
//! `PantryItem` *carries* the same identity + locator fields
//! as [`inventory::Item`] (so the YAML stays flat and the
//! inventory scanner sees them at the top level) and adds
//! food-specific fields. The two views aren't composed via
//! `#[serde(flatten)]` because Facet's wire schema would
//! diverge from the YAML — instead we duplicate the small set
//! of fields the inventory model exposes and provide
//! [`PantryItem::to_item`] / [`PantryItem::from_item`] for
//! conversion when callers need to hand a row to the
//! inventory crate.

use chrono::{DateTime, NaiveDate, Utc};
use cookbook::Nutrition;
use facet::Facet;
use inventory::Item;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct PantryItem {
    // ── Identity + locator (mirrors inventory::Item so a
    // pantry page round-trips through inventory's scanner) ──
    #[serde(skip)]
    pub path: String,

    pub id: Uuid,

    pub name: String,

    /// Free-form. Pantry pages set this to `"food"` by
    /// convention — finer-grained food taxonomy lives in
    /// [`Self::food_category`].
    #[serde(default = "default_category")]
    pub category: String,

    /// Where the item physically lives — `locations::Location`
    /// id. Pantry items typically point at a "Kitchen Pantry"
    /// or "Fridge" sub-location.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub location_id: Option<Uuid>,

    #[serde(default = "default_condition")]
    pub condition: String,

    #[serde(default = "default_status")]
    pub status: String,

    /// Inventory tags. Always contains `"pantry"` for pages
    /// owned by this crate; round-trip preserves the rest.
    #[serde(default)]
    pub tags: Vec<String>,

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

    // ── Food-specific fields (the reason this crate exists) ──
    /// Free-form food category. Canonical set in
    /// [`FoodCategory`].
    #[serde(default, rename = "foodCategory")]
    pub food_category: String,

    /// Current amount on hand. `None` for "I have some but
    /// haven't measured" — surface in the UI, don't guess.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub qty: Option<f64>,

    /// Free-form unit (`"g"`, `"ml"`, `"cup"`, `"each"`,
    /// `"clove"`, `"bunch"`). Mealplan deductions require the
    /// pantry `unit` and the recipe ingredient unit to match.
    #[serde(default)]
    pub unit: String,

    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub expiry: Option<NaiveDate>,

    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub opened: bool,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "openedDate"
    )]
    pub opened_date: Option<NaiveDate>,

    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub brand: Option<String>,

    /// Nutrition per 1 [`Self::nutrition_unit`] (defaults to
    /// `unit` when unset). Recipe scaling + (future) fitness
    /// calorie logs read this.
    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "nutritionPerUnit"
    )]
    pub nutrition_per_unit: Option<Nutrition>,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "nutritionUnit"
    )]
    pub nutrition_unit: Option<String>,

    /// Reorder threshold. When `qty <= minimum`, surface a
    /// restock task. Same units as `qty`.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub minimum: Option<f64>,

    #[serde(skip)]
    pub details: String,
}

fn default_category() -> String {
    "food".into()
}

fn default_condition() -> String {
    "good".into()
}

fn default_status() -> String {
    "stored".into()
}

impl PantryItem {
    /// Lossy down-conversion to the inventory model. Drops
    /// the food-specific fields; useful when handing a row
    /// off to the gear-inventory UI.
    pub fn to_item(&self) -> Item {
        Item {
            path: self.path.clone(),
            id: self.id,
            name: self.name.clone(),
            category: self.category.clone(),
            location_id: self.location_id,
            condition: self.condition.clone(),
            status: self.status.clone(),
            manufacturer: None,
            model: None,
            serial: None,
            purchase_date: None,
            value: None,
            tasks: Vec::new(),
            tags: self.tags.clone(),
            date_created: self.date_created,
            date_modified: self.date_modified,
            details: self.details.clone(),
        }
    }

    /// Lift an existing inventory row into a pantry shape.
    /// Food fields start empty; the caller fills them in
    /// before persisting via [`crate::Store::create`].
    pub fn from_item(item: Item) -> Self {
        let mut tags = item.tags.clone();
        if !tags.iter().any(|t| t == "pantry") {
            tags.push("pantry".to_string());
        }
        Self {
            path: item.path,
            id: item.id,
            name: item.name,
            category: if item.category.is_empty() {
                default_category()
            } else {
                item.category
            },
            location_id: item.location_id,
            condition: item.condition,
            status: item.status,
            tags,
            date_created: item.date_created,
            date_modified: item.date_modified,
            food_category: String::new(),
            qty: None,
            unit: String::new(),
            expiry: None,
            opened: false,
            opened_date: None,
            brand: None,
            nutrition_per_unit: None,
            nutrition_unit: None,
            minimum: None,
            details: item.details,
        }
    }

    pub fn is_expired(&self, today: NaiveDate) -> bool {
        self.expiry.is_some_and(|d| d < today)
    }

    pub fn is_low(&self) -> bool {
        match (self.qty, self.minimum) {
            (Some(q), Some(m)) => q <= m,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FoodCategory {
    Produce,
    Dairy,
    Grain,
    Protein,
    Spice,
    Condiment,
    Oil,
    Canned,
    Frozen,
    Baking,
    Snack,
    Drink,
    Other,
}

impl FoodCategory {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Produce => "produce",
            Self::Dairy => "dairy",
            Self::Grain => "grain",
            Self::Protein => "protein",
            Self::Spice => "spice",
            Self::Condiment => "condiment",
            Self::Oil => "oil",
            Self::Canned => "canned",
            Self::Frozen => "frozen",
            Self::Baking => "baking",
            Self::Snack => "snack",
            Self::Drink => "drink",
            Self::Other => "other",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "produce" | "veg" | "vegetable" | "fruit" => Some(Self::Produce),
            "dairy" => Some(Self::Dairy),
            "grain" | "carbs" | "pasta" | "rice" | "bread" => Some(Self::Grain),
            "protein" | "meat" | "fish" | "egg" | "eggs" => Some(Self::Protein),
            "spice" | "herb" | "seasoning" => Some(Self::Spice),
            "condiment" | "sauce" => Some(Self::Condiment),
            "oil" | "fat" => Some(Self::Oil),
            "canned" | "tinned" => Some(Self::Canned),
            "frozen" => Some(Self::Frozen),
            "baking" => Some(Self::Baking),
            "snack" => Some(Self::Snack),
            "drink" | "beverage" => Some(Self::Drink),
            "other" => Some(Self::Other),
            _ => None,
        }
    }
}
