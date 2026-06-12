//! Recipe-fulfillment **wire types** — `Fulfillment` /
//! `Shortage` / `SubstitutionSuggestion` and their enums.
//!
//! These are the values [`crate::service::MealplanService::can_cook`]
//! returns, so they live in this wasm-clean proto. The pure
//! `check` / `check_with_subs` / `check_nested` matching logic
//! (which needs the native `pantry` unit-conversion layer) stays
//! in the native `mealplan::fulfillment` module and re-exports
//! these types.

use facet::Facet;
use pantry_proto::SubReason;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Fulfillment {
    pub can_cook: bool,
    pub missing: Vec<Shortage>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct Shortage {
    /// Recipe ingredient name (cooklang `@name`).
    pub name: String,

    #[serde(rename = "ingredientIdx")]
    pub ingredient_idx: u32,

    pub need: f64,
    pub have: f64,
    pub unit: String,
    pub reason: ShortageReason,

    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub suggestions: Vec<SubstitutionSuggestion>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct SubstitutionSuggestion {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none", default, rename = "toItemId")]
    pub to_item_id: Option<uuid::Uuid>,
    pub ratio: f64,
    pub need: f64,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub have: Option<f64>,
    #[serde(default)]
    pub reasons: Vec<SubReason>,
    pub source: SubstitutionSource,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub note: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum SubstitutionSource {
    PantryItem,
    Registry,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum ShortageReason {
    NotInPantry,
    InsufficientQty,
    UnitMismatch,
    OptionalNoQty,
}
