//! `vault::VaultPage` → `PantryItem`.
//!
//! Discriminator: a page is a pantry item when it carries
//! `type: item` AND `tags:` contains `pantry`. This keeps a
//! single physical thing visible in both lists — inventory's
//! scanner picks it up via `type: item`, ours filters down
//! to the food rows via the tag.

use thiserror::Error;
use uuid::Uuid;
use vault::VaultPage;

use crate::model::{PantryItem, SubReason, Substitution};
use cookbook::Nutrition;

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("page has no frontmatter")]
    NoFrontmatter,
    #[error("frontmatter is not a YAML mapping")]
    NotAMapping,
    #[error("frontmatter parse: {0}")]
    Yaml(String),
}

#[must_use]
pub fn looks_like_pantry_item(page: &VaultPage) -> bool {
    let Some((fm, _)) = split_frontmatter(&page.raw) else {
        return false;
    };
    let Ok(map) = serde_yaml::from_str::<serde_yaml::Mapping>(fm) else {
        return false;
    };
    // Must satisfy inventory's discriminator too — `type: item`
    // (or `item` in tags) — otherwise it's not an inventory
    // row at all.
    let is_inv = map.get("type").and_then(|v| v.as_str()) == Some("item")
        || map
            .get("tags")
            .and_then(|v| v.as_sequence())
            .is_some_and(|s| s.iter().any(|v| v.as_str() == Some("item")));
    if !is_inv {
        return false;
    }
    map.get("tags")
        .and_then(|v| v.as_sequence())
        .is_some_and(|s| s.iter().any(|v| v.as_str() == Some("pantry")))
}

pub fn parse_page(page: &VaultPage) -> Result<PantryItem, ParseError> {
    let (fm, body) = split_frontmatter(&page.raw).ok_or(ParseError::NoFrontmatter)?;
    let map: serde_yaml::Mapping =
        serde_yaml::from_str(fm).map_err(|e| ParseError::Yaml(e.to_string()))?;

    let id = take_str(&map, "id")
        .and_then(|s| Uuid::parse_str(&s).ok())
        .unwrap_or_else(|| Uuid::new_v5(&Uuid::NAMESPACE_URL, page.rel_path.as_bytes()));
    let name = take_str(&map, "name").unwrap_or_else(|| page.basename.clone());
    let category = take_str(&map, "category").unwrap_or_else(|| "food".into());
    let location_id = take_str(&map, "location_id").and_then(|s| Uuid::parse_str(&s).ok());
    let condition = take_str(&map, "condition").unwrap_or_else(|| "good".into());
    let status = take_str(&map, "status").unwrap_or_else(|| "stored".into());
    let tags = take_string_list(&map, "tags");
    let date_created = take_str(&map, "dateCreated").and_then(|s| s.parse().ok());
    let date_modified = take_str(&map, "dateModified").and_then(|s| s.parse().ok());

    let food_category = take_str(&map, "foodCategory").unwrap_or_default();
    let qty = map.get("qty").and_then(serde_yaml::Value::as_f64);
    let unit = take_str(&map, "unit").unwrap_or_default();
    let purchase_unit = take_str(&map, "purchaseUnit");
    let purchase_to_stock_factor = map
        .get("purchaseToStockFactor")
        .and_then(serde_yaml::Value::as_f64);
    let expiry = take_str(&map, "expiry").and_then(|s| s.parse().ok());
    let opened = map
        .get("opened")
        .and_then(serde_yaml::Value::as_bool)
        .unwrap_or(false);
    let opened_date = take_str(&map, "openedDate").and_then(|s| s.parse().ok());
    let brand = take_str(&map, "brand");
    let nutrition_per_unit = map
        .get("nutritionPerUnit")
        .and_then(|v| serde_yaml::from_value::<Nutrition>(v.clone()).ok());
    let nutrition_unit = take_str(&map, "nutritionUnit");
    let minimum = map.get("minimum").and_then(serde_yaml::Value::as_f64);
    let default_best_before_days = map
        .get("defaultBestBeforeDays")
        .and_then(serde_yaml::Value::as_u64)
        .and_then(|n| u32::try_from(n).ok());
    let default_best_before_days_after_open = map
        .get("defaultBestBeforeDaysAfterOpen")
        .and_then(serde_yaml::Value::as_u64)
        .and_then(|n| u32::try_from(n).ok());
    let default_best_before_days_after_freezing = map
        .get("defaultBestBeforeDaysAfterFreezing")
        .and_then(serde_yaml::Value::as_u64)
        .and_then(|n| u32::try_from(n).ok());
    let default_best_before_days_after_thawing = map
        .get("defaultBestBeforeDaysAfterThawing")
        .and_then(serde_yaml::Value::as_u64)
        .and_then(|n| u32::try_from(n).ok());
    let due_type = take_str(&map, "dueType").unwrap_or_else(|| "best-before".into());
    let substitutes = map
        .get("substitutes")
        .and_then(|v| v.as_sequence())
        .map(|seq| {
            seq.iter()
                .filter_map(|row| {
                    let m = row.as_mapping()?;
                    let item_id = m
                        .get("itemId")
                        .and_then(|v| v.as_str())
                        .and_then(|s| Uuid::parse_str(s).ok())?;
                    let ratio = m
                        .get("ratio")
                        .and_then(serde_yaml::Value::as_f64)
                        .unwrap_or(1.0);
                    let reasons = m
                        .get("reasons")
                        .and_then(|v| v.as_sequence())
                        .map(|s| {
                            s.iter()
                                .filter_map(|v| v.as_str())
                                .filter_map(SubReason::from_str)
                                .collect()
                        })
                        .unwrap_or_default();
                    let note = m
                        .get("note")
                        .and_then(|v| v.as_str())
                        .map(std::string::ToString::to_string);
                    Some(Substitution {
                        item_id,
                        ratio,
                        reasons,
                        note,
                    })
                })
                .collect()
        })
        .unwrap_or_default();
    let barcodes = take_string_list(&map, "barcodes");
    let image_url = take_str(&map, "imageUrl");
    let stock_entries = map
        .get("stockEntries")
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
                    let qty = m.get("qty").and_then(serde_yaml::Value::as_f64)?;
                    let purchased_date = m
                        .get("purchasedDate")
                        .and_then(|v| v.as_str())
                        .and_then(|s| s.parse().ok())?;
                    let best_before = m
                        .get("bestBefore")
                        .and_then(|v| v.as_str())
                        .and_then(|s| s.parse().ok());
                    let opened = m
                        .get("opened")
                        .and_then(serde_yaml::Value::as_bool)
                        .unwrap_or(false);
                    let opened_date = m
                        .get("openedDate")
                        .and_then(|v| v.as_str())
                        .and_then(|s| s.parse().ok());
                    let price = m.get("price").and_then(serde_yaml::Value::as_f64);
                    let location_id = m
                        .get("locationId")
                        .and_then(|v| v.as_str())
                        .and_then(|s| Uuid::parse_str(s).ok());
                    let note = m
                        .get("note")
                        .and_then(|v| v.as_str())
                        .map(std::string::ToString::to_string);
                    Some(crate::model::StockEntry {
                        id: entry_id,
                        qty,
                        purchased_date,
                        best_before,
                        opened,
                        opened_date,
                        price,
                        location_id,
                        note,
                    })
                })
                .collect()
        })
        .unwrap_or_default();

    Ok(PantryItem {
        path: page.rel_path.clone(),
        id,
        name,
        category,
        location_id,
        condition,
        status,
        tags,
        date_created,
        date_modified,
        food_category,
        qty,
        unit,
        purchase_unit,
        purchase_to_stock_factor,
        expiry,
        opened,
        opened_date,
        brand,
        nutrition_per_unit,
        nutrition_unit,
        minimum,
        default_best_before_days,
        default_best_before_days_after_open,
        default_best_before_days_after_freezing,
        default_best_before_days_after_thawing,
        due_type,
        substitutes,
        stock_entries,
        barcodes,
        image_url,
        details: body.to_string(),
    })
}

pub(crate) fn split_frontmatter(src: &str) -> Option<(&str, &str)> {
    let rest = src.strip_prefix("---\n")?;
    let end = rest.find("\n---\n")?;
    Some((&rest[..end], &rest[end + 5..]))
}

fn take_str(map: &serde_yaml::Mapping, key: &str) -> Option<String> {
    map.get(key).and_then(|v| match v {
        serde_yaml::Value::String(s) => Some(s.clone()),
        serde_yaml::Value::Number(n) => Some(n.to_string()),
        serde_yaml::Value::Bool(b) => Some(b.to_string()),
        _ => None,
    })
}

fn take_string_list(map: &serde_yaml::Mapping, key: &str) -> Vec<String> {
    let Some(v) = map.get(key) else {
        return Vec::new();
    };
    match v {
        serde_yaml::Value::Sequence(seq) => seq
            .iter()
            .filter_map(|item| item.as_str().map(std::string::ToString::to_string))
            .collect(),
        serde_yaml::Value::String(s) => vec![s.clone()],
        _ => Vec::new(),
    }
}
