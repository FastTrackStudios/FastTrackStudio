//! Open Food Facts product-lookup client.
//!
//! Open Food Facts (and its sibling databases Open Beauty Facts / Open
//! Pet Food Facts) expose a free, no-auth `GET /api/v2/product/<barcode>`
//! endpoint that returns either a populated `product` object or
//! `{ "status": 0, "product": null }` when the barcode is unknown.
//!
//! The HTTP status is `200` either way, so callers must inspect the
//! `status` field to distinguish hits from misses. We map a hit to
//! [`OpenFoodFactsProduct`] and a miss to `Ok(None)`.
//!
//! # API etiquette
//!
//! * Always identify the caller with a descriptive `User-Agent`
//!   (`name/version (+url-or-contact)`).
//! * Cache aggressively. The server tolerates more than one call per
//!   real-world scan but we keep the rate down by writing each
//!   successful lookup into the local `food_products` table; subsequent
//!   reads hit the cache until `last_synced_at` ages out.
//!
//! # Sibling Open*Facts databases
//!
//! Open Beauty Facts (`world.openbeautyfacts.org`) and Open Pet Food
//! Facts (`world.openpetfoodfacts.org`) speak the same response shape.
//! Both work by swapping [`OpenFoodFactsConfig::base_url`].

use std::time::Duration;

use serde::Deserialize;
use thiserror::Error;

use crate::nutrition::NutritionFacts;

/// Default endpoint. Use `world.openfoodfacts.net` (the staging-grade
/// alias of the production cluster) so we don't hammer the read-only
/// production replica.
const DEFAULT_BASE_URL: &str = "https://world.openfoodfacts.net";

/// Field whitelist passed to the API as `?fields=…`. Trims the response
/// payload to just what the mapper consumes — important because the
/// full product record can be tens of kilobytes.
const FIELDS: &str = "code,product_name,brands,image_url,image_front_url,quantity,product_quantity,nutriments,ingredients_text,allergens_tags,categories_tags,nova_group,nutriscore_grade";

/// Default identifying string. The API rules require name + version +
/// contact URL so the maintainers can reach out if our usage looks off.
fn default_user_agent() -> String {
    format!(
        "task-server/{} (+https://github.com/FastTrackStudios/task)",
        env!("CARGO_PKG_VERSION")
    )
}

/// Configuration for [`OpenFoodFactsClient`].
#[derive(Debug, Clone, Default)]
pub struct OpenFoodFactsConfig {
    /// Override the base URL. Defaults to
    /// `https://world.openfoodfacts.net`. Swap for the Open Beauty
    /// Facts / Open Pet Food Facts hosts to reuse the same client
    /// against those sibling databases.
    pub base_url: Option<String>,
    /// Override the User-Agent string. Defaults to
    /// `task-server/<version> (+repo url)`.
    pub user_agent: Option<String>,
}

/// Errors returned by [`OpenFoodFactsClient::lookup`].
#[derive(Error, Debug)]
pub enum OpenFoodFactsError {
    #[error("network error: {0}")]
    Network(#[from] reqwest::Error),
    #[error("non-2xx response: {0}")]
    Status(reqwest::StatusCode),
    #[error("decode error: {0}")]
    Decode(String),
    #[error("empty body")]
    EmptyBody,
}

/// Mapped subset of an Open Food Facts product record, normalized into
/// SI units (sodium in mg, energy in kcal). See
/// [`OpenFoodFactsClient::lookup`] for the source of each field.
#[derive(Debug, Clone, Default)]
pub struct OpenFoodFactsProduct {
    pub barcode: String,
    pub product_name: Option<String>,
    pub brands: Option<String>,
    pub image_url: Option<String>,
    /// Raw `quantity` string (e.g. `"500 ml"`).
    pub quantity_label: Option<String>,
    /// `product_quantity` parsed as grams when present.
    pub package_size_g: Option<f64>,
    pub ingredients_text: Option<String>,
    pub categories: Vec<String>,
    pub allergens: Vec<String>,
    pub nova_group: Option<u8>,
    pub nutriscore_grade: Option<String>,
    pub nutrition: NutritionFacts,
}

/// HTTP client wrapper around the Open Food Facts product API.
pub struct OpenFoodFactsClient {
    base_url: String,
    user_agent: String,
    http: reqwest::Client,
}

impl OpenFoodFactsClient {
    /// Build a new client with the given configuration. Uses a 30-second
    /// HTTP timeout — long enough for the occasional slow Cloudflare
    /// path through the OFF read replica.
    #[must_use]
    pub fn new(config: OpenFoodFactsConfig) -> Self {
        let user_agent = config.user_agent.unwrap_or_else(default_user_agent);
        let http = reqwest::Client::builder()
            .timeout(Duration::from_secs(30))
            .user_agent(user_agent.clone())
            .build()
            .expect("static reqwest::Client config builds");
        Self {
            base_url: config
                .base_url
                .unwrap_or_else(|| DEFAULT_BASE_URL.to_string()),
            user_agent,
            http,
        }
    }

    /// Build with an externally-supplied [`reqwest::Client`]. Used by
    /// integration tests (and any caller that wants custom retry / proxy
    /// behavior) to point the client at a mock server.
    #[must_use]
    pub fn with_http(http: reqwest::Client, base_url: String, user_agent: String) -> Self {
        Self {
            base_url,
            user_agent,
            http,
        }
    }

    /// Configured base URL. Exposed so tests can verify config wiring.
    #[must_use]
    pub fn base_url(&self) -> &str {
        &self.base_url
    }

    /// User-Agent string this client identifies as.
    #[must_use]
    pub fn user_agent(&self) -> &str {
        &self.user_agent
    }

    /// Look up a product by barcode.
    ///
    /// Returns `Ok(None)` when the API responds `status: 0` (product not
    /// found — note: HTTP 200 in that case). Returns `Err` on network /
    /// non-2xx / decode failures. The `barcode` is treated as opaque —
    /// any non-empty string is forwarded; the API itself rejects bad
    /// inputs with a non-2xx response.
    pub async fn lookup(
        &self,
        barcode: &str,
    ) -> Result<Option<OpenFoodFactsProduct>, OpenFoodFactsError> {
        let url = format!(
            "{}/api/v2/product/{}?fields={}",
            self.base_url.trim_end_matches('/'),
            barcode,
            FIELDS,
        );
        let response = self.http.get(&url).send().await?;
        let status = response.status();
        if !status.is_success() {
            return Err(OpenFoodFactsError::Status(status));
        }
        let body = response.text().await?;
        if body.trim().is_empty() {
            return Err(OpenFoodFactsError::EmptyBody);
        }
        let envelope: ProductEnvelope = serde_json::from_str(&body)
            .map_err(|err| OpenFoodFactsError::Decode(err.to_string()))?;
        if envelope.status != 1 {
            return Ok(None);
        }
        let raw = match envelope.product {
            Some(p) => p,
            None => return Ok(None),
        };
        Ok(Some(map_product(barcode, raw)))
    }
}

/// Transcoded API envelope. Only what the mapper needs.
#[derive(Debug, Deserialize)]
struct ProductEnvelope {
    /// `1` = product present, `0` = not found.
    #[serde(default)]
    status: u8,
    #[serde(default)]
    product: Option<RawProduct>,
}

#[derive(Debug, Default, Deserialize)]
struct RawProduct {
    #[serde(default)]
    code: Option<String>,
    #[serde(default)]
    product_name: Option<String>,
    #[serde(default)]
    brands: Option<String>,
    #[serde(default)]
    image_url: Option<String>,
    #[serde(default)]
    image_front_url: Option<String>,
    #[serde(default)]
    quantity: Option<String>,
    /// OFF reports this as either a number or sometimes a stringified
    /// number; accept both via [`StringOrNumber`].
    #[serde(default)]
    product_quantity: Option<StringOrNumber>,
    #[serde(default)]
    nutriments: Option<serde_json::Value>,
    #[serde(default)]
    ingredients_text: Option<String>,
    #[serde(default)]
    allergens_tags: Vec<String>,
    #[serde(default)]
    categories_tags: Vec<String>,
    #[serde(default)]
    nova_group: Option<StringOrNumber>,
    #[serde(default)]
    nutriscore_grade: Option<String>,
}

/// Permissive numeric decoder — Open Food Facts cheerfully ships
/// `"500"`, `500`, and `500.0` for the same field across products.
#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum StringOrNumber {
    Num(f64),
    Str(String),
}

impl StringOrNumber {
    fn as_f64(&self) -> Option<f64> {
        match self {
            Self::Num(n) => Some(*n),
            Self::Str(s) => s.trim().parse::<f64>().ok(),
        }
    }
}

fn map_product(barcode: &str, raw: RawProduct) -> OpenFoodFactsProduct {
    let resolved_barcode = raw.code.clone().unwrap_or_else(|| barcode.to_string());
    let image_url = raw
        .image_front_url
        .clone()
        .or_else(|| raw.image_url.clone());
    let package_size_g = raw.product_quantity.as_ref().and_then(|v| v.as_f64());
    let nova_group = raw
        .nova_group
        .as_ref()
        .and_then(|v| v.as_f64())
        .and_then(|n| {
            let rounded = n.round();
            if (1.0..=4.0).contains(&rounded) {
                Some(rounded as u8)
            } else {
                None
            }
        });
    let nutrition = nutriments_to_facts(raw.nutriments.as_ref());
    OpenFoodFactsProduct {
        barcode: resolved_barcode,
        product_name: raw.product_name,
        brands: raw.brands,
        image_url,
        quantity_label: raw.quantity,
        package_size_g,
        ingredients_text: raw.ingredients_text,
        categories: raw.categories_tags.iter().map(|t| strip_tag(t)).collect(),
        allergens: raw.allergens_tags.iter().map(|t| strip_tag(t)).collect(),
        nova_group,
        nutriscore_grade: raw.nutriscore_grade,
        nutrition,
    }
}

/// Strip the `en:` (or any short language) prefix from a tag and turn
/// hyphens into spaces. `"en:olive-oils"` → `"olive oils"`.
fn strip_tag(tag: &str) -> String {
    let body = match tag.split_once(':') {
        Some((prefix, rest)) if prefix.len() <= 5 => rest,
        _ => tag,
    };
    body.replace('-', " ")
}

/// Pull a numeric value out of a `nutriments` object regardless of
/// whether OFF stored it as a number or string.
fn nutriment_f64(nutriments: &serde_json::Value, key: &str) -> Option<f64> {
    let v = nutriments.get(key)?;
    if let Some(n) = v.as_f64() {
        return Some(n);
    }
    v.as_str().and_then(|s| s.trim().parse::<f64>().ok())
}

/// Convert OFF's `nutriments` object into our [`NutritionFacts`] shape.
///
/// Quirks documented as `nutrition.notes` so callers can audit the
/// provenance of each row:
///
/// * Energy: prefer `energy-kcal_100g`; fall back to `energy_100g`
///   (assumed kJ → divide by 4.184).
/// * Sodium: OFF reports grams in `sodium_100g`; we multiply by 1000
///   to land in mg. If only `salt_100g` is present, divide by 2.5
///   (the canonical sodium-from-salt ratio) before scaling.
/// * `nutriments` itself is occasionally absent on legacy records; we
///   return `NutritionFacts::default()` in that case.
fn nutriments_to_facts(nutriments: Option<&serde_json::Value>) -> NutritionFacts {
    let Some(obj) = nutriments else {
        return NutritionFacts {
            source: Some("openfoodfacts".to_string()),
            ..Default::default()
        };
    };

    let mut notes: Vec<String> = Vec::new();

    let kcal = match nutriment_f64(obj, "energy-kcal_100g") {
        Some(n) => Some(n),
        None => nutriment_f64(obj, "energy_100g").map(|kj| {
            notes
                .push("energy converted from energy_100g (kJ) using 1 kcal = 4.184 kJ".to_string());
            kj / 4.184
        }),
    };

    let sodium_mg = match nutriment_f64(obj, "sodium_100g") {
        Some(g) => Some(g * 1000.0),
        None => nutriment_f64(obj, "salt_100g").map(|salt_g| {
            notes.push("sodium derived from salt_100g via salt/2.5".to_string());
            (salt_g / 2.5) * 1000.0
        }),
    };

    let notes_payload = if notes.is_empty() {
        None
    } else {
        Some(notes.join("; "))
    };

    NutritionFacts {
        kcal_per_100g: kcal,
        protein_g: nutriment_f64(obj, "proteins_100g"),
        carbs_g: nutriment_f64(obj, "carbohydrates_100g"),
        sugars_g: nutriment_f64(obj, "sugars_100g"),
        fiber_g: nutriment_f64(obj, "fiber_100g"),
        fat_g: nutriment_f64(obj, "fat_100g"),
        saturated_fat_g: nutriment_f64(obj, "saturated-fat_100g"),
        sodium_mg,
        source: Some("openfoodfacts".to_string()),
        notes: notes_payload,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn strip_tag_removes_lang_prefix_and_hyphens() {
        assert_eq!(strip_tag("en:olive-oils"), "olive oils");
        assert_eq!(strip_tag("fr:huiles-d-olive"), "huiles d olive");
        assert_eq!(strip_tag("plain-tag"), "plain tag");
    }

    #[test]
    fn nutriments_kcal_present_skips_kj_fallback() {
        let n = json!({
            "energy-kcal_100g": 884.0,
            "energy_100g": 3700.0,
            "fat_100g": 100.0,
        });
        let facts = nutriments_to_facts(Some(&n));
        assert!((facts.kcal_per_100g.unwrap() - 884.0).abs() < 1e-6);
        assert_eq!(facts.notes, None);
        assert_eq!(facts.source.as_deref(), Some("openfoodfacts"));
    }

    #[test]
    fn nutriments_falls_back_to_kj_with_warning() {
        let n = json!({ "energy_100g": 4184.0 });
        let facts = nutriments_to_facts(Some(&n));
        assert!((facts.kcal_per_100g.unwrap() - 1000.0).abs() < 1e-6);
        let notes = facts.notes.unwrap();
        assert!(notes.contains("kJ"), "expected kJ note, got {notes}");
    }

    #[test]
    fn sodium_grams_to_mg() {
        let n = json!({ "sodium_100g": 0.5 });
        let facts = nutriments_to_facts(Some(&n));
        assert!((facts.sodium_mg.unwrap() - 500.0).abs() < 1e-6);
        assert!(facts.notes.is_none());
    }

    #[test]
    fn sodium_falls_back_to_salt() {
        let n = json!({ "salt_100g": 1.25 });
        let facts = nutriments_to_facts(Some(&n));
        // 1.25 / 2.5 * 1000 = 500
        assert!((facts.sodium_mg.unwrap() - 500.0).abs() < 1e-6);
        let notes = facts.notes.unwrap();
        assert!(notes.contains("salt"));
    }

    #[test]
    fn missing_nutriments_yields_default_with_source() {
        let facts = nutriments_to_facts(None);
        assert!(facts.kcal_per_100g.is_none());
        assert_eq!(facts.source.as_deref(), Some("openfoodfacts"));
    }

    #[test]
    fn nutriments_handle_string_numbers() {
        let n = json!({
            "energy-kcal_100g": "884",
            "fat_100g": "100.0",
        });
        let facts = nutriments_to_facts(Some(&n));
        assert!((facts.kcal_per_100g.unwrap() - 884.0).abs() < 1e-6);
        assert!((facts.fat_g.unwrap() - 100.0).abs() < 1e-6);
    }

    #[test]
    fn map_product_pulls_image_front_first() {
        let raw = RawProduct {
            image_url: Some("fallback".into()),
            image_front_url: Some("front".into()),
            ..Default::default()
        };
        let p = map_product("123", raw);
        assert_eq!(p.image_url.as_deref(), Some("front"));
    }

    #[test]
    fn map_product_falls_back_to_image_url() {
        let raw = RawProduct {
            image_url: Some("fallback".into()),
            ..Default::default()
        };
        let p = map_product("123", raw);
        assert_eq!(p.image_url.as_deref(), Some("fallback"));
    }

    #[test]
    fn map_product_clamps_nova_group() {
        let raw = RawProduct {
            nova_group: Some(StringOrNumber::Num(7.0)),
            ..Default::default()
        };
        let p = map_product("123", raw);
        assert!(p.nova_group.is_none());
        let raw = RawProduct {
            nova_group: Some(StringOrNumber::Str("3".into())),
            ..Default::default()
        };
        let p = map_product("123", raw);
        assert_eq!(p.nova_group, Some(3));
    }

    #[test]
    fn map_product_uses_argument_when_code_missing() {
        let raw = RawProduct {
            code: None,
            ..Default::default()
        };
        let p = map_product("0048500201497", raw);
        assert_eq!(p.barcode, "0048500201497");
    }
}
