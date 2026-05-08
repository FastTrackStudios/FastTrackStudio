//! OpenGraph fallback. Pulled in only when JSON-LD extraction failed.
//! Produces a thin draft (no ingredients/steps) so the operator can
//! finish the recipe by hand.

use scraper::{Html, Selector};

use crate::service::CreateRecipeRequest;

#[derive(Debug, Clone, Default)]
pub(super) struct ExtractedOg {
    pub draft: CreateRecipeRequest,
    pub image_url: Option<String>,
    pub source_url: Option<String>,
    pub warnings: Vec<String>,
}

pub(super) fn extract(html: &str) -> Option<ExtractedOg> {
    let document = Html::parse_document(html);
    let selector = Selector::parse("meta").ok()?;
    let mut title: Option<String> = None;
    let mut description: Option<String> = None;
    let mut image: Option<String> = None;
    let mut url: Option<String> = None;

    for el in document.select(&selector) {
        let attrs = el.value();
        let property = attrs
            .attr("property")
            .or_else(|| attrs.attr("name"))
            .unwrap_or("")
            .to_ascii_lowercase();
        let content = match attrs.attr("content") {
            Some(c) => c.to_string(),
            None => continue,
        };
        match property.as_str() {
            "og:title" => title.get_or_insert(content),
            "og:description" => description.get_or_insert(content),
            "og:image" => image.get_or_insert(content),
            "og:url" => url.get_or_insert(content),
            _ => continue,
        };
    }

    // Title is required — without it, we have nothing actionable.
    let name = title?.trim().to_string();
    if name.is_empty() {
        return None;
    }

    let warnings = vec![
        "no JSON-LD recipe found — used OpenGraph fallback".to_string(),
        "no ingredients/steps recovered — manual entry required".to_string(),
    ];

    let draft = CreateRecipeRequest {
        name,
        description: description.map(|s| s.trim().to_string()),
        organization: None,
        prep_time_minutes: None,
        cook_time_minutes: None,
        servings: None,
        source_url: url.clone(),
        created_by: None,
        image_url: image.clone(),
        yield_label: None,
        properties_json: None,
        ingredients_json: "[]".to_string(),
        steps_json: "[]".to_string(),
    };

    Some(ExtractedOg {
        draft,
        image_url: image,
        source_url: url,
        warnings,
    })
}
