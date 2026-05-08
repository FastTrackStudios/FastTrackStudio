//! Mealie-style recipe importer.
//!
//! Pure-Rust pipeline: fetch → JSON-LD (schema.org Recipe) → OpenGraph
//! fallback. No site-specific scrapers, no AI, no Python sidecar. The
//! caller is responsible for stamping `organization` / `created_by`
//! and persisting via [`crate::service::CookingService::create_recipe`].

mod duration;
mod jsonld;
mod opengraph;

use std::time::Duration;

use thiserror::Error;

use crate::service::CreateRecipeRequest;

pub use duration::parse_iso_duration;

const DEFAULT_USER_AGENT: &str = "TaskRecipeImporter/0.1 (+https://github.com/Codys-Wright/Task)";

/// Strategy that produced the import result.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportStrategy {
    /// Schema.org JSON-LD `Recipe` block.
    JsonLd,
    /// OpenGraph `og:*` meta tags.
    OpenGraph,
}

impl ImportStrategy {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::JsonLd => "json-ld",
            Self::OpenGraph => "opengraph",
        }
    }
}

#[derive(Debug, Clone)]
pub struct RecipeImportResult {
    /// Populated `CreateRecipeRequest` ready to feed into
    /// `CookingService::create_recipe`. Caller must set the
    /// `organization` field on the request.
    pub draft: CreateRecipeRequest,
    /// Source URL the importer fetched (final URL after redirects).
    pub source_url: String,
    /// Image URL discovered, if any.
    pub image_url: Option<String>,
    /// Which strategy produced the result.
    pub strategy: ImportStrategy,
    /// Free-form warnings (e.g. "ratingValue out of range, clamped").
    pub warnings: Vec<String>,
}

#[derive(Error, Debug)]
pub enum ImportError {
    #[error("network error: {0}")]
    Network(#[from] reqwest::Error),
    #[error("non-2xx response: {0}")]
    Status(reqwest::StatusCode),
    #[error("response body was empty")]
    EmptyBody,
    #[error("no schema.org Recipe found and OpenGraph fallback insufficient")]
    NoRecipeFound,
    #[error("parse error: {0}")]
    Parse(String),
}

pub struct RecipeImporter {
    client: reqwest::Client,
}

impl Default for RecipeImporter {
    fn default() -> Self {
        Self::new()
    }
}

impl RecipeImporter {
    /// New importer with sane defaults: 30-second timeout, identifying
    /// User-Agent.
    #[must_use]
    pub fn new() -> Self {
        let client = reqwest::Client::builder()
            .timeout(Duration::from_secs(30))
            .user_agent(DEFAULT_USER_AGENT)
            .build()
            .expect("static reqwest::Client config builds");
        Self { client }
    }

    /// Use a caller-provided client (handy for tests + custom proxies).
    #[must_use]
    pub fn with_client(client: reqwest::Client) -> Self {
        Self { client }
    }

    /// Fetch a URL, parse the body, and produce a [`RecipeImportResult`].
    pub async fn import(&self, url: &str) -> Result<RecipeImportResult, ImportError> {
        let response = self.client.get(url).send().await?;
        let status = response.status();
        if !status.is_success() {
            return Err(ImportError::Status(status));
        }
        let final_url = response.url().to_string();
        let body = response.text().await?;
        if body.trim().is_empty() {
            return Err(ImportError::EmptyBody);
        }
        Self::parse(&body, &final_url)
    }

    /// Parse a previously-fetched HTML body. Exposed for the test
    /// fixtures + parity with the live `import` path.
    pub fn parse(body: &str, source_url: &str) -> Result<RecipeImportResult, ImportError> {
        if let Some(extracted) = jsonld::extract(body) {
            let mut draft = extracted.draft;
            // Force the source URL to the final fetched URL when JSON-LD
            // didn't surface one.
            if draft.source_url.is_none() {
                draft.source_url = Some(source_url.to_string());
            }
            return Ok(RecipeImportResult {
                draft,
                source_url: source_url.to_string(),
                image_url: extracted.image_url,
                strategy: ImportStrategy::JsonLd,
                warnings: extracted.warnings,
            });
        }
        if let Some(og) = opengraph::extract(body) {
            let mut draft = og.draft;
            if draft.source_url.is_none() {
                draft.source_url = Some(source_url.to_string());
            }
            return Ok(RecipeImportResult {
                draft,
                source_url: og.source_url.unwrap_or_else(|| source_url.to_string()),
                image_url: og.image_url,
                strategy: ImportStrategy::OpenGraph,
                warnings: og.warnings,
            });
        }
        Err(ImportError::NoRecipeFound)
    }
}
