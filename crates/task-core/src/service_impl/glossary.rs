//! SeaORM-backed [`GlossaryService`] implementation.
//!
//! Workflow-agnostic term catalog + a wrapper around the pure-Rust
//! [`crate::glossary::wikilinks`] resolver that batches the slug
//! lookup (one query per resolve call, alias fallback for misses).

use chrono::Utc;
use sea_orm::{
    ActiveModelTrait, ColumnTrait, DatabaseConnection, EntityTrait, QueryFilter, QueryOrder, Set,
};
use serde::Serialize;
use uuid::Uuid;

use crate::glossary::{
    self, GlossaryAliasList, GlossaryRelatedList, GlossaryTermApi, ResolvedWikilink, WikilinkSpan,
    find_wikilinks, model as glossary_model, resolve_wikilinks, slugify,
};
use crate::property::JsonObject;
use crate::service::{
    CreateGlossaryTermRequest, GlossaryService, GlossaryTermPatch, ResolveInTextRequest,
    ResolveInTextView, VaultError,
};

use super::helpers::convert_model;

pub struct GlossaryServiceDeps {
    pub db: DatabaseConnection,
}

#[derive(Clone)]
pub struct GlossaryServiceImpl {
    db: DatabaseConnection,
}

impl GlossaryServiceImpl {
    #[must_use]
    pub fn new(deps: GlossaryServiceDeps) -> Self {
        Self { db: deps.db }
    }
}

fn io(err: impl std::fmt::Display, ctx: &str) -> VaultError {
    VaultError::IoError(format!("{ctx}: {err}"))
}

fn term_to_api(model: glossary_model::Model) -> Result<GlossaryTermApi, VaultError> {
    convert_model::<glossary_model::Model, GlossaryTermApi>(model)
}

fn normalize_aliases(input: Vec<String>) -> Vec<String> {
    let mut out: Vec<String> = Vec::new();
    for raw in input {
        let alias = raw.trim().to_lowercase();
        if alias.is_empty() {
            continue;
        }
        if !out.iter().any(|existing| existing == &alias) {
            out.push(alias);
        }
    }
    out
}

#[derive(Serialize)]
struct TermSummary {
    name: String,
    slug: String,
    category: String,
}

#[derive(Serialize)]
struct ResolvedSpanView {
    span: SpanView,
    target_id: Option<Uuid>,
    term_summary: Option<TermSummary>,
}

#[derive(Serialize)]
struct SpanView {
    start: usize,
    end: usize,
    raw: String,
    slug: String,
    display: Option<String>,
}

impl From<&WikilinkSpan> for SpanView {
    fn from(s: &WikilinkSpan) -> Self {
        Self {
            start: s.start,
            end: s.end,
            raw: s.raw.clone(),
            slug: s.slug.clone(),
            display: s.display.clone(),
        }
    }
}

impl GlossaryService for GlossaryServiceImpl {
    async fn list_terms(
        &self,
        organization: Option<String>,
        category: Option<String>,
    ) -> Result<Vec<GlossaryTermApi>, VaultError> {
        let mut q = glossary::GlossaryEntity::find().order_by_asc(glossary_model::Column::Name);
        if let Some(org) = organization {
            q = q.filter(glossary_model::Column::Organization.eq(org));
        }
        if let Some(cat) = category {
            q = q.filter(glossary_model::Column::Category.eq(cat));
        }
        let rows = q.all(&self.db).await.map_err(|e| io(e, "list_terms"))?;
        rows.into_iter().map(term_to_api).collect()
    }

    async fn get_term(&self, id: Uuid) -> Result<Option<GlossaryTermApi>, VaultError> {
        let row = glossary::GlossaryEntity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "get_term"))?;
        row.map(term_to_api).transpose()
    }

    async fn find_term_by_slug_or_alias(
        &self,
        organization: Option<String>,
        category: Option<String>,
        slug_or_alias: String,
    ) -> Result<Option<GlossaryTermApi>, VaultError> {
        let hit = glossary::find_term_by_slug_or_alias(
            &self.db,
            organization.as_deref(),
            category.as_deref(),
            &slug_or_alias,
        )
        .await
        .map_err(|e| io(e, "find_term_by_slug_or_alias"))?;
        hit.map(term_to_api).transpose()
    }

    async fn create_term(
        &self,
        request: CreateGlossaryTermRequest,
    ) -> Result<GlossaryTermApi, VaultError> {
        let name = request.name.trim().to_string();
        if name.is_empty() {
            return Err(VaultError::ParseError(
                "glossary term name is empty".to_string(),
            ));
        }
        let category = if request.category.trim().is_empty() {
            "general".to_string()
        } else {
            request.category.trim().to_string()
        };
        let slug = match request.slug {
            Some(s) if !s.trim().is_empty() => slugify(s.trim()),
            _ => slugify(&name),
        };
        if slug.is_empty() {
            return Err(VaultError::ParseError(
                "glossary term slug is empty".to_string(),
            ));
        }
        let aliases = GlossaryAliasList::from(normalize_aliases(request.aliases));
        let related = GlossaryRelatedList::from(request.related_term_ids);
        let now = Utc::now();
        let active = glossary_model::ActiveModel {
            id: Set(Uuid::new_v4()),
            name: Set(name),
            slug: Set(slug),
            body_markdown: Set(request.body_markdown),
            aliases: Set(aliases),
            category: Set(category),
            related_term_ids: Set(related),
            organization: Set(request.organization),
            created_by: Set(request.created_by),
            properties: Set(JsonObject::default()),
            created_at: Set(now),
            updated_at: Set(now),
        };
        let saved = active
            .insert(&self.db)
            .await
            .map_err(|e| io(e, "insert glossary_term"))?;
        term_to_api(saved)
    }

    async fn update_term(
        &self,
        id: Uuid,
        patch: GlossaryTermPatch,
    ) -> Result<GlossaryTermApi, VaultError> {
        let model = glossary::GlossaryEntity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load glossary_term"))?
            .ok_or_else(|| VaultError::NotFound(format!("glossary_term:{id}")))?;
        let mut active: glossary_model::ActiveModel = model.into();
        if let Some(name) = patch.name {
            active.name = Set(name);
        }
        if let Some(slug) = patch.slug {
            let s = slugify(slug.trim());
            if s.is_empty() {
                return Err(VaultError::ParseError("slug is empty".to_string()));
            }
            active.slug = Set(s);
        }
        if let Some(body) = patch.body_markdown {
            active.body_markdown = Set(body);
        }
        if let Some(aliases) = patch.aliases {
            active.aliases = Set(GlossaryAliasList::from(normalize_aliases(aliases)));
        }
        if let Some(category) = patch.category {
            active.category = Set(category);
        }
        if let Some(related) = patch.related_term_ids {
            active.related_term_ids = Set(GlossaryRelatedList::from(related));
        }
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "update glossary_term"))?;
        term_to_api(saved)
    }

    async fn delete_term(&self, id: Uuid) -> Result<(), VaultError> {
        glossary::GlossaryEntity::delete_by_id(id)
            .exec(&self.db)
            .await
            .map_err(|e| io(e, "delete glossary_term"))?;
        Ok(())
    }

    async fn add_alias(&self, id: Uuid, alias: String) -> Result<GlossaryTermApi, VaultError> {
        let model = glossary::GlossaryEntity::find_by_id(id)
            .one(&self.db)
            .await
            .map_err(|e| io(e, "load glossary_term"))?
            .ok_or_else(|| VaultError::NotFound(format!("glossary_term:{id}")))?;
        let mut current: Vec<String> = model.aliases.clone().0;
        current.push(alias);
        let normalized = normalize_aliases(current);
        let mut active: glossary_model::ActiveModel = model.into();
        active.aliases = Set(GlossaryAliasList::from(normalized));
        active.updated_at = Set(Utc::now());
        let saved = active
            .update(&self.db)
            .await
            .map_err(|e| io(e, "add_alias"))?;
        term_to_api(saved)
    }

    async fn resolve_in_text(
        &self,
        request: ResolveInTextRequest,
    ) -> Result<ResolveInTextView, VaultError> {
        let spans = find_wikilinks(&request.text);
        if spans.is_empty() {
            return Ok(ResolveInTextView {
                spans_json: "[]".to_string(),
                resolved_term_ids: Vec::new(),
            });
        }
        // Distinct slugs in first-appearance order.
        let mut distinct: Vec<String> = Vec::new();
        for s in &spans {
            if !distinct.contains(&s.slug) {
                distinct.push(s.slug.clone());
            }
        }
        let index = glossary::build_slug_index(
            &self.db,
            request.organization.as_deref(),
            request.category.as_deref(),
            &distinct,
        )
        .await
        .map_err(|e| io(e, "build_slug_index"))?;

        let slug_to_id: std::collections::HashMap<String, Uuid> =
            index.iter().map(|(k, (id, _))| (k.clone(), *id)).collect();
        let resolved: Vec<ResolvedWikilink> = resolve_wikilinks(&spans, &slug_to_id);

        let mut resolved_ids: Vec<Uuid> = Vec::new();
        let mut views: Vec<ResolvedSpanView> = Vec::with_capacity(resolved.len());
        for r in &resolved {
            let summary = if let Some(id) = r.target_id {
                if !resolved_ids.contains(&id) {
                    resolved_ids.push(id);
                }
                index.get(&r.span.slug).map(|(_, term)| TermSummary {
                    name: term.name.clone(),
                    slug: term.slug.clone(),
                    category: term.category.clone(),
                })
            } else {
                None
            };
            views.push(ResolvedSpanView {
                span: SpanView::from(&r.span),
                target_id: r.target_id,
                term_summary: summary,
            });
        }
        let spans_json = serde_json::to_string(&views)
            .map_err(|e| VaultError::ParseError(format!("encode spans: {e}")))?;
        Ok(ResolveInTextView {
            spans_json,
            resolved_term_ids: resolved_ids,
        })
    }
}
