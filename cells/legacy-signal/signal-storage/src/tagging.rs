//! Preset tagging and categorization service.
//!
//! Provides tag CRUD, category management, tag-based queries, autocomplete,
//! and favorite/starred flag operations — all against the SeaORM preset entity.
//!
//! Tags are stored as a JSON array of strings in the `tags` column.
//! Categories are stored as a JSON object in the `category` column.

use std::collections::{BTreeSet, HashMap, HashSet};

use sea_orm::prelude::*;
use sea_orm::{ActiveValue, Condition, IntoActiveModel, QueryOrder, QuerySelect};
use serde_json::Value as JsonValue;

use signal_proto::id::TagId;
use signal_proto::normalized::Rating;
use signal_proto::tags::auto_tagger::AutoTagger;
use signal_proto::tags::stats::TagStats;
use signal_proto::tags::suggestions::TagCooccurrence;
use signal_proto::tags::{TagFilter, TagRegistry};

use crate::entities::preset;
use crate::error::{StorageError, StorageResult};
use crate::tag_bridge;

// region: --- PresetCategory enum

/// Preset category for organization.
///
/// Maps to the `category` JSON column, providing a simpler top-level
/// classification than the full hierarchical `signal_proto::PresetCategory`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PresetCategory {
    Amp,
    Effect,
    FullRig,
    Module,
    Snapshot,
}

impl PresetCategory {
    /// All category variants in display order.
    pub const ALL: &'static [Self] = &[
        Self::Amp,
        Self::Effect,
        Self::FullRig,
        Self::Module,
        Self::Snapshot,
    ];

    /// Display name for this category.
    pub const fn display_name(&self) -> &'static str {
        match self {
            Self::Amp => "Amp",
            Self::Effect => "Effect",
            Self::FullRig => "Full Rig",
            Self::Module => "Module",
            Self::Snapshot => "Snapshot",
        }
    }

    /// Parse from JSON value.
    pub fn from_json(value: &JsonValue) -> Option<Self> {
        serde_json::from_value(value.clone()).ok()
    }

    /// Convert to JSON value.
    pub fn to_json(&self) -> JsonValue {
        serde_json::to_value(self).expect("PresetCategory always serializes")
    }
}

impl std::fmt::Display for PresetCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.display_name())
    }
}

// endregion: --- PresetCategory enum

// region: --- Tag helpers

/// Extract tags from a preset model's JSON `tags` field.
///
/// Expects a JSON array of strings. Returns empty vec on malformed data.
pub fn extract_tags(model: &preset::Model) -> Vec<String> {
    match &model.tags {
        JsonValue::Array(arr) => arr
            .iter()
            .filter_map(|v| v.as_str().map(String::from))
            .collect(),
        _ => Vec::new(),
    }
}

/// Build a JSON array value from a list of tag strings.
fn tags_to_json(tags: &[String]) -> JsonValue {
    JsonValue::Array(tags.iter().map(|t| JsonValue::String(t.clone())).collect())
}

// endregion: --- Tag helpers

// region: --- TaggingService

/// Service for preset tagging, categorization, and favorite operations.
pub struct TaggingService;

impl TaggingService {
    // ── Tag CRUD ────────────────────────────────────────────────────────

    /// Add a tag to a preset. No-op if already present.
    pub async fn add_tag(
        db: &DatabaseConnection,
        preset_id: Uuid,
        tag: &str,
    ) -> StorageResult<preset::Model> {
        let model = Self::find_preset(db, preset_id).await?;
        let mut tags = extract_tags(&model);

        if !tags.iter().any(|t| t.eq_ignore_ascii_case(tag)) {
            tags.push(tag.to_string());
            Self::update_tags(db, model, &tags).await
        } else {
            Ok(model)
        }
    }

    /// Remove a tag from a preset. No-op if not present.
    pub async fn remove_tag(
        db: &DatabaseConnection,
        preset_id: Uuid,
        tag: &str,
    ) -> StorageResult<preset::Model> {
        let model = Self::find_preset(db, preset_id).await?;
        let mut tags = extract_tags(&model);
        let before_len = tags.len();
        tags.retain(|t| !t.eq_ignore_ascii_case(tag));

        if tags.len() != before_len {
            Self::update_tags(db, model, &tags).await
        } else {
            Ok(model)
        }
    }

    /// Get all tags for a preset.
    pub async fn get_tags(db: &DatabaseConnection, preset_id: Uuid) -> StorageResult<Vec<String>> {
        let model = Self::find_preset(db, preset_id).await?;
        Ok(extract_tags(&model))
    }

    /// Set the full tag list for a preset (replaces existing tags).
    pub async fn set_tags(
        db: &DatabaseConnection,
        preset_id: Uuid,
        tags: &[String],
    ) -> StorageResult<preset::Model> {
        let model = Self::find_preset(db, preset_id).await?;
        Self::update_tags(db, model, tags).await
    }

    // ── Tag queries ─────────────────────────────────────────────────────

    /// List all presets that have a given tag (case-insensitive).
    ///
    /// Loads all non-deleted presets and filters in memory since tags are
    /// stored as JSON arrays. For moderate-size preset libraries this is
    /// efficient enough; a GIN index approach would be needed at scale.
    pub async fn list_by_tag(
        db: &DatabaseConnection,
        tag: &str,
    ) -> StorageResult<Vec<preset::Model>> {
        let tag_lower = tag.to_lowercase();
        let all = preset::Entity::find()
            .filter(preset::Column::IsDeleted.eq(false))
            .order_by_asc(preset::Column::Name)
            .all(db)
            .await?;

        Ok(all
            .into_iter()
            .filter(|m| {
                extract_tags(m)
                    .iter()
                    .any(|t| t.to_lowercase() == tag_lower)
            })
            .collect())
    }

    /// List all distinct tags across all non-deleted presets.
    ///
    /// Returns sorted, deduplicated tag names. Used for tag panels and autocomplete.
    pub async fn list_tags(db: &DatabaseConnection) -> StorageResult<Vec<String>> {
        let all = preset::Entity::find()
            .filter(preset::Column::IsDeleted.eq(false))
            .column(preset::Column::Tags)
            .all(db)
            .await?;

        let mut unique: BTreeSet<String> = BTreeSet::new();
        for model in &all {
            for tag in extract_tags(model) {
                unique.insert(tag);
            }
        }

        Ok(unique.into_iter().collect())
    }

    /// Tag autocomplete: returns tags matching a prefix, ordered by frequency.
    ///
    /// If `prefix` is empty, returns all tags sorted by frequency (most used first).
    pub async fn autocomplete_tags(
        db: &DatabaseConnection,
        prefix: &str,
    ) -> StorageResult<Vec<(String, usize)>> {
        let all = preset::Entity::find()
            .filter(preset::Column::IsDeleted.eq(false))
            .column(preset::Column::Tags)
            .all(db)
            .await?;

        let prefix_lower = prefix.to_lowercase();
        let mut freq: HashMap<String, usize> = HashMap::new();

        for model in &all {
            for tag in extract_tags(model) {
                if prefix.is_empty() || tag.to_lowercase().starts_with(&prefix_lower) {
                    *freq.entry(tag).or_default() += 1;
                }
            }
        }

        let mut results: Vec<(String, usize)> = freq.into_iter().collect();
        // Sort by frequency descending, then name ascending for stability
        results.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(&b.0)));
        Ok(results)
    }

    // ── Category ────────────────────────────────────────────────────────

    /// Set the category for a preset.
    pub async fn set_category(
        db: &DatabaseConnection,
        preset_id: Uuid,
        category: PresetCategory,
    ) -> StorageResult<preset::Model> {
        let model = Self::find_preset(db, preset_id).await?;
        let mut active: preset::ActiveModel = model.into_active_model();
        active.category = ActiveValue::Set(category.to_json());
        Ok(active.update(db).await?)
    }

    /// Get the category for a preset.
    pub async fn get_category(
        db: &DatabaseConnection,
        preset_id: Uuid,
    ) -> StorageResult<Option<PresetCategory>> {
        let model = Self::find_preset(db, preset_id).await?;
        Ok(PresetCategory::from_json(&model.category))
    }

    /// List presets by category.
    pub async fn list_by_category(
        db: &DatabaseConnection,
        category: PresetCategory,
    ) -> StorageResult<Vec<preset::Model>> {
        let all = preset::Entity::find()
            .filter(preset::Column::IsDeleted.eq(false))
            .order_by_asc(preset::Column::Name)
            .all(db)
            .await?;

        Ok(all
            .into_iter()
            .filter(|m| PresetCategory::from_json(&m.category) == Some(category))
            .collect())
    }

    // ── Favorites ───────────────────────────────────────────────────────

    /// Toggle the favorite/starred flag for a preset. Returns the new state.
    pub async fn toggle_favorite(db: &DatabaseConnection, preset_id: Uuid) -> StorageResult<bool> {
        let model = Self::find_preset(db, preset_id).await?;
        let new_value = !model.is_favorite;
        let mut active: preset::ActiveModel = model.into_active_model();
        active.is_favorite = ActiveValue::Set(new_value);
        active.update(db).await?;
        Ok(new_value)
    }

    /// Set the favorite flag explicitly.
    pub async fn set_favorite(
        db: &DatabaseConnection,
        preset_id: Uuid,
        favorite: bool,
    ) -> StorageResult<preset::Model> {
        let model = Self::find_preset(db, preset_id).await?;
        let mut active: preset::ActiveModel = model.into_active_model();
        active.is_favorite = ActiveValue::Set(favorite);
        Ok(active.update(db).await?)
    }

    /// List all favorited presets.
    pub async fn list_favorites(db: &DatabaseConnection) -> StorageResult<Vec<preset::Model>> {
        Ok(preset::Entity::find()
            .filter(
                Condition::all()
                    .add(preset::Column::IsFavorite.eq(true))
                    .add(preset::Column::IsDeleted.eq(false)),
            )
            .order_by_asc(preset::Column::Name)
            .all(db)
            .await?)
    }

    // ── Compound queries ────────────────────────────────────────────────

    /// Filter presets by multiple criteria.
    pub async fn filter_presets(
        db: &DatabaseConnection,
        tags: &[String],
        category: Option<PresetCategory>,
        favorites_only: bool,
    ) -> StorageResult<Vec<preset::Model>> {
        let mut query = preset::Entity::find().filter(preset::Column::IsDeleted.eq(false));

        if favorites_only {
            query = query.filter(preset::Column::IsFavorite.eq(true));
        }

        let all = query.order_by_asc(preset::Column::Name).all(db).await?;

        Ok(all
            .into_iter()
            .filter(|m| {
                // Category filter
                if let Some(cat) = category {
                    if PresetCategory::from_json(&m.category) != Some(cat) {
                        return false;
                    }
                }
                // Tag filter: preset must have ALL requested tags
                if !tags.is_empty() {
                    let preset_tags = extract_tags(m);
                    let preset_tags_lower: Vec<String> =
                        preset_tags.iter().map(|t| t.to_lowercase()).collect();
                    for required in tags {
                        if !preset_tags_lower.contains(&required.to_lowercase()) {
                            return false;
                        }
                    }
                }
                true
            })
            .collect())
    }

    // ── Auto-tagging ─────────────────────────────────────────────────────

    /// Auto-tag a preset from its name using the domain [`AutoTagger`].
    ///
    /// Inferred tags are merged with existing tags (case-insensitive dedup).
    /// Does not remove any existing manually-applied tags.
    pub async fn auto_tag(
        db: &DatabaseConnection,
        preset_id: Uuid,
        registry: &TagRegistry,
    ) -> StorageResult<preset::Model> {
        let model = Self::find_preset(db, preset_id).await?;
        let tagger = AutoTagger::with_defaults(registry);
        let inferred_ids = tagger.tag_name(&model.name);

        if inferred_ids.is_empty() {
            return Ok(model);
        }

        let existing = extract_tags(&model);
        let existing_lower: HashSet<String> = existing.iter().map(|t| t.to_lowercase()).collect();

        let mut merged = existing;
        for id in inferred_ids {
            let name = tag_bridge::resolve_tag_name_or(id, registry, "");
            if !name.is_empty() && !existing_lower.contains(&name.to_lowercase()) {
                merged.push(name);
            }
        }

        Self::update_tags(db, model, &merged).await
    }

    /// Auto-tag all non-deleted presets in the database.
    ///
    /// Returns the number of presets that were modified (gained new tags).
    pub async fn auto_tag_all(
        db: &DatabaseConnection,
        registry: &TagRegistry,
    ) -> StorageResult<u32> {
        let tagger = AutoTagger::with_defaults(registry);
        let all = preset::Entity::find()
            .filter(preset::Column::IsDeleted.eq(false))
            .all(db)
            .await?;

        let mut modified = 0u32;
        for model in all {
            let inferred_ids = tagger.tag_name(&model.name);
            if inferred_ids.is_empty() {
                continue;
            }

            let existing = extract_tags(&model);
            let existing_lower: HashSet<String> =
                existing.iter().map(|t| t.to_lowercase()).collect();

            let mut merged = existing;
            let mut changed = false;
            for id in inferred_ids {
                let name = tag_bridge::resolve_tag_name_or(id, registry, "");
                if !name.is_empty() && !existing_lower.contains(&name.to_lowercase()) {
                    merged.push(name);
                    changed = true;
                }
            }

            if changed {
                Self::update_tags(db, model, &merged).await?;
                modified += 1;
            }
        }

        Ok(modified)
    }

    // ── Domain-aware queries ────────────────────────────────────────────

    /// Query presets using the domain [`TagFilter`].
    ///
    /// Supports include/exclude tags, OR-group clauses, rating threshold,
    /// text search, and sort specifications. Falls back to in-memory
    /// filtering after loading candidates from the database.
    pub async fn query_presets(
        db: &DatabaseConnection,
        filter: &TagFilter,
        category: Option<PresetCategory>,
        favorites_only: bool,
        registry: &TagRegistry,
    ) -> StorageResult<Vec<preset::Model>> {
        let candidates = Self::load_candidates(db, category, favorites_only).await?;

        let mut results: Vec<preset::Model> = Vec::new();
        for model in candidates {
            let tags = tag_bridge::strings_to_tags(&extract_tags(&model), registry);
            let rating = Rating::default(); // TODO: join ratings when needed
            if filter.matches_full(&tags, &model.name, rating) {
                results.push(model);
            }
        }

        Ok(results)
    }

    /// Query presets with relevance scoring, returned in ranked order.
    ///
    /// Each result is paired with its relevance score (0.0–1.0).
    /// Results are sorted by score descending (most relevant first).
    pub async fn query_presets_scored(
        db: &DatabaseConnection,
        filter: &TagFilter,
        category: Option<PresetCategory>,
        favorites_only: bool,
        registry: &TagRegistry,
    ) -> StorageResult<Vec<(preset::Model, f64)>> {
        let candidates = Self::load_candidates(db, category, favorites_only).await?;

        let mut scored: Vec<(preset::Model, f64)> = Vec::new();
        for model in candidates {
            let tags = tag_bridge::strings_to_tags(&extract_tags(&model), registry);
            let rating = Rating::default();
            let score = filter.score_full(&tags, &model.name, rating, registry);
            if score.is_match() {
                scored.push((model, score.get()));
            }
        }

        scored.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

        Ok(scored)
    }

    // ── Tag suggestions ─────────────────────────────────────────────────

    /// Suggest tags based on co-occurrence patterns across all presets.
    ///
    /// Given the current tags on an item, returns up to `limit` suggestions
    /// ranked by how often they appear alongside the current tags.
    pub async fn suggest_tags(
        db: &DatabaseConnection,
        current_tags: &[String],
        registry: &TagRegistry,
        limit: usize,
    ) -> StorageResult<Vec<(String, f64)>> {
        let all = preset::Entity::find()
            .filter(preset::Column::IsDeleted.eq(false))
            .column(preset::Column::Tags)
            .all(db)
            .await?;

        let tag_sets: Vec<_> = all
            .iter()
            .map(|m| tag_bridge::strings_to_tags(&extract_tags(m), registry))
            .collect();

        let co = TagCooccurrence::build_from(tag_sets.iter());

        let current_ids: HashSet<TagId> = current_tags
            .iter()
            .map(|s| tag_bridge::resolve_tag_id(s, registry))
            .collect();

        let suggestions = co.suggest(&current_ids, limit);

        Ok(suggestions
            .into_iter()
            .filter_map(|(id, score)| {
                tag_bridge::resolve_tag_name(id, registry).map(|name| (name, score))
            })
            .collect())
    }

    // ── Tag statistics ──────────────────────────────────────────────────

    /// Compute tag statistics across all non-deleted presets.
    pub async fn tag_stats(
        db: &DatabaseConnection,
        registry: &TagRegistry,
    ) -> StorageResult<TagStatsReport> {
        let all = preset::Entity::find()
            .filter(preset::Column::IsDeleted.eq(false))
            .column(preset::Column::Tags)
            .all(db)
            .await?;

        let tag_sets: Vec<_> = all
            .iter()
            .map(|m| tag_bridge::strings_to_tags(&extract_tags(m), registry))
            .collect();

        let stats = TagStats::build_from(tag_sets.iter());

        let most_used: Vec<(String, u32)> = stats
            .most_used(20)
            .into_iter()
            .filter_map(|(id, count)| {
                tag_bridge::resolve_tag_name(id, registry).map(|n| (n, count))
            })
            .collect();

        let least_used: Vec<(String, u32)> = stats
            .least_used(20)
            .into_iter()
            .filter_map(|(id, count)| {
                tag_bridge::resolve_tag_name(id, registry).map(|n| (n, count))
            })
            .collect();

        let by_category: Vec<(String, u32)> = stats
            .by_category(registry)
            .into_iter()
            .map(|(cat, count)| (cat.display_name().to_string(), count))
            .collect();

        Ok(TagStatsReport {
            total_presets: stats.total_items(),
            tagged_presets: stats.total_items() - stats.untagged_items(),
            coverage_pct: stats.coverage() * 100.0,
            avg_tags_per_preset: stats.average_tags_per_item(),
            most_used,
            least_used,
            by_category,
        })
    }

    // ── Private helpers ─────────────────────────────────────────────────

    /// Load candidate presets with SQL-level filters applied.
    async fn load_candidates(
        db: &DatabaseConnection,
        category: Option<PresetCategory>,
        favorites_only: bool,
    ) -> StorageResult<Vec<preset::Model>> {
        let mut query = preset::Entity::find().filter(preset::Column::IsDeleted.eq(false));

        if favorites_only {
            query = query.filter(preset::Column::IsFavorite.eq(true));
        }

        let all = query.order_by_asc(preset::Column::Name).all(db).await?;

        // Category filter is in-memory (JSON column)
        if let Some(cat) = category {
            Ok(all
                .into_iter()
                .filter(|m| PresetCategory::from_json(&m.category) == Some(cat))
                .collect())
        } else {
            Ok(all)
        }
    }

    async fn find_preset(db: &DatabaseConnection, id: Uuid) -> StorageResult<preset::Model> {
        preset::Entity::find_by_id(id)
            .one(db)
            .await?
            .ok_or(StorageError::NotFound {
                entity: "preset",
                id,
            })
    }

    async fn update_tags(
        db: &DatabaseConnection,
        model: preset::Model,
        tags: &[String],
    ) -> StorageResult<preset::Model> {
        let mut active: preset::ActiveModel = model.into_active_model();
        active.tags = ActiveValue::Set(tags_to_json(tags));
        Ok(active.update(db).await?)
    }
}

// endregion: --- TaggingService

// region: --- TagStatsReport

/// Storage-friendly tag statistics report.
///
/// Aggregated from all non-deleted presets. Field values are pre-computed
/// for direct display — no domain types leak to callers.
#[derive(Debug, Clone)]
pub struct TagStatsReport {
    /// Total non-deleted presets in the database.
    pub total_presets: u32,
    /// Number of presets that have at least one tag.
    pub tagged_presets: u32,
    /// Percentage of presets that have tags (0.0–100.0).
    pub coverage_pct: f64,
    /// Average number of tags per preset.
    pub avg_tags_per_preset: f64,
    /// Top tags by usage count (up to 20).
    pub most_used: Vec<(String, u32)>,
    /// Least used tags by count (up to 20).
    pub least_used: Vec<(String, u32)>,
    /// Tag counts grouped by category display name.
    pub by_category: Vec<(String, u32)>,
}

// endregion: --- TagStatsReport
