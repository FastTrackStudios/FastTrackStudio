//! Preset tagging and categorization service.
//!
//! Provides tag CRUD, category management, tag-based queries, autocomplete,
//! and favorite/starred flag operations — all against the SeaORM preset entity.
//!
//! Tags are stored as a JSON array of strings in the `tags` column.
//! Categories are stored as a JSON object in the `category` column.

use std::collections::{BTreeSet, HashMap};

use sea_orm::prelude::*;
use sea_orm::{ActiveValue, Condition, IntoActiveModel, QueryOrder, QuerySelect};
use serde_json::Value as JsonValue;

use crate::entities::preset;
use crate::error::{StorageError, StorageResult};

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
    pub async fn get_tags(
        db: &DatabaseConnection,
        preset_id: Uuid,
    ) -> StorageResult<Vec<String>> {
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
    pub async fn toggle_favorite(
        db: &DatabaseConnection,
        preset_id: Uuid,
    ) -> StorageResult<bool> {
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
    pub async fn list_favorites(
        db: &DatabaseConnection,
    ) -> StorageResult<Vec<preset::Model>> {
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
        let mut query = preset::Entity::find()
            .filter(preset::Column::IsDeleted.eq(false));

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

    // ── Private helpers ─────────────────────────────────────────────────

    async fn find_preset(
        db: &DatabaseConnection,
        id: Uuid,
    ) -> StorageResult<preset::Model> {
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
