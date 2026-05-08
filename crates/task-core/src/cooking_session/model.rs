//! `CookingSession` entity — one in-progress (or completed) walkthrough
//! of a recipe.
//!
//! State machine:
//! - `Active`     — session is in progress, user is checking off mise en
//!                   place or stepping through.
//! - `Paused`     — reserved for future "pause the whole session" flow;
//!                   per-step pauses use `step_states[i].paused_at`.
//! - `Completed`  — user finished the recipe; optionally generates a
//!                   FoodLog row + updates `Recipe.last_made`.
//! - `Abandoned`  — user gave up; no FoodLog row, no last_made update.
//!
//! `current_step_index` uses `-1` to represent "before the first step"
//! (mise en place phase) so the UI has a single linear cursor.

use chrono::{DateTime, Utc};
use crudcrate::EntityToModels;
use facet::Facet;
use sea_orm::entity::prelude::*;
use serde::{Deserialize, Serialize};

#[derive(
    Clone,
    Debug,
    Default,
    PartialEq,
    Facet,
    DeriveEntityModel,
    EntityToModels,
    Serialize,
    Deserialize,
)]
#[sea_orm(table_name = "cooking_sessions")]
#[crudcrate(
    api_struct = "CookingSessionApi",
    generate_vox_service,
    name_singular = "cooking_session",
    name_plural = "cooking_sessions"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[crudcrate(filterable)]
    pub recipe_id: Uuid,

    /// Denormalized recipe name, captured at session-start so the
    /// session survives the recipe being renamed or deleted.
    pub recipe_name_snapshot: String,

    #[crudcrate(filterable)]
    pub status: CookingSessionStatus,

    /// 0-based; `-1` means "before first step" (session just started,
    /// mise en place phase).
    pub current_step_index: i32,

    /// User-chosen serving count for this cook. Defaults to
    /// `recipe.servings` at session start.
    pub scaled_servings: Option<u32>,

    /// `Vec<bool>` (JSON), indexed by ingredient sequence. Length is
    /// snapshotted at session start — recipe edits made after the
    /// session begins do not change this vector.
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub mise_en_place_state: crate::property::JsonObject,

    /// `Vec<StepState>` (JSON), indexed by step sequence. See
    /// [`crate::cooking_session::state::StepState`].
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub step_states: crate::property::JsonObject,

    pub notes: Option<String>,
    pub started_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,

    #[crudcrate(filterable)]
    pub organization: Option<String>,
    pub created_by: Option<String>,

    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub properties: crate::property::JsonObject,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: DateTime<Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {
    fn new() -> Self {
        Self {
            mise_en_place_state: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            step_states: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            current_step_index: sea_orm::ActiveValue::Set(-1),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

/// Lifecycle of a [`CookingSession`].
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    Default,
    Serialize,
    Deserialize,
    DeriveActiveEnum,
    EnumIter,
    utoipa::ToSchema,
    facet::Facet,
)]
#[sea_orm(rs_type = "String", db_type = "String(StringLen::N(16))")]
#[repr(u8)]
pub enum CookingSessionStatus {
    #[default]
    #[sea_orm(string_value = "active")]
    Active,
    #[sea_orm(string_value = "paused")]
    Paused,
    #[sea_orm(string_value = "completed")]
    Completed,
    #[sea_orm(string_value = "abandoned")]
    Abandoned,
}

impl CookingSessionStatus {
    #[must_use]
    pub fn as_str(&self) -> &'static str {
        match self {
            CookingSessionStatus::Active => "active",
            CookingSessionStatus::Paused => "paused",
            CookingSessionStatus::Completed => "completed",
            CookingSessionStatus::Abandoned => "abandoned",
        }
    }

    #[must_use]
    pub fn parse(s: &str) -> Option<Self> {
        match s.to_ascii_lowercase().as_str() {
            "active" => Some(CookingSessionStatus::Active),
            "paused" | "pause" => Some(CookingSessionStatus::Paused),
            "completed" | "complete" | "done" => Some(CookingSessionStatus::Completed),
            "abandoned" | "abandon" => Some(CookingSessionStatus::Abandoned),
            _ => None,
        }
    }
}

pub type CookingSession = Model;
