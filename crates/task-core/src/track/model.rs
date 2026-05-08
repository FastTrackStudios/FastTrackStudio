//! `Track` entity — one song / recording belonging to a project.
//!
//! Audio-production projects (and any workflow that needs ordered,
//! status-bearing deliverables with mix/master metadata) hang `Track`
//! rows off `Project` via `project_id`. Status drives the production
//! workflow (Composing → Demo → Tracking → Editing → Mixing →
//! Mastering → Approved → Released). Polymorphic
//! [`properties`](crate::property::JsonObject) match the Obsidian
//! frontmatter shape used elsewhere.

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
#[sea_orm(table_name = "tracks")]
#[crudcrate(
    api_struct = "TrackApi",
    generate_vox_service,
    name_singular = "track",
    name_plural = "tracks"
)]
pub struct Model {
    #[facet(default)]
    #[sea_orm(primary_key, auto_increment = false)]
    #[crudcrate(primary_key, exclude(create), on_create = Uuid::new_v4())]
    pub id: Uuid,

    /// FK to `projects.id` — the owning project.
    #[crudcrate(filterable)]
    pub project_id: Uuid,

    pub title: String,

    /// 1-based track number within the project. Two tracks in the same
    /// project should not share a sequence number; `AudioProductionService`
    /// auto-appends past the current max when no explicit value is given.
    pub sequence: u32,

    #[crudcrate(filterable)]
    pub status: TrackStatus,

    /// BPM as a float (audio production routinely uses fractional BPMs
    /// like 87.5 or 132.7).
    pub bpm: Option<f64>,
    /// Musical key — free-form string for now ("C", "Am", "F#m", "D Dorian").
    pub key: Option<String>,
    /// Duration in milliseconds.
    pub duration_ms: Option<i64>,
    /// Time signature ("4/4", "6/8", "7/8", "5/4").
    pub time_signature: Option<String>,

    /// Path (relative to user's Nextcloud root or absolute filesystem
    /// path) to the DAW session file.
    pub daw_session_path: Option<String>,
    /// Path to the stems bundle / folder.
    pub stems_path: Option<String>,
    /// External URL or relative path to a reference track for mix
    /// comparison (Spotify link, file path, etc.).
    pub reference_url: Option<String>,

    /// Optional ISRC code, useful when releasing.
    pub isrc: Option<String>,
    /// Performing/credited artist name (defaults to the project's
    /// organization or lead).
    pub artist: Option<String>,
    /// One-line track notes; longer threads go in linked task comments.
    pub notes: Option<String>,
    /// Track-level revision counter — incremented on each `submit_mix`.
    pub revision_number: i32,

    pub created_by: Option<String>,

    /// Obsidian-style polymorphic properties (same JsonObject shape as
    /// Task/Project/etc.).
    #[crudcrate(exclude(list))]
    #[facet(skip)]
    #[facet(default)]
    #[sea_orm(column_type = "Json")]
    pub properties: crate::property::JsonObject,

    #[crudcrate(exclude(create), on_create = chrono::Utc::now())]
    pub created_at: chrono::DateTime<chrono::Utc>,
    #[crudcrate(exclude(create), exclude(update), on_create = chrono::Utc::now())]
    pub updated_at: chrono::DateTime<chrono::Utc>,
}

#[derive(Copy, Clone, Debug, EnumIter, DeriveRelation)]
pub enum Relation {}

impl ActiveModelBehavior for ActiveModel {
    fn new() -> Self {
        Self {
            properties: sea_orm::ActiveValue::Set(crate::property::JsonObject::default()),
            revision_number: sea_orm::ActiveValue::Set(0),
            ..<Self as sea_orm::ActiveModelTrait>::default()
        }
    }
}

/// Audio-production workflow status.
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
pub enum TrackStatus {
    #[default]
    #[sea_orm(string_value = "composing")]
    Composing,
    #[sea_orm(string_value = "demo")]
    Demo,
    #[sea_orm(string_value = "tracking")]
    Tracking,
    #[sea_orm(string_value = "editing")]
    Editing,
    #[sea_orm(string_value = "mixing")]
    Mixing,
    #[sea_orm(string_value = "mastering")]
    Mastering,
    #[sea_orm(string_value = "approved")]
    Approved,
    #[sea_orm(string_value = "released")]
    Released,
}

impl TrackStatus {
    pub fn as_str(&self) -> &'static str {
        match self {
            TrackStatus::Composing => "composing",
            TrackStatus::Demo => "demo",
            TrackStatus::Tracking => "tracking",
            TrackStatus::Editing => "editing",
            TrackStatus::Mixing => "mixing",
            TrackStatus::Mastering => "mastering",
            TrackStatus::Approved => "approved",
            TrackStatus::Released => "released",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s.to_ascii_lowercase().as_str() {
            "composing" | "compose" => Some(TrackStatus::Composing),
            "demo" => Some(TrackStatus::Demo),
            "tracking" | "track" => Some(TrackStatus::Tracking),
            "editing" | "edit" => Some(TrackStatus::Editing),
            "mixing" | "mix" => Some(TrackStatus::Mixing),
            "mastering" | "master" => Some(TrackStatus::Mastering),
            "approved" | "approve" => Some(TrackStatus::Approved),
            "released" | "release" => Some(TrackStatus::Released),
            _ => None,
        }
    }
}

pub type Track = Model;
