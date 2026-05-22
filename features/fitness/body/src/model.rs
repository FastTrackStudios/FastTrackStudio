//! `BodyMetric` + `BodyEntry` — one page per tracked
//! metric (`body/weight.md`, `body/bodyfat.md`) carrying
//! the full entry log.
//!
//! Modeled on wger's `Measurement` + `Category` tables,
//! but flattened so a year's worth of daily weigh-ins
//! doesn't explode into 365 files. Each metric page is a
//! self-contained time series; charts walk
//! `metric.entries` directly.

use chrono::{DateTime, NaiveDate, Utc};
use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct BodyMetric {
    #[serde(skip)]
    pub path: String,

    pub id: Uuid,

    /// Display name — `"Weight"`, `"Body Fat %"`,
    /// `"Waist"`.
    pub name: String,

    /// Canonical metric kind. Free-form string;
    /// canonical set in [`MetricKind`]. Drives icons,
    /// goal thresholds, and (future) HealthKit / Google
    /// Fit ingestion mapping.
    #[serde(default = "default_kind")]
    pub kind: String,

    /// Default unit for entries that don't override
    /// (`"kg"`, `"%"`, `"cm"`, `"in"`).
    #[serde(default)]
    pub unit: String,

    /// Optional reference / goal value. Drives a "you're
    /// X% to goal" surface when the metric trends toward
    /// it. Same `unit` as entries.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub goal: Option<f64>,

    /// Free-form tags — `"weekly"`, `"morning"`,
    /// `"post-cut"`.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub tags: Vec<String>,

    /// The time series. Sorted ascending by date on write
    /// so charts read in order; readers shouldn't rely on
    /// order from a hand-edited page.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub entries: Vec<BodyEntry>,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateCreated"
    )]
    pub date_created: Option<DateTime<Utc>>,

    #[serde(
        skip_serializing_if = "Option::is_none",
        default,
        rename = "dateModified"
    )]
    pub date_modified: Option<DateTime<Utc>>,

    #[serde(skip)]
    pub details: String,
}

/// One reading.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
pub struct BodyEntry {
    /// Stable id so UIs can edit individual rows without
    /// re-keying.
    pub id: Uuid,

    pub date: NaiveDate,

    pub value: f64,

    /// Per-entry unit override. `None` falls back to
    /// `BodyMetric::unit`. Useful when imports come in
    /// mixed (kg + lbs from different scales).
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub unit: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub note: Option<String>,
}

fn default_kind() -> String {
    MetricKind::Other.as_str().to_string()
}

/// Canonical metric kinds. Parsing accepts any string —
/// unknown kinds round-trip on `BodyMetric::kind`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MetricKind {
    Weight,
    BodyFat,
    LeanMass,
    Bmi,
    Waist,
    Hips,
    Chest,
    Neck,
    BicepLeft,
    BicepRight,
    ThighLeft,
    ThighRight,
    CalfLeft,
    CalfRight,
    Forearm,
    Other,
}

impl MetricKind {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Weight => "weight",
            Self::BodyFat => "body-fat",
            Self::LeanMass => "lean-mass",
            Self::Bmi => "bmi",
            Self::Waist => "waist",
            Self::Hips => "hips",
            Self::Chest => "chest",
            Self::Neck => "neck",
            Self::BicepLeft => "bicep-left",
            Self::BicepRight => "bicep-right",
            Self::ThighLeft => "thigh-left",
            Self::ThighRight => "thigh-right",
            Self::CalfLeft => "calf-left",
            Self::CalfRight => "calf-right",
            Self::Forearm => "forearm",
            Self::Other => "other",
        }
    }

    #[allow(clippy::should_implement_trait)]
    pub fn from_str(s: &str) -> Option<Self> {
        match s.trim().to_ascii_lowercase().as_str() {
            "weight" | "bw" | "bodyweight" => Some(Self::Weight),
            "body-fat" | "bodyfat" | "bf" | "bf%" => Some(Self::BodyFat),
            "lean-mass" | "lean" | "ffm" => Some(Self::LeanMass),
            "bmi" => Some(Self::Bmi),
            "waist" => Some(Self::Waist),
            "hips" | "hip" => Some(Self::Hips),
            "chest" => Some(Self::Chest),
            "neck" => Some(Self::Neck),
            "bicep-left" | "biceps-left" | "bicep_l" => Some(Self::BicepLeft),
            "bicep-right" | "biceps-right" | "bicep_r" => Some(Self::BicepRight),
            "thigh-left" | "quad-left" | "thigh_l" => Some(Self::ThighLeft),
            "thigh-right" | "quad-right" | "thigh_r" => Some(Self::ThighRight),
            "calf-left" => Some(Self::CalfLeft),
            "calf-right" => Some(Self::CalfRight),
            "forearm" | "forearm-left" | "forearm-right" => Some(Self::Forearm),
            _ => None,
        }
    }
}

impl BodyMetric {
    /// Most recent entry by date. Ties resolve by entry id
    /// (stable).
    pub fn latest(&self) -> Option<&BodyEntry> {
        self.entries.iter().max_by_key(|e| (e.date, e.id))
    }

    /// Trailing average over the last `n` entries by date
    /// order. Returns `None` when no entries exist.
    pub fn trailing_average(&self, n: usize) -> Option<f64> {
        if self.entries.is_empty() {
            return None;
        }
        let mut sorted: Vec<&BodyEntry> = self.entries.iter().collect();
        sorted.sort_by_key(|e| e.date);
        let take = sorted.len().min(n.max(1));
        let slice = &sorted[sorted.len() - take..];
        let sum: f64 = slice.iter().map(|e| e.value).sum();
        Some(sum / slice.len() as f64)
    }
}
