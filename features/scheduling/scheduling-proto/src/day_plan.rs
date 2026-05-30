//! Per-day plan — a concrete, editable instance of a day's schedule.
//!
//! A [`DayPlan`] is what the calendar's day-by-day editor reads and
//! writes. The recurring [`crate::DayTemplate`] gives the default
//! shape, but a date the user rearranges gets its own saved plan with
//! moved/resized blocks and per-block assignments. One markdown file
//! per date under `Records/dayplans/<date>.md`.

use facet::Facet;
use serde::{Deserialize, Serialize};

use crate::time_block::{BlockCategory, DayTemplateId, TimeBlockId, TimeOfDay};

/// What the user has put in a block for the day. Flat (a `kind`
/// discriminator + fields) rather than a data-carrying enum so it
/// round-trips cleanly through Facet / YAML.
#[derive(Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize)]
pub struct BlockAssignment {
    /// `"label"` (free text), `"task"`, or `"project"`.
    pub kind: String,
    /// Display text — the typed label, or the task / project title.
    pub title: String,
    /// The assigned task / project id (uuid string). `None` for a
    /// plain label.
    #[serde(default)]
    pub ref_id: Option<String>,
}

/// One block in a day plan — a [`crate::TimeBlock`] plus what's
/// assigned to it that day.
#[derive(Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize)]
pub struct PlannedBlock {
    pub id: TimeBlockId,
    pub start: TimeOfDay,
    pub end: TimeOfDay,
    pub label: String,
    pub category: BlockCategory,
    pub note: Option<String>,
    /// `None` = nothing assigned (e.g. an empty allocatable slot).
    pub assignment: Option<BlockAssignment>,
}

/// One date's concrete plan. `date` (ISO `YYYY-MM-DD`) is the key.
#[derive(Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize)]
pub struct DayPlan {
    pub date: String,
    /// The template this plan was materialized from, for provenance.
    pub from_template: Option<DayTemplateId>,
    pub blocks: Vec<PlannedBlock>,
}
