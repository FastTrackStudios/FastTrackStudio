//! Music Event workflow schema.
//!
//! Models live concerts, recording sessions, festivals, and multi-act events.
//! All types serialize as YAML frontmatter in `.md` files.

use chrono::{NaiveDate, NaiveTime};
use facet::Facet;

// ── Event ────────────────────────────────────────────────────────────────────

/// The top-level event. A concert, recording session, festival, etc.
/// Stored as `event.md` in the project folder.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Event {
    pub title: String,
    pub event_type: EventType,
    pub status: EventStatus,

    // ── When ─────────────────────────────────────────────────────
    pub date: Option<NaiveDate>,
    pub doors: Option<NaiveTime>,
    pub downbeat: Option<NaiveTime>,
    pub end_time: Option<NaiveTime>,

    // ── Where ────────────────────────────────────────────────────
    pub venue: Option<String>,
    pub address: Option<String>,
    pub room: Option<String>,

    // ── Who ──────────────────────────────────────────────────────
    /// All people involved (crew, artists, venue staff).
    #[facet(default)]
    pub contacts: Vec<Contact>,

    // ── What ─────────────────────────────────────────────────────
    /// Performance acts in order.
    #[facet(default)]
    pub performances: Vec<String>,

    // ── Logistics ────────────────────────────────────────────────
    pub load_in: Option<NaiveTime>,
    pub soundcheck: Option<NaiveTime>,
    pub curfew: Option<NaiveTime>,
    #[facet(default)]
    pub notes: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum EventType {
    #[default]
    Concert,
    RecordingSession,
    Rehearsal,
    Festival,
    Workshop,
    Showcase,
    PrivateEvent,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum EventStatus {
    #[default]
    Planning,
    Confirmed,
    Advanced,
    DayOf,
    Completed,
    Cancelled,
}

// ── Contact ──────────────────────────────────────────────────────────────────

/// A person involved in the event.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Contact {
    pub name: String,
    pub role: String,
    pub email: Option<String>,
    pub phone: Option<String>,
    pub organization: Option<String>,
    #[facet(default)]
    pub notes: String,
}

// ── Performance ──────────────────────────────────────────────────────────────

/// A single act/performance within an event.
/// Stored as `performances/<Act Name>/performance.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Performance {
    pub title: String,
    pub artist: String,
    pub performance_type: PerformanceType,

    // ── Timing ───────────────────────────────────────────────────
    pub start_time: Option<NaiveTime>,
    pub end_time: Option<NaiveTime>,
    pub set_length_minutes: Option<u32>,
    pub soundcheck_time: Option<NaiveTime>,

    // ── Personnel ────────────────────────────────────────────────
    #[facet(default)]
    pub personnel: Vec<PersonnelRole>,

    // ── Content ──────────────────────────────────────────────────
    /// Number of songs in the setlist.
    pub song_count: Option<u32>,

    #[facet(default)]
    pub notes: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum PerformanceType {
    #[default]
    FullSet,
    OpeningAct,
    Headliner,
    GuestAppearance,
    RecordingSession,
    Rehearsal,
}

// ── Personnel ────────────────────────────────────────────────────────────────

/// A role assignment for a performance.
/// Stored as `performances/<Act>/personnel/<role>.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct PersonnelRole {
    pub role: String,
    pub person: String,
    pub status: PersonnelStatus,
    pub instrument: Option<String>,
    /// Backline/gear the person is bringing.
    #[facet(default)]
    pub gear: Vec<String>,
    #[facet(default)]
    pub notes: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum PersonnelStatus {
    #[default]
    Unconfirmed,
    Accepted,
    Declined,
    Tentative,
}

// ── Setlist ──────────────────────────────────────────────────────────────────

/// A setlist for a performance.
/// Stored as `performances/<Act>/setlist.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Setlist {
    pub title: String,
    pub artist: String,
    #[facet(default)]
    pub songs: Vec<Song>,
    pub total_duration_minutes: Option<u32>,
    #[facet(default)]
    pub notes: String,
}

/// A song in a setlist.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Song {
    pub title: String,
    pub key: Option<String>,
    pub tempo: Option<u32>,
    pub duration_seconds: Option<u32>,
    pub notes: Option<String>,
    /// For recording sessions: take count, best take, etc.
    pub take_count: Option<u32>,
    pub best_take: Option<u32>,
}

// ── Stage Plot ───────────────────────────────────────────────────────────────

/// Stage layout for a performance.
/// Stored as `performances/<Act>/stage-plot.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct StagePlot {
    pub title: String,
    pub artist: String,
    #[facet(default)]
    pub positions: Vec<StagePosition>,
    pub stage_width_feet: Option<u32>,
    pub stage_depth_feet: Option<u32>,
    #[facet(default)]
    pub power_requirements: Vec<String>,
    #[facet(default)]
    pub backline_provided: Vec<String>,
    #[facet(default)]
    pub backline_needed: Vec<String>,
    #[facet(default)]
    pub notes: String,
}

/// A position on stage.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct StagePosition {
    pub label: String,
    pub person: Option<String>,
    pub instrument: Option<String>,
    /// Relative position: "center", "stage-left", "upstage-right", etc.
    pub position: Option<String>,
    #[facet(default)]
    pub needs: Vec<String>,
}

// ── Input List ───────────────────────────────────────────────────────────────

/// Audio input list for a performance.
/// Stored as `performances/<Act>/input-list.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct InputList {
    pub title: String,
    pub artist: String,
    pub channel_count: Option<u32>,
    #[facet(default)]
    pub channels: Vec<InputChannel>,
    pub monitor_type: MonitorType,
    pub monitor_mix_count: Option<u32>,
    #[facet(default)]
    pub notes: String,
}

/// A single input channel.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct InputChannel {
    pub channel: u32,
    pub source: String,
    pub mic: Option<String>,
    pub di: bool,
    pub phantom: bool,
    pub stand: Option<String>,
    pub notes: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum MonitorType {
    #[default]
    Wedge,
    IEM,
    Both,
    None,
}

// ── Schedule / Run of Show ───────────────────────────────────────────────────

/// Minute-by-minute timeline for the event.
/// Stored as `schedule/run-of-show.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct RunOfShow {
    pub title: String,
    #[facet(default)]
    pub cues: Vec<Cue>,
    #[facet(default)]
    pub notes: String,
}

/// A single cue in the run of show.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Cue {
    pub time: Option<NaiveTime>,
    pub duration_minutes: Option<u32>,
    pub label: String,
    pub responsible: Option<String>,
    pub cue_type: CueType,
    #[facet(default)]
    pub notes: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum CueType {
    #[default]
    Setup,
    Soundcheck,
    Doors,
    Performance,
    Changeover,
    Break,
    Teardown,
    Other,
}

/// Per-person call times.
/// Stored as `schedule/call-times.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct CallTimes {
    #[facet(default)]
    pub calls: Vec<CallTime>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct CallTime {
    pub person: String,
    pub role: String,
    pub arrival: Option<NaiveTime>,
    pub location: Option<String>,
    #[facet(default)]
    pub notes: String,
}

// ── Changeover ───────────────────────────────────────────────────────────────

/// Plan for transitioning between performances.
/// Stored as `changeovers/<from>-to-<to>.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Changeover {
    pub from_act: String,
    pub to_act: String,
    pub duration_minutes: Option<u32>,
    #[facet(default)]
    pub shared_backline: Vec<String>,
    #[facet(default)]
    pub items_to_remove: Vec<String>,
    #[facet(default)]
    pub items_to_add: Vec<String>,
    #[facet(default)]
    pub responsible: Vec<String>,
    #[facet(default)]
    pub notes: String,
}

// ── Advance / Venue ──────────────────────────────────────────────────────────

/// Venue and advance information.
/// Stored as `advance/venue.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Venue {
    pub name: String,
    pub address: Option<String>,
    pub capacity: Option<u32>,
    pub stage_type: Option<String>,

    // ── Contacts ─────────────────────────────────────────────────
    pub venue_contact: Option<String>,
    pub venue_phone: Option<String>,
    pub venue_email: Option<String>,
    pub production_manager: Option<String>,

    // ── Logistics ────────────────────────────────────────────────
    pub load_in_location: Option<String>,
    pub parking: Option<String>,
    pub green_room: Option<String>,
    pub wifi_network: Option<String>,
    pub wifi_password: Option<String>,

    // ── Technical ────────────────────────────────────────────────
    pub house_pa: Option<String>,
    pub house_console: Option<String>,
    pub house_monitors: Option<String>,
    pub power_available: Option<String>,

    #[facet(default)]
    pub notes: String,
}

// ── Budget ───────────────────────────────────────────────────────────────────

/// Event budget.
/// Stored as `advance/budget.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Budget {
    pub title: String,
    #[facet(default)]
    pub line_items: Vec<BudgetItem>,
    #[facet(default)]
    pub notes: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct BudgetItem {
    pub category: String,
    pub description: String,
    pub amount: Option<f64>,
    pub paid: bool,
    pub vendor: Option<String>,
    #[facet(default)]
    pub notes: String,
}

// ── Deliverables ─────────────────────────────────────────────────────────────

/// Post-event deliverables tracking.
/// Stored as `deliverables/deliverables.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Deliverables {
    #[facet(default)]
    pub items: Vec<Deliverable>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Deliverable {
    pub title: String,
    pub deliverable_type: DeliverableType,
    pub assigned_to: Option<String>,
    pub due: Option<NaiveDate>,
    pub status: DeliverableStatus,
    pub recipient: Option<String>,
    #[facet(default)]
    pub notes: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum DeliverableType {
    #[default]
    AudioMix,
    VideoEdit,
    PhotoSelects,
    LiveStream,
    Invoice,
    Report,
    Other,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum DeliverableStatus {
    #[default]
    Pending,
    InProgress,
    Review,
    Delivered,
    Invoiced,
    Paid,
}
