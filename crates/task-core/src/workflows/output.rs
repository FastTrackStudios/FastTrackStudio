//! Versioned outputs — stages + versioned files.
//!
//! A song/deliverable has **stages** — checklist items that can be completed
//! in any order. Not a linear pipeline. Each stage optionally has versioned
//! files attached (mix revisions, master exports, etc).
//!
//! ```text
//! Sunrise
//!   ☑ Writing      — locked
//!   ☑ Recording    — done
//!   ☐ Editing      — in progress
//!   ☑ Production   — done
//!   ☐ Mixing       — not started
//!   ☐ Mastering    — not started
//!   ☐ Approved
//!   ☐ Released
//! ```
//!
//! Some songs skip stages. An instrumental has no Writing.
//! A beat has no Recording. You just don't add them.
//!
//! ## Storage
//! ```text
//! songs/Sunrise/
//! ├── song.md          ← SongManifest
//! ├── ideas/           ← voice memos, scraps
//! ├── mixes/           ← versioned mix files
//! │   ├── Sunrise Mix v1.wav
//! │   ├── Sunrise Mix v2.wav
//! │   └── Sunrise Mix v3.wav
//! └── masters/
//!     └── Sunrise Master v1.wav
//! ```

use chrono::NaiveDate;
use facet::Facet;

use super::approval::Approval;
use super::comments::Comment;

// ── Stage ───────────────────────────────────────────────────────────────────

/// A production stage — a checklist item with optional versioned files.
/// Stages can be completed in any order.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Stage {
    /// What this stage is. Can be a preset or a custom string.
    pub name: String,
    /// Current status.
    pub status: StageStatus,
    /// Who's responsible for this stage.
    pub assignee: Option<String>,
    /// Versioned files in this stage (newest first). Optional — many stages
    /// don't produce files (e.g. "Approved" is just a status).
    #[facet(default)]
    pub versions: Vec<Version>,
    /// Approval state (for stages that need sign-off, like Mixes).
    #[facet(default)]
    pub approval: Approval,
    /// Notes.
    #[facet(default)]
    pub notes: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum StageStatus {
    #[default]
    NotStarted,
    InProgress,
    Done,
    Skipped,
}

impl Stage {
    pub fn is_done(&self) -> bool {
        matches!(self.status, StageStatus::Done | StageStatus::Skipped)
    }

    pub fn latest_version(&self) -> Option<&Version> {
        self.versions.first()
    }

    pub fn version_count(&self) -> usize {
        self.versions.len()
    }
}

/// Common stage presets. Use these or make your own with any string.
pub mod stage_presets {
    pub const WRITING: &str = "Writing";
    pub const RECORDING: &str = "Recording";
    pub const PRODUCTION: &str = "Production";
    pub const EDITING: &str = "Editing";
    pub const MIXING: &str = "Mixing";
    pub const MASTERING: &str = "Mastering";
    pub const APPROVED: &str = "Approved";
    pub const RELEASED: &str = "Released";

    // Video
    pub const FILMING: &str = "Filming";
    pub const ROUGH_CUT: &str = "Rough Cut";
    pub const FINE_CUT: &str = "Fine Cut";
    pub const COLOR_GRADE: &str = "Color Grade";
    pub const VFX: &str = "VFX";
    pub const SOUND_DESIGN: &str = "Sound Design";
    pub const DELIVERY: &str = "Delivery";

    pub fn audio_defaults() -> Vec<&'static str> {
        vec![WRITING, RECORDING, PRODUCTION, EDITING, MIXING, MASTERING, APPROVED, RELEASED]
    }

    pub fn video_defaults() -> Vec<&'static str> {
        vec![FILMING, ROUGH_CUT, FINE_CUT, COLOR_GRADE, SOUND_DESIGN, DELIVERY, APPROVED, RELEASED]
    }

    pub fn instrumental_defaults() -> Vec<&'static str> {
        vec![RECORDING, PRODUCTION, EDITING, MIXING, MASTERING, APPROVED, RELEASED]
    }
}

// ── Version ─────────────────────────────────────────────────────────────────

/// A single versioned file within a stage.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Version {
    /// Version number (1, 2, 3...).
    pub number: u32,
    /// Display name (e.g. "Sunrise Mix v3").
    pub name: String,
    /// File path relative to the song directory.
    pub file: String,
    pub date: Option<NaiveDate>,
    pub created_by: Option<String>,
    pub status: VersionStatus,
    #[facet(default)]
    pub notes: String,
    /// Comments/feedback on this version (with ranged timecodes).
    #[facet(default)]
    pub comments: Vec<Comment>,

    // ── Media metadata ──────────────────────────
    pub format: Option<String>,
    pub sample_rate: Option<u32>,
    pub bit_depth: Option<u32>,
    pub duration_seconds: Option<u32>,
    pub size_bytes: Option<u64>,
    pub resolution: Option<String>,
    pub frame_rate: Option<f64>,
    pub codec: Option<String>,
}

impl Version {
    pub fn display_name(&self) -> String {
        if self.name.is_empty() {
            format!("v{}", self.number)
        } else {
            self.name.clone()
        }
    }

    pub fn unresolved_comment_count(&self) -> usize {
        self.comments.iter().filter(|c| !c.resolved && c.reply_to.is_none()).count()
    }
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum VersionStatus {
    #[default]
    Draft,
    Review,
    ChangesRequested,
    Approved,
    Superseded,
    Rejected,
}

// ── Song Manifest ───────────────────────────────────────────────────────────

/// Full lifecycle manifest for a song/deliverable.
/// Stored as `songs/<Title>/song.md`.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct SongManifest {
    pub title: String,
    pub artist: Option<String>,

    // ── Musical metadata ────────────────────────
    pub key: Option<String>,
    pub tempo: Option<u32>,
    pub time_signature: Option<String>,
    pub duration_seconds: Option<u32>,

    // ── Stages ──────────────────────────────────
    /// Production stages. Any order, any subset. Add/remove as needed.
    #[facet(default)]
    pub stages: Vec<Stage>,

    // ── Sub-workflows (opt-in) ──────────────────
    /// Songwriting — only present when collaborative writing is happening.
    pub writing: Option<super::songwriting::WritingWorkflow>,

    // ── Release info ────────────────────────────
    pub isrc: Option<String>,
    pub release_date: Option<NaiveDate>,
    #[facet(default)]
    pub credits: Vec<Credit>,
    #[facet(default)]
    pub lyrics: String,
    #[facet(default)]
    pub genres: Vec<String>,
}

impl SongManifest {
    pub fn new(title: &str) -> Self {
        Self {
            title: title.to_string(),
            ..Default::default()
        }
    }

    /// Create with preset stages (only adds the names, all NotStarted).
    pub fn with_stages(title: &str, stage_names: &[&str]) -> Self {
        Self {
            title: title.to_string(),
            stages: stage_names.iter().map(|name| Stage {
                name: name.to_string(),
                ..Default::default()
            }).collect(),
            ..Default::default()
        }
    }

    /// Get a stage by name.
    pub fn stage(&self, name: &str) -> Option<&Stage> {
        self.stages.iter().find(|s| s.name == name)
    }

    /// Get a mutable stage, creating it if it doesn't exist (appends to end).
    pub fn stage_mut(&mut self, name: &str) -> &mut Stage {
        if !self.stages.iter().any(|s| s.name == name) {
            self.stages.push(Stage {
                name: name.to_string(),
                ..Default::default()
            });
        }
        self.stages.iter_mut().find(|s| s.name == name).unwrap()
    }

    /// Remove a stage by name (only if NotStarted or Skipped).
    pub fn remove_stage(&mut self, name: &str) -> bool {
        if let Some(idx) = self.stages.iter().position(|s| s.name == name) {
            if self.stages[idx].is_done() || self.stages[idx].status == StageStatus::NotStarted {
                self.stages.remove(idx);
                return true;
            }
        }
        false
    }

    /// How many stages are done.
    pub fn stages_done(&self) -> usize {
        self.stages.iter().filter(|s| s.is_done()).count()
    }

    /// Overall progress as 0.0–1.0.
    pub fn progress(&self) -> f64 {
        if self.stages.is_empty() { return 0.0; }
        self.stages_done() as f64 / self.stages.len() as f64
    }

    /// Total versions across all stages.
    pub fn total_versions(&self) -> usize {
        self.stages.iter().map(|s| s.versions.len()).sum()
    }

    /// Total unresolved comments across everything.
    pub fn unresolved_comments(&self) -> usize {
        self.stages.iter()
            .flat_map(|s| &s.versions)
            .map(|v| v.unresolved_comment_count())
            .sum()
    }

    /// Enable the writing sub-workflow.
    pub fn enable_writing(&mut self, writers: Vec<String>) {
        if self.writing.is_none() {
            self.writing = Some(super::songwriting::WritingWorkflow {
                writers,
                ..Default::default()
            });
        }
    }
}

// ── Legacy compat ───────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct OutputManifest {
    pub title: String,
    pub project: String,
    pub current_version: u32,
    #[facet(default)]
    pub outputs: Vec<Output>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Output {
    pub version: u32,
    pub file: String,
    pub date: Option<NaiveDate>,
    pub status: OutputStatus,
    #[facet(default)]
    pub notes: String,
    pub approved_by: Option<String>,
    #[facet(default)]
    pub feedback: Vec<Feedback>,
    pub format: Option<String>,
    pub sample_rate: Option<u32>,
    pub bit_depth: Option<u32>,
    pub duration_seconds: Option<u32>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum OutputStatus {
    #[default]
    Draft,
    Review,
    ChangesRequested,
    Approved,
    Superseded,
    Rejected,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Feedback {
    pub from: String,
    pub comment: String,
    pub timestamp: Option<NaiveDate>,
    pub timecode: Option<String>,
    pub resolved: bool,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct ProjectLink {
    pub project: String,
    pub output: Option<String>,
    pub version: Option<u32>,
    pub title: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct Credit {
    pub name: String,
    pub role: String,
    pub pro: Option<String>,
    pub ipi: Option<String>,
    pub split_percent: Option<f64>,
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use super::stage_presets::*;

    #[test]
    fn nonlinear_production() {
        let mut song = SongManifest::with_stages("Sunrise", &audio_defaults());
        assert_eq!(song.stages.len(), 8);
        assert_eq!(song.progress(), 0.0);

        // Write the chorus, start recording the verse — nonlinear
        song.stage_mut(WRITING).status = StageStatus::InProgress;
        song.stage_mut(RECORDING).status = StageStatus::InProgress;

        // Finish recording before writing is done
        song.stage_mut(RECORDING).status = StageStatus::Done;
        assert_eq!(song.stages_done(), 1);

        // Start editing even though writing is still in progress
        song.stage_mut(EDITING).status = StageStatus::InProgress;

        // Now finish writing
        song.stage_mut(WRITING).status = StageStatus::Done;
        assert_eq!(song.stages_done(), 2);
        assert!((song.progress() - 0.25).abs() < 0.01); // 2/8
    }

    #[test]
    fn instrumental_skips_writing() {
        let song = SongManifest::with_stages("Jazz Interlude", &instrumental_defaults());
        // No "Writing" stage at all
        assert!(song.stage(WRITING).is_none());
        assert_eq!(song.stages.len(), 7);
    }

    #[test]
    fn add_remove_stages_dynamically() {
        let mut song = SongManifest::new("Beat");

        // Start minimal
        song.stage_mut(PRODUCTION).status = StageStatus::InProgress;
        assert_eq!(song.stages.len(), 1);

        // Decide it needs mixing
        song.stage_mut(MIXING);
        assert_eq!(song.stages.len(), 2);

        // Remove mixing (not started)
        assert!(song.remove_stage(MIXING));
        assert_eq!(song.stages.len(), 1);

        // Can't remove production (in progress)
        assert!(!song.remove_stage(PRODUCTION));
    }

    #[test]
    fn stage_with_versions() {
        let mut song = SongManifest::new("Sunrise");
        let mixes = song.stage_mut(MIXING);
        mixes.status = StageStatus::InProgress;
        mixes.versions.push(Version {
            number: 1,
            name: "Sunrise Mix v1".into(),
            file: "mixes/Sunrise Mix v1.wav".into(),
            status: VersionStatus::ChangesRequested,
            notes: "Kick too loud".into(),
            ..Default::default()
        });
        mixes.versions.push(Version {
            number: 2,
            name: "Sunrise Mix v2".into(),
            file: "mixes/Sunrise Mix v2.wav".into(),
            status: VersionStatus::Approved,
            ..Default::default()
        });

        assert_eq!(mixes.version_count(), 2);
        assert_eq!(mixes.latest_version().unwrap().number, 1); // newest first
        assert_eq!(song.total_versions(), 2);
    }

    #[test]
    fn writing_subworkflow_opt_in() {
        let mut song = SongManifest::new("Sunrise");
        assert!(song.writing.is_none());

        // Bring in a co-writer
        song.enable_writing(vec!["cody".into(), "amy".into()]);
        assert!(song.writing.is_some());
        assert_eq!(song.writing.as_ref().unwrap().writers.len(), 2);
    }

    #[test]
    fn custom_stages() {
        // Podcast episode — totally different stages
        let song = SongManifest::with_stages("Episode 42", &[
            "Research", "Outline", "Record", "Edit", "Sound Design",
            "Review", "Publish",
        ]);
        assert_eq!(song.stages.len(), 7);
        assert!(song.stage("Research").is_some());
        assert!(song.stage("Publish").is_some());
    }
}
