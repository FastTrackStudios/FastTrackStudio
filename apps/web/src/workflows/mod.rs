//! Workflow extension system — panel registry.
//!
//! Each workflow type registers named **panels** — self-contained UI sections
//! that can be independently enabled, reordered, or collapsed per project.
//!
//! First-party workflows:
//! - `audio-production` — albums, singles, mixing/mastering clients
//! - `video-production` — music videos, promos, livestreams
//! - `performance-event` — concerts, festivals, recording sessions, rehearsals
//! - `code-repository` — software projects, websites, tools

pub mod audio_production;
pub mod video_production;
pub mod performance_event;
pub mod code_repository;

use dioxus::prelude::*;

// ── Panel system ────────────────────────────────────────────────────────────

/// A named, self-contained UI panel within a workflow.
pub struct PanelDef {
    /// Unique ID (e.g. "track-list", "shot-list").
    pub id: &'static str,
    /// Human-readable label shown in the panel header.
    pub label: &'static str,
    /// Whether this panel is collapsed by default.
    pub collapsed: bool,
    /// Render function.
    pub render: fn(&WorkflowContext) -> Element,
}

/// Data passed to every panel render function.
#[derive(Clone, PartialEq)]
pub struct WorkflowContext {
    pub project: crate::ApiProject,
    pub tasks: Vec<crate::ApiTask>,
    pub workflow: crate::WorkflowData,
}

// ── Extension trait ─────────────────────────────────────────────────────────

/// The contract every workflow type implements.
pub trait WorkflowExt {
    /// Human-readable name (e.g. "Audio Production").
    fn label(&self) -> &'static str;

    /// Summary stats shown between the header and progress bar.
    fn stats_panel(&self, ctx: &WorkflowContext) -> Element;

    /// Ordered list of panels this workflow provides.
    fn panels(&self) -> Vec<PanelDef>;

    /// Custom rendering for a session entry.
    /// Return None to use the generic session row.
    fn session_row(&self, _session: &crate::ApiSession) -> Option<Element> {
        None
    }
}

// ── Registry ────────────────────────────────────────────────────────────────

/// Resolve a `project_type` string to its workflow extension.
/// Supports aliases: "album", "ep", "lp", "single" all resolve to audio-production.
pub fn resolve(project_type: &str) -> Option<Box<dyn WorkflowExt>> {
    match project_type {
        // Audio production and aliases
        "audio-production" | "album" | "ep" | "lp" | "single" | "song"
        | "mixtape" | "soundtrack" | "podcast" | "audiobook"
            => Some(Box::new(audio_production::AudioProductionExt)),

        // Video production and aliases
        "video-production" | "music-video" | "documentary" | "short-film"
        | "livestream" | "promo" | "commercial"
            => Some(Box::new(video_production::VideoProductionExt)),

        // Performance events and aliases
        "performance-event" | "concert" | "festival" | "rehearsal"
        | "recording-session" | "showcase" | "workshop" | "church-service"
            => Some(Box::new(performance_event::PerformanceEventExt)),

        // Code repositories and aliases
        "code-repository" | "software" | "website" | "app" | "tool"
        | "library" | "plugin"
            => Some(Box::new(code_repository::CodeRepositoryExt)),

        // Stages within songs don't need a workflow extension
        "stage" => None,

        _ => None,
    }
}
