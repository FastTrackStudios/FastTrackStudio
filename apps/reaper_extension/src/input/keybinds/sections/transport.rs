//! Transport Action Sets
//!
//! Defines play, stop, record, and transport navigation controls.

use crate::input::keybinds::{ActionSet, Keybind};

/// Standard REAPER transport (Space = Play/Stop)
pub struct ReaperTransport;

impl ActionSet for ReaperTransport {
    fn name(&self) -> &'static str {
        "ReaperTransport"
    }

    fn keybinds(&self) -> Vec<Keybind> {
        vec![
            // Standard REAPER transport - minimal, let REAPER handle defaults
        ]
    }
}

/// Logic Pro style transport
pub struct LogicTransport;

impl ActionSet for LogicTransport {
    fn name(&self) -> &'static str {
        "LogicTransport"
    }

    fn keybinds(&self) -> Vec<Keybind> {
        vec![
            // Space = Play/Stop
            Keybind::new("<space>", "40044").with_description("Play/Stop"),
            // Enter = Go to start
            Keybind::new("<enter>", "40042").with_description("Go to start of project"),
            // R = Record
            Keybind::new("r", "1013").with_description("Toggle record"),
            // , and . for marker navigation (Logic style)
            Keybind::new(",", "40172").with_description("Go to previous marker"),
            Keybind::new(".", "40173").with_description("Go to next marker"),
        ]
    }
}

/// FastTrackStudio transport (vim-inspired)
pub struct FtsTransport;

impl ActionSet for FtsTransport {
    fn name(&self) -> &'static str {
        "FtsTransport"
    }

    fn keybinds(&self) -> Vec<Keybind> {
        vec![
            Keybind::new("<space>", "40044").with_description("Play/Stop"),
            Keybind::new("<S-space>", "1013").with_description("Play/Pause"),
            Keybind::new("<enter>", "40317").with_description("Move cursor to start of selection"),
            Keybind::new("r", "1013").with_description("Toggle record"),
            Keybind::new("<S-r>", "40490").with_description("Arm selected tracks for recording"),
            Keybind::new(",", "40172").with_description("Go to previous marker/project start"),
            Keybind::new(".", "40173").with_description("Go to next marker/project end"),
        ]
    }
}
