//! Navigation Action Sets
//!
//! Defines cursor movement, track selection, and navigation controls.

use crate::input::keybinds::{ActionSet, Keybind};

/// Standard REAPER navigation (arrow keys)
pub struct ReaperNavigation;

impl ActionSet for ReaperNavigation {
    fn name(&self) -> &'static str {
        "ReaperNavigation"
    }

    fn keybinds(&self) -> Vec<Keybind> {
        // Minimal - let REAPER handle default arrow key navigation
        vec![]
    }
}

/// Logic Pro style navigation
pub struct LogicNavigation;

impl ActionSet for LogicNavigation {
    fn name(&self) -> &'static str {
        "LogicNavigation"
    }

    fn keybinds(&self) -> Vec<Keybind> {
        vec![
            // Logic uses arrow keys for navigation - let REAPER defaults handle
        ]
    }
}

/// FastTrackStudio navigation (vim-style hjkl)
pub struct FtsNavigation;

impl ActionSet for FtsNavigation {
    fn name(&self) -> &'static str {
        "FtsNavigation"
    }

    fn keybinds(&self) -> Vec<Keybind> {
        vec![
            // === Vim-style Navigation ===
            Keybind::new("h", "40104")
                .with_description("Move cursor left (to previous grid division)"),
            Keybind::new("l", "40105")
                .with_description("Move cursor right (to next grid division)"),
            Keybind::new("j", "40285").with_description("Select next track"),
            Keybind::new("k", "40286").with_description("Select previous track"),
            // === Jump Navigation ===
            Keybind::new("<C-h>", "40042").with_description("Move cursor to previous item"),
            Keybind::new("<C-l>", "40043").with_description("Move cursor to next item"),
            Keybind::new("0", "40042").with_description("Go to start of project"),
            Keybind::new("<S-h>", "40416").with_description("Select and move to previous item"),
            Keybind::new("<S-l>", "40417").with_description("Select and move to next item"),
            // === Selection Extension ===
            Keybind::new("<S-j>", "40421").with_description("Extend selection to next track"),
            Keybind::new("<S-k>", "40420").with_description("Extend selection to previous track"),
            // === Markers ===
            Keybind::new("m", "40157").with_description("Insert marker at cursor"),
            Keybind::new("<S-m>", "40174").with_description("Go to next marker"),
            Keybind::new("<C-m>", "40172").with_description("Go to previous marker"),
        ]
    }
}
